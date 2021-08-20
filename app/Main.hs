module Main where

import qualified Data.ByteString.Lazy.UTF8 as BS
import Data.Digest.Pure.SHA
import Data.List
import System.Directory
import System.Environment
import System.Exit
import System.IO
import System.Process
import System.FilePath.Posix
import Data.Time.Clock.System
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Data.Int
import qualified Database.SQLite.Simple as DB
import Data.Foldable

data Env = Env
  { getProgramDir :: !FilePath
  , getCacheDir :: !FilePath
  , getDBConn :: !DB.Connection
  , getDelayedOutput :: !(MVar Bool)
  , getStaleThreshold :: !Int64
  }

main :: IO ()
main = do
  args <- getArgs
  home <- getEnv "HOME"
  let programDir = home </> ".cabal" </> "cabal-script"
      cacheDir = programDir </> "cache"
      dbPath = programDir </> "cache.db"
      staleThreshold = 60
  dbConn <- dbInitAndOpen dbPath
  delayedOutput <- newEmptyMVar
  let env = Env programDir cacheDir dbConn delayedOutput staleThreshold
  runFile env (head args)

-- |Run a Haskell script file.
runFile :: Env -> FilePath -> IO ()
runFile env file = do
  contents   <- readFile file
  let hash = showDigest (sha1 (BS.fromString contents))
  binaryPath <-  lookupCache env hash >>= \case
                  Just found -> return found
                  Nothing -> do { rootDir <- createProject contents hash
                                ; binPath <- buildProject env rootDir
                                ; cachedBinPath <- cacheBinary env binPath hash
                                ; forkIO $ do { dbRecordCache env hash
                                              ; deleteStaleCaches env }
                                ; deleteProject rootDir
                                ; return cachedBinPath }
  _ <- execBinary binaryPath
  return ()

-- |Try to get the path to the cached binary from the DB.  The
-- timestamp is updated simultaneously.  If not cached, Nothing is
-- returned.
lookupCache :: Env -> String -> IO (Maybe FilePath)
lookupCache env hash = do
  let cacheDir = getCacheDir env
      cachedBinPath = cacheDir </> hash
  cached <- dbCheckAndTouch env hash
  fileExists <- doesPathExist cachedBinPath
  return $ if cached && fileExists
    then Just cachedBinPath
    else Nothing

-- |Cache a binary.  Return the path to the cached binary.
cacheBinary :: Env -> FilePath -> String -> IO FilePath
cacheBinary env binPath hash = do
  let cacheDir = getCacheDir env
      cachedBinPath = cacheDir </> hash
  createDirectoryIfMissing True cacheDir
  copyFile binPath cachedBinPath
  callProcess "strip" [cachedBinPath]
  return cachedBinPath

dbInitAndOpen :: FilePath -> IO DB.Connection
dbInitAndOpen dbPath = do
  conn <- DB.open dbPath
  DB.execute_ conn "CREATE TABLE IF NOT EXISTS cache_info (hash TEXT PRIMARY KEY, last_used INTEGER NOT NULL)"
  return conn

-- |Record that a binary is newly cached.
dbRecordCache :: Env -> String -> IO ()
dbRecordCache env hash = do
  let conn = getDBConn env
  timestamp <- systemSeconds <$> getSystemTime
  DB.execute conn "INSERT INTO cache_info VALUES (?,?)" (hash, timestamp)

-- |Atomatically check if hash is cached and update the stored
-- timestamp.
dbCheckAndTouch :: Env -> String -> IO Bool
dbCheckAndTouch env hash = do
  let conn = getDBConn env
  timestamp <- systemSeconds <$> getSystemTime
  DB.execute conn "UPDATE cache_info SET last_used = ? WHERE hash = ?" (timestamp, hash)
  changes <- DB.changes conn
  return $ changes == 1

deleteStaleCaches :: Env -> IO ()
deleteStaleCaches env = do
  let threshold = getStaleThreshold env
      conn = getDBConn env
      cacheDir = getCacheDir env
  current <- systemSeconds <$> getSystemTime
  let bound = (fromIntegral ((current - threshold) `max` 0)) :: Integer
  shouldDelete <- DB.withTransaction conn $ do
    shouldDelete <- DB.query conn "SELECT hash FROM cache_info WHERE last_used < ?" [bound] :: IO [[String]]
    DB.execute conn "DELETE FROM cache_info WHERE last_used < ?" [bound]
    return shouldDelete
  traverse_ (\[h] -> removeFile (cacheDir </> h)) shouldDelete

-- |Create a fake package, based on the script contents.
createProject :: String -> String -> IO FilePath
createProject contents hash = do
  let rootPath = "/tmp/cabal-script-" <> hash
      deps     = extractDeps contents
  createDirectoryIfMissing True (rootPath </> "app")
  writeCabalFile (rootPath </> "project.cabal") hash deps
  writeSourceFile (rootPath </> "app/Main.hs") contents
  return rootPath

-- |Extract build-depends from the script file.
--
-- It collects the first group of consecutive comment lines.
extractDeps :: String -> [String]
extractDeps contents = addBaseIfMissing $ go [] (lines contents) False
  where go deps [] _ = deps
        go deps (cur:lines) insideBlock =
          if "--" `isPrefixOf` cur
          then go ((drop 2 cur) : deps) lines True
          else if insideBlock
               then deps
               else go deps lines False
        addBaseIfMissing deps =
          if any ("base" `isInfixOf`) deps then deps else "base" : deps

-- |Generate a .cabal file for the fake package.
writeCabalFile :: FilePath -> String -> [String] -> IO ()
writeCabalFile path hash deps = do
  writeFile path
    (  "cabal-version: 2.4\n" 
    ++ "name: cabal-script-" ++ hash ++ "\n"
    ++ "version: 0.1.0.0\n"
    ++ "executable main\n"
    ++ "  main-is: Main.hs\n"
    ++ "  default-language: Haskell2010\n"
    ++ "  hs-source-dirs: app\n"
    ++ "  build-depends: " ++ intercalate "," deps)

-- |Generate the source file for the fake package.
writeSourceFile :: FilePath -> String -> IO ()
writeSourceFile = writeFile

-- |Build the fake package, and return the binary path.
buildProject :: Env -> FilePath -> IO FilePath
buildProject env rootPath = do
  (_, Just hout, Just herr, handle) <- createProcess
    (proc "cabal" ["build", "-O"]) { cwd = Just rootPath
                                   , std_out = CreatePipe
                                   , std_err = CreatePipe }
  let shouldOutput = getDelayedOutput env
  forkIO $ delay_ 1000000 $ tryPutMVar shouldOutput True
  forkIO $ delayOutput shouldOutput hout stdout
  forkIO $ delayOutput shouldOutput herr stderr
  waitForProcess handle >>= \case
    ExitFailure _ -> do
      tryPutMVar shouldOutput True
      error ("Build failed for project " <> rootPath)
    ExitSuccess -> do
      tryPutMVar shouldOutput False
      (_, Just hout, _, _) <- createProcess
        (proc "cabal" ["list-bin", "main", "-O"]) { cwd = Just rootPath
                                                  , std_out = CreatePipe }
      binaryPath' <- hGetContents hout
      let binaryPath = reverse (drop 1 (reverse binaryPath'))
      return binaryPath

-- |Recursively delete the fake package.
deleteProject :: FilePath -> IO ()
deleteProject = removeDirectoryRecursive

-- |Execute the binary with supplied arguments.
execBinary :: FilePath -> IO ()
execBinary path = do
  args   <- getArgs
  handle <- spawnProcess path (drop 1 args)
  _      <- waitForProcess handle
  return ()

delay_ :: Int -> IO a -> IO ()
delay_ timeout action = do
  threadDelay timeout
  action
  return ()

delayOutput :: MVar Bool -> Handle -> Handle -> IO ()
delayOutput flag source sink = do
  shouldOutput <- readMVar flag
  if shouldOutput
    then hGetContents source >>= hPutStr sink
    else return ()

