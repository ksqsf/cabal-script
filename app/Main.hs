module Main where

import qualified Data.ByteString.Lazy.UTF8 as BS
import Data.Digest.Pure.SHA
import Data.List
import System.Directory
import System.Environment
import System.Exit
import System.IO
import System.Process

main :: IO ()
main = do
  args <- getArgs
  runFile (head args)

runFile :: FilePath -> IO ()
runFile file = do
  contents   <- readFile file
  let hash = showDigest (sha1 (BS.fromString contents))
  rootPath   <- createProject hash contents
  binaryPath <- buildProject rootPath
  _ <- execBinary binaryPath
  return ()

createProject :: String -> String -> IO FilePath
createProject hash contents = do
  let rootPath = "/tmp/cabal-script-" <> hash
      deps     = extractDeps contents
  createDirectoryIfMissing True (rootPath <> "/app")
  writeCabalFile (rootPath <> "/project.cabal") hash deps
  writeSourceFile (rootPath <> "/app/Main.hs") contents
  return rootPath

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

writeSourceFile :: FilePath -> String -> IO ()
writeSourceFile = writeFile

buildProject :: FilePath -> IO FilePath
buildProject rootPath = do
  (_, _, _, handle) <- createProcess
    (proc "cabal" ["build", "-O"]) { cwd = Just rootPath
                                   , std_out = Inherit
                                   , std_err = Inherit }
  exitCode <- waitForProcess handle
  case exitCode of
    ExitFailure _ -> error "Build failed"
    ExitSuccess -> do
      (_, Just hout, _, _) <- createProcess
        (proc "cabal" ["list-bin", "main", "-O"]) { cwd = Just rootPath
                                                  , std_out = CreatePipe }
      binaryPath' <- hGetContents hout
      let binaryPath = reverse (drop 1 (reverse binaryPath'))
      return binaryPath

execBinary :: FilePath -> IO ()
execBinary path = do
  args <- getArgs
  handle <- spawnProcess path (drop 1 args)
  _ <- waitForProcess handle
  return ()
