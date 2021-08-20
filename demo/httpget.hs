-- http-conduit, bytestring
import qualified Data.ByteString.Lazy.Char8 as B
import Network.HTTP.Simple
import System.Environment

main = do
  args <- getArgs
  resp <- httpLBS =<< parseRequest (head args)
  B.putStr (getResponseBody resp)
