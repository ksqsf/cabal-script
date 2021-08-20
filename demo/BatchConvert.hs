#!/usr/bin/env cabal-script
-- encoding >= 0.8.5 && < 0.9

-- Convert files in GB18030 to UTF-8, in-place

import Control.Monad
import System.Environment
import System.IO
import Data.Encoding.GB18030
import Data.Encoding.UTF8

main = do
  args <- getArgs
  forM_ args $ \f -> withFile f ReadWriteMode $ \h -> do
    input <- hGetContents h
    let decoded = decodeString GB18030 input
        encoded = encodeString UTF8 decoded
    hSetFileSize h 0
    hPutStr h encoded
