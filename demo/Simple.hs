#!/usr/bin/env cabal-script
-- async >= 2.2.3 && < 2.3
-- text
{-# LANGUAGE OverloadedStrings #-}
import Control.Concurrent.Async
import qualified Data.Text.IO as T
import System.Environment
import Control.Monad

main :: IO ()
main = do
  concurrently_ (T.putStrLn "cabal") (T.putStrLn "script")
  getArgs >>= (putStrLn . show)
