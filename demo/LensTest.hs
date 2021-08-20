#!/usr/bin/env cabal-script
-- lens
{-# LANGUAGE TemplateHaskell #-}

import Control.Lens
import Numeric.Lens

data Person = Person
  { _personName :: String
  , _personAge :: Int
  }
  deriving (Show)

makeLenses ''Person

main = do
  let john = Person "John" 23
      jerry = Person "Jerry" 233
  print $ john^.personName
  print $ sumOf (folded . beside negated id) [Left 1, Right 10, Left 2, Right 20]
