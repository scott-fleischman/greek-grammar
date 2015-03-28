{-# LANGUAGE TemplateHaskell #-}

module Main where

import Prelude hiding (readFile)
import Data.Char
import Data.Default (def)
import Data.Either
import Numeric
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.TH
import Test.HUnit
import Text.Greek.Corpus.Bible
import Text.Greek.NewTestament.SBL
import Text.Greek.Paths
import Text.Greek.Script
import Text.Greek.Script.Unicode
import Text.XML (readFile)

case_valid_tokens = do
  mapM_ (\p -> assertEqual (showString "'\\x" . showHex (ord . fst $ p) $ "'") [] (validateToken . snd $ p)) unicodeTokenPairs

case_load_sblgnt = do
  sblgnt <- readFile def sblgntOsisPath
  isRight (loadOsis sblgnt) @?= True

main :: IO ()
main = $(defaultMainGenerator)
