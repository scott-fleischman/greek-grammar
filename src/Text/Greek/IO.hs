{-# LANGUAGE OverloadedStrings #-}

module Text.Greek.IO where

import Prelude hiding (putStrLn)
import Data.Text.IO
import Text.Greek.Phonology.Phoneme
import Text.Greek.Phonology.ShowText

dumpWord :: [Phoneme] -> IO ()
dumpWord = putStrLn . showWord
