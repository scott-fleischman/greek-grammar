module Data.Unicode.Decompose where

import Data.Unicode.DecomposeChar

decompose :: Foldable t => t Char -> String
decompose = concatMap decomposeChar
