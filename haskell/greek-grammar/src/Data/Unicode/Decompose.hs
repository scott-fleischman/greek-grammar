module Data.Unicode.Decompose where

import Data.Unicode.DecomposeChar

decompose :: String -> String
decompose = concatMap decomposeChar
