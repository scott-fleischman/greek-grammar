module Text.Greek.Script.Raw where

import Prelude hiding (Word)
--import Data.Unicode.DecomposeChar
import Data.Set (Set)
import Text.Greek.Source.All

data Unit = Unit { rawLetter :: Char, rawMarks :: Set Char }

units :: [Word] -> [Unit]
units = undefined
