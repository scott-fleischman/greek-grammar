module Text.Greek.Script.Raw where

import Prelude hiding (Word)
--import Data.Unicode.DecomposeChar
import Data.Set (Set)
import Text.Greek.FileReference
--import Text.Greek.Source.All

data Unit = Unit { rawLetter :: Char, rawMarks :: Set Char }

ensureSingleLine :: FileReference -> Maybe (Path, Line, Column, Column)
ensureSingleLine (FileReference p (LineReference l1 c1) (LineReference l2 c2)) | l1 == l2 = Just (p, l1, c1, c2)
ensureSingleLine _ = Nothing

