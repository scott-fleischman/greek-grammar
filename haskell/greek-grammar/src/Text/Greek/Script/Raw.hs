module Text.Greek.Script.Raw where

import Prelude hiding (Word)
--import Data.Unicode.DecomposeChar
import Data.Set (Set)
import Data.Text (Text)
import Text.Greek.FileReference
--import Text.Greek.Source.All
import qualified Data.Text as T

data Unit = Unit { rawLetter :: Char, rawMarks :: Set Char }

ensureText :: Text -> FileReference -> Maybe Text
ensureText t r = do
  (_, _, c1, c2) <- ensureSingleLine r
  t' <- ensureLengthMatches t c1 c2
  return t'

ensureSingleLine :: FileReference -> Maybe (Path, Line, Column, Column)
ensureSingleLine (FileReference p (LineReference l1 c1) (LineReference l2 c2)) | l1 == l2 = Just (p, l1, c1, c2)
ensureSingleLine _ = Nothing

ensureLengthMatches :: Text -> Column -> Column -> Maybe Text--[(Char, Column)]
ensureLengthMatches t (Column c1) (Column c2) | T.length t == c2 - c1 = Just t
ensureLengthMatches _ _ _ = Nothing
