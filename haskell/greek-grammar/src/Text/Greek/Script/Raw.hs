module Text.Greek.Script.Raw where

import Prelude hiding (Word)
import Control.Lens
--import Data.Unicode.DecomposeChar
import Data.Set (Set)
import Data.Text (Text)
import Text.Greek.FileReference
--import Text.Greek.Source.All
import Text.Greek.Utility
import qualified Data.Text as T

data Unit = Unit { rawLetter :: Char, rawMarks :: Set Char }

data RawError
  = RawErrorMultipleLines FileReference Text
  | RawErrorMismatchLength FileReference Text
  deriving Show

ensureText :: Text -> FileReference -> Either RawError [(Char, FileCharReference)]
ensureText t r = do
  (p, l, c1, c2) <- onErr RawErrorMultipleLines $ ensureSingleLine r
  cs <- onErr RawErrorMismatchLength $ ensureLengthMatches t c1 c2
  return $ fmap (_2 %~ FileCharReference p . LineReference l) cs
    where onErr e = maybeToEither (e r t)

ensureSingleLine :: FileReference -> Maybe (Path, Line, Column, Column)
ensureSingleLine (FileReference p (LineReference l1 c1) (LineReference l2 c2)) | l1 == l2 = Just (p, l1, c1, c2)
ensureSingleLine _ = Nothing

ensureLengthMatches :: Text -> Column -> Column -> Maybe [(Char, Column)]
ensureLengthMatches t (Column c1) (Column c2) | T.length t == c2 - c1 = Just (zipWith (,) (T.unpack t) (fmap (Column . (+c1)) [0..]))
ensureLengthMatches _ _ _ = Nothing
