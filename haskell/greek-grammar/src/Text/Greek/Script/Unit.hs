module Text.Greek.Script.Unit where

import Prelude hiding (Word)
import Control.Lens
import Data.Unicode.DecomposeChar
import Data.Map (Map)
import Data.Text (Text)
import Text.Greek.FileReference
import Text.Greek.Utility
import qualified Data.Text as T

data Unit = Unit
  { unitLetter :: Char
  , unitReference :: FileCharReference
  , unitMarks :: Map Char FileCharReference
  }

data UnitError
  = UnitErrorMultipleLines FileReference Text
  | UnitErrorMismatchLength FileReference Text
  deriving Show

decomposeText :: Text -> FileReference -> Either UnitError [(Char, FileCharReference)]
decomposeText t r = do
  cs <- ensureConsistent t r
  return $ concatMap (\(a,b) -> fmap (flip (,) b) a) . over (traverse . _1) decomposeChar $ cs

ensureConsistent :: Text -> FileReference -> Either UnitError [(Char, FileCharReference)]
ensureConsistent t r = do
  (p, l, c1, c2) <- onErr UnitErrorMultipleLines $ ensureSingleLine r
  cs <- onErr UnitErrorMismatchLength $ ensureLengthMatches t c1 c2
  return $ fmap (_2 %~ FileCharReference p . LineReference l) cs
    where onErr e = maybeToEither (e r t)

ensureSingleLine :: FileReference -> Maybe (Path, Line, Column, Column)
ensureSingleLine (FileReference p (LineReference l1 c1) (LineReference l2 c2)) | l1 == l2 = Just (p, l1, c1, c2)
ensureSingleLine _ = Nothing

ensureLengthMatches :: Text -> Column -> Column -> Maybe [(Char, Column)]
ensureLengthMatches t (Column c1) (Column c2) | T.length t == c2 - c1 = Just (zipWith (,) (T.unpack t) (fmap (Column . (+c1)) [0..]))
ensureLengthMatches _ _ _ = Nothing
