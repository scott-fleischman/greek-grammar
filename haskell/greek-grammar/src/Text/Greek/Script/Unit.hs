{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Text.Greek.Script.Unit where

import Prelude hiding (Word, getLine)
import Control.Lens
import Data.Char
import Data.Set (Set)
import Data.Text (Text)
import Data.Unicode.DecomposeChar
import Text.Greek.FileReference
import Text.Greek.Utility
import Text.Parsec.Combinator
import Text.Parsec.Error (ParseError)
import Text.Parsec.Prim
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Text.Parsec.Pos as P

newtype LetterChar = LetterChar { getLetterChar :: Char } deriving (Eq, Show, Ord)
newtype MarkChar = MarkChar { getMarkChar :: Char } deriving (Eq, Show, Ord)

type UnitChar = Unit LetterChar MarkChar
data Unit l m = Unit
  { _unitLetter :: (l, FileCharReference)
  , _unitMarks :: [(m, FileCharReference)]
  } deriving (Eq, Ord, Show)
makeLenses ''Unit

data UnitError
  = UnitErrorMultipleLines FileReference Text
  | UnitErrorMismatchLength FileReference Text
  | UnitErrorParse ParseError
  deriving Show

toUnitChar :: (Text, FileReference) -> Either UnitError [UnitChar]
toUnitChar (t, r) = decomposeText t r >>= (over _Left UnitErrorParse . parse unitsParser "")

getMarks :: Unit l m -> [m]
getMarks = fmap (view _1) . view unitMarks

getLetter :: Unit l m -> l
getLetter = view (unitLetter . _1)

getMarkLetterPairs :: Unit l m -> [(m, l)]
getMarkLetterPairs (Unit (l, _) m) = fmap (flip (,) l) (fmap fst m)

getLetterMarkSet :: Ord m => Unit l m -> (l, Set m)
getLetterMarkSet (Unit (l, _) m) = (l, S.fromList . fmap fst $ m)

type CharPair = (Char, FileCharReference)
type CharPairParser = ParsecT [(Char, FileCharReference)] () Identity

parseCharPair :: Stream s m CharPair => (CharPair -> Maybe a) -> ParsecT s u m a
parseCharPair = tokenPrim show updateEventPos

updateEventPos :: P.SourcePos -> (t, FileCharReference) -> s -> P.SourcePos
updateEventPos p (_, r) _ = flip P.setSourceColumn column . flip P.setSourceLine line $ p
  where
    line   = r ^. fileCharReferenceLine . lineReferenceLine   . getLine
    column = r ^. fileCharReferenceLine . lineReferenceColumn . getColumn

satisfy :: (Char -> Bool) -> CharPairParser CharPair
satisfy f = parseCharPair go
  where
    go a@(c, _) | f c = Just a
    go _ = Nothing

markParser :: CharPairParser (MarkChar, FileCharReference)
markParser = over _1 MarkChar <$> satisfy isMark

letterParser :: CharPairParser (LetterChar, FileCharReference)
letterParser = over _1 LetterChar <$> satisfy isLetter

unitParser :: CharPairParser UnitChar
unitParser = Unit <$> letterParser <*> many markParser

unitsParser :: CharPairParser [UnitChar]
unitsParser = many1 unitParser <* eof

decomposeText :: Text -> FileReference -> Either UnitError [CharPair]
decomposeText t r = do
  cs <- ensureConsistent t r
  return $ concatMap (_1 id) . over (traverse . _1) decomposeChar $ cs

ensureConsistent :: Text -> FileReference -> Either UnitError [CharPair]
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
