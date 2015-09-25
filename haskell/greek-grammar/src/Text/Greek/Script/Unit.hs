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
import Text.Greek.Parse.Utility
import Text.Greek.Utility
import Text.Parsec.Combinator
import Text.Parsec.Error (ParseError)
import Text.Parsec.Prim
import qualified Data.Set as S
import qualified Data.Text as T

newtype LetterChar = LetterChar { getLetterChar :: Char } deriving (Eq, Show, Ord)
newtype MarkChar = MarkChar { getMarkChar :: Char } deriving (Eq, Show, Ord)

data Unit l m = Unit
  { _unitItem :: l
  , _unitMarks :: m
  } deriving (Eq, Ord, Show)
makeLenses ''Unit

type UnitLetter l m = Unit (l, FileCharReference) m
type UnitMarkList l m = UnitLetter l [(m, FileCharReference)]
type UnitChar = UnitMarkList LetterChar MarkChar

data UnitError
  = UnitErrorMultipleLines FileReference Text
  | UnitErrorMismatchLength FileReference Text
  | UnitErrorParse ParseError
  deriving Show

toUnitChar :: (Text, FileReference) -> Either UnitError [UnitChar]
toUnitChar (t, r) = decomposeText t r >>= (over _Left UnitErrorParse . parse unitsParser "")

getMarks :: UnitMarkList l m -> [m]
getMarks = fmap (view _1) . view unitMarks

getLetter :: UnitMarkList l m -> l
getLetter = view (unitItem . _1)

getMarkLetterPairs :: UnitMarkList l m -> [(m, l)]
getMarkLetterPairs (Unit (l, _) m) = fmap (flip (,) l) (fmap fst m)

getLetterMarkSet :: Ord m => UnitMarkList l m -> (l, Set m)
getLetterMarkSet (Unit (l, _) m) = (l, S.fromList . fmap fst $ m)

getMarkSet :: Ord m => UnitMarkList l m -> Set m
getMarkSet (Unit _ m) = S.fromList . fmap fst $ m

type CharPair = (Char, FileCharReference)
type CharPairParser = ParsecT [(Char, FileCharReference)] () Identity

satisfy :: (Char -> Bool) -> CharPairParser CharPair
satisfy f = primBool (^. _2 . fileCharReferenceLine) (f . view _1)

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
