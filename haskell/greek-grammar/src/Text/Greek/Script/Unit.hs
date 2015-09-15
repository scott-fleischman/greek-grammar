{-# LANGUAGE FlexibleContexts #-}

module Text.Greek.Script.Unit where

import Prelude hiding (Word, getLine)
import Control.Lens
import Data.Char
import Data.Map (Map)
import Data.Text (Text)
import Data.Unicode.DecomposeChar
import Text.Greek.FileReference
import Text.Greek.Utility
import Text.Parsec.Combinator
import Text.Parsec.Error (ParseError)
import Text.Parsec.Prim
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Text.Parsec.Pos as P

data Unit = Unit
  { unitLetter :: Char
  , unitReference :: FileCharReference
  , unitMarks :: Map Char FileCharReference
  } deriving Show

data UnitError
  = UnitErrorMultipleLines FileReference Text
  | UnitErrorMismatchLength FileReference Text
  | UnitErrorParse ParseError
  deriving Show

type CharPair = (Char, FileCharReference)
type CharPairParser = ParsecT [(Char, FileCharReference)] () Identity

toUnits :: Text -> FileReference -> Either UnitError [Unit]
toUnits t r = decomposeText t r >>= (over _Left UnitErrorParse . parse unitsParser "")

parseCharPair :: Stream s m CharPair => (CharPair -> Maybe a) -> ParsecT s u m a
parseCharPair = tokenPrim show updateEventPos

updateEventPos :: P.SourcePos -> (t, FileCharReference) -> s -> P.SourcePos
updateEventPos p (_, r) _ = flip P.setSourceColumn column . flip P.setSourceLine line $ p
  where
    pos = fileCharReferenceLine r
    line = getLine . lineReferenceLine $ pos
    column = getColumn . lineReferenceColumn $ pos

satisfy :: (Char -> Bool) -> CharPairParser CharPair
satisfy f = parseCharPair go
  where
    go a@(c, _) | f c = Just a
    go _ = Nothing

markParser :: CharPairParser CharPair
markParser = satisfy isMark

nonMarkParser :: CharPairParser CharPair
nonMarkParser = satisfy (not . isMark)

unitParser :: CharPairParser Unit
unitParser = do
  (l, r) <- nonMarkParser
  marks <- many markParser
  return $ Unit l r (M.fromList marks)

unitsParser :: CharPairParser [Unit]
unitsParser = many1 unitParser

decomposeText :: Text -> FileReference -> Either UnitError [CharPair]
decomposeText t r = do
  cs <- ensureConsistent t r
  return $ concatMap (\(a,b) -> fmap (flip (,) b) a) . over (traverse . _1) decomposeChar $ cs

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
