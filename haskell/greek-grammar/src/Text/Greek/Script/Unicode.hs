{-# LANGUAGE TemplateHaskell #-}

module Text.Greek.Script.Unicode where

import Control.Lens
import Data.Text (Text)
import Data.Unicode.DecomposeChar
import Text.Greek.FileReference
import Text.Greek.Parse.Utility
import Text.Greek.Utility
import Text.Parsec.Combinator
import Text.Parsec.Error (ParseError)
import Text.Parsec.Prim
import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Text.Greek.Script.Marked as Marked

newtype Composed = Composed { composed :: Char } deriving (Eq, Ord, Show)
newtype Decomposed = Decomposed { decomposed :: Char } deriving (Eq, Ord, Show)
newtype Letter = Letter Char deriving (Eq, Ord, Show)
newtype Mark = Mark Char deriving (Eq, Ord, Show)

data Error
  = ErrorMultipleLines FileReference Text
  | ErrorMismatchLength FileReference Text
  | ErrorParse ParseError
  deriving Show

decompose :: [(Composed, FileCharReference)] -> [(Decomposed, FileCharReference)]
decompose cs = over (traverse . _1) Decomposed . concatMap (_1 id) . over (traverse . _1) (decomposeChar . composed) $ cs

splitText :: Text -> FileReference -> Either Error [(Composed, FileCharReference)]
splitText t r = ensureConsistent t r & _Right . each . _1 %~ Composed

ensureConsistent :: Text -> FileReference -> Either Error [(Char, FileCharReference)]
ensureConsistent t r = do
  (p, l, c1, c2) <- onErr ErrorMultipleLines $ ensureSingleLine r
  cs <- onErr ErrorMismatchLength $ ensureLengthMatches t c1 c2
  return $ fmap (_2 %~ FileCharReference p . LineReference l) cs
    where onErr e = maybeToEither (e r t)

ensureSingleLine :: FileReference -> Maybe (Path, Line, Column, Column)
ensureSingleLine (FileReference p (LineReference l1 c1) (LineReference l2 c2)) | l1 == l2 = Just (p, l1, c1, c2)
ensureSingleLine _ = Nothing

ensureLengthMatches :: Text -> Column -> Column -> Maybe [(Char, Column)]
ensureLengthMatches t (Column c1) (Column c2) | Text.length t == c2 - c1 = Just (zipWith (,) (Text.unpack t) (fmap (Column . (+c1)) [0..]))
ensureLengthMatches _ _ _ = Nothing


type MarkedLetter = Marked.MarkList FileCharReference Letter Mark

parseMarkedLetters :: [(Decomposed, FileCharReference)] -> Either Error [MarkedLetter]
parseMarkedLetters = over _Left ErrorParse . parse markedLettersParser ""

type DecomposedParser = ParsecT [(Decomposed, FileCharReference)] () Identity

satisfy :: String -> (Char -> Bool) -> DecomposedParser (Decomposed, FileCharReference)
satisfy p f = primBool p (^. _2 . fileCharReferenceLine) (f . decomposed . view _1)

markParser :: DecomposedParser (Mark, FileCharReference)
markParser = over _1 (Mark . decomposed) <$> satisfy "Mark" Char.isMark

letterParser :: DecomposedParser (Letter, FileCharReference)
letterParser = over _1 (Letter . decomposed) <$> satisfy "Letter" Char.isLetter

markedLetterParser :: DecomposedParser MarkedLetter
markedLetterParser = Marked.Unit <$> letterParser <*> many markParser

markedLettersParser :: DecomposedParser [MarkedLetter]
markedLettersParser = many1 markedLetterParser <* eof
