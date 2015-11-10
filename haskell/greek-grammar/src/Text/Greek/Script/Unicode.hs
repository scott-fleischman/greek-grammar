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
newtype Letter = Letter { getLetter :: Char } deriving (Eq, Ord, Show)
newtype Mark = Mark { getMark :: Char } deriving (Eq, Ord, Show)

data Error
  = ErrorMultipleLines FileReference Text
  | ErrorMismatchLength FileReference Text
  | ErrorParse ParseError
  deriving Show

decompose :: [(Composed, FileCharReference)] -> [(Decomposed, FileCharReference)]
decompose cs = over (traverse . _1) Decomposed . concatMap (_1 id) . over (traverse . _1) (decomposeChar . composed) $ cs

decompose' :: Composed -> [Decomposed]
decompose' = fmap Decomposed . decomposeChar . composed

toComposed :: Text -> [Composed]
toComposed = fmap Composed . Text.unpack

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


parseMarkedLetters :: [Decomposed] -> Either Error [Marked.Unit Letter [Mark]]
parseMarkedLetters = over _Left ErrorParse . parse markedLettersParser ""

type DecomposedParser = ParsecT [Decomposed] () Identity

satisfy :: String -> (Char -> Bool) -> DecomposedParser Decomposed
satisfy p f = primBool' p (f . decomposed)

markParser :: DecomposedParser Mark
markParser = (Mark . decomposed) <$> satisfy "Mark" Char.isMark

letterParser :: DecomposedParser Letter
letterParser = (Letter . decomposed) <$> satisfy "Letter" Char.isLetter

markedLetterParser :: DecomposedParser (Marked.Unit Letter [Mark])
markedLetterParser = Marked.Unit <$> letterParser <*> many markParser

markedLettersParser :: DecomposedParser [Marked.Unit Letter [Mark]]
markedLettersParser = many1 markedLetterParser <* eof
