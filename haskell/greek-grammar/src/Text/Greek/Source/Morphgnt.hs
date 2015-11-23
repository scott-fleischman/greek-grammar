module Text.Greek.Source.Morphgnt where

import Prelude hiding (Word)
import System.FilePath ((</>))
import Text.Parsec.Prim ((<|>))
import qualified Control.Monad.Except as Except
import qualified Data.Functor.Identity as Functor
import qualified Data.List as List
import qualified System.Directory as Directory
import qualified Text.Greek.IO.Paths as Paths
import qualified Text.Parsec.Char as Parsec
import qualified Text.Parsec as Parsec

data Morphgnt = Morphgnt
  { morphgntBooks :: [Book]
  } deriving (Eq, Ord, Show)

data Book = Book [Word] deriving (Eq, Ord, Show)
data Word = Word
  { wordBookNumber :: Int
  , wordChapterNumber :: Int
  , wordVerseNumber :: Int
  , wordPartOfSpeech1 :: Char
  , wordPartOfSpeech2 :: Maybe Char
  , wordPerson :: Maybe Char
  , wordTense :: Maybe Char
  , wordVoice :: Maybe Char
  , wordMood :: Maybe Char
  , wordCase :: Maybe Char
  , wordNumber :: Maybe Char
  , wordGender :: Maybe Char
  , wordDegree :: Maybe Char
  , wordSurface :: [Char]
  , wordStrippedPunctuation :: [Char]
  , wordNormalized :: [Char]
  , wordLemma :: [Char]
  } deriving (Eq, Ord, Show)

load :: Except.ExceptT String IO Morphgnt
load = do
  allFilePaths <- Except.liftIO $ Directory.getDirectoryContents Paths.morphgntSblgnt
  let morphgntPaths = filter (List.isSuffixOf ".txt") allFilePaths
  books <- mapM (\p -> loadFile (Paths.morphgntSblgnt </> p)) morphgntPaths
  return $ Morphgnt books

loadFile :: FilePath -> Except.ExceptT String IO Book
loadFile p = do
  content <- Except.liftIO $ readFile p
  let parsed = Parsec.parse bookParser p content
  case parsed of
    Left e -> Except.throwError . show $ e
    Right b -> return b

type CharParser = Parsec.ParsecT [Char] () Functor.Identity

bookParser :: CharParser Book
bookParser = fmap Book $ Parsec.many1 $ wordParser <* Parsec.endOfLine

wordParser :: CharParser Word
wordParser = Word
  <$> twoDigitParser
  <*> twoDigitParser
  <*> (twoDigitParser <* Parsec.space)
  <*> Parsec.upper
  <*> (optionalUpper <* Parsec.space)
  <*> optionalDigit
  <*> optionalUpper
  <*> optionalUpper
  <*> optionalUpper
  <*> optionalUpper
  <*> optionalUpper
  <*> optionalUpper
  <*> (optionalUpper <* Parsec.space)
  <*> (wordTextParser <* Parsec.space)
  <*> (wordTextParser <* Parsec.space)
  <*> (wordTextParser <* Parsec.space)
  <*> wordTextParser

twoDigitParser :: CharParser Int
twoDigitParser = do
  d1 <- Parsec.digit
  d2 <- Parsec.digit
  return $ read [d1, d2]

optionalDigit :: CharParser (Maybe Char)
optionalDigit = (fmap Just Parsec.digit) <|> (fmap (const Nothing) (Parsec.char '-'))

optionalUpper :: CharParser (Maybe Char)
optionalUpper = (fmap Just Parsec.upper) <|> (fmap (const Nothing) (Parsec.char '-'))

wordTextParser :: CharParser [Char]
wordTextParser = Parsec.many1 (Parsec.noneOf " \n\r")
