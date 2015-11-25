{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Text.Greek.Source.Morphgnt where

import Prelude hiding (Word)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import System.FilePath ((</>))
import Text.Parsec.Prim ((<|>))
import qualified Control.Lens as Lens
import qualified Control.Monad.Except as Except
import qualified Data.Functor.Identity as Functor
import qualified Data.List as List
import qualified System.Directory as Directory
import qualified Text.Greek.IO.Paths as Paths
import qualified Text.Parsec.Char as Parsec
import qualified Text.Parsec as Parsec

data Morphgnt = Morphgnt
  { morphgntBooks :: [Book]
  } deriving (Eq, Ord, Show, Generic)
instance ToJSON Morphgnt
instance FromJSON Morphgnt

data Book = Book { bookWords :: [Word] } deriving (Eq, Ord, Show, Generic)
instance ToJSON Book
instance FromJSON Book

newtype BookNumber = BookNumber Int deriving (Eq, Ord, Show, Generic)
instance ToJSON BookNumber
instance FromJSON BookNumber
newtype ChapterNumber = ChapterNumber Int deriving (Eq, Ord, Show, Generic)
instance ToJSON ChapterNumber
instance FromJSON ChapterNumber
newtype VerseNumber = VerseNumber Int deriving (Eq, Ord, Show, Generic)
instance ToJSON VerseNumber
instance FromJSON VerseNumber
newtype PartOfSpeech1 = PartOfSpeech1 Char deriving (Eq, Ord, Show, Generic)
instance ToJSON PartOfSpeech1
instance FromJSON PartOfSpeech1
newtype PartOfSpeech2 = PartOfSpeech2 Char deriving (Eq, Ord, Show, Generic)
instance ToJSON PartOfSpeech2
instance FromJSON PartOfSpeech2
newtype Person = Person Char deriving (Eq, Ord, Show, Generic)
instance ToJSON Person
instance FromJSON Person
newtype Tense = Tense Char deriving (Eq, Ord, Show, Generic)
instance ToJSON Tense
instance FromJSON Tense
newtype Voice = Voice Char deriving (Eq, Ord, Show, Generic)
instance ToJSON Voice
instance FromJSON Voice
newtype Mood = Mood Char deriving (Eq, Ord, Show, Generic)
instance ToJSON Mood
instance FromJSON Mood
newtype Case = Case Char deriving (Eq, Ord, Show, Generic)
instance ToJSON Case
instance FromJSON Case
newtype Number = Number Char deriving (Eq, Ord, Show, Generic)
instance ToJSON Number
instance FromJSON Number
newtype Gender = Gender Char deriving (Eq, Ord, Show, Generic)
instance ToJSON Gender
instance FromJSON Gender
newtype Degree = Degree Char deriving (Eq, Ord, Show, Generic)
instance ToJSON Degree
instance FromJSON Degree
newtype TextWithPunctuation = TextWithPunctuation [Char] deriving (Eq, Ord, Show, Generic)
instance ToJSON TextWithPunctuation
instance FromJSON TextWithPunctuation
newtype WordNoPunctuation = WordNoPunctuation { getWordNoPunctuation :: [Char] } deriving (Eq, Ord, Show, Generic)
instance ToJSON WordNoPunctuation
instance FromJSON WordNoPunctuation
newtype WordNormalized = WordNormalized [Char] deriving (Eq, Ord, Show, Generic)
instance ToJSON WordNormalized
instance FromJSON WordNormalized
newtype Lemma = Lemma [Char] deriving (Eq, Ord, Show, Generic)
instance ToJSON Lemma
instance FromJSON Lemma

data Word = Word
  { _wordBookNumber :: BookNumber
  , _wordChapterNumber :: ChapterNumber
  , _wordVerseNumber :: VerseNumber
  , _wordPartOfSpeech1 :: PartOfSpeech1
  , _wordPartOfSpeech2 :: Maybe PartOfSpeech2
  , _wordPerson :: Maybe Person
  , _wordTense :: Maybe Tense
  , _wordVoice :: Maybe Voice
  , _wordMood :: Maybe Mood
  , _wordCase :: Maybe Case
  , _wordNumber :: Maybe Number
  , _wordGender :: Maybe Gender
  , _wordDegree :: Maybe Degree
  , _wordTextWithPunctuation :: TextWithPunctuation
  , _wordWordNoPunctuation :: WordNoPunctuation
  , _wordWordNormalized :: WordNormalized
  , _wordLemma :: Lemma
  } deriving (Eq, Ord, Show, Generic)
Lens.makeLenses ''Word
instance ToJSON Word
instance FromJSON Word

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
  <$> (fmap BookNumber twoDigitParser)
  <*> (fmap ChapterNumber twoDigitParser)
  <*> (fmap VerseNumber twoDigitParser <* Parsec.space)
  <*> (fmap PartOfSpeech1 Parsec.upper)
  <*> ((fmap . fmap) PartOfSpeech2 optionalUpper <* Parsec.space)
  <*> ((fmap . fmap) Person optionalDigit)
  <*> ((fmap . fmap) Tense optionalUpper)
  <*> ((fmap . fmap) Voice optionalUpper)
  <*> ((fmap . fmap) Mood optionalUpper)
  <*> ((fmap . fmap) Case optionalUpper)
  <*> ((fmap . fmap) Number optionalUpper)
  <*> ((fmap . fmap) Gender optionalUpper)
  <*> ((fmap . fmap) Degree optionalUpper <* Parsec.space)
  <*> (fmap TextWithPunctuation wordTextParser <* Parsec.space)
  <*> (fmap WordNoPunctuation wordTextParser <* Parsec.space)
  <*> (fmap WordNormalized wordTextParser <* Parsec.space)
  <*> (fmap Lemma wordTextParser)

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
