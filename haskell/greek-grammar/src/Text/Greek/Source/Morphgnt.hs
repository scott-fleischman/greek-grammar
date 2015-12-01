{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

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

newtype BookNumber = BookNumber Int deriving (Eq, Ord, Show, Generic)
instance ToJSON BookNumber
instance FromJSON BookNumber
newtype ChapterNumber = ChapterNumber Int deriving (Eq, Ord, Show, Generic)
instance ToJSON ChapterNumber
instance FromJSON ChapterNumber
newtype VerseNumber = VerseNumber Int deriving (Eq, Ord, Show, Generic)
instance ToJSON VerseNumber
instance FromJSON VerseNumber
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

type CharParser = Parsec.ParsecT [Char] () Functor.Identity


data PartOfSpeech
  = Adjective
  | Conjunction
  | Adverb
  | Interjection
  | Noun
  | Preposition
  | DefiniteArticle
  | DemonstrativePronoun
  | InterrogativeIndefinitePronoun
  | PersonalPronoun
  | RelativePronoun
  | Verb
  | Particle
  deriving (Eq, Ord, Show, Generic)
instance ToJSON PartOfSpeech
instance FromJSON PartOfSpeech

partOfSpeechParser :: CharParser PartOfSpeech
partOfSpeechParser
  = Parsec.string "A-" *> return Adjective
  <|> Parsec.string "C-" *> return Conjunction
  <|> Parsec.string "D-" *> return Adverb
  <|> Parsec.string "I-" *> return Interjection
  <|> Parsec.string "N-" *> return Noun
  <|> Parsec.string "P-" *> return Preposition
  <|> Parsec.string "V-" *> return Verb
  <|> Parsec.string "X-" *> return Particle
  <|> Parsec.try (Parsec.string "RA" *> return DefiniteArticle)
  <|> Parsec.try (Parsec.string "RD" *> return DemonstrativePronoun)
  <|> Parsec.try (Parsec.string "RI" *> return InterrogativeIndefinitePronoun)
  <|> Parsec.try (Parsec.string "RP" *> return PersonalPronoun)
  <|> Parsec.string "RR" *> return RelativePronoun

data Person = Person1 | Person2 | Person3 deriving (Eq, Ord, Show, Generic)
instance ToJSON Person
instance FromJSON Person

personParser :: CharParser (Maybe Person)
personParser
  = Parsec.char '1' *> return (Just Person1)
  <|> Parsec.char '2' *> return (Just Person2)
  <|> Parsec.char '3' *> return (Just Person3)
  <|> Parsec.char '-' *> return Nothing

data Tense = Imperfect | Future | Pluperfect | Perfect | Present | Aorist deriving (Eq, Ord, Show, Generic)
instance ToJSON Tense
instance FromJSON Tense

tenseParser :: CharParser (Maybe Tense)
tenseParser
  = Parsec.char 'I' *> return (Just Imperfect)
  <|> Parsec.char 'F' *> return (Just Future)
  <|> Parsec.char 'Y' *> return (Just Pluperfect)
  <|> Parsec.char 'X' *> return (Just Perfect)
  <|> Parsec.char 'P' *> return (Just Present)
  <|> Parsec.char 'A' *> return (Just Aorist)
  <|> Parsec.char '-' *> return Nothing

data Voice = Active | Middle | Passive deriving (Eq, Ord, Show, Generic)
instance ToJSON Voice
instance FromJSON Voice

voiceParser :: CharParser (Maybe Voice)
voiceParser
  = Parsec.char 'M' *> return (Just Middle)
  <|> Parsec.char 'P' *> return (Just Passive)
  <|> Parsec.char 'A' *> return (Just Active)
  <|> Parsec.char '-' *> return Nothing

data Mood = Indicative | Infinitive | Optative | Imperative | Subjunctive | Participial deriving (Eq, Ord, Show, Generic)
instance ToJSON Mood
instance FromJSON Mood

moodParser :: CharParser (Maybe Mood)
moodParser
  = Parsec.char 'I' *> return (Just Indicative)
  <|> Parsec.char 'N' *> return (Just Infinitive)
  <|> Parsec.char 'O' *> return (Just Optative)
  <|> Parsec.char 'D' *> return (Just Imperative)
  <|> Parsec.char 'S' *> return (Just Subjunctive)
  <|> Parsec.char 'P' *> return (Just Participial)
  <|> Parsec.char '-' *> return Nothing

data Case = Nominative | Genitive | Dative | Vocative | Accusative deriving (Eq, Ord, Show, Generic)
instance ToJSON Case
instance FromJSON Case

caseParser :: CharParser (Maybe Case)
caseParser
  = Parsec.char 'N' *> return (Just Nominative)
  <|> Parsec.char 'G' *> return (Just Genitive)
  <|> Parsec.char 'D' *> return (Just Dative)
  <|> Parsec.char 'V' *> return (Just Vocative)
  <|> Parsec.char 'A' *> return (Just Accusative)
  <|> Parsec.char '-' *> return Nothing

data Number = Singular | Plural deriving (Eq, Ord, Show, Generic)
instance ToJSON Number
instance FromJSON Number

numberParser :: CharParser (Maybe Number)
numberParser
  = Parsec.char 'S' *> return (Just Singular)
  <|> Parsec.char 'P' *> return (Just Plural)
  <|> Parsec.char '-' *> return Nothing

data Gender = Masculine | Feminine | Neuter deriving (Eq, Ord, Show, Generic)
instance ToJSON Gender
instance FromJSON Gender

genderParser :: CharParser (Maybe Gender)
genderParser
  = Parsec.char 'M' *> return (Just Masculine)
  <|> Parsec.char 'F' *> return (Just Feminine)
  <|> Parsec.char 'N' *> return (Just Neuter)
  <|> Parsec.char '-' *> return Nothing

data Degree = Comparative | Superlative deriving (Eq, Ord, Show, Generic)
instance ToJSON Degree
instance FromJSON Degree

degreeParser :: CharParser (Maybe Degree)
degreeParser
  = Parsec.char 'C' *> return (Just Comparative)
  <|> Parsec.char 'S' *> return (Just Superlative)
  <|> Parsec.char '-' *> return Nothing

data Word = Word
  { _wordBookNumber :: BookNumber
  , _wordChapterNumber :: ChapterNumber
  , _wordVerseNumber :: VerseNumber
  , _wordPartOfSpeech :: PartOfSpeech
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

data Book = Book { bookWords :: [Word] } deriving (Eq, Ord, Show, Generic)
instance ToJSON Book
instance FromJSON Book

data Morphgnt = Morphgnt
  { morphgntBooks :: [Book]
  } deriving (Eq, Ord, Show, Generic)
instance ToJSON Morphgnt
instance FromJSON Morphgnt


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

bookParser :: CharParser Book
bookParser = fmap Book $ Parsec.many1 $ wordParser <* Parsec.endOfLine

wordParser :: CharParser Word
wordParser = Word
  <$> (fmap BookNumber twoDigitParser)
  <*> (fmap ChapterNumber twoDigitParser)
  <*> (fmap VerseNumber twoDigitParser <* Parsec.space)
  <*> (partOfSpeechParser <* Parsec.space)
  <*> personParser
  <*> tenseParser
  <*> voiceParser
  <*> moodParser
  <*> caseParser
  <*> numberParser
  <*> genderParser
  <*> (degreeParser <* Parsec.space)
  <*> (fmap TextWithPunctuation wordTextParser <* Parsec.space)
  <*> (fmap WordNoPunctuation wordTextParser <* Parsec.space)
  <*> (fmap WordNormalized wordTextParser <* Parsec.space)
  <*> (fmap Lemma wordTextParser)

twoDigitParser :: CharParser Int
twoDigitParser = do
  d1 <- Parsec.digit
  d2 <- Parsec.digit
  return $ read [d1, d2]

wordTextParser :: CharParser [Char]
wordTextParser = Parsec.many1 (Parsec.noneOf " \n\r")

data Parse a = Parse (a Person) (a Tense) (a Voice) (a Mood) (a Case) (a Number) (a Gender) (a Degree)
deriving instance Eq (Parse Maybe)
deriving instance Ord (Parse Maybe)
deriving instance Show (Parse Maybe)

expandWord :: [Word] -> [Parse Maybe]
expandWord =
  fmap
    (\w -> Parse
     (_wordPerson w)
     (_wordTense w)
     (_wordVoice w)
     (_wordMood w)
     (_wordCase w)
     (_wordNumber w)
     (_wordGender w)
     (_wordDegree w)
    )
