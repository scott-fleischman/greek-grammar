{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Text.Greek.Script.Word where

import Prelude hiding (Word)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import Text.Greek.Source.FileReference
import qualified Text.Greek.Script.Elision as Elision
import qualified Text.Greek.Script.Punctuation as Punctuation
import qualified Control.Lens as Lens
import qualified Data.Text as Text

data IsCapitalized = IsCapitalized | IsNotCapitalized deriving (Eq, Ord, Show, Generic)
instance ToJSON IsCapitalized
instance FromJSON IsCapitalized

data Crasis = HasCrasis | NoCrasis deriving (Eq, Ord, Show, Generic)
instance ToJSON Crasis
instance FromJSON Crasis

newtype ParagraphIndex = ParagraphIndex { getParagraphIndex :: Int } deriving (Eq, Ord, Show, Generic)
instance ToJSON ParagraphIndex
instance FromJSON ParagraphIndex

newtype Index = Index { getIndex :: Int } deriving (Eq, Ord, Show, Generic)
instance ToJSON Index
instance FromJSON Index

data Word i s = Word
  { getInfo :: i
  , getSurface :: s
  } deriving (Generic)
Lens.makeLensesFor
  [ ("getInfo", "info")
  , ("getSurface", "surface")
  ]
  ''Word
instance (ToJSON i, ToJSON s) => ToJSON (Word i s)
instance (FromJSON i, FromJSON s) => FromJSON (Word i s)

newtype Source = Source { getSource :: Text } deriving (Eq, Ord, Show, Generic)
instance ToJSON Source
instance FromJSON Source

data SourceInfo = SourceInfo
  { getSourceInfoWord :: Source
  , getSourceInfoFile :: FileReference
  } deriving (Eq, Ord, Show, Generic)
instance ToJSON SourceInfo
instance FromJSON SourceInfo

newtype LetterCount = LetterCount { getLetterCount :: Int } deriving (Eq, Show, Ord, Generic)
instance ToJSON LetterCount
instance FromJSON LetterCount

newtype MarkCount = MarkCount { getMarkCount :: Int } deriving (Eq, Show, Ord, Generic)
instance ToJSON MarkCount
instance FromJSON MarkCount

newtype VowelCount = VowelCount { getVowelCount :: Int } deriving (Eq, Show, Ord, Generic)
instance ToJSON VowelCount
instance FromJSON VowelCount

newtype ConsonantCount = ConsonantCount { getConsonantCount :: Int } deriving (Eq, Show, Ord, Generic)
instance ToJSON ConsonantCount
instance FromJSON ConsonantCount

newtype Prefix = Prefix { getPrefix :: Text } deriving (Eq, Show, Ord, Generic)
newtype Suffix = Suffix { getSuffix :: Text } deriving (Eq, Show, Ord, Generic)
Lens.makeLensesFor
  [ ("getSuffix", "suffix")
  ]
  ''Suffix

makePrefix :: Text -> Maybe Prefix
makePrefix = fmap Prefix . nothingIfEmpty . Text.strip

makeSuffix :: Text -> Maybe Suffix
makeSuffix = fmap Suffix . nothingIfEmpty . Text.strip

nothingIfEmpty :: Text -> Maybe Text
nothingIfEmpty x | Text.null x = Nothing
nothingIfEmpty x = Just x

type Affix = (Maybe Prefix, Maybe Suffix)

type Basic = (Affix, ParagraphIndex)
type Elision = (Affix, ParagraphIndex, Elision.Pair)
type Capital = (Affix, ParagraphIndex, Elision.Pair, IsCapitalized)
type WithCrasis = (Affix, ParagraphIndex, Elision.Pair, IsCapitalized, Crasis)
type Sentence = (Affix, ParagraphIndex, Elision.Pair, IsCapitalized, Crasis, Punctuation.SentencePair)
type Indexed a = Word (Index, a)

index :: [Word a s] -> [Indexed a s]
index = fmap addIndex . zip (fmap Index [0..])
  where
    addIndex (i, Word a s) = Word (i, a) s

addElisionPair :: Elision.Pair -> Indexed Basic a -> Indexed Elision a
addElisionPair e = Lens.over (info . Lens._2) (\(a, p) -> (a, p, e))

addCrasis :: Crasis -> Capital -> WithCrasis
addCrasis x (a,b,c,d) = (a,b,c,d,x)

addSentencePair :: Punctuation.SentencePair -> WithCrasis -> Sentence
addSentencePair x (a,b,c,d,e) = (a,b,c,d,e,x)
