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

data LastWord = IsLastWord | NotLastWord deriving (Eq, Ord, Show, Generic)
instance ToJSON LastWord
instance FromJSON LastWord

data Crasis = HasCrasis | NoCrasis deriving (Eq, Ord, Show, Generic)
instance ToJSON Crasis
instance FromJSON Crasis

data InitialEnclitic = IsEnclitic | NotEnclitic | UncertainEnclitic deriving (Eq, Ord, Show, Generic)
instance ToJSON InitialEnclitic
instance FromJSON InitialEnclitic

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

type IndexedP a = (Index, a)
type Indexed = IndexedP ()
indexLens :: Lens.Lens (Index, a) (b, a) Index b
indexLens = Lens._1

type BasicP a = IndexedP (Affix, ParagraphIndex, a)
type Basic = BasicP ()
basicLens :: Lens.Lens (IndexedP a) (IndexedP b) a b
basicLens = Lens._2
affixLens :: Lens.Lens (IndexedP (Affix, x, y)) (IndexedP (b, x, y)) Affix b
affixLens = basicLens . Lens._1
paragraphIndexLens :: Lens.Lens (IndexedP (x, ParagraphIndex, y)) (IndexedP (x, b, y)) ParagraphIndex b
paragraphIndexLens = basicLens . Lens._2

type ElisionP a = BasicP (Elision.Pair, a)
type Elision = ElisionP ()
elisionLens' :: Lens.Lens (BasicP a) (BasicP b) a b
elisionLens' = basicLens . Lens._3
elisionLens :: Lens.Lens (BasicP (a, x)) (BasicP (b, x)) a b
elisionLens = elisionLens' . Lens._1

type CapitalP a = ElisionP (IsCapitalized, a)
type Capital = CapitalP ()
capitalLens' :: Lens.Lens (ElisionP a) (ElisionP b) a b
capitalLens' = elisionLens' . Lens._2
capitalLens :: Lens.Lens (ElisionP (IsCapitalized, x)) (ElisionP (b, x)) IsCapitalized b
capitalLens = capitalLens' . Lens._1

type WithCrasisP a = CapitalP (Crasis, a)
type WithCrasis = WithCrasisP ()
crasisLens' :: Lens.Lens (CapitalP a) (CapitalP b) a b
crasisLens' = capitalLens' . Lens._2
crasisLens :: Lens.Lens (CapitalP (Crasis, x)) (CapitalP (b, x)) Crasis b
crasisLens = crasisLens' . Lens._1

type SentenceP a = WithCrasisP (Punctuation.SentencePair, a)
type Sentence = SentenceP ()
sentenceLens' :: Lens.Lens (WithCrasisP a) (WithCrasisP b) a b
sentenceLens' = crasisLens' . Lens._2
sentenceLens :: Lens.Lens (WithCrasisP (Punctuation.SentencePair, x)) (WithCrasisP (b, x)) Punctuation.SentencePair b
sentenceLens = sentenceLens' . Lens._1

type WithEncliticP a = SentenceP (InitialEnclitic, a)
type WithEnclitic = WithEncliticP ()
encliticLens' :: Lens.Lens (SentenceP a) (SentenceP b) a b
encliticLens' = sentenceLens' . Lens._2
encliticLens :: Lens.Lens (SentenceP (InitialEnclitic, x)) (SentenceP (b, x)) InitialEnclitic b
encliticLens = encliticLens' . Lens._1

index :: [Word a s] -> [Word (IndexedP a) s]
index = fmap addIndex . zip (fmap Index [0..])
  where
    addIndex (i, Word a s) = Word (i, a) s

tagLastWords :: [Word a b] -> [(Word a b, LastWord)]
tagLastWords = reverse . go . reverse
  where
    go [] = []
    go (x : xs) = (x, IsLastWord) : finish xs
    finish = fmap (\x -> (x, NotLastWord))

addElisionPair :: Elision.Pair -> Basic -> Elision
addElisionPair e = Lens.set elisionLens' (e, ())

addCapital :: IsCapitalized -> Elision -> Capital
addCapital x = Lens.set capitalLens' (x, ())

addCrasis :: Crasis -> Capital -> WithCrasis
addCrasis c = Lens.set crasisLens' (c, ())

addSentencePair :: Punctuation.SentencePair -> WithCrasis -> Sentence
addSentencePair s = Lens.set sentenceLens' (s, ())

addInitialEnclitic :: InitialEnclitic -> Sentence -> WithEnclitic
addInitialEnclitic e = Lens.set encliticLens' (e, ())
