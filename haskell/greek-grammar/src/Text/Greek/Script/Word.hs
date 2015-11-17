{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Text.Greek.Script.Word where

import Prelude hiding (Word)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import Text.Greek.Source.FileReference
import Text.Greek.Script.Elision
import qualified Control.Lens as Lens

data IsCapitalized = IsCapitalized | IsNotCapitalized deriving (Eq, Ord, Show, Generic)
instance ToJSON IsCapitalized
instance FromJSON IsCapitalized

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


type Basic = (Maybe (ElisionChar, FileCharReference), ParagraphIndex)
type Capital = (Maybe (ElisionChar, FileCharReference), ParagraphIndex, IsCapitalized)
type Indexed a = Word (Index, a)

indexBasic :: [Word Basic s] -> [Indexed Basic s]
indexBasic = fmap addIndex . zip (fmap Index [0..])
  where
    addIndex (i, Word (e, p) s) = Word (i, (e, p)) s
