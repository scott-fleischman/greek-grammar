{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Text.Greek.Source.Work where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import qualified Control.Lens as Lens

data Source = SourceSblgnt deriving (Eq, Ord, Show, Generic)
instance ToJSON Source
instance FromJSON Source

newtype Title = Title { getTitle :: Text } deriving (Eq, Ord, Show, Generic)
instance ToJSON Title
instance FromJSON Title

newtype Index = Index { getIndex :: Int } deriving (Eq, Ord, Show, Generic)
instance ToJSON Index
instance FromJSON Index

type Basic = Work (Source, Title)
type IndexSourceTitle = (Index, Source, Title)
type Indexed = Work IndexSourceTitle

data Work i c = Work
  { getInfo :: i
  , getContent :: c
  } deriving (Show, Generic)
Lens.makeLensesFor
  [ ("getInfo", "info")
  , ("getContent", "content")
  ]
  ''Work
instance (ToJSON i, ToJSON c) => ToJSON (Work i c)
instance (FromJSON i, FromJSON c) => FromJSON (Work i c)

indexBasic :: [Basic c] -> [Indexed c]
indexBasic = fmap addIndex . zip (fmap Index [0..])
  where
    addIndex (i, Work (s, t) c) = Work (i, s, t) c
