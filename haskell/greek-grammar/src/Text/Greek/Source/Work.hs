{-# LANGUAGE TemplateHaskell #-}

module Text.Greek.Source.Work where

import Data.Text (Text)
import qualified Control.Lens as Lens

data Source = SourceSblgnt deriving (Eq, Ord, Show)
newtype Title = Title { getTitle :: Text } deriving (Eq, Ord, Show)
newtype Index = Index { getIndex :: Int } deriving (Eq, Ord, Show)

type Basic = Work (Source, Title)
type IndexSourceTitle = (Index, Source, Title)
type Indexed = Work IndexSourceTitle

data Work i c = Work
  { getInfo :: i
  , getContent :: c
  } deriving Show
Lens.makeLensesFor
  [ ("getInfo", "info")
  , ("getContent", "content")
  ]
  ''Work

indexBasic :: [Basic c] -> [Indexed c]
indexBasic = fmap addIndex . zip (fmap Index [0..])
  where
    addIndex (i, Work (s, t) c) = Work (i, s, t) c
