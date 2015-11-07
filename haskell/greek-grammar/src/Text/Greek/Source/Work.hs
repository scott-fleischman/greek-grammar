{-# LANGUAGE TemplateHaskell #-}

module Text.Greek.Source.Work where

import Data.Text (Text)
import qualified Control.Lens as Lens

data Source = SourceSblgnt deriving (Eq, Ord, Show)
newtype Title = Title Text deriving (Eq, Ord, Show)
newtype Index = Index { getIndex :: Int } deriving (Eq, Ord, Show)

type Basic = Work (Source, Title)
type Indexed = Work (Source, Title, Index)

data Work i c = Work
  { _workInfo :: i
  , _workContent :: c
  } deriving Show
Lens.makeLenses ''Work

indexBasic :: [Basic c] -> [Indexed c]
indexBasic = fmap addIndex . zip (fmap Index [0..])
  where
    addIndex (i, (Work (s, t) c)) = Work (s, t, i) c
