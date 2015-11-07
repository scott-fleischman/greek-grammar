{-# LANGUAGE TemplateHaskell #-}

module Text.Greek.Source.Work where

import Data.Text (Text)
import qualified Control.Lens as Lens
import qualified Text.Greek.Script.Word as Word

data Source = SourceSblgnt deriving (Eq, Ord, Show)

newtype Title = Title Text deriving (Eq, Ord, Show)

data SourceTitle = SourceTitle Source Title deriving (Eq, Ord, Show)

type Basic = Work SourceTitle
type WorkText = Basic [Word.BasicText]
data Work i c = Work
  { _workInfo :: i
  , _workContent :: c
  } deriving Show
Lens.makeLenses ''Work
