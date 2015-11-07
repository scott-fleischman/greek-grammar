{-# LANGUAGE TemplateHaskell #-}

module Text.Greek.Source.Work where

import Data.Text (Text)
import qualified Control.Lens as Lens
import qualified Text.Greek.Script.Word as Word

data Source = SourceSblgnt deriving (Eq, Ord, Show)

newtype Title = Title Text deriving (Eq, Ord, Show)

type WorkText = Work [Word.BasicText]
data Work a = Work
  { _workSource :: Source
  , _workTitle :: Title
  , _workContent :: a
  } deriving Show
Lens.makeLenses ''Work
