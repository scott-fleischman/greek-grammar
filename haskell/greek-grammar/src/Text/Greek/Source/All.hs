module Text.Greek.Source.All where

import Data.Text (Text)
import Text.Greek.FileReference

data Milestone = Paragraph | Verse | Line | Sentence

data Word = Word
  { wordSurface :: Text
  , wordFileReference :: FileReference
  }

data Work = Work
  { workSource :: WorkSource
  , workTitle :: Text
  }

data WorkSource
  = WorkSourceSblgnt 
