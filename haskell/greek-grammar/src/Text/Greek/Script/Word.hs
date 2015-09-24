{-# LANGUAGE TemplateHaskell #-}

module Text.Greek.Script.Word where

import Prelude hiding (Word)
import Control.Lens
import Data.Text (Text)
import Text.Greek.FileReference
import Text.Greek.Script.Elision

data Basic a = Basic
  { _basicSurface :: a
  , _basicElision :: Maybe (ElisionChar, FileCharReference)
  } deriving Show
makeLenses ''Basic

type BasicText = Basic (Text, FileReference)
