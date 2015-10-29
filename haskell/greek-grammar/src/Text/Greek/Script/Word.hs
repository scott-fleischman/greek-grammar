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
  , _basicParagraph :: Int
  } deriving Show
makeLenses ''Basic

type BasicText = Basic (Text, FileReference)

data IsCapitalized = IsCapitalized | IsNotCapitalized deriving (Eq, Ord, Show)
data Cased a = Cased
  { _casedSurface :: a
  , _casedElision :: Maybe (ElisionChar, FileCharReference)
  , _casedIsCapitalized :: IsCapitalized
  } deriving Show
makeLenses ''Cased
