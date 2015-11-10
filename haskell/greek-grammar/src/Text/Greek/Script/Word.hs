{-# LANGUAGE TemplateHaskell #-}

module Text.Greek.Script.Word where

import Prelude hiding (Word)
import Text.Greek.FileReference
import Text.Greek.Script.Elision
import qualified Control.Lens as Lens

--data IsCapitalized = IsCapitalized | IsNotCapitalized deriving (Eq, Ord, Show)
--data Cased a = Cased
--  { _casedSurface :: a
--  , _casedElision :: Maybe (ElisionChar, FileCharReference)
--  , _casedIsCapitalized :: IsCapitalized
--  } deriving Show
--makeLenses ''Cased

newtype ParagraphIndex = ParagraphIndex { getParagraphIndex :: Int } deriving (Eq, Show, Ord)
newtype Index = Index { getIndex :: Int } deriving (Eq, Ord, Show)

data Word i s = Word
  { getInfo :: i
  , getSurface :: s
  }
Lens.makeLensesFor
  [ ("getInfo", "info")
  , ("getSurface", "surface")
  ]
  ''Word

type Basic = (Maybe (ElisionChar, FileCharReference), ParagraphIndex)
type Indexed a = Word (Index, a)

indexBasic :: [Word Basic s] -> [Indexed Basic s]
indexBasic = fmap addIndex . zip (fmap Index [0..])
  where
    addIndex (i, Word (e, p) s) = Word (i, (e, p)) s
