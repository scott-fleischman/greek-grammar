{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Text.Greek.Script.Marked where

import Prelude hiding (Word, getLine)
import Control.Lens
import Data.Set (Set)
import Text.Greek.Source.FileReference
import qualified Data.Set as S

data Unit l m = Unit
  { _item :: l
  , _marks :: m
  } deriving (Eq, Ord, Show)
makeLenses ''Unit

type MarkList r l m = Unit (l, r) [(m, r)]
type MarkListReference l m = MarkList FileCharReference l m

getMarks :: MarkList r l m -> [m]
getMarks = fmap (view _1) . view marks

getItem :: MarkList r l m -> l
getItem = view (item . _1)

getMarkItemPairs :: MarkList r l m -> [(m, l)]
getMarkItemPairs (Unit (l, _) m) = fmap (flip (,) l) (fmap fst m)

getItemMarkSet :: Ord m => MarkList r l m -> (l, Set m)
getItemMarkSet (Unit (l, _) m) = (l, S.fromList . fmap fst $ m)

getMarkSet :: Ord m => MarkList r l m -> Set m
getMarkSet (Unit _ m) = S.fromList . fmap fst $ m
