{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Greek.Render where

import Data.Char
import Data.Foldable
import Data.Set (Set)
import Text.Greek.FileReference
import Text.Greek.Script.Unit (Unit(..))
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text.Format as T
import qualified Data.Text.Lazy as L
import qualified Text.Greek.Script.Unit as U

class Render a where
  render :: a -> L.Text

instance Render Char where
  render c = if isMark c
    then T.format "\x25CC{}" (T.Only c)
    else L.singleton c

instance (Render a, Render b) => Render (a, b) where
  render (a, b) = T.format "({},{})" (render a, render b)

instance Render Unit where
  render (Unit c r ms) = T.format "({},{},{})" (c, render r, render (M.toList ms))

instance Render FileCharReference where
  render (FileCharReference p l) = T.format "{}:{}" (T.Shown p, render l)

instance Render LineReference where
  render (LineReference (Line l) (Column c)) = T.format "{}:{}" (l, c)

instance Render a => Render [a] where
  render = L.intercalate "," . fmap render . toList

instance Render Int where
  render = L.pack . show

instance Render U.Property where
  render (U.PropertyLetter c) = render c
  render (U.PropertyMark c) = render c

instance Render a => Render (Set a) where
  render = render . S.toAscList
