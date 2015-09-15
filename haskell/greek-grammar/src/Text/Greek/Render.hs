{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Greek.Render where

import Data.Char
import Data.Foldable
import Text.Greek.FileReference
import Text.Greek.Script.Unit
import qualified Data.Map as M
import qualified Data.Text.Format as T
import qualified Data.Text.Lazy as L

class Render a where
  render :: a -> L.Text

instance Render (Char, FileCharReference) where
  render (c, r) = T.format "({},{})" (c', render r)
    where
      c' = if isMark c
        then T.format "\x25CC{}" (T.Only c)
        else L.singleton c

instance Render Unit where
  render (Unit c r ms) = T.format "({},{},{})" (c, render r, render (M.toList ms))

instance Render FileCharReference where
  render (FileCharReference p l) = T.format "{}:{}" (T.Shown p, render l)

instance Render LineReference where
  render (LineReference (Line l) (Column c)) = T.format "{}:{}" (l, c)

instance Render a => Render [a] where
  render = L.intercalate "," . fmap render . toList
