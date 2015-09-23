{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Greek.Render where

import Data.Foldable
import Data.Set (Set)
import Text.Greek.FileReference
import qualified Data.Set as S
import qualified Data.Text.Format as T
import qualified Data.Text.Lazy as L
import qualified Text.Greek.Script.Letter as Letter
import qualified Text.Greek.Script.Mark as Mark
import qualified Text.Greek.Script.Unicode as U
import qualified Text.Greek.Script.Unit as U

class Render a where
  render :: a -> L.Text

instance Render U.LetterChar where
  render = L.singleton . U.getLetterChar

instance Render U.MarkChar where
  render = T.format "\x25CC{}" . T.Only . U.getMarkChar

instance Render U.UnicodeLetter where
  render = render . U.unicodeLetterToLetterChar

instance Render U.UnicodeMark where
  render = render . U.unicodeMarkToMarkChar

instance (Render a, Render b) => Render (a, b) where
  render (a, b) = T.format "({},{})" (render a, render b)

instance (Render a, Render b, Render c) => Render (a, b, c) where
  render (a, b, c) = T.format "({},{},{})" (render a, render b, render c)

instance (Render l, Render m) => Render (U.Unit l m) where
  render (U.Unit (c, r) ms) = T.format "({},{},{})" (render c, render r, render ms)

instance Render FileCharReference where
  render (FileCharReference p l) = T.format "{}:{}" (T.Shown p, render l)

instance Render LineReference where
  render (LineReference (Line l) (Column c)) = T.format "{}:{}" (l, c)

instance Render a => Render [a] where
  render = L.intercalate "," . fmap render . toList

instance Render Int where
  render = L.pack . show

instance Render a => Render (Set a) where
  render = render . S.toAscList

instance Render Letter.IsLast where
  render Letter.IsLast = "last"
  render Letter.IsNotLast = "not last"

instance Render Letter.Case where
  render Letter.Uppercase = "upper"
  render Letter.Lowercase = "lower"

instance Render Letter.Letter where
  render = render . Letter.toLetterChar

instance Render Letter.LetterFinal where
  render = render . Letter.letterFinalToLetterChar

instance Render Mark.AccentAll where
  render = render . Mark.accentAllToUnicodeMark

instance Render Mark.BreathingAll where
  render = render . Mark.breathingAllToUnicodeMark

instance Render Mark.SyllabicAll where
  render = render . Mark.syllabicAllToUnicodeMark

instance Render a => Render (Maybe a) where
  render (Just x) = render x
  render Nothing = "-"
