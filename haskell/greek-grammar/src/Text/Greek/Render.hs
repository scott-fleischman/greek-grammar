{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Greek.Render where

import Control.Lens
import Data.Foldable
import Data.Set (Set)
import Text.Greek.FileReference
import Text.Greek.Script.Letter (Letter)
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

instance Render Letter where
  render = render . Letter.toLetterChar

instance Render Letter.Position where
  render Letter.Last = "last"
  render Letter.NotLast = "not last"

instance Render Letter.Info where
  render li = T.format "{} {} {}"
    ( render . view Letter.infoCase $ li
    , render . Letter.toLetterChar . view Letter.infoLetter $ li
    , render . view Letter.infoPosition $ li
    )

instance Render Letter.Case where
  render Letter.Uppercase = "upper"
  render Letter.Lowercase = "lower"

instance Render Letter.InfoFinal where
  render info | (c, l) <- Letter.finalToPair info = T.format "{} {}" (render c, render l)

instance Render Mark.AccentAll where
  render = render . Mark.accentAllToUnicodeMark

instance Render Mark.BreathingAll where
  render = render . Mark.breathingAllToUnicodeMark

instance Render Mark.Syllabic where
  render = render . Mark.syllabicToUnicodeMark

instance Render a => Render (Maybe a) where
  render (Just x) = render x
  render Nothing = "-"
