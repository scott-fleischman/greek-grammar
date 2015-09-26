{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Greek.Render where

import Prelude hiding (Word)
import Control.Lens
import Data.Set (Set)
import Text.Greek.Script.Elision
import Text.Greek.FileReference
import qualified Data.Set as S
import qualified Data.Text.Format as T
import qualified Data.Text.Lazy as L
import qualified Text.Greek.Script.Letter as Letter
import qualified Text.Greek.Script.Mark as Mark
import qualified Text.Greek.Script.Syllable as Syllable
import qualified Text.Greek.Script.Unicode as U
import qualified Text.Greek.Script.Unit as U
import qualified Text.Greek.Script.Word as Word
import qualified Text.Greek.Source.All as All

class Render a where
  render :: a -> L.Text

instance Render L.Text where
  render = id

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
  render (U.Unit l ms) = T.format "({},{})" (render l, render ms)

instance Render FileCharReference where
  render (FileCharReference p l) = T.format "{}:{}" (T.Shown p, render l)

instance Render LineReference where
  render (LineReference (Line l) (Column c)) = T.format "{}:{}" (l, c)

instance Render Int where
  render = L.pack . show

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

instance Render Word.IsCapitalized where
  render Word.IsCapitalized = "capital"
  render Word.IsNotCapitalized = "lower"

instance Render ElisionChar where
  render = L.singleton . view getElisionChar

instance Render ([(U.UnicodeMark, FileCharReference)]) where
  render = renderListLines . fmap (view _1)

instance Render a => Render (Word.Basic [a]) where
  render (Word.Basic surface elision) = T.format "Word {}\n{}" (renderElision elision, renderListLines surface)

instance Render a => Render (Word.Cased [a]) where
  render (Word.Cased surface elision cap) = T.format "Word {} {}\n{}" (renderElision elision, render cap, renderListLines surface)

instance Render a => Render (All.Work [a]) where
  render = renderListLines . view All.workContent

instance Render Letter.Consonant where
  render = render . Letter.consonantToLetter

instance Render Letter.Vowel where
  render = render . Letter.vowelToLetter

instance Render Letter.VowelConsonant where
  render = renderEitherIgnore

instance Render Syllable.VocalicConsonant where
  render = renderEitherIgnore

instance Render Syllable.VocalicPair where
  render (Syllable.OneVowel v)            = T.format "V {}" (T.Only . render $ view _1 v)
  render (Syllable.IotaSubscriptVowel v)  = T.format "I {}" (T.Only . render $ view _1 v)
  render (Syllable.TwoVowel (v1, v2))     = T.format "D {}" (T.Only . render $ (view _1 v1, view _1 v2))

instance Render Letter.VowelCluster where
  render = renderListConcat

instance Render Letter.ConsonantCluster where
  render = renderListConcat

instance Render Letter.VowelConsonantCluster where
  render = renderEitherIgnore

renderMaybeEmpty :: Render a => Maybe a -> L.Text
renderMaybeEmpty (Just a) = render a
renderMaybeEmpty Nothing = ""

instance Render [(Letter.Vowel, Maybe Mark.SyllabicAll)] where
  render = renderListConcat . fmap (\(x, y) -> L.concat [render x, renderMaybeEmpty y])

instance Render [(Letter.Consonant, Maybe Mark.SyllabicAll)] where
  render = renderListConcat . fmap (\(x, y) -> L.concat [render x, renderMaybeEmpty y])

instance Render (Either [(Letter.Vowel, Maybe Mark.SyllabicAll)] [(Letter.Consonant, Maybe Mark.SyllabicAll)]) where
  render = renderEitherIgnore

instance Render [Either [(Letter.Vowel, Maybe Mark.SyllabicAll)] [(Letter.Consonant, Maybe Mark.SyllabicAll)]] where
  render = renderListConcat

instance Render [(Either [(Letter.Vowel, Maybe Mark.SyllabicAll)] [(Letter.Consonant, Maybe Mark.SyllabicAll)], FileCharReference)] where
  render = renderListLines

renderEitherIgnore :: (Render a, Render b) => Either a b -> L.Text
renderEitherIgnore (Left x) = render x
renderEitherIgnore (Right x) = render x

renderElision :: Maybe (ElisionChar, FileCharReference) -> L.Text
renderElision = render . fmap (view _1)

renderListConcat :: Render a => [a] -> L.Text
renderListConcat = L.concat . fmap render

renderListIntercalate :: Render a => L.Text -> [a] -> L.Text
renderListIntercalate s = L.intercalate s . fmap render

renderListLines :: Render a => [a] -> L.Text
renderListLines = L.concat . fmap (flip L.append "\n") . fmap render

renderReverseListLines :: Render a => [a] -> L.Text
renderReverseListLines = renderListLines . reverse

renderSetLines :: Render a => Set a -> L.Text
renderSetLines = renderListLines . S.toAscList
