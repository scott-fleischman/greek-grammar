{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Greek.IO.Render where

import Text.Greek.Source.FileReference
import qualified Data.Char as Char
import qualified Data.Text.Format as Format
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Lazy.Builder as Lazy (toLazyText)
import qualified Text.Greek.IO.Type as Type
import qualified Text.Greek.Script.Concrete as Concrete
import qualified Text.Greek.Script.Marked as Marked
import qualified Text.Greek.Script.Unicode as Unicode
import qualified Text.Greek.Script.Word as Word
import qualified Text.Greek.Script.Elision as Elision
import qualified Text.Greek.Source.Work as Work

--import Prelude hiding (Word)
--import Control.Lens
--import Data.Set (Set)
--import qualified Data.Set as S
--import qualified Data.Text.Format as T
--import qualified Text.Greek.Script.Abstract as Abstract
--import qualified Text.Greek.Script.Concrete as Concrete
--import qualified Text.Greek.Script.Syllable as Syllable
--import qualified Text.Greek.Script.Unit as U
--import qualified Text.Greek.Script.Word as Word
--import qualified Text.Greek.Source.All as All

class Render a where
  render :: a -> Lazy.Text

instance Render Lazy.Text where
  render = id

instance Render Type.Name where
  render (Type.List a) = Format.format "[{}]" . Format.Only . render $ a
  render (Type.Function a b) = Format.format "{} → {}" (render a, render b)
  render Type.SourceWord = "Source Word"
  render Type.WorkSource = "Work Source"
  render Type.WorkTitle = "Work Title"
  render Type.SourceFile = "Source File"
  render Type.SourceFileLocation = "Source File Location"
  render Type.ParagraphNumber = "Paragraph Number"
  render Type.Elision = "Elision"
  render Type.UnicodeElision = "Unicode Elision"
  render Type.UnicodeComposed = "Unicode Composed"
  render Type.UnicodeDecomposed = "Unicode Decomposed"
  render Type.UnicodeMarkedLetter = "Unicode Marked Letter"
  render Type.UnicodeLetter = "Unicode Letter"
  render Type.UnicodeMark = "Unicode Mark"
  render Type.LetterCount = "Letter Count"
  render Type.MarkCount = "Mark Count"
  render Type.ConcreteLetter = "Concrete Letter"
  render Type.ConcreteMark = "Concrete Mark"
  render Type.ConcreteMarkedLetter = "Concrete Marked Letter"

instance Render Work.Source where
  render Work.SourceSblgnt = "SBLGNT"

instance Render Work.Title where
  render = Lazy.fromStrict . Work.getTitle

instance Render Word.Source where
  render = Lazy.fromStrict . Word.getSource

instance Render (LineReference, LineReference) where
  render (LineReference l1 c1, LineReference l2 c2) | l1 == l2 && c1 == c2 =
    Format.format "{}:{}" (render l1, render c1)
  render (LineReference l1 c1, LineReference l2 c2) | l1 == l2 =
    Format.format "{}:{}–{}" (render l1, render c1, render c2)
  render (LineReference l1 c1, LineReference l2 c2) =
    Format.format "{}:{}–{}:{}" (render l1, render c1, render l2, render c2)

instance Render Path where
  render (Path ('.' : '/' : xs)) = Lazy.pack xs
  render (Path ('/' : xs)) = Lazy.pack xs
  render (Path p) = Lazy.pack p

instance Render Line where
  render (Line l) = Format.format "{}" (Format.Only l)

instance Render Column where
  render (Column c) = Format.format "{}" (Format.Only c)

instance Render Word.ParagraphIndex where
  render = Format.format "¶ {}" . Format.Only . renderOneBasedIndex . Word.getParagraphIndex

renderOneBasedIndex :: Int -> Lazy.Text
renderOneBasedIndex = Format.format "{}" . Format.Only . (+ 1)

instance Render Char where
  render c = Format.format "{} {}" (renderCodePoint c, renderRawChar c)

instance Render Unicode.Composed where render = render . Unicode.composed
instance Render Unicode.Decomposed where render = render . Unicode.decomposed
instance Render Unicode.Letter where render = render . Unicode.getLetter
instance Render Unicode.Mark where render = render . Unicode.getMark

renderCodePoint :: Char -> Lazy.Text
renderCodePoint = Format.format "U+{}" . Format.Only . Lazy.toUpper . Lazy.toLazyText . Format.left 4 '0' . Format.hex . Char.ord

renderRawChar :: Char -> Lazy.Text
renderRawChar c | Char.isMark c = Format.format "\x25CC{}" . Format.Only $ c
renderRawChar c = Lazy.singleton c

renderFunction :: (Render a, Render b) => (a, b) -> Lazy.Text
renderFunction (a, b) = Format.format "{} → {}" (render a, render b)

renderSingleLineList :: Render a => [a] -> Lazy.Text
renderSingleLineList = Format.format "[{}]" . Format.Only . Lazy.intercalate ", " . fmap render

instance Render [Unicode.Decomposed] where render = renderSingleLineList
instance Render (Unicode.Composed, [Unicode.Decomposed]) where render = renderFunction

instance Render [Unicode.Mark] where render = renderSingleLineList

renderMarkedUnit :: (Render a, Render b) => Marked.Unit a b -> Lazy.Text
renderMarkedUnit (Marked.Unit i m) = Format.format "{}, {}" (render i, render m)

instance Render (Marked.Unit Unicode.Letter [Unicode.Mark]) where render = renderMarkedUnit

instance Render ([Unicode.Decomposed], Marked.Unit Unicode.Letter [Unicode.Mark]) where render = renderFunction

instance Render Word.LetterCount where
  render = renderLabeledNumber "letter" "letters" . Word.getLetterCount

renderLabeledNumber :: Lazy.Text -> Lazy.Text -> Int -> Lazy.Text 
renderLabeledNumber sg pl n = Format.format "{} {}" (n, if n == 1 then sg else pl)

instance Render Word.MarkCount where
  render = renderLabeledNumber "mark" "marks" . Word.getMarkCount

instance Render Elision.IsElided where
  render Elision.Elided = "Elided"
  render Elision.NotElided = "Not elided"

instance Render Elision.ElisionChar where render = render . Elision._getElisionChar

instance Render Concrete.Letter where render = renderRawChar . Unicode.getLetter . Concrete.letterToUnicode
instance Render Concrete.Mark where render = renderRawChar . Unicode.getMark . Concrete.markToUnicode
instance Render [Concrete.Mark] where render = renderSingleLineList
instance Render (Marked.Unit Concrete.Letter [Concrete.Mark]) where render = renderMarkedUnit
instance Render (Marked.Unit Unicode.Letter [Unicode.Mark], Marked.Unit Concrete.Letter [Concrete.Mark]) where
  render = renderFunction
instance Render (Unicode.Letter, Concrete.Letter) where render = renderFunction
instance Render (Unicode.Mark, Concrete.Mark) where render = renderFunction

--instance Render U.LetterChar where
--  render = L.singleton . U.getLetterChar

--instance Render U.MarkChar where
--  render = T.format "\x25CC{}" . T.Only . U.getMarkChar

--instance Render Concrete.Letter where
--  render = render . Concrete.letterToLetterChar

--instance Render Concrete.Mark where
--  render = render . Concrete.markToMarkChar

--instance (Render a, Render b) => Render (a, b) where
--  render (a, b) = T.format "({},{})" (render a, render b)

--instance (Render a, Render b, Render c) => Render (a, b, c) where
--  render (a, b, c) = T.format "({},{},{})" (render a, render b, render c)

--instance (Render l, Render m) => Render (U.Unit l m) where
--  render (U.Unit l ms) = T.format "({},{})" (render l, render ms)

--instance Render FileCharReference where
--  render (FileCharReference p l) = T.format "{}:{}" (T.Shown p, render l)

--instance Render LineReference where
--  render (LineReference (Line l) (Column c)) = T.format "{}:{}" (l, c)

--instance Render Int where
--  render = L.pack . show

--instance Render Abstract.Case where
--  render Abstract.Uppercase = "upper"
--  render Abstract.Lowercase = "lower"

--instance Render Abstract.Letter where
--  render = render . Abstract.toLetterChar

--instance Render Abstract.LetterFinal where
--  render = render . Abstract.letterFinalToLetterChar

--instance Render Mark.AccentAll where
--  render = render . Mark.accentAllToConcreteMark

--instance Render Mark.BreathingAll where
--  render = render . Mark.breathingAllToConcreteMark

--instance Render Mark.SyllabicAll where
--  render = render . Mark.syllabicAllToConcreteMark

--instance Render a => Render (Maybe a) where
--  render (Just x) = render x
--  render Nothing = "-"

--instance Render Word.IsCapitalized where
--  render Word.IsCapitalized = "capital"
--  render Word.IsNotCapitalized = "lower"

--instance Render ElisionChar where
--  render = L.singleton . view getElisionChar

--instance Render ([(Concrete.Mark, FileCharReference)]) where
--  render = renderListLines . fmap (view _1)

--instance Render a => Render (Word.Basic [a]) where
--  render (Word.Basic surface elision) = T.format "Word {}\n{}" (renderElision elision, renderListLines surface)

--instance Render a => Render (Word.Cased [a]) where
--  render (Word.Cased surface elision cap) = T.format "Word {} {}\n{}" (renderElision elision, render cap, renderListLines surface)

--instance Render a => Render (All.Work [a]) where
--  render = renderListLines . view All.workContent

--instance Render Abstract.Consonant where
--  render = render . Abstract.consonantToLetter

--instance Render Abstract.Vowel where
--  render = render . Abstract.vowelToLetter

--instance Render Abstract.VowelConsonant where
--  render = renderEitherIgnore

--instance Render Syllable.VocalicConsonant where
--  render = renderEitherIgnore

--instance Render Syllable.VocalicPair where
--  render (Syllable.OneVowel v)            = T.format "V {}" (T.Only . render $ view _1 v)
--  render (Syllable.IotaSubscriptVowel v)  = T.format "I {}" (T.Only . render $ view _1 v)
--  render (Syllable.TwoVowel (v1, v2))     = T.format "D {}" (T.Only . render $ (view _1 v1, view _1 v2))

--instance Render Abstract.VowelCluster where
--  render = renderListConcat

--instance Render Abstract.ConsonantCluster where
--  render = renderListConcat

--instance Render Abstract.VowelConsonantCluster where
--  render = renderEitherIgnore

--renderMaybeEmpty :: Render a => Maybe a -> L.Text
--renderMaybeEmpty (Just a) = render a
--renderMaybeEmpty Nothing = ""

--instance Render [(Abstract.Vowel, Maybe Mark.SyllabicAll)] where
--  render = renderListConcat . fmap (\(x, y) -> L.concat [render x, renderMaybeEmpty y])

--instance Render [(Abstract.Consonant, Maybe Mark.SyllabicAll)] where
--  render = renderListConcat . fmap (\(x, y) -> L.concat [render x, renderMaybeEmpty y])

--instance Render (Either [(Abstract.Vowel, Maybe Mark.SyllabicAll)] [(Abstract.Consonant, Maybe Mark.SyllabicAll)]) where
--  render = renderEitherIgnore

--instance Render [Either [(Abstract.Vowel, Maybe Mark.SyllabicAll)] [(Abstract.Consonant, Maybe Mark.SyllabicAll)]] where
--  render = renderListConcat

--instance Render [(Either [(Abstract.Vowel, Maybe Mark.SyllabicAll)] [(Abstract.Consonant, Maybe Mark.SyllabicAll)], FileCharReference)] where
--  render = renderListLines

--renderEitherIgnore :: (Render a, Render b) => Either a b -> L.Text
--renderEitherIgnore (Left x) = render x
--renderEitherIgnore (Right x) = render x

--renderElision :: Maybe (ElisionChar, FileCharReference) -> L.Text
--renderElision = render . fmap (view _1)

--renderListConcat :: Render a => [a] -> L.Text
--renderListConcat = L.concat . fmap render

--renderListIntercalate :: Render a => L.Text -> [a] -> L.Text
--renderListIntercalate s = L.intercalate s . fmap render

--renderListLines :: Render a => [a] -> L.Text
--renderListLines = L.concat . fmap (flip L.append "\n") . fmap render

--renderReverseListLines :: Render a => [a] -> L.Text
--renderReverseListLines = renderListLines . reverse

--renderSetLines :: Render a => Set a -> L.Text
--renderSetLines = renderListLines . S.toAscList
