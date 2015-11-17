{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Greek.IO.Render where

import Text.Greek.Source.FileReference
import qualified Data.Char as Char
import qualified Data.Text.Format as Format
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Lazy.Builder as Lazy (toLazyText)
import qualified Text.Greek.IO.Type as Type
import qualified Text.Greek.Phonology.Consonant as Consonant
import qualified Text.Greek.Script.Abstract as Abstract
import qualified Text.Greek.Script.Concrete as Concrete
import qualified Text.Greek.Script.Mark as Mark
import qualified Text.Greek.Script.Marked as Marked
import qualified Text.Greek.Script.Place as Place
import qualified Text.Greek.Script.Syllable as Syllable
import qualified Text.Greek.Script.Unicode as Unicode
import qualified Text.Greek.Script.Word as Word
import qualified Text.Greek.Script.Elision as Elision
import qualified Text.Greek.Source.Work as Work

class Render a where
  render :: a -> Lazy.Text

instance Render Lazy.Text where
  render = id

instance Render Type.Name where
  render (Type.List a) = Format.format "[{}]" . Format.Only . render $ a
  render (Type.Function a b) = Format.format "{} → {}" (render a, render b)
  render (Type.Indexed a) = Format.format "{}, Position" . Format.Only . render $ a
  render (Type.ReverseIndexed a) = Format.format "{}, Reverse Position" . Format.Only . render $ a
  render (Type.Count a) = Format.format "{} Count" . Format.Only . render $ a
  render Type.SourceWord = "Source Word"
  render Type.WorkSource = "Work Source"
  render Type.WorkTitle = "Work Title"
  render Type.SourceFile = "Source File"
  render Type.SourceFileLocation = "Source File Location"
  render Type.ParagraphNumber = "Paragraph Number"
  render Type.Elision = "Elision"
  render Type.WordPrefix = "Word Prefix"
  render Type.WordSuffix = "Word Suffix"
  render Type.UnicodeElision = "Unicode Elision"
  render Type.UnicodeComposed = "Unicode Composed"
  render Type.UnicodeDecomposed = "Unicode Decomposed"
  render Type.UnicodeLetterMarks = "Unicode Letter, [Unicode Mark]"
  render Type.UnicodeLetter = "Unicode Letter"
  render Type.UnicodeMark = "Unicode Mark"
  render Type.ConcreteLetter = "Concrete Letter"
  render Type.ConcreteMark = "Concrete Mark"
  render Type.ConcreteLetterMarks = "Concrete Letter, [Concrete Mark]"
  render Type.AbstractLetterCaseFinalMarks = "Abstract Letter, Case, Final, [Concrete Mark]"
  render Type.AbstractLetter = "Abstract Letter"
  render Type.LetterCase = "Letter Case"
  render Type.LetterFinalForm   = "Letter Final Form"
  render Type.AbstractLetterCaseFinal = "Abstract Letter, Case, Final"
  render Type.WordCapitalization = "Word Capitalization"
  render Type.MarkKind = "Mark Kind"
  render Type.Accent = "Accent"
  render Type.Breathing = "Breathing"
  render Type.SyllabicMark = "Syllabic Mark"
  render Type.AbstractLetterMarkKinds = "Abstract Letter, [Mark Kind]"
  render Type.MarkGroup = "Mark Group"
  render Type.AbstractLetterMarkGroup = "Abstract Letter, Mark Group"
  render Type.VowelConsonantMarkGroup = "Vowel / Consonant, Mark Group"
  render Type.VowelConsonant = "Vowel / Consonant"
  render Type.Vowel = "Vowel"
  render Type.Consonant = "Consonant"
  render Type.SyllabicMarkVowelConsonant = "Syllabic Mark, Vowel / Consonant"
  render Type.StartSyllable = "Start Syllable"
  render Type.VocalicSyllableABConsonantB = "Vocalic Syllable, Accent, Breathing / Consonant, Breathing"
  render Type.VocalicSyllable = "Vocalic Syllable"
  render Type.VocalicSyllableSingle = "Vocalic Syllable Single Vowel"
  render Type.ImproperDiphthong = "Improper Diphthong"
  render Type.Diphthong = "Diphthong"
  render Type.Syllable = "Syllable"
  render Type.VocalicSyllableABConsonantRh = "Vocalic Syllable, Accent, Breathing / Consonant+ῥ"
  render Type.ConsonantBreathing = "Consonant, Breathing"
  render Type.ConsonantRh = "Consonant+ῥ"
  render Type.VocalicSyllableABConsonantRhCluster = "Vocalic Syllable, Accent, Breathing / [Consonant+ῥ]"
  render Type.ConsonantRhCluster = "[Consonant+ῥ]"
  render Type.ConsonantRhClusterPlace3 = "[Consonant+ῥ], Initial, Medial, Final"
  render Type.ConsonantRhClusterPlace3Swap = "Initial, Medial, Final, [Consonant+ῥ]"
  render Type.ConsonantRhClusterPlaceInfo = "Medial, Attested Initial, Length, Stop+μ/ν, Double, [Consonant+ῥ]"

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

renderSingleLineString :: Render a => [a] -> Lazy.Text
renderSingleLineString = Lazy.concat . fmap render

instance Render (Maybe Word.Prefix, Maybe Word.Suffix) where render = renderPair
instance Render (Maybe Word.Prefix) where render = renderMaybe "No prefix"
instance Render (Maybe Word.Suffix) where render = renderMaybe "No suffix"
instance Render Word.Prefix where render = Lazy.fromStrict . Word.getPrefix
instance Render Word.Suffix where render = Lazy.fromStrict . Word.getSuffix

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

instance Render Mark.AccentCount where
  render = renderLabeledNumber "accent" "accents" . Mark.getAccentCount
instance Render Mark.BreathingCount where
  render = renderLabeledNumber "breathing mark" "breathing marks" . Mark.getBreathingCount
instance Render Mark.SyllabicCount where
  render = renderLabeledNumber "syllabic mark" "syllabic marks" . Mark.getSyllabicCount

instance Render Elision.Elided where
  render Elision.IsElided = "Is elided"
  render Elision.NotElided = "Not elided"

instance Render Elision.ElisionChar where render = render . Elision._getElisionChar
instance Render (Maybe Elision.ElisionChar) where render = renderMaybe "No elision character"
instance Render Elision.Pair where render = renderPair

instance Render Concrete.Letter where render = renderRawChar . Unicode.getLetter . Concrete.letterToUnicode
instance Render [Concrete.Mark] where render = renderSingleLineList
instance Render (Marked.Unit Concrete.Letter [Concrete.Mark]) where render = renderMarkedUnit
instance Render (Marked.Unit Unicode.Letter [Unicode.Mark], Marked.Unit Concrete.Letter [Concrete.Mark]) where
  render = renderFunction
instance Render (Unicode.Letter, Concrete.Letter) where render = renderFunction
instance Render (Unicode.Mark, Concrete.Mark) where render = renderFunction
instance Render Concrete.Mark where
  render m = Format.format "{} {}" (getName m, renderRawChar . Unicode.getMark . Concrete.markToUnicode $ m)

getName :: Concrete.Mark -> Lazy.Text
getName Concrete.Grave = "Grave Accent Mark"
getName Concrete.Acute = "Acute Accent Mark"
getName Concrete.Diaeresis = "Diaeresis Mark"
getName Concrete.Smooth = "Smooth Breathing Mark"
getName Concrete.Rough = "Rough Breathing Mark"
getName Concrete.Circumflex = "Circumflex Mark"
getName Concrete.IotaSubscript = "Iota Subscript Mark"

renderLetterPosition :: Int -> Lazy.Text
renderLetterPosition = Format.format "Letter Position {}" . Format.Only . renderOneBasedIndex
renderReverseLetterPosition :: Int -> Lazy.Text
renderReverseLetterPosition = Format.format "Letter Reverse Position {}" . Format.Only . renderOneBasedIndex

instance Render Abstract.Letter where render = renderRawChar . Unicode.getLetter . Abstract.letterToUnicode
instance Render Abstract.Case where
  render Abstract.Lowercase = "Lowercase Letter"
  render Abstract.Uppercase = "Uppercase Letter"
instance Render Abstract.Final where
  render Abstract.FinalNotSupported = "N/A final form"
  render Abstract.IsFinal = "Is final form"
  render Abstract.IsNotFinal = "Not final form"

instance Render (Abstract.Letter, Abstract.LetterIndex) where render = renderPair
instance Render Abstract.LetterIndex where render = renderLetterPosition . Abstract.getLetterIndex
instance Render (Abstract.Letter, Abstract.LetterReverseIndex) where render = renderPair
instance Render Abstract.LetterReverseIndex where render = renderReverseLetterPosition . Abstract.getLetterReverseIndex

instance Render (Abstract.Case, Abstract.CaseIndex) where render = renderPair
instance Render Abstract.CaseIndex where render = renderLetterPosition . Abstract.getCaseIndex
instance Render (Abstract.Final, Abstract.FinalReverseIndex) where render = renderPair
instance Render Abstract.FinalReverseIndex where render = renderReverseLetterPosition . Abstract.getFinalReverseIndex

instance Render Word.IsCapitalized where
  render Word.IsCapitalized = "Word is capitalized"
  render Word.IsNotCapitalized = "Word is not capitalized"

renderPair :: (Render a, Render b) => (a, b) -> Lazy.Text
renderPair (a, b) = Format.format "{}, {}" (render a, render b)

renderTriple :: (Render a, Render b, Render c) => (a, b, c) -> Lazy.Text
renderTriple (a, b, c) = Format.format "{}, {}, {}" (render a, render b, render c)

renderQuad :: (Render a, Render b, Render c, Render d) => (a, b, c, d) -> Lazy.Text
renderQuad (a, b, c, d) = Format.format "{}, {}, {}, {}" (render a, render b, render c, render d)

instance Render (Concrete.Letter, (Abstract.Letter, Abstract.Case, Abstract.Final)) where render = renderFunction
instance Render (Abstract.Letter, Abstract.Case, Abstract.Final) where render = renderTriple
instance Render (Marked.Unit (Abstract.Letter, Abstract.Case, Abstract.Final) [Concrete.Mark]) where render = renderMarkedUnit

instance Render (Abstract.Letter, Abstract.Final) where render = renderPair
instance Render (Marked.Unit (Abstract.Letter, Abstract.Final) [Concrete.Mark]) where render = renderMarkedUnit

instance Render (Marked.Unit Abstract.Letter [Concrete.Mark]) where render = renderMarkedUnit

instance Render (Concrete.Mark, Mark.Kind) where render = renderFunction
instance Render Mark.Kind where
  render (Mark.KindAccent x) = Format.format "Accent: {}" (Format.Only $ render x)
  render (Mark.KindBreathing x) = Format.format "Breathing: {}" (Format.Only $ render x)
  render (Mark.KindSyllabic x) = Format.format "Syllabic Mark: {}" (Format.Only $ render x)

instance Render Mark.Accent where
  render Mark.AccentAcute = "Acute"
  render Mark.AccentGrave = "Grave"
  render Mark.AccentCircumflex = "Circumflex"

instance Render Mark.Breathing where
  render Mark.BreathingSmooth = "Smooth"
  render Mark.BreathingRough = "Rough"

instance Render Mark.Syllabic where
  render Mark.SyllabicDiaeresis = "Diaeresis"
  render Mark.SyllabicIotaSubscript = "Iota Subscript"

instance Render [Mark.Kind] where render = renderSingleLineList
instance Render (Marked.Unit Abstract.Letter [Mark.Kind]) where render = renderMarkedUnit

instance Render ([Mark.Kind], Mark.Group Maybe) where render = renderFunction
instance Render (Mark.Group Maybe) where render = renderTriple

renderMaybe :: Render a => Lazy.Text -> Maybe a -> Lazy.Text
renderMaybe _ (Just x) = render x
renderMaybe t Nothing = t

instance Render (Maybe Mark.Accent) where render = renderMaybe "No Accent"
instance Render (Maybe Mark.Breathing) where render = renderMaybe "No Breathing"
instance Render (Maybe Mark.Syllabic) where render = renderMaybe "No Syllabic Mark"

instance Render (Marked.Unit Abstract.Letter (Mark.Group Maybe)) where render = renderMarkedUnit

instance Render (Abstract.Letter, Either Abstract.Vowel Abstract.Consonant) where render = renderPair
instance Render (Either Abstract.Vowel Abstract.Consonant) where render = renderEitherIgnore
instance Render Abstract.Vowel where render = Format.format "Vowel {}" . Format.Only . render . Abstract.vowelToLetter
instance Render Abstract.Consonant where render = Format.format "Consonant {}" . Format.Only . render . Abstract.consonantToLetter

renderEitherIgnore :: (Render a, Render b) => Either a b -> Lazy.Text
renderEitherIgnore (Left x) = render x
renderEitherIgnore (Right x) = render x

instance Render (Marked.Unit (Either Abstract.Vowel Abstract.Consonant) (Mark.Group Maybe)) where render = renderMarkedUnit

instance Render Word.VowelCount where render = renderLabeledNumber "vowel" "vowels" . Word.getVowelCount
instance Render Word.ConsonantCount where render = renderLabeledNumber "consonant" "consonants" . Word.getConsonantCount

instance Render (Mark.Syllabic, Abstract.VowelConsonant) where render = renderPair

instance Render (Syllable.Start (Mark.Group Maybe)) where render = renderEitherIgnore
instance Render (Abstract.Consonant, Mark.Group Maybe) where render = renderPair
instance Render (Abstract.Vowel, Mark.Group Maybe) where render = renderPair
instance Render (Syllable.StartVocalic (Abstract.Vowel, Mark.Group Maybe)) where
  render (Syllable.StartVocalicSingle v) = Format.format "Start Vocalic Single — {}" (Format.Only . render $ v)
  render (Syllable.StartVocalicDiaeresis v) = Format.format "Start Vocalic Diaeresis — {}" (Format.Only . render $ v)
  render (Syllable.StartVocalicIota v) = Format.format "Start Vocalic Iota — {}" (Format.Only . render $ v)
  render (Syllable.StartVocalicDiphthong v1 v2) = Format.format "Start Vocalic Diphthong — {} — {}" (render v1, render v2)

instance Render (Syllable.Start (Mark.Group Maybe), Syllable.VocalicConsonant (Mark.AccentBreathing Maybe) (Maybe Mark.Breathing)) where
  render = renderFunction
instance Render (Syllable.VocalicConsonant (Mark.AccentBreathing Maybe) (Maybe Mark.Breathing)) where
  render = renderEitherIgnore
instance Render (Abstract.Consonant, Maybe Mark.Breathing) where render = renderPair
instance Render (Syllable.Vocalic (Mark.AccentBreathing Maybe)) where
  render (Syllable.VocalicSingle v m) = Format.format "Single {}, {}" (render v, render m)
  render (Syllable.VocalicIota d m) = Format.format "Improper Diphthong {}, {}" (render d, render m)
  render (Syllable.VocalicDiphthong d m) = Format.format "Diphthong {}, {}" (render d, render m)
instance Render (Syllable.Vocalic ()) where
  render (Syllable.VocalicSingle v _) = Format.format "Single Vowel {}" (Format.Only $ render v)
  render (Syllable.VocalicIota d _) = Format.format "Improper Diphthong {}" (Format.Only $ render d)
  render (Syllable.VocalicDiphthong d _) = Format.format "Diphthong {}" (Format.Only $ render d)
instance Render (Mark.AccentBreathing Maybe) where render = renderPair
instance Render Syllable.ImproperDiphthong where
  render Syllable.I_α = "ᾳ"
  render Syllable.I_η = "ῃ"
  render Syllable.I_ω = "ῳ"
instance Render Syllable.Diphthong where
  render Syllable.D_αι = "αι" 
  render Syllable.D_αυ = "αυ" 
  render Syllable.D_ει = "ει" 
  render Syllable.D_ευ = "ευ" 
  render Syllable.D_ηυ = "ηυ" 
  render Syllable.D_οι = "οι" 
  render Syllable.D_ου = "ου" 
  render Syllable.D_υι = "υι"

instance Render Syllable.Count where render (Syllable.Count c) = renderLabeledNumber "syllable" "syllables" c
instance Render Syllable.VocalicSingleCount where render (Syllable.VocalicSingleCount c) = renderLabeledNumber "single vowel syllable" "single vowel syllables" c
instance Render Syllable.ImproperDiphthongCount where render (Syllable.ImproperDiphthongCount c) = renderLabeledNumber "improper diphthong" "improper diphthongs" c
instance Render Syllable.DiphthongCount where render (Syllable.DiphthongCount c) = renderLabeledNumber "diphthong" "diphthongs" c

instance Render ((Abstract.Consonant, Maybe Mark.Breathing), Consonant.PlusRoughRho) where render = renderFunction
instance Render (Either (Syllable.Vocalic (Mark.AccentBreathing Maybe)) Consonant.PlusRoughRho) where render = renderEitherIgnore

instance Render Consonant.PlusRoughRho where render = renderRhConsonant

renderRhConsonant :: Consonant.PlusRoughRho -> Lazy.Text
renderRhConsonant Consonant.Rh_β = "Consonant β"
renderRhConsonant Consonant.Rh_γ = "Consonant γ"
renderRhConsonant Consonant.Rh_δ = "Consonant δ"
renderRhConsonant Consonant.Rh_ζ = "Consonant ζ"
renderRhConsonant Consonant.Rh_θ = "Consonant θ"
renderRhConsonant Consonant.Rh_κ = "Consonant κ"
renderRhConsonant Consonant.Rh_λ = "Consonant λ"
renderRhConsonant Consonant.Rh_μ = "Consonant μ"
renderRhConsonant Consonant.Rh_ν = "Consonant ν"
renderRhConsonant Consonant.Rh_ξ = "Consonant ξ"
renderRhConsonant Consonant.Rh_π = "Consonant π"
renderRhConsonant Consonant.Rh_ρ = "Consonant ρ"
renderRhConsonant Consonant.Rh_ῥ = "Consonant ῥ"
renderRhConsonant Consonant.Rh_σ = "Consonant σ"
renderRhConsonant Consonant.Rh_τ = "Consonant τ"
renderRhConsonant Consonant.Rh_φ = "Consonant φ"
renderRhConsonant Consonant.Rh_χ = "Consonant χ"
renderRhConsonant Consonant.Rh_ψ = "Consonant ψ"

renderRhChar :: Consonant.PlusRoughRho -> Lazy.Text
renderRhChar Consonant.Rh_β = "β"
renderRhChar Consonant.Rh_γ = "γ"
renderRhChar Consonant.Rh_δ = "δ"
renderRhChar Consonant.Rh_ζ = "ζ"
renderRhChar Consonant.Rh_θ = "θ"
renderRhChar Consonant.Rh_κ = "κ"
renderRhChar Consonant.Rh_λ = "λ"
renderRhChar Consonant.Rh_μ = "μ"
renderRhChar Consonant.Rh_ν = "ν"
renderRhChar Consonant.Rh_ξ = "ξ"
renderRhChar Consonant.Rh_π = "π"
renderRhChar Consonant.Rh_ρ = "ρ"
renderRhChar Consonant.Rh_ῥ = "ῥ"
renderRhChar Consonant.Rh_σ = "σ"
renderRhChar Consonant.Rh_τ = "τ"
renderRhChar Consonant.Rh_φ = "φ"
renderRhChar Consonant.Rh_χ = "χ"
renderRhChar Consonant.Rh_ψ = "ψ"

instance Render [Consonant.PlusRoughRho] where render = renderSingleLineString . fmap renderRhChar

instance Render (Syllable.VocalicEither (Mark.AccentBreathing Maybe) [Consonant.PlusRoughRho]) where render = renderEitherIgnore
instance Render
  ( [Syllable.VocalicEither (Mark.AccentBreathing Maybe) Consonant.PlusRoughRho]
  , [Syllable.VocalicEither (Mark.AccentBreathing Maybe) [Consonant.PlusRoughRho]]
  )
  where render = renderFunction
instance Render [Syllable.VocalicEither (Mark.AccentBreathing Maybe) [Consonant.PlusRoughRho]] where render = renderSingleLineList
instance Render [Syllable.VocalicEither (Mark.AccentBreathing Maybe) Consonant.PlusRoughRho] where render = renderSingleLineList

instance Render ([Consonant.PlusRoughRho], Place.Place3) where render = renderPair
instance Render (Place.Place3, [Consonant.PlusRoughRho]) where render = renderPair
instance Render Place.Place3 where render = renderTriple

instance Render Place.Initial where
  render Place.IsInitial = "Is Initial"
  render Place.NotInitial = "Not Initial"
instance Render Place.Medial where
  render Place.IsMedial = "Is Medial"
  render Place.NotMedial = "Not Medial"
instance Render Place.Final where
  render Place.IsFinal = "Is Final"
  render Place.NotFinal = "Not Final"
instance Render Place.AttestedInitial where
  render Place.AttestedInitial = "Attested Initial"
  render Place.UnattestedInitial = "Unattested Initial"

instance Render ([Consonant.PlusRoughRho], Place.Place4) where render = renderPair
instance Render Place.Place4 where render = renderQuad
instance Render ([Consonant.PlusRoughRho], (Place.Medial, Place.AttestedInitial)) where render = renderPair
instance Render (Place.Medial, Place.AttestedInitial) where render = renderPair

instance Render (Place.Medial, Place.AttestedInitial, 
  (Consonant.ClusterLength, Consonant.StopMuNu, [Consonant.PlusRoughRho]))
  where render = renderTriple
instance Render (Consonant.ClusterLength, Consonant.StopMuNu, [Consonant.PlusRoughRho]) where render = renderTriple
instance Render Consonant.StopMuNu where
  render Consonant.IsStopMuNu = "Is Stop+μ/ν"
  render Consonant.IsNotStopMuNu = "Not Stop+μ/ν"

instance Render Consonant.ClusterLength where render = renderLabeledNumber "consonant" "consonants" . Consonant.getClusterLength
instance Render (Place.Medial, Place.AttestedInitial,
  (Consonant.ClusterLength, Consonant.StopMuNu, Consonant.IsolatedDouble, [Consonant.PlusRoughRho])) where render = renderTriple
instance Render (Consonant.ClusterLength, Consonant.StopMuNu, Consonant.IsolatedDouble, [Consonant.PlusRoughRho]) where render = renderQuad
instance Render Consonant.IsolatedDouble where
  render Consonant.IsIsolatedDouble = "Is Isolated Double"
  render Consonant.NotIsolatedDouble = "Not Isolated Double"
