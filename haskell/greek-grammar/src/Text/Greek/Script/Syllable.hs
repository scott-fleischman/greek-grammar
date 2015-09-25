{-# LANGUAGE RankNTypes #-}

module Text.Greek.Script.Syllable where

import Control.Lens
import Text.Greek.FileReference
import Text.Greek.Parse.Utility
import Text.Greek.Script.Unit (Unit)
import Text.Parsec.Error (ParseError)
import Text.Parsec.Prim
import Text.Parsec.Combinator
import qualified Text.Greek.Script.Letter as Letter
import qualified Text.Greek.Script.Mark as Mark
import qualified Text.Greek.Script.Unit as Unit

data Vocalic a b c
  = OneVowel a
  | IotaSubscriptVowel b
  | TwoVowel c
  deriving (Eq, Ord, Show)

type VocalicSimple a = Vocalic a a (a, a)
type VocalicVowel = VocalicSimple Letter.Vowel
type VocalicPair = VocalicSimple (Letter.Vowel, FileCharReference)

type VocalicConsonant = Either VocalicPair (Letter.Consonant, FileCharReference)

type UnitLetter = Unit.UnitLetter Letter.VowelConsonant Mark.AllPair
type UnitVocalicConsonant = Unit VocalicConsonant Mark.AccentBreathingAllPair

type UnitLetterParser = Parser [UnitLetter] UnitVocalicConsonant

primUnitLetter :: (UnitLetter -> Maybe a) -> Parser [UnitLetter] a
primUnitLetter = primMaybe (^. Unit.unitItem . _2 . fileCharReferenceLine)

parseIotaSubscript :: UnitLetterParser
parseIotaSubscript = primUnitLetter go
  where
    go :: UnitLetter -> Maybe UnitVocalicConsonant
    go u
      | (Left v, r) <- u ^. Unit.unitItem
      , m <- u ^. Unit.unitMarks
      , Just (Mark.IotaSubscriptAll, _) <- m ^. _3
      , v == Letter.V_α || v == Letter.V_η || v == Letter.V_ω
      = Just $ Unit.Unit (Left $ IotaSubscriptVowel (v, r)) (Mark.getAccentBreathingAllPair m)
    go _ = Nothing

vocalicConsonantParser :: UnitLetterParser
vocalicConsonantParser = undefined

parseVocalicSyllable :: [UnitLetter] -> Either ParseError [UnitVocalicConsonant]
parseVocalicSyllable = parse (many1 vocalicConsonantParser) ""

--parseCase :: Show s => (s -> LineReference) -> Lens s t Case () -> [s] -> Either ParseError (Word.IsCapitalized, [t])
--parseCase f g = parse (try (capitalizedParser f g) <|> uncapitalizedParser f g) ""

--lowercaseParser :: Show s => (s -> LineReference) -> Lens s t Case () -> Parser [s] t
--lowercaseParser f g = primLensMaybe f g apply
--  where
--    apply Lowercase = Just ()
--    apply Uppercase = Nothing



-- V_ι
-- V_υ
