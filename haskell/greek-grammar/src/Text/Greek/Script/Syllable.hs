{-# LANGUAGE RankNTypes #-}

module Text.Greek.Script.Syllable where

import Control.Lens
import Text.Greek.FileReference
import Text.Greek.Parse.Utility
import Text.Greek.Script.Unit (Unit)
import Text.Parsec.Error (ParseError)
import Text.Parsec.Prim
import Text.Parsec.Combinator
import qualified Text.Greek.Script.Abstract as Abstract
import qualified Text.Greek.Script.Mark as Mark
import qualified Text.Greek.Script.Unit as Unit

data Vocalic a b c
  = OneVowel a
  | IotaSubscriptVowel b
  | TwoVowel c
  deriving (Eq, Ord, Show)

type VocalicSimple a = Vocalic a a (a, a)
type VocalicVowel = VocalicSimple Abstract.Vowel
type VocalicPair = VocalicSimple (Abstract.Vowel, FileCharReference)

type VocalicConsonant = Either VocalicPair (Abstract.Consonant, FileCharReference)

type UnitLetter = Unit.UnitLetter Abstract.VowelConsonant Mark.AllPair
type UnitVocalicConsonant = Unit VocalicConsonant Mark.AccentBreathingAllPair

type UnitLetterParser a = Parser [UnitLetter] a

primUnitLetter :: String -> (UnitLetter -> Maybe a) -> Parser [UnitLetter] a
primUnitLetter p = primMaybe p (^. Unit.unitItem . _2 . fileCharReferenceLine)

isIotaSubscriptVowel :: Abstract.Vowel -> Bool
isIotaSubscriptVowel v
  =  v == Abstract.V_α
  || v == Abstract.V_η
  || v == Abstract.V_ω

iotaSubscriptParser :: UnitLetterParser UnitVocalicConsonant
iotaSubscriptParser = primUnitLetter "Iota subscript vowel" go
  where
    go u
      | (Left v, r) <- u ^. Unit.unitItem
      , isIotaSubscriptVowel v
      , m <- u ^. Unit.unitMarks
      , Just (Mark.IotaSubscriptAll, _) <- m ^. _3
      = Just $ Unit.Unit (Left $ IotaSubscriptVowel (v, r)) (Mark.getAccentBreathingAllPair m)
    go _ = Nothing

isTwoSoundVowel :: Abstract.Vowel -> Bool
isTwoSoundVowel v
  =  v == Abstract.V_ι
  || v == Abstract.V_υ

diaeresisVowelParser :: UnitLetterParser UnitVocalicConsonant
diaeresisVowelParser = primUnitLetter "Diaeresis vowel" go
  where
    go u
      | (Left v, r) <- u ^. Unit.unitItem
      , isTwoSoundVowel v
      , m <- u ^. Unit.unitMarks
      , Just (Mark.DiaeresisAll, _) <- m ^. _3
      = Just $ Unit.Unit (Left $ OneVowel (v, r)) (Mark.getAccentBreathingAllPair m)
    go _ = Nothing

midTwoSoundFirstVowelParser :: UnitLetterParser (Abstract.Vowel, FileCharReference)
midTwoSoundFirstVowelParser = primUnitLetter "Mid two sound vowel 1" go
  where
    go u
      | (Left v, r) <- u ^. Unit.unitItem
      , m <- u ^. Unit.unitMarks
      , (Nothing, Nothing, Nothing) <- m
      = Just (v, r)
    go _ = Nothing

midTwoSoundSecondVowelParser :: UnitLetterParser ((Abstract.Vowel, FileCharReference), Mark.AccentBreathingAllPair)
midTwoSoundSecondVowelParser = primUnitLetter "Mid two sound vowel 2" go
  where
    go u
      | (Left v, r) <- u ^. Unit.unitItem
      , m <- u ^. Unit.unitMarks
      , Nothing <- m ^. _3
      = Just ((v, r), Mark.getAccentBreathingAllPair m)
    go _ = Nothing

midTwoSoundParser :: UnitLetterParser UnitVocalicConsonant
midTwoSoundParser = do
  v1p <- midTwoSoundFirstVowelParser
  (v2p, m) <- midTwoSoundSecondVowelParser
  return $ Unit.Unit (Left $ TwoVowel (v1p, v2p)) m

consonantParser :: UnitLetterParser UnitVocalicConsonant
consonantParser = primUnitLetter "Consonant" go
  where
    go u
      | (Right c, r) <- u ^. Unit.unitItem
      , m <- u ^. Unit.unitMarks
      , Nothing <- m ^. _3
      = Just $ Unit.Unit (Right (c, r)) (Mark.getAccentBreathingAllPair m)
    go _ = Nothing

singleVowelParser :: UnitLetterParser UnitVocalicConsonant
singleVowelParser = primUnitLetter "Single vowel" go
  where
    go u
      | (Left v, r) <- u ^. Unit.unitItem
      , False <- isTwoSoundVowel v
      , m <- u ^. Unit.unitMarks
      , Nothing <- m ^. _3
      = Just $ Unit.Unit (Left $ OneVowel (v, r)) (Mark.getAccentBreathingAllPair m)
    go _ = Nothing

vocalicConsonantParser :: UnitLetterParser UnitVocalicConsonant
vocalicConsonantParser
  =   consonantParser
  <|> iotaSubscriptParser
  <|> diaeresisVowelParser
  <|> try midTwoSoundParser
  <|> singleVowelParser
  <?> "VocalicConsonant"

parseVocalicSyllable :: [UnitLetter] -> Either ParseError [UnitVocalicConsonant]
parseVocalicSyllable = parse (many1 vocalicConsonantParser <* eof) ""
