-- {-# LANGUAGE RankNTypes #-}

module Text.Greek.Script.Syllable where

--import Control.Lens
--import Text.Greek.FileReference
--import Text.Greek.Parse.Utility
--import Text.Greek.Script.Unit (Unit)
--import Text.Parsec.Error (ParseError)
--import Text.Parsec.Prim
--import Text.Parsec.Combinator
--import qualified Text.Greek.Script.Abstract as Abstract
--import qualified Text.Greek.Script.Mark as Mark
--import qualified Text.Greek.Script.Unit as Unit

import qualified Text.Greek.Script.Abstract as Abstract
import qualified Text.Greek.Script.Mark as Mark

data StartVocalic v
  = StartVocalicSingle v
  | StartVocalicDiaeresis v
  | StartVocalicIota v
  | StartVocalicDiphthong v v
  deriving (Eq, Ord, Show)

instance Functor StartVocalic where
  fmap f (StartVocalicSingle v) = StartVocalicSingle (f v)
  fmap f (StartVocalicDiaeresis v) = StartVocalicDiaeresis (f v)
  fmap f (StartVocalicIota v) = StartVocalicIota (f v)
  fmap f (StartVocalicDiphthong v1 v2) = StartVocalicDiphthong (f v1) (f v2)

type Start m = Either (StartVocalic (Abstract.Vowel, m)) (Abstract.Consonant, m)

makeStartVocalic :: [(Abstract.VowelConsonant, Abstract.Case, Mark.Group Maybe)] -> [Start (Abstract.Case, Mark.Group Maybe)]
makeStartVocalic = foldr go []
  where
    go (Right a, b, c) xs = Right (a, (b, c)) : xs
    go (Left a, b, c@(_, _, Just Mark.SyllabicIotaSubscript)) xs = Left (StartVocalicIota (a, (b, c))) : xs
    go (Left a, b, c@(_, _, Just Mark.SyllabicDiaeresis)) xs = Left (StartVocalicDiaeresis (a, (b, c))) : xs
    go (Left a, b, c@(_, _, Nothing)) [] = Left (StartVocalicSingle (a, (b, c))) : []
    go (Left a, b, c@(_, _, Nothing)) (Left (StartVocalicSingle v@(vw, _)) : xs) | isCombiner vw = Left (StartVocalicDiphthong (a, (b, c)) v) : xs
    go (Left a, b, c) xs = Left (StartVocalicSingle (a, (b, c))) : xs

    isCombiner Abstract.V_ι = True
    isCombiner Abstract.V_υ = True
    isCombiner _ = False

data Diphthong = D_αι | D_αυ | D_ει | D_ευ | D_ηυ | D_οι | D_ου | D_υι deriving (Eq, Ord, Show)
data ImproperDiphthong = I_α | I_η | I_ω deriving (Eq, Ord, Show)

data Vocalic m
  = VocalicSingle Abstract.Vowel m
  | VocalicIota ImproperDiphthong m
  | VocalicDiphthong Diphthong m
  deriving (Eq, Ord, Show)

type VocalicConsonant mv mc = Either (Vocalic mv) (Abstract.Consonant, mc)

--validateVocalicConsonant :: [Start (Mark.Group Maybe)] -> Maybe [VocalicConsonant (Mark.AccentBreathing Maybe) (Maybe Mark.Breathing)]

--validateStartVocalic :: StartVocalic (Mark.Group Maybe) -> Maybe (Vocalic Mark.AccentBreathing)
--validateStartVocalic StartVocalicSingle

validateConsonantBreathing :: (Abstract.Consonant, Mark.Group Maybe) -> Maybe (Abstract.Consonant, Maybe Mark.Breathing)
validateConsonantBreathing (x, (Nothing, b, Nothing)) = Just (x, b)
validateConsonantBreathing _ = Nothing


--data Vocalic a b c
--  = OneVowel a
--  | IotaSubscriptVowel b
--  | TwoVowel c
--  deriving (Eq, Ord, Show)

--type VocalicSimple a = Vocalic a a (a, a)
--type VocalicVowel = VocalicSimple Abstract.Vowel
--type VocalicPair = VocalicSimple (Abstract.Vowel, FileCharReference)

--type VocalicConsonant = Either VocalicPair (Abstract.Consonant, FileCharReference)

--type UnitLetter = Unit.UnitLetter Abstract.VowelConsonant Mark.AllPair
--type UnitVocalicConsonant = Unit VocalicConsonant Mark.AccentBreathingAllPair

--type UnitLetterParser a = Parser [UnitLetter] a

--primUnitLetter :: String -> (UnitLetter -> Maybe a) -> Parser [UnitLetter] a
--primUnitLetter p = primMaybe p (^. Unit.unitItem . _2 . fileCharReferenceLine)

--isIotaSubscriptVowel :: Abstract.Vowel -> Bool
--isIotaSubscriptVowel v
--  =  v == Abstract.V_α
--  || v == Abstract.V_η
--  || v == Abstract.V_ω

--iotaSubscriptParser :: UnitLetterParser UnitVocalicConsonant
--iotaSubscriptParser = primUnitLetter "Iota subscript vowel" go
--  where
--    go u
--      | (Left v, r) <- u ^. Unit.unitItem
--      , isIotaSubscriptVowel v
--      , m <- u ^. Unit.unitMarks
--      , Just (Mark.IotaSubscriptAll, _) <- m ^. _3
--      = Just $ Unit.Unit (Left $ IotaSubscriptVowel (v, r)) (Mark.getAccentBreathingAllPair m)
--    go _ = Nothing

--isTwoSoundVowel :: Abstract.Vowel -> Bool
--isTwoSoundVowel v
--  =  v == Abstract.V_ι
--  || v == Abstract.V_υ

--diaeresisVowelParser :: UnitLetterParser UnitVocalicConsonant
--diaeresisVowelParser = primUnitLetter "Diaeresis vowel" go
--  where
--    go u
--      | (Left v, r) <- u ^. Unit.unitItem
--      , isTwoSoundVowel v
--      , m <- u ^. Unit.unitMarks
--      , Just (Mark.DiaeresisAll, _) <- m ^. _3
--      = Just $ Unit.Unit (Left $ OneVowel (v, r)) (Mark.getAccentBreathingAllPair m)
--    go _ = Nothing

--midTwoSoundFirstVowelParser :: UnitLetterParser (Abstract.Vowel, FileCharReference)
--midTwoSoundFirstVowelParser = primUnitLetter "Mid two sound vowel 1" go
--  where
--    go u
--      | (Left v, r) <- u ^. Unit.unitItem
--      , m <- u ^. Unit.unitMarks
--      , (Nothing, Nothing, Nothing) <- m
--      = Just (v, r)
--    go _ = Nothing

--midTwoSoundSecondVowelParser :: UnitLetterParser ((Abstract.Vowel, FileCharReference), Mark.AccentBreathingAllPair)
--midTwoSoundSecondVowelParser = primUnitLetter "Mid two sound vowel 2" go
--  where
--    go u
--      | (Left v, r) <- u ^. Unit.unitItem
--      , m <- u ^. Unit.unitMarks
--      , Nothing <- m ^. _3
--      = Just ((v, r), Mark.getAccentBreathingAllPair m)
--    go _ = Nothing

--midTwoSoundParser :: UnitLetterParser UnitVocalicConsonant
--midTwoSoundParser = do
--  v1p <- midTwoSoundFirstVowelParser
--  (v2p, m) <- midTwoSoundSecondVowelParser
--  return $ Unit.Unit (Left $ TwoVowel (v1p, v2p)) m

--consonantParser :: UnitLetterParser UnitVocalicConsonant
--consonantParser = primUnitLetter "Consonant" go
--  where
--    go u
--      | (Right c, r) <- u ^. Unit.unitItem
--      , m <- u ^. Unit.unitMarks
--      , Nothing <- m ^. _3
--      = Just $ Unit.Unit (Right (c, r)) (Mark.getAccentBreathingAllPair m)
--    go _ = Nothing

--singleVowelParser :: UnitLetterParser UnitVocalicConsonant
--singleVowelParser = primUnitLetter "Single vowel" go
--  where
--    go u
--      | (Left v, r) <- u ^. Unit.unitItem
--      , False <- isTwoSoundVowel v
--      , m <- u ^. Unit.unitMarks
--      , Nothing <- m ^. _3
--      = Just $ Unit.Unit (Left $ OneVowel (v, r)) (Mark.getAccentBreathingAllPair m)
--    go _ = Nothing

--vocalicConsonantParser :: UnitLetterParser UnitVocalicConsonant
--vocalicConsonantParser
--  =   consonantParser
--  <|> iotaSubscriptParser
--  <|> diaeresisVowelParser
--  <|> try midTwoSoundParser
--  <|> singleVowelParser
--  <?> "VocalicConsonant"

--parseVocalicSyllable :: [UnitLetter] -> Either ParseError [UnitVocalicConsonant]
--parseVocalicSyllable = parse (many1 vocalicConsonantParser <* eof) ""
