module Text.Greek.Script.Mark where

import Control.Lens
import Text.Greek.FileReference
import Text.Greek.Script.Unicode

data AccentAll = AcuteAll | GraveAll | CircumflexAll deriving (Eq, Ord, Show)
type AccentAllPair = (AccentAll, FileCharReference)

data BreathingAll = SmoothAll | RoughAll deriving (Eq, Ord, Show)
type BreathingAllPair = (BreathingAll, FileCharReference)

data IotaSubscript = IotaSubscript deriving (Eq, Ord, Show)
data Diaeresis = Diaeresis deriving (Eq, Ord, Show)
data SyllabicAll = IotaSubscriptAll | DiaeresisAll deriving (Eq, Ord, Show)
type SyllabicAllPair = (SyllabicAll, FileCharReference)

type AllPair = (Maybe AccentAllPair, Maybe BreathingAllPair, Maybe SyllabicAllPair)
type All = (Maybe AccentAll, Maybe BreathingAll, Maybe SyllabicAll)

forgetAllReference :: AllPair -> All
forgetAllReference (a, b, s) = (fst <$> a, fst <$> b, fst <$> s)

data Error
  = ErrorDoubleAccent AccentAllPair AccentAllPair
  | ErrorDoubleBreathing BreathingAllPair BreathingAllPair
  | ErrorDoubleSyllabic SyllabicAllPair SyllabicAllPair
  deriving Show

toAllPair :: [(UnicodeMark, FileCharReference)] -> Either Error AllPair
toAllPair = foldr go $ Right (Nothing, Nothing, Nothing)
  where
    go :: (UnicodeMark, FileCharReference) -> Either Error AllPair -> Either Error AllPair
    go m b = b >>= combineAll m

combineAccentAll :: AccentAllPair -> AllPair -> Either Error AllPair
combineAccentAll x (Just x', _, _) = Left $ ErrorDoubleAccent x' x
combineAccentAll x a = Right $ set _1 (Just x) a

combineBreathingAll :: BreathingAllPair -> AllPair -> Either Error AllPair
combineBreathingAll x (_, Just x', _) = Left $ ErrorDoubleBreathing x' x
combineBreathingAll x a = Right $ set _2 (Just x) a

combineSyllabicAll :: SyllabicAllPair -> AllPair -> Either Error AllPair
combineSyllabicAll x (_, _, Just x') = Left $ ErrorDoubleSyllabic x' x
combineSyllabicAll x a = Right $ set _3 (Just x) a

combineAll :: (UnicodeMark, FileCharReference) -> AllPair -> Either Error AllPair
combineAll (U_Acute, r)         = combineAccentAll    (AcuteAll, r)
combineAll (U_Grave, r)         = combineAccentAll    (GraveAll, r)
combineAll (U_Circumflex, r)    = combineAccentAll    (CircumflexAll, r)
combineAll (U_Smooth, r)        = combineBreathingAll (SmoothAll, r)
combineAll (U_Rough, r)         = combineBreathingAll (RoughAll, r)
combineAll (U_IotaSubscript, r) = combineSyllabicAll  (IotaSubscriptAll, r)
combineAll (U_Diaeresis, r)     = combineSyllabicAll  (DiaeresisAll, r)

accentAllToUnicodeMark :: AccentAll -> UnicodeMark
accentAllToUnicodeMark AcuteAll      = U_Acute
accentAllToUnicodeMark GraveAll      = U_Grave
accentAllToUnicodeMark CircumflexAll = U_Circumflex

syllabicAllToUnicodeMark :: SyllabicAll -> UnicodeMark
syllabicAllToUnicodeMark DiaeresisAll     = U_Diaeresis
syllabicAllToUnicodeMark IotaSubscriptAll = U_IotaSubscript

breathingAllToUnicodeMark :: BreathingAll -> UnicodeMark
breathingAllToUnicodeMark SmoothAll = U_Smooth
breathingAllToUnicodeMark RoughAll  = U_Rough

type AccentBreathingAll = (Maybe AccentAll, Maybe BreathingAll)
type AccentBreathingAllPair = (Maybe AccentAllPair, Maybe BreathingAllPair)

getAccentBreathingAllPair :: AllPair -> AccentBreathingAllPair
getAccentBreathingAllPair (a, b, _) = (a, b)
