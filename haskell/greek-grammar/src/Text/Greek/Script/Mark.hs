module Text.Greek.Script.Mark where

import Control.Lens
import Text.Greek.Script.Unicode
import qualified Data.Set as S

data AccentAll = AcuteAll | GraveAll | CircumflexAll
data BreathingAll = SmoothAll | RoughAll
data Syllabic = IotaSubscript | Diaeresis

type All = (Maybe AccentAll, Maybe BreathingAll, Maybe Syllabic)

data MarkError
  = MarkErrorDoubleAccent AccentAll AccentAll
  | MarkErrorDoubleBreathing BreathingAll BreathingAll
  | MarkErrorDoubleSyllabic Syllabic Syllabic

toAll :: S.Set UnicodeMark -> Either MarkError All
toAll = foldr go $ Right (Nothing, Nothing, Nothing)
  where
    go :: UnicodeMark -> Either MarkError All -> Either MarkError All
    go m b = b >>= combineAll m

combineAccentAll :: AccentAll -> All -> Either MarkError All
combineAccentAll x (Just x', _, _) = Left $ MarkErrorDoubleAccent x' x
combineAccentAll x a = Right $ set _1 (Just x) a

combineBreathingAll :: BreathingAll -> All -> Either MarkError All
combineBreathingAll x (_, Just x', _) = Left $ MarkErrorDoubleBreathing x' x
combineBreathingAll x a = Right $ set _2 (Just x) a

combineSyllabicAll :: Syllabic -> All -> Either MarkError All
combineSyllabicAll x (_, _, Just x') = Left $ MarkErrorDoubleSyllabic x' x
combineSyllabicAll x a = Right $ set _3 (Just x) a

combineAll :: UnicodeMark -> All -> Either MarkError All
combineAll U_Acute = combineAccentAll AcuteAll
combineAll U_Grave = combineAccentAll GraveAll
combineAll U_Circumflex = combineAccentAll CircumflexAll
combineAll U_Smooth = combineBreathingAll SmoothAll
combineAll U_Rough = combineBreathingAll RoughAll
combineAll U_IotaSubscript = combineSyllabicAll IotaSubscript
combineAll U_Diaeresis = combineSyllabicAll Diaeresis
