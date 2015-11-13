module Text.Greek.Script.Mark where

import qualified Text.Greek.Script.Concrete as Concrete

data Accent = AccentAcute | AccentGrave | AccentCircumflex deriving (Eq, Ord, Show)
data Breathing = BreathingSmooth | BreathingRough deriving (Eq, Ord, Show)
data Syllabic = SyllabicIotaSubscript | SyllabicDiaeresis deriving (Eq, Ord, Show)

type Group f = (f Accent, f Breathing, f Syllabic)

data Kind = KindAccent Accent | KindBreathing Breathing | KindSyllabic Syllabic deriving (Eq, Ord, Show)
data KindTag = KindTagAccent | KindTagBreathing | KindTagSyllabic

toKindTag :: Kind -> KindTag
toKindTag (KindAccent _) = KindTagAccent
toKindTag (KindBreathing _) = KindTagBreathing
toKindTag (KindSyllabic _) = KindTagSyllabic

toKind :: Concrete.Mark -> Kind
toKind Concrete.Acute = KindAccent AccentAcute
toKind Concrete.Grave = KindAccent AccentGrave
toKind Concrete.Circumflex = KindAccent AccentCircumflex
toKind Concrete.Smooth = KindBreathing BreathingSmooth
toKind Concrete.Rough = KindBreathing BreathingRough
toKind Concrete.IotaSubscript = KindSyllabic SyllabicIotaSubscript
toKind Concrete.Diaeresis= KindSyllabic SyllabicDiaeresis

toMarkGroup :: [Kind] -> Maybe (Group Maybe)
toMarkGroup = foldr go (Just (Nothing, Nothing, Nothing))
  where
    go (KindAccent a) (Just (Nothing, y, z)) = Just (Just a, y, z)
    go (KindBreathing b) (Just (x, Nothing, z)) = Just (x, Just b, z)
    go (KindSyllabic c) (Just (x, y, Nothing)) = Just (x, y, Just c)
    go _ _ = Nothing

--data AccentAll = AcuteAll | GraveAll | CircumflexAll deriving (Eq, Ord, Show)
--type AccentAllPair = (AccentAll, FileCharReference)

--data BreathingAll = SmoothAll | RoughAll deriving (Eq, Ord, Show)
--type BreathingAllPair = (BreathingAll, FileCharReference)

--data SyllabicAll = IotaSubscriptAll | DiaeresisAll deriving (Eq, Ord, Show)
--type SyllabicAllPair = (SyllabicAll, FileCharReference)

--type AllPair = (Maybe AccentAllPair, Maybe BreathingAllPair, Maybe SyllabicAllPair)
--type All = (Maybe AccentAll, Maybe BreathingAll, Maybe SyllabicAll)

--forgetAllReference :: AllPair -> All
--forgetAllReference (a, b, s) = (fst <$> a, fst <$> b, fst <$> s)

--data Error
--  = ErrorDoubleAccent AccentAllPair AccentAllPair
--  | ErrorDoubleBreathing BreathingAllPair BreathingAllPair
--  | ErrorDoubleSyllabic SyllabicAllPair SyllabicAllPair
--  deriving Show

--toAllPair :: [(Concrete.Mark, FileCharReference)] -> Either Error AllPair
--toAllPair = foldr go $ Right (Nothing, Nothing, Nothing)
--  where
--    go :: (Concrete.Mark, FileCharReference) -> Either Error AllPair -> Either Error AllPair
--    go m b = b >>= combineAll m

--combineAccentAll :: AccentAllPair -> AllPair -> Either Error AllPair
--combineAccentAll x (Just x', _, _) = Left $ ErrorDoubleAccent x' x
--combineAccentAll x a = Right $ set _1 (Just x) a

--combineBreathingAll :: BreathingAllPair -> AllPair -> Either Error AllPair
--combineBreathingAll x (_, Just x', _) = Left $ ErrorDoubleBreathing x' x
--combineBreathingAll x a = Right $ set _2 (Just x) a

--combineSyllabicAll :: SyllabicAllPair -> AllPair -> Either Error AllPair
--combineSyllabicAll x (_, _, Just x') = Left $ ErrorDoubleSyllabic x' x
--combineSyllabicAll x a = Right $ set _3 (Just x) a

--combineAll :: (Concrete.Mark, FileCharReference) -> AllPair -> Either Error AllPair
--combineAll (Concrete.Acute, r)         = combineAccentAll    (AcuteAll, r)
--combineAll (Concrete.Grave, r)         = combineAccentAll    (GraveAll, r)
--combineAll (Concrete.Circumflex, r)    = combineAccentAll    (CircumflexAll, r)
--combineAll (Concrete.Smooth, r)        = combineBreathingAll (SmoothAll, r)
--combineAll (Concrete.Rough, r)         = combineBreathingAll (RoughAll, r)
--combineAll (Concrete.IotaSubscript, r) = combineSyllabicAll  (IotaSubscriptAll, r)
--combineAll (Concrete.Diaeresis, r)     = combineSyllabicAll  (DiaeresisAll, r)

--accentAllToConcreteMark :: AccentAll -> Concrete.Mark
--accentAllToConcreteMark AcuteAll      = Concrete.Acute
--accentAllToConcreteMark GraveAll      = Concrete.Grave
--accentAllToConcreteMark CircumflexAll = Concrete.Circumflex

--syllabicAllToConcreteMark :: SyllabicAll -> Concrete.Mark
--syllabicAllToConcreteMark DiaeresisAll     = Concrete.Diaeresis
--syllabicAllToConcreteMark IotaSubscriptAll = Concrete.IotaSubscript

--breathingAllToConcreteMark :: BreathingAll -> Concrete.Mark
--breathingAllToConcreteMark SmoothAll = Concrete.Smooth
--breathingAllToConcreteMark RoughAll  = Concrete.Rough

--type AccentBreathingAll = (Maybe AccentAll, Maybe BreathingAll)
--type AccentBreathingAllPair = (Maybe AccentAllPair, Maybe BreathingAllPair)

--getAccentBreathingAllPair :: AllPair -> AccentBreathingAllPair
--getAccentBreathingAllPair (a, b, _) = (a, b)
