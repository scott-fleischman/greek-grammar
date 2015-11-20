module Text.Greek.Script.Mark where

import qualified Text.Greek.Script.Concrete as Concrete

data Accent = AccentAcute | AccentGrave | AccentCircumflex deriving (Eq, Ord, Show)
data Breathing = BreathingSmooth | BreathingRough deriving (Eq, Ord, Show)
data Syllabic = SyllabicIotaSubscript | SyllabicDiaeresis deriving (Eq, Ord, Show)

type Group f = (f Accent, f Breathing, f Syllabic)

type AccentBreathing f = (f Accent, f Breathing)

data Kind = KindAccent Accent | KindBreathing Breathing | KindSyllabic Syllabic deriving (Eq, Ord, Show)

newtype AccentCount = AccentCount { getAccentCount :: Int } deriving (Eq, Show, Ord)
newtype BreathingCount = BreathingCount { getBreathingCount :: Int } deriving (Eq, Show, Ord)
newtype SyllabicCount = SyllabicCount { getSyllabicCount :: Int } deriving (Eq, Show, Ord)

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

forgetSyllabic :: Group f -> AccentBreathing f
forgetSyllabic (x, y, _) = (x, y)

data AcuteCircumflex = Acute | Circumflex deriving (Eq, Ord, Show)

accentNotGrave :: Accent -> Maybe AcuteCircumflex
accentNotGrave AccentGrave = Nothing
accentNotGrave AccentAcute = Just Acute
accentNotGrave AccentCircumflex = Just Circumflex

convertGraveToAcute :: Accent -> AcuteCircumflex
convertGraveToAcute AccentGrave = Acute
convertGraveToAcute AccentAcute = Acute
convertGraveToAcute AccentCircumflex = Circumflex

data WordAccent
  = WordAccentNone
  | WordAccentAcuteUltima
  | WordAccentAcutePenult
  | WordAccentAcuteAntepenult
  | WordAccentCircumflexUltima
  | WordAccentCircumflexPenult
  deriving (Eq, Ord, Show)

data UltimaAccented = IsUltimaAccented | NotUltimaAccented deriving (Eq, Ord, Show)
