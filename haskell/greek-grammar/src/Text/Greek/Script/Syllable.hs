{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Text.Greek.Script.Syllable where

import qualified Control.Lens as Lens
import qualified Text.Greek.Script.Abstract as Abstract
import qualified Text.Greek.Script.Mark as Mark
import qualified Text.Greek.Script.Place as Place

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

makeStartVocalic :: [(Abstract.VowelConsonant, Mark.Group Maybe)] -> [Start (Mark.Group Maybe)]
makeStartVocalic = foldr go []
  where
    go (Right a, c) xs = Right (a, c) : xs
    go (Left a, c@(_, _, Just Mark.SyllabicIotaSubscript)) xs = Left (StartVocalicIota (a, c)) : xs
    go (Left a, c@(_, _, Just Mark.SyllabicDiaeresis)) xs = Left (StartVocalicDiaeresis (a, c)) : xs
    go (Left a, c@(_, _, Nothing)) [] = Left (StartVocalicSingle (a, c)) : []
    go (Left a, c@(_, _, Nothing)) (Left (StartVocalicSingle v@(vw, _)) : xs) | isCombiner vw = Left (StartVocalicDiphthong (a, c) v) : xs
    go (Left a, c) xs = Left (StartVocalicSingle (a, c)) : xs

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

getVocalicMark :: Vocalic m -> m
getVocalicMark (VocalicSingle _ m) = m
getVocalicMark (VocalicIota _ m) = m
getVocalicMark (VocalicDiphthong _ m) = m

type Vocalic' = Vocalic ()

instance Functor Vocalic where
  fmap f (VocalicSingle v m) = VocalicSingle v (f m)
  fmap f (VocalicIota v m) = VocalicIota v (f m)
  fmap f (VocalicDiphthong v m) = VocalicDiphthong v (f m)

type VocalicEither mv c = Either (Vocalic mv) c
type VocalicConsonant mv mc = VocalicEither mv (Abstract.Consonant, mc)

clusterConsonants :: [VocalicEither mv c] -> [VocalicEither mv [c]]
clusterConsonants = foldr go []
  where
    go (Left v) xs = Left v : xs
    go (Right c) (Right cs : xs) = Right (c : cs) : xs
    go (Right c) xs = Right [c] : xs

newtype Count = Count Int deriving (Eq, Ord, Show, Num)
newtype VocalicSingleCount = VocalicSingleCount Int deriving (Eq, Ord, Show, Num)
newtype ImproperDiphthongCount = ImproperDiphthongCount Int deriving (Eq, Ord, Show, Num)
newtype DiphthongCount = DiphthongCount Int deriving (Eq, Ord, Show, Num)


tagConsonantPositions :: [Either v c] -> [Either v (c, Place.Place3)]
tagConsonantPositions [] = []
tagConsonantPositions [x] = pure . Lens.over Lens._Right (flip (,) Place.initialFinal) $ x
tagConsonantPositions (x : xs) = (Lens.over Lens._Right (flip (,) Place.initial) x) : (reverse . tagReverseMedialFinal . reverse $ xs)

tagReverseMedialFinal :: [Either v c] -> [Either v (c, Place.Place3)]
tagReverseMedialFinal [] = []
tagReverseMedialFinal (x : xs) = (Lens.over Lens._Right (flip (,) Place.final) x) : (Lens.over (traverse . Lens._Right) (flip (,) Place.medial) xs)

getSyllableCount :: VocalicConsonant a b -> Count
getSyllableCount (Left _) = 1
getSyllableCount (Right _) = 0

getVocalicSingleCount :: VocalicConsonant a b -> VocalicSingleCount
getVocalicSingleCount (Left (VocalicSingle _ _)) = 1
getVocalicSingleCount _ = 0

getImproperDiphthongCount :: VocalicConsonant a b -> ImproperDiphthongCount
getImproperDiphthongCount (Left (VocalicIota _ _)) = 1
getImproperDiphthongCount _ = 0

getDiphthongCount :: VocalicConsonant a b -> DiphthongCount
getDiphthongCount (Left (VocalicDiphthong _ _)) = 1
getDiphthongCount _ = 0

validateVocalicConsonant :: Start (Mark.Group Maybe) -> Maybe (VocalicConsonant (Mark.AccentBreathing Maybe) (Maybe Mark.Breathing))
validateVocalicConsonant x = Lens._Left validateStartVocalic x >>= Lens._Right validateConsonantBreathing

validateStartVocalic :: StartVocalic (Abstract.Vowel, Mark.Group Maybe) -> Maybe (Vocalic (Mark.AccentBreathing Maybe))
validateStartVocalic (StartVocalicSingle (v, (a, b, Nothing))) =
  Just $ VocalicSingle v (a, b)
validateStartVocalic (StartVocalicDiaeresis (v, (a, b, Just Mark.SyllabicDiaeresis))) =
  Just $ VocalicSingle v (a, b)
validateStartVocalic (StartVocalicIota (v, (a, b, Just Mark.SyllabicIotaSubscript))) =
  VocalicIota <$> vowelToImproperDiphthong v <*> pure (a, b)
validateStartVocalic (StartVocalicDiphthong (v1, (Nothing, Nothing, Nothing)) (v2, (a, b, Nothing))) =
  VocalicDiphthong <$> vowelPairToDiphthong v1 v2 <*> pure (a, b)
validateStartVocalic _ =
  Nothing

vowelToImproperDiphthong :: Abstract.Vowel -> Maybe ImproperDiphthong
vowelToImproperDiphthong Abstract.V_α = Just I_α
vowelToImproperDiphthong Abstract.V_η = Just I_η
vowelToImproperDiphthong Abstract.V_ω = Just I_ω
vowelToImproperDiphthong _ = Nothing

vowelPairToDiphthong :: Abstract.Vowel -> Abstract.Vowel -> Maybe Diphthong
vowelPairToDiphthong Abstract.V_α Abstract.V_ι = Just D_αι
vowelPairToDiphthong Abstract.V_α Abstract.V_υ = Just D_αυ
vowelPairToDiphthong Abstract.V_ε Abstract.V_ι = Just D_ει
vowelPairToDiphthong Abstract.V_ε Abstract.V_υ = Just D_ευ
vowelPairToDiphthong Abstract.V_η Abstract.V_υ = Just D_ηυ
vowelPairToDiphthong Abstract.V_ο Abstract.V_ι = Just D_οι
vowelPairToDiphthong Abstract.V_ο Abstract.V_υ = Just D_ου
vowelPairToDiphthong Abstract.V_υ Abstract.V_ι = Just D_υι
vowelPairToDiphthong _ _ = Nothing

validateConsonantBreathing :: (Abstract.Consonant, Mark.Group Maybe) -> Maybe (Abstract.Consonant, Maybe Mark.Breathing)
validateConsonantBreathing (x, (Nothing, b, Nothing)) = Just (x, b)
validateConsonantBreathing _ = Nothing

vocalicToSingle :: Vocalic m -> [Abstract.Vowel]
vocalicToSingle (VocalicSingle v _) = [v]
vocalicToSingle _ = []

vocalicToImproperDiphthong :: Vocalic m -> [ImproperDiphthong]
vocalicToImproperDiphthong (VocalicIota d _) = [d]
vocalicToImproperDiphthong _ = []

vocalicToDiphthong :: Vocalic m -> [Diphthong]
vocalicToDiphthong (VocalicDiphthong d _) = [d]
vocalicToDiphthong _ = []

data Syllable m c = Syllable
  { syllableInitialConsonants :: c
  , syllableVocalic :: Vocalic m
  , syllableFinalConsonants :: c
  } deriving (Eq, Ord, Show)

mapSyllableMark :: (m -> m2) -> Syllable m c -> Syllable m2 c
mapSyllableMark f (Syllable c1 m c2) = Syllable c1 (fmap f m) c2

splitMedial :: ([c] -> ([c], [c])) -> [Syllable m [c]] -> Maybe [Syllable m [c]]
splitMedial split = foldr go (Just [])
  where
    go (Syllable _ _ (_:_)) (Just (Syllable [] _ _ : _)) = Nothing
    go (Syllable cl1 v1 []) (Just (Syllable cl2 v2 cr2 : xs)) | (l, r) <- split cl2 = Just (Syllable cl1 v1 l : Syllable r v2 cr2 : xs)
    go x (Just xs) = Just (x : xs)
    go _ Nothing = Nothing

makeSyllableMedialNext :: [VocalicEither m [c]] -> Maybe [Syllable m [c]]
makeSyllableMedialNext = tryGetSecond . foldr go (Just ([], []))
  where
    go (Left v) (Just (cs, ss)) = Just ([], Syllable [] v cs : ss)
    go (Right csl) (Just ([], Syllable [] v csr : ss)) = Just ([], Syllable csl v csr : ss)
    go (Right csr) (Just ([], [])) = Just (csr, [])
    go _ _ = Nothing

    tryGetSecond (Just ([], ss)) = Just ss
    tryGetSecond _ = Nothing
