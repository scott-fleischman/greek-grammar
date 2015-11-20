{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Text.Greek.Script.Syllable where

import qualified Control.Lens as Lens
import qualified Data.Maybe as Maybe
import qualified Text.Greek.Phonology.Consonant as Consonant
import qualified Text.Greek.Script.Abstract as Abstract
import qualified Text.Greek.Script.Mark as Mark
import qualified Text.Greek.Script.Place as Place
import qualified Text.Greek.Script.Punctuation as Punctuation
import qualified Text.Greek.Script.Word as Word

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

vocalicMarkLens :: forall a b f. Functor f => (a -> f b) -> Vocalic a -> f (Vocalic b)
vocalicMarkLens f (VocalicSingle v m) = (\m' -> VocalicSingle v m') <$> f m
vocalicMarkLens f (VocalicIota v m) = (\m' -> VocalicIota v m') <$> f m
vocalicMarkLens f (VocalicDiphthong v m) = (\m' -> VocalicDiphthong v m') <$> f m

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

getSyllableMark :: Syllable m a -> m
getSyllableMark (Syllable _ v _) = getVocalicMark v

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

syllableMarkLens :: forall m m' c f. Functor f => (m -> f m') -> Syllable m c -> f (Syllable m' c)
syllableMarkLens f (Syllable cl v cr) = (\m' -> Syllable cl (fmap (const m') v) cr) <$> f (getVocalicMark v)

type SyllableListOrConsonants m c = Either [Syllable m c] c
type SyllableOrConsonants m c = Either (Syllable m c) c

unifySyllableConsonant :: SyllableListOrConsonants m c -> [SyllableOrConsonants m c]
unifySyllableConsonant (Left ss) = fmap Left ss
unifySyllableConsonant (Right cs) = pure (Right cs)

mapSyllableMark :: (m -> m2) -> Syllable m c -> Syllable m2 c
mapSyllableMark f (Syllable c1 m c2) = Syllable c1 (fmap f m) c2

splitMedial :: ([c] -> ([c], [c])) -> [Syllable m [c]] -> Maybe [Syllable m [c]]
splitMedial split = foldr go (Just [])
  where
    go (Syllable _ _ (_:_)) (Just (Syllable [] _ _ : _)) = Nothing
    go (Syllable cl1 v1 []) (Just (Syllable cl2 v2 cr2 : xs)) | (l, r) <- split cl2 = Just (Syllable cl1 v1 l : Syllable r v2 cr2 : xs)
    go x (Just xs) = Just (x : xs)
    go _ Nothing = Nothing

makeSyllableMedialNext :: [VocalicEither m [c]] -> Maybe (SyllableListOrConsonants m [c])
makeSyllableMedialNext = tryFinish . foldr go (Just ([], []))
  where
    go (Left v) (Just (cs, ss)) = Just ([], Syllable [] v cs : ss)
    go (Right csl) (Just ([], Syllable [] v csr : ss)) = Just ([], Syllable csl v csr : ss)
    go (Right csr) (Just ([], [])) = Just (csr, [])
    go _ _ = Nothing

    tryFinish (Just ([], ss)) = Just . Left $ ss
    tryFinish (Just (cs@(_:_), [])) = Just . Right $ cs
    tryFinish _ = Nothing

getCrasis :: SyllableListOrConsonants (Mark.AccentBreathing Maybe) [c] -> Word.Crasis
getCrasis (Left ((Syllable (_:_) v _) : _))
  | (_, Just Mark.BreathingSmooth) <- getVocalicMark v = Word.HasCrasis
getCrasis _ = Word.NoCrasis

processBreathing :: SyllableListOrConsonants (Mark.AccentBreathing Maybe) [Consonant.PlusRoughRho]
  -> Maybe (SyllableListOrConsonants (Maybe Mark.Accent) [Consonant.PlusRoughRhoRoughBreathing])
processBreathing (Right r) = Just . Right . fmap Consonant.promotePlusRoughRho $ r
processBreathing (Left ss) = go ss >>= (pure . Left)
  where
    go ((Syllable [] v cr) : ss')
      | (_, Just Mark.BreathingRough) <- getVocalicMark v
      = (:) <$> pure (Syllable [Consonant.RB_Rough] (dropBreathing v) (promote cr)) <*> validateAllNoBreathing ss'
    go ((Syllable cl v cr) : ss')
      | smoothOrNone v
      = (:) <$> promoteC (Syllable cl (dropBreathing v) cr) <*> validateAllNoBreathing ss'
    go _ = Nothing

    promote = fmap Consonant.promotePlusRoughRho
    promoteC :: Syllable a [Consonant.PlusRoughRho]
      -> Maybe (Syllable a [Consonant.PlusRoughRhoRoughBreathing])
    promoteC (Syllable cl v cr) = Just (Syllable (promote cl) v (promote cr))

    dropBreathing = fmap fst

    smoothOrNone v | (_, Just Mark.BreathingSmooth) <- getVocalicMark v = True
    smoothOrNone v | (_, Nothing) <- getVocalicMark v = True
    smoothOrNone _ = False

    validateAllNoBreathing :: [Syllable (Mark.AccentBreathing Maybe) [Consonant.PlusRoughRho]]
      -> Maybe ([Syllable (Maybe Mark.Accent) [Consonant.PlusRoughRhoRoughBreathing]])
    validateAllNoBreathing = traverse validateNoBreathing

    validateNoBreathing :: Syllable (Mark.AccentBreathing Maybe) [Consonant.PlusRoughRho]
      -> Maybe (Syllable (Maybe Mark.Accent) [Consonant.PlusRoughRhoRoughBreathing])
    validateNoBreathing (Syllable cl v cr) | (_, Nothing) <- getVocalicMark v = promoteC $ (Syllable cl (dropBreathing v) cr)
    validateNoBreathing _ = Nothing

processGrave :: Punctuation.EndOfSentence -> Mark.Accent -> Maybe Mark.AcuteCircumflex
processGrave Punctuation.IsEndOfSentence = Mark.accentNotGrave
processGrave _ = Just . Mark.convertGraveToAcute

newtype ReverseIndex = ReverseIndex { getReverseIndex :: Int } deriving (Eq, Show, Ord)

applyReverseIndex :: [a] -> [(Int, a)]
applyReverseIndex = reverse . zip [0..] . reverse

getAccents :: SyllableListOrConsonants (Maybe Mark.AcuteCircumflex) c -> [Mark.AcuteCircumflex]
getAccents = Lens.toListOf (Lens._Left . traverse . syllableMarkLens . Lens._Just)

getAccentCount :: SyllableListOrConsonants (Maybe Mark.AcuteCircumflex) c -> Int
getAccentCount = length . getAccents

isDoubleAccentWithFinalAcute :: SyllableListOrConsonants (Maybe Mark.AcuteCircumflex) c -> Bool
isDoubleAccentWithFinalAcute s = length accents == 2 && checkLast accents
  where
    accents = getAccents s
    checkLast (Mark.Acute : _) = True
    checkLast _ = False

getTopLevelSyllableCount :: SyllableListOrConsonants (Maybe Mark.AcuteCircumflex) c -> Int
getTopLevelSyllableCount = length . Lens.toListOf (Lens._Left . traverse)

dropFinalAcute :: SyllableListOrConsonants (Maybe Mark.AcuteCircumflex) c
  -> SyllableListOrConsonants (Maybe Mark.AcuteCircumflex) c
dropFinalAcute = Lens.over Lens._Left dropLastAcute
  where
    dropLastAcute = reverse . dropFirstAcute . reverse
    dropFirstAcute (x : xs)
      | Just Mark.Acute <- getSyllableMark x
      = Lens.over syllableMarkLens (const Nothing) x : xs
    dropFirstAcute xs = xs

markInitialEnclitic :: [Word.Word Word.Sentence (SyllableListOrConsonants (Maybe Mark.AcuteCircumflex) c)]
  -> [Word.Word Word.WithEnclitic (SyllableListOrConsonants (Maybe Mark.AcuteCircumflex) c)]
markInitialEnclitic = foldr go []
  where
    go :: Word.Word Word.Sentence (SyllableListOrConsonants (Maybe Mark.AcuteCircumflex) c)
      -> [Word.Word Word.WithEnclitic (SyllableListOrConsonants (Maybe Mark.AcuteCircumflex) c)]
      -> [Word.Word Word.WithEnclitic (SyllableListOrConsonants (Maybe Mark.AcuteCircumflex) c)]
    go w (y : ys)
      | x <- Word.getSurface w
      , isDoubleAccentWithFinalAcute x
      , Word.UncertainEnclitic <- Lens.view (Word.info . Word.encliticLens) y
      , x' <- Word.getSurface y
      , 0 <- getAccentCount x'
      , i <- Word.getInfo w
      = Word.Word (Word.addInitialEnclitic Word.NotEnclitic i) (dropFinalAcute x)
        : (Lens.set (Word.info . Word.encliticLens) Word.IsEnclitic y) : ys
    go w ys
      | x <- Word.getSurface w
      , getAccentCount x /= 0 || getTopLevelSyllableCount x == 0
      = Lens.over Word.info (Word.addInitialEnclitic Word.NotEnclitic) w : ys
    go w ys
      = Lens.over Word.info (Word.addInitialEnclitic Word.UncertainEnclitic) w : ys

getWordAccent :: SyllableListOrConsonants (Maybe Mark.AcuteCircumflex) c
   -> Maybe Mark.WordAccent
getWordAccent (Right _) = Just Mark.WordAccentNone
getWordAccent (Left ss) = go . reverse . fmap getSyllableMark $ ss
  where
    go (Just Mark.Acute : xs) | allEmptyAccents xs = Just Mark.WordAccentAcuteUltima
    go (Just Mark.Circumflex : xs) | allEmptyAccents xs = Just Mark.WordAccentCircumflexUltima
    go (Nothing : Just Mark.Acute : xs) | allEmptyAccents xs = Just Mark.WordAccentAcutePenult
    go (Nothing : Just Mark.Circumflex : xs) | allEmptyAccents xs = Just Mark.WordAccentCircumflexPenult
    go (Nothing : Nothing : Just Mark.Acute : xs) | allEmptyAccents xs = Just Mark.WordAccentAcuteAntepenult
    go _ = Nothing

allEmptyAccents :: [Maybe Mark.AcuteCircumflex] -> Bool
allEmptyAccents = all Maybe.isNothing
