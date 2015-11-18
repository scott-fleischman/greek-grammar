{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Text.Greek.IO.Process where

import Prelude hiding (words)
import Control.Monad.Except
import Data.Map (Map)
import Data.Text (Text)
import Text.Greek.Source.FileReference
import qualified Control.Lens as Lens
import qualified Data.Foldable as Foldable
import qualified Data.Functor.Identity as Functor
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text.Lazy as Lazy
--import qualified Data.Tuple as Tuple
import qualified Text.Greek.IO.Json as Json
import qualified Text.Greek.IO.Render as Render
import qualified Text.Greek.IO.Type as Type
import qualified Text.Greek.Source.All as All
import qualified Text.Greek.Source.Work as Work
--import qualified Text.Greek.Phonology.Consonant as Consonant
import qualified Text.Greek.Script.Abstract as Abstract
import qualified Text.Greek.Script.Concrete as Concrete
import qualified Text.Greek.Script.Elision as Elision
import qualified Text.Greek.Script.Mark as Mark
import qualified Text.Greek.Script.Marked as Marked
--import qualified Text.Greek.Script.Place as Place
--import qualified Text.Greek.Script.Syllable as Syllable
import qualified Text.Greek.Script.Word as Word
import qualified Text.Greek.Script.Unicode as Unicode
import qualified Text.Greek.Utility as Utility

runProcess :: IO ()
runProcess = do
  result <- runExceptT process
  handleResult result

--import qualified Data.Aeson as Aeson
--import qualified Data.ByteString.Lazy.Char8 as BL
--import qualified System.Directory as Directory
--import qualified Text.Greek.IO.Paths as Paths
--import System.FilePath ((</>))
--import qualified Codec.Compression.GZip as GZip
--  _ <- liftIO $ Directory.createDirectoryIfMissing True Paths.buildData

--writeCompressed :: Aeson.ToJSON a => String -> a -> IO ()
--writeCompressed n = BL.writeFile (Paths.buildData </> n) . GZip.compress . Aeson.encode

--readCompressed :: Aeson.FromJSON a => String -> ExceptT String IO a
--readCompressed n = do
--  bytes <- liftIO $ fmap GZip.decompress . BL.readFile $ Paths.buildData </> n
--  handleMaybe ("readCompressed " ++ n) $ Aeson.decode bytes

process :: ExceptT String IO ()
process = do
  sourceWords <- handleIOError All.loadAll
  _ <- liftIO $ putStrLn "Processing"

  let (stage0, composedWords) = makeStage0 sourceWords
  let (stage1, decomposedWords) = makeStage1 composedWords
  (stage2, unicodeLetterMarks) <- handleError $ tryMakeStage2 decomposedWords
  (stage3, _) <- handleMaybe "stage3" $ tryMakeStage3 unicodeLetterMarks -- concreteLetterConcreteMarks

  --let markedAbstractLetterPairs = Lens.over (wordSurfaceLens . traverse . Marked.item) (\x -> (x, Abstract.toLetterCaseFinal x)) markedConcreteLetters
  --let markedAbstractLettersCF = Lens.over (wordSurfaceLens . traverse . Marked.item) snd markedAbstractLetterPairs
  --capMarkedAbstractLettersF <- handleMaybe "IsCapitalized" $ toCapitalWord markedAbstractLettersCF
  --capMarkedAbstractLetters <- handleMaybe "FinalForm" $ validateFinalForm capMarkedAbstractLettersF

  --let markedAbstractLetterMarkKindPairs = toMarkedAbstractLetterMarkKindPairs capMarkedAbstractLetters
  --let markedAbstractLetterMarkKinds = Lens.over (wordSurfaceLens . traverse . Marked.marks . traverse) snd markedAbstractLetterMarkKindPairs
  --markedAbstractLetterMarkGroupPairs <- handleMaybe "Mark Group" $ dupApply (wordSurfaceLens . traverse . Marked.marks) Mark.toMarkGroup markedAbstractLetterMarkKinds
  --let markedAbstractLetterMarkGroups = Lens.over (wordSurfaceLens . traverse . Marked.marks) snd markedAbstractLetterMarkGroupPairs
  --let markedVowelConsonantMarkGroupPairs = dupApply' (wordSurfaceLens . traverse . Marked.item) Abstract.toVowelConsonant markedAbstractLetterMarkGroups
  --let vowelConsonantMarkGroup = Lens.over (wordSurfaceLens . traverse . Marked.item) snd markedVowelConsonantMarkGroupPairs
  --let startSyllable = Lens.over wordSurfaceLens (Syllable.makeStartVocalic . fmap (\(Marked.Unit a b) -> (a, b))) vowelConsonantMarkGroup
  --vocalicSyllableABConsonantBPair <- handleMaybe "Vocalic Syllable Consonant" $ dupApply (wordSurfaceLens . traverse) Syllable.validateVocalicConsonant startSyllable
  --let vocalicSyllableABConsonantB = Lens.over (wordSurfaceLens . traverse) snd vocalicSyllableABConsonantBPair
  --vocalicSyllableABConsonantRhPair <- handleMaybe "Reify Rho Rough" $ dupApply (wordSurfaceLens . traverse . Lens._Right) Consonant.reifyBreathing vocalicSyllableABConsonantB
  --let vocalicSyllableABConsonantRh = Lens.over (wordSurfaceLens . traverse . Lens._Right) snd vocalicSyllableABConsonantRhPair
  --let vocalicSyllableABConsonantCluster = Lens.over wordSurfaceLens Syllable.clusterConsonants vocalicSyllableABConsonantRh
  --let vocalicSyllableABConsonantClusterPlace3 = Lens.over wordSurfaceLens Syllable.tagConsonantPositions vocalicSyllableABConsonantCluster
  --let vocalicSyllableABConsonantClusterPlace3Swap = Lens.over (wordSurfaceLens . traverse . Lens._Right) Tuple.swap vocalicSyllableABConsonantClusterPlace3
  --let initialConsonantClusterSet = Place.getInitialSet . Lens.toListOf (wordSurfaceLens . traverse . Lens._Right) $ vocalicSyllableABConsonantClusterPlace3
  --let vocalicSyllableABConsonantClusterPlace4 = Lens.over (wordSurfaceLens . traverse . Lens._Right) (Place.applyAttestation initialConsonantClusterSet) vocalicSyllableABConsonantClusterPlace3
  --let vocalicSyllableABConsonantClusterMAI = Lens.over (wordSurfaceLens . traverse . Lens._Right . Lens._2) (\(_,b,_,d) -> (b,d)) vocalicSyllableABConsonantClusterPlace4

  --let
  --  storedTypeDatas =
  --    [ 

  --    , makeSurfacePartType Json.WordStagePartFunctionTypeKind (Type.Function Type.ConcreteLetter Type.AbstractLetterCaseFinal) (pure . Marked._item) markedAbstractLetterPair
  --    , makeSurfaceType Json.WordStageTypeKind Type.AbstractLetterCaseFinalMarks markedAbstractLettersCF
  --    , makeSurfacePartType Json.WordStagePartTypeKind Type.AbstractLetter (pure . Lens.view (Marked.item . Lens._1)) markedAbstractLettersCF
  --    , makeSurfacePartType Json.WordStagePartTypeKind Type.LetterCase (pure . Lens.view (Marked.item . Lens._2)) markedAbstractLettersCF
  --    , makeSurfacePartType Json.WordStagePartTypeKind Type.LetterFinalForm (pure . Lens.view (Marked.item . Lens._3)) markedAbstractLettersCF
  --    , makeIndexedSurfacePartType Json.CompositePropertyTypeKind Type.LetterCase Abstract.CaseIndex (Lens.view (Marked.item . Lens._2)) markedAbstractLettersCF
  --    , makeReverseIndexedSurfacePartType Json.CompositePropertyTypeKind Type.LetterFinalForm Abstract.FinalReverseIndex (Lens.view (Marked.item . Lens._3)) markedAbstractLettersCF
  --    , makeIndexedSurfacePartType Json.CompositePropertyTypeKind Type.AbstractLetter Abstract.LetterIndex (Lens.view (Marked.item . Lens._1)) markedAbstractLettersCF
  --    , makeReverseIndexedSurfacePartType Json.CompositePropertyTypeKind Type.AbstractLetter Abstract.LetterReverseIndex (Lens.view (Marked.item . Lens._1)) markedAbstractLettersCF

  --    , makeWordPartType Json.WordPropertyTypeKind Type.WordCapitalization (pure . Lens.view (Word.info . Lens._2 . Lens._4)) capMarkedAbstractLettersF

  --    , makeSurfacePartType Json.WordStageFunctionTypeKind (Type.Function Type.ConcreteMark Type.MarkKind) Marked._marks markedAbstractLetterMarkKindPairs
  --    , makeSurfaceType Json.WordStageTypeKind (Type.AbstractLetterMarkKinds) markedAbstractLetterMarkKinds

  --    , makeSurfacePartType Json.WordStageFunctionTypeKind (Type.Function (Type.List Type.MarkKind) (Type.MarkGroup)) (pure . Marked._marks) markedAbstractLetterMarkGroupPairs
  --    , makeSurfaceType Json.WordStageTypeKind (Type.AbstractLetterMarkGroup) markedAbstractLetterMarkGroups
  --    , makeWordPartType Json.WordPropertyTypeKind (Type.Count Type.Accent) (pure . Mark.AccentCount . sum . fmap (maybeToOneOrZero . Lens.view (Marked.marks . Lens._1)) . Word.getSurface) markedAbstractLetterMarkGroups
  --    , makeWordPartType Json.WordPropertyTypeKind (Type.Count Type.Breathing) (pure . Mark.BreathingCount . sum . fmap (maybeToOneOrZero . Lens.view (Marked.marks . Lens._2)) . Word.getSurface) markedAbstractLetterMarkGroups
  --    , makeWordPartType Json.WordPropertyTypeKind (Type.Count Type.SyllabicMark) (pure . Mark.SyllabicCount . sum . fmap (maybeToOneOrZero . Lens.view (Marked.marks . Lens._3)) . Word.getSurface) markedAbstractLetterMarkGroups

  --    , makeSurfacePartType Json.WordStageFunctionTypeKind (Type.Function Type.AbstractLetter Type.VowelConsonant) (pure . Marked._item) markedVowelConsonantMarkGroupPairs
  --    , makeSurfaceType Json.WordStageTypeKind Type.VowelConsonantMarkGroup vowelConsonantMarkGroup
  --    , makeSurfacePartType Json.WordStagePartTypeKind Type.VowelConsonant (pure . Marked._item) vowelConsonantMarkGroup
  --    , makeSurfacePartType Json.WordStagePartTypeKind Type.Vowel (Lens.toListOf (Marked.item . Lens._Left)) vowelConsonantMarkGroup
  --    , makeSurfacePartType Json.WordStagePartTypeKind Type.Consonant (Lens.toListOf (Marked.item . Lens._Right)) vowelConsonantMarkGroup
  --    , makeWordPartType Json.WordPropertyTypeKind (Type.Count Type.Vowel) (pure . Word.VowelCount . sum . fmap (length . (Lens.toListOf (Marked.item . Lens._Left))) . Word.getSurface) vowelConsonantMarkGroup
  --    , makeWordPartType Json.WordPropertyTypeKind (Type.Count Type.Consonant) (pure . Word.ConsonantCount . sum . fmap (length . (Lens.toListOf (Marked.item . Lens._Right))) . Word.getSurface) vowelConsonantMarkGroup
  --    , makeSurfacePartType Json.CompositePropertyTypeKind  Type.SyllabicMarkVowelConsonant getSyllabicMarkVowelConsonant vowelConsonantMarkGroup

  --    , makeSurfaceType Json.WordStageTypeKind Type.StartSyllable startSyllable
  --    , makeSurfaceType Json.WordStageFunctionTypeKind (Type.Function Type.StartSyllable Type.VocalicSyllableABConsonantB) vocalicSyllableABConsonantBPair
  --    , makeSurfaceType Json.WordStageTypeKind Type.VocalicSyllableABConsonantB vocalicSyllableABConsonantB
  --    , makeSurfacePartType Json.WordStagePartTypeKind Type.VocalicSyllable (Lens.toListOf Lens._Left) $ Lens.over (wordSurfaceLens . traverse . Lens._Left) (fmap (const ())) vocalicSyllableABConsonantB
  --    , makeSurfacePartType Json.WordStagePartTypeKind Type.VocalicSyllableSingle (concat . Lens.toListOf Lens._Left . Lens.over Lens._Left Syllable.vocalicToSingle) vocalicSyllableABConsonantB
  --    , makeSurfacePartType Json.WordStagePartTypeKind Type.ImproperDiphthong (concat . Lens.toListOf Lens._Left . Lens.over Lens._Left Syllable.vocalicToImproperDiphthong) vocalicSyllableABConsonantB
  --    , makeSurfacePartType Json.WordStagePartTypeKind Type.Diphthong (concat . Lens.toListOf Lens._Left . Lens.over Lens._Left Syllable.vocalicToDiphthong) vocalicSyllableABConsonantB
  --    , makeWordPartType Json.WordPropertyTypeKind (Type.Count Type.Syllable) (pure . sum . fmap Syllable.getSyllableCount . Word.getSurface) vocalicSyllableABConsonantB
  --    , makeWordPartType Json.WordPropertyTypeKind (Type.Count Type.VocalicSyllableSingle) (pure . sum . fmap Syllable.getVocalicSingleCount . Word.getSurface) vocalicSyllableABConsonantB
  --    , makeWordPartType Json.WordPropertyTypeKind (Type.Count Type.ImproperDiphthong) (pure . sum . fmap Syllable.getImproperDiphthongCount . Word.getSurface) vocalicSyllableABConsonantB
  --    , makeWordPartType Json.WordPropertyTypeKind (Type.Count Type.Diphthong) (pure . sum . fmap Syllable.getDiphthongCount . Word.getSurface) vocalicSyllableABConsonantB

  --    , makeSurfaceType Json.WordStageTypeKind Type.VocalicSyllableABConsonantRh vocalicSyllableABConsonantRh
  --    , makeSurfacePartType Json.WordStageFunctionTypeKind (Type.Function Type.ConsonantBreathing Type.ConsonantRh) (Lens.toListOf Lens._Right) vocalicSyllableABConsonantRhPair
  --    , makeSurfacePartType Json.WordStagePartTypeKind Type.ConsonantRh (Lens.toListOf Lens._Right) vocalicSyllableABConsonantRh

  --    , makeSurfaceType Json.WordStageTypeKind Type.VocalicSyllableABConsonantRhCluster vocalicSyllableABConsonantCluster
  --    , makeSurfacePartType Json.WordStagePartTypeKind Type.ConsonantRhCluster (Lens.toListOf Lens._Right) vocalicSyllableABConsonantCluster

  --    , makeSurfacePartType Json.CompositePropertyTypeKind Type.ConsonantRhClusterPlace3 (Lens.toListOf Lens._Right) vocalicSyllableABConsonantClusterPlace3
  --    , makeSurfacePartType Json.CompositePropertyTypeKind Type.ConsonantRhClusterPlace3Swap (Lens.toListOf Lens._Right) vocalicSyllableABConsonantClusterPlace3Swap
  --    , makeSurfacePartType Json.CompositePropertyTypeKind Type.ConsonantRhClusterPlaceInfo (fmap (\(a, (b, c)) -> (b, c, Consonant.splitScriptSyllableInfo a)) . Lens.toListOf Lens._Right) vocalicSyllableABConsonantClusterMAI
  --    ]

  let
    stages =
      [ stage0
      , stage1
      , stage2
      , stage3
      ]
  let indexedStages = indexStages stages
  let indexedTypeDatas = getIndexedStageTypeDatas indexedStages
  let storedTypeDatas = fmap snd indexedTypeDatas

  let typeNameMap = Map.fromList . fmap (\(i,t) -> (typeDataName t, i)) $ indexedTypeDatas

  workInfoTypeIndexes <- handleMaybe "workInfoTypeIndexes" $
    lookupAll typeNameMap
      [ Type.SourceWord
      , Type.WorkTitle
      , Type.ParagraphNumber
      , Type.WorkSource
      ]
  summaryTypeIndexes <- handleMaybe "summaryTypeIndexes" $
    lookupAll typeNameMap
      [ Type.SourceWord
--      , (Type.Count Type.Syllable)
--      , (Type.Count Type.VocalicSyllableSingle)
--      , (Type.Count Type.ImproperDiphthong)
--      , (Type.Count Type.Diphthong)
--      , (Type.Count Type.Vowel)
--      , (Type.Count Type.Consonant)
--      , Type.WordCapitalization      
--      , (Type.Count Type.Accent)
--      , (Type.Count Type.Breathing)
--      , (Type.Count Type.SyllabicMark)
--      , (Type.Count Type.AbstractLetter)
--      , (Type.Count Type.ConcreteMark)
--      , Type.Elision
      , Type.ParagraphNumber
      ]
  specialTypes <- handleMaybe "special types" $ Json.SpecialTypes
    <$> Map.lookup Type.SourceWord typeNameMap
    <*> Map.lookup Type.WordPrefix typeNameMap
    <*> Map.lookup Type.WordSuffix typeNameMap
  let storedTypes = fmap typeDataJson storedTypeDatas
  liftIO $ putStrLn "Writing types"
  liftIO $ Json.writeTypes storedTypes

  let instanceMap = Json.makeInstanceMap storedTypes
  let ourWorks = getWorks summaryTypeIndexes instanceMap sourceWords
  liftIO $ putStrLn "Writing works"
  liftIO $ Json.writeWorks ourWorks

  let ourWorkInfos = fmap (Json.workToWorkInfo workInfoTypeIndexes) ourWorks
  let ourTypeInfos = fmap Json.makeTypeInfo storedTypes
  let ourStageInfos = fmap getStageInfo indexedStages
  let ourIndex = Json.Index ourWorkInfos ourTypeInfos specialTypes ourStageInfos
  liftIO $ putStrLn "Writing index"
  liftIO $ Json.writeIndex ourIndex

type WordSurface a b = [Work.Indexed [Word.Indexed a b]]
type WordSurfaceBasic a = WordSurface Word.Basic a

data Stage a = Stage
  { stagePrimaryType :: a
  , stageParts :: [a]
  }

getStageInfo :: Stage (Json.TypeIndex, a) -> Json.StageInfo
getStageInfo (Stage p ps) = Json.StageInfo (fst p) (fmap fst ps)

makeStage0 :: [Work.Indexed [Word.Indexed Word.Basic Word.SourceInfo]]
  -> ( Stage TypeData
    , [Work.Indexed [Word.Indexed Word.Basic [Unicode.Composed]]]
    )
makeStage0 sourceWords = (stage, composedWords)
  where
    composedWords = toComposedWords sourceWords
    stage = Stage primaryType typeParts
    primaryType = makeSurfaceType Json.WordStageTypeKind Type.UnicodeComposed composedWords
    typeParts =
      [ makeWordPartType Json.WordPropertyTypeKind Type.SourceWord (pure . Word.getSourceInfoWord . Word.getSurface) sourceWords
      , makeWordPartType Json.WordPropertyTypeKind Type.SourceFile (pure . _fileReferencePath . Word.getSourceInfoFile . Word.getSurface) sourceWords
      , makeWordPartType Json.WordPropertyTypeKind Type.SourceFileLocation (pure . (\(FileReference _ l1 l2) -> (l1, l2)) . Word.getSourceInfoFile . Word.getSurface) sourceWords
      , makeWordPartType Json.WordPropertyTypeKind Type.ParagraphNumber (pure . snd . snd . Word.getInfo) sourceWords
      , makeWordPartType Json.WordPropertyTypeKind Type.WordPrefix (pure . fst . fst . snd . Word.getInfo) sourceWords
      , makeWordPartType Json.WordPropertyTypeKind Type.WordSuffix (pure . snd . fst . snd . Word.getInfo) sourceWords
      , makeWorkInfoType Json.WorkPropertyTypeKind Type.WorkSource (Lens.view Lens._2) sourceWords
      , makeWorkInfoType Json.WorkPropertyTypeKind Type.WorkTitle (Lens.view Lens._3) sourceWords
      ]

makeStage1 :: [Work.Indexed [Word.Indexed Word.Basic [Unicode.Composed]]]
  -> ( Stage TypeData
    , [Work.Indexed [Word.Indexed Word.Basic [Unicode.Decomposed]]]
    )
makeStage1 composedWords = (stage, decomposedWords)
  where
    decomposedWordPairs = toDecomposedWordPairs composedWords
    decomposedWords = toDecomposedWords decomposedWordPairs
    stage = Stage primaryType typeParts
    primaryType = makeSurfaceType Json.WordStageTypeKind Type.UnicodeDecomposed decomposedWords
    typeParts =
      [ makeSurfaceType Json.WordStageFunctionTypeKind (Type.Function Type.UnicodeComposed (Type.List Type.UnicodeDecomposed)) decomposedWordPairs
      ]

tryMakeStage2 :: WordSurface Word.Basic [Unicode.Decomposed]
  -> Either Unicode.Error
    ( Stage TypeData
    , WordSurface Word.Elision [Marked.Unit Unicode.Letter [Unicode.Mark]]
    )
tryMakeStage2 decomposedWords = (,) <$> mStage <*> mUnicodeLetterMarks
  where
    decomposedWordsE = splitDecomposedElision decomposedWords
    mUnicodeLetterMarksPairs = toUnicodeLetterMarksPairs decomposedWordsE
    mUnicodeLetterMarks = toUnicodeLetterMarks <$> mUnicodeLetterMarksPairs
    mStage = Stage <$> mPrimaryType <*> mTypeParts
    mPrimaryType = makeSurfaceType Json.WordStageTypeKind Type.UnicodeLetterMarks <$> mUnicodeLetterMarks
    mTypeParts = sequence
      [ makeSurfaceType Json.WordStageFunctionTypeKind (Type.Function (Type.List Type.UnicodeDecomposed) Type.UnicodeLetterMarks) <$> mUnicodeLetterMarksPairs
      , makeSurfacePartType Json.WordStagePartTypeKind Type.UnicodeLetter (pure . Marked._item) <$> mUnicodeLetterMarks
      , makeSurfacePartType Json.WordStagePartTypeKind Type.UnicodeMark Marked._marks <$> mUnicodeLetterMarks
      , makeWordPartType Json.WordPropertyTypeKind (Type.Count Type.AbstractLetter) (pure . Word.LetterCount . length . Word.getSurface) <$> mUnicodeLetterMarks
      , makeWordPartType Json.WordPropertyTypeKind (Type.Count Type.ConcreteMark) (pure . Word.MarkCount . sum . fmap (length . Marked._marks) . Word.getSurface) <$> mUnicodeLetterMarks
      , pure $ makeWordPartType Json.WordPropertyTypeKind Type.Elision (pure . Lens.view (Word.info . Lens._2 . Lens._3 . Lens._1)) decomposedWordsE
      , pure $ makeWordPartType Json.WordPropertyTypeKind Type.UnicodeElision (Lens.toListOf (Word.info . Lens._2 . Lens._3 . Lens._2 . Lens._Just)) decomposedWordsE
      ]

tryMakeStage3 :: WordSurface Word.Elision [Marked.Unit Unicode.Letter [Unicode.Mark]]
  -> Maybe
    ( Stage TypeData
    , WordSurface Word.Elision [Marked.Unit Concrete.Letter [Concrete.Mark]]
    )
tryMakeStage3 unicodeLetterMarks = (,) <$> mStage <*> mConcreteLetterConcreteMarks
  where
    mMarkedUnicodeConcretePairsLM = toMarkedConcreteLetters unicodeLetterMarks >>= toMarkedConcreteMarks
    mMarkedUnicodeConcretePairsB = toMarkedUnicodeConcretePairs <$> mMarkedUnicodeConcretePairsLM
    mConcreteLetterConcreteMarks = Lens.over (Lens._Just . wordSurfaceLens . traverse) snd mMarkedUnicodeConcretePairsB

    mStage = Stage <$> mPrimaryType <*> mTypeParts
    mPrimaryType = makeSurfaceType Json.WordStageTypeKind Type.ConcreteLetterMarks <$> mConcreteLetterConcreteMarks
    mTypeParts = sequence
      [ makeSurfaceType Json.WordStageFunctionTypeKind (Type.Function Type.UnicodeLetterMarks Type.ConcreteLetterMarks) <$> mMarkedUnicodeConcretePairsB
      , makeSurfacePartType Json.WordStagePartFunctionTypeKind (Type.Function Type.UnicodeLetter Type.ConcreteLetter) (pure . Marked._item) <$> mMarkedUnicodeConcretePairsLM
      , makeSurfacePartType Json.WordStagePartFunctionTypeKind (Type.Function Type.UnicodeMark Type.ConcreteMark) Marked._marks <$> mMarkedUnicodeConcretePairsLM
      , makeSurfacePartType Json.WordStagePartTypeKind Type.ConcreteLetter (pure . Marked._item) <$> mConcreteLetterConcreteMarks
      , makeSurfacePartType Json.WordStagePartTypeKind Type.ConcreteMark Marked._marks <$> mConcreteLetterConcreteMarks
      ]

getIndexedStageTypeDatas :: [Stage (Json.TypeIndex, TypeData)] -> [(Json.TypeIndex, TypeData)]
getIndexedStageTypeDatas = List.sortOn fst . concatMap getTypes
  where
    getTypes (Stage s ss) = s : ss

indexStage :: Json.TypeIndex -> Stage TypeData -> (Json.TypeIndex, Stage (Json.TypeIndex, TypeData))
indexStage i (Stage t parts) = (i', Stage (i, t) indexedParts)
  where
    (i', indexedParts) = Lens.over Lens._2 reverse . Foldable.foldl' go (i + 1, []) $ parts
    go (x, ps) p = (x + 1, (x, p) : ps)

indexStages :: [Stage TypeData] -> [Stage (Json.TypeIndex, TypeData)]
indexStages = reverse . snd . Foldable.foldl' go (0, [])
  where
    go (i, ss) s = (i', s' : ss)
      where
        (i', s') = indexStage i s

maybeToOneOrZero :: Maybe a -> Int
maybeToOneOrZero Nothing = 0
maybeToOneOrZero (Just _) = 1

lookupAll :: Ord a => Map a b -> [a] -> Maybe [b]
lookupAll m = traverse (flip Map.lookup m)

getWorks :: [Json.TypeIndex] -> Map WordLocation [(Json.TypeIndex, [Json.ValueIndex])] -> [Work.Indexed [Word.Indexed Word.Basic a]] -> [Json.Work]
getWorks summaryTypes m works = workInfos
  where
    workInfos = fmap getWorkInfo works
    getWorkInfo (Work.Work (workIndex, workSource, workTitle) workWords) =
      Json.Work workSource workTitle (getWords workIndex workWords) (getWordGroups workWords) summaryTypes
    getWords workIndex = fmap (getWord workIndex)
    getWord workIndex (Word.Word (i, _) _) = Json.Word . concat . Maybe.maybeToList . Map.lookup (workIndex, i) $ m

    getWordGroups ws = [Json.WordGroup "Paragraphs" (getParagraphs ws)]

    getParagraphs :: [Word.Indexed Word.Basic a] -> [[Word.Index]]
    getParagraphs = fmap snd . Map.toAscList . (fmap . fmap) (fst . Word.getInfo) . Utility.mapGroupBy (snd . snd . Word.getInfo)

toComposedWords
  :: WordSurfaceBasic Word.SourceInfo
  -> WordSurfaceBasic [Unicode.Composed]
toComposedWords = Lens.over wordSurfaceLens (Unicode.toComposed . Word.getSource . Word.getSourceInfoWord)

makeSimpleValue :: Render.Render a => a -> Value
makeSimpleValue = ValueSimple . Lazy.toStrict . Render.render

makeWorkInfoType :: (Ord a, Render.Render a) => Json.TypeKind -> Type.Name -> (Work.IndexSourceTitle -> a) -> [Work.Indexed [Word.Indexed b c]] -> TypeData
makeWorkInfoType k t f = generateType k t makeSimpleValue . flattenWords (\x _ -> f x)

makeWordPartType :: (Ord b, Render.Render b) => Json.TypeKind -> Type.Name -> (Word.Indexed t a -> [b]) -> WordSurface t a -> TypeData
makeWordPartType k t f = generateType k t makeSimpleValue . flatten . flattenWords (\_ x -> f x)
  where flatten = concatMap (\(l, m) -> fmap (\x -> (l, x)) m)

makeSurfaceType :: (Ord a, Render.Render a) => Json.TypeKind -> Type.Name -> WordSurface t [a] -> TypeData
makeSurfaceType k t = generateType k t makeSimpleValue . flattenSurface

makeSurfacePartType :: (Ord b, Render.Render b) => Json.TypeKind -> Type.Name -> (a -> [b]) -> WordSurface t [a] -> TypeData
makeSurfacePartType k t f = generateType k t makeSimpleValue . extract . flattenSurface
  where extract = concatMap (\(l, m) -> fmap (\x -> (l, x)) (f m))

makeIndexedSurfacePartType :: (Ord (b, i), Render.Render (b, i)) => Json.TypeKind -> Type.Name -> (Int -> i) -> (a -> b) -> WordSurface t [a] -> TypeData
makeIndexedSurfacePartType k t g f
  = generateType k (Type.Indexed t) makeSimpleValue
  . Lens.over (traverse . Lens._2 . Lens._2) g
  . concatIndexedSnd
  . flattenWords (\_ -> fmap f . Word.getSurface)

makeReverseIndexedSurfacePartType :: (Ord (b, i), Render.Render (b, i)) => Json.TypeKind -> Type.Name -> (Int -> i) -> (a -> b) -> WordSurface t [a] -> TypeData
makeReverseIndexedSurfacePartType k t g f
  = generateType k (Type.ReverseIndexed t) makeSimpleValue
  . Lens.over (traverse . Lens._2 . Lens._2) g
  . concatReverseIndexedSnd
  . flattenWords (\_ -> fmap f . Word.getSurface)

toDecomposedWordPairs
  :: WordSurfaceBasic [Unicode.Composed]
  -> WordSurfaceBasic [(Unicode.Composed, [Unicode.Decomposed])]
toDecomposedWordPairs = Lens.over (wordSurfaceLens . traverse) (\x -> (x, Unicode.decompose' x))

toDecomposedWords
  :: WordSurfaceBasic [(Unicode.Composed, [Unicode.Decomposed])]
  -> WordSurfaceBasic [Unicode.Decomposed]
toDecomposedWords = Lens.over wordSurfaceLens (concatMap snd)

splitDecomposedElision
  :: WordSurfaceBasic [Unicode.Decomposed]
  -> WordSurface Word.Elision [Unicode.Decomposed]
splitDecomposedElision = Lens.over (traverse . Work.content . traverse) go
  where
    go w = Word.addElisionPair e . Lens.set Word.surface as $ w
      where
        (e, as) = Elision.split Unicode.decomposed (Word.getSurface w)

toUnicodeLetterMarksPairs
  :: WordSurface b [Unicode.Decomposed]
  -> Either Unicode.Error (WordSurface b [([Unicode.Decomposed], Marked.Unit Unicode.Letter [Unicode.Mark])])
toUnicodeLetterMarksPairs = wordSurfaceLens Unicode.parseMarkedLetters

toUnicodeLetterMarks
  :: WordSurface b [([Unicode.Decomposed], Marked.Unit Unicode.Letter [Unicode.Mark])]
  -> WordSurface b [Marked.Unit Unicode.Letter [Unicode.Mark]]
toUnicodeLetterMarks = Lens.over (wordSurfaceLens . traverse) snd

toMarkedConcreteLetters
  :: WordSurface b [Marked.Unit Unicode.Letter a]
  -> Maybe (WordSurface b [Marked.Unit (Unicode.Letter, Concrete.Letter) a])
toMarkedConcreteLetters = dupApply (wordSurfaceLens . traverse . Marked.item) Concrete.toMaybeLetter

toMarkedConcreteMarks
  :: WordSurface b [Marked.Unit a [Unicode.Mark]]
  -> Maybe (WordSurface b [Marked.Unit a [(Unicode.Mark, Concrete.Mark)]])
toMarkedConcreteMarks = dupApply (wordSurfaceLens . traverse . Marked.marks . traverse) Concrete.toMaybeMark

toMarkedUnicodeConcretePairs
  :: WordSurface b [Marked.Unit (Unicode.Letter, Concrete.Letter) [(Unicode.Mark, Concrete.Mark)]]
  -> WordSurface b [(Marked.Unit Unicode.Letter [Unicode.Mark], Marked.Unit Concrete.Letter [Concrete.Mark])]
toMarkedUnicodeConcretePairs = Lens.over (wordSurfaceLens . traverse) go
  where
    overBoth f g = Lens.over (Marked.marks . traverse) g . Lens.over Marked.item f
    go x = (overBoth fst fst x, overBoth snd snd x)

toMarkedAbstractLetterMarkKindPairs
  :: WordSurface b [Marked.Unit a [Concrete.Mark]]
  -> WordSurface b [Marked.Unit a ([(Concrete.Mark, Mark.Kind)])]
toMarkedAbstractLetterMarkKindPairs = dupApply' (wordSurfaceLens . traverse . Marked.marks . traverse) Mark.toKind

toCapitalWord :: [Work.Indexed [Word.Indexed Word.Elision [Marked.Unit (t, Abstract.Case, t1) m0]]]
  -> Maybe [Work.Indexed [Word.Indexed Word.Capital [Marked.Unit (t, t1) m0]]]
toCapitalWord = fmap transferCapitalSurfaceToWord . toCapitalWordSurface

toCapitalWordSurface :: [Work.Indexed [Word.Indexed Word.Elision [Marked.Unit (t, Abstract.Case, t1) m0]]]
 -> Maybe [Work.Indexed [Word.Indexed Word.Elision (Word.IsCapitalized, [Marked.Unit (t, t1) m0])]]
toCapitalWordSurface = wordSurfaceLens (Abstract.validateIsCapitalized ((\(_,x,_) -> x) . Marked._item) (Lens.over Marked.item (\(x,_,y) -> (x,y))))

transferCapitalSurfaceToWord :: [Work.Indexed [Word.Indexed Word.Elision (Word.IsCapitalized, [Marked.Unit (t, t1) m0])]]
  -> [Work.Indexed [Word.Indexed Word.Capital [Marked.Unit (t, t1) m0]]]
transferCapitalSurfaceToWord = Lens.over (traverse . Work.content . traverse) setCapital
  where
    setCapital (Word.Word (wi, (a, p, e)) (c, m)) = Word.Word (wi, (a, p, e, c)) m

validateFinalForm :: [Work.Indexed [Word.Indexed a [Marked.Unit (t, Abstract.Final) m0]]]
  -> Maybe [Work.Indexed [Word.Indexed a [Marked.Unit t m0]]]
validateFinalForm = wordSurfaceLens $ Abstract.validateLetterFinal (Lens.view $ Marked.item . Lens._2) (Lens.over Marked.item fst)

getSyllabicMarkVowelConsonant :: Marked.Unit Abstract.VowelConsonant (Mark.Group Maybe) -> [(Mark.Syllabic, Abstract.VowelConsonant)]
getSyllabicMarkVowelConsonant (Marked.Unit vc (_, _, Just m)) = pure (m, vc)
getSyllabicMarkVowelConsonant _ = mempty


dupApply' :: ((d -> Functor.Identity (d, b)) -> a -> Functor.Identity c) -> (d -> b) -> a -> c
dupApply' a b = Functor.runIdentity . dupApply a (Functor.Identity . b)

dupApply :: Functor f => ((a -> f (a, b)) -> t) -> (a -> f b) -> t
dupApply lens f = lens (apply . dup)
  where
    apply = Lens._2 f
    dup x = (x, x)

wordSurfaceLens :: Applicative f =>
  (a -> f b)
  -> [Work.Indexed [Word.Indexed c a]]
  -> f [Work.Indexed [Word.Indexed c b]]
wordSurfaceLens = traverse . Work.content . traverse . Word.surface

type WordLocation = (Work.Index, Word.Index)
data Value
  = ValueSimple Text
  deriving (Eq, Ord, Show)
data TypeData = TypeData
  { typeDataName :: Type.Name
  , typeDataJson :: Json.Type
  }

generateType :: forall a. Ord a => Json.TypeKind -> Type.Name -> (a -> Value) -> [(Json.Instance, a)] -> TypeData
generateType k t f is = TypeData t $ Json.Type (Lazy.toStrict . Render.render $ t) k (fmap storeValue typedValueInstances)
  where
    valueInstances :: [(a, [Json.Instance])]
    valueInstances = Lens.over (traverse . Lens._2 . traverse) fst . Map.assocs . Utility.mapGroupBy snd $ is

    typedValueInstances :: [(Value, [Json.Instance])]
    typedValueInstances = Lens.over (traverse . Lens._1) f valueInstances

    storeValue :: (Value, [Json.Instance]) -> Json.Value
    storeValue ((ValueSimple vt), ls) = Json.Value vt ls

flattenSurface :: forall a b. [Work.Indexed [Word.Indexed a [b]]] -> [(Json.Instance, b)]
flattenSurface = concatInstanceValues . flattenWords (\_ -> Word.getSurface)

concatInstanceValues :: [(Json.Instance, [b])] -> [(Json.Instance, b)]
concatInstanceValues = concatMap (\(x, ys) -> mapAtomIndexes x ys)

mapAtomIndexes :: Json.Instance -> [t] -> [(Json.Instance, t)]
mapAtomIndexes a = fmap (\(i, y) -> (setAtomIndex i a, y)) . zip [0..]
  where
    setAtomIndex z (Json.Instance x y _) = Json.Instance x y (Just . Json.AtomIndex $ z)

concatIndexedSnd :: [(Json.Instance, [b])] -> [(Json.Instance, (b, Int))]
concatIndexedSnd = concatMap (\(x, ys) -> fmap (\(i, (a, b)) -> (a, (b, i))) . zip [0..] . mapAtomIndexes x $ ys)

concatReverseIndexedSnd :: [(Json.Instance, [b])] -> [(Json.Instance, (b, Int))]
concatReverseIndexedSnd = concatMap (\(x, ys) -> reverse . fmap (\(i, (a, b)) -> (a, (b, i))) . zip [0..] . reverse . mapAtomIndexes x $ ys)

flattenWords :: forall a b c. (Work.IndexSourceTitle -> Word.Indexed a b -> c) -> [Work.Indexed [Word.Indexed a b]] -> [(Json.Instance, c)]
flattenWords f = concatMap getIndexedWorkProps
  where
    getIndexedWorkProps :: Work.Indexed [Word.Indexed a b] -> [(Json.Instance, c)]
    getIndexedWorkProps w = fmap (\(i, p) -> (Json.Instance (getWorkIndex w) i Nothing, p)) (getWorkProps w)

    getWorkProps :: Work.Indexed [Word.Indexed a b] -> [(Word.Index, c)]
    getWorkProps k = fmap (getIndexedWordProp (Work.getInfo k)) . Work.getContent $ k

    getWorkIndex :: Work.Indexed x -> Work.Index
    getWorkIndex = Lens.view (Work.info . Lens._1)

    getIndexedWordProp :: Work.IndexSourceTitle -> Word.Indexed a b -> (Word.Index, c)
    getIndexedWordProp k d = (getWordIndex d, f k d)

    getWordIndex :: Word.Indexed a b -> Word.Index
    getWordIndex = Lens.view (Word.info . Lens._1)

handleResult :: Either String () -> IO ()
handleResult (Left e) = putStrLn e
handleResult (Right ()) = putStrLn "Complete"

handleIOError :: Show a => IO (Either a b) -> ExceptT String IO b
handleIOError x = liftIO x >>= handleError

handleMaybe :: String -> Maybe a -> ExceptT String IO a
handleMaybe s = handleError . Utility.maybeToEither s

handleError :: Show a => Either a b -> ExceptT String IO b
handleError (Left x) = throwError . show $ x
handleError (Right x) = return x

showError :: Show a => Either a b -> Either String b
showError = Lens.over Lens._Left show
