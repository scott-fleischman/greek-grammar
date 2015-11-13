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
import qualified Data.Functor.Identity as Functor
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text.Lazy as Lazy
import qualified Text.Greek.IO.Json as Json
import qualified Text.Greek.IO.Render as Render
import qualified Text.Greek.IO.Type as Type
import qualified Text.Greek.Source.All as All
import qualified Text.Greek.Source.Work as Work
import qualified Text.Greek.Script.Abstract as Abstract
import qualified Text.Greek.Script.Concrete as Concrete
import qualified Text.Greek.Script.Elision as Elision
import qualified Text.Greek.Script.Mark as Mark
import qualified Text.Greek.Script.Marked as Marked
import qualified Text.Greek.Script.Word as Word
import qualified Text.Greek.Script.Unicode as Unicode
import qualified Text.Greek.Utility as Utility

runProcess :: IO ()
runProcess = do
  result <- runExceptT process
  handleResult result

process :: ExceptT String IO ()
process = do
  sourceWords <- handleIOError All.loadAll
  _ <- liftIO $ putStrLn "Processing"

  let composedWords = toComposedWords sourceWords
  let decomposedWordPairs = toDecomposedWordPairs composedWords
  let decomposedWords = toDecomposedWords decomposedWordPairs
  markedLetterPairs <- handleError $ toMarkedLetterPairs decomposedWords
  let markedUnicodeLetters = toMarkedUnicodeLetters markedLetterPairs
  markedUnicodeConcretePairsL <- handleMaybe "Concrete Letter" $ toMarkedConcreteLetters markedUnicodeLetters
  markedUnicodeConcretePairsLM <- handleMaybe "Concrete Mark" $ toMarkedConcreteMarks markedUnicodeConcretePairsL
  let markedUnicodeConcretePairsB = toMarkedUnicodeConcretePairs markedUnicodeConcretePairsLM
  let markedConcreteLetters = Lens.over (wordSurfaceLens . traverse) snd markedUnicodeConcretePairsB
  let markedAbstractLetterPairs = Lens.over (wordSurfaceLens . traverse . Marked.item) (\x -> (x, Abstract.toLetterCaseFinal x)) markedConcreteLetters
  let markedAbstractLetters = Lens.over (wordSurfaceLens . traverse . Marked.item) snd markedAbstractLetterPairs
  let markedAbstractLetterMarkKindPairs = toMarkedAbstractLetterMarkKindPairs markedAbstractLetters
  let markedAbstractLetterMarkKinds = Lens.over (wordSurfaceLens . traverse . Marked.marks . traverse) snd markedAbstractLetterMarkKindPairs
  markedAbstractLetterMarkGroupPairs <- handleMaybe "Mark Group" $ dupApply (wordSurfaceLens . traverse . Marked.marks) Mark.toMarkGroup markedAbstractLetterMarkKinds
  let markedAbstractLetterMarkGroups = Lens.over (wordSurfaceLens . traverse . Marked.marks) snd markedAbstractLetterMarkGroupPairs

  let
    storedTypeDatas =
      [ makeWordPartType Type.SourceWord (pure . Word.getSourceInfoWord . Word.getSurface) sourceWords
      , makeWorkInfoType Type.WorkSource (Lens.view Lens._2) sourceWords
      , makeWorkInfoType Type.WorkTitle (Lens.view Lens._3) sourceWords
      , makeWordPartType Type.SourceFile (pure . _fileReferencePath . Word.getSourceInfoFile . Word.getSurface) sourceWords
      , makeWordPartType Type.SourceFileLocation (pure . (\(FileReference _ l1 l2) -> (l1, l2)) . Word.getSourceInfoFile . Word.getSurface) sourceWords
      , makeWordPartType Type.ParagraphNumber (pure . snd . snd . Word.getInfo) sourceWords
      , makeWordPartType Type.Elision (pure . getElision . fst . snd . Word.getInfo) sourceWords
      , makeWordPartType Type.UnicodeElision (getUnicodeElision . fst . snd . Word.getInfo) sourceWords
      , makeSurfaceType Type.UnicodeComposed composedWords

      , makeSurfaceType (Type.Function Type.UnicodeComposed (Type.List Type.UnicodeDecomposed)) decomposedWordPairs
      , makeSurfaceType Type.UnicodeDecomposed decomposedWords

      , makeSurfaceType (Type.Function (Type.List Type.UnicodeDecomposed) Type.UnicodeMarkedLetter) markedLetterPairs
      , makeSurfaceType Type.UnicodeMarkedLetter markedUnicodeLetters

      , makeSurfacePartType Type.UnicodeLetter (pure . Marked._item) markedUnicodeLetters
      , makeSurfacePartType Type.UnicodeMark Marked._marks markedUnicodeLetters
      , makeWordPartType Type.LetterCount (pure . Word.LetterCount . length . Word.getSurface) markedUnicodeLetters
      , makeWordPartType Type.MarkCount (pure . Word.MarkCount . sum . fmap (length . Marked._marks) . Word.getSurface) markedUnicodeLetters

      , makeSurfaceType (Type.Function Type.UnicodeMarkedLetter Type.ConcreteMarkedLetter) markedUnicodeConcretePairsB
      , makeSurfacePartType (Type.Function Type.UnicodeLetter Type.ConcreteLetter) (pure . Marked._item) markedUnicodeConcretePairsLM
      , makeSurfacePartType (Type.Function Type.UnicodeMark Type.ConcreteMark) Marked._marks markedUnicodeConcretePairsLM

      , makeSurfaceType Type.ConcreteMarkedLetter markedConcreteLetters
      , makeSurfacePartType Type.ConcreteLetter (pure . Marked._item) markedConcreteLetters
      , makeSurfacePartType Type.ConcreteMark Marked._marks markedConcreteLetters
      , makeSurfacePartType (Type.Function Type.ConcreteLetter Type.AbstractLetterCaseFinal) (pure . Marked._item) markedAbstractLetterPairs

      , makeSurfaceType Type.AbstractMarkedLetter markedAbstractLetters
      , makeSurfacePartType Type.AbstractLetter (pure . Lens.view (Marked.item . Lens._1)) markedAbstractLetters
      , makeSurfacePartType Type.LetterCase (pure . Lens.view (Marked.item . Lens._2)) markedAbstractLetters
      , makeIndexedSurfacePartType Type.LetterCase Abstract.CaseIndex (Lens.view (Marked.item . Lens._2)) markedAbstractLetters
      , makeSurfacePartType Type.LetterFinalForm (pure . Lens.view (Marked.item . Lens._3)) markedAbstractLetters
      , makeReverseIndexedSurfacePartType Type.LetterFinalForm Abstract.FinalReverseIndex (Lens.view (Marked.item . Lens._3)) markedAbstractLetters

      , makeSurfacePartType (Type.Function Type.ConcreteMark Type.MarkKind) Marked._marks markedAbstractLetterMarkKindPairs
      , makeSurfaceType (Type.AbstractLetterCaseFinalMarkKind) markedAbstractLetterMarkKinds

      , makeSurfacePartType (Type.Function (Type.List Type.MarkKind) (Type.MarkGroup)) (pure . Marked._marks) markedAbstractLetterMarkGroupPairs
      , makeSurfaceType (Type.AbstractLetterCaseFinalMarkGroup) markedAbstractLetterMarkGroups
      , makeWordPartType Type.AccentCount (pure . Mark.AccentCount . sum . fmap (maybeToOneOrZero . Lens.view (Marked.marks . Lens._1)) . Word.getSurface) markedAbstractLetterMarkGroups
      , makeWordPartType Type.BreathingCount (pure . Mark.BreathingCount . sum . fmap (maybeToOneOrZero . Lens.view (Marked.marks . Lens._2)) . Word.getSurface) markedAbstractLetterMarkGroups
      , makeWordPartType Type.SyllabicMarkCount (pure . Mark.SyllabicCount . sum . fmap (maybeToOneOrZero . Lens.view (Marked.marks . Lens._3)) . Word.getSurface) markedAbstractLetterMarkGroups
      ]
  let typeNameMap = Map.fromList . zip (fmap typeDataName storedTypeDatas) $ (fmap Json.TypeIndex [0..])
  let
    workInfoTypeIndexes = lookupAll typeNameMap
      [ Type.SourceWord
      , Type.WorkTitle
      , Type.ParagraphNumber
      , Type.WorkSource
      ]
  let
    summaryTypeIndexes = lookupAll typeNameMap
      [ Type.SourceWord
      , Type.AccentCount
      , Type.BreathingCount
      , Type.SyllabicMarkCount
      , Type.LetterCount
      , Type.MarkCount
      , Type.Elision
      , Type.ParagraphNumber
      ]
  let storedTypes = fmap typeDataJson storedTypeDatas
  let instanceMap = Json.makeInstanceMap storedTypes
  let ourWorks = getWorks summaryTypeIndexes instanceMap sourceWords
  let ourWorkInfos = fmap (Json.workToWorkInfo workInfoTypeIndexes) ourWorks
  let ourTypeInfos = fmap Json.makeTypeInfo storedTypes
  let ourIndex = Json.Index ourWorkInfos ourTypeInfos
  liftIO $ dumpData (Json.Data ourIndex ourWorks storedTypes)

type WordSurface a b = [Work.Indexed [Word.Indexed a b]]
type WordSurfaceBasic a = WordSurface Word.Basic a

maybeToOneOrZero :: Maybe a -> Int
maybeToOneOrZero Nothing = 0
maybeToOneOrZero (Just _) = 1

lookupAll :: Ord a => Map a b -> [a] -> [b]
lookupAll m = Maybe.catMaybes . fmap (flip Map.lookup m)

getElision :: Maybe a -> Elision.IsElided
getElision Nothing = Elision.NotElided
getElision _ = Elision.Elided

getUnicodeElision :: Maybe (Elision.ElisionChar, a) -> [Elision.ElisionChar]
getUnicodeElision Nothing = []
getUnicodeElision (Just (e, _)) = [e]

getWorks :: [Json.TypeIndex] -> Map WordLocation [(Json.TypeIndex, Json.ValueIndex)] -> [Work.Indexed [Word.Indexed Word.Basic a]] -> [Json.Work]
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

makeWorkInfoType :: (Ord a, Render.Render a) => Type.Name -> (Work.IndexSourceTitle -> a) -> [Work.Indexed [Word.Indexed b c]] -> TypeData
makeWorkInfoType t f = generateType t makeSimpleValue . flattenWords (\x _ -> f x)

makeWordPartType :: (Ord b, Render.Render b) => Type.Name -> (Word.Indexed t a -> [b]) -> WordSurface t a -> TypeData
makeWordPartType t f = generateType t makeSimpleValue . flatten . flattenWords (\_ x -> f x)
  where flatten = concatMap (\(l, m) -> fmap (\x -> (l, x)) m)

makeSurfaceType :: (Ord a, Render.Render a) => Type.Name -> WordSurface t [a] -> TypeData
makeSurfaceType t = generateType t makeSimpleValue . flattenSurface

makeSurfacePartType :: (Ord b, Render.Render b) => Type.Name -> (a -> [b]) -> WordSurface t [a] -> TypeData
makeSurfacePartType t f = generateType t makeSimpleValue . extract . flattenSurface
  where extract = concatMap (\(l, m) -> fmap (\x -> (l, x)) (f m))

makeIndexedSurfacePartType :: (Ord (i, b), Render.Render (i, b)) => Type.Name -> (Int -> i) -> (a -> b) -> WordSurface t [a] -> TypeData
makeIndexedSurfacePartType t g f
  = generateType (Type.Indexed t) makeSimpleValue
  . Lens.over (traverse . Lens._2 . Lens._1) g
  . concatIndexedSnd
  . flattenWords (\_ -> fmap f . Word.getSurface)

makeReverseIndexedSurfacePartType :: (Ord (i, b), Render.Render (i, b)) => Type.Name -> (Int -> i) -> (a -> b) -> WordSurface t [a] -> TypeData
makeReverseIndexedSurfacePartType t g f
  = generateType (Type.Indexed t) makeSimpleValue
  . Lens.over (traverse . Lens._2 . Lens._1) g
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

toMarkedLetterPairs
  :: WordSurfaceBasic [Unicode.Decomposed]
  -> Either Unicode.Error (WordSurfaceBasic [([Unicode.Decomposed], Marked.Unit Unicode.Letter [Unicode.Mark])])
toMarkedLetterPairs = wordSurfaceLens Unicode.parseMarkedLetters

toMarkedUnicodeLetters
  :: WordSurfaceBasic [([Unicode.Decomposed], Marked.Unit Unicode.Letter [Unicode.Mark])]
  -> WordSurfaceBasic [Marked.Unit Unicode.Letter [Unicode.Mark]]
toMarkedUnicodeLetters = Lens.over (wordSurfaceLens . traverse) snd

toMarkedConcreteLetters
  :: WordSurfaceBasic [Marked.Unit Unicode.Letter a]
  -> Maybe (WordSurfaceBasic [Marked.Unit (Unicode.Letter, Concrete.Letter) a])
toMarkedConcreteLetters = dupApply (wordSurfaceLens . traverse . Marked.item) Concrete.toMaybeLetter

toMarkedConcreteMarks
  :: WordSurfaceBasic [Marked.Unit a [Unicode.Mark]]
  -> Maybe (WordSurfaceBasic [Marked.Unit a [(Unicode.Mark, Concrete.Mark)]])
toMarkedConcreteMarks = dupApply (wordSurfaceLens . traverse . Marked.marks . traverse) Concrete.toMaybeMark

toMarkedUnicodeConcretePairs
  :: WordSurfaceBasic [Marked.Unit (Unicode.Letter, Concrete.Letter) [(Unicode.Mark, Concrete.Mark)]]
  -> WordSurfaceBasic [(Marked.Unit Unicode.Letter [Unicode.Mark], Marked.Unit Concrete.Letter [Concrete.Mark])]
toMarkedUnicodeConcretePairs = Lens.over (wordSurfaceLens . traverse) go
  where
    overBoth f g = Lens.over (Marked.marks . traverse) g . Lens.over Marked.item f
    go x = (overBoth fst fst x, overBoth snd snd x)

toMarkedAbstractLetterMarkKindPairs
  :: WordSurfaceBasic [Marked.Unit a [Concrete.Mark]]
  -> WordSurfaceBasic [Marked.Unit a ([(Concrete.Mark, Mark.Kind)])]
toMarkedAbstractLetterMarkKindPairs = dupApply' (wordSurfaceLens . traverse . Marked.marks . traverse) Mark.toKind

dupApply' :: ((d -> Functor.Identity (d, b)) -> a -> Functor.Identity c) -> (d -> b) -> a -> c
dupApply' a b = Functor.runIdentity . dupApply a (Functor.Identity . b)

dupApply :: Functor f => ((a -> f (a, b)) -> t) -> (a -> f b) -> t
dupApply lens f = lens (apply . dup)
  where
    apply = Lens._2 f
    dup x = (x, x)

wordSurfaceLens :: Applicative f =>
  (a -> f b)
  -> [Work.Indexed [Word.Indexed Word.Basic a]]
  -> f [Work.Indexed [Word.Indexed Word.Basic b]]
wordSurfaceLens = traverse . Work.content . traverse . Word.surface

type WordLocation = (Work.Index, Word.Index)
data Value
  = ValueSimple Text
  deriving (Eq, Ord, Show)
data TypeData = TypeData
  { typeDataName :: Type.Name
  , typeDataJson :: Json.Type
  }

generateType :: forall a. Ord a => Type.Name -> (a -> Value) -> [(WordLocation, a)] -> TypeData
generateType t f is = TypeData t $ Json.Type (Lazy.toStrict . Render.render $ t) (fmap storeValue typedValueInstances)
  where
    valueInstances :: [(a, [WordLocation])]
    valueInstances = Lens.over (traverse . Lens._2 . traverse) fst . Map.assocs . Utility.mapGroupBy snd $ is

    typedValueInstances :: [(Value, [WordLocation])]
    typedValueInstances = Lens.over (traverse . Lens._1) f valueInstances

    storeValue :: (Value, [WordLocation]) -> Json.Value
    storeValue ((ValueSimple vt), ls) = Json.Value vt (fmap locationToInstance ls)

    locationToInstance :: WordLocation -> Json.Instance
    locationToInstance = uncurry Json.Instance

flattenSurface :: forall a b. [Work.Indexed [Word.Indexed a [b]]] -> [(WordLocation, b)]
flattenSurface = concatSnd . flattenWords (\_ -> Word.getSurface)

concatSnd :: [(a, [b])] -> [(a, b)]
concatSnd = concatMap (\(x, ys) -> fmap (\y -> (x, y)) ys)

concatIndexedSnd :: [(a, [b])] -> [(a, (Int, b))]
concatIndexedSnd = concatMap (\(x, ys) -> fmap (\(i, y) -> (x, (i, y))) . zip [0..] $ ys)

concatReverseIndexedSnd :: [(a, [b])] -> [(a, (Int, b))]
concatReverseIndexedSnd = concatMap (\(x, ys) -> reverse . fmap (\(i, y) -> (x, (i, y))) . zip [0..] . reverse $ ys)

flattenWords :: forall a b c. (Work.IndexSourceTitle -> Word.Indexed a b -> c) -> [Work.Indexed [Word.Indexed a b]] -> [(WordLocation, c)]
flattenWords f = concatMap getIndexedWorkProps
  where
    getIndexedWorkProps :: Work.Indexed [Word.Indexed a b] -> [(WordLocation, c)]
    getIndexedWorkProps w = fmap (\(i, p) -> ((getWorkIndex w, i), p)) (getWorkProps w)

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

dumpData :: Json.Data -> IO ()
dumpData = Json.dumpJson
