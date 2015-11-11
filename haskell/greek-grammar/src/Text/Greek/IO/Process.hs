{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Greek.IO.Process where

import Prelude hiding (words)
import Control.Monad.Except
import Data.Map (Map)
import Data.Text (Text)
import qualified Control.Lens as Lens
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text.Lazy as Lazy
import qualified Text.Greek.IO.Json as Json
import qualified Text.Greek.IO.Render as Render
import qualified Text.Greek.Source.All as All
import qualified Text.Greek.Source.Work as Work
import qualified Text.Greek.Script.Marked as Marked
import qualified Text.Greek.Script.Word as Word
import qualified Text.Greek.Script.Unicode as Unicode
import qualified Text.Greek.Utility as Utility

go :: IO ()
go = do
  result <- runExceptT process
  handleResult result

process :: ExceptT String IO ()
process = do
  sourceWords <- handleIOError All.loadAll
  _ <- liftIO $ putStrLn "Processing"

  let wordType = generateType "Source Word" ValueSimple . flattenWords (Word.getSource . Word.getSourceInfoWord . Word.getSurface) $ sourceWords
  let composedWords = toComposedWords sourceWords
  let decomposedWordPairs = toDecomposedWordPairs composedWords
  let decomposedWords = toDecomposedWords decomposedWordPairs
  markedLetterPairs <- handleError $ toMarkedLetterPairs decomposedWords
  let markedLetters = toMarkedLetters markedLetterPairs
  let
    storedTypes =
      [ wordType
      , toComposedType composedWords
      , toDecomposedFunctionType decomposedWordPairs
      , toDecomposedType decomposedWords
      , toMarkedLetterFunctionType markedLetterPairs
      , toMarkedLetterType markedLetters
      , toUnicodeLetterType markedLetters
      , toUnicodeMarkType markedLetters
      ]
  let instanceMap = Json.makeInstanceMap storedTypes
  let ourWorks = getWorks instanceMap sourceWords
  let workInfoTypeIndexes = Set.fromList . fmap Json.TypeIndex $ [0]
  let ourWorkInfos = fmap (Json.workToWorkInfo workInfoTypeIndexes) ourWorks
  let ourTypeInfos = fmap Json.makeTypeInfo storedTypes
  let ourIndex = Json.Index ourWorkInfos ourTypeInfos
  liftIO $ dumpData (Json.Data ourIndex ourWorks storedTypes)

type WordSurface a b = [Work.Indexed [Word.Indexed a b]]
type WordSurfaceBasic a = WordSurface Word.Basic a

getWorks :: Map WordLocation [(Json.TypeIndex, Json.ValueIndex)] -> [Work.Indexed [Word.Indexed Word.Basic a]] -> [Json.Work]
getWorks m works = workInfos
  where
    workInfos = fmap getWorkInfo works
    getWorkInfo (Work.Work (workIndex, workSource, workTitle) workWords) =
      Json.Work (Json.WorkSource workSource) workTitle (getWords workIndex workWords) (getWordGroups workWords) []
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

toComposedType :: WordSurfaceBasic [Unicode.Composed] -> Json.Type
toComposedType = generateType "Unicode Composed" makeSimpleValue
  . flattenSurface Word.getSurface

toDecomposedWordPairs
  :: WordSurfaceBasic [Unicode.Composed]
  -> WordSurfaceBasic [(Unicode.Composed, [Unicode.Decomposed])]
toDecomposedWordPairs = Lens.over (wordSurfaceLens . traverse) (\x -> (x, Unicode.decompose' x))

toDecomposedFunctionType
  :: WordSurfaceBasic [(Unicode.Composed, [Unicode.Decomposed])]
  -> Json.Type
toDecomposedFunctionType = generateType "Unicode Composed → [Unicode Decomposed]"
  makeSimpleValue
  . flattenSurface Word.getSurface

toDecomposedWords
  :: WordSurfaceBasic [(Unicode.Composed, [Unicode.Decomposed])]
  -> WordSurfaceBasic [Unicode.Decomposed]
toDecomposedWords = Lens.over wordSurfaceLens (concatMap snd)

toDecomposedType :: [Work.Indexed [Word.Indexed Word.Basic [Unicode.Decomposed]]] -> Json.Type
toDecomposedType = generateType "Unicode Decomposed" makeSimpleValue
  . flattenSurface Word.getSurface

toMarkedLetterPairs
  :: WordSurfaceBasic [Unicode.Decomposed]
  -> Either Unicode.Error (WordSurfaceBasic [([Unicode.Decomposed], Marked.Unit Unicode.Letter [Unicode.Mark])])
toMarkedLetterPairs = wordSurfaceLens Unicode.parseMarkedLetters

toMarkedLetterFunctionType
  :: WordSurfaceBasic [([Unicode.Decomposed], Marked.Unit Unicode.Letter [Unicode.Mark])]
  -> Json.Type
toMarkedLetterFunctionType = generateType "[Unicode Decomposed] → Unicode Letter, [Unicode Mark]"
  makeSimpleValue
  . flattenSurface Word.getSurface

toMarkedLetters
  :: WordSurfaceBasic [([Unicode.Decomposed], Marked.Unit Unicode.Letter [Unicode.Mark])]
  -> WordSurfaceBasic [Marked.Unit Unicode.Letter [Unicode.Mark]]
toMarkedLetters = Lens.over (wordSurfaceLens . traverse) snd

toMarkedLetterType :: WordSurfaceBasic [Marked.Unit Unicode.Letter [Unicode.Mark]] -> Json.Type
toMarkedLetterType = generateType "Unicode Marked Letter"
  makeSimpleValue
  . flattenSurface Word.getSurface

toUnicodeLetterType :: WordSurfaceBasic [Marked.Unit Unicode.Letter [Unicode.Mark]] -> Json.Type
toUnicodeLetterType = generateType "Unicode Letter"
  makeSimpleValue
  . Lens.over (traverse . Lens._2) Marked._item
  . flattenSurface Word.getSurface

toUnicodeMarkType :: WordSurfaceBasic [Marked.Unit Unicode.Letter [Unicode.Mark]] -> Json.Type
toUnicodeMarkType = generateType "Unicode Mark"
  makeSimpleValue
  . concatMap (\(l, m) -> fmap (\x -> (l, x)) (Marked._marks m))
  . flattenSurface Word.getSurface

wordSurfaceLens :: Applicative f =>
  (a -> f b)
  -> [Work.Indexed [Word.Indexed Word.Basic a]]
  -> f [Work.Indexed [Word.Indexed Word.Basic b]]
wordSurfaceLens = traverse . Work.content . traverse . Word.surface

type WordLocation = (Work.Index, Word.Index)
data Value
  = ValueSimple Text
  deriving (Eq, Ord, Show)

generateType :: forall a. Ord a => Text -> (a -> Value) -> [(WordLocation, a)] -> Json.Type
generateType t f is = Json.Type t (fmap storeValue typedValueInstances)
  where
    valueInstances :: [(a, [WordLocation])]
    valueInstances = Lens.over (traverse . Lens._2 . traverse) fst . Map.assocs . Utility.mapGroupBy snd $ is

    typedValueInstances :: [(Value, [WordLocation])]
    typedValueInstances = Lens.over (traverse . Lens._1) f valueInstances

    storeValue :: (Value, [WordLocation]) -> Json.Value
    storeValue ((ValueSimple vt), ls) = Json.Value vt (fmap locationToInstance ls)

    locationToInstance :: WordLocation -> Json.Instance
    locationToInstance = uncurry Json.Instance

flattenSurface :: forall a b c. (Word.Indexed a b -> [c]) -> [Work.Indexed [Word.Indexed a b]] -> [(WordLocation, c)]
flattenSurface f = concatSurface . flattenWords f

concatSurface :: [(a, [b])] -> [(a, b)]
concatSurface = concatMap (\(x, ys) -> fmap (\y -> (x, y)) ys)

flattenWords :: forall a b c. (Word.Indexed a b -> c) -> [Work.Indexed [Word.Indexed a b]] -> [(WordLocation, c)]
flattenWords f = concatMap getIndexedWorkProps
  where
    getIndexedWorkProps :: Work.Indexed [Word.Indexed a b] -> [(WordLocation, c)]
    getIndexedWorkProps w = fmap (\(i, p) -> ((getWorkIndex w, i), p)) (getWorkProps w)

    getWorkProps :: Work.Indexed [Word.Indexed a b] -> [(Word.Index, c)]
    getWorkProps = fmap getIndexedWordProp . Work.getContent

    getWorkIndex :: Work.Indexed x -> Work.Index
    getWorkIndex = Lens.view (Work.info . Lens._1)

    getIndexedWordProp :: Word.Indexed a b -> (Word.Index, c)
    getIndexedWordProp w = (getWordIndex w, f w)

    getWordIndex :: Word.Indexed a b -> Word.Index
    getWordIndex = Lens.view (Word.info . Lens._1)

handleResult :: Either String () -> IO ()
handleResult (Left e) = putStrLn e
handleResult (Right ()) = putStrLn "Complete"

handleIOError :: Show a => IO (Either a b) -> ExceptT String IO b
handleIOError x = liftIO x >>= handleError

handleError :: Show a => Either a b -> ExceptT String IO b
handleError (Left x) = throwError . show $ x
handleError (Right x) = return x

showError :: Show a => Either a b -> Either String b
showError = Lens.over Lens._Left show

dumpData :: Json.Data -> IO ()
dumpData = Json.dumpJson
