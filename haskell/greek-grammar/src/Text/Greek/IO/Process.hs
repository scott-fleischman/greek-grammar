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
import qualified Data.Text as Text
import qualified Text.Greek.IO.Json as Json
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
  let workInfos = getWorkInfos wordType sourceWords
  let composedWords = toComposedWords sourceWords
  let decomposedWordPairs = toDecomposedWordPairs composedWords
  let decomposedWords = toDecomposedWords decomposedWordPairs
  markedLetterPairs <- handleError $ toMarkedLetterPairs decomposedWords
  let markedLetters = toMarkedLetters markedLetterPairs
  let
    storedTypes =
      [ storeType wordType
      , storeType (toComposedType composedWords)
      , storeType (toDecomposedFunctionType decomposedWordPairs)
      , storeType (toDecomposedType decomposedWords)
      , storeType (toMarkedLetterFunctionType markedLetterPairs)
      , storeType (toMarkedLetterType markedLetters)
      , storeType (toUnicodeLetterType markedLetters)
      , storeType (toUnicodeMarkType markedLetters)
      ]
  let instanceMap = Json.makeInstanceMap storedTypes
  let ourWorks = getWorks instanceMap sourceWords
  let ourIndex = Json.Index workInfos . fmap Json.makeTypeInfo $ storedTypes
  liftIO $ dumpData (Json.Data ourIndex ourWorks storedTypes)

type WordSurface a b = [Work.Indexed [Word.Indexed a b]]
type WordSurfaceBasic a = WordSurface Word.Basic a

getWorkInfos :: Type Text -> WordSurfaceBasic Word.SourceInfo -> [Json.WorkInfo]
getWorkInfos wordType = fmap (toWorkInfo wordType)

toWorkInfo :: Type Text -> Work.Indexed [Word.Indexed Word.Basic Word.SourceInfo] -> Json.WorkInfo
toWorkInfo wordType (Work.Work (_, s, t) ws) = Json.WorkInfo t (Json.WorkSource s) contexts typeIndexes wordInfos
  where
    contexts = []
    typeIndexes = [Json.TypeIndex 0]
    wordInfos = fmap toSourceWord ws

    toSourceWord :: Word.Indexed Word.Basic Word.SourceInfo -> Json.WordInfo
    toSourceWord (Word.Word _ (Word.SourceInfo (Word.Source x) f)) = Json.WordInfo wordTypeValues contextIndex f
      where
        contextIndex = Json.WordContextIndex 0
        wordTypeValues = fmap (Json.ValueIndex . getValueIndex) . Maybe.maybeToList $ lookupValueIndex wordType x

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

toComposedType :: WordSurfaceBasic [Unicode.Composed] -> Type Unicode.Composed
toComposedType = generateType "Unicode Composed" (ValueSimple . Json.titleUnicodeDetail . Unicode.composed)
  . flattenSurface Word.getSurface

toDecomposedWordPairs
  :: WordSurfaceBasic [Unicode.Composed]
  -> WordSurfaceBasic [(Unicode.Composed, [Unicode.Decomposed])]
toDecomposedWordPairs = Lens.over (wordSurfaceLens . traverse) (\x -> (x, Unicode.decompose' x))

toDecomposedFunctionType
  :: WordSurfaceBasic [(Unicode.Composed, [Unicode.Decomposed])]
  -> Type (Unicode.Composed, [Unicode.Decomposed])
toDecomposedFunctionType = generateType "Unicode Composed → [Unicode Decomposed]"
  (ValueSimple . Json.formatFunction (Json.titleUnicodeDetail . Unicode.composed) (Json.formatList (Json.titleUnicodeDetail . Unicode.decomposed)))
  . flattenSurface Word.getSurface

toDecomposedWords
  :: WordSurfaceBasic [(Unicode.Composed, [Unicode.Decomposed])]
  -> WordSurfaceBasic [Unicode.Decomposed]
toDecomposedWords = Lens.over wordSurfaceLens (concatMap snd)

toDecomposedType :: [Work.Indexed [Word.Indexed Word.Basic [Unicode.Decomposed]]] -> Type Unicode.Decomposed
toDecomposedType = generateType "Unicode Decomposed" (ValueSimple . Json.titleUnicodeDetail . Unicode.decomposed)
  . flattenSurface Word.getSurface

toMarkedLetterPairs
  :: WordSurfaceBasic [Unicode.Decomposed]
  -> Either Unicode.Error (WordSurfaceBasic [([Unicode.Decomposed], Marked.Unit Unicode.Letter [Unicode.Mark])])
toMarkedLetterPairs = wordSurfaceLens Unicode.parseMarkedLetters

toMarkedLetterFunctionType
  :: WordSurfaceBasic [([Unicode.Decomposed], Marked.Unit Unicode.Letter [Unicode.Mark])]
  -> Type ([Unicode.Decomposed], Marked.Unit Unicode.Letter [Unicode.Mark])
toMarkedLetterFunctionType = generateType "[Unicode Decomposed] → Unicode Letter, [Unicode Mark]"
  (ValueSimple . Json.formatFunction (Json.formatList (Json.titleUnicodeDetail . Unicode.decomposed)) titleMarkedLetter)
  . flattenSurface Word.getSurface

toMarkedLetters
  :: WordSurfaceBasic [([Unicode.Decomposed], Marked.Unit Unicode.Letter [Unicode.Mark])]
  -> WordSurfaceBasic [Marked.Unit Unicode.Letter [Unicode.Mark]]
toMarkedLetters = Lens.over (wordSurfaceLens . traverse) snd

toMarkedLetterType :: WordSurfaceBasic [Marked.Unit Unicode.Letter [Unicode.Mark]] -> Type (Marked.Unit Unicode.Letter [Unicode.Mark])
toMarkedLetterType = generateType "Unicode Marked Letter"
  (ValueSimple . titleMarkedLetter)
  . flattenSurface Word.getSurface

toUnicodeLetterType :: WordSurfaceBasic [Marked.Unit Unicode.Letter [Unicode.Mark]] -> Type Unicode.Letter
toUnicodeLetterType = generateType "Unicode Letter"
  (ValueSimple . Json.titleUnicodeDetail . Unicode.getLetter)
  . Lens.over (traverse . Lens._2) Marked._item
  . flattenSurface Word.getSurface

toUnicodeMarkType :: WordSurfaceBasic [Marked.Unit Unicode.Letter [Unicode.Mark]] -> Type Unicode.Mark
toUnicodeMarkType = generateType "Unicode Mark"
  (ValueSimple . Json.titleUnicodeDetail . Unicode.getMark)
  . concatMap (\(l, m) -> fmap (\x -> (l, x)) (Marked._marks m))
  . flattenSurface Word.getSurface

titleMarkedLetter :: Marked.Unit Unicode.Letter [Unicode.Mark] -> Text
titleMarkedLetter (Marked.Unit l ms) = Text.concat
  [ Json.titleUnicodeDetail . Unicode.getLetter $ l
  , ", ["
  , Text.intercalate ", " (fmap (Json.titleUnicodeDetail . Unicode.getMark) ms)
  , "]"
  ]

wordSurfaceLens :: Applicative f =>
  (a -> f b)
  -> [Work.Indexed [Word.Indexed Word.Basic a]]
  -> f [Work.Indexed [Word.Indexed Word.Basic b]]
wordSurfaceLens = traverse . Work.content . traverse . Word.surface

type WordLocation = (Work.Index, Word.Index)
newtype ValueIndex = ValueIndex { getValueIndex :: Int } deriving (Eq, Ord, Show)
data Value
  = ValueSimple Text
  deriving (Eq, Ord, Show)
data Type a = Type
  { typeTitle :: Text
  , typeInstances :: [(WordLocation, a)]
  , typeValueMap :: Map a ValueIndex
  , typeValueInstances :: [(Value, [WordLocation])]
  }

lookupValueIndex :: Ord a => Type a -> a -> Maybe ValueIndex
lookupValueIndex t = flip Map.lookup (typeValueMap t)

storeType :: Ord a => Type a -> Json.Type
storeType (Type t _ _ vs) = Json.Type t (fmap storeValue vs)
  where
    storeValue :: (Value, [WordLocation]) -> Json.Value
    storeValue ((ValueSimple vt), ls) = Json.Value vt (fmap locationToInstance ls)

    locationToInstance :: WordLocation -> Json.Instance
    locationToInstance = uncurry Json.Instance

generateType :: forall a. Ord a => Text -> (a -> Value) -> [(WordLocation, a)] -> Type a
generateType t f is = Type t is valueMap typedValueInstances
  where
    valueInstances :: [(a, [WordLocation])]
    valueInstances = Lens.over (traverse . Lens._2 . traverse) fst . Map.assocs . Utility.mapGroupBy snd $ is

    typedValueInstances :: [(Value, [WordLocation])]
    typedValueInstances = Lens.over (traverse . Lens._1) f valueInstances

    indexedValues :: [(a, ValueIndex)]
    indexedValues = Lens.over (traverse . Lens._2) ValueIndex . flip zip [0..] . fmap fst $ valueInstances

    valueMap :: Map a ValueIndex
    valueMap = Map.fromList indexedValues

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
