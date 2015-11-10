{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Greek.Process where

import Control.Monad.Except
import Data.Map (Map)
import Data.Text (Text)
import Text.Greek.FileReference (FileReference)
import qualified Control.Lens as Lens
import qualified Data.Map as Map
import qualified Text.Greek.Json as Json
import qualified Text.Greek.Source.All as All
import qualified Text.Greek.Source.Work as Work
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
  let wordType = generateType "Source Word" ValueSimple . flattenWords (fst . Word.getSurface) $ sourceWords
  let composedWords = toComposedWords sourceWords
  let decomposedWordPairs = toDecomposedWordPairs composedWords
  let decomposedWords = toDecomposedWords decomposedWordPairs
  let
    storedTypes =
      [ storeType wordType
      , storeType (toComposedType composedWords)
      , storeType (toDecomposedFunctionType decomposedWordPairs)
      , storeType (toDecomposedType decomposedWords)
      ]
  let workInfos = []
  let ourIndex = Json.Index workInfos . fmap Json.makeTypeInfo $ storedTypes
  liftIO $ dumpData (Json.Data ourIndex [] storedTypes)

toComposedWords
  :: [Work.Indexed [Word.IndexedBasic (Text, FileReference)]]
  -> [Work.Indexed [Word.IndexedBasic [Unicode.Composed]]]
toComposedWords = Lens.over (traverse . Work.content . traverse . Word.surface) (Unicode.toComposed . fst)

toComposedType :: [Work.Indexed [Word.IndexedBasic [Unicode.Composed]]] -> Type Unicode.Composed
toComposedType = generateType "Unicode Composed" (ValueSimple . Json.titleUnicodeDetail . Unicode.composed)
  . flattenSurface Word.getSurface

toDecomposedWordPairs
  :: [Work.Indexed [Word.IndexedBasic [Unicode.Composed]]]
  -> [Work.Indexed [Word.IndexedBasic [(Unicode.Composed, [Unicode.Decomposed])]]]
toDecomposedWordPairs = Lens.over (traverse . Work.content . traverse . Word.surface . traverse) (\x -> (x, Unicode.decompose' x))

toDecomposedFunctionType
  :: [Work.Indexed [Word.IndexedBasic [(Unicode.Composed, [Unicode.Decomposed])]]]
  -> Type (Unicode.Composed, [Unicode.Decomposed])
toDecomposedFunctionType = generateType "Unicode Composed → [Unicode Decomposed]"
  (ValueSimple . Json.formatFunction (Json.titleUnicodeDetail . Unicode.composed) (Json.formatList (Json.titleUnicodeDetail . Unicode.decomposed)))
  . flattenSurface Word.getSurface

toDecomposedWords
  :: [Work.Indexed [Word.IndexedBasic [(Unicode.Composed, [Unicode.Decomposed])]]]
  -> [Work.Indexed [Word.IndexedBasic [Unicode.Decomposed]]]
toDecomposedWords = Lens.over (traverse . Work.content . traverse . Word.surface) (concatMap snd)

toDecomposedType :: [Work.Indexed [Word.IndexedBasic [Unicode.Decomposed]]] -> Type Unicode.Decomposed
toDecomposedType = generateType "Unicode Decomposed" (ValueSimple . Json.titleUnicodeDetail . Unicode.decomposed)
  . flattenSurface Word.getSurface

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
