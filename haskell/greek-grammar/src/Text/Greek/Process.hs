{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Greek.Process where

import Data.Map (Map)
import Data.Text (Text)
import Text.Greek.FileReference (FileReference)
import qualified Control.Lens as Lens
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Text.Greek.Json as Json
import qualified Text.Greek.Source.All as All
import qualified Text.Greek.Source.Work as Work
import qualified Text.Greek.Script.Word as Word

go :: IO ()
go = All.loadAll >>= handleResult process . showError

process :: [Work.Indexed [Word.IndexedBasic (Text, FileReference)]] -> IO ()
process ws = dumpData (Json.Data ourIndex [] types)
  where
    ourIndex = Json.Index workInfos typeInfos
    typeInfos = fmap Json.makeTypeInfo types
    workInfos = []
    types :: [Json.Type]
    types = Maybe.maybeToList . store . generateType "Source Text" ValueSimple . extractWordProperty getSourceText $ ws

type WordLocation = (Work.Index, Word.Index)
newtype ValueIndex = ValueIndex { getValueIndex :: Int } deriving (Eq, Ord, Show)
data Value
  = ValueSimple Text
  deriving (Eq, Ord, Show)
data Type a = Type
  { typeTitle :: Text
  , typeInstances :: [(WordLocation, a)]
  , typeValueMap :: Map a ValueIndex
  , typeValues :: [Value]
  }

store :: Ord a => Type a -> Maybe Json.Type
store (Type t is vm vs) = Json.Type <$> pure t <*> pure (fmap storeValue vs) <*> traverse (storeInstance vm) is
  where
    storeInstance m ((wki, wdi), x) = Json.Instance <$> pure wki <*> pure wdi <*> (Lens.over Lens._Just getValueIndex . Map.lookup x $ m)

storeValue :: Value -> Json.Value
storeValue (ValueSimple t) = Json.Value t

generateType :: forall a. Ord a => Text -> (a -> Value) -> [(WordLocation, a)] -> Type a
generateType t f is = Type t is valueMap transformedValueList
  where
    typedValues :: [a]
    typedValues = Lens.toListOf (Lens.each . Lens._2) is

    indexedValues :: [(a, ValueIndex)]
    indexedValues = Lens.over (traverse . Lens._2) ValueIndex . flip zip [0..] . Set.toAscList . Set.fromList $ typedValues

    valueMap :: Map a ValueIndex
    valueMap = Map.fromList indexedValues

    transformedValueList :: [Value]
    transformedValueList = fmap f . Lens.toListOf (traverse . Lens._1) $ indexedValues

getSourceText :: Word.IndexedBasic (Text, FileReference) -> Text
getSourceText = Lens.view (Word.surface . Lens._1)

extractWordProperty :: forall a b c. (Word.Indexed a b -> c) -> [Work.Indexed [Word.Indexed a b]] -> [(WordLocation, c)]
extractWordProperty f = concatMap getIndexedWorkProps
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

handleResult :: (a -> IO ()) -> Either String a -> IO ()
handleResult _ (Left e) = putStrLn e
handleResult f (Right a) = f a

showError :: Show a => Either a b -> Either String b
showError = Lens.over Lens._Left show

dumpData :: Json.Data -> IO ()
dumpData = Json.dumpJson
