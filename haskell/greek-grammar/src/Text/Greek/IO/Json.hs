{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Text.Greek.IO.Json where

import Prelude hiding (Word)
import Data.Aeson ((.=))
import Data.Map (Map)
import Data.Text (Text)
import System.FilePath
import qualified Control.Lens as Lens
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified System.Directory as Directory
import qualified Text.Greek.IO.Paths as Paths
import qualified Text.Greek.IO.Render as Render
import qualified Text.Greek.Script.Word as Word
import qualified Text.Greek.Source.Work as Work
import qualified Text.Greek.Utility as Utility


data Data = Data
  { dataIndex :: Index
  , dataWorks :: [Work]
  , dataTypes :: [Type]
  }

data TypeKind
  = WordStageTypeKind
  | WordStageFunctionTypeKind
  | WordStagePartFunctionTypeKind
  | WordStagePartTypeKind
  | WordPropertyTypeKind
  | WorkPropertyTypeKind
  | CompositePropertyTypeKind
instance Aeson.ToJSON TypeKind where
  toJSON WordStageTypeKind = "Word Stage"
  toJSON WordStageFunctionTypeKind = "Word Stage Transition"
  toJSON WordStagePartFunctionTypeKind = "Word Stage Part Transition"
  toJSON WordStagePartTypeKind = "Word Stage Part"
  toJSON WordPropertyTypeKind = "Word Property"
  toJSON WorkPropertyTypeKind = "Work Property"
  toJSON CompositePropertyTypeKind = "Composite Property"

data Type = Type
  { typeTitle :: Text
  , typeKind :: TypeKind
  , typeValues :: [Value]
  }
instance Aeson.ToJSON Type where toJSON (Type t k vs) = Aeson.object ["title" .= t, "kind" .= k, "values" .= vs]

data Value = Value
  { valueText :: Text
  , valueInstances :: [Instance]
  }
instance Aeson.ToJSON Value where toJSON (Value t is) = Aeson.object ["t" .= t, "i" .= is]

newtype AtomIndex = AtomIndex { getAtomIndex :: Int } deriving (Eq, Ord, Show)

data Instance = Instance
  { instanceWorkIndex :: Work.Index
  , instanceWordIndex :: Word.Index
  , instanceAtomIndex :: Maybe AtomIndex
  } deriving (Eq, Ord, Show)
instance Aeson.ToJSON Instance where toJSON (Instance wk wd _) = Aeson.toJSON [Work.getIndex wk, Word.getIndex wd]

makeInstanceMap :: [Type] -> Map (Work.Index, Word.Index) [(TypeIndex, [ValueIndex])]
makeInstanceMap = fmap (groupTypes . fmap fst) . Utility.mapGroupBy snd . flattenInstances
  where
    groupTypes :: [(TypeIndex, Maybe AtomIndex, ValueIndex)] -> [(TypeIndex, [ValueIndex])]
    groupTypes = Map.toAscList . fmap sortValues . (fmap . fmap) (\(_, x, y) -> (x, y)) . Utility.mapGroupBy (Lens.view Lens._1)

    sortValues :: [(Maybe AtomIndex, ValueIndex)] -> [ValueIndex]
    sortValues = fmap snd . List.sortOn fst

flattenInstances :: [Type] -> [((TypeIndex, Maybe AtomIndex, ValueIndex), (Work.Index, Word.Index))]
flattenInstances = typeLeaf
  where
    index :: (Int -> a) -> [b] -> [(a, b)]
    index f = Lens.over (traverse . Lens._1) f . zip [0..]

    instancePair :: Instance -> (Work.Index, Word.Index)
    instancePair (Instance k d _) = (k, d)

    instanceLeaf :: TypeIndex -> ValueIndex -> [Instance] -> [((TypeIndex, Maybe AtomIndex, ValueIndex), (Work.Index, Word.Index))]
    instanceLeaf ti vi = fmap (\i -> ((ti, instanceAtomIndex i, vi), instancePair i))

    valueLeaf :: TypeIndex -> [Value] -> [((TypeIndex, Maybe AtomIndex, ValueIndex), (Work.Index, Word.Index))]
    valueLeaf ti = concatMap (\(vi, Value _ is) -> instanceLeaf ti vi is) . index ValueIndex

    typeLeaf :: [Type] -> [((TypeIndex, Maybe AtomIndex, ValueIndex), (Work.Index, Word.Index))]
    typeLeaf = concatMap (\(ti, Type _ _ vs) -> valueLeaf ti vs) . index TypeIndex

data Index = Index
  { indexWorkInfos :: [WorkInfo]
  , indexTypeInfos :: [TypeInfo]
  , indexSpecialTypes :: SpecialTypes
  }
instance Aeson.ToJSON Index where toJSON (Index ws ts s) = Aeson.object ["works" .= ws, "types" .= ts, "special" .= s]

data SpecialTypes = SpecialTypes
  { specialTypesWordSurface :: TypeIndex
  , specialTypesWordPrefix :: TypeIndex
  , specialTypesWordSuffix :: TypeIndex
  }
instance Aeson.ToJSON SpecialTypes where toJSON (SpecialTypes a b c) = Aeson.object [ "surface" .= a, "prefix" .= b, "suffix" .= c]

data WorkInfo = WorkInfo
  { workInfoTitle :: Work.Title
  , workInfoSource :: Work.Source
  , workWordContexts :: [WordContext]
  , workWordInfos :: [WordInfo]
  }
instance Aeson.ToJSON WorkInfo where
  toJSON (WorkInfo t s wc wi) = Aeson.object
    [ "title" .= Render.render t
    , "source" .= Render.render s
    , "wordContexts" .= wc
    , "wordInfos" .= wi
    ]


newtype WordContext = WordContext [Word.Index]
instance Aeson.ToJSON WordContext where toJSON (WordContext is) = Aeson.toJSON . fmap Word.getIndex $ is 

newtype WordContextIndex = WordContextIndex Int deriving (Eq, Ord, Show)
instance Aeson.ToJSON WordContextIndex where toJSON (WordContextIndex i) = Aeson.toJSON i

data WordInfo = WordInfo
  { wordInfoValues :: [(TypeIndex, [ValueIndex])]
  , wordInfoContext :: WordContextIndex
  }
instance Aeson.ToJSON WordInfo
  where
    toJSON (WordInfo v c) = Aeson.object
      [ "v" .= v
      , "c" .= c
      ]

data TypeInfo = TypeInfo
  { typeInfoTitle :: Text
  , typeInfoKind :: TypeKind
  , typeInfoValueInfos :: [ValueInfo]
  }
instance Aeson.ToJSON TypeInfo where toJSON (TypeInfo t k vs) = Aeson.object ["title" .= t, "kind" .= k, "values" .= vs]

data ValueInfo = ValueInfo
  { valueInfoTitle :: Text
  , valueInfoInstanceCount :: Int
  }
instance Aeson.ToJSON ValueInfo where toJSON (ValueInfo t ic) = Aeson.object ["t" .= t, "i" .= ic]

instance Aeson.ToJSON (TypeIndex, ValueIndex) where toJSON (TypeIndex a, ValueIndex b) = Aeson.toJSON [a, b]

data Word = Word
  { wordValues :: [(TypeIndex, [ValueIndex])]
  }
instance Aeson.ToJSON Word where toJSON (Word vs) = Aeson.toJSON vs

newtype TypeIndex = TypeIndex Int deriving (Eq, Ord, Show)
instance Aeson.ToJSON TypeIndex where toJSON (TypeIndex i) = Aeson.toJSON i

newtype ValueIndex = ValueIndex Int deriving (Eq, Ord, Show)
instance Aeson.ToJSON ValueIndex where toJSON (ValueIndex i) = Aeson.toJSON i

data WordGroup = WordGroup
  { wordGroupTitle :: Text
  , wordGroupWords :: [[Word.Index]]
  }
instance Aeson.ToJSON WordGroup where toJSON (WordGroup t ws) = Aeson.object ["title" .= t, "words" .= (fmap . fmap) Word.getIndex ws]

data Work = Work
  { workSource :: Work.Source
  , workTitle :: Work.Title
  , workWords :: [Word]
  , workWordGroups :: [WordGroup]
  , workWordSummary :: [TypeIndex]
  }
instance Aeson.ToJSON Work where
  toJSON (Work s t ws wgs sm) = Aeson.object
    [ "source" .= Render.render s
    , "title" .= Render.render t
    , "words" .= ws
    , "wordGroups" .= wgs
    , "wordSummary" .= sm
    ]


workToWorkInfo :: [TypeIndex] -> Work -> WorkInfo
workToWorkInfo ts (Work s t ws _ _) = WorkInfo t s [] (fmap (\(Word vs) -> WordInfo (filterValues vs) (WordContextIndex 0)) ws)
  where
    typeToOrder :: Map TypeIndex Int
    typeToOrder = Map.fromList . zip ts $ [0..]
    filterValues = List.sortOn (flip Map.lookup typeToOrder . fst) . filter (flip Map.member typeToOrder . fst)

makeValueMap :: Ord a => [a] -> Map a ValueIndex
makeValueMap
  = Map.fromList
  . Lens.over (Lens.each . Lens._2) ValueIndex
  . flip zip [0..]
  . Set.toAscList
  . Set.fromList

newtype WordText = WordText { getWordText :: Text } deriving (Eq, Ord, Show)
instance Aeson.ToJSON WordText where toJSON (WordText t) = Aeson.toJSON t

makeTypeInfo :: Type -> TypeInfo
makeTypeInfo (Type t k vs) = TypeInfo t k (fmap makeValueInfo vs)

makeValueInfo :: Value -> ValueInfo
makeValueInfo (Value t is) = ValueInfo t (length is)

writeIndex :: Index -> IO ()
writeIndex i = do
  _ <- Directory.createDirectoryIfMissing True Paths.pagesData
  encodeWrite "index.json" i

writeWorks :: [Work] -> IO ()
writeWorks ws = do
  _ <- Directory.createDirectoryIfMissing True (Paths.pagesData </> "works")
  encodeWriteAll "works/work" ws

writeTypes :: [Type] -> IO ()
writeTypes ts = do
  _ <- Directory.createDirectoryIfMissing True (Paths.pagesData </> "types")
  encodeWriteAll "types/type" ts

encodeWrite :: Aeson.ToJSON a => FilePath -> a -> IO ()
encodeWrite n = BL.writeFile (Paths.pagesData </> n) . Aeson.encode

encodeWriteAll :: Aeson.ToJSON a => String -> [a] -> IO ()
encodeWriteAll n = sequence_ . fmap (\(xi, x) -> encodeWrite (n ++ show xi ++ ".json") x) . zip ([0..] :: [Int])
