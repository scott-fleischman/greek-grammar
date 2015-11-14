{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Text.Greek.IO.Json where

import Prelude hiding (Word)
import Data.Aeson ((.=))
import Data.Map (Map)
import Data.Text (Text)
import Text.Greek.Source.FileReference
--import Text.Greek.Xml.Common
import System.FilePath
import qualified Control.Lens as Lens
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Format as Format
import qualified System.Directory as Directory
import qualified Text.Greek.IO.Paths as Paths
import qualified Text.Greek.IO.Render as Render
import qualified Text.Greek.Script.Elision as Elision
import qualified Text.Greek.Script.Unicode as Unicode
import qualified Text.Greek.Script.Word as Word
import qualified Text.Greek.Source.Work as Work
import qualified Text.Greek.Utility as Utility

data Data = Data
  { dataIndex :: Index
  , dataWorks :: [Work]
  , dataTypes :: [Type]
  }

data Type = Type
  { typeTitle :: Text
  , typeValues :: [Value]
  }
instance Aeson.ToJSON Type where toJSON (Type t vs) = Aeson.object ["title" .= t, "values" .= vs]

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
    typeLeaf = concatMap (\(ti, Type _ vs) -> valueLeaf ti vs) . index TypeIndex

data Index = Index
  { indexWorkInfos :: [WorkInfo]
  , indexTypeInfos :: [TypeInfo]
  }
instance Aeson.ToJSON Index where toJSON (Index ws ts) = Aeson.object ["works" .= ws, "types" .= ts]

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
  , typeInfoValueInfos :: [ValueInfo]
  }
instance Aeson.ToJSON TypeInfo where toJSON (TypeInfo t vs) = Aeson.object ["title" .= t, "values" .= vs]

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

--process
--  :: Either [XmlError] [All.Work [Word.Basic (Text, FileReference)]]
--  -> Either String     [All.Work [Word.Basic [(Unicode.Composed, FileCharReference)]]]
--process x
--  =   showError x
--  >>= showError . toStage0Hierarchy

makeValueMap :: Ord a => [a] -> Map a ValueIndex
makeValueMap
  = Map.fromList
  . Lens.over (Lens.each . Lens._2) ValueIndex
  . flip zip [0..]
  . Set.toAscList
  . Set.fromList

newtype WordText = WordText { getWordText :: Text } deriving (Eq, Ord, Show)
instance Aeson.ToJSON WordText where toJSON (WordText t) = Aeson.toJSON t

--makeType :: Text -> (a -> Text) -> [a] -> Type
--makeType t f = Type t . fmap f

--getData' :: [Work.Basic [Word.Basic (Text, FileReference)]] -> Data
--getData' ws = Data ourIndex [] ourTypes
--  where
--    ourIndex = Index [] (fmap makeTypeInfo ourTypes)
--    ourTypes = [wordTextType]

--    wordTextType = makeType "Source Text" getWordText (Map.keys wordTextMap)
--    wordTextMap = makeValueMap (workWordTexts ws)
--    wordTexts = fmap (WordText . fst . Word.getSurface)
--    workWordTexts = concatMap (wordTexts . Work.getContent)

makeTypeInfo :: Type -> TypeInfo
makeTypeInfo (Type t vs) = TypeInfo t (fmap makeValueInfo vs)

makeValueInfo :: Value -> ValueInfo
makeValueInfo (Value t is) = ValueInfo t (length is)

--getData :: [All.Work [Word.Basic [(Unicode.Composed, FileCharReference)]]] -> Data
--getData ws = Data ourIndex stage0Works ourTypes
--  where
--    ourIndex = Index (fmap makeWorkInfo ws) (fmap makeTypeInfo ourTypes)

--    ourTypes = [Type "Simple Type" ["Value1", "Value2"]]

--    makeWorkInfo (All.Work s t c) = WorkInfo (titleWorkTitle t) (titleWorkSource s) (length c)
--    makeTypeInfo (Type t vs) = TypeInfo t (length vs) 0

--    stage0Works = fmap makeWork ws
--    makeWork (All.Work s t c) = Work (titleWorkSource s) (titleWorkTitle t) (fmap (\(_,_,w) -> w) iws) [ps] propertyNames summaryProperties
--      where
--        ps = paragraphs iws
--        iws = indexedWords c
--        propertyNames = ["Elision", "Unicode Composed", "Line:Column", "File"]
--        summaryProperties = [0..3]
--    paragraphs = WordGroup "Paragraph" . (fmap . fmap) fst . List.groupBy (\(_,p1) (_,p2) -> p1 == p2) . fmap (\(i,p,_) -> (i,p))
--    indexedWords = fmap (uncurry makeWord) . zip [0..]
--    makeWord i w@(Word.Basic s _ p) = (i, p, Word (titleStage0Word . getStageWord $ s) (getWordProperties w))

getWordProperties :: Word.Word Word.Basic [(Unicode.Composed, FileCharReference)] -> [Text]
getWordProperties (Word.Word (e, _) s) =
  [ getElisionProperty e
  , unicodeComposedProperty . fmap fst $ s
  , lineProperty . fmap snd $ s
  , fileProperty . fmap snd $ s
  ]
  where
    lineProperty ((FileCharReference _ (LineReference (Line l) (Column c))) : _) = Lazy.toStrict $ Format.format "{}:{}" (l, c)
    lineProperty _ = "No line"

    fileProperty ((FileCharReference (Path p) _) : _) = Text.pack p
    fileProperty [] = "No file"

    unicodeComposedProperty = Text.intercalate ", " . fmap (Lazy.toStrict . Render.render . Unicode.composed)

getElisionProperty :: Maybe (Elision.ElisionChar, FileCharReference) -> Text
getElisionProperty (Just (Elision.ElisionChar c, r)) = Lazy.toStrict $ Format.format "Elided {} {}" (Render.render c, titleFileCharReference r)
getElisionProperty _ = "Not elided"

dumpJson :: Data -> IO ()
dumpJson (Data i ws ts) = do
  _ <- Directory.createDirectoryIfMissing True Paths.pagesData
  _ <- Directory.createDirectoryIfMissing True (Paths.pagesData </> "works")
  _ <- Directory.createDirectoryIfMissing True (Paths.pagesData </> "types")

  _ <- putStrLn "Writing index"
  _ <- write "index.json" i

  _ <- putStrLn "Writing works"
  _ <- writeAll "works/work" ws

  _ <- putStrLn "Writing types"
  _ <- writeAll "types/type" ts
  return ()
  where
    write n = BL.writeFile (Paths.pagesData </> n) . Aeson.encode
    writeAll n = sequence . fmap (\(xi, x) -> write (n ++ show xi ++ ".json") x) . zip ([0..] :: [Int])


newtype Stage0Word = Stage0Word [Unicode.Composed] deriving (Eq, Ord, Show)
type Stage0 = (Work.Source, Work.Title, Stage0Word, FileCharReference, Unicode.Composed)

toStage0Hierarchy
  ::  [Work.Basic [Word.Word Word.Basic (Text, FileReference)]]
  -> Either Unicode.Error
      [Work.Basic [Word.Word Word.Basic [(Unicode.Composed, FileCharReference)]]]
toStage0Hierarchy = (traverse . Work.content . traverse . Word.surface) (uncurry Unicode.splitText)

flattenStage0
  :: [Work.Basic [Word.Word Word.Basic [(Unicode.Composed, FileCharReference)]]]
  -> [Stage0]
flattenStage0 = concatMap flattenWork
  where
    flattenWork :: Work.Basic [Word.Word Word.Basic [(Unicode.Composed, FileCharReference)]] -> [Stage0]
    flattenWork (Work.Work (source, title) content) = fmap (\(w, r, c) -> (source, title, w, r, c)) $ concatMap flattenWord content
  
    flattenWord :: Word.Word Word.Basic [(Unicode.Composed, FileCharReference)] -> [(Stage0Word, FileCharReference, Unicode.Composed)]
    flattenWord (Word.Word _ surface) = fmap (\(c, r) -> (stageWord, r, c)) surface
      where
        stageWord = getStageWord surface

getStageWord :: [(Unicode.Composed, FileCharReference)] -> Stage0Word
getStageWord = Stage0Word . fmap fst

workSourceName :: Text
workSourceName = "WorkSource"

workTitleName :: Text
workTitleName = "WorkTitle"

stage0WordName :: Text
stage0WordName = "Stage0Word"

fileCharReferenceName :: Text
fileCharReferenceName = "FileLocation"

unicodeComposedName :: Text
unicodeComposedName = "UnicodeComposed"

titleWorkSource :: Work.Source -> Text
titleWorkSource Work.SourceSblgnt = "SBLGNT"

titleWorkTitle :: Work.Title -> Text
titleWorkTitle (Work.Title t) = t

titleStage0Word :: Stage0Word -> Text
titleStage0Word (Stage0Word cs) = Text.pack . fmap (\(Unicode.Composed c) -> c) $ cs

titleFileCharReference :: FileCharReference -> Text
titleFileCharReference (FileCharReference (Path p) (LineReference (Line l) (Column c))) = Lazy.toStrict $ Format.format "{}:{}:{}" (p, l, c)

formatList :: (a -> Text) -> [a] -> Text
formatList f xs = Lazy.toStrict $ Format.format "[{}]" (Format.Only . Text.intercalate ", " . fmap f $ xs)

formatFunction :: (a -> Text) -> (b -> Text) -> (a, b) -> Text
formatFunction f g (a, b) = Lazy.toStrict $ Format.format "{} → {}" (f a, g b)
