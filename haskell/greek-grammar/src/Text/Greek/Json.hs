{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Text.Greek.Json where

import Prelude hiding (Word)
import Data.Aeson ((.=))
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics
import Text.Greek.FileReference
--import Text.Greek.Xml.Common
import System.FilePath
import qualified Control.Lens as Lens
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Char as Char
--import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Lazy.Builder as Lazy
import qualified Data.Text.Format as Format
import qualified Text.Greek.Script.Elision as Elision
import qualified Text.Greek.Paths as Path
import qualified Text.Greek.Script.Unicode as Unicode
import qualified Text.Greek.Script.Word as Word
import qualified Text.Greek.Source.Work as Work

data Data = Data
  { dataIndex :: Index
  , dataWorks :: [Work]
  , dataTypes :: [Type]
  }

data Type = Type
  { typeTitle :: Text
  , typeValues :: [Text]
  }
instance Aeson.ToJSON Type where toJSON (Type t vs) = Aeson.object ["title" .= t, "values" .= vs]

data Index = Index
  { indexWorkInfos :: [WorkInfo]
  , indexTypeInfos :: [TypeInfo]
  } deriving (Generic, Show)
instance Aeson.ToJSON Index where toJSON (Index ws ts) = Aeson.object ["works" .= ws, "types" .= ts]

data WorkInfo = WorkInfo Text Text Int deriving Show
instance Aeson.ToJSON WorkInfo where toJSON (WorkInfo t s wc) = Aeson.object ["title" .= t, "source" .= s, "wordCount" .= wc]

data TypeInfo = TypeInfo Text Int Int deriving Show
instance Aeson.ToJSON TypeInfo where toJSON (TypeInfo t vc ic) = Aeson.object ["title" .= t, "valueCount" .= vc, "instanceCount" .= ic]

data Word = Word
  { wordText :: WordText
  , wordValues :: [ValueIndex]
  } deriving (Show)
instance Aeson.ToJSON Word where toJSON (Word t vs) = Aeson.object ["t" .= t, "v" .= vs]

newtype WordIndex = WordIndex Int deriving (Eq, Ord, Show)
instance Aeson.ToJSON WordIndex where toJSON (WordIndex i) = Aeson.toJSON i

newtype TypeIndex = TypeIndex Int deriving (Eq, Ord, Show)
instance Aeson.ToJSON TypeIndex where toJSON (TypeIndex i) = Aeson.toJSON i

newtype ValueIndex = ValueIndex Int deriving (Eq, Ord, Show)
instance Aeson.ToJSON ValueIndex where toJSON (ValueIndex i) = Aeson.toJSON i

data WordGroup = WordGroup
  { wordGroupTitle :: Text
  , wordGroupWords :: [[WordIndex]]
  } deriving (Generic, Show)
instance Aeson.ToJSON WordGroup where toJSON (WordGroup t ws) = Aeson.object ["title" .= t, "words" .= ws]

data Work = Work
  { workSource :: Text
  , workTitle :: Text
  , workWords :: [Word]
  , workWordGroups :: [WordGroup]
  , workWordTypes :: [TypeIndex]
  , workWordSummary :: [Int]
  } deriving (Generic, Show)
instance Aeson.ToJSON Work where
  toJSON (Work s t ws wgs ts sm) = Aeson.object
    [ "source" .= s
    , "title" .= t
    , "words" .= ws
    , "wordGroups" .= wgs
    , "wordTypes" .= ts
    , "wordSummary" .= sm
    ]

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

makeType :: Text -> (a -> Text) -> [a] -> Type
makeType t f = Type t . fmap f

getData' :: [Work.Basic [Word.Basic (Text, FileReference)]] -> Data
getData' ws = Data ourIndex [] ourTypes
  where
    ourIndex = Index [] (fmap makeTypeInfo ourTypes)
    ourTypes = [wordTextType]

    wordTextType = makeType "Source Text" getWordText (Map.keys wordTextMap)
    wordTextMap = makeValueMap (workWordTexts ws)
    wordTexts = fmap (WordText . fst . Word._basicSurface)
    workWordTexts = concatMap (wordTexts . Work._workContent)

    makeTypeInfo (Type t vs) = TypeInfo t (length vs) 0


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

getWordProperties :: Word.Basic [(Unicode.Composed, FileCharReference)] -> [Text]
getWordProperties (Word.Basic s e _) =
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

    unicodeComposedProperty = Text.intercalate ", " . fmap titleUnicodeComposed

getElisionProperty :: Maybe (Elision.ElisionChar, FileCharReference) -> Text
getElisionProperty (Just (Elision.ElisionChar c, r)) = Lazy.toStrict $ Format.format "Elided {} {}" (formatUnicodeCodePoint c, titleFileCharReference r)
getElisionProperty _ = "Not elided"

dumpJson :: Data -> IO ()
dumpJson (Data i ws ts) = do
  _ <- write "index.json" i
  _ <- writeAll "works/work" ws
  _ <- writeAll "values/type" ts
  return ()
  where
    write n = BL.writeFile (Path.pagesData </> n) . Aeson.encode
    writeAll n = sequence . fmap (\(xi, x) -> write (n ++ show xi ++ ".json") x) . zip ([0..] :: [Int])


newtype Stage0Word = Stage0Word [Unicode.Composed] deriving (Eq, Ord, Show)
type Stage0 = (Work.Source, Work.Title, Stage0Word, FileCharReference, Unicode.Composed)

toStage0Hierarchy
  ::  [Work.Basic [Word.Basic (Text, FileReference)]]
  -> Either Unicode.Error
      [Work.Basic [Word.Basic [(Unicode.Composed, FileCharReference)]]]
toStage0Hierarchy = (traverse . Work.workContent . traverse . Word.basicSurface) (uncurry Unicode.splitText)

flattenStage0
  :: [Work.Basic [Word.Basic [(Unicode.Composed, FileCharReference)]]]
  -> [Stage0]
flattenStage0 = concatMap flattenWork
  where
    flattenWork :: Work.Basic [Word.Basic [(Unicode.Composed, FileCharReference)]] -> [Stage0]
    flattenWork (Work.Work (Work.SourceTitle source title) content) = fmap (\(w, r, c) -> (source, title, w, r, c)) $ concatMap flattenWord content
  
    flattenWord :: Word.Basic [(Unicode.Composed, FileCharReference)] -> [(Stage0Word, FileCharReference, Unicode.Composed)]
    flattenWord (Word.Basic surface _ _) = fmap (\(c, r) -> (stageWord, r, c)) surface
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

titleUnicodeComposed :: Unicode.Composed -> Text
titleUnicodeComposed (Unicode.Composed c) = Lazy.toStrict $ Format.format "{} {}" (formatUnicodeCodePoint c, c)

formatUnicodeCodePoint :: Char -> Text
formatUnicodeCodePoint c = Lazy.toStrict $ Format.format "U+{}" (Format.Only . Lazy.toUpper . Lazy.toLazyText . Format.left 4 '0' . Format.hex . Char.ord $ c)
