{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Text.Greek.Json where

import Control.Lens
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics
import Text.Greek.FileReference
import Text.Greek.Xml.Common
import System.FilePath
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Char as Char
import qualified Data.Foldable as Foldable
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Format as Format
import qualified Text.Greek.Paths as Path
import qualified Text.Greek.Source.All as All
import qualified Text.Greek.Script.Unicode as Unicode
import qualified Text.Greek.Script.Word as Word

data Data = Data
  { stages :: [Stage]
  , types :: [Type]
  } deriving (Generic, Show)

data Stage = Stage
  { stageIndex :: Int
  , topLevelType :: Text
  , allTypes :: [Text]
  , focusResultType :: Maybe Text
  , focusSourceType :: Maybe Text
  } deriving (Generic, Show)

data Type = Type
  { typeName :: Text
  , typeTitle :: Text
  , propertyTypes :: [Text]
  , values :: [Value]
  } deriving (Generic, Show)

_propertyTypes :: Applicative f => ([Text] -> f [Text]) -> Type -> f Type
_propertyTypes f (Type a b c d) = Type <$> pure a <*> pure b <*> f c <*> pure d

_values :: Applicative f => ([Value] -> f [Value]) -> Type -> f Type
_values f (Type a b c d) = Type <$> pure a <*> pure b <*> pure c <*> f d

data Value = Value
  { valueIndex :: Int
  , valueTitle :: Text
  , propertyValues :: Map Text Int
  } deriving (Generic, Show)

instance Aeson.ToJSON Data
instance Aeson.ToJSON Stage
instance Aeson.ToJSON Type
instance Aeson.ToJSON Value

go :: IO ()
go = All.loadAll >>= handleResult dumpJson . over _Right getData . process

process
  :: Either [XmlError] [All.Work [Word.Basic (Text, FileReference)]]
  -> Either String     [All.Work [Word.Basic [(Unicode.Composed, FileCharReference)]]]
process x
  =   showError x
  >>= showError . toStage0Hierarchy

getData :: [All.Work [Word.Basic [(Unicode.Composed, FileCharReference)]]] -> Data
getData xs = Data [stage0Stage] stage0Types
  where
    flatStage0 = flattenStage0 . take 5 . drop 15 $ xs
    (stage0Stage, stage0Types) = makeStage0Types flatStage0

handleResult :: (a -> IO ()) -> Either String a -> IO ()
handleResult _ (Left e) = putStrLn e
handleResult f (Right a) = f a

dumpJson :: Data -> IO ()
dumpJson = BL.writeFile (Path.pagesData </> "data.json") . Aeson.encode

showError :: Show a => Either a b -> Either String b
showError = over _Left show


newtype Stage0Word = Stage0Word [Unicode.Composed] deriving (Eq, Ord, Show)
type Stage0 = (All.WorkSource, All.WorkTitle, Stage0Word, FileCharReference, Unicode.Composed)

toStage0Hierarchy
  ::  [All.Work [Word.Basic (Text, FileReference)]]
  -> Either Unicode.Error
      [All.Work [Word.Basic [(Unicode.Composed, FileCharReference)]]]
toStage0Hierarchy = (traverse . All.workContent . traverse . Word.basicSurface) (uncurry Unicode.splitText)

flattenStage0
  :: [All.Work [Word.Basic [(Unicode.Composed, FileCharReference)]]]
  -> [Stage0]
flattenStage0 = concatMap flattenWork
  where
    flattenWork :: All.Work [Word.Basic [(Unicode.Composed, FileCharReference)]] -> [Stage0]
    flattenWork (All.Work source title content) = fmap (\(w, r, c) -> (source, title, w, r, c)) $ concatMap flattenWord content
  
    flattenWord :: Word.Basic [(Unicode.Composed, FileCharReference)] -> [(Stage0Word, FileCharReference, Unicode.Composed)]
    flattenWord (Word.Basic surface _) = fmap (\(c, r) -> (stageWord, r, c)) surface
      where
        stageWord = getStageWord surface
  
    getStageWord :: [(Unicode.Composed, FileCharReference)] -> Stage0Word
    getStageWord = Stage0Word . fmap fst

makeValueMap :: Ord a => [a] -> Map a Int
makeValueMap xs = Map.fromList indexedList
  where
    uniqueValues = Set.toAscList . Set.fromList $ xs
    indexedList = zip uniqueValues [0..]

makeStage0Types :: [Stage0] -> (Stage, [Type])
makeStage0Types ss = (stage0Stage, stage0Types)
  where
    workSourceMap        = makeValueMap $ fmap (\(x,_,_,_,_) -> x) ss
    workTitleMap         = makeValueMap $ fmap (\(_,x,_,_,_) -> x) ss
    stage0WordMap        = makeValueMap $ fmap (\(_,_,x,_,_) -> x) ss
    fileCharReferenceMap = makeValueMap $ fmap (\(_,_,_,x,_) -> x) ss
    unicodeComposedMap   = makeValueMap $ fmap (\(_,_,_,_,x) -> x) ss

    stage0Stage = Stage 0 workTitleName (fmap typeName stage0Types) Nothing Nothing

    stage0Types = [workSourceType, workTitleType, stage0WordType, fileCharReferenceType, unicodeComposedType] ++ instanceTypeAsList

    workSourceType = makeSimpleType workSourceName titleWorkSource workSourceMap
    workTitleType = makeSimpleType workTitleName titleWorkTitle workTitleMap
    stage0WordType = makeSimpleType stage0WordName titleStage0Word stage0WordMap
    fileCharReferenceType = makeSimpleType fileCharReferenceName titleFileCharReference fileCharReferenceMap
    unicodeComposedType = makeSimpleType unicodeComposedName titleUnicodeComposed unicodeComposedMap

    instanceTypeAsList = fmap makeInstanceType . Foldable.toList $ maybeInstanceValues

    makeInstanceType :: [Value] -> Type
    makeInstanceType = Type "Stage0Instance" "Stage0Instance" [workSourceName, workTitleName, stage0WordName, fileCharReferenceName, unicodeComposedName]

    maybeInstanceValues :: Maybe [Value]
    maybeInstanceValues = traverse makeInstanceValue . zip [0..] $ ss

    makeInstanceValue :: (Int, Stage0) -> Maybe Value
    makeInstanceValue (i, (a, b, c, d, e)) = over _Just (Value i (makeInstanceTitle i e) . Map.fromList) $ sequence
      [ lookupValueIndex workSourceName workSourceMap a
      , lookupValueIndex workTitleName workTitleMap b
      , lookupValueIndex stage0WordName stage0WordMap c
      , lookupValueIndex fileCharReferenceName fileCharReferenceMap d
      , lookupValueIndex unicodeComposedName unicodeComposedMap e
      ]

    makeInstanceTitle i c = Lazy.toStrict $ Format.format "{} {}" (i, titleUnicodeComposed c)

lookupValueIndex :: Ord a => Text -> Map a Int -> a -> Maybe (Text, Int)
lookupValueIndex t m v = over _Just (\x -> (t, x)) $ Map.lookup v m

makeSimpleType :: Ord a => Text -> (a -> Text) -> Map a Int -> Type
makeSimpleType t f = Type t t [] . fmap (\(x,i) -> Value i (f x) Map.empty) . Map.toList

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

titleWorkSource :: All.WorkSource -> Text
titleWorkSource = Text.pack . show

titleWorkTitle :: All.WorkTitle -> Text
titleWorkTitle (All.WorkTitle t) = t

titleStage0Word :: Stage0Word -> Text
titleStage0Word (Stage0Word cs) = Text.pack . fmap (\(Unicode.Composed c) -> c) $ cs

titleFileCharReference :: FileCharReference -> Text
titleFileCharReference (FileCharReference p (LineReference (Line l) (Column c))) = Lazy.toStrict $ Format.format "{}:{}:{}" (Format.Shown p, l, c)

titleUnicodeComposed :: Unicode.Composed -> Text
titleUnicodeComposed (Unicode.Composed c) = Lazy.toStrict $ Format.format "U+{} {}" (Format.left 4 '0' . Format.hex . Char.ord $ c, c)
