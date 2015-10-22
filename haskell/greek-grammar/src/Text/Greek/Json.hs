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
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Text.Greek.Paths as Path
import qualified Text.Greek.Source.All as All
import qualified Text.Greek.Script.Unicode as Unicode
import qualified Text.Greek.Script.Word as Word
import qualified Data.ByteString.Lazy.Char8 as BL

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

myData :: Data
myData =
  Data
  [ Stage 0
      "List Work List Word () List Unicode.Composed"
      [ "List Work List Word () List Unicode.Composed"
      , "Work List Word () List Unicode.Composed"
      , "List Word () List Unicode.Composed"
      , "Word () List Unicode.Composed"
      , "()"
      , "List Unicode.Composed"
      , "Unicode.Composed"
      , "Author"
      ]
      Nothing
      (Just "Unicode.Composed")
  ]
  [ Type
      "List Work List Word () List Unicode.Composed"
      "List Work List Word () List Unicode.Composed"
      []
      [ Value 0 "List of Works" Map.empty ]
  , Type
      "Work List Word () List Unicode.Composed"
      "Work List Word () List Unicode.Composed"
      []
      [ Value 0 "Matthew Book" Map.empty ]
  , Type
      "List Word () List Unicode.Composed"
      "List Word () List Unicode.Composed"
      []
      [ Value 0 "List of Words" Map.empty ]
  , Type
      "Word () List Unicode.Composed"
      "Word () List Unicode.Composed"
      []
      [ Value 0 "Word" Map.empty ]
  , Type
      "()"
      "()"
      []
      [ Value 0 "Unit" Map.empty ]
  , Type
      "List Unicode.Composed"
      "List Unicode.Composed"
      []
      [ Value 0 "List chars" Map.empty ]
  , Type
      "Unicode.Composed"
      "Unicode.Composed"
      []
      [ Value 0 "Composed char" Map.empty ]
  , Type
      "Author"
      "Author"
      []
      [ Value 0 "Author" Map.empty ]
  ]

go :: IO ()
go = All.loadAll >>= handleResult dumpJson . over _Right getData . process

process
  :: Either [XmlError] [All.Work [Word.Basic (Text, FileReference)]]
  -> Either String     [All.Work [Word.Basic [(Unicode.Composed, FileCharReference)]]]
process x
  =   showError x
  >>= showError . toStage0Hierarchy

getData :: [All.Work [Word.Basic [(Unicode.Composed, FileCharReference)]]] -> Data
getData = undefined

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

makeStage0Types :: [Stage0] -> Either String (Stage, [Type])
makeStage0Types ss = undefined
  where
    workSourceMap = makeValueMap $ fmap (\(x,_,_,_,_) -> x) ss
    workTitleMap  = makeValueMap $ fmap (\(_,x,_,_,_) -> x) ss
    stage0WordMap = makeValueMap $ fmap (\(_,_,x,_,_) -> x) ss
    fileCharMap   = makeValueMap $ fmap (\(_,_,_,x,_) -> x) ss
    composedMap   = makeValueMap $ fmap (\(_,_,_,_,x) -> x) ss
