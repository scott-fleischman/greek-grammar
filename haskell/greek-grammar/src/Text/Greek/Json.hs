{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Text.Greek.Json where

import Control.Lens
import Data.Char
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
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Format as Format

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
go = All.loadAll >>= handleResult . process . startData

startData :: Either a b -> Either a (b, Data)
startData = over _Right (\x -> (x, Data [] []))

process
  :: Either [XmlError] ([All.Work [Word.Basic (Text, FileReference)]], Data)
  -> Either String     ([All.Work [Word.Basic [(Unicode.Composed, FileCharReference)]]], Data)
process x
  =   showError x
  >>= showError . toStage0 _1

appendData :: (Stageable a, Valuable a) => Either e (a, [Stage], [Type])
appendData = undefined

handleResult :: Either String (a, Data) -> IO ()
handleResult (Left e) = putStrLn e
handleResult (Right (_, d)) = BL.writeFile (Path.pagesData </> "data.json") . Aeson.encode $ d

showError :: Show a => Either a b -> Either String b
showError = over _Left show

toStage0 :: Lens s t
    [All.Work [Word.Basic (Text, FileReference)]]
    [All.Work [Word.Basic [(Unicode.Composed, FileCharReference)]]]
  -> s -> Either Unicode.Error t
toStage0 l = (l . traverse . All.workContent . traverse . Word.basicSurface) (uncurry Unicode.splitText)

data StringValue = StringValue
  { stringValueType :: Text
  , stringValueName :: Text
  , stringValueProperties :: [StringValue]
  } deriving (Eq, Ord, Show)

class Valuable a where
  getStringValue :: a -> (StringValue, [StringValue])

singleValue :: Lazy.Text -> Lazy.Text -> [StringValue] -> (StringValue, [StringValue])
singleValue t v cs = (StringValue (Lazy.toStrict t) (Lazy.toStrict v) [], cs)

instance Valuable Unicode.Composed where
  getStringValue (Unicode.Composed c) = singleValue
    "Unicode.Composed"
    (Format.format "U+{} '{}'" (Format.left 4 '0' . Format.hex . ord $ c, c))
    []

instance Valuable a => Valuable ([a]) where
  getStringValue as = composite
    where
      childType = case items of
        ((StringValue n _ _, _) : _) -> n
        _ -> "Unknown Type"
      items = fmap getStringValue as
      composite = singleValue
        (Format.format "[{}]" (Format.Only childType))
        ""
        (concatChildren items)

instance Valuable FileCharReference where
  getStringValue (FileCharReference (Path p) (LineReference (Line l) (Column c))) = singleValue
    "Source Line/Column"
    (Format.format "{} {}:{}" (p, l, c))
    []

concatChildren :: [(a, [a])] -> [a]
concatChildren = concatMap (\(a, as) -> a : as)

instance (Valuable a, Valuable b) => Valuable (a, b) where
  getStringValue (a, b) = composite
    where
      stringValueA@(StringValue typeA valueA _, _) = getStringValue a
      stringValueB@(StringValue typeB valueB _, _) = getStringValue b
      composite = singleValue
        (Format.format "({}, {})" (typeA, typeB))
        (Format.format "({}, {})" (valueA, valueB))
        (concatChildren [stringValueA, stringValueB])

instance Valuable a => Valuable (All.Work a) where
  getStringValue (All.Work _ t a) = composite
    where
      child@((StringValue contentType _ _), _) = getStringValue a
      composite = singleValue
        (Format.format "All.Work {}" (Format.Only contentType))
        (Format.format "{}" (Format.Only t))
        (concatChildren [child])

instance Valuable a => Valuable (Word.Basic a) where
  getStringValue (Word.Basic a _) = composite
    where
      child@((StringValue surfaceType surfaceValue _), _) = getStringValue a
      composite = singleValue
        (Format.format "Word.Basic {}" (Format.Only surfaceType))
        (Format.format "Word.Basic {}" (Format.Only surfaceValue))
        (concatChildren [child])

class Stageable a where 
  stage :: a -> Stage

instance Stageable [All.Work [Word.Basic [(Unicode.Composed, FileCharReference)]]] where
  stage x = Stage 0 t stageTypes Nothing Nothing
    where
      child@(StringValue t _ _, _) = getStringValue x
      stageValues = concatChildren [child]
      stageTypes = Set.toList . Set.fromList . fmap stringValueType $ stageValues
