{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

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
import qualified Text.Greek.Paths as Path
import qualified Text.Greek.Source.All as All
import qualified Text.Greek.Script.Unicode as Unicode
import qualified Text.Greek.Script.Word as Word
import qualified Data.ByteString.Lazy.Char8 as BL
--import qualified Data.Text.Lazy as Lazy
--import qualified Data.Text.Format as Format

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
go = BL.writeFile (Path.pagesData </> "data.json") . Aeson.encode $ myData

--go = do
--  result <- fmap process All.loadAll
--  handleResult result

    --Right works -> BL.writeFile (Path.pagesData </> "work.json") . encode . TopLevelArray "work" . makeWorks $ works

process
  :: Either [XmlError] [All.Work [Word.Basic (Text, FileReference)]]
  -> Either String     [All.Work [Word.Basic [(Unicode.Composed, FileCharReference)]]]
process x
  =   showError x
  >>= showError . toStage0

handleResult :: Either String a -> IO ()
handleResult (Left s) = putStrLn s
handleResult (Right _) = putStrLn "success"

showError :: Show a => Either a b -> Either String b
showError = over _Left show

toStage0 ::               [All.Work [Word.Basic (Text, FileReference)]]
  -> Either Unicode.Error [All.Work [Word.Basic [(Unicode.Composed, FileCharReference)]]]
toStage0 = (traverse . All.workContent . traverse . Word.basicSurface) (uncurry Unicode.splitText)


class Extractable a where
  extract :: Map Text Type -> a -> Map Text Type

instance Extractable Unicode.Composed where
  extract m (Unicode.Composed _) = m

--makeWorks :: [All.WorkText] -> [Work]
--makeWorks = fmap (uncurry toWork) . zip [0..]
--  where
--    toWork i (All.Work _ t ws) = Work i t (getWords ws)
--    getWords = fmap (^. Word.basicSurface . _1)
