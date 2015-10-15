{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Greek.Json where

import Control.Lens
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics
import Text.Greek.FileReference
import Text.Greek.Xml.Common
--import System.FilePath
import qualified Data.Aeson as Aeson
--import qualified Data.Map as Map
--import qualified Text.Greek.Paths as Path
import qualified Text.Greek.Source.All as All
import qualified Text.Greek.Script.Unicode as Unicode
import qualified Text.Greek.Script.Word as Word
--import qualified Data.ByteString.Lazy.Char8 as BL
--import qualified Data.Text.Lazy as Lazy
--import qualified Data.Text.Format as Format

data Data = Data
  { stages :: [Stage]
  , types :: Map Text Type
  } deriving (Generic, Show)

data Stage = Stage
  { stageIndex :: Int
  , topLevelType :: Text
  , allTypes :: [Text]
  , focusResultType :: Text
  , focusSourceType :: Text
  } deriving (Generic, Show)

data Type = Type
  { typeTitle :: Text
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

go :: IO ()
go = do
  result <- fmap process All.loadAll
  handleResult result

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
