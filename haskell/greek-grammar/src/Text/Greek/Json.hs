{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Greek.Json where

--import Control.Lens
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics
--import System.FilePath
import qualified Data.Aeson as Aeson
--import qualified Data.Map as Map
--import qualified Text.Greek.Paths as Path
--import qualified Text.Greek.Source.All as All
--import qualified Text.Greek.Script.Word as Word
--import qualified Data.ByteString.Lazy.Char8 as BL

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
go = putStrLn "Hi"
--go = do
--  result <- All.loadAll
--  case result of
--    Left x -> putStrLn . show $ x
--    Right works -> BL.writeFile (Path.pagesData </> "work.json") . encode . TopLevelArray "work" . makeWorks $ works

--makeWorks :: [All.WorkText] -> [Work]
--makeWorks = fmap (uncurry toWork) . zip [0..]
--  where
--    toWork i (All.Work _ t ws) = Work i t (getWords ws)
--    getWords = fmap (^. Word.basicSurface . _1)
