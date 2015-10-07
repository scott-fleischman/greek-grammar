{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Greek.Json where

import Control.Lens
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import System.FilePath
import qualified Text.Greek.Paths as Path
import qualified Text.Greek.Source.All as All
import qualified Text.Greek.Script.Word as Word
import qualified Data.ByteString.Lazy.Char8 as BL

data Work = Work
  { work :: Int
  , title :: Text
  , words :: [Text]
  } deriving (Generic, Show)

data TopLevelArray a = TopLevelArray
  { name :: Text
  , items :: [a]
  } deriving (Generic, Show)

instance ToJSON Work
instance ToJSON a => ToJSON (TopLevelArray a)

go :: IO ()
go = do
  result <- All.loadAll
  case result of
    Left x -> putStrLn . show $ x
    Right works -> BL.writeFile (Path.pagesData </> "work.json") . encode . TopLevelArray "work" . makeWorks $ works

makeWorks :: [All.WorkText] -> [Work]
makeWorks = fmap (uncurry toWork) . zip [0..]
  where
    toWork i (All.Work _ t ws) = Work i t (getWords ws)
    getWords = fmap (^. Word.basicSurface . _1)
