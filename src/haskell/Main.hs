{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import BasicPrelude hiding (readFile, Word)
import Data.Set (fromList, toList)
import Data.Text (pack)
import Text.XML
import Text.XML.Cursor

data Reference = Reference
  { getResource :: Text
  , getLocation :: Text
  } deriving (Show)

data Word = Word
  { getWord :: Text
--  , reference :: Reference
  } deriving (Show)

data Resource = Resource
  { getResourceId :: Text
  , getWords :: [Word]
  } deriving (Show)

osisName :: Text -> Name
osisName localName = Name localName (Just "http://www.bibletechnologies.net/2003/OSIS/namespace") Nothing

osisElement :: Text -> Axis
osisElement = element . osisName

osisAttribute :: Text -> Cursor -> [Text]
osisAttribute = attribute . osisName

loadSblgnt :: Document -> Resource
loadSblgnt d = Resource resourceId words
  where
    cursor = fromDocument d
    resourceId = concat $ concatMap (attribute "osisIDWork") $ cursor $/ osisElement "osisText"
    words = fmap Word $ cursor $// osisElement "w" &/ content

distinctBySet :: Ord a => [a] -> [a]
distinctBySet ws = toList $ fromList ws

main :: IO ()
main = do
  sblgntDocument <- readFile def $ ".." </> "sblgnt" </> "osis" </> "SBLGNT" <.> "osis" <.> "xml" -- http://sblgnt.com/
  let r = loadSblgnt sblgntDocument
  putStrLn $ getResourceId r
  mapM_ putStrLn $ distinctBySet $ fmap getWord $ getWords r
