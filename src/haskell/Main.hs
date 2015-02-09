{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import BasicPrelude hiding (readFile, Word, lookup)
import Data.Char (ord)
import Data.Map (lookup)
import Data.Set (fromList, toList)
import Data.Text (pack, unpack)
import Numeric (showHex)
import Text.XML
import Text.XML.Cursor

data BibleVerse = BibleVerse
  { bibleVerse :: Text
  } deriving (Show)

data Word = Word
  { wordText :: Text
  , wordBibleVerse :: BibleVerse
  } deriving (Show)

data Resource = Resource
  { resourceName :: Text
  , resourceWords :: [Word]
  } deriving (Show)

osisNamespace :: Text
osisNamespace = "http://www.bibletechnologies.net/2003/OSIS/namespace"

osisName :: Text -> Name
osisName localName = Name localName (Just osisNamespace) Nothing

osisElement :: Text -> Axis
osisElement = element . osisName

loadSblgntWords :: Cursor -> [Word]
loadSblgntWords cursor = reverse $ snd $ foldl' buildWords (BibleVerse "", []) elements
  where
    buildWords = \ state@(r, ws) c -> case node c of
      n@(NodeElement e) -> case nameLocalName $ elementName e of
        "verse" -> case lookup "sID" (elementAttributes e) of
          Just v -> (BibleVerse v, ws)
          _ -> state
        "w" -> (r, Word (concat $ fromNode n $// content) r : ws)
        _ -> state
      _ -> state
    elements = cursor $// osisElement "div" >=> attributeIs "type" "book" &/ child

loadSblgnt :: Document -> Resource
loadSblgnt d = Resource resourceId words
  where
    cursor = fromDocument d
    resourceId = concat $ concatMap (attribute "osisIDWork") $ cursor $/ osisElement "osisText"
    words = loadSblgntWords cursor

distinctBySet :: Ord a => [a] -> [a]
distinctBySet = toList . fromList

main :: IO ()
main = do
  sblgntDocument <- readFile def $ ".." </> "sblgnt" </> "osis" </> "SBLGNT" <.> "osis" <.> "xml" -- http://sblgnt.com/
  let r = loadSblgnt sblgntDocument
  putStrLn $ resourceName r
  mapM_ (putStrLn) $ fmap (\w -> concat [bibleVerse . wordBibleVerse $ w, ": ", wordText w]) $ resourceWords r
