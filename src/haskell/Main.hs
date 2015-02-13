{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (lookup, toList, readFile, concat, FilePath, putStrLn)
import Data.Char (ord)
import Data.List (foldl')
import Data.Map (lookup)
import Data.Set (fromList, toList)
import Data.Text (Text, pack, unpack, concat)
import Data.Text.IO (putStrLn)
import Filesystem.Path (FilePath, (</>), (<.>))
import Numeric (showHex)
import Text.XML
import Text.XML.Cursor

data BibleVerse = BibleVerse
  { bibleVerse :: Text
  } deriving (Show)

newtype ParagraphIndex = ParagraphIndex { paragraphIndex :: Int }
  deriving (Eq, Ord, Show)

data Word = Word
  { wordText :: Text
  , wordBibleVerse :: BibleVerse
  , wordParagraphIndex :: ParagraphIndex
  } deriving (Show)

data WordBuilderError =
  TextWithoutVerse Text |
  SkippedElement Text |
  SkippedNode Node
  deriving (Show)

data Builder = Builder
  { builderBibleVerse :: Maybe BibleVerse
  , builderParagraphIndex :: ParagraphIndex
  , builderWords :: [Word]
  , builderErrors :: [WordBuilderError]
  } deriving (Show)

data Resource = Resource
  { resourceName :: Text
  , resourceBuilder :: Builder
  } deriving (Show)

osisNamespace :: Text
osisNamespace = "http://www.bibletechnologies.net/2003/OSIS/namespace"

osisName :: Text -> Name
osisName n = Name n (Just osisNamespace) Nothing

osisElement :: Text -> Axis
osisElement = element . osisName

emptyBuilder :: Builder
emptyBuilder = Builder Nothing (ParagraphIndex 0) [] []

buildWords :: Builder -> Cursor -> Builder
buildWords builder@(Builder mv p ws es) c =
  case node c of
    n@(NodeElement e) ->
      case nameLocalName $ elementName e of
        "verse" ->
          case (lookup "sID" (elementAttributes e), lookup "eID" (elementAttributes e)) of
            (Just v, _) -> builder { builderBibleVerse = Just $ BibleVerse v }
            (_, Just _) -> builder { builderBibleVerse = Nothing }
            _ -> builder
        "w" -> let t = (concat $ fromNode n $// content) in
          case mv of
            Just v -> builder { builderWords = w : ws } where w = Word t v p
            _ -> builder { builderErrors = (TextWithoutVerse t) : es }
        "p" -> builder { builderParagraphIndex = ParagraphIndex (1 + (paragraphIndex p)) }
        _ -> builder
    _ -> builder

loadSblgntWords :: Cursor -> Builder
loadSblgntWords cursor = foldl' buildWords emptyBuilder elements
  where elements = cursor $// osisElement "div" >=> attributeIs "type" "book" &.// descendant

loadSblgnt :: Document -> Resource
loadSblgnt d = Resource resourceId words
  where
    cursor = fromDocument d
    resourceId = concat $ concatMap (attribute "osisIDWork") $ cursor $/ osisElement "osisText"
    words = loadSblgntWords cursor

distinctBySet :: Ord a => [a] -> [a]
distinctBySet = toList . fromList

showWords :: Builder -> [Text]
showWords = fmap (\(Word t v i) -> concat [bibleVerse v, " [p=", pack . show . paragraphIndex $ i, "] ", t] ) . reverse . builderWords

sblgntPath :: FilePath
sblgntPath = ".." </> "sblgnt" </> "osis" </> "SBLGNT" <.> "osis" <.> "xml" -- http://sblgnt.com/

main :: IO ()
main = do
  sblgntDocument <- readFile def sblgntPath
  let r = loadSblgnt sblgntDocument
  putStrLn $ resourceName r
  mapM_ putStrLn $ showWords . resourceBuilder $ r
