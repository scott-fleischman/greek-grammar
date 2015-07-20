{-# LANGUAGE OverloadedStrings #-}

module Text.Greek.Source.Osis where

import Data.Char
import Prelude hiding (Word)
import Data.Text (Text)
import Text.XML
import qualified Data.Text as T

data OsisText = OsisText
  { osisTextOsisIDWork :: Text
  , osisTextHeader :: Header
  , osisTextDivs :: [Div]
  }
  deriving (Show)

data Header = Header
  { headerWork :: Text
  , headerTitle :: Text
  , headerCreator :: Text
  , headerDate :: Text
  , headerPublisher :: Text
  , headerRights :: Text
  }
  deriving (Show)

data Div = Div
  { divType :: Text
  , divOsisID :: Maybe Text
  , divContent :: [DivContent]
  }
  deriving (Show)

data DivContent
  = DivParagraph Paragraph
  | DivTitle Title
  | DivWord Word
  | DivChapter Chapter
  | DivVerse Verse
  | DivText Text
  deriving (Show)

data Paragraph = Paragraph [ParagraphContent]
  deriving (Show)

data ParagraphContent
  = ParagraphTitle Title
  | ParagraphWord Word
  | ParagraphChapter Chapter
  | ParagraphVerse Verse
  | ParagraphText Text
  deriving (Show)

data Title = Title Text
  deriving (Show)
data Word = Word Text
  deriving (Show)

data Milestone
  = MilestoneStart { milestoneOsisID :: Text, sID :: Text }
  | MilestoneEnd   { milestoneOsisID :: Text, eID :: Text }
  deriving (Show)

data Chapter = Chapter Milestone
  deriving (Show)
data Verse = Verse Milestone
  deriving (Show)

data OsisError
  = InvalidName Name
  | NotSingleNode [Node]
  | NotElement Node
  deriving (Show)

documentToOsisText :: Document -> Either OsisError OsisText
documentToOsisText = handleOsis . documentRoot

singleNode :: [Node] -> Either OsisError Node
singleNode [n] = Right n
singleNode ns = Left (NotSingleNode ns)

isSpaceNode :: Node -> Bool
isSpaceNode (NodeContent c) | T.all isSpace c = True
isSpaceNode _ = False

trimStart :: [Node] -> [Node]
trimStart = dropWhile isSpaceNode

trimEnd :: [Node] -> [Node]
trimEnd = reverse . trimStart . reverse

trim :: [Node] -> [Node]
trim = trimEnd . trimStart

nodeAsElement :: Node -> Either OsisError Element
nodeAsElement (NodeElement e) = Right e
nodeAsElement n = Left . NotElement $ n

osisNamespace :: Text
osisNamespace = "http://www.bibletechnologies.net/2003/OSIS/namespace"

makeOsisName :: Text -> Name
makeOsisName n = Name n (Just osisNamespace) Nothing

osisName :: Name -> Text -> Either OsisError Name
osisName n@(Name actual (Just namespace) _) expected
  | namespace == osisNamespace
  , actual == expected
  = Right n
osisName n _ = Left (InvalidName n)

osisElementName :: Text -> Element -> Either OsisError Element
osisElementName expected (Element name attributes nodes) = Element <$> osisName name expected <*> pure attributes <*> pure nodes

handleOsis :: Element -> Either OsisError OsisText
handleOsis e
    = osisElementName "osis" e
  >>= singleNode . trim . elementNodes
  >>= nodeAsElement
  >>= handleOsisText

handleOsisText :: Element -> Either OsisError OsisText
handleOsisText = const . Right . OsisText "" h $ []
  where h = Header "" "" "" "" "" ""
