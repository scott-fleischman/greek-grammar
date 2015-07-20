{-# LANGUAGE OverloadedStrings #-}

module Text.Greek.Source.Osis where

import Prelude hiding (Word)
import Data.Text (Text)
import Text.XML

data OsisText = OsisText
  { osisIDWork :: Text
  , header :: Header
  , books :: [BookDiv]
  }

data Header = Header
  { work :: Text
  , title :: Text
  , creator :: Text
  , date :: Text
  , publisher :: Text
  , rights :: Text
  }

data BookDiv = BookDiv
  { divType :: Text
  , divOsisID :: Text
  , divContent :: [DivContent]
  }

data DivContent
  = DivParagraph Paragraph
  | DivTitle Title
  | DivWord Word
  | DivChapter Chapter
  | DivVerse Verse
  | DivText Text

data Paragraph = Paragraph ParagraphContent

data ParagraphContent
  = ParagraphTitle Title
  | ParagraphWord Word
  | ParagraphChapter Chapter
  | ParagraphVerse Verse
  | ParagraphText Text

data Title = Title Text
data Word = Word Text

data Milestone
  = MilestoneStart { milestoneOsisID :: Text, sID :: Text }
  | MilestoneEnd   { milestoneOsisID :: Text, eID :: Text }

data Chapter = Chapter Milestone
data Verse = Verse Milestone

data OsisError
  = InvalidName Name
  | NotSingleNode [Node]
  | NotElement Node

documentToOsisText :: Document -> Either OsisError OsisText
documentToOsisText = handleOsis . documentRoot

singleNode :: [Node] -> Either OsisError Node
singleNode [n] = Right n
singleNode ns = Left (NotSingleNode ns)

singleChildNode :: Element -> Either OsisError Node
singleChildNode (Element name attributes nodes) = singleNode nodes

nodeAsElement :: Node -> Either OsisError Element
nodeAsElement (NodeElement e) = Right e
nodeAsElement n = Left . NotElement $ n

osisNamespace :: Text
osisNamespace = "http://www.bibletechnologies.net/2003/OSIS/namespace"

osisName :: Name -> Text -> Either OsisError Name
osisName n@(Name actual (Just namespace) _) expected
  | namespace == osisNamespace
  , actual == expected
  = Right n
  | otherwise
  = Left (InvalidName n)

osisElementName :: Text -> Element -> Either OsisError Element
osisElementName expected (Element name attributes nodes) = Element <$> osisName name expected <*> pure attributes <*> pure nodes

handleOsis :: Element -> Either OsisError OsisText
handleOsis e
    = osisElementName "osis" e
  >>= singleChildNode
  >>= nodeAsElement
  >>= handleOsisText

handleOsisText :: Element -> Either OsisError OsisText
handleOsisText = const . Right . OsisText "" header $ []
  where header = Header "" "" "" "" "" ""
