{-# LANGUAGE OverloadedStrings #-}

module Text.Greek.NewTestament.SBLSimple where

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
  = OsisError

documentToOsisText :: Document -> Either OsisError OsisText
documentToOsisText = handleOsis . documentRoot

handleOsis :: Element -> Either OsisError OsisText
handleOsis _ = Left OsisError
