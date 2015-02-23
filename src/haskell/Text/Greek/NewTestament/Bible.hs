module Text.Greek.NewTestament.Bible where

import Prelude (Eq, Show)
import Data.Text (Text)

data Bible = Bible
  { bibleId :: Text
  , bibleTitle :: Text
  , bibleBooks :: [Book]
  } deriving (Eq, Show)

data Book = Book
  { bookId :: Text
  , bookTitle :: Text
  , segments :: [Segment]
  } deriving (Eq, Show)

data Segment =
    SegmentParagraph (Milestone Paragraph)
  | SegmentChapter (Milestone Chapter)
  | SegmentVerse (Milestone Verse)
  | SectionTitle Text
  | Separator Text
  | SegmentWord Text
  deriving (Eq, Show)

data (Eq a, Show a) => Milestone a = Start a | End a
  deriving (Eq, Show)

data Paragraph = Paragraph
  deriving (Eq, Show)
data Chapter = Chapter Text
  deriving (Eq, Show)
data Verse = Verse Text
  deriving (Eq, Show)
