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
    Paragraph
  | Chapter Text
  | Verse Text
  | SectionTitle Text
  | Separator Text
  | Word Text
  deriving (Eq, Show)
