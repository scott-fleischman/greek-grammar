{-# LANGUAGE OverloadedStrings #-}

module Text.Greek.Source.Sblgnt where

import Prelude hiding (Word)
import Data.Maybe
import Data.Text (Text)
import Text.Greek.FileReference
import Text.Greek.Xml.Parse
import Text.Parsec.Combinator
import Text.Parsec.Prim

newtype Target = Target Text deriving Show

hrefAttributeParser :: AttributeParser Target
hrefAttributeParser = Target <$> simpleAttributeParser "href"

newtype Paragraph = Paragraph Text deriving Show

paragraphParser :: EventParser c -> EventParser c
paragraphParser p = elementSimple "p" p

paragraphSimpleParser :: EventParser Paragraph
paragraphSimpleParser = fmap Paragraph (paragraphParser contentParser)

newtype Title = Title [Paragraph] deriving Show

titleParser :: EventParser Title
titleParser = fmap Title $ elementSimple "title" (many paragraphSimpleParser)

data Link = Link { linkHref :: Target, linkContent :: Text } deriving Show

newtype Content = Content Text deriving Show
data ParagraphLink
  = ParagraphLinkContent Content
  | ParagraphLinkLink Link
  deriving Show

linkParser :: EventParser Link
linkParser = element "a" hrefAttributeParser contentParser Link

paragraphLinkParser :: EventParser ParagraphLink
paragraphLinkParser = content <|> link
  where
    content = fmap (ParagraphLinkContent . Content) contentParser
    link = fmap ParagraphLinkLink linkParser

newtype License = License [ParagraphLink] deriving Show
licenseParser :: EventParser License
licenseParser = fmap License $ elementSimple "license" (paragraphParser (many paragraphLinkParser))

data Verse = Verse { verseId :: Text, verseText :: Text } deriving Show

verseParser :: EventParser Verse
verseParser = element "verse-number" (simpleAttributeParser "id") contentParser Verse

newtype MarkEnd = MarkEnd Text deriving Show

markEndParser :: EventParser MarkEnd
markEndParser = element "mark-end" (xmlLangAttributeParser "en") contentParser (const MarkEnd)

data Word = Word
  { wordSurface :: Text
  , wordFileReference :: FileReference
  , wordPrefix :: Maybe Text
  , wordSuffix :: Text
  } deriving Show

-- prefixes/suffixes
-- ⸀ or ⸁ or ⸀1 ⸀2 (following word; dot = second occurrence; number = third and subsequent)
-- ⸂ ⸃ or ⸄ ⸅  (enclosed words)
-- [ ]  (doubtful)

wordParser :: EventParser Word
wordParser = do
  prefix <- optionMaybe (elementContent "prefix")
  (r, surface) <- elementContentReference "w"
  suffix <- elementContent "suffix"
  return $ Word surface r prefix suffix

data Item = ItemVerse Verse | ItemWord Word deriving Show

itemParser :: EventParser Item
itemParser = fmap ItemVerse verseParser <|> fmap ItemWord wordParser

data BookParagraph = BookParagraphContent [Item] | BookParagraphMarkEnd MarkEnd deriving Show

bookParagraphParser :: EventParser (Maybe BookParagraph)
bookParagraphParser
  =   try (fmap (Just . BookParagraphContent) (paragraphParser (many1 itemParser)))
  <|> fmap (Just . BookParagraphMarkEnd) markEndParser
  <|> fmap (const Nothing) (emptyElement "p")
  <?> "Unknown book paragraph item"

data Book = Book
  { bookId :: Text
  , bookTitle :: Text
  , bookParagraphs :: [BookParagraph]
  }
  deriving Show

bookParser :: EventParser Book
bookParser = element "book" (simpleAttributeParser "id") bookContentParser (\i (t,p) -> Book i t p)
  where
    bookContentParser = do
      title <- elementContent "title"
      paragraphs <- many1 bookParagraphParser
      return (title, catMaybes paragraphs)

data Sblgnt = Sblgnt { sblgntTitle :: Title, sblgntLicense :: License, sblgntBooks :: [Book] } deriving Show

sblgntParser :: EventParser Sblgnt
sblgntParser = elementSimple "sblgnt" $ do
  title <- titleParser
  license <- licenseParser
  books <- many bookParser
  return $ Sblgnt title license books
