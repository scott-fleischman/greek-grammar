{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Greek.Source.Sblgnt where

import Prelude hiding ((*), (+), Word)
import Control.Lens hiding (element)
import Data.Maybe
import Data.Text (Text)
import Text.Greek.Utility
import Text.Greek.Xml
import Text.Greek.Xml.Parsec
import Text.Parsec.Combinator
import Text.Parsec.Error (ParseError)
import Text.Parsec.Prim
import qualified Data.XML.Types as X

readSblgntEvents :: FilePath -> IO ([SblgntError] + Sblgnt)
readSblgntEvents p = fmap (sblgntTransform p) . readEvents $ p

sblgntTransform
  :: FilePath
  -> [XmlInternalError] + [FileReference * X.Event]
  -> [SblgntError] + Sblgnt
sblgntTransform p x
  =   liftErrors SblgntErrorXmlInternal x
  >>. dropComments
  >>. trimContent _2
  >>= liftErrors SblgntErrorXml . toBasicEvents
  >>= tryOverAll (_2 . _BasicEventBeginElement . _1) tryDropNamespace (errorContext SblgntErrorUnexpectedNamespace _1)
  >>= tryOverAll (_2 . _BasicEventEndElement) tryDropNamespace (errorContext SblgntErrorUnexpectedNamespace _1)
  >>= over _Left (pure . SblgntErrorEventParse) . parseEvents p

data SblgntError
  = SblgntErrorXmlInternal XmlInternalError
  | SblgntErrorXml (XmlError FileReference)
  | SblgntErrorUnexpectedNamespace FileReference X.Name
  | SblgntErrorEventParse ParseError
  deriving (Show)

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
sblgntParser = elementSimple "sblgnt" sblgntContent
  where
    sblgntContent = do
      title <- titleParser
      license <- licenseParser
      books <- many bookParser
      return $ Sblgnt title license books

parseEvents :: FilePath -> [Event] -> ParseError + Sblgnt
parseEvents = parse sblgntParser
