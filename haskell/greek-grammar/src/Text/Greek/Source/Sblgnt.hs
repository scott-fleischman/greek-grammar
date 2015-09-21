{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Greek.Source.Sblgnt where

import Prelude hiding (Word)
import Control.Lens hiding (element)
import Data.Char
import Data.Maybe
import Data.Text (Text)
import Text.Greek.FileReference
import Text.Greek.Parse.Utility
import Text.Greek.Script.Elision
import Text.Greek.Xml.Parse
import Text.Parsec.Combinator
import Text.Parsec.Prim
import qualified Data.Text as T
import qualified Text.ParserCombinators.Parsec as C

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
markEndParser = element "mark-end" (xmlLangAttributeValueParser "en") contentParser (const MarkEnd)

data Word = Word
  { wordSurface :: (Text, FileReference)
  , wordElision :: Maybe (ElisionChar, FileCharReference)
  , wordPrefix :: Maybe Text
  , wordSuffix :: Text
  } deriving Show

-- prefixes/suffixes
-- ⸀ or ⸁ or ⸀1 ⸀2 (following word; dot = second occurrence; number = third and subsequent)
-- ⸂ ⸃ or ⸄ ⸅  (enclosed words)
-- [ ]  (doubtful)

wordContentParser :: C.CharParser () (Text, Maybe ElisionChar)
wordContentParser = (,)
  <$> (T.pack <$> many1 (C.satisfy isLetter))
  <*> (fmap ElisionChar <$> optionMaybe (C.oneOf elisionCharacters))
  <* eof <?> "unexpected end of word content"

splitReference :: FileReference -> Maybe a -> (FileReference, Maybe (a, FileCharReference))
splitReference r Nothing = (r, Nothing)
splitReference r (Just a) =
  ( r & fileReferenceEnd . lineReferenceColumn %~ (\x -> x - 1)
  , Just (a, FileCharReference (r ^. fileReferencePath) (r ^. fileReferenceEnd))
  )

wordParser :: EventParser Word
wordParser = do
  prefix <- optionMaybe (elementContent "prefix")
  (r, content) <- elementContentReference "w"
  (surface, elision) <- embedParser wordContentParser (T.unpack content)
  let (r', elision') = splitReference r elision
  suffix <- elementContent "suffix"
  return $ Word (surface, r') elision' prefix suffix

data Item = ItemVerse Verse | ItemWord Word deriving Show
makePrisms ''Item

itemParser :: EventParser Item
itemParser = fmap ItemVerse verseParser <|> fmap ItemWord wordParser

data BookParagraph = BookParagraphContent [Item] | BookParagraphMarkEnd MarkEnd deriving Show
makePrisms ''BookParagraph

bookParagraphParser :: EventParser (Maybe BookParagraph)
bookParagraphParser
  =   fmap (Just . BookParagraphMarkEnd) markEndParser
  <|> try (fmap (const Nothing) (emptyElement "p"))
  <|> fmap (Just . BookParagraphContent) (paragraphParser (many1 itemParser))
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

data Sblgnt = Sblgnt
  { sblgntTitle :: Title
  , sblgntLicense :: License
  , sblgntBooks :: [Book]
  } deriving Show

sblgntParser :: EventParser Sblgnt
sblgntParser = elementSimple "sblgnt" $ do
  title <- titleParser
  license <- licenseParser
  books <- many bookParser
  return $ Sblgnt title license books
