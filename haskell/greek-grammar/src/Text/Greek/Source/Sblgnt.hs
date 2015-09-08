{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Greek.Source.Sblgnt where

import Prelude hiding ((*), (+), getLine)
import Control.Lens hiding (element)
import Data.Text (Text)
import Text.Greek.Utility
import Text.Greek.Xml
import Text.Parsec.Combinator
import Text.Parsec.Error (ParseError)
import Text.Parsec.Pos (SourcePos)
import Text.Parsec.Prim
import qualified Data.XML.Types as X
import qualified Text.Parsec.Pos as P

type Event = FileReference * BasicEvent XmlLocalName X.Content [XmlAttribute]

type EventParser = ParsecT [Event] () Identity
type AttributeParser = ParsecT [XmlAttribute] () Identity

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

parseEvent :: Stream s m Event => (Event -> Maybe a) -> ParsecT s u m a
parseEvent = tokenPrim show updateSourcePos

parseAttribute :: Stream s m XmlAttribute => (XmlAttribute -> Maybe a) -> ParsecT s u m a
parseAttribute = tokenPrim show (const . const)

satisfy :: Stream s m Event => (Event -> Bool) -> ParsecT s u m Event
satisfy p = tokenPrim show updateSourcePos testEvent
  where testEvent x = if p x then Just x else Nothing

updateSourcePos :: SourcePos -> (FileReference, t) -> s -> SourcePos
updateSourcePos p (r, _) _ = flip P.setSourceColumn column . flip P.setSourceLine line $ p
  where
    beginPos = fileReferenceBegin r
    line = getLine . lineReferenceLine $ beginPos
    column = getColumn . lineReferenceColumn $ beginPos

beginSimple :: Stream s m Event => XmlLocalName -> ParsecT s u m Event
beginSimple n = satisfy isBegin
  where
    isBegin :: Event -> Bool
    isBegin (_, (BasicEventBeginElement n' _)) | n == n' = True
    isBegin _ = False

endSimple :: Stream s m Event => XmlLocalName -> ParsecT s u m Event
endSimple n = satisfy isEnd
  where
    isEnd :: Event -> Bool
    isEnd (_, (BasicEventEndElement n')) | n == n' = True
    isEnd _ = False

elementOpen :: Stream s m Event => Text -> ParsecT s u m [Event]
elementOpen n = beginSimple name *> manyTill anyEvent (try (endSimple name))
  where name = XmlLocalName n

elementSimple :: Text -> EventParser a -> (a -> b) -> EventParser b
elementSimple n p f = f <$> go
  where
    go = beginSimple name *> p <* endSimple name
    name = XmlLocalName n

contentParser :: EventParser Text
contentParser = parseEvent getContent
  where
    getContent :: Event -> Maybe Text
    getContent (_, BasicEventContent (X.ContentText t)) = Just t
    getContent _ = Nothing

newtype Paragraph = Paragraph Text deriving Show

paragraphParser :: EventParser c -> EventParser c
paragraphParser p = elementSimple "p" p id

paragraphSimpleParser :: EventParser Paragraph
paragraphSimpleParser = fmap Paragraph (paragraphParser contentParser)

newtype Title = Title [Paragraph] deriving Show

titleParser :: EventParser Title
titleParser = elementSimple "title" (many paragraphSimpleParser) Title

data Link = Link { linkHref :: Text, linkContent :: Text } deriving Show

newtype Content = Content Text deriving Show
data ParagraphLink
  = ParagraphLinkContent Content
  | ParagraphLinkLink Link
  deriving Show

linkParser :: EventParser Link
linkParser = elementSimple "a" contentParser (Link "")

paragraphLinkParser :: EventParser ParagraphLink
paragraphLinkParser = content <|> link
  where
    content = fmap (ParagraphLinkContent . Content) contentParser
    link = fmap ParagraphLinkLink linkParser

newtype License = License [ParagraphLink] deriving Show
licenseParser :: EventParser License
licenseParser = elementSimple "license" (paragraphParser (many paragraphLinkParser)) License

data Book = Book deriving Show

bookParser :: EventParser Book
bookParser = fmap (const Book) (elementOpen "book")

anyEvent :: Stream s m Event => ParsecT s u m Event
anyEvent = satisfy (const True)

data Sblgnt = Sblgnt { sblgntTitle :: Title, sblgntLicense :: License, sblgntBooks :: [Book] } deriving Show

sblgntParser :: EventParser Sblgnt
sblgntParser = elementSimple "sblgnt" sblgntContent id
  where
    sblgntContent = do
      title <- titleParser
      license <- licenseParser
      books <- many bookParser
      return $ Sblgnt title license books

parseEvents :: FilePath -> [Event] -> ParseError + Sblgnt
parseEvents = parse sblgntParser
