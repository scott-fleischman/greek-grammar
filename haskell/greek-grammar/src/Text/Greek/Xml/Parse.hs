{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Greek.Xml.Parse where

import Prelude hiding ((*), (+), getLine)
import Data.Functor.Identity
import Data.Text (Text)
import Text.Greek.FileReference
import Text.Greek.Utility
import Text.Greek.Xml.Common
import Text.Greek.Xml.Event
import Text.Parsec.Combinator
import Text.Parsec.Pos (SourcePos)
import Text.Parsec.Prim
import qualified Data.XML.Types as X
import qualified Text.Parsec.Pos as P

type Event = FileReference * BasicEvent

type EventParser = ParsecT [Event] () Identity
type AttributeParser = ParsecT [XmlAttribute] () Identity

parseEvent :: Stream s m Event => (Event -> Maybe a) -> ParsecT s u m a
parseEvent = tokenPrim show updateEventPos

parseAttribute :: Stream s m XmlAttribute => (XmlAttribute -> Maybe a) -> ParsecT s u m a
parseAttribute = tokenPrim show (const . const)

updateEventPos :: SourcePos -> (FileReference, t) -> s -> SourcePos
updateEventPos p (r, _) _ = flip P.setSourceColumn column . flip P.setSourceLine line $ p
  where
    beginPos = fileReferenceBegin r
    line = getLine . lineReferenceLine $ beginPos
    column = getColumn . lineReferenceColumn $ beginPos

emptyAttributes :: AttributeParser ()
emptyAttributes = fmap (const ()) eof

simpleAttributeParser :: Text -> AttributeParser Text
simpleAttributeParser n = parseAttribute parseSimple
  where
    parseSimple ((X.Name n' Nothing Nothing), [X.ContentText t]) | n == n' = Just t
    parseSimple _ = Nothing

xmlLangAttributeParser :: Text -> AttributeParser Text
xmlLangAttributeParser v = parseAttribute parseSimple <?> "Invalid xml:lang attribute"
  where
    parseSimple ((X.Name "lang" (Just ns) (Just p)), [X.ContentText t]) | ns == xmlNamespace, p == xmlNamespacePrefix, t == v = Just t
    parseSimple _ = Nothing

begin :: Stream s m Event => X.Name -> AttributeParser a -> ParsecT s u m a
begin n ap = parseEvent parseBeginEvent
  where
    parseBeginEvent (_, (BasicEventBeginElement n' a)) | n == n' = case parse ap "" a of
      Left _ -> Nothing
      Right x -> Just x
    parseBeginEvent _ = Nothing

beginSimple :: Stream s m Event => X.Name -> ParsecT s u m ()
beginSimple n = begin n emptyAttributes

end :: Stream s m Event => X.Name -> ParsecT s u m ()
end n = parseEvent parseEndEvent
  where
    parseEndEvent :: Event -> Maybe ()
    parseEndEvent (_, (BasicEventEndElement n')) | n == n' = Just ()
    parseEndEvent _ = Nothing

emptyElement :: X.Name -> EventParser ()
emptyElement n = beginSimple n <* end n

elementOpen :: Stream s m Event => X.Name -> ParsecT s u m [Event]
elementOpen n = begin n anyAttribute *> manyTill anyEvent (try (end n))

element :: X.Name -> AttributeParser a -> EventParser b -> (a -> b -> c) -> EventParser c
element n ap cp f = go
  where
    go = do
      a <- begin n ap
      b <- cp
      _ <- end n
      return $ f a b

elementSimple :: X.Name -> EventParser a -> EventParser a
elementSimple n p = beginSimple n *> p <* end n

contentReferenceParser :: EventParser (FileReference, Text)
contentReferenceParser = parseEvent getContent
  where
    getContent :: Event -> Maybe (FileReference, Text)
    getContent (r, BasicEventContent (X.ContentText t)) = Just (r, t)
    getContent _ = Nothing

contentParser :: EventParser Text
contentParser = fmap snd contentReferenceParser

contentValueParser :: Text -> EventParser Text
contentValueParser t = parseEvent getContent
  where
    getContent :: Event -> Maybe Text
    getContent (_, BasicEventContent (X.ContentText t')) | t == t' = Just t'
    getContent _ = Nothing

elementContent :: X.Name -> EventParser Text
elementContent = flip elementSimple contentParser

elementContentReference :: X.Name -> EventParser (FileReference, Text)
elementContentReference = flip elementSimple contentReferenceParser

anyEvent :: Stream s m Event => ParsecT s u m Event
anyEvent = parseEvent Just

anyAttribute :: Stream s m XmlAttribute => ParsecT s u m XmlAttribute
anyAttribute = parseAttribute Just

readParseEvents :: EventParser a -> FilePath -> IO ([XmlError] + a)
readParseEvents parser path = fmap ((=<<) (liftError XmlErrorParse . parse parser path)) . readBasicEvents $ path
