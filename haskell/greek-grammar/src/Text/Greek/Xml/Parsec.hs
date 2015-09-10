{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Greek.Xml.Parsec where

import Prelude hiding ((*), (+), getLine)
import Data.Functor.Identity
import Data.Text (Text)
import Text.Greek.Utility
import Text.Greek.Xml
import Text.Parsec.Combinator
import Text.Parsec.Pos (SourcePos)
import Text.Parsec.Prim
import qualified Data.XML.Types as X
import qualified Text.Parsec.Pos as P

type Event = FileReference * BasicEvent X.Name X.Content [XmlAttribute]

type EventParser = ParsecT [Event] () Identity
type AttributeParser = ParsecT [XmlAttribute] () Identity

parseEvent :: Stream s m Event => (Event -> Maybe a) -> ParsecT s u m a
parseEvent = tokenPrim show updateEventPos

parseAttribute :: Stream s m XmlAttribute => (XmlAttribute -> Maybe a) -> ParsecT s u m a
parseAttribute = tokenPrim show (const . const)

satisfy :: Stream s m Event => (Event -> Bool) -> ParsecT s u m Event
satisfy p = tokenPrim show updateEventPos testEvent
  where testEvent x = if p x then Just x else Nothing

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

end :: Stream s m Event => X.Name -> ParsecT s u m Event
end n = satisfy isEnd
  where
    isEnd :: Event -> Bool
    isEnd (_, (BasicEventEndElement n')) | n == n' = True
    isEnd _ = False

emptyElement :: X.Name -> EventParser ()
emptyElement n = beginSimple n <* end n

elementOpen :: Stream s m Event => X.Name -> ParsecT s u m [Event]
elementOpen n = beginSimple n *> manyTill anyEvent (try (end n))

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

elementContent :: X.Name -> EventParser Text
elementContent = flip elementSimple contentParser

elementContentReference :: X.Name -> EventParser (FileReference, Text)
elementContentReference = flip elementSimple contentReferenceParser

anyEvent :: Stream s m Event => ParsecT s u m Event
anyEvent = satisfy (const True)
