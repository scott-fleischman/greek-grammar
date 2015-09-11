{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Greek.Xml.Parse where

import Prelude hiding ((*), (+), getLine)
import Data.Either
import Data.Functor.Identity
import Data.Map (Map)
import Data.Text (Text)
import Text.Greek.FileReference
import Text.Greek.Utility
import Text.Greek.Xml.Common
import Text.Greek.Xml.Event
import Text.Parsec.Combinator
import Text.Parsec.Pos (SourcePos)
import Text.Parsec.Prim
import qualified Data.Map as M
import qualified Data.Text as T
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

xmlLangAttribute :: X.Name
xmlLangAttribute = X.Name "lang" (Just xmlNamespace) (Just xmlNamespacePrefix)

xmlLangAttributeValueParser :: Text -> AttributeParser Text
xmlLangAttributeValueParser v = parseAttribute parseSimple <?> "xml:lang attribute value"
  where
    parseSimple (n, [X.ContentText t]) | n == xmlLangAttribute, t == v = Just t
    parseSimple _ = Nothing

begin :: Stream s m Event => X.Name -> AttributeParser a -> ParsecT s u m a
begin n ap = parseEvent parseBeginEvent
  where
    parseBeginEvent (_, (BasicEventBeginElement n' a)) | n == n' = case parse ap "" a of
      Left _ -> Nothing
      Right x -> Just x
    parseBeginEvent _ = Nothing

beginA :: Stream s m Event => X.Name -> ParsecT s u m [XmlAttribute]
beginA n = parseEvent parseBeginEvent
  where
    parseBeginEvent (_, (BasicEventBeginElement n' a)) | n == n' = Just a
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

emptyElementA :: X.Name -> EventParser (Map X.Name [X.Content])
emptyElementA n = M.fromList <$> beginA n <* end n

elementOpen :: Stream s m Event => X.Name -> ParsecT s u m [Event]
elementOpen n = begin n (many anyAttribute) *> manyTill anyEvent (try (end n))

element :: X.Name -> AttributeParser a -> EventParser b -> (a -> b -> c) -> EventParser c
element n ap cp f = do
  a <- begin n ap
  b <- cp
  _ <- end n
  return $ f a b

elementA :: X.Name -> (Map X.Name [X.Content] -> EventParser b) -> EventParser b
elementA n f = do
  as <- beginA n
  b <- f (M.fromList as)
  _ <- end n
  return b

getAttribute :: Stream s m Event => X.Name -> Map X.Name [X.Content] -> ParsecT s u m Text
getAttribute n m = case M.lookup n m of
  Just cs -> fmap T.concat $ mapM tryC cs
  Nothing -> fail $ show n ++ " not found"
  where
    tryC (X.ContentText t) = return t
    tryC _ = fail "unexpected attribute entity"

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

elementContent' :: X.Name -> EventParser Text
elementContent' = flip elementA (const (many contentParser >>= return . T.concat))

elementContentReference :: X.Name -> EventParser (FileReference, Text)
elementContentReference = flip elementSimple contentReferenceParser

anyEvent :: Stream s m Event => ParsecT s u m Event
anyEvent = parseEvent Just

anyAttribute :: Stream s m XmlAttribute => ParsecT s u m XmlAttribute
anyAttribute = parseAttribute Just

manyRights :: Stream s m x => ParsecT s u m a -> ParsecT s u m b -> ParsecT s u m [b]
manyRights a b = fmap rights $ many (fmap Left a <|> fmap Right b)

readParseEvents :: EventParser a -> FilePath -> IO ([XmlError] + a)
readParseEvents parser path = fmap ((=<<) (liftError XmlErrorParse . parse parser path)) . readBasicEvents $ path
