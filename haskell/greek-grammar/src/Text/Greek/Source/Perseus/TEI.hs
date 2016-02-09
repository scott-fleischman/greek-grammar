{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Greek.Source.Perseus.TEI where

--import Text.Greek.Parse.Utility
import Text.Greek.Xml.Parse
--import Text.Parsec.Char
--import Text.Parsec.Combinator
import qualified Data.Text as Text
import qualified Data.XML.Types as Xml

teiNamespace :: Text.Text
teiNamespace = "http://www.tei-c.org/ns/1.0"

tei :: Text.Text -> Xml.Name
tei t = Xml.Name t (Just teiNamespace) Nothing

data Document = Document
  { documentHeader :: TeiHeader
  , documentText :: Text
  } deriving Show

documentParser :: EventParser Document
documentParser = elementA (tei "TEI") $ \_ -> Document
  <$> teiHeaderParser
  <*> textParser

data TeiHeader = TeiHeader
  {
  } deriving Show

teiHeaderParser :: EventParser TeiHeader
teiHeaderParser = do
  _ <- elementOpen (tei "teiHeader")
  return TeiHeader

data Text = Text
  {
  } deriving Show

textParser :: EventParser Text
textParser = do
  _ <- elementOpen (tei "text")
  return Text
