{-# LANGUAGE OverloadedStrings #-}

module Text.Greek.Source.PerseusInventory where

import Data.Text (Text)
import Text.Greek.Xml.Parse
import Text.Parsec.Combinator
import qualified Data.XML.Types as X

tiNamespace :: Text
tiNamespace = "http://chs.harvard.edu/xmlns/cts/ti"

ti :: Text -> X.Name
ti t = X.Name t (Just tiNamespace) Nothing

newtype Description = Description Text deriving Show
tiDescriptionParser :: EventParser Description
tiDescriptionParser = element (ti "description") (xmlLangAttributeParser "eng") contentParser (const Description)

data CtsNamespace = CtsNamespace { ctsNamespaceAbbr :: Text, ctsNamespaceValue :: Text, ctsNamespaceDescription :: Description } deriving Show
ctsNamespaceParser :: EventParser CtsNamespace
ctsNamespaceParser = elementA (ti "ctsnamespace") $ \as -> do
  abbr <- getAttribute "abbr" as
  ns <- getAttribute "ns" as
  desc <- tiDescriptionParser
  return $ CtsNamespace abbr ns desc

collectionParser :: EventParser [Event]
collectionParser = elementOpen (ti "collection")

textGroupParser :: EventParser [Event]
textGroupParser = elementOpen (ti "textgroup")

perseusInventoryParser :: EventParser ()
perseusInventoryParser = element (ti "TextInventory") anyAttribute body (\_ _ -> ())
  where
    body = do
      _ <- many1 ctsNamespaceParser
      _ <- many1 collectionParser
      _ <- many1 textGroupParser
      return ()
