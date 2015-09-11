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

ctsNamespaceParser :: EventParser [Event]
ctsNamespaceParser = elementOpen (ti "ctsnamespace")

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
