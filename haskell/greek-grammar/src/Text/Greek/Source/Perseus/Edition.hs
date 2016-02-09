{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Greek.Source.Perseus.Edition where

import Data.Text (Text)
import Text.Greek.Parse.Utility
import Text.Greek.Xml.Parse
import Text.Parsec.Char
import Text.Parsec.Combinator
import qualified Data.Text as Text
import qualified Data.XML.Types as Xml

teiNamespace :: Text
teiNamespace = "http://www.tei-c.org/ns/1.0"

tei :: Text -> X.Name
tei t = X.Name t (Just dcNamespace) Nothing
