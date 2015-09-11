{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Greek.Source.PerseusInventory where

import Data.Map (Map)
import Data.Text (Text)
import Text.Greek.Xml.Parse
import Text.Parsec.Char
import Text.Parsec.Combinator
import qualified Data.Text as T
import qualified Data.XML.Types as X

tiNamespace :: Text
tiNamespace = "http://chs.harvard.edu/xmlns/cts/ti"

ti :: Text -> X.Name
ti t = X.Name t (Just tiNamespace) Nothing

dcNamespace :: Text
dcNamespace = "http://purl.org/dc/elements/1.1/"

dc :: Text -> X.Name
dc t = X.Name t (Just dcNamespace) Nothing

newtype Description = Description Text deriving Show
tiDescriptionParser :: EventParser Description
tiDescriptionParser = element (ti "description") (xmlLangAttributeValueParser "eng") contentParser (const Description)

data CtsNamespace = CtsNamespace
  { ctsNamespaceAbbr :: Text
  , ctsNamespaceValue :: Text
  , ctsNamespaceDescription :: Description
  } deriving Show
ctsNamespaceParser :: EventParser CtsNamespace
ctsNamespaceParser = elementA (ti "ctsnamespace") $ \as -> CtsNamespace
  <$> getAttribute "abbr" as
  <*> getAttribute "ns" as
  <*> tiDescriptionParser

data Collection = Collection
  { collectionId :: Text
  , collectionTitle :: Text
  , collectionCreator :: Text
  , collectionCoverage :: Text
  , collectionDescription :: Text
  , collectionRights :: Text
  } deriving Show
collectionParser :: EventParser Collection
collectionParser = elementA (ti "collection") $ \as -> Collection
  <$> getAttribute "id" as
  <*> elementContent' (dc "title")
  <*> elementContent' (dc "creator")
  <*> elementContent' (dc "coverage")
  <*> elementContent' (dc "description")
  <*> elementContent' (dc "rights")

data TextGroup = TextGroup
  { textGroupProjid :: Projid
  , textGroupUrn :: Text
  , textGroupName :: Text
  , textGroupWorks :: [Work]
  } deriving Show
textGroupParser :: EventParser TextGroup
textGroupParser = elementA (ti "textgroup") $ \as -> TextGroup
  <$> parseProjidAttribute as
  <*> getAttribute "urn" as
  <*> elementContent' (ti "groupname")
  <*> many1 workParser

data Work = Work
  { workProjid :: Projid
  , workUrn :: Text
  , workLang :: Text
  , workTitle :: Text
  , workEditions :: [Edition]
  } deriving Show
workParser :: EventParser Work
workParser = elementA (ti "work") $ \as -> Work
  <$> parseProjidAttribute as
  <*> getAttribute "urn" as
  <*> getAttribute xmlLangAttribute as
  <*> elementContent' (ti "title")
  <*> manyRights translationParser editionParser

data Edition = Edition
  { editionProjid :: Projid
  , editionUrn :: Text
  , editionLabel :: Text
  , editionDescription :: Text
  , editionMemberOf :: Text
  } deriving Show
editionParser :: EventParser Edition
editionParser = elementA (ti "edition") $ \as -> Edition
  <$> parseProjidAttribute as
  <*> getAttribute "urn" as
  <*> elementContent' (ti "label")
  <*> elementContent' (ti "description")
  <* optionMaybe (elementOpen (ti "online"))
  <*> (emptyElementA (ti "memberof") >>= getAttribute "collection")

data Translation = Translation deriving Show
translationParser :: EventParser Translation
translationParser = fmap (const Translation) $ elementOpen (ti "translation")

data Inventory = Inventory
  { inventoryNamespaces :: [CtsNamespace]
  , inventoryCollections :: [Collection]
  , inventoryTextGroups :: [TextGroup]
  } deriving Show

inventoryParser :: EventParser Inventory
inventoryParser = elementA (ti "TextInventory") $ \_ -> Inventory
  <$> many1 ctsNamespaceParser
  <*> many1 collectionParser
  <*> many1 textGroupParser

data Projid = Projid { projidNamespace :: Text, projidValue :: Text } deriving Show
projidParser :: CharParser Projid
projidParser = Projid
  <$> fmap T.pack (many1 (noneOf ":"))
  <* char ':'
  <*> fmap T.pack (many1 (noneOf ":"))

parseProjidAttribute :: Map X.Name [X.Content] -> EventParser Projid
parseProjidAttribute as = nestParser (getAttribute "projid" as) projidParser
