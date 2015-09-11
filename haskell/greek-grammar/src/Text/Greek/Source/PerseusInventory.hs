{-# LANGUAGE OverloadedStrings #-}

module Text.Greek.Source.PerseusInventory where

import Data.Text (Text)
import Text.Greek.Xml.Parse
import Text.Parsec.Combinator
import Text.Parsec.Prim
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

data CtsNamespace = CtsNamespace { ctsNamespaceAbbr :: Text, ctsNamespaceValue :: Text, ctsNamespaceDescription :: Description } deriving Show
ctsNamespaceParser :: EventParser CtsNamespace
ctsNamespaceParser = elementA (ti "ctsnamespace") $ \as -> do
  abbr <- getAttribute "abbr" as
  ns <- getAttribute "ns" as
  desc <- tiDescriptionParser
  return $ CtsNamespace abbr ns desc

data Collection = Collection
  { collectionId :: Text
  , collectionTitle :: Text
  , collectionCreator :: Text
  , collectionCoverage :: Text
  , collectionDescription :: Text
  , collectionRights :: Text
  } deriving Show
collectionParser :: EventParser Collection
collectionParser = elementA (ti "collection") $ \as -> do
  id' <- getAttribute "id" as
  title <- elementContent' (dc "title")
  creator <- elementContent' (dc "creator")
  coverage <- elementContent' (dc "coverage")
  description <- elementContent' (dc "description")
  rights <- elementContent' (dc "rights")
  return $ Collection id' title creator coverage description rights

data TextGroup = TextGroup
  { textGroupProjid :: Text
  , textGroupUrn :: Text
  , textGroupName :: Text
  , textGroupWorks :: [Work]
  } deriving Show
textGroupParser :: EventParser TextGroup
textGroupParser = elementA (ti "textgroup") $ \as -> do
  projid <- getAttribute "projid" as
  urn <- getAttribute "urn" as
  name <- elementContent' (ti "groupname")
  works <- many1 workParser
  return $ TextGroup projid urn name works

data Work = Work
  { workProjid :: Text
  , workUrn :: Text
  , workLang :: Text
  , workTitle :: Text
  , workEditions :: [Edition]
  } deriving Show
workParser :: EventParser Work
workParser = elementA (ti "work") $ \as -> do
  projid   <- getAttribute "projid" as         <?> "work projid"
  urn      <- getAttribute "urn" as            <?> "work urn"
  lang     <- getAttribute xmlLangAttribute as <?> "work lang"
  title    <- elementContent' (ti "title")     <?> "work title"
  editions <- manyRights (translationParser <?> "work translation") (editionParser <?> "work edition")
  return $ Work projid urn lang title editions

data Edition = Edition
  {
  } deriving Show
editionParser :: EventParser Edition
editionParser = fmap (const Edition) $ elementOpen (ti "edition")

data Translation = Translation
  {
  } deriving Show
translationParser :: EventParser Translation
translationParser = fmap (const Translation) $ elementOpen (ti "translation")

perseusInventoryParser :: EventParser ()
perseusInventoryParser = element (ti "TextInventory") anyAttribute body (\_ _ -> ())
  where
    body = do
      _ <- many1 ctsNamespaceParser
      _ <- many1 collectionParser
      _ <- many1 textGroupParser
      return ()
