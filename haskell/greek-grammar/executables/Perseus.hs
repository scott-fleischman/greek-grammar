{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Format as Format
import qualified Data.Text.Format.Strict as Format
import System.FilePath ((</>), (<.>))
import qualified Text.Greek.IO.Paths as Paths
import qualified Text.Greek.Source.PerseusInventory as Inventory
import qualified Text.Greek.Xml.Parse as Parse

main :: IO ()
main = do
  perseusCatalog <- Parse.readParseEvents Inventory.inventoryParser Paths.perseusInventoryXml
  case perseusCatalog of
    Left es -> mapM_ (Text.putStrLn . Text.pack . show) es
    Right inventory -> mapM_ Text.putStrLn editionInfo
      where
        works = getWorks inventory
        editions = concatMap (\w -> (fmap (\e -> (workTitle w, e)) (workEditions w))) works
        editionInfo = fmap (\(_, e) -> Format.format' "{}" (Format.Only $ editionPath e)) editions

data Work = Work
  { workTitle :: Text.Text
  , workEditions :: [Edition]
  }

data Edition = Edition
  { editionLabel :: Text.Text
  , editionDescription :: Text.Text
  , editionPath :: FilePath
  }

makeWork :: Inventory.Work -> Work
makeWork (Inventory.Work _ _ _ t es) = Work t $ Maybe.catMaybes $ fmap makeEdition es

makeEdition :: Inventory.Edition -> Maybe Edition
makeEdition (Inventory.Edition _ u l d _) = Edition l d <$> (makeRelativePath u)

greekLitUrnPrefix :: Text.Text
greekLitUrnPrefix = "greekLit"

makeRelativePath :: Inventory.CtsUrn -> Maybe FilePath
makeRelativePath (Inventory.CtsUrn [p, f]) | p == greekLitUrnPrefix =
  (Paths.perseusGreekData </>) <$> (convertPath $ fmap Text.unpack $ Text.splitOn "." f)
    where
      convertPath :: [String] -> Maybe FilePath
      convertPath [x,y,_] = Just $ x </> y </> (Text.unpack f <.> ".xml")
      convertPath _ = Nothing
makeRelativePath _ = Nothing

isValidWork :: Work -> Bool
isValidWork = (/= 0) . length . workEditions

getWorks :: Inventory.Inventory -> [Work]
getWorks inventory = presentGreekWorks
  where
    textGroups = Inventory.inventoryTextGroups inventory
    flatWorks = concatMap Inventory.textGroupWorks textGroups
    greekWorks = filter ((== "grc") . Inventory.workLang) flatWorks
    greekWorkEditionCount = fmap makeWork greekWorks
    presentGreekWorks = filter isValidWork greekWorkEditionCount
