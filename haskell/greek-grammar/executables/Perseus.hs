{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Format.Strict as Format
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
        editionInfo = fmap (\(t, e) -> Format.format' "Work:{}, Title:{}, Desc:{}" (t, editionLabel e, editionDescription e)) editions

data Work = Work
  { workTitle :: Text.Text
  , workEditions :: [Edition]
  }

data Edition = Edition
  { editionLabel :: Text.Text
  , editionDescription :: Text.Text
  , editionRelativePath :: [Text.Text]
  }

makeWork :: Inventory.Work -> Work
makeWork (Inventory.Work _ _ _ t es) = Work t (fmap makeEdition es)

makeEdition :: Inventory.Edition -> Edition
makeEdition (Inventory.Edition _ u l d _) = Edition l d (makeRelativePath u)

makeRelativePath :: Inventory.CtsUrn -> [Text.Text]
makeRelativePath (Inventory.CtsUrn ("greekLit" : ns)) = ns
makeRelativePath _ = []

getWorks :: Inventory.Inventory -> [Work]
getWorks inventory = presentGreekWorks
  where
    textGroups = Inventory.inventoryTextGroups inventory
    flatWorks = concatMap Inventory.textGroupWorks textGroups
    greekWorks = filter ((== "grc") . Inventory.workLang) flatWorks
    greekWorkEditionCount = fmap makeWork greekWorks
    presentGreekWorks = filter ((/= 0) . length . workEditions) greekWorkEditionCount
