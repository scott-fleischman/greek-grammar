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
    Right xs -> Text.putStrLn $ Format.format' "groups: {}, Greek works: {}"
      ( length $ greekCounts
      , sum $ greekCounts
      )
      where greekCounts = filter (/= 0) . fmap (length . filter ((== "grc") . Inventory.workLang) . Inventory.textGroupWorks) . Inventory.inventoryTextGroups $ xs
