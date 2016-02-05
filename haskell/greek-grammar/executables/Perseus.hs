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
    Right inventory -> mapM_ Text.putStrLn greekWorkInfo
      where
        greekWorks = getGreekWorks inventory
        greekWorkInfo = fmap (\x -> Format.format' "Work: {}, Editions: {}" x) greekWorks

getGreekWorks :: Inventory.Inventory -> [(Text.Text, Int)]
getGreekWorks inventory = presentGreekWorks
  where
    textGroups = Inventory.inventoryTextGroups inventory
    flatWorks = concatMap Inventory.textGroupWorks textGroups
    greekWorks = filter ((== "grc") . Inventory.workLang) flatWorks
    greekWorkEditionCount = fmap (\x -> (Inventory.workTitle x, length $ Inventory.workEditions x)) greekWorks
    presentGreekWorks = filter (\x -> snd x /= 0) greekWorkEditionCount