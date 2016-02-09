{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Maybe as Maybe
import qualified Data.Function as Function
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Format.Strict as Format
import System.FilePath ((</>), (<.>))
import qualified System.FilePath.Find as FilePath
import System.FilePath.Find ((~~?))
import qualified System.Directory as Directory
import qualified Text.Greek.IO.Paths as Paths
import qualified Text.Greek.Source.Perseus.Catalog as Catalog
import qualified Text.Greek.Xml.Parse as Parse

main :: IO ()
main = do
  paths <- FilePath.find FilePath.always (FilePath.fileName ~~? "*-grc?.xml") Paths.perseusGreekData
  mapM_ putStrLn paths

loadCatalog :: IO ()
loadCatalog = do
  perseusCatalog <- Parse.readParseEvents Catalog.inventoryParser Paths.perseusInventoryXml
  case perseusCatalog of
    Left es -> mapM_ (Text.putStrLn . Text.pack . show) es
    Right inventory -> do
      infos <- editionInfos
      let sortedInfos = List.sortBy (compare `Function.on` snd) infos
      mapM_ (Text.putStrLn . printEditionInfo) sortedInfos
      where
        works = getWorks inventory
        editions = concatMap workEditions works
        editionFiles = fmap editionPath editions
        editionInfos = traverse getEditionInfo editionFiles
        getEditionInfo x = do
          fileExists <- Directory.doesFileExist x
          return (x, fileExists)
        printEditionInfo (x, fileExists) = Format.format' "{}-{}" (existsMessage, x)
          where existsMessage = if fileExists then "Ok" else "Missing" :: Text.Text

data Work = Work
  { workTitle :: Text.Text
  , workEditions :: [Edition]
  }

data Edition = Edition
  { editionLabel :: Text.Text
  , editionDescription :: Text.Text
  , editionPath :: FilePath
  }

makeWork :: Catalog.Work -> Work
makeWork (Catalog.Work _ _ _ t es) = Work t $ Maybe.catMaybes $ fmap makeOpenEdition es

makeOpenEdition :: Catalog.Edition -> Maybe Edition
makeOpenEdition (Catalog.Edition _ u l d m) | m /= protectedGroup = Edition l d <$> (makeRelativePath u)
makeOpenEdition _ = Nothing

greekLitUrnPrefix :: Text.Text
greekLitUrnPrefix = "greekLit"

protectedGroup :: Text.Text
protectedGroup = "Perseus:collection:Greco-Roman-protected"

makeRelativePath :: Catalog.CtsUrn -> Maybe FilePath
makeRelativePath (Catalog.CtsUrn [p, f]) | p == greekLitUrnPrefix =
  (Paths.perseusGreekData </>) <$> (convertPath $ fmap Text.unpack $ Text.splitOn "." f)
    where
      convertPath :: [String] -> Maybe FilePath
      convertPath [x,y,_] = Just $ x </> y </> (Text.unpack f <.> ".xml")
      convertPath _ = Nothing
makeRelativePath _ = Nothing

isValidWork :: Work -> Bool
isValidWork = (/= 0) . length . workEditions

getWorks :: Catalog.Inventory -> [Work]
getWorks inventory = presentGreekWorks
  where
    textGroups = Catalog.inventoryTextGroups inventory
    flatWorks = concatMap Catalog.textGroupWorks textGroups
    greekWorks = filter ((== "grc") . Catalog.workLang) flatWorks
    greekWorkEditionCount = fmap makeWork greekWorks
    presentGreekWorks = filter isValidWork greekWorkEditionCount
