{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import BasicPrelude hiding (readFile)
import Data.Set (fromList, toList)
import Data.Text (pack)
import Text.XML
import Text.XML.Cursor

osisElement :: Text -> Cursor -> [Cursor]
osisElement localName = element $ Name localName (Just "http://www.bibletechnologies.net/2003/OSIS/namespace") Nothing

sblgntWords :: Document -> [Text]
sblgntWords d = fromDocument d $// osisElement "w" &// content

distinctWords :: [Text] -> [Text]
distinctWords ws = toList $ fromList ws

main :: IO ()
main = do
  sblgntDocument <- readFile def $ ".." </> "sblgnt" </> "osis" </> "SBLGNT" <.> "osis" <.> "xml"
  mapM_ putStrLn $ distinctWords $ sblgntWords sblgntDocument
