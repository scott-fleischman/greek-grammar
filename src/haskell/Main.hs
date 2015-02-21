{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude ((.), ($), Show(show))
import Data.Default (def)
import Data.Text (pack)
import Data.Text.IO (putStrLn)
import Filesystem.Path (FilePath, (</>), (<.>))
import System.IO (IO)
import Text.XML (readFile)
import Text.Greek.NewTestament.Bible ()
import Text.Greek.NewTestament.SBL

sblgntPath :: FilePath
sblgntPath = ".." </> "sblgnt" </> "osis" </> "SBLGNT" <.> "osis" <.> "xml" -- http://sblgnt.com/

main :: IO ()
main = do
  doc <- readFile def sblgntPath
  let bible = loadOsis doc
  putStrLn . pack . show $ bible
