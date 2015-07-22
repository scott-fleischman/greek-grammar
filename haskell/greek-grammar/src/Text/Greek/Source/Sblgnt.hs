module Text.Greek.Source.Sblgnt where

import Conduit
import System.FilePath
import Text.XML
import Text.XML.Stream.Parse

readEvents :: FilePath -> IO [EventPos]
readEvents path = runResourceT $ sourceFile path =$= parseBytesPos def $$ sinkList
