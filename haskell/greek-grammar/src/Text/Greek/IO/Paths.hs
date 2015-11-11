{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.Greek.IO.Paths where

import System.FilePath

projectRootPath :: FilePath
projectRootPath = "."

sblgntOsisPath :: FilePath
sblgntOsisPath = projectRootPath </> "data" </> "sblgnt-osis" </> "SBLGNT" <.> "osis" <.> "xml"

sblgntXmlFolderPath :: FilePath
sblgntXmlFolderPath = projectRootPath </> "data" </> "sblgnt-xml"

sblgntXmlPath :: FilePath
sblgntXmlPath = sblgntXmlFolderPath </> "sblgnt" <.> "xml"

sblgntAppXmlPath :: FilePath
sblgntAppXmlPath = sblgntXmlFolderPath </> "sblgntapp" <.> "xml"

agdaSblgntPath :: FilePath
agdaSblgntPath = projectRootPath </> "agda" </> "Text" </> "Greek" </> "SBLGNT"

unicodeDataPath :: FilePath
unicodeDataPath = projectRootPath </> "data" </> "ucd" </> "UnicodeData" <.> "txt"

perseusFolderPath :: FilePath
perseusFolderPath = projectRootPath </> "data" </> "PerseusDL"

perseusCatalogPath :: FilePath
perseusCatalogPath = perseusFolderPath </> "catalog_data"

perseusGreekPath :: FilePath
perseusGreekPath = perseusFolderPath </> "canonical-greekLit"

perseusInventoryXmlPath :: FilePath
perseusInventoryXmlPath = perseusCatalogPath </> "perseus" </> "perseuscts.xml"

pagesData :: FilePath
pagesData = projectRootPath </> "html" </> "dev" </> "data"
