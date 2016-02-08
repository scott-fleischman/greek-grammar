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

perseusFolder :: FilePath
perseusFolder = projectRootPath </> "data" </> "PerseusDL"

perseusCatalog :: FilePath
perseusCatalog = perseusFolder </> "catalog_data"

perseusGreekData :: FilePath
perseusGreekData = perseusFolder </> "canonical-greekLit" </> "data"

perseusInventoryXml :: FilePath
perseusInventoryXml = perseusCatalog </> "perseus" </> "perseuscts.xml"

pagesData :: FilePath
pagesData = projectRootPath </> "html" </> "dev" </> "data"

buildData :: FilePath
buildData = projectRootPath </> "build" </> "data"

buildSblgnt :: FilePath
buildSblgnt = buildData </> "sblgnt"

morphgntRoot :: FilePath
morphgntRoot = projectRootPath </> "data" </> "morphgnt"

morphgntSblgnt :: FilePath
morphgntSblgnt = morphgntRoot </> "sblgnt"
