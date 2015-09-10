{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.Greek.Paths where

import System.FilePath

projectRootPath :: FilePath
projectRootPath = "."

sblgntOsisPath :: FilePath
sblgntOsisPath = projectRootPath </> "data" </> "sblgnt-osis" </> "SBLGNT" <.> "osis" <.> "xml"

sblgntFolderPath :: FilePath
sblgntFolderPath = projectRootPath </> "data" </> "sblgnt-xml"

sblgntXmlPath :: FilePath
sblgntXmlPath = sblgntFolderPath </> "sblgnt" <.> "xml"

sblgntAppXmlPath :: FilePath
sblgntAppXmlPath = sblgntFolderPath </> "sblgntapp" <.> "xml"

agdaSblgntPath :: FilePath
agdaSblgntPath = projectRootPath </> "agda" </> "Text" </> "Greek" </> "SBLGNT"

unicodeDataPath :: FilePath
unicodeDataPath = projectRootPath </> "data" </> "ucd" </> "UnicodeData" <.> "txt"
