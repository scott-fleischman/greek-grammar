{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.Greek.Paths where

import System.FilePath

projectRootPath :: FilePath
projectRootPath = ".." </> ".."

sblgntOsisPath :: FilePath
sblgntOsisPath = projectRootPath </> "data" </> "sblgnt-osis" </> "SBLGNT" <.> "osis" <.> "xml"

agdaSblgntPath :: FilePath
agdaSblgntPath = projectRootPath </> "agda" </> "Text" </> "Greek" </> "SBLGNT"

unicodeDataPath :: FilePath
unicodeDataPath = projectRootPath </> "data" </> "ucd" </> "UnicodeData" <.> "txt"
