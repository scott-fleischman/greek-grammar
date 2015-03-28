{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.Greek.Paths where

import Filesystem.Path.CurrentOS (FilePath, (</>), (<.>))

sblgntOsisPath :: FilePath
sblgntOsisPath = "data" </> "sblgnt-osis" </> "SBLGNT" <.> "osis" <.> "xml"

agdaSblgntPath :: FilePath
agdaSblgntPath = "agda" </> "Text" </> "Greek" </> "SBLGNT"

unicodeDataPath :: FilePath
unicodeDataPath = "data" </> "ucd" </> "UnicodeData" <.> "txt"

haskellUnicodeScriptPath :: FilePath
haskellUnicodeScriptPath = "src" </> "Text" </> "Greek" </> "Script" </> "Unicode" <.> "hs"
