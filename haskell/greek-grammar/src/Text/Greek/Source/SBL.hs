module Text.Greek.Source.SBL where

import Text.XML

documentToString :: Document -> String
documentToString x = show . elementName . documentRoot $ x
