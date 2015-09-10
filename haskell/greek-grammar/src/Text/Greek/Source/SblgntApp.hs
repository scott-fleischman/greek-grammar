{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Greek.Source.SblgntApp where

import Prelude hiding ((*), (+), Word)
import Text.Greek.Utility
import Text.Greek.Xml
import Text.Greek.Xml.Parsec
import Text.Parsec.Combinator
import Text.Parsec.Prim
