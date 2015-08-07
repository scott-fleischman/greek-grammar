{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Text.Greek.Source.Sblgnt where

import Prelude hiding ((*), (+), log, FilePath)
import Control.Lens
import Data.Text (Text)
import Text.Greek.Utility
import Text.Greek.Xml
import qualified Prelude as X (FilePath)

readSblgntEvents :: X.FilePath -> IO ([ErrorMessage] + [FileReference * EventSimple])
readSblgntEvents = fmap (>>= tx) . readEvents

tx :: [FileReference * EventAll]
   -> [ErrorMessage] + [FileReference * EventSimple]
tx x = return x
  >>= tx9
  >>= tx10
  >>= tx11
  >>= tx12
  >>= tx13

type EventSimple
  = EventBeginElement * XmlName * XmlAttributes
  + EventEndElement * XmlName
  + EventContent * XmlContent

tx9 :: Handler e (a * (EventBeginDoctype * Text * (Maybe XmlExternalId) + b)) =>          
                 [a * (EventBeginDoctype * Text * (Maybe XmlExternalId) + b)]
    -> [e] +     [a * (                                                   b)]
tx9 = handleMap _2 tryDrop1

tx10 :: Handler e (a * (EventEndDoctype + b)) =>
                  [a * (EventEndDoctype + b)]
     -> [e] +     [a * (                  b)]
tx10 = handleMap _2 tryDrop1

tx11 :: Handler e (a * (EventInstruction * XmlInstruction + b)) =>
                  [a * (EventInstruction * XmlInstruction + b)]
     -> [e] +     [a * (                                    b)]
tx11 = handleMap _2 tryDrop1

tx12 :: Handler e (a * (b1 + b2 + b3 + EventComment * Text + c)) =>
                  [a * (b1 + b2 + b3 + EventComment * Text + c)]
     -> [e] +     [a * (b1 + b2 + b3 +                       c)]
tx12 = handleMap _2 tryDrop4

tx13 :: Handler e (a * (b1 + b2 + b3 + EventCDATA * Text)) =>
                  [a * (b1 + b2 + b3 + EventCDATA * Text)]
     -> [e] +     [a * (b1 + b2 + b3                    )]
tx13 = handleMap _2 tryDrop4e
