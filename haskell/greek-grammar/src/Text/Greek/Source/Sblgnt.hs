{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Greek.Source.Sblgnt where

import Prelude hiding ((*), (+), Word)
import Control.Lens
import Text.Greek.Utility
import Text.Greek.Xml
import qualified Data.XML.Types as X


readSblgntEvents :: FilePath -> IO
  ( [SblgntError]
  + [FileReference * BasicEvent XmlLocalName X.Content XmlAttributes]
  )
readSblgntEvents = fmap sblgntTransform . readEvents

sblgntTransform
  :: [XmlInternalError] + [FileReference * X.Event]
  ->  [SblgntError]
    + [FileReference * BasicEvent XmlLocalName X.Content XmlAttributes]
sblgntTransform x
  =   liftErrors SblgntErrorXmlInternal x
  >>. dropComments
  >>. trimContent _2
  >>= liftErrors SblgntErrorXml . toBasicEvents
  >>= tryOverAll (_2 . _BasicEventBeginElement . _1) tryDropNamespace (errorContext SblgntErrorUnexpectedNamespace _1)
  >>= tryOverAll (_2 . _BasicEventEndElement) tryDropNamespace (errorContext SblgntErrorUnexpectedNamespace _1)

data SblgntError
  = SblgntErrorXmlInternal XmlInternalError
  | SblgntErrorXml (XmlError FileReference)
  | SblgntErrorUnexpectedNamespace FileReference X.Name
  deriving (Show)
