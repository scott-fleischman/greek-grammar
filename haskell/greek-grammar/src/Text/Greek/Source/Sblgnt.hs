{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Greek.Source.Sblgnt where

import Prelude hiding ((*), (+), getLine)
import Control.Lens
import Text.Greek.Utility
import Text.Greek.Xml
import Text.Parsec.Pos (SourcePos)
import Text.Parsec.Prim
import qualified Data.XML.Types as X
import qualified Text.Parsec.Pos as P

type Event = FileReference * BasicEvent XmlLocalName X.Content XmlAttributes
type Parser = Parsec Event ()
type GenParser tok st = Parsec [tok] st
type EventParser st = GenParser Event st

readSblgntEvents :: FilePath -> IO ([SblgntError] + [Event])
readSblgntEvents = fmap sblgntTransform . readEvents

sblgntTransform
  :: [XmlInternalError] + [FileReference * X.Event]
  -> [SblgntError] + [Event]
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

satisfy :: Stream s m Event => (Event -> Bool) -> ParsecT s u m Event
satisfy p = tokenPrim show updateSourcePos testEvent
  where testEvent x = if p x then Just x else Nothing

updateSourcePos :: SourcePos -> (FileReference, t) -> s -> SourcePos
updateSourcePos p (r, _) _ = flip P.setSourceColumn column . flip P.setSourceLine line $ p
  where
    beginPos = fileReferenceBegin r
    line = getLine . lineReferenceLine $ beginPos
    column = getColumn . lineReferenceColumn $ beginPos

begin :: Stream s m Event => Event -> ParsecT s u m Event
begin = undefined
