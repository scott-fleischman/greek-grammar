module Text.Greek.Xml.Common where

import Text.Greek.FileReference
import Text.Parsec.Error (ParseError)
import qualified Data.Conduit.Attoparsec as X
import qualified Data.XML.Types as X

data XmlError
  = XmlErrorNonBasicEvent FileReference X.Event
  | XmlErrorInternal XmlInternalError
  | XmlErrorParse ParseError
  deriving (Show)

data XmlInternalError
  = XmlInternalErrorEmptyEvents
  | XmlInternalErrorExpectedBeginDocument (Maybe X.PositionRange, X.Event)
  | XmlInternalErrorExpectedEndDocument (Maybe X.PositionRange, X.Event)
  | XmlInternalErrorUnexpectedEmptyPositionRange (Maybe X.PositionRange, X.Event)
  deriving (Show)
