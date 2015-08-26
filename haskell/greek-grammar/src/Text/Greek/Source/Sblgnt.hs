{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Greek.Source.Sblgnt where

import Prelude hiding ((*), (+))
import Control.Lens
import Text.Greek.Utility
import Text.Greek.Xml
import qualified Data.XML.Types as X
import qualified Text.Greek.Sublist as S


readSblgntEvents :: FilePath -> IO ([SblgntError] + [FileReference * BasicEvent Element10 X.Content XmlAttributes])
readSblgntEvents = fmap sblgntTransform . readEvents

sblgntTransform
  :: [XmlInternalError] + [FileReference * X.Event]
  -> [SblgntError] + [FileReference * BasicEvent Element10 X.Content XmlAttributes]
sblgntTransform x
  =   liftError SblgntErrorXmlInternal x
  >>. dropComments
  >>. trimContent _2
  >>= liftError SblgntErrorXml . toBasicEvents
  >>= liftError SblgntErrorXml . tryOverAll (_2 . _BasicEventBeginElement . _1) tryDropNamespace (errorContext XmlErrorUnexpectedNamespace _1)
  >>= liftError SblgntErrorXml . tryOverAll (_2 . _BasicEventEndElement) tryDropNamespace (errorContext XmlErrorUnexpectedNamespace _1)
  >>= tryOverAll (_2 . _BasicEventBeginElement . _1) toElement11 (errorContext SblgntErrorUnexpectedElementName _1)
  >>= tryOverAll (_2 . _BasicEventEndElement) toElement11 (errorContext SblgntErrorUnexpectedElementName _1)
  >>= liftError SblgntErrorSblgntSublist . over _Left pure . S.foldrSublist splitSblgnt
  >>= split . over (each . _Left) SblgntErrorUnexpectedTopLevel
  >>= tryOverAll (_1 . _2) empty (errorContext SblgntErrorInvalidSblgntAttributes (_1 . _1))
  >>. fmap (view _3)
  >>= single SblgntErrorEmptyTopLevel SblgntErrorUnexpectedTopLevels

data SblgntError
  = SblgntErrorXmlInternal XmlInternalError
  | SblgntErrorXml (XmlError FileReference)
  | SblgntErrorUnexpectedElementName FileReference XmlLocalName
  | SblgntErrorSblgntSublist (S.Error (FileReference * XmlAttributes) FileReference)
  | SblgntErrorUnexpectedTopLevel (FileReference * BasicEvent Element10 X.Content XmlAttributes)
  | SblgntErrorInvalidSblgntAttributes FileReference XmlAttributes
  | SblgntErrorEmptyTopLevel
  | SblgntErrorUnexpectedTopLevels [FileReference * BasicEvent Element10 X.Content XmlAttributes]
  deriving (Show)

data Element11
  = Element11Sblgnt
  | Element11A
  | Element11Book
  | Element11License
  | Element11MarkEnd
  | Element11P
  | Element11Prefix
  | Element11Suffix
  | Element11Title
  | Element11VerseNumber
  | Element11W
  deriving (Eq, Ord, Show)

toElement11 :: XmlLocalName -> XmlLocalName + Element11
toElement11 (XmlLocalName "sblgnt"      ) = Right Element11Sblgnt
toElement11 (XmlLocalName "a"           ) = Right Element11A
toElement11 (XmlLocalName "book"        ) = Right Element11Book
toElement11 (XmlLocalName "license"     ) = Right Element11License
toElement11 (XmlLocalName "mark-end"    ) = Right Element11MarkEnd
toElement11 (XmlLocalName "p"           ) = Right Element11P
toElement11 (XmlLocalName "prefix"      ) = Right Element11Prefix
toElement11 (XmlLocalName "suffix"      ) = Right Element11Suffix
toElement11 (XmlLocalName "title"       ) = Right Element11Title
toElement11 (XmlLocalName "verse-number") = Right Element11VerseNumber
toElement11 (XmlLocalName "w"           ) = Right Element11W
toElement11 t                             = Left t

data Element10
  = Element10A
  | Element10Book
  | Element10License
  | Element10MarkEnd
  | Element10P
  | Element10Prefix
  | Element10Suffix
  | Element10Title
  | Element10VerseNumber
  | Element10W
  deriving (Eq, Ord, Show)

splitSblgnt11 :: Element11 -> () + Element10
splitSblgnt11 Element11Sblgnt      = Left ()
splitSblgnt11 Element11A           = Right Element10A
splitSblgnt11 Element11Book        = Right Element10Book
splitSblgnt11 Element11License     = Right Element10License
splitSblgnt11 Element11MarkEnd     = Right Element10MarkEnd
splitSblgnt11 Element11P           = Right Element10P
splitSblgnt11 Element11Prefix      = Right Element10Prefix
splitSblgnt11 Element11Suffix      = Right Element10Suffix
splitSblgnt11 Element11Title       = Right Element10Title
splitSblgnt11 Element11VerseNumber = Right Element10VerseNumber
splitSblgnt11 Element11W           = Right Element10W

splitSblgnt
  :: (FileReference * BasicEvent Element11 X.Content XmlAttributes)
  -> S.TopLevel
    (FileReference * XmlAttributes)
    (FileReference)
    (FileReference * BasicEvent Element10 X.Content XmlAttributes)
splitSblgnt (r, BasicEventBeginElement e a) = case splitSblgnt11 e of
  Left _ -> S.TopLevelBegin (r, a)
  Right e' -> S.TopLevelPass (r, BasicEventBeginElement e' a)
splitSblgnt (r, BasicEventEndElement e) = case splitSblgnt11 e of
  Left _ -> S.TopLevelEnd r
  Right e' -> S.TopLevelPass (r, BasicEventEndElement e')
splitSblgnt (r, BasicEventContent c) = S.TopLevelPass (r, BasicEventContent c)
