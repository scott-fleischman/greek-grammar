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


readSblgntEvents :: FilePath -> IO ([SblgntError] + [FileReference * BasicEvent ElementAll' X.Content XmlAttributes])
readSblgntEvents = fmap sblgntTransform . readEvents

sblgntTransform
  :: [XmlInternalError] + [FileReference * X.Event]
  -> [SblgntError] + [FileReference * BasicEvent ElementAll' X.Content XmlAttributes]
sblgntTransform x
  =   liftError SblgntErrorXmlInternal x
  >>. dropComments
  >>. trimContent _2
  >>= liftError SblgntErrorXml . toBasicEvents
  >>= liftError SblgntErrorXml . tryOverAll (_2 . _BasicEventBeginElement . _1) tryDropNamespace (errorContext XmlErrorUnexpectedNamespace _1)
  >>= liftError SblgntErrorXml . tryOverAll (_2 . _BasicEventEndElement) tryDropNamespace (errorContext XmlErrorUnexpectedNamespace _1)
  >>= tryOverAll (_2 . _BasicEventBeginElement . _1) toElementAll (errorContext SblgntErrorUnexpectedElementName _1)
  >>= tryOverAll (_2 . _BasicEventEndElement) toElementAll (errorContext SblgntErrorUnexpectedElementName _1)
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
  | SblgntErrorUnexpectedTopLevel (FileReference * BasicEvent ElementAll' X.Content XmlAttributes)
  | SblgntErrorInvalidSblgntAttributes FileReference XmlAttributes
  | SblgntErrorEmptyTopLevel
  | SblgntErrorUnexpectedTopLevels [FileReference * BasicEvent ElementAll' X.Content XmlAttributes]
  deriving (Show)

data ElementAll
  = ElementAllSblgnt
  | ElementAllA
  | ElementAllBook
  | ElementAllLicense
  | ElementAllMarkEnd
  | ElementAllP
  | ElementAllPrefix
  | ElementAllSuffix
  | ElementAllTitle
  | ElementAllVerseNumber
  | ElementAllW
  deriving (Eq, Ord, Show)

toElementAll :: XmlLocalName -> XmlLocalName + ElementAll
toElementAll (XmlLocalName "sblgnt"      ) = Right ElementAllSblgnt
toElementAll (XmlLocalName "a"           ) = Right ElementAllA
toElementAll (XmlLocalName "book"        ) = Right ElementAllBook
toElementAll (XmlLocalName "license"     ) = Right ElementAllLicense
toElementAll (XmlLocalName "mark-end"    ) = Right ElementAllMarkEnd
toElementAll (XmlLocalName "p"           ) = Right ElementAllP
toElementAll (XmlLocalName "prefix"      ) = Right ElementAllPrefix
toElementAll (XmlLocalName "suffix"      ) = Right ElementAllSuffix
toElementAll (XmlLocalName "title"       ) = Right ElementAllTitle
toElementAll (XmlLocalName "verse-number") = Right ElementAllVerseNumber
toElementAll (XmlLocalName "w"           ) = Right ElementAllW
toElementAll t                             = Left t

data ElementAll'
  = ElementAll'A
  | ElementAll'Book
  | ElementAll'License
  | ElementAll'MarkEnd
  | ElementAll'P
  | ElementAll'Prefix
  | ElementAll'Suffix
  | ElementAll'Title
  | ElementAll'VerseNumber
  | ElementAll'W
  deriving (Eq, Ord, Show)

splitElementAll :: ElementAll -> () + ElementAll'
splitElementAll ElementAllSblgnt      = Left ()
splitElementAll ElementAllA           = Right ElementAll'A
splitElementAll ElementAllBook        = Right ElementAll'Book
splitElementAll ElementAllLicense     = Right ElementAll'License
splitElementAll ElementAllMarkEnd     = Right ElementAll'MarkEnd
splitElementAll ElementAllP           = Right ElementAll'P
splitElementAll ElementAllPrefix      = Right ElementAll'Prefix
splitElementAll ElementAllSuffix      = Right ElementAll'Suffix
splitElementAll ElementAllTitle       = Right ElementAll'Title
splitElementAll ElementAllVerseNumber = Right ElementAll'VerseNumber
splitElementAll ElementAllW           = Right ElementAll'W

splitSblgnt
  :: (FileReference * BasicEvent ElementAll X.Content XmlAttributes)
  -> S.TopLevel
    (FileReference * XmlAttributes)
    (FileReference)
    (FileReference * BasicEvent ElementAll' X.Content XmlAttributes)
splitSblgnt (r, BasicEventBeginElement e a) = case splitElementAll e of
  Left _ -> S.TopLevelBegin (r, a)
  Right e' -> S.TopLevelPass (r, BasicEventBeginElement e' a)
splitSblgnt (r, BasicEventEndElement e) = case splitElementAll e of
  Left _ -> S.TopLevelEnd r
  Right e' -> S.TopLevelPass (r, BasicEventEndElement e')
splitSblgnt (r, BasicEventContent c) = S.TopLevelPass (r, BasicEventContent c)
