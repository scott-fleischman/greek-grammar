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
  >>. fmap (view _3)
  >>= single SblgntErrorEmptyTopLevel SblgntErrorUnexpectedTopLevels

data SblgntError
  = SblgntErrorXmlInternal XmlInternalError
  | SblgntErrorXml (XmlError FileReference)
  | SblgntErrorUnexpectedElementName FileReference XmlLocalName
  | SblgntErrorSblgntSublist (S.Error (FileReference * XmlAttributes) FileReference)
  | SblgntErrorUnexpectedTopLevel (FileReference * BasicEvent ElementAll' X.Content XmlAttributes)
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
splitElementAll ElementAllSblgnt = Left ()
splitElementAll ElementAllA = Right ElementAll'A
splitElementAll ElementAllBook = Right ElementAll'Book
splitElementAll ElementAllLicense = Right ElementAll'License
splitElementAll ElementAllMarkEnd = Right ElementAll'MarkEnd
splitElementAll ElementAllP = Right ElementAll'P
splitElementAll ElementAllPrefix = Right ElementAll'Prefix
splitElementAll ElementAllSuffix = Right ElementAll'Suffix
splitElementAll ElementAllTitle = Right ElementAll'Title
splitElementAll ElementAllVerseNumber = Right ElementAll'VerseNumber
splitElementAll ElementAllW = Right ElementAll'W

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



{-
type FinalXmlEvent
  = XmlBeginElement * ElementAll * XmlAttributes
  + XmlEndElement * ElementAll
  + XmlContent

newtype AElement = AElement () deriving (Eq, Ord, Show)
newtype BookElement = BookElement () deriving (Eq, Ord, Show)
newtype LicenseElement = LicenseElement () deriving (Eq, Ord, Show)
newtype MarkEndElement = MarkEndElement () deriving (Eq, Ord, Show)
newtype PElement = PElement () deriving (Eq, Ord, Show)
newtype PrefixElement = PrefixElement () deriving (Eq, Ord, Show)
newtype SblgntElement = SblgntElement () deriving (Eq, Ord, Show)
newtype SuffixElement = SuffixElement () deriving (Eq, Ord, Show)
newtype TitleElement = TitleElement () deriving (Eq, Ord, Show)
newtype VerseNumberElement = VerseNumberElement () deriving (Eq, Ord, Show)
newtype WElement = WElement () deriving (Eq, Ord, Show)

type ElementAll
  = SblgntElement
  + AElement
  + BookElement
  + LicenseElement
  + MarkEndElement
  + PElement
  + PrefixElement
  + SuffixElement
  + TitleElement
  + VerseNumberElement
  + WElement

toElementAll :: XmlLocalName -> XmlLocalName + ElementAll
toElementAll (XmlLocalName "sblgnt"      ) = Right . sum1   $ SblgntElement ()
toElementAll (XmlLocalName "a"           ) = Right . sum2   $ AElement ()
toElementAll (XmlLocalName "book"        ) = Right . sum3   $ BookElement ()
toElementAll (XmlLocalName "license"     ) = Right . sum4   $ LicenseElement ()
toElementAll (XmlLocalName "mark-end"    ) = Right . sum5   $ MarkEndElement ()
toElementAll (XmlLocalName "p"           ) = Right . sum6   $ PElement ()
toElementAll (XmlLocalName "prefix"      ) = Right . sum7   $ PrefixElement ()
toElementAll (XmlLocalName "suffix"      ) = Right . sum8   $ SuffixElement ()
toElementAll (XmlLocalName "title"       ) = Right . sum9   $ TitleElement ()
toElementAll (XmlLocalName "verse-number") = Right . sum10  $ VerseNumberElement ()
toElementAll (XmlLocalName "w"           ) = Right . sum11e $ WElement ()
toElementAll t                             = Left t



readSblgntEvents :: X.FilePath -> IO ([ErrorMessage] + [Sblgnt * [FileReference * FinalXmlEvent]])
readSblgntEvents = fmap (>>= sblgntTransform) . readEvents

sblgntTransform
  :: [FileReference * XmlEventAll]
  -> [ErrorMessage] + [Sblgnt * [FileReference * FinalXmlEvent]]
sblgntTransform x = return x
  >>. trimContent
  >>= removeUnusedXmlEvents
  >>= removeBeginElementNamespace
  >>= removeEndElementNamespace
  >>= useBeginElementType
  >>= useEndElementType
  >>= extractSblgnt
  >>= topLevelSblgnt

type XmlUnused
  = XmlCDATA
  + XmlBeginDoctype * XmlDoctypeName * (None + XmlExternalId)
  + XmlEndDoctype
  + XmlInstruction
  + XmlComment

removeUnusedXmlEvents
  :: Handler e (a * (b + c + d + XmlUnused))
  =>           [a * (b + c + d + XmlUnused)]
  -> [e] +     [a * (b + c + d            )]
removeUnusedXmlEvents = handleMap lens2e tryDrop4e

removeBeginElementNamespace
  :: Handler e (a * (XmlBeginElement * (XmlLocalName * (None + XmlNamespace) * (None + XmlNamePrefix)) * y + b2))
  =>           [a * (XmlBeginElement * (XmlLocalName * (None + XmlNamespace) * (None + XmlNamePrefix)) * y + b2)]
  -> [e] +     [a * (XmlBeginElement *  XmlLocalName                                                   * y + b2)]
removeBeginElementNamespace = handleMap (lens2e . prism1 . lens2) removeNamespace

removeEndElementNamespace
  :: Handler e (a * (b1 + XmlEndElement * XmlLocalName * (None + XmlNamespace) * (None + XmlNamePrefix) + b2))
  =>           [a * (b1 + XmlEndElement * XmlLocalName * (None + XmlNamespace) * (None + XmlNamePrefix) + b2)]
  -> [e] +     [a * (b1 + XmlEndElement * XmlLocalName                                                  + b2)]
removeEndElementNamespace = handleMap (lens2e . prism2 . lens2e) removeNamespace

useBeginElementType
  :: Handler e (a * (XmlBeginElement * XmlLocalName * as + b2))
  =>           [a * (XmlBeginElement * XmlLocalName * as + b2)]
  -> [e] +     [a * (XmlBeginElement * ElementAll   * as + b2)]
useBeginElementType = handleMap' (lens2e . prism1 . lens2) toElementAll

useEndElementType
  :: Handler e (a * (b1 + XmlEndElement * XmlLocalName + b3))
  =>           [a * (b1 + XmlEndElement * XmlLocalName + b3)]
  -> [e] +     [a * (b1 + XmlEndElement * ElementAll   + b3)]
useEndElementType = handleMap' (lens2e . prism2 . lens2e) toElementAll


newtype Sblgnt = Sblgnt () deriving (Eq, Ord, Show)

extractSblgnt
  :: Handler e (SublistError [a * FinalXmlEvent])
  =>       [a * FinalXmlEvent]
  -> [e] + [a * FinalXmlEvent + Sblgnt * [a * FinalXmlEvent]]
extractSblgnt = handleSublist (buildSublist (^? lens2e . prism1 . lens2 . prism1) (^? lens2e . prism2 . lens2e . prism1) (Sblgnt ()))

topLevelSblgnt
  :: Handler e (a * FinalXmlEvent + Sblgnt * [a * FinalXmlEvent])
  =>           [a * FinalXmlEvent + Sblgnt * [a * FinalXmlEvent]]
  -> [e] +     [                    Sblgnt * [a * FinalXmlEvent]]
topLevelSblgnt = handleMap id tryDrop1


-}
