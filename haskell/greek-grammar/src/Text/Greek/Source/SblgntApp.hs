{-# LANGUAGE OverloadedStrings #-}

module Text.Greek.Source.SblgntApp where

import Data.Text (Text)
import Text.Greek.Xml.Parse
import Text.Parsec.Combinator
import Text.Parsec.Prim
import qualified Data.Text as T

data Book = Book
  { bookTitle :: Text
  , bookVerses :: [Verse]
  } deriving Show

data Verse = Verse
  { verseName :: Text
  , verseVariants :: [Variant]
  } deriving Show

data Variant = Variant
  { variantReading :: MarkedReading
  , variantContent :: Text
  } deriving Show

-- symbols
-- • Separates multiple variation units within a verse.
-- ] Separates the reading of the text (and its support) from variant readings.
-- ; Separates multiple variants within a single variation unit.
-- + The following text is added by the listed witness(es).
-- – The indicated text is omitted by the listed witness(es).
-- ⟦ ⟧ Used by Westcott and Hort to mark material that they did not think belonged to the genuine text... When placed around their initials in the apparatus (i.e., ⟦WH⟧), double brackets signal that WH placed them around the text or variant reading in question.
-- … Replaces identical text shared by all the variants in a particular variation unit.
-- em emendation

omittedSymbol :: Text
omittedSymbol = "–" -- en dash

abbreviations :: [(Text, Text)]
abbreviations =
  [ ("WH", "Brooke Foss Westcott and Fenton John Anthony Hort, The New Testament in the Original Greek, vol. 1: Text; vol. 2: Introduction [and] Appendix (Cambridge: Macmillan, 1881).")
  , ("Treg", "Samuel Prideaux Tregelles, The Greek New Testament, Edited from Ancient Authorities, with their Various Readings in Full, and the Latin Version of Jerome (London: Bagster; Stewart, 1857–1879).")
  , ("NIV", "Richard J. Goodrich and Albert L. Lukaszewski, A Reader’s Greek New Testament (Grand Rapids: Zondervan, 2003).")
  , ("RP", "The New Testament in the Original Greek: Byzantine Textform 2005, compiled and arranged by Maurice A. Robinson and William G. Pierpont (Southborough, Mass.: Chilton, 2005).")
  , ("ECM", "Novum Testamentum Graecum: Editio Critica Maior, ed. The Institute for New Testament Textual Research, vol. 4: Catholic Letters, ed. Barbara Aland, Kurt Aland, Gerd Mink, Holger Strutwolf, and Klaus Wachtel (4 installments; Stuttgart: Deutsche Biblegesellschaft, 1997–2005): inst. 1: James (1997; 2nd rev. impr., 1998); inst. 2: The Letters of Peter (2000); inst. 3: The First Letter of John (2003); inst. 4: The Second and Third Letter of John, The Letter of Jude (2005).")
  , ("Greeven", "Heinrich Greeven in Albert Huck, Synopse der drei ersten Evangelien/Synopsis of the First Three Gospels (13th ed. fundamentally revised by Heinrich Greeven; Tübingen: Mohr Siebeck), 1981).")
  , ("Holmes", "Apparatus for the Greek New Testament: SBL Edition")
  , ("NA", "NA 26–27/UBS 3–4")
  , ("TR", "Textus Receptus.")
  , ("Tregmarg", "Tregelles in the margin of his edition.")
  , ("WHapp", "WH in the Appendix")
  , ("WHmarg", "WH in the margin of their edition.")
  ]

verseNumberParser :: EventParser Text
verseNumberParser = element "verse-number" (simpleAttributeParser "id") contentParser const

boldParser :: EventParser Text
boldParser = elementContent "b"

omittedParser :: EventParser Text
omittedParser = elementSimple "p" (contentValueParser omittedSymbol)

data MarkedReading
  = MarkedReadingAll Text
  | MarkedReadingRange { markedReadingRangeStart :: Text, markedReadingRangeEnd :: Text }
  deriving Show

markedReadingParser :: EventParser MarkedReading
markedReadingParser = fmap (MarkedReadingAll . T.concat) (many1 boldParser)

variantContentParser :: EventParser Text
variantContentParser = try omittedVariantContentParser <|> simpleVariantContentParser

omittedVariantContentParser :: EventParser Text
omittedVariantContentParser = do
  text1 <- contentParser
  text2 <- omittedParser
  text3 <- contentParser
  return $ T.concat [text1, text2, text3]

simpleVariantContentParser :: EventParser Text
simpleVariantContentParser = contentParser

variantParser :: EventParser Variant
variantParser = do
  reading <- markedReadingParser
  content <- variantContentParser
  return $ Variant reading content

verseParser :: EventParser Verse
verseParser = elementSimple "p" $ do
  verse <- verseNumberParser
  variants <- many1 variantParser
  return $ Verse verse variants

bookParser :: EventParser Book
bookParser = elementSimple "book" $ do
  title <- elementContent "title"
  verses <- many1 verseParser
  return $ Book title verses

sblgntAppParser :: EventParser [Book]
sblgntAppParser = elementSimple "sblgntapp" (many1 bookParser)
