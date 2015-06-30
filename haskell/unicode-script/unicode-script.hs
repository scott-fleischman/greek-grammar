{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Prelude (($), return, (.), flip, (==), Show(..), not, (&&), Bool(..))
import Control.Applicative (Applicative(..), (<$>))
import Control.Lens (makeLenses, (.~), (&), (^.))
import Data.Foldable (concatMap)
import Data.Functor (fmap)
import Data.List (filter, elem, intersperse)
import Data.Maybe (Maybe(..), maybeToList)
import Data.Map.Strict (fromList, lookup, member)
import Data.Text.Lazy (Text, lines, split, concat)
import Data.Text.Lazy.IO (readFile, writeFile)
import Data.Text.Format (format, Only(..))
import System.IO (IO)
import System.FilePath

rootPath :: FilePath
rootPath = "."

unicodeDataPath :: FilePath
unicodeDataPath = rootPath </> "data" </> "ucd" </> "UnicodeData" <.> "txt"

haskellUnicodeScriptPath :: FilePath
haskellUnicodeScriptPath = rootPath </> "haskell" </> "greek-grammar" </> "src" </> "Text" </> "Greek" </> "Script" </> "UnicodeTokenPairs" <.> "hs"

data TextRecord = TextRecord
  { textRecordCodePoint :: Text
  , textRecordName :: Text
  , textRecordGeneralCategory :: Text
  , textRecordCanonicalCombiningClass :: Text
  , textRecordBidiClass :: Text
  , textRecordDecompositionMapping :: Text
  , textRecordNumericType6 :: Text
  , textRecordNumericType7 :: Text
  , textRecordNumericType8 :: Text
  , textRecordBidiMirrored :: Text
  , textRecordUnicode1NameObsolete :: Text
  , textRecordIsoCommentObsolete :: Text
  , textRecordSimpleUppercaseMapping :: Text
  , textRecordSimpleLowercaseMapping :: Text
  , textRecordSimpleTitlecaseMapping :: Text
  }

letterNames :: [(Text, Text)]
letterNames =
  [ ("ALPHA", "Alpha")
  , ("BETA", "Beta")
  , ("GAMMA", "Gamma")
  , ("DELTA", "Delta")
  , ("EPSILON", "Epsilon")
  , ("ZETA", "Zeta")
  , ("ETA", "Eta")
  , ("DIGAMMA", "Digamma")
  , ("THETA", "Theta")
  , ("IOTA", "Iota")
  , ("KAPPA", "Kappa")
  , ("LAMDA", "Lambda")
  , ("MU", "Mu")
  , ("NU", "Nu")
  , ("XI", "Xi")
  , ("OMICRON", "Omicron")
  , ("PI", "Pi")
  , ("RHO", "Rho")
  , ("SIGMA", "Sigma")
  , ("TAU", "Tau")
  , ("UPSILON", "Upsilon")
  , ("PHI", "Phi")
  , ("CHI", "Chi")
  , ("PSI", "Psi")
  , ("OMEGA", "Omega")
  ]

haskellUppercaseName :: Text
haskellUppercaseName = "Uppercase"

letterCaseNames :: [(Text, Text)]
letterCaseNames =
  [ ("CAPITAL", haskellUppercaseName)
  , ("SMALL", "Lowercase")
  ]

greekName :: Text
greekName = "GREEK"

letterName :: Text
letterName = "LETTER"

ignoreNameParts :: [Text]
ignoreNameParts = ["WITH", "AND"]

iotaSubscriptNames :: [(Text, Text)]
iotaSubscriptNames =
  [ ("YPOGEGRAMMENI", "IotaSubscript")
  , ("PROSGEGRAMMENI", "IotaSubscript")
  ]

diaeresisNames :: [(Text, Text)]
diaeresisNames = [("DIALYTIKA", "Diaeresis")]

finalFormNames :: [(Text, Text)]
finalFormNames = [("FINAL", "FinalForm")]

accentNames :: [(Text, Text)]
accentNames =
  [ ("TONOS", "Acute")
  , ("OXIA", "Acute")
  , ("VARIA", "Grave")
  , ("PERISPOMENI", "Circumflex")
  ]

breathingNames :: [(Text, Text)]
breathingNames =
  [ ("PSILI", "Smooth")
  , ("DASIA", "Rough")
  ]

data TextToken = TextToken
  { _letter :: Text
  , _letterCase :: Text
  , _accent :: Maybe Text
  , _breathing :: Maybe Text
  , _iotaSubscript :: Maybe Text
  , _diaeresis :: Maybe Text
  , _finalForm :: Maybe Text
  }
  deriving (Show)
makeLenses ''TextToken

maybeToHaskell :: Maybe Text -> Text
maybeToHaskell Nothing = "Nothing"
maybeToHaskell (Just t) = format "(Just {})" (Only t)

tokenToHaskell :: TextToken -> Text
tokenToHaskell t = format "Token {} {} {} {} {} {} {}" (t ^. letter, t ^. letterCase, haskellAccent, haskellBreathing, haskellIotaSubscript, haskellDiaeresis, haskellFinalForm)
  where
    haskellAccent = maybeToHaskell $ t ^. accent
    haskellBreathing = maybeToHaskell $ t ^. breathing
    haskellIotaSubscript = maybeToHaskell $ t ^. iotaSubscript
    haskellDiaeresis = maybeToHaskell $ t ^. diaeresis
    haskellFinalForm = maybeToHaskell $ t ^. finalForm

makePlainTextToken :: Text -> Text -> TextToken
makePlainTextToken el c = TextToken el c Nothing Nothing Nothing Nothing Nothing

makeFinalFormTextToken :: Text -> Text -> Text -> TextToken
makeFinalFormTextToken el c f = TextToken el c Nothing Nothing Nothing Nothing (Just f)

applyNameToToken :: Text -> TextToken -> TextToken
applyNameToToken n t
  | n `member` accentMap = t & accent .~ lookup n accentMap
  | n `member` breathingMap = t & breathing .~ lookup n breathingMap
  | n `member` iotaSubscriptMap = t & iotaSubscript .~ lookup n iotaSubscriptMap
  | n `member` diaeresisMap = t & diaeresis .~ lookup n diaeresisMap
  | True = t
  where
    accentMap = fromList accentNames
    breathingMap = fromList breathingNames
    iotaSubscriptMap = fromList iotaSubscriptNames
    diaeresisMap = fromList diaeresisNames

updateTokenAccents :: [Text] -> TextToken -> TextToken
updateTokenAccents [] t = t
updateTokenAccents (n : ns) t = updateTokenAccents ns (applyNameToToken n t)

namesToToken :: [Text] -> Maybe TextToken
namesToToken ns = case validNameParts ns of
  gc : lc : lnc : fln : ln : ns'
    | gc == greekName && lnc == letterName && fln `member` finalFormMap ->
      (makeFinalFormTextToken <$> letterLookup ln  <*> letterCaseLookup lc <*> finalFormLookup fln) `addAccents` ns'
    | gc == greekName && lnc == letterName ->
      (makePlainTextToken     <$> letterLookup fln <*> letterCaseLookup lc) `addAccents` (ln : ns')
    | True -> Nothing
  gc : lc : lnc : ln : ns'
    | gc == greekName && lnc == letterName ->
      (makePlainTextToken     <$> letterLookup ln  <*> letterCaseLookup lc) `addAccents` ns'
    | True -> Nothing
  gc : lnc : ln : ns'
    | gc == greekName && lnc == letterName ->
      (makePlainTextToken     <$> letterLookup ln  <*> pure haskellUppercaseName) `addAccents` ns'
    | True -> Nothing
  _ -> Nothing
  where
    validNameParts = filter (not . (`elem` ignoreNameParts))

    finalFormMap = fromList finalFormNames
    finalFormLookup = flip lookup finalFormMap

    letterLookup = makeLookup letterNames
    letterCaseLookup = makeLookup letterCaseNames

    makeLookup = flip lookup . fromList

    addAccents :: Maybe TextToken -> [Text] -> Maybe TextToken
    addAccents mt ns'' = (updateTokenAccents ns'') <$> mt

toSplitName :: Text -> [Text]
toSplitName = split (== ' ')

toSplitTextRecord :: [Text] -> Maybe TextRecord
toSplitTextRecord (f0 : f1 : f2 : f3 : f4 : f5 : f6 : f7 : f8 : f9 : f10 : f11 : f12 : f13 : f14 : []) = Just $ TextRecord f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14
toSplitTextRecord _ = Nothing

toTextRecord :: Text -> Maybe TextRecord
toTextRecord = toSplitTextRecord . split (== ';')

toTextRecords :: Text -> [TextRecord]
toTextRecords = concatMap (maybeToList . toTextRecord) . lines

toCodePointTextToken :: TextRecord -> Maybe (Text, TextToken)
toCodePointTextToken tr = (,) <$> pure (textRecordCodePoint tr) <*> (namesToToken . toSplitName . textRecordName $ tr)

toTextTokens :: [TextRecord] -> [(Text, TextToken)]
toTextTokens = concatMap (maybeToList . toCodePointTextToken)

toHaskellToken :: (Text, TextToken) -> Text
toHaskellToken (cp, tt) = format "('\\x{}', {})" (cp, tokenToHaskell tt)

toHaskellScript :: Text -> Text
toHaskellScript c = format "-- generated by unicode-script.hs\n\
\module Text.Greek.Script.UnicodeTokenPairs where\n\
\\n\
\import Text.Greek.Script.Token\n\
\\n\
\unicodeTokenPairs :: [(Char, Token)]\n\
\unicodeTokenPairs =\n\
\  [ {}\n\
\  ]\n\
\" (Only tokenText)
  where tokenText = concat . intersperse "\n  , " . fmap toHaskellToken . toTextTokens . toTextRecords $ c

main :: IO ()
main = do
  content <- readFile unicodeDataPath
  writeFile haskellUnicodeScriptPath (toHaskellScript content)
  return ()
