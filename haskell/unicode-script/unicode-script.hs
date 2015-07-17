{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Prelude hiding (lookup, readFile, writeFile, lines, concat, null, words)
import Control.Lens (makeLenses, (.~), (&), (^.))
import Control.Monad (join)
import Data.List (intersperse, sortBy)
import Data.Maybe (maybeToList, catMaybes)
import Data.Map.Strict (Map, fromList, lookup, member, assocs)
import Data.Text.Lazy (Text, lines, split, concat, unpack, append, null, isPrefixOf, words)
import Data.Text.Lazy.IO (readFile, writeFile)
import Data.Text.Format (format, Only(..))
import Numeric (readHex)
import System.FilePath
import Language.Haskell.TH

rootPath :: FilePath
rootPath = "."

unicodeDataPath :: FilePath
unicodeDataPath = rootPath </> "data" </> "ucd" </> "UnicodeData" <.> "txt"

haskellUnicodeScriptPath :: FilePath
haskellUnicodeScriptPath = rootPath </> "haskell" </> "greek-grammar" </> "src" </> "Text" </> "Greek" </> "Script" </> "UnicodeTokenPairs" <.> "hs"

haskellDecomposeCharPath :: FilePath
haskellDecomposeCharPath = rootPath </> "haskell" </> "greek-grammar" </> "src" </> "Text" </> "Greek" </> "Script" </> "DecomposeChar" <.> "hs"

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
  [ ("ALPHA", "L_α")
  , ("BETA", "L_β")
  , ("GAMMA", "L_γ")
  , ("DELTA", "L_δ")
  , ("EPSILON", "L_ε")
  , ("ZETA", "L_ζ")
  , ("ETA", "L_η")
  , ("THETA", "L_θ")
  , ("IOTA", "L_ι")
  , ("KAPPA", "L_κ")
  , ("LAMDA", "L_λ")
  , ("MU", "L_μ")
  , ("NU", "L_ν")
  , ("XI", "L_ξ")
  , ("OMICRON", "L_ο")
  , ("PI", "L_π")
  , ("RHO", "L_ρ")
  , ("SIGMA", "L_σ")
  , ("TAU", "L_τ")
  , ("UPSILON", "L_υ")
  , ("PHI", "L_φ")
  , ("CHI", "L_χ")
  , ("PSI", "L_ψ")
  , ("OMEGA", "L_ω")
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

applyNameToToken :: Text -> TextToken -> Maybe TextToken
applyNameToToken n t
  | n `member` accentMap = Just $ t & accent .~ lookup n accentMap
  | n `member` breathingMap = Just $ t & breathing .~ lookup n breathingMap
  | n `member` iotaSubscriptMap = Just $ t & iotaSubscript .~ lookup n iotaSubscriptMap
  | n `member` diaeresisMap = Just $ t & diaeresis .~ lookup n diaeresisMap
  | True = Nothing
  where
    accentMap = fromList accentNames
    breathingMap = fromList breathingNames
    iotaSubscriptMap = fromList iotaSubscriptNames
    diaeresisMap = fromList diaeresisNames

updateTokenAccents :: [Text] -> TextToken -> Maybe TextToken
updateTokenAccents [] t = Just t
updateTokenAccents (n : ns) t = (applyNameToToken n t) >>= (updateTokenAccents ns)

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
    addAccents mt ns'' = join ((updateTokenAccents ns'') <$> mt)

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

outputTokenPairs :: IO ()
outputTokenPairs = do
  content <- readFile unicodeDataPath
  writeFile haskellUnicodeScriptPath (toHaskellScript content)
  return ()

codePointDec :: [TextRecord] -> Dec
codePointDec rs = DataD [] (mkName "CodePoint") [] cs []
  where
    cs = fmap (mkC . textRecordCodePoint) rs
    mkC n = NormalC (mkN n) []
    mkN t = mkName . unpack . append "U_" $ t

showCodePoints :: IO ()
showCodePoints = do
  content <- readFile unicodeDataPath
  let records = toTextRecords content
  let codePoint = codePointDec records
  putStrLn . pprint $ codePoint

isCanonicalMapping :: Text -> Bool
isCanonicalMapping x = not $ null x || isPrefixOf "<" x

data DecomposeSingleStep
  = Id Text
  | SingleStep Text Text

makeDecomposeSingleStep :: [Text] -> Maybe DecomposeSingleStep
makeDecomposeSingleStep [x]    = Just $ Id x
makeDecomposeSingleStep [x, y] = Just $ SingleStep x y
makeDecomposeSingleStep _      = Nothing

makeDecompositionMap :: Text -> Map Text DecomposeSingleStep
makeDecompositionMap content = fromList singleStepDecompositionPairs
  where
    records = toTextRecords content
    nonCanonicalRecords = filter (isCanonicalMapping . textRecordDecompositionMapping) records
    singleStepDecompositionMaybePairs = fmap (\x -> (textRecordCodePoint x, makeDecomposeSingleStep . words . textRecordDecompositionMapping $ x)) nonCanonicalRecords
    singleStepDecompositionPairs = catMaybes $ fmap (\(x, y) -> case y of { Just y' -> Just (x, y'); Nothing -> Nothing }) singleStepDecompositionMaybePairs

transitiveDecomposition :: Map Text DecomposeSingleStep -> DecomposeSingleStep -> [Text]
transitiveDecomposition _ (Id x) = [x]
transitiveDecomposition singleStepMap (SingleStep x y)
  | Just s <- lookup x singleStepMap = transitiveDecomposition singleStepMap s ++ [y]
  | otherwise = [x, y]

transitiveDecompositionPairs :: Map Text DecomposeSingleStep -> [(Text, [Text])]
transitiveDecompositionPairs singleStepMap = sortedPairs
  where
    readTextHex :: Text -> Maybe Int
    readTextHex x
      | ((x', _) : _) <- readHex . unpack $ x = Just x'
      | otherwise = Nothing
    hexPairSort x y
      | Just x' <- readTextHex x
      , Just y' <- readTextHex y
      = compare x' y'
      | otherwise = compare x y
    sortedPairs = sortBy (\(x, _) (x', _) -> hexPairSort x x') transitivePairs
    transitivePairs = fmap (\(x, y) -> (x, transitiveDecomposition singleStepMap y)) $ assocs singleStepMap

toHaskellDecompose :: Text -> Text
toHaskellDecompose content = format "-- generated by unicode-script.hs\n\
\module Text.Greek.Script.DecomposeChar where\n\
\\n\
\decomposeChar :: Char -> String\n\
\{}\
\decomposeChar c = [c]\n\
\" (Only patternMatches)
  where
    formatCodePoint x = format "\\x{}" (Only x)
    formatPair (x, ys) = format "decomposeChar '{}' = \"{}\"\n" (formatCodePoint x, concat . fmap formatCodePoint $ ys)
    patternMatches = concat . fmap formatPair $ pairs
    pairs = transitiveDecompositionPairs . makeDecompositionMap $ content

decompositionMain :: IO ()
decompositionMain = do
  content <- readFile unicodeDataPath
  writeFile haskellDecomposeCharPath (toHaskellDecompose content)
  return ()

main :: IO ()
main = decompositionMain
