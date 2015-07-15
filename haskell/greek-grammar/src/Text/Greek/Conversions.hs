module Text.Greek.Conversions where

import Prelude hiding (lookup)
import Control.Lens ((^.))
import Data.Text (Text, unpack)
import Data.Tuple (swap)
import Data.Map.Strict (lookup, fromList)
import Text.Greek.Corpus.Bible
import Text.Greek.Script.Sound
import Text.Greek.Script.Token
import Text.Greek.Script.UnicodeTokenPairs

data TokenError = InvalidChar Char
  deriving (Show)

textToSounds :: Text -> Either TokenError [Sound]
textToSounds t = tokensToSounds <$> textToTokens t

textToTokens :: Text -> Either TokenError [Token]
textToTokens t = traverse charToEither (unpack t)
  where
    charToEither :: Char -> Either TokenError Token
    charToEither c = case charToTokenPair c of
      Just x -> Right x
      Nothing -> Left $ InvalidChar c

    charToTokenPair :: Char -> Maybe Token
    charToTokenPair = flip lookup (fromList unicodeTokenPairs)

getBookTokens :: Book -> [Token]
getBookTokens = snd . charactersToTokenContexts . wordsToCharacters . segmentsToWords . segments

tokensToString :: [Token] -> String
tokensToString = fmap tokenToChar
  where
    tokenToChar t = case tokenToMaybeChar t of
      Just x -> x
      Nothing -> case tokenToMaybeChar (Token (t ^. letter) (t ^. letterCase) Nothing Nothing Nothing Nothing Nothing) of
        Just x -> x
        Nothing -> '_'
    tokenToMaybeChar = flip lookup (fromList . fmap swap $ unicodeTokenPairs)

soundsToString :: [Sound] -> String
soundsToString ss = tokensToString . concat . fmap soundToTokens $ ss
