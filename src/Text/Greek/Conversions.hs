module Text.Greek.Conversions where

import Prelude hiding (lookup)
import Control.Lens ((^.))
import Data.Text (Text, unpack)
import Data.Map.Strict (lookup, fromList)
import Text.Greek.Corpus.Bible
import Text.Greek.Script.Sound
import Text.Greek.Script.Token
import Text.Greek.Script.UnicodeTokenPairs

data TokenError = InvalidChar Char
  deriving (Show)

textToSounds :: Text -> Either TokenError [Sound]
textToSounds t = fmap (fmap (^. sound) . tokensToSounds) $ textToTokenContexts t

textToTokenContexts :: Text -> Either TokenError [TokenContext ()]
textToTokenContexts t = fmap emptyTokenContext <$> textToTokens t

emptyTokenContext :: Token -> TokenContext ()
emptyTokenContext t = TokenContext t ()

textToTokens :: Text -> Either TokenError [Token]
textToTokens t = traverse charToEither (unpack t)
  where
    charToEither :: Char -> Either TokenError Token
    charToEither c = case charToTokenPair c of
      Just x -> Right x
      Nothing -> Left $ InvalidChar c

    charToTokenPair :: Char -> Maybe Token
    charToTokenPair = flip lookup (fromList unicodeTokenPairs)

getBookTokens :: Book -> [TokenContext Character]
getBookTokens = snd . charactersToTokenContexts . wordsToCharacters . segmentsToWords . segments
