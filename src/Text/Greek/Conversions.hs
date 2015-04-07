module Text.Greek.Conversions where

import Prelude hiding (lookup)
import Data.Maybe (maybeToList)
import Data.Text (Text, unpack)
import Data.Map.Strict (lookup, fromList)
import Text.Greek.Corpus.Bible
import Text.Greek.Script.Sound
import Text.Greek.Script.Token
import Text.Greek.Script.Unicode

textToSounds :: Text -> [Sound ()]
textToSounds t = tokensToSounds $ textToTokenContexts t

textToTokenContexts :: Text -> [TokenContext ()]
textToTokenContexts t = fmap emptyTokenContext . textToTokens $ t

emptyTokenContext :: Token -> TokenContext ()
emptyTokenContext t = TokenContext t ()

textToTokens :: Text -> [Token]
textToTokens t = concatMap (maybeToList . lookupChar) . unpack $ t
  where lookupChar = flip lookup (fromList unicodeTokenPairs)

getBookTokens :: Book -> [TokenContext Character]
getBookTokens = snd . charactersToTokenContexts . wordsToCharacters . segmentsToWords . segments
