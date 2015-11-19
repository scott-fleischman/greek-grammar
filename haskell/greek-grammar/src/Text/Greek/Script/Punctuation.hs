module Text.Greek.Script.Punctuation where

import qualified Data.Set as Set

greekQuestionMark :: Char
greekQuestionMark = ';'

period :: Char
period = '.'

endOfSentenceMarks :: Set.Set Char
endOfSentenceMarks = Set.fromList [greekQuestionMark, period]

data EndOfSentence = IsEndOfSentence | NotEndOfSentence deriving (Eq, Ord, Show)
newtype EndOfSentenceChar = EndOfSentenceChar { getEndOfSentenceChar :: Char } deriving (Eq, Ord, Show)

type SentencePair = (EndOfSentence, Maybe EndOfSentenceChar)

charToEndOfSentence :: Char -> EndOfSentence
charToEndOfSentence c | flip Set.member endOfSentenceMarks c = IsEndOfSentence
charToEndOfSentence _ = NotEndOfSentence

tryGetSentencePair :: [Char] -> Maybe SentencePair
tryGetSentencePair xs = go values
  where
    go [] = Just (NotEndOfSentence, Nothing)
    go [(e@IsEndOfSentence, c)] = Just (e, Just (EndOfSentenceChar c))
    go _ = Nothing
    values = filter (\(e, _) -> e == IsEndOfSentence) . fmap (\x -> (charToEndOfSentence x, x)) $ xs
