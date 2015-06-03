module Text.Greek.Mounce.Parse where

import Data.Text (Text, pack)
import Text.ParserCombinators.Parsec
import Text.Greek.Conversions
import Text.Greek.Mounce.Morphology
import Text.Greek.Mounce.Phonology
import Text.Greek.Script.Sound
import Text.Greek.Script.UnicodeTokenPairs

greekCharacter :: CharParser () Char
greekCharacter = oneOf $ greekDasia : map fst unicodeTokenPairs
  where greekDasia = '\x1FFE'

greekWord :: CharParser () Text
greekWord = pack <$> many1 greekCharacter

euphonyRule :: CharParser () EuphonyRule
euphonyRule = EuphonyRule
  <$> (greekWord <* spaces <* char '+' <* spaces)
  <*> (greekWord <* spaces <* char '}' <* spaces)
  <*> (greekWord <* spaces)

euphonyRules :: CharParser () [EuphonyRule]
euphonyRules = sepBy1 euphonyRule spaces

greekWordsParser :: CharParser () [Text]
greekWordsParser = endBy1 greekWord spaces

greekWordSounds :: CharParser () [Sound]
greekWordSounds = do
  w <- greekWord
  case textToSounds w of
    Left (InvalidChar c) -> fail ("InvalidChar " ++ c : [])
    Right x -> return x

caseEnding :: CharParser () Affix
caseEnding = pure EmptyAffix <* string "-"
  <|> pure UnattestedAffix <* string "*"
  <|> AttestedAffix <$> greekWordSounds

nounCaseEndingsParser :: CharParser () (NounForms Affix)
nounCaseEndingsParser = NounForms <$> e <*> e <*> e <*> e <*> e <*> e <*> e <*> e <*> e <*> e
  where e = caseEnding <* spaces

nounCategoryParser :: CharParser () NounCategory
nounCategoryParser = NounCategory
  <$> (spaces *> (pack <$> (many1 (noneOf "\n\r") <* spaces)))
  <*> (spaces *> labeledNounCaseEndingsParser <* spaces)
  <*> (spaces *> string "lemmas:" *> spaces *> greekWordsParser)

labeledNounCaseEndingsParser :: CharParser () (NounForms Affix)
labeledNounCaseEndingsParser =
  string "sg:" *> spaces *> string "pl:" *> spaces *>
  (NounForms
    <$> le "nom:" <*> e
    <*> le "gen:" <*> e
    <*> le "dat:" <*> e
    <*> le "acc:" <*> e
    <*> le "voc:" <*> e)
      where
        le x = string x *> spaces *> caseEnding <* spaces
        e = caseEnding <* spaces

topLevel :: CharParser () a -> CharParser () a
topLevel x = spaces *> x <* eof
