module Text.Greek.Mounce.Parse where

import Data.List (intersperse)
import Data.Text (Text, pack, unpack)
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

nounLemmaParser :: CharParser () NounLemma
nounLemmaParser = do
  w <- greekWord
  case textToSounds w of
    Left (InvalidChar c) -> fail ("InvalidChar " ++ c : [])
    Right x -> return $ NounLemma w x

greekWordParser :: CharParser () [Sound]
greekWordParser = _nounLemmaSounds <$> nounLemmaParser

nounLemmasParser :: CharParser () [NounLemma]
nounLemmasParser = endBy1 nounLemmaParser spaces

caseEndingParser :: CharParser () Affix
caseEndingParser = pure (AttestedAffix []) <* string "-"
  <|> pure UnattestedAffix <* string "*"
  <|> AttestedAffix <$> greekWordParser

nounFormsParser :: CharParser () (NounForms Affix)
nounFormsParser =
  string "sg:" *> spaces *> string "pl:" *> spaces *>
  (NounForms
    <$> le "nom:" <*> e
    <*> le "gen:" <*> e
    <*> le "dat:" <*> e
    <*> le "acc:" <*> e
    <*> le "voc:" <*> e)
      where
        le x = string x *> spaces *> caseEndingParser <* spaces
        e = caseEndingParser <* spaces

nounCategoryParser :: CharParser () NounCategory
nounCategoryParser = NounCategory
  <$> (spaces *> (pack <$> (many1 (noneOf "\n\r") <* spaces)))
  <*> (spaces *> nounFormsParser <* spaces)
  <*> (spaces *> string "lemmas:" *> spaces *> nounLemmasParser)

adjective3FormsParser :: CharParser () (Adjective3Forms Affix)
adjective3FormsParser =
  string "m:" *> spaces *> string "f:" *> spaces *> string "n:" *> spaces *>
  (Adjective3Forms
    <$> le "nom sg:" <*> e <*> e
    <*> le "gen sg:" <*> e <*> e
    <*> le "dat sg:" <*> e <*> e
    <*> le "acc sg:" <*> e <*> e
    <*> le "voc sg:" <*> e <*> e
    <*> le "nom pl:" <*> e <*> e
    <*> le "gen pl:" <*> e <*> e
    <*> le "dat pl:" <*> e <*> e
    <*> le "acc pl:" <*> e <*> e
    <*> le "voc pl:" <*> e <*> e)
      where
        le x = string x *> spaces *> caseEndingParser <* spaces
        e = caseEndingParser <* spaces

validNounCategoryParser :: CharParser () NounCategory
validNounCategoryParser = do
  nc <- nounCategoryParser
  let ms = getMismatches nc
  case ms of
    [] -> return nc
    (_ : _) -> fail $ "Lemmas do not match nom sg case ending "
      ++ (affixToString . nomSg . _nounCategoryEndings $ nc)
      ++ " for " ++ (unpack . _nounCategoryName $ nc) ++ ":\n"
      ++ (concat . intersperse "\n" . fmap unpack . fmap _nounLemmaText $ ms)

topLevel :: CharParser () a -> CharParser () a
topLevel x = spaces *> x <* eof
