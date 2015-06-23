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

lemmaParser :: CharParser () Lemma
lemmaParser = do
  w <- greekWord
  case textToSounds w of
    Left (InvalidChar c) -> fail ("InvalidChar " ++ c : [])
    Right x -> return $ Lemma w x

greekWordParser :: CharParser () [Sound]
greekWordParser = _lemmaSounds <$> lemmaParser

lemmasParser :: CharParser () [Lemma]
lemmasParser = endBy1 lemmaParser spaces

caseEndingParser :: CharParser () Suffix
caseEndingParser = pure (AttestedSuffix []) <* string "-"
  <|> pure UnattestedSuffix <* string "*"
  <|> AttestedSuffix <$> greekWordParser

nounFormsParser :: CharParser () (NounForms Suffix)
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
  <*> (spaces *> string "lemmas:" *> spaces *> lemmasParser)

validNounCategoryParser :: CharParser () NounCategory
validNounCategoryParser = do
  nc <- nounCategoryParser
  let ms = getNounMismatches nc
  case ms of
    [] -> return nc
    (_ : _) -> fail $ "Lemmas do not match nom sg/pl case ending "
      ++ (suffixToString . nomSg . _nounCategoryEndings $ nc)
      ++ " for " ++ (unpack . _nounCategoryName $ nc) ++ ":\n"
      ++ (concat . intersperse "\n" . fmap unpack . fmap _lemmaText $ ms)

adjective3FormsParser :: CharParser () (AdjectiveForms Suffix)
adjective3FormsParser =
  string "m:" *> spaces *> string "f:" *> spaces *> string "n:" *> spaces *>
  (AdjectiveForms
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

adjective2FormsParser :: CharParser () (AdjectiveForms Suffix)
adjective2FormsParser =
  string "mf:" *> spaces *> string "n:" *> spaces *>
  (adjective2Forms
    <$> le "nom sg:" <*> e
    <*> le "gen sg:" <*> e
    <*> le "dat sg:" <*> e
    <*> le "acc sg:" <*> e
    <*> le "voc sg:" <*> e
    <*> le "nom pl:" <*> e
    <*> le "gen pl:" <*> e
    <*> le "dat pl:" <*> e
    <*> le "acc pl:" <*> e
    <*> le "voc pl:" <*> e)
      where
        le x = string x *> spaces *> caseEndingParser <* spaces
        e = caseEndingParser <* spaces

adjectiveFormsParser :: CharParser () (AdjectiveForms Suffix)
adjectiveFormsParser = try adjective3FormsParser <|> adjective2FormsParser

adjectiveCategoryParser :: CharParser () AdjectiveCategory
adjectiveCategoryParser = AdjectiveCategory
  <$> (spaces *> (pack <$> (many1 (noneOf "\n\r") <* spaces)))
  <*> (spaces *> adjectiveFormsParser <* spaces)
  <*> (spaces *> string "lemmas:" *> spaces *> lemmasParser)

validAdjectiveCategoryParser :: CharParser () AdjectiveCategory
validAdjectiveCategoryParser = do
  ac <- adjectiveCategoryParser
  let ms = getAdjectiveMismatches ac
  case ms of
    [] -> return ac
    (_ : _) -> fail $ "Lemmas do not match nom sg/pl masc case ending "
      ++ (suffixToString . nomSgMasc . _adjectiveCategoryEndings $ ac)
      ++ " for " ++ (unpack . _adjectiveCategoryName $ ac) ++ ":\n"
      ++ (concat . intersperse "\n" . fmap unpack . fmap _lemmaText $ ms)

topLevel :: CharParser () a -> CharParser () a
topLevel x = spaces *> x <* eof
