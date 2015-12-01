{-# LANGUAGE ScopedTypeVariables #-}

module Text.Greek.IO.Morphgnt where

import Text.Greek.IO.Stage
import qualified Control.Lens as Lens
import qualified Text.Greek.IO.Json as Json
import qualified Text.Greek.IO.Type as Type
import qualified Text.Greek.Phonology.Consonant as Consonant
import qualified Text.Greek.Script.Mark as Mark
import qualified Text.Greek.Script.Syllable as Syllable
import qualified Text.Greek.Script.Word as Word
import qualified Text.Greek.Source.Morphgnt as Morphgnt

makeStage :: WordSurface Word.WithAccent (Syllable.SyllableListOrConsonants (Maybe Mark.AcuteCircumflex) [Consonant.PlusRoughRhoRoughBreathing])
  -> (Stage TypeData, WordSurface Word.WithAccent (Syllable.SyllableListOrConsonants (Maybe Mark.AcuteCircumflex) [Consonant.PlusRoughRhoRoughBreathing]))
makeStage accent = (stage, accent)
  where
    stage = Stage primaryType typeParts
    primaryType = makeWordPartType Json.WordPropertyTypeKind Type.MorphgntLemma (Lens.toListOf (Word.info . Word.morphgntWordLens . Morphgnt.wordLemma)) accent
    typeParts =
      [ makeWordPartType Json.WordPropertyTypeKind Type.MorphgntPartOfSpeech (Lens.toListOf (Word.info . Word.morphgntWordLens . Morphgnt.wordPartOfSpeech)) accent
      , makeWordPartType Json.WordPropertyTypeKind Type.MorphgntPerson (Lens.toListOf (Word.info . Word.morphgntWordLens . Morphgnt.wordPerson)) accent
      , makeWordPartType Json.WordPropertyTypeKind Type.MorphgntTense (Lens.toListOf (Word.info . Word.morphgntWordLens . Morphgnt.wordTense)) accent
      , makeWordPartType Json.WordPropertyTypeKind Type.MorphgntVoice (Lens.toListOf (Word.info . Word.morphgntWordLens . Morphgnt.wordVoice)) accent
      , makeWordPartType Json.WordPropertyTypeKind Type.MorphgntMood (Lens.toListOf (Word.info . Word.morphgntWordLens . Morphgnt.wordMood)) accent
      , makeWordPartType Json.WordPropertyTypeKind Type.MorphgntCase (Lens.toListOf (Word.info . Word.morphgntWordLens . Morphgnt.wordCase)) accent
      , makeWordPartType Json.WordPropertyTypeKind Type.MorphgntNumber (Lens.toListOf (Word.info . Word.morphgntWordLens . Morphgnt.wordNumber)) accent
      , makeWordPartType Json.WordPropertyTypeKind Type.MorphgntGender (Lens.toListOf (Word.info . Word.morphgntWordLens . Morphgnt.wordGender)) accent
      , makeWordPartType Json.WordPropertyTypeKind Type.MorphgntDegree (Lens.toListOf (Word.info . Word.morphgntWordLens . Morphgnt.wordDegree)) accent
      , makeWordPartType Json.WordPropertyTypeKind Type.MorphgntText (Lens.toListOf (Word.info . Word.morphgntWordLens . Morphgnt.wordTextWithPunctuation)) accent
      , makeWordPartType Json.WordPropertyTypeKind Type.MorphgntWord (Lens.toListOf (Word.info . Word.morphgntWordLens . Morphgnt.wordWordNoPunctuation)) accent
      , makeWordPartType Json.WordPropertyTypeKind Type.MorphgntNormalizedWord (Lens.toListOf (Word.info . Word.morphgntWordLens . Morphgnt.wordWordNormalized)) accent
      , makeWordPartType Json.WordPropertyTypeKind Type.MorphgntParsingCode getParse accent
      ]

getParse
  :: Word.Word (Word.IndexedP ((x, y, z, Morphgnt.Word), p)) s0
  -> [Morphgnt.Parse Maybe]
getParse = Morphgnt.expandWord . Lens.toListOf (Word.info . Word.morphgntWordLens)
