module Text.Greek.SBLGNT where

open import Data.List
open import Text.Greek.Bible
open import Text.Greek.SBLGNT.Matt
open import Text.Greek.SBLGNT.Mark
open import Text.Greek.SBLGNT.Luke
open import Text.Greek.SBLGNT.John
open import Text.Greek.SBLGNT.Acts
open import Text.Greek.SBLGNT.Rom
open import Text.Greek.SBLGNT.1Cor
open import Text.Greek.SBLGNT.2Cor
open import Text.Greek.SBLGNT.Gal
open import Text.Greek.SBLGNT.Eph
open import Text.Greek.SBLGNT.Phil
open import Text.Greek.SBLGNT.Col
open import Text.Greek.SBLGNT.1Thess
open import Text.Greek.SBLGNT.2Thess
open import Text.Greek.SBLGNT.1Tim
open import Text.Greek.SBLGNT.2Tim
open import Text.Greek.SBLGNT.Titus
open import Text.Greek.SBLGNT.Phlm
open import Text.Greek.SBLGNT.Heb
open import Text.Greek.SBLGNT.Jas
open import Text.Greek.SBLGNT.1Pet
open import Text.Greek.SBLGNT.2Pet
open import Text.Greek.SBLGNT.1John
open import Text.Greek.SBLGNT.2John
open import Text.Greek.SBLGNT.3John
open import Text.Greek.SBLGNT.Jude
open import Text.Greek.SBLGNT.Rev

books : List (List (Word))
books =
    ΚΑΤΑ-ΜΑΘΘΑΙΟΝ
  ∷ ΚΑΤΑ-ΜΑΡΚΟΝ
  ∷ ΚΑΤΑ-ΛΟΥΚΑΝ
  ∷ ΚΑΤΑ-ΙΩΑΝΝΗΝ
  ∷ ΠΡΑΞΕΙΣ-ΑΠΟΣΤΟΛΩΝ
  ∷ ΠΡΟΣ-ΡΩΜΑΙΟΥΣ
  ∷ ΠΡΟΣ-ΚΟΡΙΝΘΙΟΥΣ-Α
  ∷ ΠΡΟΣ-ΚΟΡΙΝΘΙΟΥΣ-Β
  ∷ ΠΡΟΣ-ΓΑΛΑΤΑΣ
  ∷ ΠΡΟΣ-ΕΦΕΣΙΟΥΣ
  ∷ ΠΡΟΣ-ΦΙΛΙΠΠΗΣΙΟΥΣ
  ∷ ΠΡΟΣ-ΚΟΛΟΣΣΑΕΙΣ
  ∷ ΠΡΟΣ-ΘΕΣΣΑΛΟΝΙΚΕΙΣ-Α
  ∷ ΠΡΟΣ-ΘΕΣΣΑΛΟΝΙΚΕΙΣ-Β
  ∷ ΠΡΟΣ-ΤΙΜΟΘΕΟΝ-Α
  ∷ ΠΡΟΣ-ΤΙΜΟΘΕΟΝ-Β
  ∷ ΠΡΟΣ-ΤΙΤΟΝ
  ∷ ΠΡΟΣ-ΦΙΛΗΜΟΝΑ
  ∷ ΠΡΟΣ-ΕΒΡΑΙΟΥΣ
  ∷ ΙΑΚΩΒΟΥ
  ∷ ΠΕΤΡΟΥ-Α
  ∷ ΠΕΤΡΟΥ-Β
  ∷ ΙΩΑΝΝΟΥ-Α
  ∷ ΙΩΑΝΝΟΥ-Β
  ∷ ΙΩΑΝΝΟΥ-Γ
  ∷ ΙΟΥΔΑ
  ∷ ΑΠΟΚΑΛΥΨΙΣ-ΙΩΑΝΝΟΥ
  ∷ []
