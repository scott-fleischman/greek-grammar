module Text.Greek.Script.Unicode where

open import Text.Greek.Script renaming
  ( α to α′
  ; β to β′
  ; γ to γ′
  ; δ to δ′
  ; ε to ε′
  ; ζ to ζ′
  ; η to η′
  ; θ to θ′
  ; ι to ι′
  ; κ to κ′
  ; μ to μ′
  ; ν to ν′
  ; ξ to ξ′
  ; ο to ο′
  ; π to π′
  ; ρ to ρ′
  ; σ to σ′
  ; τ to τ′
  ; υ to υ′
  ; φ to φ′
  ; χ to χ′
  ; ψ to ψ′
  ; ω to ω′
  )

-- U+0390 - U+03CE
ΐ : Token ι′ lower -- U+0390 GREEK SMALL LETTER IOTA WITH DIALYTIKA AND TONOS
ΐ = with-accent-diaeresis acute
Α : Token α′ upper -- U+0391 GREEK CAPITAL LETTER ALPHA
Α = unmarked
Β : Token β′ upper -- U+0392 GREEK CAPITAL LETTER BETA
Β = unmarked
Γ : Token γ′ upper -- U+0393 GREEK CAPITAL LETTER GAMMA
Γ = unmarked
Δ : Token δ′ upper -- U+0394 GREEK CAPITAL LETTER DELTA
Δ = unmarked
Ε : Token ε′ upper -- U+0395 GREEK CAPITAL LETTER EPSILON
Ε = unmarked
Ζ : Token ζ′ upper -- U+0396 GREEK CAPITAL LETTER ZETA
Ζ = unmarked
Η : Token η′ upper -- U+0397 GREEK CAPITAL LETTER ETA
Η = unmarked
Θ : Token θ′ upper -- U+0398 GREEK CAPITAL LETTER THETA
Θ = unmarked
Ι : Token ι′ upper -- U+0399 GREEK CAPITAL LETTER IOTA
Ι = unmarked
Κ : Token κ′ upper -- U+039A GREEK CAPITAL LETTER KAPPA
Κ = unmarked
Λ : Token λ′ upper -- U+039B GREEK CAPITAL LETTER LAMDA
Λ = unmarked
Μ : Token μ′ upper -- U+039C GREEK CAPITAL LETTER MU
Μ = unmarked
Ν : Token ν′ upper -- U+039D GREEK CAPITAL LETTER NU
Ν = unmarked
Ξ : Token ξ′ upper -- U+039E GREEK CAPITAL LETTER XI
Ξ = unmarked
Ο : Token ο′ upper -- U+039F GREEK CAPITAL LETTER OMICRON
Ο = unmarked
Π : Token π′ upper -- U+03A0 GREEK CAPITAL LETTER PI
Π = unmarked
Ρ : Token ρ′ upper -- U+03A1 GREEK CAPITAL LETTER RHO
Ρ = unmarked
                  -- U+03A2 <reserved>
Σ : Token σ′ upper -- U+03A3 GREEK CAPITAL LETTER SIGMA
Σ = unmarked
Τ : Token τ′ upper -- U+03A4 GREEK CAPITAL LETTER TAU
Τ = unmarked
Υ : Token υ′ upper -- U+03A5 GREEK CAPITAL LETTER UPSILON
Υ = unmarked
Φ : Token φ′ upper -- U+03A6 GREEK CAPITAL LETTER PHI
Φ = unmarked
Χ : Token χ′ upper -- U+03A7 GREEK CAPITAL LETTER CHI
Χ = unmarked
Ψ : Token ψ′ upper -- U+03A8 GREEK CAPITAL LETTER PSI
Ψ = unmarked
Ω : Token ω′ upper -- U+03A9 GREEK CAPITAL LETTER OMEGA
Ω = unmarked
Ϊ : Token ι′ upper -- U+03AA GREEK CAPITAL LETTER IOTA WITH DIALYTIKA
Ϊ = with-diaeresis
Ϋ : Token υ′ upper -- U+03AB GREEK CAPITAL LETTER UPSILON WITH DIALYTIKA
Ϋ = with-diaeresis
ά : Token α′ lower -- U+03AC GREEK SMALL LETTER ALPHA WITH TONOS
ά = with-accent acute
έ : Token ε′ lower -- U+03AD GREEK SMALL LETTER EPSILON WITH TONOS
έ = with-accent acute
ή : Token η′ lower -- U+03AE GREEK SMALL LETTER ETA WITH TONOS
ή = with-accent acute
ί : Token ι′ lower -- U+03AF GREEK SMALL LETTER IOTA WITH TONOS
ί = with-accent acute
ΰ : Token υ′ lower -- U+03B0 GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND TONOS
ΰ = with-accent-diaeresis acute
α : Token α′ lower -- U+03B1 GREEK SMALL LETTER ALPHA
α = unmarked
β : Token β′ lower -- U+03B2 GREEK SMALL LETTER BETA
β = unmarked
γ : Token γ′ lower -- U+03B3 GREEK SMALL LETTER GAMMA
γ = unmarked
δ : Token δ′ lower -- U+03B4 GREEK SMALL LETTER DELTA
δ = unmarked
ε : Token ε′ lower -- U+03B5 GREEK SMALL LETTER EPSILON
ε = unmarked
ζ : Token ζ′ lower -- U+03B6 GREEK SMALL LETTER ZETA
ζ = unmarked
η : Token η′ lower -- U+03B7 GREEK SMALL LETTER ETA
η = unmarked
θ : Token θ′ lower -- U+03B8 GREEK SMALL LETTER THETA
θ = unmarked
ι : Token ι′ lower -- U+03B9 GREEK SMALL LETTER IOTA
ι = unmarked
κ : Token κ′ lower -- U+03BA GREEK SMALL LETTER KAPPA
κ = unmarked
∙λ : Token λ′ lower -- U+03BB GREEK SMALL LETTER LAMDA
∙λ = unmarked
μ : Token μ′ lower -- U+03BC GREEK SMALL LETTER MU
μ = unmarked
ν : Token ν′ lower -- U+03BD GREEK SMALL LETTER NU
ν = unmarked
ξ : Token ξ′ lower -- U+03BE GREEK SMALL LETTER XI
ξ = unmarked
ο : Token ο′ lower -- U+03BF GREEK SMALL LETTER OMICRON
ο = unmarked
π : Token π′ lower -- U+03C0 GREEK SMALL LETTER PI
π = unmarked
ρ : Token ρ′ lower -- U+03C1 GREEK SMALL LETTER RHO
ρ = unmarked
ς : Token σ′ lower -- U+03C2 GREEK SMALL LETTER FINAL SIGMA
ς = final
σ : Token σ′ lower -- U+03C3 GREEK SMALL LETTER SIGMA
σ = unmarked
τ : Token τ′ lower -- U+03C4 GREEK SMALL LETTER TAU
τ = unmarked
υ : Token υ′ lower -- U+03C5 GREEK SMALL LETTER UPSILON
υ = unmarked
φ : Token φ′ lower -- U+03C6 GREEK SMALL LETTER PHI
φ = unmarked
χ : Token χ′ lower -- U+03C7 GREEK SMALL LETTER CHI
χ = unmarked
ψ : Token ψ′ lower -- U+03C8 GREEK SMALL LETTER PSI
ψ = unmarked
ω : Token ω′ lower -- U+03C9 GREEK SMALL LETTER OMEGA
ω = unmarked
ϊ : Token ι′ lower -- U+03CA GREEK SMALL LETTER IOTA WITH DIALYTIKA
ϊ = with-diaeresis
ϋ : Token υ′ lower -- U+03CB GREEK SMALL LETTER UPSILON WITH DIALYTIKA
ϋ = with-diaeresis
ό : Token ο′ lower -- U+03CC GREEK SMALL LETTER OMICRON WITH TONOS
ό = with-accent acute
ύ : Token υ′ lower -- U+03CD GREEK SMALL LETTER UPSILON WITH TONOS
ύ = with-accent acute
ώ : Token ω′ lower -- U+03CE GREEK SMALL LETTER OMEGA WITH TONOS
ώ = with-accent acute

-- U+1F00 - U+1FFF
ἀ : Token α′ lower -- U+1F00  ἀ GREEK SMALL LETTER ALPHA WITH PSILI
ἀ = with-breathing smooth
ἁ : Token α′ lower -- U+1F01  ἁ GREEK SMALL LETTER ALPHA WITH DASIA
ἁ = with-breathing rough
ἂ : Token α′ lower -- U+1F02  ἂ GREEK SMALL LETTER ALPHA WITH PSILI AND VARIA
ἂ = with-accent-breathing grave smooth
ἃ : Token α′ lower -- U+1F03  ἃ GREEK SMALL LETTER ALPHA WITH DASIA AND VARIA
ἃ = with-accent-breathing grave rough
ἄ : Token α′ lower -- U+1F04  ἄ GREEK SMALL LETTER ALPHA WITH PSILI AND OXIA
ἄ = with-accent-breathing acute smooth
ἅ : Token α′ lower -- U+1F05  ἅ GREEK SMALL LETTER ALPHA WITH DASIA AND OXIA
ἅ = with-accent-breathing acute rough
ἆ : Token α′ lower -- U+1F06  ἆ GREEK SMALL LETTER ALPHA WITH PSILI AND PERISPOMENI
ἆ = with-accent-breathing circumflex smooth
ἇ : Token α′ lower -- U+1F07  ἇ GREEK SMALL LETTER ALPHA WITH DASIA AND PERISPOMENI
ἇ = with-accent-breathing circumflex rough
Ἀ : Token α′ upper -- U+1F08  Ἀ GREEK CAPITAL LETTER ALPHA WITH PSILI
Ἀ = with-breathing smooth
Ἁ : Token α′ upper -- U+1F09  Ἁ GREEK CAPITAL LETTER ALPHA WITH DASIA
Ἁ = with-breathing rough
Ἂ : Token α′ upper -- U+1F0A  Ἂ GREEK CAPITAL LETTER ALPHA WITH PSILI AND VARIA
Ἂ = with-accent-breathing grave smooth
Ἃ : Token α′ upper -- U+1F0B  Ἃ GREEK CAPITAL LETTER ALPHA WITH DASIA AND VARIA
Ἃ = with-accent-breathing grave rough
Ἄ : Token α′ upper -- U+1F0C  Ἄ GREEK CAPITAL LETTER ALPHA WITH PSILI AND OXIA
Ἄ = with-accent-breathing acute smooth
Ἅ : Token α′ upper -- U+1F0D  Ἅ GREEK CAPITAL LETTER ALPHA WITH DASIA AND OXIA
Ἅ = with-accent-breathing acute rough
Ἆ : Token α′ upper -- U+1F0E  Ἆ GREEK CAPITAL LETTER ALPHA WITH PSILI AND PERISPOMENI
Ἆ = with-accent-breathing circumflex smooth
Ἇ : Token α′ upper -- U+1F0F  Ἇ GREEK CAPITAL LETTER ALPHA WITH DASIA AND PERISPOMENI
Ἇ = with-accent-breathing circumflex rough
ἐ : Token ε′ lower -- U+1F10  ἐ GREEK SMALL LETTER EPSILON WITH PSILI
ἐ = with-breathing smooth
ἑ : Token ε′ lower -- U+1F11  ἑ GREEK SMALL LETTER EPSILON WITH DASIA
ἑ = with-breathing rough
ἒ : Token ε′ lower -- U+1F12  ἒ GREEK SMALL LETTER EPSILON WITH PSILI AND VARIA
ἒ = with-accent-breathing grave smooth
ἓ : Token ε′ lower -- U+1F13  ἓ GREEK SMALL LETTER EPSILON WITH DASIA AND VARIA
ἓ = with-accent-breathing grave rough
ἔ : Token ε′ lower -- U+1F14  ἔ GREEK SMALL LETTER EPSILON WITH PSILI AND OXIA
ἔ = with-accent-breathing acute smooth
ἕ : Token ε′ lower -- U+1F15  ἕ GREEK SMALL LETTER EPSILON WITH DASIA AND OXIA
ἕ = with-accent-breathing acute rough
-- U+1F16
-- U+1F17
Ἐ : Token ε′ upper -- U+1F18  Ἐ GREEK CAPITAL LETTER EPSILON WITH PSILI
Ἐ = with-breathing smooth
Ἑ : Token ε′ upper -- U+1F19  Ἑ GREEK CAPITAL LETTER EPSILON WITH DASIA
Ἑ = with-breathing rough
Ἒ : Token ε′ upper -- U+1F1A  Ἒ GREEK CAPITAL LETTER EPSILON WITH PSILI AND VARIA
Ἒ = with-accent-breathing grave smooth
Ἓ : Token ε′ upper -- U+1F1B  Ἓ GREEK CAPITAL LETTER EPSILON WITH DASIA AND VARIA
Ἓ = with-accent-breathing grave rough
Ἔ : Token ε′ upper -- U+1F1C  Ἔ GREEK CAPITAL LETTER EPSILON WITH PSILI AND OXIA
Ἔ = with-accent-breathing acute smooth
Ἕ : Token ε′ upper -- U+1F1D  Ἕ GREEK CAPITAL LETTER EPSILON WITH DASIA AND OXIA
Ἕ = with-accent-breathing acute rough
-- U+1F1E
-- U+1F1F
ἠ : Token η′ lower -- U+1F20  ἠ GREEK SMALL LETTER ETA WITH PSILI
ἠ = with-breathing smooth
ἡ : Token η′ lower -- U+1F21  ἡ GREEK SMALL LETTER ETA WITH DASIA
ἡ = with-breathing rough
ἢ : Token η′ lower -- U+1F22  ἢ GREEK SMALL LETTER ETA WITH PSILI AND VARIA
ἢ = with-accent-breathing grave smooth
ἣ : Token η′ lower -- U+1F23  ἣ GREEK SMALL LETTER ETA WITH DASIA AND VARIA
ἣ = with-accent-breathing grave rough
ἤ : Token η′ lower -- U+1F24  ἤ GREEK SMALL LETTER ETA WITH PSILI AND OXIA
ἤ = with-accent-breathing acute smooth
ἥ : Token η′ lower -- U+1F25  ἥ GREEK SMALL LETTER ETA WITH DASIA AND OXIA
ἥ = with-accent-breathing acute rough
ἦ : Token η′ lower -- U+1F26  ἦ GREEK SMALL LETTER ETA WITH PSILI AND PERISPOMENI
ἦ = with-accent-breathing circumflex smooth
ἧ : Token η′ lower -- U+1F27  ἧ GREEK SMALL LETTER ETA WITH DASIA AND PERISPOMENI
ἧ = with-accent-breathing circumflex rough
Ἠ : Token η′ upper -- U+1F28  Ἠ GREEK CAPITAL LETTER ETA WITH PSILI
Ἠ = with-breathing smooth
Ἡ : Token η′ upper -- U+1F29  Ἡ GREEK CAPITAL LETTER ETA WITH DASIA
Ἡ = with-breathing rough
Ἢ : Token η′ upper -- U+1F2A  Ἢ GREEK CAPITAL LETTER ETA WITH PSILI AND VARIA
Ἢ = with-accent-breathing grave smooth
Ἣ : Token η′ upper -- U+1F2B  Ἣ GREEK CAPITAL LETTER ETA WITH DASIA AND VARIA
Ἣ = with-accent-breathing grave rough
Ἤ : Token η′ upper -- U+1F2C  Ἤ GREEK CAPITAL LETTER ETA WITH PSILI AND OXIA
Ἤ = with-accent-breathing acute smooth
Ἥ : Token η′ upper -- U+1F2D  Ἥ GREEK CAPITAL LETTER ETA WITH DASIA AND OXIA
Ἥ = with-accent-breathing acute rough
Ἦ : Token η′ upper -- U+1F2E  Ἦ GREEK CAPITAL LETTER ETA WITH PSILI AND PERISPOMENI
Ἦ = with-accent-breathing circumflex smooth
Ἧ : Token η′ upper -- U+1F2F  Ἧ GREEK CAPITAL LETTER ETA WITH DASIA AND PERISPOMENI
Ἧ = with-accent-breathing circumflex rough
ἰ : Token ι′ lower -- U+1F30  ἰ GREEK SMALL LETTER IOTA WITH PSILI
ἰ = with-breathing smooth
ἱ : Token ι′ lower -- U+1F31  ἱ GREEK SMALL LETTER IOTA WITH DASIA
ἱ = with-breathing rough
ἲ : Token ι′ lower -- U+1F32  ἲ GREEK SMALL LETTER IOTA WITH PSILI AND VARIA
ἲ = with-accent-breathing grave smooth
ἳ : Token ι′ lower -- U+1F33  ἳ GREEK SMALL LETTER IOTA WITH DASIA AND VARIA
ἳ = with-accent-breathing grave rough
ἴ : Token ι′ lower -- U+1F34  ἴ GREEK SMALL LETTER IOTA WITH PSILI AND OXIA
ἴ = with-accent-breathing acute smooth
ἵ : Token ι′ lower -- U+1F35  ἵ GREEK SMALL LETTER IOTA WITH DASIA AND OXIA
ἵ = with-accent-breathing acute rough
ἶ : Token ι′ lower -- U+1F36  ἶ GREEK SMALL LETTER IOTA WITH PSILI AND PERISPOMENI
ἶ = with-accent-breathing circumflex smooth
ἷ : Token ι′ lower -- U+1F37  ἷ GREEK SMALL LETTER IOTA WITH DASIA AND PERISPOMENI
ἷ = with-accent-breathing circumflex rough
Ἰ : Token ι′ upper -- U+1F38  Ἰ GREEK CAPITAL LETTER IOTA WITH PSILI
Ἰ = with-breathing smooth
Ἱ : Token ι′ upper -- U+1F39  Ἱ GREEK CAPITAL LETTER IOTA WITH DASIA
Ἱ = with-breathing rough
Ἲ : Token ι′ upper -- U+1F3A  Ἲ GREEK CAPITAL LETTER IOTA WITH PSILI AND VARIA
Ἲ = with-accent-breathing grave smooth
Ἳ : Token ι′ upper -- U+1F3B  Ἳ GREEK CAPITAL LETTER IOTA WITH DASIA AND VARIA
Ἳ = with-accent-breathing grave rough
Ἴ : Token ι′ upper -- U+1F3C  Ἴ GREEK CAPITAL LETTER IOTA WITH PSILI AND OXIA
Ἴ = with-accent-breathing acute smooth
Ἵ : Token ι′ upper -- U+1F3D  Ἵ GREEK CAPITAL LETTER IOTA WITH DASIA AND OXIA
Ἵ = with-accent-breathing acute rough
Ἶ : Token ι′ upper -- U+1F3E  Ἶ GREEK CAPITAL LETTER IOTA WITH PSILI AND PERISPOMENI
Ἶ = with-accent-breathing circumflex smooth
Ἷ : Token ι′ upper -- U+1F3F  Ἷ GREEK CAPITAL LETTER IOTA WITH DASIA AND PERISPOMENI
Ἷ = with-accent-breathing circumflex rough
ὀ : Token ο′ lower -- U+1F40  ὀ GREEK SMALL LETTER OMICRON WITH PSILI
ὀ = with-breathing smooth
ὁ : Token ο′ lower -- U+1F41  ὁ GREEK SMALL LETTER OMICRON WITH DASIA
ὁ = with-breathing rough
ὂ : Token ο′ lower -- U+1F42  ὂ GREEK SMALL LETTER OMICRON WITH PSILI AND VARIA
ὂ = with-accent-breathing grave smooth
ὃ : Token ο′ lower -- U+1F43  ὃ GREEK SMALL LETTER OMICRON WITH DASIA AND VARIA
ὃ = with-accent-breathing grave rough
ὄ : Token ο′ lower -- U+1F44  ὄ GREEK SMALL LETTER OMICRON WITH PSILI AND OXIA
ὄ = with-accent-breathing acute smooth
ὅ : Token ο′ lower -- U+1F45  ὅ GREEK SMALL LETTER OMICRON WITH DASIA AND OXIA
ὅ = with-accent-breathing acute rough
-- U+1F46
-- U+1F47
Ὀ : Token ο′ upper -- U+1F48  Ὀ GREEK CAPITAL LETTER OMICRON WITH PSILI
Ὀ = with-breathing smooth
Ὁ : Token ο′ upper -- U+1F49  Ὁ GREEK CAPITAL LETTER OMICRON WITH DASIA
Ὁ = with-breathing rough
Ὂ : Token ο′ upper -- U+1F4A  Ὂ GREEK CAPITAL LETTER OMICRON WITH PSILI AND VARIA
Ὂ = with-accent-breathing grave smooth
Ὃ : Token ο′ upper -- U+1F4B  Ὃ GREEK CAPITAL LETTER OMICRON WITH DASIA AND VARIA
Ὃ = with-accent-breathing grave rough
Ὄ : Token ο′ upper -- U+1F4C  Ὄ GREEK CAPITAL LETTER OMICRON WITH PSILI AND OXIA
Ὄ = with-accent-breathing acute smooth
Ὅ : Token ο′ upper -- U+1F4D  Ὅ GREEK CAPITAL LETTER OMICRON WITH DASIA AND OXIA
Ὅ = with-accent-breathing acute rough
-- U+1F4E
-- U+1F4F
ὐ : Token υ′ lower -- U+1F50  ὐ GREEK SMALL LETTER UPSILON WITH PSILI
ὐ = with-breathing smooth
ὑ : Token υ′ lower -- U+1F51  ὑ GREEK SMALL LETTER UPSILON WITH DASIA
ὑ = with-breathing rough
ὒ : Token υ′ lower -- U+1F52  ὒ GREEK SMALL LETTER UPSILON WITH PSILI AND VARIA
ὒ = with-accent-breathing grave smooth
ὓ : Token υ′ lower -- U+1F53  ὓ GREEK SMALL LETTER UPSILON WITH DASIA AND VARIA
ὓ = with-accent-breathing grave rough
ὔ : Token υ′ lower -- U+1F54  ὔ GREEK SMALL LETTER UPSILON WITH PSILI AND OXIA
ὔ = with-accent-breathing acute smooth
ὕ : Token υ′ lower -- U+1F55  ὕ GREEK SMALL LETTER UPSILON WITH DASIA AND OXIA
ὕ = with-accent-breathing acute rough
ὖ : Token υ′ lower -- U+1F56  ὖ GREEK SMALL LETTER UPSILON WITH PSILI AND PERISPOMENI
ὖ = with-accent-breathing circumflex smooth
ὗ : Token υ′ lower -- U+1F57  ὗ GREEK SMALL LETTER UPSILON WITH DASIA AND PERISPOMENI
ὗ = with-accent-breathing circumflex rough
-- U+1F58
Ὑ : Token υ′ upper -- U+1F59  Ὑ GREEK CAPITAL LETTER UPSILON WITH DASIA
Ὑ = with-breathing rough
-- U+1F5A
Ὓ : Token υ′ upper -- U+1F5B  Ὓ GREEK CAPITAL LETTER UPSILON WITH DASIA AND VARIA
Ὓ = with-accent-breathing grave rough
-- U+1F5C
Ὕ : Token υ′ upper -- U+1F5D  Ὕ GREEK CAPITAL LETTER UPSILON WITH DASIA AND OXIA
Ὕ = with-accent-breathing acute rough
-- U+1F5E
Ὗ : Token υ′ upper -- U+1F5F  Ὗ GREEK CAPITAL LETTER UPSILON WITH DASIA AND PERISPOMENI
Ὗ = with-accent-breathing circumflex rough
ὠ : Token ω′ lower -- U+1F60  ὠ GREEK SMALL LETTER OMEGA WITH PSILI
ὠ = with-breathing smooth
ὡ : Token ω′ lower -- U+1F61  ὡ GREEK SMALL LETTER OMEGA WITH DASIA
ὡ = with-breathing rough
ὢ : Token ω′ lower -- U+1F62  ὢ GREEK SMALL LETTER OMEGA WITH PSILI AND VARIA
ὢ = with-accent-breathing grave smooth
ὣ : Token ω′ lower -- U+1F63  ὣ GREEK SMALL LETTER OMEGA WITH DASIA AND VARIA
ὣ = with-accent-breathing grave rough
ὤ : Token ω′ lower -- U+1F64  ὤ GREEK SMALL LETTER OMEGA WITH PSILI AND OXIA
ὤ = with-accent-breathing acute smooth
ὥ : Token ω′ lower -- U+1F65  ὥ GREEK SMALL LETTER OMEGA WITH DASIA AND OXIA
ὥ = with-accent-breathing acute rough
ὦ : Token ω′ lower -- U+1F66  ὦ GREEK SMALL LETTER OMEGA WITH PSILI AND PERISPOMENI
ὦ = with-accent-breathing circumflex smooth
ὧ : Token ω′ lower -- U+1F67  ὧ GREEK SMALL LETTER OMEGA WITH DASIA AND PERISPOMENI
ὧ = with-accent-breathing circumflex rough
Ὠ : Token ω′ upper -- U+1F68  Ὠ GREEK CAPITAL LETTER OMEGA WITH PSILI
Ὠ = with-breathing smooth
Ὡ : Token ω′ upper -- U+1F69  Ὡ GREEK CAPITAL LETTER OMEGA WITH DASIA
Ὡ = with-breathing rough
Ὢ : Token ω′ upper -- U+1F6A  Ὢ GREEK CAPITAL LETTER OMEGA WITH PSILI AND VARIA
Ὢ = with-accent-breathing grave smooth
Ὣ : Token ω′ upper -- U+1F6B  Ὣ GREEK CAPITAL LETTER OMEGA WITH DASIA AND VARIA
Ὣ = with-accent-breathing grave rough
Ὤ : Token ω′ upper -- U+1F6C  Ὤ GREEK CAPITAL LETTER OMEGA WITH PSILI AND OXIA
Ὤ = with-accent-breathing acute smooth
Ὥ : Token ω′ upper -- U+1F6D  Ὥ GREEK CAPITAL LETTER OMEGA WITH DASIA AND OXIA
Ὥ = with-accent-breathing acute rough
Ὦ : Token ω′ upper -- U+1F6E  Ὦ GREEK CAPITAL LETTER OMEGA WITH PSILI AND PERISPOMENI
Ὦ = with-accent-breathing circumflex smooth
Ὧ : Token ω′ upper -- U+1F6F  Ὧ GREEK CAPITAL LETTER OMEGA WITH DASIA AND PERISPOMENI
Ὧ = with-accent-breathing circumflex rough
ὰ : Token α′ lower -- U+1F70  ὰ GREEK SMALL LETTER ALPHA WITH VARIA
ὰ = with-accent grave
ά : Token α′ lower -- U+1F71  ά GREEK SMALL LETTER ALPHA WITH OXIA
ά = with-accent acute
ὲ : Token ε′ lower -- U+1F72  ὲ GREEK SMALL LETTER EPSILON WITH VARIA
ὲ = with-accent grave
έ : Token ε′ lower -- U+1F73  έ GREEK SMALL LETTER EPSILON WITH OXIA
έ = with-accent acute
ὴ : Token η′ lower -- U+1F74  ὴ GREEK SMALL LETTER ETA WITH VARIA
ὴ = with-accent grave
ή : Token η′ lower -- U+1F75  ή GREEK SMALL LETTER ETA WITH OXIA
ή = with-accent acute
ὶ : Token ι′ lower -- U+1F76  ὶ GREEK SMALL LETTER IOTA WITH VARIA
ὶ = with-accent grave
ί : Token ι′ lower -- U+1F77  ί GREEK SMALL LETTER IOTA WITH OXIA
ί = with-accent acute
ὸ : Token ο′ lower -- U+1F78  ὸ GREEK SMALL LETTER OMICRON WITH VARIA
ὸ = with-accent grave
ό : Token ο′ lower -- U+1F79  ό GREEK SMALL LETTER OMICRON WITH OXIA
ό = with-accent acute
ὺ : Token υ′ lower -- U+1F7A  ὺ GREEK SMALL LETTER UPSILON WITH VARIA
ὺ = with-accent grave
ύ : Token υ′ lower -- U+1F7B  ύ GREEK SMALL LETTER UPSILON WITH OXIA
ύ = with-accent acute
ὼ : Token ω′ lower -- U+1F7C  ὼ GREEK SMALL LETTER OMEGA WITH VARIA
ὼ = with-accent grave
ώ : Token ω′ lower -- U+1F7D  ώ GREEK SMALL LETTER OMEGA WITH OXIA
ώ = with-accent acute
-- U+1F7E
-- U+1F7F
ᾀ : Token α′ lower -- U+1F80  ᾀ GREEK SMALL LETTER ALPHA WITH PSILI AND YPOGEGRAMMENI
ᾀ = with-breathing-iota smooth
ᾁ : Token α′ lower -- U+1F81  ᾁ GREEK SMALL LETTER ALPHA WITH DASIA AND YPOGEGRAMMENI
ᾁ = with-breathing-iota rough
ᾂ : Token α′ lower -- U+1F82  ᾂ GREEK SMALL LETTER ALPHA WITH PSILI AND VARIA AND YPOGEGRAMMENI
ᾂ = with-accent-breathing-iota grave smooth
ᾃ : Token α′ lower -- U+1F83  ᾃ GREEK SMALL LETTER ALPHA WITH DASIA AND VARIA AND YPOGEGRAMMENI
ᾃ = with-accent-breathing grave rough
ᾄ : Token α′ lower -- U+1F84  ᾄ GREEK SMALL LETTER ALPHA WITH PSILI AND OXIA AND YPOGEGRAMMENI
ᾄ = with-accent-breathing-iota acute smooth
ᾅ : Token α′ lower -- U+1F85  ᾅ GREEK SMALL LETTER ALPHA WITH DASIA AND OXIA AND YPOGEGRAMMENI
ᾅ = with-accent-breathing-iota acute rough
ᾆ : Token α′ lower -- U+1F86  ᾆ GREEK SMALL LETTER ALPHA WITH PSILI AND PERISPOMENI AND YPOGEGRAMMENI
ᾆ = with-accent-breathing-iota circumflex smooth
ᾇ : Token α′ lower -- U+1F87  ᾇ GREEK SMALL LETTER ALPHA WITH DASIA AND PERISPOMENI AND YPOGEGRAMMENI
ᾇ = with-accent-breathing-iota circumflex rough
ᾈ : Token α′ upper -- U+1F88  ᾈ GREEK CAPITAL LETTER ALPHA WITH PSILI AND PROSGEGRAMMENI
ᾈ = with-breathing-iota smooth
ᾉ : Token α′ upper -- U+1F89  ᾉ GREEK CAPITAL LETTER ALPHA WITH DASIA AND PROSGEGRAMMENI
ᾉ = with-breathing-iota rough
ᾊ : Token α′ upper -- U+1F8A  ᾊ GREEK CAPITAL LETTER ALPHA WITH PSILI AND VARIA AND PROSGEGRAMMENI
ᾊ = with-accent-breathing-iota grave smooth
ᾋ : Token α′ upper -- U+1F8B  ᾋ GREEK CAPITAL LETTER ALPHA WITH DASIA AND VARIA AND PROSGEGRAMMENI
ᾋ = with-accent-breathing-iota grave rough
ᾌ : Token α′ upper -- U+1F8C  ᾌ GREEK CAPITAL LETTER ALPHA WITH PSILI AND OXIA AND PROSGEGRAMMENI
ᾌ = with-accent-breathing-iota acute smooth
ᾍ : Token α′ upper -- U+1F8D  ᾍ GREEK CAPITAL LETTER ALPHA WITH DASIA AND OXIA AND PROSGEGRAMMENI
ᾍ = with-accent-breathing-iota acute rough
ᾎ : Token α′ upper -- U+1F8E  ᾎ GREEK CAPITAL LETTER ALPHA WITH PSILI AND PERISPOMENI AND PROSGEGRAMMENI
ᾎ = with-accent-breathing-iota circumflex smooth
ᾏ : Token α′ upper -- U+1F8F  ᾏ GREEK CAPITAL LETTER ALPHA WITH DASIA AND PERISPOMENI AND PROSGEGRAMMENI
ᾏ = with-accent-breathing-iota circumflex rough
ᾐ : Token η′ lower -- U+1F90  ᾐ GREEK SMALL LETTER ETA WITH PSILI AND YPOGEGRAMMENI
ᾐ = with-breathing-iota smooth
ᾑ : Token η′ lower -- U+1F91  ᾑ GREEK SMALL LETTER ETA WITH DASIA AND YPOGEGRAMMENI
ᾑ = with-breathing-iota rough
ᾒ : Token η′ lower -- U+1F92  ᾒ GREEK SMALL LETTER ETA WITH PSILI AND VARIA AND YPOGEGRAMMENI
ᾒ = with-accent-breathing-iota grave smooth
ᾓ : Token η′ lower -- U+1F93  ᾓ GREEK SMALL LETTER ETA WITH DASIA AND VARIA AND YPOGEGRAMMENI
ᾓ = with-accent-breathing-iota grave rough
ᾔ : Token η′ lower -- U+1F94  ᾔ GREEK SMALL LETTER ETA WITH PSILI AND OXIA AND YPOGEGRAMMENI
ᾔ = with-accent-breathing-iota acute smooth
ᾕ : Token η′ lower -- U+1F95  ᾕ GREEK SMALL LETTER ETA WITH DASIA AND OXIA AND YPOGEGRAMMENI
ᾕ = with-accent-breathing-iota acute rough
ᾖ : Token η′ lower -- U+1F96  ᾖ GREEK SMALL LETTER ETA WITH PSILI AND PERISPOMENI AND YPOGEGRAMMENI
ᾖ = with-accent-breathing-iota circumflex smooth
ᾗ : Token η′ lower -- U+1F97  ᾗ GREEK SMALL LETTER ETA WITH DASIA AND PERISPOMENI AND YPOGEGRAMMENI
ᾗ = with-accent-breathing-iota circumflex rough
ᾘ : Token η′ upper -- U+1F98  ᾘ GREEK CAPITAL LETTER ETA WITH PSILI AND PROSGEGRAMMENI
ᾘ = with-breathing-iota smooth
ᾙ : Token η′ upper -- U+1F99  ᾙ GREEK CAPITAL LETTER ETA WITH DASIA AND PROSGEGRAMMENI
ᾙ = with-breathing-iota rough
ᾚ : Token η′ upper -- U+1F9A  ᾚ GREEK CAPITAL LETTER ETA WITH PSILI AND VARIA AND PROSGEGRAMMENI
ᾚ = with-accent-breathing-iota grave smooth
ᾛ : Token η′ upper -- U+1F9B  ᾛ GREEK CAPITAL LETTER ETA WITH DASIA AND VARIA AND PROSGEGRAMMENI
ᾛ = with-accent-breathing-iota grave rough
ᾜ : Token η′ upper -- U+1F9C  ᾜ GREEK CAPITAL LETTER ETA WITH PSILI AND OXIA AND PROSGEGRAMMENI
ᾜ = with-accent-breathing-iota acute smooth
ᾝ : Token η′ upper -- U+1F9D  ᾝ GREEK CAPITAL LETTER ETA WITH DASIA AND OXIA AND PROSGEGRAMMENI
ᾝ = with-accent-breathing-iota acute rough
ᾞ : Token η′ upper -- U+1F9E  ᾞ GREEK CAPITAL LETTER ETA WITH PSILI AND PERISPOMENI AND PROSGEGRAMMENI
ᾞ = with-accent-breathing-iota circumflex smooth
ᾟ : Token η′ upper -- U+1F9F  ᾟ GREEK CAPITAL LETTER ETA WITH DASIA AND PERISPOMENI AND PROSGEGRAMMENI
ᾟ = with-accent-breathing-iota circumflex rough
ᾠ : Token ω′ lower -- U+1FA0  ᾠ GREEK SMALL LETTER OMEGA WITH PSILI AND YPOGEGRAMMENI
ᾠ = with-breathing-iota smooth
ᾡ : Token ω′ lower -- U+1FA1  ᾡ GREEK SMALL LETTER OMEGA WITH DASIA AND YPOGEGRAMMENI
ᾡ = with-breathing-iota rough
ᾢ : Token ω′ lower -- U+1FA2  ᾢ GREEK SMALL LETTER OMEGA WITH PSILI AND VARIA AND YPOGEGRAMMENI
ᾢ = with-accent-breathing-iota grave smooth
ᾣ : Token ω′ lower -- U+1FA3  ᾣ GREEK SMALL LETTER OMEGA WITH DASIA AND VARIA AND YPOGEGRAMMENI
ᾣ = with-accent-breathing-iota grave rough
ᾤ : Token ω′ lower -- U+1FA4  ᾤ GREEK SMALL LETTER OMEGA WITH PSILI AND OXIA AND YPOGEGRAMMENI
ᾤ = with-accent-breathing-iota acute smooth
ᾥ : Token ω′ lower -- U+1FA5  ᾥ GREEK SMALL LETTER OMEGA WITH DASIA AND OXIA AND YPOGEGRAMMENI
ᾥ = with-accent-breathing-iota acute rough
ᾦ : Token ω′ lower -- U+1FA6  ᾦ GREEK SMALL LETTER OMEGA WITH PSILI AND PERISPOMENI AND YPOGEGRAMMENI
ᾦ = with-accent-breathing-iota circumflex smooth
ᾧ : Token ω′ lower -- U+1FA7  ᾧ GREEK SMALL LETTER OMEGA WITH DASIA AND PERISPOMENI AND YPOGEGRAMMENI
ᾧ = with-accent-breathing-iota circumflex rough
ᾨ : Token ω′ upper -- U+1FA8  ᾨ GREEK CAPITAL LETTER OMEGA WITH PSILI AND PROSGEGRAMMENI
ᾨ = with-breathing-iota smooth
ᾩ : Token ω′ upper -- U+1FA9  ᾩ GREEK CAPITAL LETTER OMEGA WITH DASIA AND PROSGEGRAMMENI
ᾩ = with-breathing-iota rough
ᾪ : Token ω′ upper -- U+1FAA  ᾪ GREEK CAPITAL LETTER OMEGA WITH PSILI AND VARIA AND PROSGEGRAMMENI
ᾪ = with-accent-breathing-iota grave smooth
ᾫ : Token ω′ upper -- U+1FAB  ᾫ GREEK CAPITAL LETTER OMEGA WITH DASIA AND VARIA AND PROSGEGRAMMENI
ᾫ = with-accent-breathing-iota grave rough
ᾬ : Token ω′ upper -- U+1FAC  ᾬ GREEK CAPITAL LETTER OMEGA WITH PSILI AND OXIA AND PROSGEGRAMMENI
ᾬ = with-accent-breathing-iota acute smooth
ᾭ : Token ω′ upper -- U+1FAD  ᾭ GREEK CAPITAL LETTER OMEGA WITH DASIA AND OXIA AND PROSGEGRAMMENI
ᾭ = with-accent-breathing-iota acute rough
ᾮ : Token ω′ upper -- U+1FAE  ᾮ GREEK CAPITAL LETTER OMEGA WITH PSILI AND PERISPOMENI AND PROSGEGRAMMENI
ᾮ = with-accent-breathing-iota circumflex smooth
ᾯ : Token ω′ upper -- U+1FAF  ᾯ GREEK CAPITAL LETTER OMEGA WITH DASIA AND PERISPOMENI AND PROSGEGRAMMENI
ᾯ = with-accent-breathing-iota circumflex rough
-- U+1FB0 ᾰ GREEK SMALL LETTER ALPHA WITH VRACHY
-- U+1FB1 ᾱ GREEK SMALL LETTER ALPHA WITH MACRON
ᾲ : Token α′ lower -- U+1FB2  ᾲ GREEK SMALL LETTER ALPHA WITH VARIA AND YPOGEGRAMMENI
ᾲ = with-accent-iota grave
ᾳ : Token α′ lower -- U+1FB3  ᾳ GREEK SMALL LETTER ALPHA WITH YPOGEGRAMMENI
ᾳ = with-iota
ᾴ : Token α′ lower -- U+1FB4  ᾴ GREEK SMALL LETTER ALPHA WITH OXIA AND YPOGEGRAMMENI
ᾴ = with-accent-iota acute
-- U+1FB5
ᾶ : Token α′ lower -- U+1FB6  ᾶ GREEK SMALL LETTER ALPHA WITH PERISPOMENI
ᾶ = with-accent-iota circumflex
ᾷ : Token α′ lower -- U+1FB7  ᾷ GREEK SMALL LETTER ALPHA WITH PERISPOMENI AND YPOGEGRAMMENI
ᾷ = with-accent-iota circumflex
-- U+1FB8 Ᾰ GREEK CAPITAL LETTER ALPHA WITH VRACHY
-- U+1FB9 Ᾱ GREEK CAPITAL LETTER ALPHA WITH MACRON
Ὰ : Token α′ upper -- U+1FBA  Ὰ GREEK CAPITAL LETTER ALPHA WITH VARIA
Ὰ = with-accent grave
Ά : Token α′ upper -- U+1FBB  Ά GREEK CAPITAL LETTER ALPHA WITH OXIA
Ά = with-accent acute
ᾼ : Token α′ upper -- U+1FBC  ᾼ GREEK CAPITAL LETTER ALPHA WITH PROSGEGRAMMENI
ᾼ = with-iota
-- U+1FBD  ᾽  GREEK KORONIS
-- U+1FBE  ι  GREEK PROSGEGRAMMENI
-- U+1FBF  ᾿  GREEK PSILI
-- U+1FC0  ῀  GREEK PERISPOMENI
-- U+1FC1  ῁  GREEK DIALYTIKA AND PERISPOMENI
ῂ : Token η′ lower -- U+1FC2  ῂ GREEK SMALL LETTER ETA WITH VARIA AND YPOGEGRAMMENI accent circumflex
ῂ = with-accent-iota grave
ῃ : Token η′ lower -- U+1FC3  ῃ GREEK SMALL LETTER ETA WITH YPOGEGRAMMENI
ῃ = with-iota
ῄ : Token η′ lower -- U+1FC4  ῄ GREEK SMALL LETTER ETA WITH OXIA AND YPOGEGRAMMENI
ῄ = with-accent-iota acute
-- U+1FC5
ῆ : Token η′ lower -- U+1FC6  ῆ GREEK SMALL LETTER ETA WITH PERISPOMENI
ῆ = with-accent circumflex
ῇ : Token η′ lower -- U+1FC7  ῇ GREEK SMALL LETTER ETA WITH PERISPOMENI AND YPOGEGRAMMENI
ῇ = with-accent-iota circumflex
Ὲ : Token ε′ upper -- U+1FC8  Ὲ GREEK CAPITAL LETTER EPSILON WITH VARIA
Ὲ = with-accent grave
Έ : Token ε′ upper -- U+1FC9  Έ GREEK CAPITAL LETTER EPSILON WITH OXIA
Έ = with-accent acute
Ὴ : Token η′ upper -- U+1FCA  Ὴ GREEK CAPITAL LETTER ETA WITH VARIA
Ὴ = with-accent grave
Ή : Token η′ upper -- U+1FCB  Ή GREEK CAPITAL LETTER ETA WITH OXIA
Ή = with-accent acute
ῌ : Token η′ upper -- U+1FCC  ῌ GREEK CAPITAL LETTER ETA WITH PROSGEGRAMMENI
ῌ = with-iota
-- U+1FCD  ῍ GREEK PSILI AND VARIA
-- U+1FCE  ῎ GREEK PSILI AND OXIA
-- U+1FCF  ῏ GREEK PSILI AND PERISPOMENI
-- U+1FD0  ῐ GREEK SMALL LETTER IOTA WITH VRACHY
-- U+1FD1  ῑ GREEK SMALL LETTER IOTA WITH MACRON
ῒ : Token ι′ lower -- U+1FD2  ῒ GREEK SMALL LETTER IOTA WITH DIALYTIKA AND VARIA
ῒ = with-accent-diaeresis grave
ΐ : Token ι′ lower -- U+1FD3  ΐ GREEK SMALL LETTER IOTA WITH DIALYTIKA AND OXIA
ΐ = with-accent-diaeresis acute
-- U+1FD4
-- U+1FD5
ῖ : Token ι′ lower -- U+1FD6  ῖ GREEK SMALL LETTER IOTA WITH PERISPOMENI
ῖ = with-accent circumflex
ῗ : Token ι′ lower -- U+1FD7  ῗ GREEK SMALL LETTER IOTA WITH DIALYTIKA AND PERISPOMENI
ῗ = with-accent-diaeresis circumflex
-- U+1FD8 Ῐ GREEK CAPITAL LETTER IOTA WITH VRACHY
-- U+1FD9 Ῑ GREEK CAPITAL LETTER IOTA WITH MACRON
Ὶ : Token ι′ upper -- U+1FDA  Ὶ GREEK CAPITAL LETTER IOTA WITH VARIA
Ὶ = with-accent grave
Ί : Token ι′ upper -- U+1FDB  Ί GREEK CAPITAL LETTER IOTA WITH OXIA
Ί = with-accent acute
-- U+1FDC
-- U+1FDD  ῝ GREEK DASIA AND VARIA
-- U+1FDE  ῞ GREEK DASIA AND OXIA
-- U+1FDF  ῟ GREEK DASIA AND PERISPOMENI
-- U+1FE0 ῠ GREEK SMALL LETTER UPSILON WITH VRACHY
-- U+1FE1 ῡ GREEK SMALL LETTER UPSILON WITH MACRON
ῢ : Token υ′ lower -- U+1FE2  ῢ GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND VARIA
ῢ = with-accent-diaeresis grave
ΰ : Token υ′ lower -- U+1FE3  ΰ GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND OXIA
ΰ = with-accent-diaeresis acute
ῤ : Token ρ′ lower -- U+1FE4 ῤ GREEK SMALL LETTER RHO WITH PSILI
ῤ = with-breathing smooth
ῥ : Token ρ′ lower -- U+1FE5 ῥ GREEK SMALL LETTER RHO WITH DASIA
ῥ = with-breathing rough
ῦ : Token υ′ lower -- U+1FE6  ῦ GREEK SMALL LETTER UPSILON WITH PERISPOMENI
ῦ = with-accent circumflex
ῧ : Token υ′ lower -- U+1FE7  ῧ GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND PERISPOMENI
ῧ = with-accent-diaeresis circumflex
-- U+1FE8 Ῠ GREEK CAPITAL LETTER UPSILON WITH VRACHY
-- U+1FE9 Ῡ GREEK CAPITAL LETTER UPSILON WITH MACRON
Ὺ : Token υ′ upper -- U+1FEA  Ὺ GREEK CAPITAL LETTER UPSILON WITH VARIA
Ὺ = with-accent grave
Ύ : Token υ′ upper -- U+1FEB  Ύ GREEK CAPITAL LETTER UPSILON WITH OXIA
Ύ = with-accent acute
Ῥ : Token ρ′ upper -- U+1FEC Ῥ GREEK CAPITAL LETTER RHO WITH DASIA
Ῥ = with-breathing rough
-- U+1FED  ῭ GREEK DIALYTIKA AND VARIA
-- U+1FEE  ΅ GREEK DIALYTIKA AND OXIA
-- U+1FEF  ` GREEK VARIA
-- U+1FF0
-- U+1FF1
ῲ : Token ω′ lower -- U+1FF2  ῲ GREEK SMALL LETTER OMEGA WITH VARIA AND YPOGEGRAMMENI
ῲ = with-accent-iota grave
ῳ : Token ω′ lower -- U+1FF3  ῳ GREEK SMALL LETTER OMEGA WITH YPOGEGRAMMENI
ῳ = with-iota
ῴ : Token ω′ lower -- U+1FF4  ῴ GREEK SMALL LETTER OMEGA WITH OXIA AND YPOGEGRAMMENI
ῴ = with-accent-iota acute
-- U+1FF5
ῶ : Token ω′ lower -- U+1FF6  ῶ GREEK SMALL LETTER OMEGA WITH PERISPOMENI
ῶ = with-accent circumflex
ῷ : Token ω′ lower -- U+1FF7  ῷ GREEK SMALL LETTER OMEGA WITH PERISPOMENI AND YPOGEGRAMMENI
ῷ = with-accent-iota circumflex
Ὸ : Token ο′ upper -- U+1FF8  Ὸ GREEK CAPITAL LETTER OMICRON WITH VARIA
Ὸ = with-accent grave
Ό : Token ο′ upper -- U+1FF9  Ό GREEK CAPITAL LETTER OMICRON WITH OXIA
Ό = with-accent acute
Ὼ : Token ω′ upper -- U+1FFA  Ὼ GREEK CAPITAL LETTER OMEGA WITH VARIA
Ὼ = with-accent grave
Ώ : Token ω′ upper -- U+1FFB  Ώ GREEK CAPITAL LETTER OMEGA WITH OXIA
Ώ = with-accent acute
ῼ : Token ω′ upper -- U+1FFC  ῼ GREEK CAPITAL LETTER OMEGA WITH PROSGEGRAMMENI
ῼ = with-iota
-- U+1FFD  ´ GREEK OXIA
-- U+1FFE  ῾ GREEK DASIA
-- U+1FFF
