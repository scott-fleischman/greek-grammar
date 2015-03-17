module Text.Greek.Script.Unicode where

open import Text.Greek.Script

-- U+0390 - U+03CE
ΐ : Token -- U+0390 GREEK SMALL LETTER IOTA WITH DIALYTIKA AND TONOS
ΐ = with-accent-diaeresis ι′ lower acute
Α : Token -- U+0391 GREEK CAPITAL LETTER ALPHA
Α = unmarked α′ upper
Β : Token -- U+0392 GREEK CAPITAL LETTER BETA
Β = unmarked β′ upper
Γ : Token -- U+0393 GREEK CAPITAL LETTER GAMMA
Γ = unmarked γ′ upper
Δ : Token -- U+0394 GREEK CAPITAL LETTER DELTA
Δ = unmarked δ′ upper
Ε : Token -- U+0395 GREEK CAPITAL LETTER EPSILON
Ε = unmarked ε′ upper
Ζ : Token -- U+0396 GREEK CAPITAL LETTER ZETA
Ζ = unmarked ζ′ upper
Η : Token -- U+0397 GREEK CAPITAL LETTER ETA
Η = unmarked η′ upper
Θ : Token -- U+0398 GREEK CAPITAL LETTER THETA
Θ = unmarked θ′ upper
Ι : Token -- U+0399 GREEK CAPITAL LETTER IOTA
Ι = unmarked ι′ upper
Κ : Token -- U+039A GREEK CAPITAL LETTER KAPPA
Κ = unmarked κ′ upper
Λ : Token -- U+039B GREEK CAPITAL LETTER LAMDA
Λ = unmarked λ′ upper
Μ : Token -- U+039C GREEK CAPITAL LETTER MU
Μ = unmarked μ′ upper
Ν : Token -- U+039D GREEK CAPITAL LETTER NU
Ν = unmarked ν′ upper
Ξ : Token -- U+039E GREEK CAPITAL LETTER XI
Ξ = unmarked ξ′ upper
Ο : Token -- U+039F GREEK CAPITAL LETTER OMICRON
Ο = unmarked ο′ upper
Π : Token -- U+03A0 GREEK CAPITAL LETTER PI
Π = unmarked π′ upper
Ρ : Token -- U+03A1 GREEK CAPITAL LETTER RHO
Ρ = unmarked ρ′ upper
                  -- U+03A2 <reserved>
Σ : Token -- U+03A3 GREEK CAPITAL LETTER SIGMA
Σ = unmarked σ′ upper
Τ : Token -- U+03A4 GREEK CAPITAL LETTER TAU
Τ = unmarked τ′ upper
Υ : Token -- U+03A5 GREEK CAPITAL LETTER UPSILON
Υ = unmarked υ′ upper
Φ : Token -- U+03A6 GREEK CAPITAL LETTER PHI
Φ = unmarked φ′ upper
Χ : Token -- U+03A7 GREEK CAPITAL LETTER CHI
Χ = unmarked χ′ upper
Ψ : Token -- U+03A8 GREEK CAPITAL LETTER PSI
Ψ = unmarked ψ′ upper
Ω : Token -- U+03A9 GREEK CAPITAL LETTER OMEGA
Ω = unmarked ω′ upper
Ϊ : Token -- U+03AA GREEK CAPITAL LETTER IOTA WITH DIALYTIKA
Ϊ = with-diaeresis ι′ upper
Ϋ : Token -- U+03AB GREEK CAPITAL LETTER UPSILON WITH DIALYTIKA
Ϋ = with-diaeresis υ′ upper
ά : Token -- U+03AC GREEK SMALL LETTER ALPHA WITH TONOS
ά = with-accent α′ lower acute
έ : Token -- U+03AD GREEK SMALL LETTER EPSILON WITH TONOS
έ = with-accent ε′ lower acute
ή : Token -- U+03AE GREEK SMALL LETTER ETA WITH TONOS
ή = with-accent η′ lower acute
ί : Token -- U+03AF GREEK SMALL LETTER IOTA WITH TONOS
ί = with-accent ι′ lower acute
ΰ : Token -- U+03B0 GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND TONOS
ΰ = with-accent-diaeresis υ′ lower acute
α : Token -- U+03B1 GREEK SMALL LETTER ALPHA
α = unmarked α′ lower
β : Token -- U+03B2 GREEK SMALL LETTER BETA
β = unmarked β′ lower
γ : Token -- U+03B3 GREEK SMALL LETTER GAMMA
γ = unmarked γ′ lower
δ : Token -- U+03B4 GREEK SMALL LETTER DELTA
δ = unmarked δ′ lower
ε : Token -- U+03B5 GREEK SMALL LETTER EPSILON
ε = unmarked ε′ lower
ζ : Token -- U+03B6 GREEK SMALL LETTER ZETA
ζ = unmarked ζ′ lower
η : Token -- U+03B7 GREEK SMALL LETTER ETA
η = unmarked η′ lower
θ : Token -- U+03B8 GREEK SMALL LETTER THETA
θ = unmarked θ′ lower
ι : Token -- U+03B9 GREEK SMALL LETTER IOTA
ι = unmarked ι′ lower
κ : Token -- U+03BA GREEK SMALL LETTER KAPPA
κ = unmarked κ′ lower
∙λ : Token -- U+03BB GREEK SMALL LETTER LAMDA
∙λ = unmarked λ′ lower
μ : Token -- U+03BC GREEK SMALL LETTER MU
μ = unmarked μ′ lower
ν : Token -- U+03BD GREEK SMALL LETTER NU
ν = unmarked ν′ lower
ξ : Token -- U+03BE GREEK SMALL LETTER XI
ξ = unmarked ξ′ lower
ο : Token -- U+03BF GREEK SMALL LETTER OMICRON
ο = unmarked ο′ lower
π : Token -- U+03C0 GREEK SMALL LETTER PI
π = unmarked π′ lower
ρ : Token -- U+03C1 GREEK SMALL LETTER RHO
ρ = unmarked ρ′ lower
ς : Token -- U+03C2 GREEK SMALL LETTER FINAL SIGMA
ς = final σ′ lower
σ : Token -- U+03C3 GREEK SMALL LETTER SIGMA
σ = unmarked σ′ lower
τ : Token -- U+03C4 GREEK SMALL LETTER TAU
τ = unmarked τ′ lower
υ : Token -- U+03C5 GREEK SMALL LETTER UPSILON
υ = unmarked υ′ lower
φ : Token -- U+03C6 GREEK SMALL LETTER PHI
φ = unmarked φ′ lower
χ : Token -- U+03C7 GREEK SMALL LETTER CHI
χ = unmarked χ′ lower
ψ : Token -- U+03C8 GREEK SMALL LETTER PSI
ψ = unmarked ψ′ lower
ω : Token -- U+03C9 GREEK SMALL LETTER OMEGA
ω = unmarked ω′ lower
ϊ : Token -- U+03CA GREEK SMALL LETTER IOTA WITH DIALYTIKA
ϊ = with-diaeresis ι′ lower
ϋ : Token -- U+03CB GREEK SMALL LETTER UPSILON WITH DIALYTIKA
ϋ = with-diaeresis υ′ lower
ό : Token -- U+03CC GREEK SMALL LETTER OMICRON WITH TONOS
ό = with-accent ο′ lower acute
ύ : Token -- U+03CD GREEK SMALL LETTER UPSILON WITH TONOS
ύ = with-accent υ′ lower acute
ώ : Token -- U+03CE GREEK SMALL LETTER OMEGA WITH TONOS
ώ = with-accent ω′ lower acute

-- U+1F00 - U+1FFF
ἀ : Token -- U+1F00  ἀ GREEK SMALL LETTER ALPHA WITH PSILI
ἀ = with-breathing α′ lower smooth
ἁ : Token -- U+1F01  ἁ GREEK SMALL LETTER ALPHA WITH DASIA
ἁ = with-breathing α′ lower rough
ἂ : Token -- U+1F02  ἂ GREEK SMALL LETTER ALPHA WITH PSILI AND VARIA
ἂ = with-accent-breathing α′ lower grave smooth
ἃ : Token -- U+1F03  ἃ GREEK SMALL LETTER ALPHA WITH DASIA AND VARIA
ἃ = with-accent-breathing α′ lower grave rough
ἄ : Token -- U+1F04  ἄ GREEK SMALL LETTER ALPHA WITH PSILI AND OXIA
ἄ = with-accent-breathing α′ lower acute smooth
ἅ : Token -- U+1F05  ἅ GREEK SMALL LETTER ALPHA WITH DASIA AND OXIA
ἅ = with-accent-breathing α′ lower acute rough
ἆ : Token -- U+1F06  ἆ GREEK SMALL LETTER ALPHA WITH PSILI AND PERISPOMENI
ἆ = with-accent-breathing α′ lower circumflex smooth
ἇ : Token -- U+1F07  ἇ GREEK SMALL LETTER ALPHA WITH DASIA AND PERISPOMENI
ἇ = with-accent-breathing α′ lower circumflex rough
Ἀ : Token -- U+1F08  Ἀ GREEK CAPITAL LETTER ALPHA WITH PSILI
Ἀ = with-breathing α′ upper smooth
Ἁ : Token -- U+1F09  Ἁ GREEK CAPITAL LETTER ALPHA WITH DASIA
Ἁ = with-breathing α′ upper rough
Ἂ : Token -- U+1F0A  Ἂ GREEK CAPITAL LETTER ALPHA WITH PSILI AND VARIA
Ἂ = with-accent-breathing α′ upper grave smooth
Ἃ : Token -- U+1F0B  Ἃ GREEK CAPITAL LETTER ALPHA WITH DASIA AND VARIA
Ἃ = with-accent-breathing α′ upper grave rough
Ἄ : Token -- U+1F0C  Ἄ GREEK CAPITAL LETTER ALPHA WITH PSILI AND OXIA
Ἄ = with-accent-breathing α′ upper acute smooth
Ἅ : Token -- U+1F0D  Ἅ GREEK CAPITAL LETTER ALPHA WITH DASIA AND OXIA
Ἅ = with-accent-breathing α′ upper acute rough
Ἆ : Token -- U+1F0E  Ἆ GREEK CAPITAL LETTER ALPHA WITH PSILI AND PERISPOMENI
Ἆ = with-accent-breathing α′ upper circumflex smooth
Ἇ : Token -- U+1F0F  Ἇ GREEK CAPITAL LETTER ALPHA WITH DASIA AND PERISPOMENI
Ἇ = with-accent-breathing α′ upper circumflex rough
ἐ : Token -- U+1F10  ἐ GREEK SMALL LETTER EPSILON WITH PSILI
ἐ = with-breathing ε′ lower smooth
ἑ : Token -- U+1F11  ἑ GREEK SMALL LETTER EPSILON WITH DASIA
ἑ = with-breathing ε′ lower rough
ἒ : Token -- U+1F12  ἒ GREEK SMALL LETTER EPSILON WITH PSILI AND VARIA
ἒ = with-accent-breathing ε′ lower grave smooth
ἓ : Token -- U+1F13  ἓ GREEK SMALL LETTER EPSILON WITH DASIA AND VARIA
ἓ = with-accent-breathing ε′ lower grave rough
ἔ : Token -- U+1F14  ἔ GREEK SMALL LETTER EPSILON WITH PSILI AND OXIA
ἔ = with-accent-breathing ε′ lower acute smooth
ἕ : Token -- U+1F15  ἕ GREEK SMALL LETTER EPSILON WITH DASIA AND OXIA
ἕ = with-accent-breathing ε′ lower acute rough
-- U+1F16
-- U+1F17
Ἐ : Token -- U+1F18  Ἐ GREEK CAPITAL LETTER EPSILON WITH PSILI
Ἐ = with-breathing ε′ upper smooth
Ἑ : Token -- U+1F19  Ἑ GREEK CAPITAL LETTER EPSILON WITH DASIA
Ἑ = with-breathing ε′ upper rough
Ἒ : Token -- U+1F1A  Ἒ GREEK CAPITAL LETTER EPSILON WITH PSILI AND VARIA
Ἒ = with-accent-breathing ε′ upper grave smooth
Ἓ : Token -- U+1F1B  Ἓ GREEK CAPITAL LETTER EPSILON WITH DASIA AND VARIA
Ἓ = with-accent-breathing ε′ upper grave rough
Ἔ : Token -- U+1F1C  Ἔ GREEK CAPITAL LETTER EPSILON WITH PSILI AND OXIA
Ἔ = with-accent-breathing ε′ upper acute smooth
Ἕ : Token -- U+1F1D  Ἕ GREEK CAPITAL LETTER EPSILON WITH DASIA AND OXIA
Ἕ = with-accent-breathing ε′ upper acute rough
-- U+1F1E
-- U+1F1F
ἠ : Token -- U+1F20  ἠ GREEK SMALL LETTER ETA WITH PSILI
ἠ = with-breathing η′ lower smooth
ἡ : Token -- U+1F21  ἡ GREEK SMALL LETTER ETA WITH DASIA
ἡ = with-breathing η′ lower rough
ἢ : Token -- U+1F22  ἢ GREEK SMALL LETTER ETA WITH PSILI AND VARIA
ἢ = with-accent-breathing η′ lower grave smooth
ἣ : Token -- U+1F23  ἣ GREEK SMALL LETTER ETA WITH DASIA AND VARIA
ἣ = with-accent-breathing η′ lower grave rough
ἤ : Token -- U+1F24  ἤ GREEK SMALL LETTER ETA WITH PSILI AND OXIA
ἤ = with-accent-breathing η′ lower acute smooth
ἥ : Token -- U+1F25  ἥ GREEK SMALL LETTER ETA WITH DASIA AND OXIA
ἥ = with-accent-breathing η′ lower acute rough
ἦ : Token -- U+1F26  ἦ GREEK SMALL LETTER ETA WITH PSILI AND PERISPOMENI
ἦ = with-accent-breathing η′ lower circumflex smooth
ἧ : Token -- U+1F27  ἧ GREEK SMALL LETTER ETA WITH DASIA AND PERISPOMENI
ἧ = with-accent-breathing η′ lower circumflex rough
Ἠ : Token -- U+1F28  Ἠ GREEK CAPITAL LETTER ETA WITH PSILI
Ἠ = with-breathing η′ upper smooth
Ἡ : Token -- U+1F29  Ἡ GREEK CAPITAL LETTER ETA WITH DASIA
Ἡ = with-breathing η′ upper rough
Ἢ : Token -- U+1F2A  Ἢ GREEK CAPITAL LETTER ETA WITH PSILI AND VARIA
Ἢ = with-accent-breathing η′ upper grave smooth
Ἣ : Token -- U+1F2B  Ἣ GREEK CAPITAL LETTER ETA WITH DASIA AND VARIA
Ἣ = with-accent-breathing η′ upper grave rough
Ἤ : Token -- U+1F2C  Ἤ GREEK CAPITAL LETTER ETA WITH PSILI AND OXIA
Ἤ = with-accent-breathing η′ upper acute smooth
Ἥ : Token -- U+1F2D  Ἥ GREEK CAPITAL LETTER ETA WITH DASIA AND OXIA
Ἥ = with-accent-breathing η′ upper acute rough
Ἦ : Token -- U+1F2E  Ἦ GREEK CAPITAL LETTER ETA WITH PSILI AND PERISPOMENI
Ἦ = with-accent-breathing η′ upper circumflex smooth
Ἧ : Token -- U+1F2F  Ἧ GREEK CAPITAL LETTER ETA WITH DASIA AND PERISPOMENI
Ἧ = with-accent-breathing η′ upper circumflex rough
ἰ : Token -- U+1F30  ἰ GREEK SMALL LETTER IOTA WITH PSILI
ἰ = with-breathing ι′ lower smooth
ἱ : Token -- U+1F31  ἱ GREEK SMALL LETTER IOTA WITH DASIA
ἱ = with-breathing ι′ lower rough
ἲ : Token -- U+1F32  ἲ GREEK SMALL LETTER IOTA WITH PSILI AND VARIA
ἲ = with-accent-breathing ι′ lower grave smooth
ἳ : Token -- U+1F33  ἳ GREEK SMALL LETTER IOTA WITH DASIA AND VARIA
ἳ = with-accent-breathing ι′ lower grave rough
ἴ : Token -- U+1F34  ἴ GREEK SMALL LETTER IOTA WITH PSILI AND OXIA
ἴ = with-accent-breathing ι′ lower acute smooth
ἵ : Token -- U+1F35  ἵ GREEK SMALL LETTER IOTA WITH DASIA AND OXIA
ἵ = with-accent-breathing ι′ lower acute rough
ἶ : Token -- U+1F36  ἶ GREEK SMALL LETTER IOTA WITH PSILI AND PERISPOMENI
ἶ = with-accent-breathing ι′ lower circumflex smooth
ἷ : Token -- U+1F37  ἷ GREEK SMALL LETTER IOTA WITH DASIA AND PERISPOMENI
ἷ = with-accent-breathing ι′ lower circumflex rough
Ἰ : Token -- U+1F38  Ἰ GREEK CAPITAL LETTER IOTA WITH PSILI
Ἰ = with-breathing ι′ upper smooth
Ἱ : Token -- U+1F39  Ἱ GREEK CAPITAL LETTER IOTA WITH DASIA
Ἱ = with-breathing ι′ upper rough
Ἲ : Token -- U+1F3A  Ἲ GREEK CAPITAL LETTER IOTA WITH PSILI AND VARIA
Ἲ = with-accent-breathing ι′ upper grave smooth
Ἳ : Token -- U+1F3B  Ἳ GREEK CAPITAL LETTER IOTA WITH DASIA AND VARIA
Ἳ = with-accent-breathing ι′ upper grave rough
Ἴ : Token -- U+1F3C  Ἴ GREEK CAPITAL LETTER IOTA WITH PSILI AND OXIA
Ἴ = with-accent-breathing ι′ upper acute smooth
Ἵ : Token -- U+1F3D  Ἵ GREEK CAPITAL LETTER IOTA WITH DASIA AND OXIA
Ἵ = with-accent-breathing ι′ upper acute rough
Ἶ : Token -- U+1F3E  Ἶ GREEK CAPITAL LETTER IOTA WITH PSILI AND PERISPOMENI
Ἶ = with-accent-breathing ι′ upper circumflex smooth
Ἷ : Token -- U+1F3F  Ἷ GREEK CAPITAL LETTER IOTA WITH DASIA AND PERISPOMENI
Ἷ = with-accent-breathing ι′ upper circumflex rough
ὀ : Token -- U+1F40  ὀ GREEK SMALL LETTER OMICRON WITH PSILI
ὀ = with-breathing ο′ lower smooth
ὁ : Token -- U+1F41  ὁ GREEK SMALL LETTER OMICRON WITH DASIA
ὁ = with-breathing ο′ lower rough
ὂ : Token -- U+1F42  ὂ GREEK SMALL LETTER OMICRON WITH PSILI AND VARIA
ὂ = with-accent-breathing ο′ lower grave smooth
ὃ : Token -- U+1F43  ὃ GREEK SMALL LETTER OMICRON WITH DASIA AND VARIA
ὃ = with-accent-breathing ο′ lower grave rough
ὄ : Token -- U+1F44  ὄ GREEK SMALL LETTER OMICRON WITH PSILI AND OXIA
ὄ = with-accent-breathing ο′ lower acute smooth
ὅ : Token -- U+1F45  ὅ GREEK SMALL LETTER OMICRON WITH DASIA AND OXIA
ὅ = with-accent-breathing ο′ lower acute rough
-- U+1F46
-- U+1F47
Ὀ : Token -- U+1F48  Ὀ GREEK CAPITAL LETTER OMICRON WITH PSILI
Ὀ = with-breathing ο′ upper smooth
Ὁ : Token -- U+1F49  Ὁ GREEK CAPITAL LETTER OMICRON WITH DASIA
Ὁ = with-breathing ο′ upper rough
Ὂ : Token -- U+1F4A  Ὂ GREEK CAPITAL LETTER OMICRON WITH PSILI AND VARIA
Ὂ = with-accent-breathing ο′ upper grave smooth
Ὃ : Token -- U+1F4B  Ὃ GREEK CAPITAL LETTER OMICRON WITH DASIA AND VARIA
Ὃ = with-accent-breathing ο′ upper grave rough
Ὄ : Token -- U+1F4C  Ὄ GREEK CAPITAL LETTER OMICRON WITH PSILI AND OXIA
Ὄ = with-accent-breathing ο′ upper acute smooth
Ὅ : Token -- U+1F4D  Ὅ GREEK CAPITAL LETTER OMICRON WITH DASIA AND OXIA
Ὅ = with-accent-breathing ο′ upper acute rough
-- U+1F4E
-- U+1F4F
ὐ : Token -- U+1F50  ὐ GREEK SMALL LETTER UPSILON WITH PSILI
ὐ = with-breathing υ′ lower smooth
ὑ : Token -- U+1F51  ὑ GREEK SMALL LETTER UPSILON WITH DASIA
ὑ = with-breathing υ′ lower rough
ὒ : Token -- U+1F52  ὒ GREEK SMALL LETTER UPSILON WITH PSILI AND VARIA
ὒ = with-accent-breathing υ′ lower grave smooth
ὓ : Token -- U+1F53  ὓ GREEK SMALL LETTER UPSILON WITH DASIA AND VARIA
ὓ = with-accent-breathing υ′ lower grave rough
ὔ : Token -- U+1F54  ὔ GREEK SMALL LETTER UPSILON WITH PSILI AND OXIA
ὔ = with-accent-breathing υ′ lower acute smooth
ὕ : Token -- U+1F55  ὕ GREEK SMALL LETTER UPSILON WITH DASIA AND OXIA
ὕ = with-accent-breathing υ′ lower acute rough
ὖ : Token -- U+1F56  ὖ GREEK SMALL LETTER UPSILON WITH PSILI AND PERISPOMENI
ὖ = with-accent-breathing υ′ lower circumflex smooth
ὗ : Token -- U+1F57  ὗ GREEK SMALL LETTER UPSILON WITH DASIA AND PERISPOMENI
ὗ = with-accent-breathing υ′ lower circumflex rough
-- U+1F58
Ὑ : Token -- U+1F59  Ὑ GREEK CAPITAL LETTER UPSILON WITH DASIA
Ὑ = with-breathing υ′ upper rough
-- U+1F5A
Ὓ : Token -- U+1F5B  Ὓ GREEK CAPITAL LETTER UPSILON WITH DASIA AND VARIA
Ὓ = with-accent-breathing υ′ upper grave rough
-- U+1F5C
Ὕ : Token -- U+1F5D  Ὕ GREEK CAPITAL LETTER UPSILON WITH DASIA AND OXIA
Ὕ = with-accent-breathing υ′ upper acute rough
-- U+1F5E
Ὗ : Token -- U+1F5F  Ὗ GREEK CAPITAL LETTER UPSILON WITH DASIA AND PERISPOMENI
Ὗ = with-accent-breathing υ′ upper circumflex rough
ὠ : Token -- U+1F60  ὠ GREEK SMALL LETTER OMEGA WITH PSILI
ὠ = with-breathing ω′ lower smooth
ὡ : Token -- U+1F61  ὡ GREEK SMALL LETTER OMEGA WITH DASIA
ὡ = with-breathing ω′ lower rough
ὢ : Token -- U+1F62  ὢ GREEK SMALL LETTER OMEGA WITH PSILI AND VARIA
ὢ = with-accent-breathing ω′ lower grave smooth
ὣ : Token -- U+1F63  ὣ GREEK SMALL LETTER OMEGA WITH DASIA AND VARIA
ὣ = with-accent-breathing ω′ lower grave rough
ὤ : Token -- U+1F64  ὤ GREEK SMALL LETTER OMEGA WITH PSILI AND OXIA
ὤ = with-accent-breathing ω′ lower acute smooth
ὥ : Token -- U+1F65  ὥ GREEK SMALL LETTER OMEGA WITH DASIA AND OXIA
ὥ = with-accent-breathing ω′ lower acute rough
ὦ : Token -- U+1F66  ὦ GREEK SMALL LETTER OMEGA WITH PSILI AND PERISPOMENI
ὦ = with-accent-breathing ω′ lower circumflex smooth
ὧ : Token -- U+1F67  ὧ GREEK SMALL LETTER OMEGA WITH DASIA AND PERISPOMENI
ὧ = with-accent-breathing ω′ lower circumflex rough
Ὠ : Token -- U+1F68  Ὠ GREEK CAPITAL LETTER OMEGA WITH PSILI
Ὠ = with-breathing ω′ upper smooth
Ὡ : Token -- U+1F69  Ὡ GREEK CAPITAL LETTER OMEGA WITH DASIA
Ὡ = with-breathing ω′ upper rough
Ὢ : Token -- U+1F6A  Ὢ GREEK CAPITAL LETTER OMEGA WITH PSILI AND VARIA
Ὢ = with-accent-breathing ω′ upper grave smooth
Ὣ : Token -- U+1F6B  Ὣ GREEK CAPITAL LETTER OMEGA WITH DASIA AND VARIA
Ὣ = with-accent-breathing ω′ upper grave rough
Ὤ : Token -- U+1F6C  Ὤ GREEK CAPITAL LETTER OMEGA WITH PSILI AND OXIA
Ὤ = with-accent-breathing ω′ upper acute smooth
Ὥ : Token -- U+1F6D  Ὥ GREEK CAPITAL LETTER OMEGA WITH DASIA AND OXIA
Ὥ = with-accent-breathing ω′ upper acute rough
Ὦ : Token -- U+1F6E  Ὦ GREEK CAPITAL LETTER OMEGA WITH PSILI AND PERISPOMENI
Ὦ = with-accent-breathing ω′ upper circumflex smooth
Ὧ : Token -- U+1F6F  Ὧ GREEK CAPITAL LETTER OMEGA WITH DASIA AND PERISPOMENI
Ὧ = with-accent-breathing ω′ upper circumflex rough
ὰ : Token -- U+1F70  ὰ GREEK SMALL LETTER ALPHA WITH VARIA
ὰ = with-accent α′ lower grave
ά : Token -- U+1F71  ά GREEK SMALL LETTER ALPHA WITH OXIA
ά = with-accent α′ lower acute
ὲ : Token -- U+1F72  ὲ GREEK SMALL LETTER EPSILON WITH VARIA
ὲ = with-accent ε′ lower grave
έ : Token -- U+1F73  έ GREEK SMALL LETTER EPSILON WITH OXIA
έ = with-accent ε′ lower acute
ὴ : Token -- U+1F74  ὴ GREEK SMALL LETTER ETA WITH VARIA
ὴ = with-accent η′ lower grave
ή : Token -- U+1F75  ή GREEK SMALL LETTER ETA WITH OXIA
ή = with-accent η′ lower acute
ὶ : Token -- U+1F76  ὶ GREEK SMALL LETTER IOTA WITH VARIA
ὶ = with-accent ι′ lower grave
ί : Token -- U+1F77  ί GREEK SMALL LETTER IOTA WITH OXIA
ί = with-accent ι′ lower acute
ὸ : Token -- U+1F78  ὸ GREEK SMALL LETTER OMICRON WITH VARIA
ὸ = with-accent ο′ lower grave
ό : Token -- U+1F79  ό GREEK SMALL LETTER OMICRON WITH OXIA
ό = with-accent ο′ lower acute
ὺ : Token -- U+1F7A  ὺ GREEK SMALL LETTER UPSILON WITH VARIA
ὺ = with-accent υ′ lower grave
ύ : Token -- U+1F7B  ύ GREEK SMALL LETTER UPSILON WITH OXIA
ύ = with-accent υ′ lower acute
ὼ : Token -- U+1F7C  ὼ GREEK SMALL LETTER OMEGA WITH VARIA
ὼ = with-accent ω′ lower grave
ώ : Token -- U+1F7D  ώ GREEK SMALL LETTER OMEGA WITH OXIA
ώ = with-accent ω′ lower acute
-- U+1F7E
-- U+1F7F
ᾀ : Token -- U+1F80  ᾀ GREEK SMALL LETTER ALPHA WITH PSILI AND YPOGEGRAMMENI
ᾀ = with-breathing-iota α′ lower smooth
ᾁ : Token -- U+1F81  ᾁ GREEK SMALL LETTER ALPHA WITH DASIA AND YPOGEGRAMMENI
ᾁ = with-breathing-iota α′ lower rough
ᾂ : Token -- U+1F82  ᾂ GREEK SMALL LETTER ALPHA WITH PSILI AND VARIA AND YPOGEGRAMMENI
ᾂ = with-accent-breathing-iota α′ lower grave smooth
ᾃ : Token -- U+1F83  ᾃ GREEK SMALL LETTER ALPHA WITH DASIA AND VARIA AND YPOGEGRAMMENI
ᾃ = with-accent-breathing α′ lower grave rough
ᾄ : Token -- U+1F84  ᾄ GREEK SMALL LETTER ALPHA WITH PSILI AND OXIA AND YPOGEGRAMMENI
ᾄ = with-accent-breathing-iota α′ lower acute smooth
ᾅ : Token -- U+1F85  ᾅ GREEK SMALL LETTER ALPHA WITH DASIA AND OXIA AND YPOGEGRAMMENI
ᾅ = with-accent-breathing-iota α′ lower acute rough
ᾆ : Token -- U+1F86  ᾆ GREEK SMALL LETTER ALPHA WITH PSILI AND PERISPOMENI AND YPOGEGRAMMENI
ᾆ = with-accent-breathing-iota α′ lower circumflex smooth
ᾇ : Token -- U+1F87  ᾇ GREEK SMALL LETTER ALPHA WITH DASIA AND PERISPOMENI AND YPOGEGRAMMENI
ᾇ = with-accent-breathing-iota α′ lower circumflex rough
ᾈ : Token -- U+1F88  ᾈ GREEK CAPITAL LETTER ALPHA WITH PSILI AND PROSGEGRAMMENI
ᾈ = with-breathing-iota α′ upper smooth
ᾉ : Token -- U+1F89  ᾉ GREEK CAPITAL LETTER ALPHA WITH DASIA AND PROSGEGRAMMENI
ᾉ = with-breathing-iota α′ upper rough
ᾊ : Token -- U+1F8A  ᾊ GREEK CAPITAL LETTER ALPHA WITH PSILI AND VARIA AND PROSGEGRAMMENI
ᾊ = with-accent-breathing-iota α′ upper grave smooth
ᾋ : Token -- U+1F8B  ᾋ GREEK CAPITAL LETTER ALPHA WITH DASIA AND VARIA AND PROSGEGRAMMENI
ᾋ = with-accent-breathing-iota α′ upper grave rough
ᾌ : Token -- U+1F8C  ᾌ GREEK CAPITAL LETTER ALPHA WITH PSILI AND OXIA AND PROSGEGRAMMENI
ᾌ = with-accent-breathing-iota α′ upper acute smooth
ᾍ : Token -- U+1F8D  ᾍ GREEK CAPITAL LETTER ALPHA WITH DASIA AND OXIA AND PROSGEGRAMMENI
ᾍ = with-accent-breathing-iota α′ upper acute rough
ᾎ : Token -- U+1F8E  ᾎ GREEK CAPITAL LETTER ALPHA WITH PSILI AND PERISPOMENI AND PROSGEGRAMMENI
ᾎ = with-accent-breathing-iota α′ upper circumflex smooth
ᾏ : Token -- U+1F8F  ᾏ GREEK CAPITAL LETTER ALPHA WITH DASIA AND PERISPOMENI AND PROSGEGRAMMENI
ᾏ = with-accent-breathing-iota α′ upper circumflex rough
ᾐ : Token -- U+1F90  ᾐ GREEK SMALL LETTER ETA WITH PSILI AND YPOGEGRAMMENI
ᾐ = with-breathing-iota η′ lower smooth
ᾑ : Token -- U+1F91  ᾑ GREEK SMALL LETTER ETA WITH DASIA AND YPOGEGRAMMENI
ᾑ = with-breathing-iota η′ lower rough
ᾒ : Token -- U+1F92  ᾒ GREEK SMALL LETTER ETA WITH PSILI AND VARIA AND YPOGEGRAMMENI
ᾒ = with-accent-breathing-iota η′ lower grave smooth
ᾓ : Token -- U+1F93  ᾓ GREEK SMALL LETTER ETA WITH DASIA AND VARIA AND YPOGEGRAMMENI
ᾓ = with-accent-breathing-iota η′ lower grave rough
ᾔ : Token -- U+1F94  ᾔ GREEK SMALL LETTER ETA WITH PSILI AND OXIA AND YPOGEGRAMMENI
ᾔ = with-accent-breathing-iota η′ lower acute smooth
ᾕ : Token -- U+1F95  ᾕ GREEK SMALL LETTER ETA WITH DASIA AND OXIA AND YPOGEGRAMMENI
ᾕ = with-accent-breathing-iota η′ lower acute rough
ᾖ : Token -- U+1F96  ᾖ GREEK SMALL LETTER ETA WITH PSILI AND PERISPOMENI AND YPOGEGRAMMENI
ᾖ = with-accent-breathing-iota η′ lower circumflex smooth
ᾗ : Token -- U+1F97  ᾗ GREEK SMALL LETTER ETA WITH DASIA AND PERISPOMENI AND YPOGEGRAMMENI
ᾗ = with-accent-breathing-iota η′ lower circumflex rough
ᾘ : Token -- U+1F98  ᾘ GREEK CAPITAL LETTER ETA WITH PSILI AND PROSGEGRAMMENI
ᾘ = with-breathing-iota η′ upper smooth
ᾙ : Token -- U+1F99  ᾙ GREEK CAPITAL LETTER ETA WITH DASIA AND PROSGEGRAMMENI
ᾙ = with-breathing-iota η′ upper rough
ᾚ : Token -- U+1F9A  ᾚ GREEK CAPITAL LETTER ETA WITH PSILI AND VARIA AND PROSGEGRAMMENI
ᾚ = with-accent-breathing-iota η′ upper grave smooth
ᾛ : Token -- U+1F9B  ᾛ GREEK CAPITAL LETTER ETA WITH DASIA AND VARIA AND PROSGEGRAMMENI
ᾛ = with-accent-breathing-iota η′ upper grave rough
ᾜ : Token -- U+1F9C  ᾜ GREEK CAPITAL LETTER ETA WITH PSILI AND OXIA AND PROSGEGRAMMENI
ᾜ = with-accent-breathing-iota η′ upper acute smooth
ᾝ : Token -- U+1F9D  ᾝ GREEK CAPITAL LETTER ETA WITH DASIA AND OXIA AND PROSGEGRAMMENI
ᾝ = with-accent-breathing-iota η′ upper acute rough
ᾞ : Token -- U+1F9E  ᾞ GREEK CAPITAL LETTER ETA WITH PSILI AND PERISPOMENI AND PROSGEGRAMMENI
ᾞ = with-accent-breathing-iota η′ upper circumflex smooth
ᾟ : Token -- U+1F9F  ᾟ GREEK CAPITAL LETTER ETA WITH DASIA AND PERISPOMENI AND PROSGEGRAMMENI
ᾟ = with-accent-breathing-iota η′ upper circumflex rough
ᾠ : Token -- U+1FA0  ᾠ GREEK SMALL LETTER OMEGA WITH PSILI AND YPOGEGRAMMENI
ᾠ = with-breathing-iota ω′ lower smooth
ᾡ : Token -- U+1FA1  ᾡ GREEK SMALL LETTER OMEGA WITH DASIA AND YPOGEGRAMMENI
ᾡ = with-breathing-iota ω′ lower rough
ᾢ : Token -- U+1FA2  ᾢ GREEK SMALL LETTER OMEGA WITH PSILI AND VARIA AND YPOGEGRAMMENI
ᾢ = with-accent-breathing-iota ω′ lower grave smooth
ᾣ : Token -- U+1FA3  ᾣ GREEK SMALL LETTER OMEGA WITH DASIA AND VARIA AND YPOGEGRAMMENI
ᾣ = with-accent-breathing-iota ω′ lower grave rough
ᾤ : Token -- U+1FA4  ᾤ GREEK SMALL LETTER OMEGA WITH PSILI AND OXIA AND YPOGEGRAMMENI
ᾤ = with-accent-breathing-iota ω′ lower acute smooth
ᾥ : Token -- U+1FA5  ᾥ GREEK SMALL LETTER OMEGA WITH DASIA AND OXIA AND YPOGEGRAMMENI
ᾥ = with-accent-breathing-iota ω′ lower acute rough
ᾦ : Token -- U+1FA6  ᾦ GREEK SMALL LETTER OMEGA WITH PSILI AND PERISPOMENI AND YPOGEGRAMMENI
ᾦ = with-accent-breathing-iota ω′ lower circumflex smooth
ᾧ : Token -- U+1FA7  ᾧ GREEK SMALL LETTER OMEGA WITH DASIA AND PERISPOMENI AND YPOGEGRAMMENI
ᾧ = with-accent-breathing-iota ω′ lower circumflex rough
ᾨ : Token -- U+1FA8  ᾨ GREEK CAPITAL LETTER OMEGA WITH PSILI AND PROSGEGRAMMENI
ᾨ = with-breathing-iota ω′ upper smooth
ᾩ : Token -- U+1FA9  ᾩ GREEK CAPITAL LETTER OMEGA WITH DASIA AND PROSGEGRAMMENI
ᾩ = with-breathing-iota ω′ upper rough
ᾪ : Token -- U+1FAA  ᾪ GREEK CAPITAL LETTER OMEGA WITH PSILI AND VARIA AND PROSGEGRAMMENI
ᾪ = with-accent-breathing-iota ω′ upper grave smooth
ᾫ : Token -- U+1FAB  ᾫ GREEK CAPITAL LETTER OMEGA WITH DASIA AND VARIA AND PROSGEGRAMMENI
ᾫ = with-accent-breathing-iota ω′ upper grave rough
ᾬ : Token -- U+1FAC  ᾬ GREEK CAPITAL LETTER OMEGA WITH PSILI AND OXIA AND PROSGEGRAMMENI
ᾬ = with-accent-breathing-iota ω′ upper acute smooth
ᾭ : Token -- U+1FAD  ᾭ GREEK CAPITAL LETTER OMEGA WITH DASIA AND OXIA AND PROSGEGRAMMENI
ᾭ = with-accent-breathing-iota ω′ upper acute rough
ᾮ : Token -- U+1FAE  ᾮ GREEK CAPITAL LETTER OMEGA WITH PSILI AND PERISPOMENI AND PROSGEGRAMMENI
ᾮ = with-accent-breathing-iota ω′ upper circumflex smooth
ᾯ : Token -- U+1FAF  ᾯ GREEK CAPITAL LETTER OMEGA WITH DASIA AND PERISPOMENI AND PROSGEGRAMMENI
ᾯ = with-accent-breathing-iota ω′ upper circumflex rough
-- U+1FB0 ᾰ GREEK SMALL LETTER ALPHA WITH VRACHY
-- U+1FB1 ᾱ GREEK SMALL LETTER ALPHA WITH MACRON
ᾲ : Token -- U+1FB2  ᾲ GREEK SMALL LETTER ALPHA WITH VARIA AND YPOGEGRAMMENI
ᾲ = with-accent-iota α′ lower grave
ᾳ : Token -- U+1FB3  ᾳ GREEK SMALL LETTER ALPHA WITH YPOGEGRAMMENI
ᾳ = with-iota α′ lower
ᾴ : Token -- U+1FB4  ᾴ GREEK SMALL LETTER ALPHA WITH OXIA AND YPOGEGRAMMENI
ᾴ = with-accent-iota α′ lower acute
-- U+1FB5
ᾶ : Token -- U+1FB6  ᾶ GREEK SMALL LETTER ALPHA WITH PERISPOMENI
ᾶ = with-accent-iota α′ lower circumflex
ᾷ : Token -- U+1FB7  ᾷ GREEK SMALL LETTER ALPHA WITH PERISPOMENI AND YPOGEGRAMMENI
ᾷ = with-accent-iota α′ lower circumflex
-- U+1FB8 Ᾰ GREEK CAPITAL LETTER ALPHA WITH VRACHY
-- U+1FB9 Ᾱ GREEK CAPITAL LETTER ALPHA WITH MACRON
Ὰ : Token -- U+1FBA  Ὰ GREEK CAPITAL LETTER ALPHA WITH VARIA
Ὰ = with-accent α′ upper grave
Ά : Token -- U+1FBB  Ά GREEK CAPITAL LETTER ALPHA WITH OXIA
Ά = with-accent α′ upper acute
ᾼ : Token -- U+1FBC  ᾼ GREEK CAPITAL LETTER ALPHA WITH PROSGEGRAMMENI
ᾼ = with-iota α′ upper
-- U+1FBD  ᾽  GREEK KORONIS
-- U+1FBE  ι  GREEK PROSGEGRAMMENI
-- U+1FBF  ᾿  GREEK PSILI
-- U+1FC0  ῀  GREEK PERISPOMENI
-- U+1FC1  ῁  GREEK DIALYTIKA AND PERISPOMENI
ῂ : Token -- U+1FC2  ῂ GREEK SMALL LETTER ETA WITH VARIA AND YPOGEGRAMMENI accent circumflex
ῂ = with-accent-iota η′ lower grave
ῃ : Token -- U+1FC3  ῃ GREEK SMALL LETTER ETA WITH YPOGEGRAMMENI
ῃ = with-iota η′ lower
ῄ : Token -- U+1FC4  ῄ GREEK SMALL LETTER ETA WITH OXIA AND YPOGEGRAMMENI
ῄ = with-accent-iota η′ lower acute
-- U+1FC5
ῆ : Token -- U+1FC6  ῆ GREEK SMALL LETTER ETA WITH PERISPOMENI
ῆ = with-accent η′ lower circumflex
ῇ : Token -- U+1FC7  ῇ GREEK SMALL LETTER ETA WITH PERISPOMENI AND YPOGEGRAMMENI
ῇ = with-accent-iota η′ lower circumflex
Ὲ : Token -- U+1FC8  Ὲ GREEK CAPITAL LETTER EPSILON WITH VARIA
Ὲ = with-accent ε′ upper grave
Έ : Token -- U+1FC9  Έ GREEK CAPITAL LETTER EPSILON WITH OXIA
Έ = with-accent ε′ upper acute
Ὴ : Token -- U+1FCA  Ὴ GREEK CAPITAL LETTER ETA WITH VARIA
Ὴ = with-accent η′ upper grave
Ή : Token -- U+1FCB  Ή GREEK CAPITAL LETTER ETA WITH OXIA
Ή = with-accent η′ upper acute
ῌ : Token -- U+1FCC  ῌ GREEK CAPITAL LETTER ETA WITH PROSGEGRAMMENI
ῌ = with-iota η′ upper
-- U+1FCD  ῍ GREEK PSILI AND VARIA
-- U+1FCE  ῎ GREEK PSILI AND OXIA
-- U+1FCF  ῏ GREEK PSILI AND PERISPOMENI
-- U+1FD0  ῐ GREEK SMALL LETTER IOTA WITH VRACHY
-- U+1FD1  ῑ GREEK SMALL LETTER IOTA WITH MACRON
ῒ : Token -- U+1FD2  ῒ GREEK SMALL LETTER IOTA WITH DIALYTIKA AND VARIA
ῒ = with-accent-diaeresis ι′ lower grave
ΐ : Token -- U+1FD3  ΐ GREEK SMALL LETTER IOTA WITH DIALYTIKA AND OXIA
ΐ = with-accent-diaeresis ι′ lower acute
-- U+1FD4
-- U+1FD5
ῖ : Token -- U+1FD6  ῖ GREEK SMALL LETTER IOTA WITH PERISPOMENI
ῖ = with-accent ι′ lower circumflex
ῗ : Token -- U+1FD7  ῗ GREEK SMALL LETTER IOTA WITH DIALYTIKA AND PERISPOMENI
ῗ = with-accent-diaeresis ι′ lower circumflex
-- U+1FD8 Ῐ GREEK CAPITAL LETTER IOTA WITH VRACHY
-- U+1FD9 Ῑ GREEK CAPITAL LETTER IOTA WITH MACRON
Ὶ : Token -- U+1FDA  Ὶ GREEK CAPITAL LETTER IOTA WITH VARIA
Ὶ = with-accent ι′ upper grave
Ί : Token -- U+1FDB  Ί GREEK CAPITAL LETTER IOTA WITH OXIA
Ί = with-accent ι′ upper acute
-- U+1FDC
-- U+1FDD  ῝ GREEK DASIA AND VARIA
-- U+1FDE  ῞ GREEK DASIA AND OXIA
-- U+1FDF  ῟ GREEK DASIA AND PERISPOMENI
-- U+1FE0 ῠ GREEK SMALL LETTER UPSILON WITH VRACHY
-- U+1FE1 ῡ GREEK SMALL LETTER UPSILON WITH MACRON
ῢ : Token -- U+1FE2  ῢ GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND VARIA
ῢ = with-accent-diaeresis υ′ lower grave
ΰ : Token -- U+1FE3  ΰ GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND OXIA
ΰ = with-accent-diaeresis υ′ lower acute
ῤ : Token -- U+1FE4 ῤ GREEK SMALL LETTER RHO WITH PSILI
ῤ = with-breathing ρ′ lower smooth
ῥ : Token -- U+1FE5 ῥ GREEK SMALL LETTER RHO WITH DASIA
ῥ = with-breathing ρ′ lower rough
ῦ : Token -- U+1FE6  ῦ GREEK SMALL LETTER UPSILON WITH PERISPOMENI
ῦ = with-accent υ′ lower circumflex
ῧ : Token -- U+1FE7  ῧ GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND PERISPOMENI
ῧ = with-accent-diaeresis υ′ lower circumflex
-- U+1FE8 Ῠ GREEK CAPITAL LETTER UPSILON WITH VRACHY
-- U+1FE9 Ῡ GREEK CAPITAL LETTER UPSILON WITH MACRON
Ὺ : Token -- U+1FEA  Ὺ GREEK CAPITAL LETTER UPSILON WITH VARIA
Ὺ = with-accent υ′ upper grave
Ύ : Token -- U+1FEB  Ύ GREEK CAPITAL LETTER UPSILON WITH OXIA
Ύ = with-accent υ′ upper acute
Ῥ : Token -- U+1FEC Ῥ GREEK CAPITAL LETTER RHO WITH DASIA
Ῥ = with-breathing ρ′ upper rough
-- U+1FED  ῭ GREEK DIALYTIKA AND VARIA
-- U+1FEE  ΅ GREEK DIALYTIKA AND OXIA
-- U+1FEF  ` GREEK VARIA
-- U+1FF0
-- U+1FF1
ῲ : Token -- U+1FF2  ῲ GREEK SMALL LETTER OMEGA WITH VARIA AND YPOGEGRAMMENI
ῲ = with-accent-iota ω′ lower grave
ῳ : Token -- U+1FF3  ῳ GREEK SMALL LETTER OMEGA WITH YPOGEGRAMMENI
ῳ = with-iota ω′ lower
ῴ : Token -- U+1FF4  ῴ GREEK SMALL LETTER OMEGA WITH OXIA AND YPOGEGRAMMENI
ῴ = with-accent-iota ω′ lower acute
-- U+1FF5
ῶ : Token -- U+1FF6  ῶ GREEK SMALL LETTER OMEGA WITH PERISPOMENI
ῶ = with-accent ω′ lower circumflex
ῷ : Token -- U+1FF7  ῷ GREEK SMALL LETTER OMEGA WITH PERISPOMENI AND YPOGEGRAMMENI
ῷ = with-accent-iota ω′ lower circumflex
Ὸ : Token -- U+1FF8  Ὸ GREEK CAPITAL LETTER OMICRON WITH VARIA
Ὸ = with-accent ο′ upper grave
Ό : Token -- U+1FF9  Ό GREEK CAPITAL LETTER OMICRON WITH OXIA
Ό = with-accent ο′ upper acute
Ὼ : Token -- U+1FFA  Ὼ GREEK CAPITAL LETTER OMEGA WITH VARIA
Ὼ = with-accent ω′ upper grave
Ώ : Token -- U+1FFB  Ώ GREEK CAPITAL LETTER OMEGA WITH OXIA
Ώ = with-accent ω′ upper acute
ῼ : Token -- U+1FFC  ῼ GREEK CAPITAL LETTER OMEGA WITH PROSGEGRAMMENI
ῼ = with-iota ω′ upper
-- U+1FFD  ´ GREEK OXIA
-- U+1FFE  ῾ GREEK DASIA
-- U+1FFF
