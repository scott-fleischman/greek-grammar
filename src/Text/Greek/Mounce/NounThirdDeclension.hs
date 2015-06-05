{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Text.Greek.Mounce.NounThirdDeclension where

import Text.Greek.Grammar
import Text.Greek.Mounce.Morphology
import Text.Greek.Mounce.Quote

thirdDeclensionNouns :: [Cited NounCategory]
thirdDeclensionNouns = 
  [ mounce §§ ["n-3a(1)"] $
    [nounCategory|
      Stems ending in π
           sg: pl:
      nom: ψ   πες
      gen: πος πων
      dat: πι  ψι
      acc: πα  πας
      voc: ψ   πες
      lemmas:
        Αἰθίοψ κώνωψ λαῖλαψ μώλωψ σκόλοψ
    |]
  , mounce §§ ["n-3a(2)"] $
    [nounCategory| 
      Stems ending in β
           sg: pl:
      nom: ψ   βες
      gen: βος βων
      dat: βι  ψι
      acc: βα  βας
      voc: ψ   βες
      lemmas:
        Ἄραψ λίψ
    |]
  , mounce §§ ["n-3b(1)"] $
    [nounCategory| 
      Stems ending in κ
           sg: pl:
      nom: ξ   κες
      gen: κος κων
      dat: κι  ξι
      acc: κα  κας
      voc: ξ   κες
      lemmas:
        ἀλώπηξ ἄνθραξ δεσμοφύλαξ θώραξ
        κῆρυξ κίλιξ κόραξ ὄρνιξ πίναξ
        πλάξ σάρξ σκώληξ Φῆλιξ Φοῖνιξ
        φοῖνιξ φύλαξ χάραξ χοῖνιξ 
        γυναι -
      exceptions:
        γυναι- nom sg γυνή
        γυναι- voc sg γυνή
    |]
  , mounce §§ ["n-3b(2)"] $
    [nounCategory| 
      Stems ending in γ
           sg: pl:
      nom: ξ   γες
      gen: γος γων
      dat: γι  ξι
      acc: γα  γας
      voc: ξ   γες
      lemmas: 
        αἴξ ἅρπαξ λάρυγξ μάστιξ πτέρυξ
        σάλπιγξ φάραγξ φλόξ
    |]
  , mounce §§ ["n-3b(3)"] $
    [nounCategory| 
      Stems ending in χ
           sg: pl:
      nom: ξ   χες
      gen: χος χων
      dat: χι  ξι
      acc: χα  χας
      voc: ξ   χες
      lemmas:
        θρίξ σαρδόνυξ ψίξ
    |]
  , mounce §§ ["n-3c(1)"] $
    [nounCategory| 
      Stems ending in τ
           sg: pl:
      nom: ς   τες
      gen: τος των
      dat: τι  σι
      acc: τα  τας
      voc: -   τες
      lemmas: 
        ἁγιότης ἁγνότης ἀδελφότης ἀδηλότης ἁδρότης
        αἰσχρότης ἀκαθάρτης ἁπλότης ἀφελότης βραδύτης
        γέλως γόης γυμνότης ἑνότης ἐσθής
        εὐθύτης θειότης θεότης ἱδρώς ἱκανότης
        ἱλαρότης ἰσότης Ἰωσῆς καθαρότης καινότης
        Κρής κυριότης λαμπρότης ματαιότης μεγαλειότης
        νεότης ὁμοιότης ὁσιότης παλαιότης
        πένης πιότης πλάνης πραότης πραΰτης
        σεμνότης σής σκληρότης τελειότης χάρις
        χρηστότης χρώς νυκ- 
      exceptions: 
        νυκ- nom sg νύξ
    |]
  , mounce §§ ["n-3c(1)"] $
    [nounCategory| 
      Stems ending in τ with accusative, singular ν
           sg: pl:
      nom: ς   τες
      gen: τος των
      dat: τι  σι
      acc: ν   τας
      voc: -   τες
      lemmas:
        χάρις
    |]
  , mounce §§ ["n-3c(2)"] $
    [nounCategory| 
      Stems ending in δ
           sg: pl:
      nom: ς   δες
      gen: δος δων
      dat: δι  σι
      acc: δα  δας
      voc: -   δες
      lemmas:
        ἀκρίς Ἀντιπατρίς Ἄρτεμις ἀσπίς ἀτμίς
        βολίς Δάμαρις δισμυριάς Δορκάς Ἑβραΐς
        Ἑλλάς ἐλπίς ἔρις Ἡρῳδιάς θυρίς
        ἴασπις ἰκμάς ἶρις Ἰωσῆς κεφαλίς
        κλείς λαμπάς λεπίς Λωΐς μερίς
        μοιχαλίς μυριάς νῆστις παγίς παῖς
        παραστάτις παροψίς πατρίς Περσίς πινακίς
        πορφυρόπωλις πρεσβῦτις προστάτις προφῆτις
        Πτολεμαΐς ῥαφίς ῥυτίς Σαμαρῖτις σανίς
        σπιλάς σπυρίς στιβάς στοιβάς συγγενίς
        σφραγίς Τιβεριάς Τραχωνῖτις Τρῳάς
        ὑπολαμπάς χιλιάς χλαμύς πο- τετράπο-
      exceptions:
        πο- nom sg πούς
        πο- voc sg πούς
        τετραπο- nom sg τετράπους
        τετραπο- voc sg τετράπους
    |]
  , mounce §§ ["n-3c(3)"] $
    [nounCategory| 
      Stems ending in θ
           sg: pl:
      nom: ς   θες
      gen: θος θων
      dat: θι  σι
      acc: θα  θας
      voc: -   θες
      lemmas: 
        ὄρνις
    |]
  , mounce §§ ["n-3c(4)"] $
    [nounCategory|
      Stems ending in ματ
           sg: pl:
      nom: -   τα
      gen: τος των
      dat: τι  σι
      acc: -   τα
      voc: -   τα
      lemmas:
        ἀγνόημα ἀδίκημα αἷμα αἴνιγμα αἴτημα
        αἰτίαμα αἰτίωμα ἀλίσγημα ἁμάρτημα ἀνάθεμα
        ἀνάθημα ἀντάλλαγμα ἀνταπόδομα ἄντλημα ἀπαύγασμα
        ἀπόκριμα ἀποσκίασμα ἅρμα ἄρωμα ἀσθένημα
        βάπτισμα βδέλυγμα βῆμα βλέμμα βούλημα
        βρῶμα γένημα γέννημα γράμμα δεῖγμαδέρμα
        διάδημα διανόημα διάστημα διάταγμα δικαίωμα
        διόρθωμα δόγμα δόμα δῶμα δώρημα
        ἔγκλημα ἑδραίωμα ἔκτρωμα ἕλιγμα ἔνδειγμα
        ἔνδυμα ἐνέργημα ἔνταλμα ἐξέραμα ἐπάγγελμα
        ἐπερώτημα ἐπίβλημα ἐπικάλυμμα ζήτημα ἥττημα
        θαῦμα θέλημα θρέμμα ουμίαμα ἴαμα
        ἱεράτευμα κάθαρμα κάλυμμα κατάθεμα κατάκριμα
        καταλείμμα κατάλυμα κατανάθεμα καταπέτασμα κατάστημα
        κατόρθωμα καῦμα καύχημα κέλευσμα κέρμα
        κήρυγμα κλάσμα κλέμμα κλῆμα κλίμα
        κρίμα κτῆμα κτίσμα κύλισμα κῦμα
        λεῖμμα μεσουράνημα μίασμα μίγμα μίσθωμα
        μνῆμα νόημα νόμισμα νόσημα οἴκημα
        ὁλοκαύτωμα ὄμμα ὁμοίωμα ὄνομα ὅραμα
        ὅρμημα ὀφείλημα ὀχύρωμα πάθημα παράπτωμα
        περικάθαρμα περίσσευμα περίψημα πλάσμα πλέγμα
        πλήρωμα πνεῦμα ποίημα πολίτευμα πόμα
        πρᾶγμα πρόκριμα πρόσκομμα πτύσμα πιῶμα
        ῥᾳδιούργημα ῥάπισμα ῥῆγμα ῥῆμα σέβασμα
        σκέπασμα σκήνωμα σπέρμα στέμμα στερέωμα
        στίγμα στόμα στράτευμα σύντριμμα σχῆμα
        σχίσμα σῶμα τάγμα τραῦμα τρῆμα
        τρύπημα ὑπόδειγμα ὑπόδημα ὑπόλειμμα ὑστέρημα
        ὕψωμα φάντασμα φίλημα φρόνημα φύραμα
        χάραγμα χάρισμα χάσμα χόρτασμα χρῆμα
        χρῖσμα ψεῦσμα
    |]
  , mounce §§ ["n-3c(5a)"] $
    [nounCategory| 
      Stems ending in ντ (with σ in the nominative singular, ο vowel)
           sg:   pl:
      nom: ους   οντες
      gen: οντος οντων
      dat: οντι  ουσι
      acc: οντα  οντας
      voc: ους   οντες
      lemmas:
        ὀδούς
    |]
  , mounce §§ ["n-3c(5a)"] $
    [nounCategory|
      Stems ending in ντ (with σ in the nominative singular, α vowel)
           sg:   pl:
      nom: ας    αντες
      gen: αντος αντων
      dat: αντι  ασι
      acc: αντα  αντας
      voc: ας    αντες
      lemmas: 
        ἱμάς
    |]
  , mounce §§ ["n-3c(5a)"] $
    [nounCategory| 
      Stems ending in ντ (with σ in the nominative singular, ε vowel)
           sg:   pl:
      nom: ης    εντες
      gen: εντος εντων
      dat: εντι  ησι
      acc: εντα  εντας
      voc: ης    εντες
      lemmas:
        Κρήσκης
        Πούδης
        Κλήμης
    |]
  , mounce §§ ["n-3c(5b)"] $
    [nounCategory| 
      Stems ending in ντ (with no ending in the nominative singular)
           sg:   pl:
      nom: ων    οντες
      gen: οντος οντων
      dat: οντι  ουσι
      acc: οντα  οντας
      voc: ων    οντες
      lemmas: 
        ἄρχων γέρων δράκων θεράπων λέων
        Σαλωμών Σολομών Φλέγων
    |]
  , mounce §§ ["n-3c(6a)"] $
    [nounCategory|
      Nouns ending ας
           sg:  pl:
      nom: ας   ατα
      gen: ατος ατων
      dat: ατι  ασι
      acc: ας   ατα
      voc: ας   ατα
      lemmas: 
        ἅλας κέρας πέρας τέρας
    |]
  , mounce §§ ["n-3c(6b)"] $
    [nounCategory| 
      Nouns ending in ρ, ω vowel
           sg:  pl:
      nom: ωρ   ατα
      gen: ατος ατων
      dat: ατι  ασι
      acc: ωρ   ατα
      voc: ωρ   ατα
      lemmas:
        ὕδωρ
    |]
  , mounce §§ ["n-3c(6b)"] $
    [nounCategory| 
      Nouns ending in ρ, α vowel
           sg:  pl:
      nom: αρ   ατα
      gen: ατος ατων
      dat: ατι  ασι
      acc: αρ   ατα
      voc: αρ   ατα
      lemmas: 
        ὄναρ φρέαρ
    |]
  , mounce §§ ["n-3c(6c)"] $
    [nounCategory|
      Nouns ending in ς (ως)
           sg:  pl:
      nom: ως   ωτα
      gen: ωτος ωτων
      dat: ωτι  *
      acc: ως   ωτα
      voc: ως   ωτα
      lemmas:
        φῶς
    |]
  , mounce §§ ["n-3c(6c)"] $
    [nounCategory| 
      Nouns ending in ς (ους)
           sg:   pl:
      nom: ους   ωτα
      gen: ωτος  ωτων
      dat: ωτι   ωσι
      acc: ους   ωτα
      voc: ους   ωτα
      lemmas: 
        οὖς
    |]
  , mounce §§ ["n-3c(6d)"] $
    [nounCategory|
      Irregular stems - γάλα
           sg:   pl: 
      nom: αλα   *
      gen: ακτος *
      dat: *     *
      acc: αλα   *
      voc: αλα   *
      lemmas: 
        γάλα
    |]
  , mounce §§ ["n-3c(6d)"] $
    [nounCategory| 
      Irregular stems - γόνυ
           sg:  pl:
      nom: υ    ατα
      gen: ατος *
      dat: *    ασι
      acc: υ    ατα
      voc: υ    ατα
      lemmas:
        γόνυ
    |]
  , mounce §§ ["n-3c(6d)"] $
    [nounCategory| 
      Irregular stems - μέλι
           sg:  pl:
      nom: ι    *
      gen: ιτος *
      dat: *    *
      acc: ι    *
      voc: ι    *
      lemmas:
        μέλι
    |]
  , mounce §§ ["n-3c(6d)"] $
    [nounCategory| 
      Irregular stems - κρέας
           sg: pl: 
      nom: ας  α
      gen: ως  *
      dat: *   *
      acc: ας  α
      voc: ας  α
      lemmas: 
        κρέας
    |]
  , mounce §§ ["n-3d(1)"] $
    [nounCategory| 
      Stems ending in ας
           sg: pl:
      nom: ας  *
      gen: ους *
      dat: ει  *
      acc: ας  *
      voc: ας  *
      lemmas: 
        γῆρας
    |]
  , mounce §§ ["n-3d(2)"] $
    [nounCategory| 
      Stems ending in ες
           sg: pl:
      nom: ος  η
      gen: ους ων
      dat: ει  εσι
      acc: ος  η
      voc: ος  η
      lemmas:
        ἄγγος ἄνθος βάρος βέλος βρέφος
        γένος γλεῦκος δέος δίψος ἔδαφος 
        ἔθνος ἔθος εἶδος ἔλεος ἕλκος
        ἔπος ἔτος ζεῦγος ζῆλος ἦθος
        ἦχος θάμβος θάρσος θέρος ἴχνος
        κάρφος κέρδος κῆτος κλέος κράτος
        κτῆνος μέγεθος μέλος μέρος μῆκος 
        νέφος νῖκος ὄνειδος ὄξος ὄρος 
        ὄφελος πάθος πέλαγος πένθος πλάτος
        πλῆθος ῥάκος σκέλος σκεῦος σκῆνος
        σκότος στῆθος στρῆνος τάχος τεῖχος 
        τέλος ὕψος φέγγος χεῖλος ψεῦδος
        ψῦχος
    |]
  , mounce §§ ["n-3d(2)"] $
    [nounCategory| 
      Stems ending in ες
           sg: pl:
      nom: ης  *
      gen: ους *
      dat: ει  *
      acc: ην  *
      voc: ης  *
      lemmas:
        Διοτρέφης Ἑρμογένης Σωσθένης
     |]
   , mounce §§ ["n-3d(3)"] $
     [nounCategory| 
       Stems ending in ος
            sg: pl:
       nom: ως  *
       gen: ους *
       dat: οι  *
       acc: ω   *
       voc: ως  *
       lemmas:
         αἰδώς
    |]
  , mounce §§ ["n-3e(1)"] $
    [nounCategory| 
      Stems ending in ϝ
           sg: pl:
      nom: υς  υες
      gen: υος υων
      dat: υι  υσι
      acc: υν  υας
      voc: υ   υες
      lemmas:
        ἀχλύς βότρυς ἰσχύς ἰχθύς
        ὀσφῦς ὀφρῦς πῆχυς Στάχυς στάχυς
        ὗς δάκρ-
      exceptions:
        δάκρ- nom sg δάκρυ
    |]
  , mounce §§ ["n-3e(2)"] $
    [nounCategory| 
      Stems ending in αϝ
           sg: pl:
      nom: αυς ες
      gen: ως  ων
      dat: ι   υσι
      acc: υν  υς
      voc: υ   ες
      lemmas:
        ναῦς
    |]
  , mounce §§ ["n-3e(3)"] $
    [nounCategory| 
      Stems ending in εϝ
           sg: pl:
      nom: ευς εις
      gen: εως εων
      dat: ει  ευσι
      acc: εα  εις
      voc: ευ  εις
      lemmas:      
        Ἀλεξανδρεύς ἁλιεύς Ἀντιοχεύς ἀρχιερεύς βηρεύς
        βασιλεύς βυρσεύς γναφεύς γραμματεύς γονεύς
        Θεσσαλονικεύς ἱερεύς ἱππεύς καταγγελεύς κεραμεύς
        Κολασσαεύς Κολοσσαεύς Λαοδικεύς Νηρεύς πανδοχεύς
        Ταρσεύς φαρμακεύς φονεύς χαλκεύς
    |]
  , mounce §§ ["n-3e(4)"] $
    [nounCategory| 
      Stems ending in οϝ
           sg: pl:
      nom: ους οες
      gen: οος οων
      dat: οϊ  ουσι
      acc: ουν οας
      voc: ου  οες
      lemmas:
        βοῦς νοῦς πλοῦς χοῦς
    |]
  , mounce §§ ["n-3e(5a)"] $
    [nounCategory| 
      Stems ending in ι - no ablaut
           sg: pl:
      nom: ις  *
      gen: *   *
      dat: *   *
      acc: *   εις
      voc: ις  * 
      lemmas:
        νῆστις
     |]
   , mounce §§ ["n-3e(5b)"] $
     [nounCategory| 
       Stems ending in ι - with ablaut
            sg: pl:
       nom: ις  εις
       gen: εως εων
       dat: ει  εσι
       acc: ιν  εις
       voc: ι   εις
       lemmas: 
         ἀγαλλίασις ἀγανάκτησις ἀθέτησις ἄθλησις αἴνεσις
         αἵρεσις αἴσθησις ἅλυσις ἅλωσις Ἀμφίπολις
         ἀνάβλεψις ἀνάγνωσις ἀνάδειξις ἀναίρεσις ἀνακαίνωσις
         ἀνάκρισις ἀνάλημψις ἀνάλυσις ἀνάμνησις ἀνάπαυσις
         ἀνάστασις ἀνάχυσις ἀνάψυξις ἄνεσις ἄνοιξις
         ἀνταπόδοσις ἀντίθεσις ἀντίλημψις ἀπάντησις ἀπέκδυσις
         ἀπόδειξις ἀπόθεσις ἀποκάλυψις ἀποκατάστασις ἀπόκρισις
         ἀπόλαυσις ἀπολύτρωσις ἀπόχρησις αὔξησις ἄφεσις
         ἄφιξις βάσις βεβαίωσις βίωσις βρῶσις
         γένεσις γέννησις γνῶσις δάμαλις δέησις
         Δεκάπολις δέρρις διάγνωσις διαίρεσις διάκρισις
         διήγησις δικαίωσις διόρθωσις δόσις δύναμις
         δύσις ἔγερσις ἔκβασις ἐκδίκησις ἐκζήτησις
         ἐκπλήρωσις ἔκστασις ἔλεγξις ἔλευσις ἔνδειξις
         ἔνδυσις ἐνδόμησις ἐνδώμησις ἐνθύμησις ἔντευξις
         ἐξανάστασις ἕξις ἐπανόρθωσις ἔπαυλις ἐπίγνωσις
         ἐπίθεσις ἐπίλυσις ἐπιπόθησις ἐπίστασις ἐπισύστασις
         ἐπιχείρησις ἐρήμωσις ζήτησις θέλησις θλῖψις
         ἴασις Ἱεράπολις καθαίρεσις κάκωσις κατάβασις
         κατάκρισις κατάνυξις κατάπαυσις κατάρτισις κατασκήνωσις
         κατάσχεσις κατοίκησις καῦσις καύχησις κίνησις
         κλάσις κλῆσις κοίμησις κόλασις κρίσις
         κτίσις κυβέρνησις κωμόπολις λῆμψις λῆψις
         λύσις λύτρωσις μέμψις μετάθεσις μετάλημψις
         μητρόπολις μόρφωσις Νεάπολις νέκρωσις Νικόπολις
         ὁμοίωσις ὅρασις ὄρεξις ὄσφρησις ὄφις 
         ὄψις πανήγυρις παράβασις παράδοσις παράκλησις
         παρατήρησις πάρδαλις πάρεσις πεποίθησις περίθεσις
         περιποίησις πήρωσις πίστις ποίησις πόλις
         πόσις πρᾶξις πρόγνωσις πρόθεσις προσκαρτέρησις
         πρόσκλησις πρόσκλισις πρόσλημψις πρόσληψις πρόσχυσις
         πρόφασις πτόησις πτῶσις πύρωσις πώρωσις        
         ῥύσις Σάρδεις σεμίδαλις στάσις
         συγκατάθεσις σύγχυσις συζήτησις συμφώνησις συνάντησις
         συνείδησις σύνεσις Σύρτις τάξις απείνωσις
         τελείωσις τήρησις ὕβρις ὑπάντησις ὕπαρξις       
         ὑπόκρισις ὑπόμνησις ὑπόστασις ὑποτύπωσις ὑστέρησις
         φανέρωσις φάσις φρόνησις φύσις φυσίωσις
         χρῆσις σίναπ-
       exceptions: 
         σίναπ- nom sg σίναπι
         σίναπ- voc sg σίναπι
     |]
   , mounce §§ ["n-3e(6)"] $
     [nounCategory| 
       Stems ending in οι
            sg: pl:
       nom: ω   *
       gen: ους *
       dat: οι  *
       acc: ω   *
       voc: οι  *
       lemmas: 
        πειθώ ἠχώ
     |]
   , mounce §§ ["n-3f(1a)"] $
     [nounCategory| 
       Stems ending in ν - no ablaut
            sg: pl:
       nom: ν   νες
       gen: νος νων
       dat: νι  σι
       acc: να  νας
       voc: ν   νες
       lemmas:
         ἀγών αἰών ἅλων ἀμπελών ἀρραβών
         ἀρτέμων Ἀσσάρων ἀφεδρών Βαβυλών Γαλλίων
         ἐλαιών Ἕλλην εὐρακύλων εὐροκλύδων
         ζήνων Ἡρῳδίων καύσων κεντυρίων κλύδων
         κοιτών λεγιών μεγιστάν μέλαν μήν
         Μνάσων μυλών Νέρων νυμφών πύθων
         πυλών Σαρών Σιδών Σίμων Σολομών
         Τίμων χειμών χιτών ὠδίν
         δεῖν-
       exceptions:
         δεῖν- nom sg δεῖνα
         Σαλαμί- nom sg Σαλαμίς
         Σαλαμί- voc sg Σαλαμίν
     |]
   , mounce §§ ["n-3f(1b)"] $
     [nounCategory| 
       Stems ending in ν - showing strong and weak ablaut - ω stem
            sg:  pl:
       nom: ων   ονες
       gen: ονος ονων
       dat: ονι  οσι
       acc: ονα  ονας
       voc: ων   ονες
       lemmas:
         ἀλαζών Ἀπολλύων ἀρχιτέκτων βραχίων
         γείτων δαίμων εἰκών ἡγεμών Ἰάσων
         κανών Μακεδών σιαγών
         σινδών τέκτων τρυγών Φιλήμων
         χαλκηδών χιών
     |]
   , mounce §§ ["n-3f(1b)"] $
     [nounCategory| 
       Stems ending in ν - showing strong and weak ablaut - η stem
            sg:  pl:
       nom: ην   ενες
       gen: ενος ενην
       dat: ενι  εσι
       acc: ενα  ενας
       voc: ην   ενες
       lemmas:
         ἀρχιποίμην λιμήν ποιμήν φρήν
     |]
   , mounce §§ ["n-3f(1c)"] $
     [nounCategory| 
       Stems ending in ν - showing strong and zero ablaut - ω stem
            sg: pl:
       nom: ων  νες
       gen: νος νων
       dat: νι  σι
       acc: να  νας
       voc: ων  νες
       lemmas:
         κύων
     |]
   , mounce §§ ["n-3f(1c)"] $
     [nounCategory| 
       Stems ending in ν - showing strong and zero ablaut - η stem
            sg: pl:
       nom: ην  νες
       gen: νος νων
       dat: νι  σι
       acc: να  νας
       voc: ην  νες
       lemmas:
         ἀρήν
     |]
   , mounce §§ ["n-3f(2a)"] $
     [nounCategory| 
       Stems ending in a ρ with no ablaut
            sg: pl:
       nom: ρ   ρες
       gen: ρος ρων
       dat: ρι  ρσι
       acc: ρα  ρας
       voc: ρ   ρες
       lemmas:
         αὐτόχειρ Καῖσαρ νιπτήρ
         ποδινιπτήρ πῦρ στατήρ σωτήρ
         φωστήρ χαρακτήρ χείρ
     |]
   , mounce §§ ["n-3f(2a)"] $
     [nounCategory| 
       Stems ending in a ρ with no ablaut - χείρ dative χερσί
            sg: pl:
       nom: ειρ   ειρες
       gen: ειρος ειρων
       dat: ειρι  ερσι
       acc: ειρα  ειρας
       voc: ειρ   ειρες
       lemmas:
         χείρ
     |]
   , mounce §§ ["n-3f(2a)"] $
     [nounCategory| 
       Stems ending in a λ with no ablaut
            sg: pl:
       nom: λς  λες
       gen: λος λων
       dat: λι  λσι
       acc: λα  λας
       voc: λς  λες
       lemmas:
         ἅλς
     |]
   , mounce §§ ["n-3f(2a)"] $
     [nounCategory| 
       Stems ending in a ρ with no ablaut - μάρτυς
            sg: pl:
       nom: ς   ρες
       gen: ρος ρων
       dat: ρι  ρσι
       acc: ρα  ρας
       voc: ς   ρες
       lemmas:
         μάρτυς πρωτόμαρτυς ψευδόμαρτυς
     |]
   , mounce §§ ["n-3f(2b)"] $
     [nounCategory| 
       Stems ending in a λ or ρ showing strong and weak ablaut
            sg: pl:
       nom: ρ   ρες
       gen: ρος ρων
       dat: ρι  ρσι
       acc: ρα  ρας
       voc: ρ   ρες
       lemmas:
         ἀήρ ἀλέκτωρ ἀστήρ δειπνοκλήτωρ κατήγωρ
         κοσμοκράτωρ κτήτωρ Νικάνωρ παντοκράτωρ πράκτωρ
         προπάτωρ ῥήτωρ σπεκουλάτωρ
     |]
   , mounce §§ ["n-3f(2c)"] $
     [nounCategory| 
       Stems ending in a λ or ρ showing strong, weak and zero ablaut
            sg: pl:
       nom: ρ   ρες
       gen: ρος ρων
       dat: ρι  ρσι
       acc: ρα  ρας
       voc: ρ   ρες
       lemmas:
         ἀνήρ γαστήρ θυγάτηρ μήτηρ πατήρ
    |]
  , mounce §§ ["n-3g(1)"] $
    [nounCategory|
      Irregularly and partially declined stems - Ζηνᾶς
           sg: pl:
      nom: ας  *
      gen: ας  *
      dat: ας  *
      acc: αν  *
      voc: ας  *
      lemmas:
        Ἀκύλας Ζηνᾶς
    |] 
  , mounce §§ ["n-3g(1)"] $
    [nounCategory| 
      Irregularly and partially declined stems - Ἰησοῦς
           sg: pl:
      nom: ους *
      gen: ου  *
      dat: ου  *
      acc: ουν *
      voc: ου  *
      lemmas:
        Ἰησοῦς
    |]
  , mounce §§ ["n-3g(1)"] $
    [nounCategory|
      Irregularly and partially declined stems - Μωϋσῆς
           sg: pl:
      nom: ης  *
      gen: εως *
      dat: ει  *
      acc: ην  *
      voc: *   *
      lemmas:
        Μωϋσῆς Μωσῆς
    |]
  , mounce §§ ["n-3g(1)"] $
    [nounCategory| 
      Irregularly and partially declined stems - Θυάτειρα
           sg: pl:
      nom: α   *
      gen: ων  *
      dat: οις *
      acc: α   *
      voc: α   *
      lemmas:
        Θυάτιρα Θυάτειρα
    |]
  , mounce §§ ["n-3g(1)"] $
    [nounCategory| 
      Irregularly and partially declined stems - Λύδδα"
           sg: pl: 
      nom: -   *
      gen: ς   *
      dat: *   *
      acc: -   *
      voc: -   *
      lemmas:
        Λύδδα
   |]
 , mounce §§ ["n-3g(1)"] $
   [nounCategory| 
     Irregularly and partially declined stems - Λυστρά
          sg: pl:
     nom: -   *
     gen: *   *
     dat: *   *
     acc: ν   *
     voc: -   *
     lemmas:  
       Λυστρά Γολγοθᾶ
   |]
 , mounce §§ ["n-3g(1)"] $
   [nounCategory| 
     Irregularly and partially declined stems - Λευίς
          sg: pl: 
     nom: ς   *
     gen: -   *
     dat: -   *
     acc: ν   *
     voc: *   *
     lemmas: 
       Λευίς
    |]
  , mounce §§ ["n-3g(1)"] $
    [nounCategory| 
      Irregularly and partially declined stems - Ζεύς
           sg:   pl:
      nom: Ζευς  *
      gen: Διος  *
      dat: Διι   *
      acc: Δια   *
      voc: Ζευ   *
      lemmas:
        Ζεύς 
   |]
 , mounce §§ ["n-3g(2)"] $
   [nounCategory| 
     Indeclinable stems
           sg: pl:
     nom: -   -
     gen: -   -
     dat: -   -
     acc: -   -
     voc: -   -
     lemmas:
       Ἀαρών Ἀβαδδών ἀββά Ἅβελ Ἀβιά 
       Ἀβιαθάρ Ἀβιούδ Ἀβραάμ Ἁγάρ Ἀδάμ
       Ἀδδί Ἀδμίν Ἀζώρ Αἰνών Ἀκελδαμάχ
       ἁλληλουϊά ἄλφα Ἀμιναδάβ Ἀμών Ἀμώς
       Ἀράμ Ἁρμαγεδών Ἀρνί Ἀρφαξάδ Ἀσά
       Ἀσάφ Ἀσήρ Ἀχάζ Ἀχάς Ἀχίμ 
       Βάαλ Βαλαάμ Βαλάκ βάρ Βαράκ 
       Βαριωνᾶ Βεεζεβούλ Βελιάρ Βενιαμίν Βεώρ
       Βηθεσδά Βηθζαθά Βηθλέεμ Βηθσαϊδά Βηθσαϊδάν Βηθφαγή
       Βοανηργές Βόες Βόοζ Βόος Βοσόρ
       Γαββαθᾶ Γαβριήλ Γάδ Γαμαλιήλ Γεδεών
       Γεθσημανί Γεννησαρέτ Γώγ Δαβίδ Δαλμανουθά
       Δάν Δανιήλ Δαυίδ Ἔβερ Ἐλεάζαρ
       Ἐλιακίμ Ἐλιέζερ Ἐλιούδ Ἐλισάβετ Ἐλμαδάμ
       Ἐλμωδάμ ἐλωι Ἐμμανουήλ Ἑμμώρ Ἐνώς
       Ἑνώχ Ἑσλί Ἑσρώμ Ἐφραίμ ἐφφαθά
       Ζαβουλών Ζάρα ζαφθάνι Ζοροβαβέλ ηλι
       Ἠλί Ἤρ Ἠσαῦ θάβιτα Θαμάρ
       Θάρα Ἰακώβ Ἰανναί Ἰάρετ Ἰαχίν
       Ἰεζάβελ Ἰεριχώ Ἰερουσαλήμ Ἰεσσαί Ἰεφθάε 
       Ἰσαάκ Ἰσαχάρ Ἰσκαριώθ Ἰσραήλ Ἰσσαχάρ
       Ἰωαθάμ Ἰωανάν Ἰωᾶς Ἰώβ Ἰωβήδ
       Ἰωδά Ἰωήλ Ἰωνμ Ἰωράμ Ἰωρίμ
       Ἰωσαφάτ Ἰωσήφ Ἰωσήχ ἰῶτα Κάϊν
       Καϊνάμ Καϊνάν Κανά Καπερναούμ Καφαρναούμ
       Καῦδα Κεδρών Κίς Κλαῦδα κορβᾶν
       Κόρε κοῦμ κοῦμι Κωσάμ λαμα
       Λάμεχ λεμά Λευί Λώτ Μάαθ 
       Μαγδαλά Μαγαδάν Μαγεδών Μαγώγ Μαδιάμ
       Μαθθάτ Μαθουσάλα Μαϊνάν Μαλελεήλ Μαναήν
       μάννα μαρὰν ἀθᾶ μαράνα θᾶ Μαριάμ Ματθάν
       Ματθάτ Ματταθά Μελεά Μελχί Μελχισέδεκ
       Μεννά Μιχαήλ Μολόχ Ναασσών Ναγγαί
       Ναζαρά Ναζαρέθ Ναζαρέτ Ναθάμ Ναθάν
       Ναθαναήλ Ναιμάν Ναΐν Ναούμ Ναχώρ
       Νεεμάν Νευης Νεφθαλίμ Νηρί Νίγερ
       Νινευή Νινευΐ Νῶε πάσχα Ῥαάβ
       ῥαββί ῥαββονί ῥαββουνί ῥαβιθά Ῥαγαύ
       Ῥαιφάν ῥακά Ῥαμά ῥαχά Ῥαχάβ
       Ῥαχήλ Ῥεμφάν Ῥεφάν Ῥησά Ῥοβοάμ
       Ῥομφά Ῥουβήν Ῥούθ σαβαχθάνι Σαβαώθ
       Σαδώκ Σαλά Σαλαθιήλ Σαλήμ Σαλίμ
       Σαλμών Σαμουήλ Σαμφουρειν Σαμψών
       Σαούλ Σαρούχ Σατάν Σεμεΐν Σερούχ
       Σήθ Σήμ σίκερα Σιλωάμ Σινά Σιχάρ      
       Σιχέμ Σιών Σκαριώθ Σκαριώτης Συμεών
       Συχάρ Συχέμ Ταβιθά ταλιθά Φάλεκ
       Φανουήλ Φαραώ Φαρές Χανάαν χαρράν
       Χοραζίν Χωραζίν Χερουβιν Ὠβήδ ὡσαννά
       Ὡσηέ
      |]
    ]   
