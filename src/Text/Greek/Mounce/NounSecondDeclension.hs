{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Text.Greek.Mounce.NounSecondDeclension where

import Text.Greek.Grammar
import Text.Greek.Mounce.Morphology
import Text.Greek.Mounce.Quote

secondDeclensionNouns :: [Cited Noun]
secondDeclensionNouns = 
  [ mounce § "n-2a" $
    Noun "Masculine nouns with stems ending in ο(ς)"
      [nounCaseEndings|
        ος οι
        ου ων
        ῳ  οις
        ον ους
        ε  οι
      |]
      [greekWords|
        Ἅγαβος ἄγαμος ἄγγελος ἁγιασμός ἁγνισμός
        ἀγρός ἀδελφός ἀετός αἰγιαλός αἶνος
        αἰχμάλωτος ἀλάβαστρος Ἀλέξανδρος ἀλλοτριεπίσκοπος Ἁλφαῖος
        ἀμνός ἀμπελουργός Ἀμπλιᾶτος ἀναβαθμός Ἀνδρόνικος
        ἀνδροφόνος ἄνεμος ἀνεψιός ἀνθρωποκτόνος ἄνθρωπος
        ἀνθύπατος ἀντίδικος ἀντίχριστος ἀπαρτισμός ἀπελεγμός
        ἀπελεύθερος ἀπόστολος Ἄππιος Ἄραβοι ἀργυροκόπος
        ἄργυρος ἀριθμός Ἀρίσταρχος Ἀριστόβουλος ἄρκος
        ἄρκτος ἁρμός ἁρπαγμός ἄρτος ἀρχάγγελος
        Ἀρχέλαος ἀρχηγός Ἄρχιππος ἀρχισυνάγωγος ἀρχιτρίκλινος
        Ἀσιανός ἀσκός ἀσπασμός Ἀσύγκριτος Αὐγοῦστος
        αὐλός ἀφανισμός ἀφρός Ἀχαϊκός βαθμός
        βαπτισμός Βαρθολομαῖος Βαρτιμαῖος βασανισμός βασιλίσκος
        βάτος βάτος βάτραχος βήρυλλος βίος
        Βλάστος βόθρος βόθυνος βόρβορος βουνός
        βρόχος βρυγμός βυθός βωμός Γάϊος
        γάμος γεωργός γνόφος γογγυσμός γόμος
        δακτύλιος δάκτυλος δεξιολάβος δέσμιος δεσμός
        Δημήτριος δημιουργός δῆμος διάδοχος διάκονος
        διαλογισμός διαμερισμός διδάσκαλος Δίδυμος Διονύσιος
        Διόσκουροι διωγμός δόλος δοῦλος δρόμος
        Ἑβραῖος ἑκατόνταρχος ἐλεγμός ἔλεγχος Ἐλισαῖος
        Ἐλισσαῖος ἐμπαιγμός ἔμπορος ἐνιαυτός ἐνταφιασμός
        Ἐπαίνετος ἔπαινος Ἐπαφρόδιτος Ἐπικούρειος ἐπισιτισμός
        ἐπίσκοπος ἐπίτροπος Ἔραστος ἔριφος ἑταῖρος
        Εὔβουλος εὐνοῦχος Εὔτυχος Ζακχαῖος Ζεβεδαῖος
        ζῆλος ζόφος ζυγός ἥλιος ἧλος
        Ἡρῳδιανοί ἦχος Θαδδαῖος θάμβος θάνατος
        θεμέλιος θεολόγος θεός Θεόφιλος θερισμός
        θησαυρός θόρυβος θρῆνος θρόμβος θρόνος
        θυμός θυρεός θυρωρός Ἰάϊρος Ἰάκωβος
        ἰατρός ἱερόσυλος ἱλασμός ἱματισμός ἰός
        Ἰουδαϊσμός Ἰούλιος Ἰοῦστος ἵππος κάβος
        κάδος καθαρισμός καιρός κάλαμος κάμηλος
        κάμιλος Καναναῖος καπνός Κάρπος καρπός
        κατακλυσμός καταρτισμός κατάσκοπος κατήγορος κέραμος
        κῆνσος κῆπος κηπουρός κιθαρῳδός κίνδυνος
        κλάδος Κλαύδιος κλαυθμός κληρονόμος κλῆρος
        κλίβανος κοινωνός κόκκος κόλπος κονιορτός
        κοπετός κόπος Κορίνθιος Κορνήλιος κόρος
        κόσμος Κούαρτος κόφινος κράβαττος κράββατος
        κρημνός Κρίσπος κρύσταλλος κυλισμός Κύπριος
        Κυρηναῖος Κυρήνιος κύριος κῶμος Λάζαρος
        λαός Λεββαῖος λειτουργός λῆρος λίβανος
        λιβανωτός Λιβερτῖνος Λιβυστῖνος λίθος λιμός
        Λίνος λογισμός λόγος λοίδορος λοιμός
        Λούκιος λύκος λύχνος μάγος μαζός
        Μαθθαῖος μακαρισμός Μάλχος Μᾶρκος μάρμαρος
        μασθός μαστός Ματθαῖος μέθυσος μερισμός
        Μῆδος μηρός μιασμός μίσθιος μισθός
        μισθωτός μόδιος μοιχός μολυσμός μόσχος
        μόχθος μυελός μῦθος μύλος μῶμος
        Ναζωραῖος ναός Νάρκισσος ναύκληρος νεανίσκος
        νεοσσός νεφρός νεωκόρος Νικόδημος Νικόλαος
        νομοδιδάσκαλος νόμος νοσσός νότος νυμφίος
        νῶτος ὄγκος ὁδηγός ὀδυρμός οἰκιακός
        οἰκοδόμος οἰκονόμος οἶκος οἰκτιρμός οἶνος
        ὄλεθρος ὄλυνθος ὄμβρος ὅμιλος ὀνειδισμός
        Ὀνήσιμος Ὀνησίφορος ὄνος ὄρθρος ὅρκος
        ὅρος οὐρανός Οὐρβανός ὀφθαλμός ὀχετός
        ὄχλος πάγος παιδαγωγός παράδεισος παράκλητος
        παραπικρασμός παρθένος Πάρθοι παροξυσμός παροργισμός
        Πάτμος Παῦλος πειρασμός πενθερός Πέτρος
        πηλός Πιλᾶτος πλόος πλοῦτος πόλεμος
        πόνος Πόντιος Πόντος πόντος Πόπλιος
        πορισμός Πόρκιος πόρνος ποταμός Ποτίολοι
        πότος προσήλυτος Πρόχορος πύργος πυρετός
        Πύρρος πῶλος ῥαβδοῦχος ῥαντισμός Ῥοῦφος
        ῥύπος σαββατισμός Σαδδουκαῖος σάκκος σάλος
        σάρδινος Σαῦλος σειρός σεισμός Σεκοῦνδος
        Σέργιος σίδηρος σικάριος Σιλουανός σιρός
        σῖτος σκηνοποιός σκοπός σκορπίος σμάραγδος
        σπίλος σπόγγος σπόρος σταυρός στεναγμός
        Στέφανος στέφανος στηριγμός στόμαχος στρατηγός
        στρατοπέδορχος στῦλος συγκοινωνός σύζυγος σύμβουλος
        συμπρεσβύτερος συναιχμάλωτος σύνδεσμος σύνδουλος συνέδριος
        σύνεδρος συνέκδημος συνεπίσκοτος συνεργός Σύρος
        Σώπατρος Σωσίπατρος σωφρονισμός τάραχος ταῦρος
        τάφος Τέρτιος Τέρτουλλος Τέρτυλλος Τιβέριος
        Τιμαῖος Τιμόθεος Τίτιος τίτλος Τίτος
        τοῖχος τόκος τόπος τράγος τράχηλος
        τρίβολος τρόμος τρόπος Τρόφιμος τροχός
        τύπος Τύραννος τύραννος Τύριος Τυχικός
        ὑάκινθος ὑετός υἱός Ὑμέναιος ὕμνος
        ὕπνος ὑπογραμμός ὑσσός φάγος φανός
        φαρισαῖος φάρμακος Φῆστος φθόγγος φθόνος
        Φίλητος Φιλιππήσιος Φιλιπποι Φίλιππος Φιλόλογος
        φιλόσοφος φόβος φόνος φόρος φόρτος
        Φορτουνᾶτος φραγμός Φύγελος φωλεός φωτισμός
        Χαλδαῖος χαλινός χαλκός χείμαρρος χειραγωγός
        χιλίαρχος χοῖρος χορός χόρτος χρηματισμός
        Χριστιανός Χριστός χρόνος χρυσόλιθος χρυσόπρασος
        χρυσός χωρισμός χῶρος ψαλμός ψευδάδελφος
        ψευδαπόστολος ψευδοδιδάσκαλος ψευδόχριστος ψιθυρισμός ὦμος
    |]
  , mounce § "n-2b" $
    Noun "Femine nouns with stems ending in ο(ς)"
      [nounCaseEndings|
        ος οι
        ου ων
        ῳ οις
        ον ους
        ε  οι
      |]
      [greekWords|
        ἄβυσσος ἄγαμος ἀγριέλαιος Ἄζωτος Αἴγυπτος ἀλάβαστρος ἀμέθυσος ἀμέθυστος ἄμμος ἄμπελος ἄρκος ἄρκτος Ἆσσος ἄψινθος βάσανος βάτος βήρυλλος 
        βίβλος βύσσος Δαμασκός διάκονος διάλεκτος διέξοδος διέξοδος δοκός εἴσοδος ἔξοδος Ἔφεσος θεός θυρωρός καλλιέλαιος κάμηλος κάμινος κέδρος          
        κιβωτός Κνίδος κοινωνός κόπρος Κόρινθος Κύπρος ληνός λιμός Μίλητος νάρδος νῆσος νόσος ὁδός ὄνος παρθένος Πάφος Πέργαμος ῥάβδος Ῥόδος Σάμος 
        σάπφιρος σορός σποδός στάμνος συκάμινος Ταρσός τρίβος τροφός Τύρος ὕαλος ὕσσωπος χίος ψῆφος 
    |]
  , mounce § "n-2c" $
    Noun "Neuter nouns with stems ending in ο(ν)"
      [nounCaseEndings|
        ον α
        ου ων
        ῳ οις
        ον α 
        ε  οι
      |]
      [greekWords|
         ἀγγεῖον ἄγκιστρον αἰσθητήριον ἀκροατήριον ἀκροθίνιον ἄκρον ἀλάβαστρον ἄλευρον ἄμμον ἀμφίβληστρον ἄμφοδον ἄμωμον ἀνάγαιον ἄνηθον ἀντίλυτρον ἀποστάσιον
         ργύριον ἄριστον ἀρνίον ἄροτρον ἀσσάριον ἄστρον ἄχυρον ἀψίνθιον βάϊον βαλλάντιον βιβλαρίδιον βιβλίον βραβεῖον γαζοφυλάκιον γενέσια γεώργιον γλωσσόκομον
         Γόμορρα γυναικάριον δαιμόνιον δάκρυον δάνειον δεῖπνον δένδρον δεσμωτήριον δηνάριον δίδραχμον δίκτυον δοκίμιον δρέπανον δυσεντέριον δωδεκάφυλον
         δῶρον ἐγκαίνια εἰδωλεῖον εἴδωλον ἔλαιον ἐμπόριον ἔνεδρον ἐνύπνιον ἐπικεφάλαιον ἔργον ἔριον ἐρίφιον ἑρπετόν ἔσοπτρον εὐαγγέλιον ζιζάνιον ζυγόν
         ζῷον ἡδύοσμον ἡμιώριον ἡμίωρον θέατρον θεῖον θεμέλιον θηρίον θυγάτριον θυμιατήριον θυσιαστήριον ἱερόν Ἱεροσόλυμα Ἰκόνιον ἱλαστήριον Ἰλλυρικόν
         ἱμάτιον ἱστίον ἰχθύδιον κατοικητήριον κέντρον κεράμιον κεράτιον κεφάλαιον ηρίον κιβώριον κιννάμωμον κλινάριον κλινίδιον κολλούριον κόπριον κοράσιον
         κρανίον κράσπεδον κρίνον κριτήριον κύμβαλον κύμινον κυνάριον κῶλον λάχανον λέντιον λίνον λόγιον λουτρόν λύτρον μάκελλον μαρτύριον μεθόριον μελισσ(ε)ῖον
         μεσονύκτιον μεσότοιχον μέτρον μέτωπον μίλιον μνημεῖον μνημόσυνον Μύρα8 μύρον μυστήριον νησίον νοσσίον νυχθήμερον ξύλον ὀθόνιον οἰκητήριον ὀνάριον
         ὅπλον ὅριον ὄρνεον ὀστέον ὀψάριον ὀψώνιον παιδάριον παιδίον πανδοχεῖον παραμύθιον Πάταρα περιβόλαιον Πέργαμον πετεινόν πήγανον πηδάλιον πινακίδιον
         πλοιάριον πλοῖον ποίμνιον ποτήριον πραιτώριον πρεσβυτέριον προαύλιον προβάτιον πρόβατον προσάββατον προσκεφάλαιον προσφάγιον πρόσωπον πρωτοτόκια πτερύγιον
         πτύον Ῥήγιον σάββατον σανδάλιον σάρδιον Σάρεπτα σάτον σημεῖον σημικίθιον σιμικίνθιον σιτίον σιτομέτριον σκάνδαλον σκύβαλον σκῦλον Σόδομα σουδάριον
         σπήλαιον σπλάγχνον στάδιον στοιχεῖον στρατόπεδον στρουθίον σῦκον συμβούλιον συμπόσιον συνέδριον σύσσημον σφάγιον σφυδρόν σφυρόν σχοινίον τάλαντον
         ταμεῖον ταμιεῖον τεκμήριον τεκνίον τέκνον τελωνεῖον τελώνιον τετάδιον τόξον τοπάζιον τρίστεγον τρύβλιον Τρωγύλλιον ὑπερῷον ὑποζύγιον ὑπολήνιον
         ὑποπόδιον φάρμακον φόβητρον φόρον φορτίον φραγέλλιον φρύγανον φυλακτήριον φύλλον χαλκίον χαλκολίβανον χειρόγραφον χρυσίον χωρίον ψιχίον ψωμίον
         ᾠόν ὠτάριον ὠτίον
     |]
