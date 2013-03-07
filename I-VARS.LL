;; ---------------------------------------------------------------------------
;; - I-Vars.ll                                                  VARUNA 1.2   -
;; - Copyright Abstract, France 1993-2006                                    -
;; - under licence MIT Expat since 2006                                      -
;; - Interface Graphique                                                     -
;; - Definitions des variables globales                                      -
;; ---------------------------------------------------------------------------
;; - Creation: Mars 93                                                       -
;; - Mise a jour pour version 1.07: Decembre 93                              -
;; - Build 1.22.1 01.07.99 / JH MO                                           -
;; ---------------------------------------------------------------------------

;;;  ajt FD - 21/06/95
;;;  ajout des contraintes: meme jour, simultaneite, meme horaire
;;;  => ajt de defmessage, MOD de LISTE-LIBELLE-CONTRAINTE,
;;;    libelle-contrainte2code-contrainte et code-contrainte2libelle-contrainte

;;; chg (24/05/95) mo
;;; j'ai enleve sal-aff qui etait devenu obsolete: ancien liste-salles-aff
;;; chg (09/06/95) mo
;;; j'ai enleve (enfin!) cpt-app

;;; les noms des applications

(defvar Tty-App ())
(defvar Gen-App ())
(defvar Input-App ())

;;; un predicat designant le type de mode resolution 
(defvar Stop-Running ())

(defvar Week-Aff ())
(defvar Liste-Groupes-Aff ())
(defvar Liste-Salles-Aff ())
(defvar Liste-Profs-Aff ())     ;; CRE RM - 04/12/95
(defvar Liste-Resas-Aff ())     ;; xxx FD 31/05/95 (when MODULE-RESA
(defvar Liste-Ajouts-Aff ())    ;; RM 27/07/95
(defvar Liste-Cours-Aff ())

(defvar Jours-Aff ())
(defvar Semaine-Debut-Aff ())
(defvar Semaine-Fin-Aff ())
(defvar Heures-Debut-Aff)
(defvar Grid-Type 'year)
(defvar I-ECOLE)
;; AJT - FD - 01/08/96
(defvar SEM-CALENDAIRES)
(defvar Total-Fiches 0)
(defvar Base 0)
(defvar Aff-Avec-Chevauchement ())  ;; CRE RM - 20/12/95 

(defvar Mode 'resolution)
(defvar Dirty ())

;;; REM (13/03/95) MO
;;; dirty-file sert-il a quelque chose, je le mets en remarque pour voir ...
;(defvar Dirty-file ())

(defvar Saisie-Ok t)
(defvar I-Running ())
(defvar I-Heures ())
(defvar I-Nombre-Cours-Figes 0)
(defvar Impressionp ())
(defvar Format-Impression ())
(defvar Groupe-Reste ())


; Quelques constantes

(defvar app-name "Varuna")
(current-language 'french)


;;; definition des intitulles des contraintes

(defmessage v_non-simultaneite
  (french "Non-chevauchement")
  (english "???"))
(defmessage v_simultaneite
  (french "Simultaneité (début)")
  (english "???"))
(defmessage v_precedence
  (french "Précédence")
  (english "???"))
(defmessage v_jours-differents
  (french "Jours différents")
  (english "???"))
(defmessage v_meme-jour
  (french "Même jour")
  (english "???"))
(defmessage v_meme-semaine
  (french "Même semaine")
  (english "???"))
(defmessage v_proximite-semaine
  (french "Proximité semaine")
  (english "???"))

;;;  FD - 21/06/95
(defmessage v_meme-horaire
  (french "Même horaire")
  (english "???"))

;; RM - 06/09/96
(defmessage v_simultaneite-fin-seance
  (french "Simultanéité (fin)")
  (english "???"))

(defmessage v_k-ecart-simultaneite
  (french "K Alternance")
  (english "???"))
  
(defvar LISTE-LIBELLES-CONTRAINTES 
  (list 
    #Mv_non-simultaneite
    #Mv_simultaneite
    #Mv_simultaneite-fin-seance 		;; RM 06/09/96
    #Mv_jours-differents
    #Mv_meme-jour
    #Mv_meme-semaine
    #Mv_meme-horaire    ;; FD 21/06/95
    ))

(defun libelle-contrainte2code-contrainte (libelle)
  (cond 
    ((equal libelle #Mv_non-simultaneite) 'poser-non-chevauchement)
    ((equal libelle #Mv_meme-semaine) 'poser-meme-semaine)
    ((equal libelle #Mv_precedence) 'poser-precedence)
    ((equal libelle #Mv_proximite-semaine) 'poser-proximite-semaine)
    ((equal libelle #Mv_jours-differents) 'poser-jours-differents)
    ((equal libelle #Mv_meme-jour) 'poser-meme-jour);;AJT FD - 21/06/95
    ((equal libelle #Mv_simultaneite) 'poser-simultaneite);;AJT FD - 21/06/95
    ((equal libelle #Mv_meme-horaire) 'poser-meme-horaire);;AJT FD - 21/06/95
    ((equal libelle #Mv_k-ecart-simultaneite)
      'poser-k-ecart-simultaneite) 		;; AJT RM - 26/03/96
    ((equal libelle #Mv_simultaneite-fin-seance)
      'poser-simultaneite-fin-seance)  		;; AJT RM - 06/09/96
    (libelle (and (nyi libelle) ()))))

(defun code-contrainte2libelle-contrainte (code)
  (selectq code
    ('poser-non-chevauchement #Mv_non-simultaneite)
    ('poser-meme-semaine #Mv_meme-semaine)
    ('poser-precedence #Mv_precedence)
    ('poser-proximite-semaine #Mv_proximite-semaine)
    ('poser-jours-differents #Mv_jours-differents)
    ('poser-meme-jour #Mv_meme-jour);; AJT FD - 21/06/95
    ('poser-simultaneite #Mv_simultaneite);; AJT FD - 21/06/95
    ('poser-meme-horaire #Mv_meme-horaire);; AJT FD - 21/06/95
    ('poser-k-ecart-simultaneite #Mv_k-ecart-simultaneite);; AJT RM - 26/03/96
    ('poser-simultaneite-fin-seance #Mv_simultaneite-fin-seance);;RM - 06/09/96
    (t (string code))))

;;; CRE RM - 06/09/96 : retourne le message d'erreur d'une contrainte

(defun code-contrainte2erreur (code)
  (selectq code
    ('poser-non-chevauchement #Mv_err-non-chevauchement)
    ('poser-meme-semaine #Mv_err-meme-semaine)
    ('poser-precedence #Mv_err-precedence)
    ('poser-proximite-semaine #Mv_err-proximite-semaine)
    ('poser-jours-differents #Mv_err-jours-differents)
    ('poser-meme-jour #Mv_err-meme-jour)
    ('poser-simultaneite #Mv_err-simultaneite)
    ('poser-meme-horaire #Mv_err-meme-horaire)
    ('poser-k-ecart-simultaneite #Mv_err-k-ecart)
    ('poser-simultaneite-fin-seance #Mv_err-simultaneite-fin-seance)
    (t #Mv_err-contrainte)))

(defvar PLUS-GROS 1.5)
(defvar PLUS-PETIT 0.5)

;; xxx FD - 31/05/95 (when MODULE-RESA ??

(defvar COULEUR-RESA)

;; xxx RM - 27/07/95

(defvar COULEUR-AJOUT)

;;; variable pour les items de menus

(defvar couleur-item-inhibe ())
(defvar couleur-item-actif ())
(defvar police-item-inhibe arial8b)
(defvar police-item-actif arial8b)

(defvar couleur-separateur-inhibe ())


(defvar GRAIN-DISPO 30)
(defvar JOURS-OUVRABLES
  '(lundi mardi mercredi jeudi vendredi samedi dimanche))

(defvar NB-SEMAINES 52)

(defvar FICHIER-TEMPORAIRE "varuna.tmp")
(defvar FICHIER-POSTSCRIPT "psdump.ps")
(defvar FICHIER-PS-MULTI "ps.ps")

;; fichier contenant le nombre d'elements MOD 22/02/96
(defvar FICHIER-VARCODE "varcode.trs")

;; fichier contenant la date reelle (du ... au ...)
(defvar DATESFILE "dates.trs")

;; fichier de dates : DATES.EDT
(defvar FICHIER-DATES "dates.edt")

;; fichier contenant le prefixe a ajouter devant chaque code 22/02/96 MultiP
(defvar FICHIER-PREFIXE "prefixe.txt")
;; fichier contenant un prefixe qui indique quels sont les cours qui ne sont
;; pas modifiables (ex. cours du site central) - 01/03/96 - MultiP
(defvar FICHIER-PREFIXE-INTERDIT "interdit.txt")

(defvar COPYRIGHT "Copyright Abstract sarl 1993 - 2006 under licence MIT Expat since 2006")

;;; variable globale ??? a mettre dans VARS.LL
;;; pour pouvoir verifier que l'horaire qui est choisi est ok
;;; rem (06/06/95) MO
;;; c'est pas beau !!!, on doit pouvoir se débrouiller avec des environnements
;;; internes à l'éditeur des résas !!!!!!!!!

(defvar Salles-Disponibles ())

;; FD - 11/07/95
(defvar *contrainte* ())

;;; AJT - FD - 21/07/95
(defvar Liste-Caracteres-Autorises ".-_/àâêçéèëîïôöùû")

;;; 23/02/96 - FD - MultiP
(defvar PREFIXE)
(defvar LECTURE-SEULE?)
(defvar PREFIXE-INTERDIT)

;;; gestion des sites
(defvar TYPES-SALLE-AUTORISES ())
(defvar TYPES-COURS-AUTORISES ())

;;; AJT RM - 23/05/96 : definition de la font des editeurs
(defvar FONT-EDITEUR ())

;;; AJT RM - 19/08/96 : globales definissant l'etat de sortie
;;; couleur et postscript
;;; nom du fichier contenant les options d'impression
;;; ligne de commande pour une impression non postscript

(defvar COULEUR-DEVICE ())
(defvar POSTSCRIPT-DEVICE t)
(defvar FICHIER-PRINTER "printer.ini")
(defvar CMDLINE-NON-PS ())

;;; AJT RM - 15/10/96 : fichier temporaire pour les traces

(defvar FICHIER-TRACE-TMP "trace.tmp")

;;; CRE RM 31/10/97 : liste des erreurs lors d'un deplacement de cours
;;; contraintes non respectees

(defvar lst-err-contraintes ())

(defvar ACCES-REFUSE -1)
(defvar ACCES-AUTORISE 0)

;;; CRE RM 05/05/99 : une resa sur plusieurs semaines ?

(defvar CONFIGURER-RESA t)
(defvar RESA-N-SEMAINES ())
(defvar nb-seances-resa 1)
