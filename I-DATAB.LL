;;; --------------------------------------------------------------------------
;;; - I-datab.ll                                                VARUNA 1.2   -
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; - Le mode saisie                                                         -
;;; - Définition des applications de la base de donees                       -
;;; - des actions d'edition                                                  -
;;; - des editeurs pour chaque concept                                       -
;;; - de la macro saisie pour les groupes specifiques                        -
;;; --------------------------------------------------------------------------
;;; - Creation:  Juillet 94                                                  -
;;; - Mise a jour pour version 1.1:                                          -
;;; - Build 1.22.1 01.07.99 / JH                                             -
;;; --------------------------------------------------------------------------

;;; dbg (16/05/95) mo pour bug ref mo050500.bug
;;; <def-i-editeur> pour les enseignements

;;; - AJT FD-16/04/95 classe <i-resa> pour le module de reservation de salle

;;; 1. Definition des editeurs pour chaque concept
;;; -------------------------------------------


;;; objets cours, salle, prof, groupe
;;; <avant : les quatre fichiers i-cours, i-salles, i-prof, i-fili>
  
;;;                       
;;; la liste a passer en argument de la macro est du type :
;;; ((champ type + parametres)          (ligne colone nb-lignes nb-colones)) 
;;; ex : la fenetre cours a 15 lignes 2 colones
;;; sa taille est de 500x300 points 
;;;                                     


;;; OBJET COURS 
;;; ------------

;;; Le concept

(def-iclasse i-cours "c"
  (type symbol)                         ; type du cours
  (type-salle symbol)                   ; type de la salle requise
  (audience integer)                    ; audience attendue
  (duree integer)                       ; duree de la seance type
  (nb-seances integer)                  ; nombre de seances
  (profs cons)                          ; profs enseignant
  (groupes cons)
  ;; MOD FD - RM 14/05/96 : DISPO-PLEINE -> DISPO-ECOLE
  (dispo cons DISPO-ECOLE)              ; disponibilite de ce cours
  (instant-trouve integer)              ; instant trouve du cours
  (fige integer)                        ; joue aussi le role de flag pour
                                        ; un cours fige
  (salle-trouvee symbol)                ; salle trouvee pour le cours
  (salle-figee symbol)                  ; salle souhaitee (en saisie)
  (salle-figee-reelle symbol)		; salle figee reelement 
  (index integer)
  (commentaires ()))

;; fonction d'impression (utiles pour l'ecriture fichier)

(defun #:tclass:i-cours:prin (i-cours)
  (when i-cours
    (prin (code-i-cours i-cours))))

;;; AJT 29/08/95 : methode qui retourne le code d'un objet sous forme de string

(defun #:tclass:i-cours:objet2string (i-cours)
   (when i-cours
     (string (code-i-cours i-cours))))

;; La fenetre

(def-editeur i-cours standard controle-i-cours 18 2 500 350  
  ((code symbole t t)                                  (2 1 1 1))
  ((libelle chaine t t)                                (3 1 1 2))
  ((nb-seances entier 1
    (nb-seances-max) t t t () ())                      (4 1 1 1))
  ((duree horaire Unite-Temps (duree-max) t t)         (5 1 1 1))
  ((type-salle enum (liste-types-salle-autorises)
    () t t 'valider-type-salle ())                      (6 1 1 1))
  ((type enum (liste-types-cours-avec-libelle) () () t 'tester-type-cours t)
                                                       (4 2 1 1))
  ((audience entier 1 () t t t () ())                          (5 2 1 1))
  ((salle-figee enum (liste-code-salle) () () () 'tester-acces-objet t)
                                                       (6 2 1 1))
  ((profs liste-i-prof 'cours-i-prof t () () () ())          (7 1 6 1))
  ((groupes liste-i-groupe 'cours-i-groupe t () () () ())    (7 2 6 1))
  ((dispo disponibilite #Mv_du-cours 'libelle-i-cours) (15 2 2 1))
  ((commentaires texte)                                (14 1 3 1)))

;; Les actions

(actions-edition i-cours standard () () supprimer-i-cours-special ())


;;; OBJET TYPE DE ECOLE ET TYPE DE COURS
;;; -------------------

(def-iclasse i-type-cours "tc"
  (liste-moments-cumules ())
  (index () 1))


; (def-editeur i-type-cours standard vrai 16 1 300 450
(def-editeur i-type-cours standard controle-i-type-cours 16 1 300 450
  ((code symbole t t)                 (2 1 1 1))
  ((liste-moments-cumules liste
    'demande-moments-cumules
    'libelle-moment2moment-cumule
    'tri-moment-cumule
    'libelle-moment-cumule t t)       (4 1 11 1))
  ((libelle chaine t t)               (3 1 1 1)))

;;; AJT 31/10/95 : methode qui retourne le code d'un objet sous forme de string

(defun #:tclass:i-type-cours:objet2string (i-type-cours)
   (when i-type-cours
     (string (code-i-type-cours i-type-cours))))

(actions-edition i-type-cours standard () () () ())


;;; OBJET ECOLE (LES VALEURS PAR DEFAUT SE TROUVENT DANS LE I-ECOLE.LL)
;;; --------------------------------------------------------------------
(def-iclasse i-ecole "ecole"
  (unite-temps ())
  (param-chevauchement ())
  (param-chevauchement-max ())
  (horaire-cumule-pause ())
  (semaine-debut ())
  (semaine-fin ())
  (dispo ())
  (types-cours ())
  (jours-ouvres ())
  (index ())
  (grain-dispo ())
; les champs calcules a ecrire dans le fichier
; avec variables par defaut HardCodees
  
  (libelle-ecole ())
  (dispo-ecole ())
  (liste-moments-debut-ecole ())
  (liste-codes-types-cours ())
  (liste-libelles-types-cours ())
  (semaines-debut-ecole ())
  ;; AJT - FD - 26/07/96 - pour semaines calendaires
  (periodes-calendaires ())
  (calendaire ())  ;; booleen pr savoir si nous sommes en semaines calendaires
  (pause ()))					;; CRE RM 17/12/96

;;; FD - 22/05/95  !!!! ASTUCE !!!! ASTUCE !!!!
;;; le champ semaine-fin a ete mis a la fin pour resoudre un probleme
;;; de gestion de focus qui plantait quand le dernier champ de
;;; de l'editeur n'est pas selectionnable.
;;; Il faut aller voir dans i-util.ll la gestion de focus pour voir
;;; ce qui cloche.

(def-editeur i-ecole standard controle-i-ecole 10 2 600 400
  ((libelle chaine t ())                      (2 1 1 2))
  ((types-cours liste-i-type-cours () () t t 'supprimer-i-type-cours-special 'ajouter-i-type-cours-special)
                                              (4 1 5 1))
  ((dispo
    liste
    'demande-intervalles
    'libelle-intervalle2intervalle
    'tri-intervalle
    'intervalle2libelle t t)                 (4 2 5 1))
  ((semaine-fin entier 1 NB-SEMAINES t t () () ())    (3 1 1 1))
  ((calendaire bool)                         (3 2 1 1))) 

(def-editeur i-ecole owner vrai 5 2 500 200
  ((libelle chaine t t)                          (2 1 1 2))
  ((unite-temps entier 5 240 () t t () ())               (3 1 1 1))
  ((param-chevauchement entier 1 () () () t () ())       (4 1 1 1))
  ((param-chevauchement-max entier 1 () () () t () ())   (3 2 1 1))
  ((grain-dispo entier 1 () () () t () ())               (4 2 1 1)))

(actions-edition i-ecole standard () () () ()) 

;;; OBJET SALLE
;;; -----------

(def-iclasse i-salle "s"
  (type symbol)                         ; type de salle
  (capacite integer)                    ; capacite de la salle 
  (dispo () DISPO-PLEINE)               ; disponibilite de la salle
  (index integer)
  (commentaires ())
  (liberte ()))

;;; MOD - FD - 29/02/96 ajout du controle

(def-editeur i-salle standard controle-i-salle 14 1 300 300
  ((code symbole t t)                     (2 1 1 1))
  ((libelle chaine t t)                   (3 1 1 1))
  ((capacite entier 1 () t t t () ())             (4 1 1 1))
  ((type enum (liste-types-salle-autorises) () t t 'valider-type-salle ())
                                          (5 1 1 1))
  ((commentaires texte)                   (6 1 3 1))) 

(defun #:tclass:i-salle:prin (i-salle)
  (when i-salle
    (prin (code-i-salle i-salle))))

;;; AJT 29/08/95 : methode qui retourne le code d'un objet sous forme de string

(defun #:tclass:i-salle:objet2string (i-salle)
   (when i-salle
     (string (code-i-salle i-salle))))

(actions-edition i-salle standard () () supprimer-i-salle-special ())


;;; OBJET PROF
;;; ----------
  
(def-iclasse i-prof "p"
  (civilite string)                     ; Mme, M. ou Mle
  (cours ())                            ; ens des cours de l enseignant
  ;; MOD FD - RM 14/05/96 : DISPO-PLEINE -> DISPO-ECOLE
  (dispo cons DISPO-ECOLE)              ; disponibilite de l'enseignant
  (index integer)
  (commentaires ()))
 
(def-editeur i-prof standard controle-i-prof 12 3 400 400
  ((code symbole t t)                                  (2 1 1 3))
  ((libelle chaine t t)                                (3 1 1 3))
  ((civilite enum (liste-civilite-prof) t t () 'vrai ())
                                                       (4 1 1 1))
  ((dispo disponibilite #Mv_du-prof 'libelle-i-prof)   (9 3 1 1))
  ((cours liste-i-cours 'profs-i-cours t () () () ())        (5 1 3 3))
  ((commentaires texte)                                (9 1 2 2)))

(defun #:tclass:i-prof:prin (i-prof)
  (when i-prof
    (prin (code-i-prof i-prof))))

;;; AJT 29/08/95 : methode qui retourne le code d'un objet sous forme de string

(defun #:tclass:i-prof:objet2string (i-prof)
   (when i-prof
     (string (code-i-prof i-prof))))

(actions-edition i-prof standard () () supprimer-i-prof-special ())


;;; OBJET GROUPE
;;; ------------
;;; la "macro-saisie" est implementee en...
;;;
;;; Les champs de macro saisie sont defini dans l'objet groupe
;;; on defini ensuite un editeur pour chaque sous-objet
;;; les macro qui genere les fenetres et les fonctions d'edition
;;; supportent une gestion "personnalisee" des objet
;;; voir la macro actions-edition


(def-iclasse i-groupe "gr"
  (cours cons)                      ; cours appartenant au groupe
  (groupes cons)                    ; groupes appatneant au groupe
  (contraintes cons)                ; contraintes implicites sur les groupes
  ;; MOD FD - RM 14/05/96 : DISPO-PLEINE -> DISPO-ECOLE
  (dispo cons DISPO-ECOLE)          ; disponibilite du groupe
  (index integer)
  (commentaires ())
  (type-cours ())

;;; LES CHAMPS DE MACRO SAISIE
  
  (type-macro ())                     ; type de la macro saisie
  
;;; GROUPE = ENSEIGNEMENT
  
  (type-salle ())                       ; type de salle
  (nb-seances ())                       ; nombre de seances de l'enseignement 
  (audience ())                         ; audience de l'enseignement
  (durees ())                           ; duree de chaque seance
  (profs ())                            ; ses profs

;;; XXX BUG 4 ATTENTION IL FAUT BIEN VOIR COMMENT CELA SE GENERE SUR LES COURS
;;; (salle-figee ())


;;; GROUPE = FILIERE
                                        ; aucun champs
                                        ; tout est deja defini dans le groupe
;;; GROUPE = GROUPE TD A
                                        ; on a deja defini les profs
  (duree ())                            ; la duree de chaque td
  (nb-td ())                            ; le nombre de td a generer
  
;;; GROUPE = GROUPE TD B
                                        ; on a deja defini le nombre de td
                                        ; et la duree
  )


;;; La fenetre pour un groupe standard
  
(def-editeur i-groupe standard controle-i-groupe 16 2 600 450
  ((code symbole t t)                               (2 1 1 1))
  ((libelle chaine t t)                             (3 1 1 2))
  ((cours liste-i-cours 'groupes-i-cours t () () () ())   (4 1 8 1))
  ((groupes liste-i-groupe () t () () () ())              (8 2 6 1))
  ((commentaires texte)                             (13 1 2 1))
  ((dispo disponibilite #Mv_du-groupe 'libelle-i-groupe) (15 2 1 1))

;;; XXX BUG 4
;;; ((salle-figee enum (liste-libelle-salle) () () ()) (x x x x))

  ((contraintes liste
    'demande-contrainte
    'libelle-contrainte2code-contrainte
    ()
    'code-contrainte2libelle-contrainte
    ()
    t)                                          (4 2 3 1)))

;;; La fenetre pour un groupe de type filiere

(def-editeur i-groupe filiere controle-i-groupe 16 2 600 450
  ((code symbole t t)    (2 1 1 1))
  ((libelle chaine t t)  (3 1 1 2))
  ((cours liste-i-cours 'groupes-i-cours t () () () ()) (4 1 10 1))
  ((groupes liste-i-groupe () t () () () ())            (4 2 5 1))
  ((commentaires texte)                           (10 2 3 1))
  ((dispo disponibilite #Mv_de-la-filiere 'libelle-i-groupe)
                                                  (14 2 1 1)))

;;; Fenetre pour un groupe enseignement...
;;; dbg (16/05/95) mo pour bug ref mo050500.bug

(def-editeur i-groupe enseignement controle-i-groupe 16 2 500 450
  ((code symbole t t)                                    (2 1 1 1))
  ((libelle chaine t t)                                  (3 1 1 2))
  ((nb-seances entier 1 (nb-seances-max) t t t () ())            (4 1 1 1))
  ((type-salle enum (liste-types-salle-autorises) () t t 'tester-type-salle ())
                                                         (5 1 1 1))
  ((type-cours enum (liste-types-cours-avec-libelle) () () t 'tester-type-cours t)
                                                         (4 2 1 1))
  ((audience entier 1 () t t t () ())                            (5 2 1 1))
  ((profs liste-i-prof () t () () () ())                       (6 1 6 1))
  ((commentaires texte)                                  (13 1 2 1))
  ((dispo disponibilite
          #Mv_de-l-enseignement 'libelle-i-groupe)
                                                         (13 2 1 1))
   ((durees liste
    'demande-duree-cours
    'naturel2cumule
    ()
    'cumule2naturel
    
;;; dbg (16/05/95) mo pour bug ref mo050500.bug
;;; le denier flag est mis a () au lieu de t pour permettre la saisie de
;;; memes durees pour chacune des seances de l'enseignement

    () ())                                                  (6 2 6 1)))
    
;;; td simultanes

(def-editeur i-groupe td-a controle-i-groupe 12 2 600 350
  ((code symbole t t)                           (2 1 1 1))
  ((libelle chaine t t)                         (3 1 1 2))
  ((dispo disponibilite
    #Mv_du-groupe-td 'libelle-i-groupe)         (8 2 1 1)) 
  ((nb-seances entier 1 (nb-seances-max) t t t () ())   (4 1 1 1))
  ((duree horaire Unite-Temps (duree-max) t t)  (5 1 1 1))
  ((nb-td entier 1 100 t t t () ())                     (6 1 1 1))
  ((type-salle enum (liste-types-salle-autorises) () t t 'tester-type-salle ())
                                                (4 2 1 1))
  ((audience entier 1 () t t t () ())                   (5 2 1 1))
  ((type-cours enum (liste-types-cours-avec-libelle) () () t 'tester-type-cours t) (6 2 1 1)) 
  ((commentaires texte)                         (7 1 3 1)))

;;; td meme semaine

(def-editeur i-groupe td-b controle-i-groupe 12 2 600 350
  ((code symbole t t)                           (2 1 1 1))
  ((libelle chaine t t)                         (3 1 1 2))
  ((dispo disponibilite
    #Mv_du-groupe-td 'libelle-i-groupe)         (8 2 1 1)) 
  ((nb-seances entier 1 (nb-seances-max) t t t () ())   (4 1 1 1))
  ((duree horaire Unite-Temps (duree-max) t t)  (5 1 1 1))
  ((nb-td entier 1 100 t t t () ())                     (6 1 1 1))
  ((type-salle enum (liste-types-salle-autorises) () t t 'tester-type-salle ())
                                                (4 2 1 1))
  ((audience entier 1 () t t t () ())                   (5 2 1 1))
  ((type-cours enum (liste-types-cours-avec-libelle) () () t 'tester-type-cours t)
                                                (6 2 1 1)) 
  ((commentaires texte)                         (7 1 3 1)))

;;; CRE RM - 26/03/96
;;; Fenetre pour un groupe alternance (seance 1 semaine sur n)

(def-editeur i-groupe alternance controle-i-groupe 12 2 500 350
  ((code symbole t t)                                    (2 1 1 1))
  ((libelle chaine t t)                                  (3 1 1 2))
  ((nb-seances entier 1 (nb-seances-max) t t t () ())            (4 1 1 1))
  ((nb-td entier 1 10 t t t () ())                               (4 2 1 1)
    i-frequence)
  ((duree horaire Unite-Temps (duree-max) t t)           (5 1 1 1))
  ((audience entier 1 () t t t () ())                            (5 2 1 1))
  ((type-salle enum (liste-types-salle-autorises) () t t 'tester-type-salle ())           (6 1 1 1))
  ((type-cours enum (liste-types-cours-avec-libelle) () () t 'tester-type-cours t)          (6 2 1 1))
  ((profs liste-i-prof () t () () () ())                    (7 1 4 1))
  ((commentaires texte)                                  (9 2 2 1))
  ((dispo disponibilite
          #Mv_de-l-alternance 'libelle-i-groupe)
                                                         (7 2 1 1)))



(defun #:tclass:i-groupe:prin (i-groupe)
  (when i-groupe
    (prin (code-i-groupe i-groupe))))
  
;;; AJT 29/08/95 : methode qui retourne le code d'un objet sous forme de string

(defun #:tclass:i-groupe:objet2string (i-groupe)
   (when i-groupe
     (string (code-i-groupe i-groupe))))

(actions-edition i-groupe standard 
  editer-i-groupe-special
  ajouter-i-groupe-special
  supprimer-i-groupe-special
  copier-i-groupe-special)

(defvar Liste-Contraintes)



;;; OBJET RESA
;;; ----------
;;;


(def-iclasse i-resa "res"
  (instant integer)
  (duree integer)
  (salle symbol)
  (index integer)    
  (semaine ())
  (audience ())
  (jour ())
  (horaire1 ())
  (horaire2 ())
  (type-cours ())
  (type-salle ())
  (salles-disponibles ())
  (salle-resa ())
  (heure-resa ())
  (commentaires ()))

;;; Fenetre pour une reservation

(def-editeur-resa i-resa standard
    reserver          ;; fonction de controle
    rechercher        ;; fonction de recherche 
    '(salle heure-resa)
    '(salles-disponibles)
    36 10 620 500
    (" DEMANDE"    ;;; glob
    ("obligatoire"    ;;; glob
      ((duree horaire Unite-Temps (duree-max) t t)     (4 1 1 4))
      ((semaine entier 1 (nb-seances-max) t t t
	'lire-numero-semaine 'ecrire-numero-semaine)       (6 1 1 4))
      ((jour enum (mapcar 'string 
                      (mapcar 'libelle-jour Jours-Ouvres))
                  () () t () ())                            (6 5 1 4)))
    ("facultatif"    ;;; glob
      ((horaire1 horaire Unite-Temps () () t)          (10 1 1 3))
      ((horaire2 horaire Unite-Temps () () t)          (10 4 1 2))
      ((type-cours enum (liste-types-cours-avec-libelle) () () () 'tester-type-cours t)
	                                               (12 1 1 4))
      ((audience entier 0 (capacite-max) () () t () ())       (12 5 1 4))
      ((salle-resa enum ;(mapcar 'string
                        ;  (mapcar 'libelle-i-salle
                        ;    Liste-I-Salle)) 
                        (liste-code-salle) () () () 'tester-acces-objet t)
                                                       (14 1 1 4))
      ((type-salle enum
	(liste-types-salle-autorises) () () () 'tester-type-salle ())
                                                       (14 5 1 4))))
    (" RECHERCHE"    ;;; glob
      ((salles-disponibles listefd1 
           'salle       ;; nom du champ qui est mis a jour lors de la selection
           (lambda () ())t
           ()
           (mapcar 'libelle-i-salle
               liste-i-salle)
           '()
           '())                                     
                                                     (3 1 9 10)))
    (" RESERVATION"     ;;; glob                   
      ((libelle chaine t t)                          (3 1 1 6))
      ((code symbole t t)                            (3 7 1 2))
                                                   
      ((salle chaine t ())                           (5 1 1 4))
      ((heure-resa horaire Unite-Temps () t t)         (5 5 1 4))))

;; fonction d'impression (utiles pour l'ecriture fichier)

(defun #:tclass:i-resa:prin (i-resa)
  (when i-resa
    (prin (code-i-resa i-resa))))


;;; AJT 29/08/95 : methode qui retourne le code d'un objet sous forme de string

(defun #:tclass:i-resa:objet2string (i-resa)
   (when i-resa
     (string (code-i-resa i-resa))))

(actions-edition i-resa standard () () () ())
       
;;; OBJET ANNULATION DE SEANCE
;;; --------------------------
;;; CRE RM - 24/07/95
;;; Definition du concept d'annulation de seance pour le rattrapage de
;;; cours.

(def-iclasse i-annulation "ann"
    (index integer)
    (cours ())     	;  cours dont on annule une seance
    (code-cours symbol)    ; symbole du cours dont on annule une seance
    (libelle-cours ())     ; libelle du cours dont on annule une seance
    (semaine chaine)       ; instant de la seance annulee
    (duree integer)        ; duree de la seance annulee
    (instant integer)
    (rattrapage ())        ; booleen sur le rattrapage imm‚diat  
    (commentaires ())
    (flag integer))	   ; vaut t si creation automatique

;; fonction d'impression (utiles pour l'ecriture fichier)

(defun #:tclass:i-annulation:prin (i-annulation)
  (when i-annulation
    (prin (code-i-annulation i-annulation))))

;;; AJT 29/08/95 : methode qui retourne le code d'un objet sous forme de string

(defun #:tclass:i-annulation:objet2string (i-annulation)
   (when i-annulation
     (string (code-i-annulation i-annulation))))


;; La fenetre

(def-editeur i-annulation standard
  annuler 	;;; fonction de controle
  11 4 400 250
  ((libelle-cours chaine t ())                              (2 1 1 4))
  ((code symbole t t)                                       (3 1 1 2))
  ((libelle chaine t t)                                     (4 1 1 4))  
  ((code-cours symbole () ())                               (3 3 1 2))  
  ((semaine enum (liste-semaines-seance cours) t () t () ()) (5 1 1 2))
  ((duree horaire Unite-Temps (duree-max) t () )            (5 3 1 2))
  ((rattrapage bool)                                        (10 2 1 3))
  ((commentaires texte)                                     (6 1 3 4)))

;; Les actions

(actions-edition i-annulation standard () () () ())

;;; OBJET AJOUT DE SEANCE
;;; -----------------------
;;; CRE - FD - 24/07/95


(def-iclasse i-ajout "ajt"
  (index integer)    
  (cours-rattrape symbol)
  (instant-cours-rattrape integer)
  (duree interger)
  (semaine ())    
  (jour ())
  (profs cons)
  (horaire1 ())
  (horaire2 ())
  (audience ())
  (type-salle ())
  (contraintes-familles ())
  (dispo-cours-familles ())
  (dispo-profs ())
  (salles-disponibles ())
  (salle symbol)
  (heure-ajout ())
  (instant integer)
  (flag interger))          ; vaut t si creation automatique

;;; Fenetre pour un ajout de seance

(def-editeur-resa i-ajout standard
    ajouter                 ;;; fonction de controle
    rechercher-ajout       ;;; fonction de recherche
    '(salle heure-ajout)       
    '(salles-disponibles)
    42 10 620 500
    (" AJOUT DE SEANCE"    ;;; glob
    ("obligatoire"    ;;; glob
      ((duree horaire Unite-Temps (duree-max) t t)     (4 1 1 4))
      ((semaine entier 1 (nb-seances-max) t t t
	'lire-numero-semaine 'ecrire-numero-semaine)         (6 1 1 4))
      ((jour enum (mapcar 'string 
                      (mapcar 'libelle-jour Jours-Ouvres))
                  () () t () ())                              (6 5 1 4)))
    ("facultatif"    ;;; glob
      ((horaire1 horaire Unite-Temps () () t)          (10 1 1 3))
      ((horaire2 horaire Unite-Temps () () t)          (10 4 1 2))
      ((contraintes-familles bool)                     (16 5 1 4))
      ((dispo-cours-familles bool)                     (18 5 1 4))
      ((dispo-profs bool)                              (20 5 1 4))
      ((audience entier 0 (capacite-max) () () t () ())        (12 5 1 4))
      ((type-salle enum (liste-types-salle-autorises)
	                () () () 'tester-type-salle ())   (14 5 1 4))
      ((profs liste-i-prof () t () () () ())      (12 1 8 4))))
    (" CRENEAUX DISPONIBLES"    ;;; glob
      ((salles-disponibles listefd1 
           'salle       ;; nom du champ qui est mis a jour lors de la selection
           (lambda () ())t
           ()
	   (makelist (length liste-i-salle) "")
	   ; (mapcar 'libelle-i-salle
	   ;     liste-i-salle)
           '()
           '())                                     
                                                     (3 1 8 10)))
    (" SEANCE AJOUTEE"     ;;; glob                   
      ((libelle chaine t t)                          (3 1 1 6))
      ((code symbole t t)                            (3 7 1 2))
                                                   
      ((salle chaine t ())                           (5 1 1 4))
      ((heure-ajout horaire Unite-Temps () t t)         (5 5 1 4))))


;; fonction d'impression (utiles pour l'ecriture fichier)

(defun #:tclass:i-ajout:prin (i-ajout)
  (when i-ajout
    (prin (code-i-ajout i-ajout))))

;;; AJT 29/08/95 : methode qui retourne le code d'un objet sous forme de string

(defun #:tclass:i-ajout:objet2string (i-ajout)
   (when i-ajout
     (string (code-i-ajout i-ajout))))

(actions-edition i-ajout standard () () () ())
       
;;; AJT - FD - 03/06/96
;;; concept de site

(def-iclasse i-site "site"
  (index integer)
  (numero-dd symbol)
  (type symbol)           	;; central ou secondaire
  (types-cours cons) 		;; liste des types de cours accessibles
  (types-salles cons))	        ;; liste des types de salles accessibles

;; fonction d'impression
(defun #:tclass:i-site:prin (i-site)
  (when i-site
    (prin (code-i-site i-site))))

;;; methode qui retourne le code d'un objet sous forme de string

(defun #:tclass:i-site:objet2string (i-site)
   (when i-site
     (string (code-i-site i-site))))

(def-editeur i-site standard controle-i-site 26 9 350 350
  ((code symbole t t)                   (2 1 1 8))
  ((libelle chaine t t)                 (4 1 1 8))
  ((types-cours liste
    'tester-demande-type-cours
    'tester-sortie-type-cours
    ()
    'tester-print-type-cours () ())
                                        (7 1 6 8))
  ((types-salles liste
    'tester-demande-type-salle
    'tester-sortie-type-salle
    ()
    'tester-print-type-salle () ())
                                        (15 1 6 8))
  ((numero-dd chaine t t)             (24 1 1 8)))

(actions-edition i-site standard () () () ())

;;; AJT - FD - 30/07/96
;;; concept de site

(def-iclasse i-calendaire "cal"
  (index integer)
  (periodes ())		;; liste d'intervalles de semaines calendaires
  (premiere-sem integer))

;; fonction d'impression
(defun #:tclass:i-calendaire:prin (i-calend)
  (when i-calend
    (prin (code-i-calendaire i-calend))))

;;; methode qui retourne le code d'un objet sous forme de string

(defun #:tclass:i-calendaire:objet2string (i-calend)
   (when i-calend
     (string (code-i-calendaire i-calend))))

(def-editeur i-calendaire standard controle-i-calendaire 26 9 200 300
  ((code symbole t t)                   (-1 -1 0 0))
  ((periodes liste
    'demande-intervalle-semaines
    'libelle-intervalle-semaines2intervalle-semaines
    ()
    'intervalle-semaines2libelle t t)
                                        (2 1 19 8))
  ((premiere-sem entier 1 NB-SEMAINES t t t () ()) (23 1 1 8)))

(actions-edition i-calendaire standard () () () ())

          