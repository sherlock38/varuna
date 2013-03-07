;;; --------------------------------------------------------------------------
;;; - Frontal.ll                                                VARUNA 1.2   -
;;; - le frontal de varuna: point d'entree du logiciel                       - 
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; --------------------------------------------------------------------------
;;; - Creation:  Mai 1995 FD MO                                              -
;;; - Derniere mise a jour pour version 1.13: Mai 1995                       -
;;; - Build 1.22.1 01.07.99 / MO FD                                          -
;;; --------------------------------------------------------------------------

;;; <VARUNA>
;;; la primitive a lancer
;;; elle met a jour:
;;;     - les variables globales selon les clefs associ‚es au poste
;;;     - les variables globales selon les droits d'acces de l'utilisateur
;;;     - la constante VARUNA-LANGUE, langue de communication avec
;;;       l'utilisateur, pour le moment (30/05/95) le Fran‡ais

(defun varuna ()
  (ifn (check-varuna-p)
    (end)
    (initialiser-messages)		; initialisation des messages
    (maj-varuna-selon-clefs)		; verification des cles d'acces
    (maj-varuna-selon-droits)		; verification des doits
    (maj-varuna-selon-version)		; initialisation de la version
    (maj-varuna-langue)			; initialisation de la langue
    (maj-varuna-repertoires)		; initialisation des repertoires
    (initialiser-polices)		; initialisation des polices
    (current-font FONT-EDITEUR)		; definition de la police courante
    (setq {asker}:font FONT-EDITEUR)    ; evite l'ecrasement en 800x600
    (initialiser-couleurs)		; initialisation des couleurs
    (initialiser-icones)		; initialisation des icones
    (initialiser-curseurs)		; initialisation des curseurs
    (init-prefixe-code)			; initialisation du prefixe de code
    (rechercher-site)			; recherche du site connecte
    (when MODULE-RESEAU
      (init-semaphores))
    (when
      (not (interface-varuna () ()))	; initialisation de Varuna
      (end))))

;;; Verification de l'environnement Varuna

(defun check-varuna-p ()
  (eq (funcall 'checkvaruna) 0))

;;; Verification des cles d'acces

(defun maj-varuna-selon-clefs ()

  (if (eq (funcall 'mygale "EDITIONS-VAR") 12)
    (setq MODULE-EDITIONS t)
    (setq MODULE-EDITIONS ()))

  (if (eq (funcall 'mygale "RESERVAT-VAR") 12)
    (setq MODULE-RESA t)
    (setq MODULE-RESA ()))

  (if (eq (funcall 'mygale "RATTRAPC-VAR") 12)
    (setq MODULE-RACO t)
    (setq MODULE-RACO ()))
  
  (if (eq (funcall 'mygale "CONCEPTION-VAR") 14)
    (setq MODULE-CONCEPTION t)
    (setq MODULE-CONCEPTION ()))

  (if (eq (funcall 'mygale "SAISIE-VAR") 10)
    (setq MODULE-SAISIE t)
    (setq MODULE-SAISIE ()))

  (if (eq (funcall 'mygale "CONSULT-VAR") 11)
    (setq MODULE-CONSULTATION t)
    (setq MODULE-CONSULTATION ()))

  (if (eq (funcall 'mygale "AFFICHAGE-VAR") 13)
    (setq MODULE-AFFICHAGE t)
    (setq MODULE-AFFICHAGE ()))

  (if (eq (funcall 'mygale "SALLES-VAR") 10)
    (setq MODULE-SALLES t)
    (setq MODULE-SALLES ()))

  (if (eq (funcall 'mygale "RESEAU-VAR") 10)
     (setq MODULE-RESEAU t)
     (setq MODULE-RESEAU ()))

  (if (eq (funcall 'mygale "GESTSITE-VAR") 12)
    (setq MODULE-GESTSITE t)   
    (setq MODULE-GESTSITE ()))

  (if (eq (funcall 'mygale "TRACE-VAR") 9)
    (setq MODULE-TRACE t)   
    (setq MODULE-TRACE ()))
)

;;; Verification des droits (un jour)

(defun maj-varuna-selon-droits ()
  ())

;;; Initialisation de la version (un jour une version light ?)

(defun maj-varuna-selon-version ()
  (setq LISTE-ITEMS-INHIBES-VERSION ()))

;;; Initialisation de la langue (un jour un Varuna en chinois ?)

(defun maj-varuna-langue ()
  (setq VARUNA-LANGUE 'french)
  (current-language VARUNA-LANGUE))

;;; Initialisation des repertoires

(defun maj-varuna-repertoires ()
  (setq REP-VARUNA (string (getenv "VARUNA")))
  (setq REP-DONNEES (string (getenv "VARDATA")))
  (setq REP-EDITIONS (string (getenv "VAREDIT")))
  (setq REP-DLL (string (getenv "VARDLL")))
  (current-directory REP-DONNEES)
  (repertoire-donnees (catenate REP-DONNEES "\\"))
  (repertoire-tmp (catenate REP-VARUNA "\\")))

;;; intiialisation du PREFIXE valable pour tous les codes

(defun init-prefixe-code ()
  (with ((current-directory REP-VARUNA))
    (if (probefile FICHIER-PREFIXE)
      (with ((inchan (openi FICHIER-PREFIXE)))
	(setq PREFIXE (catenate (string (read)) "-"))
	(close))
      (setq PREFIXE ""))))

