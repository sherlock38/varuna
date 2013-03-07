;;; --------------------------------------------------------------------------
;;; - i-transa.ll                                               VARUNA 1.2   -
;;; - les fonctions d'integrite de donnees de VARUNA                         -
;;; - pour la version en reseau                                              -
;;; --------------------------------------------------------------------------
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; - Creation:  Aout 95 RM                                                  -
;;; - Build 1.22.1 01.07.99 / RM                                             -
;;; --------------------------------------------------------------------------

;;; Variables globales

(defvar Liste-Objets-Traces '())	; liste des objets crees et modifies

(defvar VARLOGIN "varlogin.trs")
(defvar TRANSFILE "varuna.trs")
(defvar TRANSFILE-TMP "cleanup.$$$")

(defvar INIT-SEM "init.sem")		; 0
(defvar EDT-SEM "edt.sem")		; 1
(defvar RESOLUTION-SEM "resoud.sem")	; 2
(defvar VARUNA-SEM "varuna.sem")	; 3

(defvar CREATION 0)
(defvar MODIFICATION 1)
(defvar SUPPRESSION 2)

(defvar ETAT-NORMAL 0)
(defvar ETAT-MODIF 1)
(defvar ETAT-SUPPR 2)

(defvar SITE "")

(defmessage v_modif-acces-refuse
  (french "Modification de donnée impossible :")
  (english "???"))

(defmessage v_sup-acces-refuse
  (french "Suppression de donnée impossible :")
  (english "???"))

(defmessage v_duplique-acces-refuse
  (french "Copie de donnée impossible :")
  (english "???"))

(defmessage v_donnee
  (french "la donnée ")
  (english "???"))

(defmessage v_modifiee
  (french " est en cours de modification ")
  (english "???"))

(defmessage v_modifiee2
  (french "est en cours de modification ")
  (english "???"))

(defmessage v_relation
  (french " ou l'un de ses composants")
  (english "???"))

(defmessage v_autre-utilisateur
  (french "sur un autre poste.")
  (english "???"))

(defmessage v_site-deja-connecte
  (french "Site deja connecté.")
  (english "???"))

(defmessage v_acces-edt-interdit
  (french "Accès aux fichiers de données impossible.")
  (english "???"))

;;; Fonctions externes de la DLL <transac.dll>

;;; Fonctions de haut niveau pour la gestion des donnees
;;; en reseau

;;; Lors de la connexion a un jeu de donnees

(defun debut-transaction (site)
  (if LECTURE-SEULE?
    t
    (funcall 'debut_transaction site)))

;;; Lors de la deconnexion d'un jeu de donnees

(defun fin-transaction (site)
  (if LECTURE-SEULE?
    t
    (funcall 'fin_transaction site)))

;;; Lorsque l'on commence a modifier une donnee
;;; AJT - FD - 15/01/96 - mise a jour d'une liste des objets modifies
;;; AJT - RM - 14/10/96 - trace des modifications
;;; MOD RM 18/08/98 : verification complete lors de la modif d'une data

(defun debut-modification (site donnee type)
  (if LECTURE-SEULE?
    ACCES-AUTORISE
    (let
      ((acces)
       (trace))
      (maj-liste-modifications donnee type)
      (setq acces (autoriser-acces-complet site donnee type))
      (when (and MODULE-TRACE (eq acces ACCES-AUTORISE))
	(trace-donnee donnee type))
      acces)))
    
;;; Lorsque l'on termine de modifier une donnee

(defun fin-modification (site donnee)
  (if LECTURE-SEULE?
    t
    (if (symbolp donnee)
      (funcall 'fin_modif SITE (string donnee))
      (funcall 'fin_modif SITE (send 'objet2string donnee)))))


;;; Recherche du nom du site (login de l'utilisateur)

(defun rechercher-site ()
  (when MODULE-RESEAU
    (funcall 'recupere_site)
    (with ((current-directory REP-VARUNA))
      (when (probefile VARLOGIN)
	(with ((inchan (openi VARLOGIN)))
	  (setq SITE (string (readstring)))
	  (close))
	(deletefile VARLOGIN)))))

;;; rechercher l'etat courant de l'objet  
;;; travail avec le fichier VARUNA.TRS se trouvant dans le repertoire de
;;; donnees
;;; 18/01/96

(defun etat-courant-objet (objet)
  (if (symbolp objet)
    (funcall 'etat_courant_objet SITE (string objet))
    (funcall 'etat_courant_objet SITE (send 'objet2string objet))))

;;; Recherche de l'etat d'un objet lors de l'enregistrement
;;; travail avec la copie de VARUNA.TRS qui se trouve dans le repertoire
;;; VARUNA

(defun etat-objet (objet)
  (if (symbolp objet)
    (funcall 'etat_objet SITE (string objet))
    (funcall 'etat_objet SITE (send 'objet2string objet))))

;;; Creation des semaphores si MODULE-RESEAU

(defun init-semaphores ()
  (mapc
    (lambda (file)
      (when (not (probefile file))
	(creer-fichier file)))
    (list INIT-SEM EDT-SEM VARUNA-SEM)))

;;; Autoriser l'acces aux fichiers (lever un semaphore)

(defun autoriser-acces (fichier)
  (let
    ((retour))
    (cond
      ((equal fichier INIT-SEM)
	(setq retour (funcall 'autoriser_acces INIT-SEM 0)))
      ((equal fichier EDT-SEM)
	(setq retour (funcall 'autoriser_acces EDT-SEM 1)))
      ((equal fichier RESOUD-SEM)
	(setq retour (funcall 'autoriser_acces RESOUD-SEM 2)))
      ((equal fichier VARUNA-SEM)
	(setq retour (funcall 'autoriser_acces VARUNA-SEM 3)))
      (t (setq retour -1)))
    (and (eq retour 0) t)))

;;; Interdit l'acces aux fichiers (poser un semaphore)

(defun interdire-acces (fichier)
  (let
    ((retour))
    (cond
      ((equal fichier INIT-SEM)
	(setq retour (funcall 'interdire_acces INIT-SEM 0)))
      ((equal fichier EDT-SEM)
	(setq retour (funcall 'interdire_acces EDT-SEM 1)))
      ((equal fichier RESOUD-SEM)
	(setq retour (funcall 'interdire_acces RESOUD-SEM 2)))
      ((equal fichier VARUNA-SEM)
	(setq retour (funcall 'interdire_acces VARUNA-SEM 3)))
      (t (setq retour -1)))
    (and (eq retour 0) t)))

(defun interdire-acces-attente (fichier)
  (cond
    ((equal fichier INIT-SEM)
      (setq retour (funcall 'interdire_acces_attente INIT-SEM 0)))
    ((equal fichier EDT-SEM)
      (setq retour (funcall 'interdire_acces_attente EDT-SEM 1)))
    ((equal fichier RESOUD-SEM)
      (setq retour (funcall 'interdire_acces_attente RESOUD-SEM 2)))
    ((equal fichier VARUNA-SEM)
      (setq retour (funcall 'interdire_acces_attente VARUNA-SEM 3)))
    (t ())))

;;; <resolution-p>
;;; CRE - FD - 05/09/95
;;; Verifie qu'il y a qu'un seul site de connecte et donc qu'il est possible
;;; de lancer une resolution
;;; QUE MO 14/09/95
;;; est-ce que le deplacement des rectangles necessite le meme soin ???

(defun resolution-p ()
  (eq (funcall 'nb_utilisateurs) 1))
    
;;; CRE RM - 09/10/95 : Mise a jour du repertoire de donnees courant

(defun repertoire-donnees (rep)
  (funcall 'repertoire_donnees rep))

(defun repertoire-tmp (rep)
  (funcall 'repertoire_tmp rep))

;;; <maj-liste-modifications>
;;; fct mettant a jour la liste des objets qui ont ete modifie

(defun maj-liste-modifications (donnees type)
  (when
    (and
      MODULE-TRACE
      (or (eq type CREATION) (eq type MODIFICATION))
      (not (appartenir-liste donnee Liste-Objets-Traces)))
    (setq Liste-Objets-Traces (cons donnee Liste-Objets-Traces))))
    
;;; <nettoyer-transfile>
;;; Nettoyage du fichier Varuna.trs a la fin d'un enregistrement

(defun nettoyer-transfile ()
  (funcall 'nettoyer_transfile SITE)
  (setq Liste-Objets-Traces '()))


;;; <debut-enregistrement>
;;; on copie VARUNA.TRS en local sur lequel nous travaillerons pour connaitre
;;; l'etat de chacun des objets

(defun debut-enregistrement ()
  (when (and MODULE-RESEAU (probefile TRANSFILE))
    (interdire-acces VARUNA-SEM)
    (copyfile TRANSFILE (catenate REP-VARUNA "\" TRANSFILE-TMP))
    (autoriser-acces VARUNA-SEM)))

(defun fin-enregistrement ()
  (when MODULE-RESEAU
    (when (probefile (catenate REP-VARUNA "\" TRANSFILE-TMP))
      (deletefile (catenate REP-VARUNA "\" TRANSFILE-TMP)))))
  
;;; <initialiser-lecture-seule>
;;; CRE - 04/03/96

(defun initialiser-lecture-seule ()
  (let
    ((prefixe-code
      (if PREFIXE
	(substring PREFIXE 0 (- (slength PREFIXE) 1))
	"")))
    (if (eq (funcall 'lecture_seule prefixe-code) 1)
      (setq LECTURE-SEULE? t)
      (setq LECTURE-SEULE? ()))))

;;; CRE 15/10/96 : trace des donnees

(defun trace-donnee (donnee type)
  (when
    (or (eq type MODIFICATION) (eq type SUPPRESSION))
    (with ((current-directory REP-VARUNA))
      (with ((outchan (openo FICHIER-TRACE-TMP)))
	(ecrire-trace-donnee donnee)
	(close)))
    (maj-fichier-trace)))
 
(defun maj-fichier-trace ()
  (funcall 'maj_fichier_trace))

(defun initialiser-trace ()
  (if (eq (funcall 'mygale "TRACE-VAR") 9)
    (setq MODULE-TRACE t)   
    (setq MODULE-TRACE ()))
  (if (not (disque-local-p))
    (funcall 'etat_trace 1)
    (setq MODULE-TRACE ())
    (funcall 'etat_trace 0)))

;;; CRE RM 05/11/96 : test si le repertoire courant est sur le disque
;;; local (getenv VARUNA)

(defun disque-local-p ()
  (equal
    (lowercase (chrnth 0 (getenv "VARUNA")))
    (chrnth 0 (current-directory))))


