;;; --------------------------------------------------------------------------
;;; - I-Sedit.ll                                                VARUNA 1.2   -
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; - Interface Graphique                                                    -
;;; - Bibliotheque d'editeur: editeur de symboles                            -
;;; --------------------------------------------------------------------------
;;; - Creation:  Mars 93                                                     -
;;; - Mise a jour pour version 1.07: Decembre 93                             -
;;; - Build 1.22.1 01.07.99  / JH MO                                         -
;;; --------------------------------------------------------------------------

;;; !!appel explicite a des routines de controle definies "ailleurs"
;;; ne gere pas l'option obligatoire/pas obligatoire
;;; ce champ est toujours obligatoire

(eval-when (load eval local-compile)
  (defabbrev sedit {stringedit}:sedit))

(eval-when (load eval local-compile)
  (defstruct {sedit} 
    valeur
    accesseur
    codep
    saisissable))

(defmessage v_smust-be-symb
  (french "Format de symbole incorrecte.")
  (english "Wrong symbol format"))

(defmessage v_cerror
  (french "Le code existe déjà.")
  (english "This value already exists"))

;; AJT - 17/01/96
(defmessage v_cerror_site
  (french "Le code est déjà utilisé par un autre site.")
  (english "????"))

(in-package '{sedit})

(defun sedit (sym accesseur codep saisissable)
  (:initialise-structure (new '{sedit}) sym accesseur codep saisissable))

(defun :initialise-structure (editeur sym accesseur codep saisissable)
  (with ((current-font FONT-EDITEUR))
    ({stringedit}:initialise-structure
      editeur
      0
      0
      0
      (string sym)
      ()))
  (add-component editeur 'user editeur)
  (:accesseur editeur accesseur)
  (:valeur editeur sym)
  (:codep editeur codep)
  (:saisissable editeur saisissable)
  (set-action editeur #'(lambda (ed) (send 'i-next ed)))
  (when (not saisissable)
    (send 'inhibit-application editeur))
  editeur)

;;; Commentaires : voir i-hedit.ll
;;; pour les codes

(defun :i-valider (ed)
  (let
    ((valeur (minuscule (send 'get-line ed)))
     (error ()))

    (cond
      ((and				; code existe et LECTURE-SEULE
	(boundp (:valeur ed))
	(donnee-lecture-seule-p (eval (:valeur ed)) t))
      (traiter-erreur)
      (setq error t))
	
      ((or				; pas de code
	(not valeur)
	(nullstrp valeur))
      (alerte (funcall (:accesseur ed) ()))
      (setq error t))

      ((not (est-un-symbole valeur))	; symbole invalide
	(afficher-erreur #Mv_smust-be-symb)
	(setq error t))

      ((and				
	(:codep ed)
	(if (neq (symbol () valeur) (:valeur ed))
	  (cond
	    ((boundp (symbol () valeur))
 	      (afficher-erreur #Mv_cerror))
	    ((and
	      MODULE-RESEAU
	      (neq (etat-courant-objet (symbol () valeur)) ETAT-NORMAL))
	    (afficher-erreur #Mv_cerror_site)))
	  (when (boundp (symbol () valeur))
	    (and
	      (neq
		(eval (symbol () valeur))
		({application}:userdata (component 'appli ed)))
	      (afficher-erreur #Mv_cerror)))))
      (setq error t)))

    (when
      (and (not error) (neq (symbol () valeur) (:valeur ed)) MODULE-RESEAU)
      (fin-modification SITE (:valeur ed)))
    (not error)))


(defun :i-next (ed)
  (current-keyboard-focus-window
    (send 'window
      (if (send 'i-valider ed)
	(next-user-appli ed 'user)
	ed))))

(defun :i-get-symbol (editeur)
  (symbol () (minuscule (send 'get-line editeur))))

(defun :i-sauver (ed)
  (let
    ((sym (:i-get-symbol ed))
     (objet-edite ({application}:userdata (component 'appli ed))))
    (and objet-edite
      (:accesseur ed)
      (funcall (:accesseur ed) objet-edite sym)
      (if (not (:codep ed)) t
	(when 
	  (and
	    (:valeur ed)
	    (boundp (:valeur ed))
	    (eq (eval (:valeur ed)) objet-edite)
	    (makunbound (:valeur ed))))
	(set sym objet-edite)
	(when MODULE-RESEAU
	  (debut-modification SITE objet-edite CREATION))))))
      

(defun :i-restore (ed)
  (let
    ((objet-edite ({application}:userdata (component 'appli ed))))
    (and objet-edite
      (:accesseur ed)
      (funcall (:accesseur ed) objet-edite (:valeur ed)))))
