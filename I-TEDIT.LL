;;; --------------------------------------------------------------------------
;;; - I-tedit.ll                                                VARUNA 1.2   -
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; - Interface Graphique                                                    -
;;; - Bibliotheque d'editeur: editeur de chaine de caracteres                -
;;; - sur plusieurs lignes (sous classe de medite)                           -
;;; --------------------------------------------------------------------------
;;; - Creation:  Octobre 94                                                  -
;;; - Mise a jour pour version 1.1 :                                         -
;;; - Build 1.22.1 01.07.99 / JH                                             -
;;; --------------------------------------------------------------------------

;;; tedit est une sous-classe de medite

(eval-when (load eval local-compile)
  (defabbrev tedit {medite}:tedit))

(eval-when (load eval local-compile)
  (defstruct {tedit} 
    valeur
    accesseur))

(defmessage v_commentaire-sans-guillemet
  (french "Un commentaire ne doit pas comporte de guillemet")
  (english "Wrong comment format"))

(in-package '{tedit})

(defun tedit (liste-chaines accesseur)
  (:initialise-structure (new '{tedit}) liste-chaines accesseur))

(defun :initialise-structure (editeur liste-chaines accesseur)
  (with ((current-font FONT-EDITEUR))
    ({medite}:initialise-structure editeur 0 0 0 0)
    (add-component editeur 'user editeur)
    (:valeur editeur liste-chaines)
    (send 'set-text editeur liste-chaines)
    (:accesseur editeur accesseur)
    editeur))

;;; DBG (07/06/95) MO    !!!! il faudrait creuser cette histoire !!!!
;;; je mets un filtre en sortie pour debuuger l'anomalie creation de
;;; lignes blanches

(defun :i-valider (ed)
  (let ((erreur ()))
    (send 'set-text ed (filtre-lignes-blanches (send 'get-text ed)))
    ;; 29/02/96 - AJT verifier qu'il n'y a pas de "
    (when (not (:sans-guillemet-p ed))
      (afficher-erreur #Mv_commentaire-sans-guillemet)
      (setq erreur t))
    (not erreur)))

;;; <:sans-guillemet-p>
;;; CRE - 29/02/96 - FD
;;; verifie qu'il n'y a pas de " dans les commentaires (car si non: pb pour
;;; servens, editions, vartrans ... )
(defun :sans-guillemet-p (ed)
  (not (premier 'avec-guillemet-p (send 'get-text ed))))

;;; <filtre-lignes-blanches>
;;; un filtre qui eilimine les lignes blanches a la fin d'une liste de
;;; lignes. Je n'ai pas utilis‚ d'astuce de contraction du type <nextl>
;;; pour la lisibilite du programme.
;;;  a charge de cette primitive deux constructions de listes avec le <reverse>
;;;  a voir si il y a un autre moyen ?


(defun filtre-lignes-blanches (liste-chaines)
  (let
    ((liste-travail (reverse liste-chaines)))
    (while
      (and
	liste-travail
	(ligne-blanche-p (car liste-travail)))
      (setq liste-travail (cdr liste-travail)))
    (reverse liste-travail)))

;;;  cre (07/06/95) MO
;;;  a mettre dans ulisp????
;;; <ligne-blanche-p>
;;; rend t si la ligne est vide ou composee de seuls espaces (ci 32)
;;;  j'ai prefere un any a un every, histoire de ne pas parcourir la liste
;;;  jusqu'au bout a chaque fois mais seulement jusqu'au premier caractere
;;;  non blanc.


(defun ligne-blanche-p (chaine)
  (not
    (any
      (lambda (code-interne) (neq code-interne 32))
      (pname chaine))))


(defun :i-next (ed)
  (current-keyboard-focus-window
    (send 'window (if (send 'i-valider ed) (next-user-appli ed 'user) ed))))

(defun :i-sauver (ed)
  (let
    ((objet-edite ({application}:userdata (component 'appli ed))))
    (and objet-edite
      (:accesseur ed)
      (funcall (:accesseur ed) objet-edite (send 'get-text ed)))))

(defun :i-restore (ed)
  (let
    ((objet-edite ({application}:userdata (component 'appli ed))))
    (and
      objet-edite
      (:accesseur ed)
      (funcall (:accesseur ed) objet-edite (:valeur ed)))))

