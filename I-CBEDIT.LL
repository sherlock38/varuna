;;; --------------------------------------------------------------------------
;;; - I-CBedit.ll                                                VARUNA 1.2  -
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; - Interface Graphique                                                    -
;;; - Bibliotheque d'editeur: editeur de check box                           -
;;; --------------------------------------------------------------------------
;;; - Creation:  Juillet 95                                                  -
;;; - Build 1.22.1 01.07.99 / FD                                             -
;;; --------------------------------------------------------------------------

(eval-when (load eval local-compile)
  (defabbrev cbedit {checkbox}:cbedit))

(eval-when (load eval local-compile)
  (defstruct {cbedit}
    valeur
    accesseur))

(in-package '{cbedit})

(defun cbedit (valeur accesseur)
  (:initialise-structure (new '{cbedit} valeur accesseur)))

(defun :initialise-structure (editeur chaine accesseur)
  (with ((current-font FONT-EDITEUR))
    ({checkbox}:initialise-structure
      editeur
      0  		;; x
      0		        ;; y
      200		;; w
      50		;; h
      chaine))
  (add-component editeur 'user editeur)
  (:accesseur editeur accesseur)
  (:valeur editeur chaine)
  (set-action editeur #'(lambda (ed) (send 'i-next ed)))
  editeur)

(defun :i-valider (editeur)
  )

(defun :i-next (editeur)
  )

(defun :i-get-symbol (editeur)
  (send 'checkedp editeur))

(defun :i-sauver (editeur)
  (let
    ((val (:i-get-symbol editeur))
     (objet ({application}:userdata (component 'appli ed))))
    (and objet
      (:accesseur editeur)
      (funcall (:accesseur editeur) objet val))))

(defun :i-restore (editeur)
  (let
    ((objet ({application}:userdata (component 'appli editeur))))
    (and
      objet
      (:accesseur editeur)
      (funcall (:accesseur editeur) objet (:valeur editeur)))))