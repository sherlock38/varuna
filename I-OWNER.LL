;;; --------------------------------------------------------------------------
;;; - I-owner.ll                                                VARUNA 1.2   -
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; - Interface Graphique                                                    -
;;; - Bibliotheque d'editeur: editeur de chaine de caracteres                -
;;; --------------------------------------------------------------------------
;;; - Creation:  Mars 93                                                     -
;;; - Mise a jour pour version 1.07: Decembre 93                             -
;;; - Build 1.22.1 01.07.99 / JH MO                                          -
;;; --------------------------------------------------------------------------

(eval-when (load eval local-compile)
  (defabbrev owner {application}:owner))

(eval-when (load eval local-compile)
  (defstruct {owner} 
    action))

(in-package '{owner})

(defun owner (x y w h image action)
  (:initialise-structure (new '{owner}) x y w h image action))

(defun :initialise-structure (appli x y w h image action)
  (:action appli action)
  (send-super '{owner} 'initialise-structure appli x y h w image)
  appli)

(defun :down-event (appli event)
  (when
    (and
      (:action appli)
      (eq ({event}:detail event) 1))
    (let ((new-image (funcall (:action appli))))
      (when new-image
	(send 'set-image appli new-image)))))

