;;; --------------------------------------------------------------------------
;;; - M-Concepts.ll                                             VARUNA 1.2   -
;;; - Noyau                                                                  -
;;; - Definition du concept rect                                             -
;;; --------------------------------------------------------------------------
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; - Creation:  Mars 93                                                     -
;;; - Derniere mise a jour pour version 1.13:: Decembre 93                   -
;;; - Build 1.22.1 01.07.99 / JH                                             -
;;; --------------------------------------------------------------------------



;; Grace a la macro defconcept, on genere toutes les operations relatives
;; a chaque concept.
;; Attention, ceci complique un peu la partie compilation


(defconcept rect
  (code symbol)                         ; code unique du cours
  (duree integer)                       ; duree de la seance type
  (nb-seances integer)                  ; nombre de seances
  (fige symbol) ;;FD-11/05/95())        ; () si non fige, instant fige sinon
  (dispo cons)                          ; disponibilite de ce cours
  (type integer)                        ; le type du rectangle (entier)
  (a-resoudre ())                       ; flag de resolution
  (moments ())
  (semaines ())

  (instant () (ct-fix-var) constrained) ; variable pecos
  (instant-trouve integer)              ; instant trouve
  (nb-contraintes () 0 reversible)                 ; entier utilise par l algo
  (total-contraintes () 0 reversible)              ; entier utilise par l algo
  (coeff ()))                           ; entier utilise par l algo



;; Ce qui est non genere : 2 variables globales, 1 accesseur

;; already declared in m-vars.ll
;; (defvar Liste-Rect-Instancies ())

(defvar Liste-Contraintes ())

(ct-make-var-accessor {rect} instant position-rect)

