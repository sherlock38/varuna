;;; --------------------------------------------------------------------------
;;; - MS-Concep.ll                                             VARUNA 1.2    -
;;; - Moteur des salles                                                      -
;;; - Definition des concepts cours et salles                                -
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


(defconcept cours
  (code symbol)
  (duree integer)
  (nb-seances integer)
  (instant-trouve integer)
  (type-salle symbol)
  (audience integer)
  (salle-trouvee symbol)
  (salle-figee symbol)
  (interdites ()))


(defconcept salle
  (code symbol)
  (type symbol)
  (capacite integer))

;; xxx FD - 31/05/95 pour MODULE-RESA

(defconcept resa
  (code symbol)
  (instant integer)
  (duree integer)
  (salle symbol))

;;; CRE RM 11/06/97 : ajout de seance

(defconcept ajout
  (code symbol)
  (instant integer)
  (duree integer)
  (salle symbol))
  