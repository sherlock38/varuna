;;; --------------------------------------------------------------------------
;;; - M- VARS.ll                                                VARUNA 1.2   -
;;; - Moteur                                                                 -
;;; - Definition des variables globales du moteur                            -
;;; --------------------------------------------------------------------------
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; - Creation:  Aout 94    MO                                               -
;;; - Derniere mise a jour pour version 1.13:: Aout 94                       -
;;; - Build 1.22.1 01.07.99 / JH MO                                          -
;;; --------------------------------------------------------------------------

;;; variable prefixe moteur

(defvar Nom-Package-Moteur 'Moteur)

;;; Resolution

(defvar Nombre-Rect 200)
(defvar Nombre-Echecs 0)
(defvar Stop-Running ())
(defvar Seuil-Echec 2500 )			; ??? TRV FD 2500)

;;; 


;;; Listes 

(defvar Liste-M-Contraintes ())
(defvar Liste-M-Groupes ())
(defvar Liste-Rect-Instancies)
(defvar Liste-Rects-Actifs)
(defvar Liste-Rects-Passifs)

;;; TRV - 15/04/96
(defvar PARAM-CHEVAUCHEMENT-1 1000)

(defvar compteur-contraintes-posees 0.)
(defvar compter-contraintes-p ())

;;; CRE RM - 10/06/97
;;; Cette globale est a vrai si l'on veut resoudre en prenant en compte
;;; toutes les contraintes entre les cours et les ajouts de seances.
;;; Elle est a faux : aucune contrainte entre les cours et les ajouts
;;; n'est posee

(defvar RESOLUTION-CONTRAINTES-MAX t)

;;; CRE RM - 11/06/97

;;; Liste contenant des pairs (cours . ajout)
(defvar Liste-M-Cours-Ajouts ())

;;; Liste contenant des pairs (prof . ajout)
(defvar Liste-M-Prof-Ajouts ())
