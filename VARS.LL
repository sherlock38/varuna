;;; --------------------------------------------------------------------------
;;; - VARS.ll                                                   VARUNA 1.2   -
;;; - VARUNA                                                                 -
;;; - Definition des variables globales du moteur                            -
;;; --------------------------------------------------------------------------
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; - Creation:  Aout 94    MO                                               -
;;; - Derniere mise a jour pour version 1.13:: Aout 94                       -
;;; - Build 1.22.1 01.07.99 / JH MO                                          -
;;; --------------------------------------------------------------------------

;;; rem (13/09/94) MO
;;; Horaire-Interne-Pause a ete retiree de tous les fichiers


;;; 1. Fichiers de donnees

(defvar EXT-EDT ".EDT")
(defvar EXT-BACK ".EDS")
(defvar EXTENSION ".")
(defvar EXT-SAV ".SAV")

(defvar FICHIER-ECOLE (catenate "ECOLE" EXT-EDT))
(defvar FICHIER-COURS (catenate "COURS" EXT-EDT))       
(defvar FICHIER-PROFS (catenate "PROFS" EXT-EDT))
(defvar FICHIER-SALLES (catenate "SALLES" EXT-EDT))
(defvar FICHIER-CONTRAINTES (catenate "CONTR" EXT-EDT))
(defvar FICHIER-RESULTAT (catenate "RESULTAT" EXT-EDT))
(defvar FICHIER-GROUPES (catenate "FAMILLES" EXT-EDT))   
(defvar FICHIER-RESA (catenate "RESA" EXT-EDT))
;;; AJT - FD - 31/07/95 pour module de rattrapage
(defvar FICHIER-AJOUT (catenate "AJOUTS" EXT-EDT))
(defvar FICHIER-ANNUL (catenate "ANNULS" EXT-EDT))

(defvar FICHIER-PERMIT "PERMIT.dll")

(defvar Fichier-Icon-Varuna "varuna.i")
(defvar Fichier-Icon-Grow "gros.i")
(defvar Fichier-Icon-Ungrow "petit.i")
(defvar Fichier-Icon-Scale "scale.i")
(defvar Fichier-Icon-Size "size.i")
;; AJT - FD - 03/07/96
(defvar Fichier-Icon-Suivant "suivant.i")
(defvar Fichier-Icon-Precede "precede.i")

;; AJT - RM - 05/05/97
(defvar Fichier-Icon-Exclamation "exclam.i")
(defvar Fichier-Icon-Question "quest.i")
(defvar Fichier-Icon-Information "info.i")
(defvar Fichier-Icon-Stop "stop.i") 

(defvar Fichier-Pat "pat.i")

;;; 2. Repertoires de donnees

(defvar REP-DONNEES)
(defvar REP-DLL)
(defvar REP-EDITIONS)
(defvar REP-VARUNA)


;;; 3. globales a l'ecriture/lecture de fichiers

;;;  AJT - FD - 31/07/95
;;;  ajout de 'ajout' et 'annulation' pour module de rattrapage
(defvar LISTE-TYPES-ENREGISTREMENTS-RECONNUS
  '(cours groupe prof salle contrainte message ecole resa ajout annulation site))

(defvar TAB (ascii 9))
(defvar RET (ascii 13))
(defvar GUILLEMET (ascii 34))
(defvar ESP " ")
(defvar TRAIT "-")


;;; 4. Variables Ecole

(defvar Dispo-Ecole)
(defvar Liste-Horaires-Cumules-Debut-Cours)
(defvar Jours-Ouvrables)
(defvar Jours-Ouvres)
(defvar Horaire-Cumule-Pause)
(defvar Horaire-Cumule-Ouverture)
(defvar Horaire-Cumule-Fermeture)
(defvar Semaine-Fin)
(defvar Semaine-Debut)
(defvar Semaine-Figee ())
(defvar LIBELLE-ECOLE)
(defvar Liste-Moments-Debut-Ecole)
(defvar Semaines-Debut-Ecole)
(defvar Nb-Moments)
(defvar Nb-Jours)
(defvar Nb-Unites)
(defvar Param-chevauchement)


;;; 5. Info

(defvar affichage-interface)


;;; 6. Erreur

(defvar tag-erreurs-lecture)


;;; 7. Les modules 

(defvar MODULE-EDITIONS)
(defvar MODULE-RESA)
(defvar MODULE-RACO)
(defvar MODULE-CONCEPTION)
(defvar MODULE-SAISIE)
(defvar MODULE-CONSULTATION)
(defvar MODULE-AFFICHAGE)
(defvar MODULE-SALLES)
(defvar MODULE-RESEAU)
(defvar MODULE-GESTSITE)			;; AJT - FD - 23/07/96
(defvar MODULE-TRACE)				;; AJT - RM - 14/10/96

(defvar VARUNA-LANGUE)
(defvar LISTE-ITEMS-INHIBES-VERSION)

;; AJT - FD - 26/03/96
(defvar SITE-CENTRAL t)

;;; Gestion des sites - AJT - FD - 03/06/96
(defvar FICHIER-SITES (catenate "SITES" EXT-EDT))
(defvar SITE-CONNECTE ())

