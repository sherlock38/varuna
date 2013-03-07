;;; --------------------------------------------------------------------------
;;; - i-inhibi.ll                                                VARUNA 1.2  -
;;; - module de primitives pour passage en reseau                            -
;;; --------------------------------------------------------------------------
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; - Creation:  Juin 1995                                                   -
;;; - Build 1.22.1 01.07.99 / MO                                             -
;;; --------------------------------------------------------------------------

(defun maj-items-menus-selon-environnement ()
  (inhiber-separateurs)				;; AJT 02/02/96
  (maj-varuna-module-resa)
  (maj-varuna-module-raco)
  (maj-varuna-module-salles)
  (maj-varuna-module-conception)
  (maj-varuna-module-saisie)
  (maj-varuna-module-editions)
  (maj-varuna-module-consultation)
  ;; AJT - FD - 27/02/96 MultiP
  (maj-varuna-lecture-seule)
  ;; AJT - RM - 13/08/96 : Gestion de sites
  (maj-varuna-module-gestsite))

;;; <inhiber-separateurs> 
;;; CRE 02/02/96

(defun inhiber-separateurs ()
  (inhiber-varuna-menu-couleur Input-App 'menu-direct '(4 6 11)
    couleur-separateur-inhibe)
  (inhiber-varuna-menu-couleur Input-App 'menu-contr '(5 8)
    couleur-separateur-inhibe)
  (inhiber-varuna-menu-couleur Gen-App 'menu-direct '(4 6)
    couleur-separateur-inhibe)
  (inhiber-varuna-menu-couleur Gen-App 'menu-resol '(2)
    couleur-separateur-inhibe)
  (inhiber-varuna-menu-couleur Gen-App 'menu-gestion '(1 4)
    couleur-separateur-inhibe)
  (inhiber-varuna-menu-couleur Gen-App 'menu-affich '(3 8)
    couleur-separateur-inhibe)
  (inhiber-varuna-menu-couleur Gen-App 'sous-menu-affich-options '(2 5)
    couleur-separateur-inhibe)
  (inhiber-varuna-menu-couleur Gen-App 'menu-outils '(2)
    couleur-separateur-inhibe)
  (inhiber-varuna-menu-couleur Gen-App 'sous-menu-outils-manip '(1 5)
    couleur-separateur-inhibe)
  (inhiber-varuna-menu-couleur Gen-App 'sous-menu-outils-ferie '(3)
    couleur-separateur-inhibe))
  

;;; MODULE RESERVATION DE SALLES

(defun maj-varuna-module-resa ()
  (if MODULE-RESA
    (activer-varuna-module-resa)
    (inhiber-varuna-module-resa)))

(defun inhiber-varuna-module-resa ()
  (inhiber-varuna-menu Gen-App 'menu-outils (list PLACE-MENU-RESA))
  (inhiber-varuna-menu Gen-App 'menu-data (list PLACE-MENU-DONNEES-RESA)))

(defun activer-varuna-module-resa ()
  (activer-varuna-menu Gen-App 'menu-outils (list PLACE-MENU-RESA))
  (activer-varuna-menu Gen-App 'menu-data (list PLACE-MENU-DONNEES-RESA)))


;;; MODULE SALLES

(defun maj-varuna-module-salles ()
  (if MODULE-SALLES
    (activer-varuna-module-salles)
    (inhiber-varuna-module-salles)))

(defun inhiber-varuna-module-salles ()
  (inhiber-varuna-menu Gen-App 'menu-resol (list PLACE-MENU-RES-SALLES))
  (inhiber-varuna-menu Gen-App 'menu-outils
    (list PLACE-MENU-PERMUT PLACE-MENU-FERIE)))

(defun activer-varuna-module-salles ()
  (when MODULE-CONCEPTION
    (activer-varuna-menu Gen-App 'menu-resol (list PLACE-MENU-RES-SALLES)))
  (activer-varuna-menu Gen-App 'menu-outils
    (list PLACE-MENU-PERMUT PLACE-MENU-FERIE)))  


;;; MODULE MOTEUR

(defun maj-varuna-module-conception ()
  (if MODULE-CONCEPTION
    (activer-varuna-module-conception)
    (inhiber-varuna-module-conception)))

(defun inhiber-varuna-module-conception ()
  (inhiber-varuna-menu Gen-App 'menu-resol
    (list PLACE-MENU-RES-TOTALE PLACE-MENU-RES-PARTIELLE
      PLACE-MENU-RES-COURS-TOTAL PLACE-MENU-RES-COURS-PARTIELLE
      PLACE-MENU-RES-SALLES))
  (inhiber-varuna-menu Gen-App 'menu-outils (list PLACE-MENU-MANIP))
  (send 'new-image (component 'bouton Gen-App) (buttonbox 0 0 72 32 ""))
  (inhibit-application
    (send 'component 'Bouton Gen-App)))

(defun activer-varuna-module-conception ()
  (activer-varuna-menu Gen-App 'menu-resol
    (list PLACE-MENU-RES-TOTALE PLACE-MENU-RES-PARTIELLE
      PLACE-MENU-RES-COURS-TOTAL PLACE-MENU-RES-COURS-PARTIELLE
      PLACE-MENU-RES-SALLES))
  (activer-varuna-menu Gen-App 'menu-outils (list PLACE-MENU-MANIP))  
  (authorize-application
    (send 'component 'Bouton Gen-App)))


;;; MODULE SAISIE

(defun maj-varuna-module-saisie ()
  (if MODULE-SAISIE
    (activer-varuna-module-saisie)
    (inhiber-varuna-module-saisie)))

(defun inhiber-varuna-module-saisie ()
  (inhiber-varuna-menu Gen-App 'menu-direct (list PLACE-MENU-MODE))
  (inhiber-varuna-menu Gen-App 'menu-gestion (list PLACE-MENU-CONTRAINTES))
  (inhiber-varuna-menu Input-App 'menu-contr
    (list
      PLACE-MENU-NON-CHEVAUCHEMENT
      PLACE-MENU-PRECEDENCE
      PLACE-MENU-MEME-SEMAINE
      PLACE-MENU-JOURS-DIFFERENTS
      PLACE-MENU-PROXIMITE-SEMAINE))
  (inhiber-varuna-menu input-app 'menu-direct
    (list PLACE-MENU-ENREGISTRER PLACE-MENU-ENREGISTRER-SOUS))
  (inhiber-varuna-menu Gen-App 'menu-direct 
    (list PLACE-MENU-ENREGISTRER PLACE-MENU-ENREGISTRER-SOUS))
  (inhibit-application
      (send 'component 'Bouton Gen-App)))

(defun activer-varuna-module-saisie ()
  (activer-varuna-menu Gen-App 'menu-direct (list PLACE-MENU-MODE))
  (activer-varuna-menu Gen-App 'menu-gestion (list PLACE-MENU-CONTRAINTES))
  (activer-varuna-menu input-app 'menu-contr
    (list
      PLACE-MENU-NON-CHEVAUCHEMENT
      PLACE-MENU-PRECEDENCE
      PLACE-MENU-MEME-SEMAINE
      PLACE-MENU-JOURS-DIFFERENTS
      PLACE-MENU-PROXIMITE-SEMAINE))
  (activer-varuna-menu input-app 'menu-direct 
    (list PLACE-MENU-ENREGISTRER PLACE-MENU-ENREGISTRER-SOUS))
  (activer-varuna-menu Gen-App 'menu-direct 
    (list PLACE-MENU-ENREGISTRER PLACE-MENU-ENREGISTRER-SOUS))
  (authorize-application
      (send 'component 'Bouton Gen-App)))


;;; MODULE CONSULTATION

(defun maj-varuna-module-consultation ()
  ())


;;; MODULE RACO

(defun maj-varuna-module-raco ()
  (if MODULE-RACO
    (activer-varuna-module-raco)
    (inhiber-varuna-module-raco)))

(defun inhiber-varuna-module-raco ()
  (inhiber-varuna-menu Gen-App 'menu-outils
    (list PLACE-MENU-RACO PLACE-MENU-PERMUT PLACE-MENU-FERIE))
  (inhiber-varuna-menu Gen-App 'menu-data
    (list PLACE-MENU-DONNEES-ANNUL PLACE-MENU-DONNEES-AJOUT)))

(defun activer-varuna-module-raco ()
  (activer-varuna-menu Gen-App 'menu-outils
    (list PLACE-MENU-RACO PLACE-MENU-PERMUT PLACE-MENU-FERIE))
  (activer-varuna-menu Gen-App 'menu-data
    (list PLACE-MENU-DONNEES-ANNUL PLACE-MENU-DONNEES-AJOUT)))


;;; MODULE-EDITIONS
;;;

(defun maj-varuna-module-editions ()
  (if MODULE-EDITIONS
    (activer-varuna-module-editions)
    (inhiber-varuna-module-editions)))

(defun inhiber-varuna-module-editions ()
  (inhiber-varuna-menu Gen-App 'menu-outils (list PLACE-MENU-EDITION)))

(defun activer-varuna-module-editions ()
  (activer-varuna-menu Gen-App 'menu-outils (list PLACE-MENU-EDITION)))

;;; <inhiber-varuna-module-gestion>
;;; CRE - FD - 26/02/96
;;; menus: Figer, Defiger, Placer un cours, Retirer les cours non figes,
;;;        gestions des TDs/TPs

(defun inhiber-varuna-module-gestion ()
  (inhiber-varuna-menu Gen-App 'menu-outils (list PLACE-MENU-GESTION-TD))
  (inhiber-varuna-menu Gen-App 'menu-gestion
    (list
      PLACE-MENU-FIGER PLACE-MENU-DEFIGER
      PLACE-MENU-PLACER PLACE-MENU-DEPLACER
      PLACE-MENU-RETIRER-NON-FIGER PLACE-MENU-RETIRER-SALLES)))

(defun inhiber-varuna-module-outils ()
  (inhiber-varuna-menu Gen-App 'menu-outils
    (list
      PLACE-MENU-FORMAT-IMPRESSION PLACE-MENU-IMPRIMER 
      PLACE-MENU-GESTION-TD PLACE-MENU-RESA PLACE-MENU-RACO
      PLACE-MENU-EDITION PLACE-MENU-PERMUT PLACE-MENU-MANIP
      PLACE-MENU-FERIE)))

;;; <activer-varuna-module-gestion>
;;; CRE - FD - 26/02/96
;;; menus: Figer, Defiger, Placer un cours, Retirer les cours non figes,
;;;        gestions des TDs/TPs

(defun activer-varuna-module-gestion ()
  (activer-varuna-menu Gen-App 'menu-outils (list PLACE-MENU-GESTION-TD))
  (activer-varuna-menu Gen-App 'menu-gestion
    (list
      PLACE-MENU-FIGER PLACE-MENU-DEFIGER
      PLACE-MENU-PLACER PLACE-MENU-DEPLACER
      PLACE-MENU-RETIRER-NON-FIGER PLACE-MENU-RETIRER-SALLES)))


;;; <maj-varuna-lecture-seule>
;;; CRE 26/02/96 - MultiP

(defun maj-varuna-lecture-seule ()
  (if LECTURE-SEULE?
    (inhiber-varuna-module-modification)
    (activer-varuna-module-modification)))

;;; <inhiber-varuna-module-modification>
;;; CRE 26/02/96 - MultiP

(defun inhiber-varuna-module-modification ()
  (inhiber-varuna-module-saisie)
  ;; mais on autorise le passage en mode saisie
  (activer-varuna-menu Gen-App 'menu-direct (list PLACE-MENU-MODE))
  (inhiber-varuna-module-gestion)
  (inhiber-varuna-module-conception)
  (inhiber-varuna-module-outils)
  (inhiber-varuna-menu Input-App 'menu-direct
    (list PLACE-MENU-ETABLISSEMENT PLACE-MENU-SEMAINE PLACE-MENU-PAUSE)))

;;; <activer-varuna-module-modification>
;;; CRE 26/02/96 - MultiP
;;; activation de certains menus quand nous sommes en lecture seule sur un jeux
;;; de donnees

(defun activer-varuna-module-modification ()
  (activer-varuna-module-gestion)
  (maj-varuna-module-raco)
  (activer-varuna-menu Gen-App 'sous-menu-outils-raco
    (list PLACE-MENU-SUPPRIMER-ANNUL PLACE-MENU-SUPPRIMER-AJOUT))
  (maj-varuna-module-saisie)
  (maj-varuna-module-conception)
  (maj-varuna-module-salles))

;;; CRE RM - 13/08/96 : Gestion de sites

(defun maj-varuna-module-gestsite ()
  (if (and MODULE-GESTSITE MODULE-RESEAU MODULE-CONCEPTION)
    (activer-varuna-module-gestsite)
    (inhiber-varuna-module-gestsite)))

(defun inhiber-varuna-module-gestsite ()
  (inhiber-varuna-menu Input-App 'menu-direct (list PLACE-MENU-SITE)))

(defun activer-varuna-module-gestsite ()
  (activer-varuna-menu Input-App 'menu-direct (list PLACE-MENU-SITE)))

