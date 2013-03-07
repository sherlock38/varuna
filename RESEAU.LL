;;; --------------------------------------------------------------------------
;;; - reseau.ll                                                 VARUNA 1.2   -
;;; - module de primitives pour passage en reseau                            -
;;; --------------------------------------------------------------------------
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; - Creation:  Janvier 1995                                                -
;;; - Build 1.22.1 01.07.99 / MO                                             -
;;; --------------------------------------------------------------------------

;;;
;;; fonction:   environnement-selon-acces-autorises
;;;             appelle des fonctions d'activation de menu et de bouton
;;;
;;; activation-resoudre
;;; activation-consulter
;;; activation-saisir
;;; activation-editer
;;; ces fonctions seton internes a ce module et donc non exportees
;;; on ne pourra donc pas se passer du point d'entree a la fonction principale
;;; qui verifie le site par dll

;;; permit "saisir"
;;; permit "consulter"
;;; permit "resoudre"
;;; permit "editer"

(defun environnement-selon-acces-autorises ()
  ())

;;; <maj-site-connecte>
;;; fonction initialisation les droits d'acces pour le site courant
;;; recuperer le nom du site (numero de DD)
;;; rem: quand nous ne sommes pas en reseau et que nous n'avons pas la gestion
;;; de sites, SITE-CENTRAL vaut t pour pouvoir acceder a toutes les donnees

(defun maj-site-connecte ()
  (when (and MODULE-RESEAU MODULE-GESTSITE)
    (let
      ((courant (rechercher-site-courant)))
      (when (not courant)
	(setq courant (creer-nouveau-site)))
      (setq SITE-CONNECTE courant)
      (if (eq (type-i-site SITE-CONNECTE) 'central)
	(setq SITE-CENTRAL t)
	(setq SITE-CENTRAL ())))))

(defun rechercher-site-courant ()
  (let*
    ((lst-tmp LISTE-I-SITE)
     (site-trouve (nextl lst-tmp)))
    (until
      (or
	(null site-trouve)
	(equal (numero-dd-i-site site-trouve) SITE))
      (nextl lst-tmp site-trouve))
    site-trouve))

(defun creer-nouveau-site ()
  (let
    ((nouveau (nouveau-i-site)))
    (newl Liste-I-Site nouveau)
    (code-i-site nouveau (gencode-i-site))
    (numero-dd-i-site nouveau SITE)
    (if MODULE-CONCEPTION
      (progn
	(type-i-site nouveau 'central)
	(libelle-i-site nouveau "Site Central"))
      (type-i-site nouveau 'secondaire)
      (libelle-i-site nouveau SITE))
    (if (and MODULE-CONCEPTION MODULE-RESEAU (not (interdire-acces EDT-SEM)))
      (afficher-attention #Mv_acces-edt-interdit)
      (ecrire-fichier FICHIER-SITES 'interface 'site ())
      (setq Liste-I-Site ())
      (lire-fichier FICHIER-SITES 'interface)
      (autoriser-acces EDT-SEM))
    nouveau))

