;;; --------------------------------------------------------------------------
;;; - ms-lire.ll                                                VARUNA 1.2   -
;;; - Moteur des salles                                                      -
;;; - Module de lecture des donnees et calcul des principaux champs          -
;;; --------------------------------------------------------------------------
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; - Creation:  septembre 94                                                -
;;; - Mise a jour pour version 1.09: Septembre                               -
;;; - Build 1.22.1 01.07.99 / MO                                             -
;;; --------------------------------------------------------------------------

;;; AJT (11/04/95) MO
;;; ajout de la primitive de lecture des resa
;;; Module reservation de salles


;;; PRIMITIVE de LECTURE GENERALE des seules donnees fichier utiles au moteur
;;; avec les calculs mimimaux

(defun lire-donnees-ms ()
  (setq liste-salle ())     
  (setq liste-cours ())
  (setq liste-resa ())
  (setq liste-ajout ())				; AJT RM 11/06/97
  (lire-fichier FICHIER-COURS 'ms)
  (lire-fichier FICHIER-SALLES 'ms)
  (lire-fichier FICHIER-RESA 'ms)
  (lire-fichier FICHIER-AJOUT 'ms))		; AJT RM 11/06/97


;;; primitive de lecture des donnees de l'ecole
;;; 

(dmd lire-ecole-ms (enr index)
  `(lire-ecole ,enr))

(dmd lire-cours-ms (enr index)
  `(let ,(cdr enr)
    (if salle-figee-reelle
      (creer-cours 
	(prefixer-code-ms code)
	(div duree Unite-Temps)
	nb-seances instant-trouve type-salle audience
	(prefixer-code-ms salle-figee-reelle)
	t)
      (creer-cours 
	(prefixer-code-ms code)
	(div duree Unite-Temps)
	nb-seances instant-trouve type-salle audience
	(if salle-figee (prefixer-code-ms salle-figee) ())
	()))
    (construire-erreur-valeurs-manquantes ',enr)))

(dmd lire-salle-ms (enr index)
  `(let ,(cdr enr) 
    (creer-salle (prefixer-code-ms code) type capacite)
    (construire-erreur-valeurs-manquantes ',enr)))

(dmd lire-resa-ms (enr index)
  `(let ,(cdr enr) 
    (creer-resa 
      (prefixer-code-ms code)
      instant
      (div duree Unite-Temps) 
      (prefixer-code-ms salle))
    (construire-erreur-valeurs-manquantes ',enr)))

;;; CRE RM 11/06/97 : prise en compte des ajouts de seance

(dmd lire-ajout-ms (enr index)
  `(let ,(cdr enr) 
    (creer-ajout
      (prefixer-code-ms code)
      instant
      (div duree Unite-Temps) 
      (prefixer-code-ms salle))
    (construire-erreur-valeurs-manquantes ',enr)))
