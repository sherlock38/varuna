;;; --------------------------------------------------------------------------
;;; - m-lire.ll                                                 VARUNA 1.2   -
;;; - Moteur                                                                 -
;;; - Module de lecture des donnees et calcul des principaux champs          -
;;; --------------------------------------------------------------------------
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; - Creation:  septembre 94                                                -
;;; - Mise a jour pour version 1.09: Septembre                               -
;;; - Build 1.22.1 01.07.99 / MO                                             -
;;; --------------------------------------------------------------------------


;;; PRIMITIVE de LECTURE GENERALE des seules donnees fichier utiles au moteur
;;; avec les calculs mimimaux

(defun lire-donnees-moteur ()
  (setq Liste-Rect ())     
  (setq Liste-M-Contraintes ())     
  (setq Liste-M-Groupes ())
  (setq Liste-M-Cours-Ajouts ())                ; CRE RM 11/06/97
  (setq Liste-M-Prof-Ajouts ())                 ; CRE RM 11/06/97    
  (lire-fichier FICHIER-COURS 'moteur)
  (lire-fichier FICHIER-AJOUT 'moteur)  	; CRE RM 10/06/97
  (lire-fichier FICHIER-PROFS 'moteur)
  (lire-fichier FICHIER-GROUPES 'moteur)
  (lire-fichier FICHIER-CONTRAINTES 'moteur))

;;;  PRIMITIVES DE LECTURE SPECIFIQUES AU MOTEUR
;;;

;;; primitive de lecture des donnees de l'ecole
;;; 

(dmd lire-ecole-moteur (enr index)
  `(lire-ecole ,enr))

;;; primitive de lecture specifique au type cours
;;; on cree un concept moteur -rect- avec le code du cours, la duree, la dispo
;;; et le nombre de seances
;;; rem (02/09/94) MO: le code est problematique car risque de concurencer
;;; celui de l'interface -> fait en prefixant le code par (prefixer-code-moteur)


(dmd lire-cours-moteur (enr index)
  `(let ,(cdr enr) 
    (creer-rect (prefixer-code-moteur code)
      (+ duree (pause-i-ecole i-ecole))
      nb-seances 
      fige
      (dispo-avec-inter-cours
	(intersection-dispo dispo dispo-ecole) (pause-i-ecole i-ecole))
      (numero-dans-liste type liste-codes-types-cours)
      (if fige instant-trouve ()))
    (construire-erreur-valeurs-manquantes ',enr)))


;;; primitive de lecture d'une enregistrement du type prof
;;; on met a jour la dispo des cours du prof en invoquant traiter-dispo et
;;; on cree une contrainte de non chevauchement pour la non-ubiquite du prof
;;; MOD RM 11/06/97 : on prend en compte les ajouts de seances

(dmd lire-prof-moteur (enr index)
  `(let ,(cdr enr)
    (setq dispo dispo)
    (mapc
      (lambda (c)
	(rajouter-dispo-rect
	  (dispo-avec-inter-cours dispo (pause-i-ecole i-ecole)) c)) 
      (evlis1 (mapcar 'prefixer-code-moteur cours)))
    (when (cdr cours) 
      (newl Liste-m-Contraintes 
        (cons-m-contrainte
	  'poser-non-chevauchement
	  (liste-rects-cours cours))))
    (construire-erreur-valeurs-manquantes ',enr)))

;;; primitive de lecture d'un enregistrement du type groupe
;;; on cree une liste des groupes avec le code du groupe, les objets rects,
;;; les codes des groupes fils
;;; on genere une erreur en cas d'absence de cours et de groupes fils dans un 
;;; groupe

;;; COR JH/ prefixage des cours de groupes
;;; MOD - FD - 18/04/96 ajout du parametre nb-td pour gestion du 
;;; k-ecart-simultanneite
;;; MOD RM 11/06/97 : on prend en compte les ajouts de seances

(dmd lire-groupe-moteur (enr index)
  `(let ,(cdr enr) 
    (set 
      (prefixer-code-moteur code) 
      (cons-m-groupe 
        contraintes 
        (mapcar 'prefixer-code-moteur groupes)
	(evlis1 (liste-rects-cours cours))
        (dispo-avec-inter-cours dispo (pause-i-ecole i-ecole))
        'pas-vu
	nb-td))
    (if (not (prefixer-code-moteur code)) 
      (cons-erreur 'ERRGRPNOFILS code))
      (newl Liste-m-Groupes (prefixer-code-moteur code)) 
    (construire-erreur-valeurs-manquantes ',enr)))      


;;; primitive de lecture d'un enregistrement du type contrainte
;;; on rajoute toutes les contraintes a la liste des contraintes
;;; rem (09/09/94) MO !!
;;; il faudrait faire ici un controle du type de contrainte
;;; MOD RM 11/06/97 : on prend en compte les ajouts de seances

(dmd lire-contrainte-moteur (enr index)
  `(let ,(cdr enr)
    (newl Liste-m-Contraintes 
      (cons-m-contrainte 
        type
	(liste-rects-cours cours)))
    (construire-erreur-valeurs-manquantes ',enr)))


;;; primitive de lecture specifique d'un ajout de seance
;;; CRE RM - 10/06/97

(dmd lire-ajout-moteur (enr index)
  `(let ,(cdr enr) 
    (creer-rect
      (prefixer-code-moteur code)
      (+ duree (pause-i-ecole i-ecole))
      1					; nb-seances d'un ajout
      t					; fige
      (dispo-avec-inter-cours
	(construire-dispo-rect instant duree) (pause-i-ecole i-ecole))
      (rechercher-type-cours (prefixer-code-moteur cours-rattrape))
      instant)
    (construire-erreur-valeurs-manquantes ',enr)
    (newl Liste-M-Cours-Ajouts
      (cons
	(prefixer-code-moteur cours-rattrape)
	(prefixer-code-moteur code)))
    (mapc
      (lambda (p)
	(newl Liste-M-Prof-Ajouts
	  (cons p (prefixer-code-moteur code))))
      profs)))
