;;; --------------------------------------------------------------------------
;;; - I-Mod.ll                                                  VARUNA 1.2   -
;;; - Interface Graphique                                                    -
;;; - Modules des actions associees a un menu                                -
;;; - ici <mode>                                                             -
;;; --------------------------------------------------------------------------
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; - Creation:  Mars 93                                                     -
;;; - Mise a jour pour version 1.07: Decembre 93                             -
;;; - Build 1.22.1 01.07.99 / JH MO                                          -
;;; --------------------------------------------------------------------------
;;; MOD - FD - 13/01/96
;;; mise en commentaire des fonctions:
;;;    - groupe-ok
;;;    - dispo-compatibles-groupe
;;;    - dispo-compatibles-prof
;;;    - cours-duree-incompatibles
;;; car ces fonctions sont deja definies dans le fichier i-control.ll

;;; CONTROLES LORS DU PASSAGE EN MODE RESOLUTION (Integrite globale)
;;; un groupe est correct si il a au moins un cours ou au moins un groupe
;;; MOD - mise en commentaire: cette fct est deja def dans le fich i-control.ll

; (defun groupe-ok (groupe)
;   (or
;     (cours-i-groupe groupe)
;     (groupes-i-groupe groupe)))



;;; meme chose, mais a partir d'un groupe
;;; tous les cours du groupe sont OK
;;; tous les sous-groupes du groupe sont OK
;;; MOD FD - 24/05/95 
;;;    dispo-compatibles-cours -> dispo-compatibles-cours-p
;;; MOD - mise en commentaire:cette fonction est deja definie dans i-control.ll

; (defun dispo-compatibles-groupe (groupe)
;   (and
;     (every 'dispo-compatibles-cours-p (cours-i-groupe groupe))
;     (every 'dispo-compatibles-groupe (groupes-i-groupe groupe))))
  
;;; meme chose en partant d'un prof 
;;; (tous les cours du prof sont OK)
;;; MOD FD - 24/05/95
;;;    dispo-compatibles-cours -> dispo-compatibles-cours-p
;;; MOD - mise en com. : cette fct est deja def. dans i-control.ll

; (defun dispo-compatibles-prof (prof)
;   (every 'dispo-compatibles-cours-p (cours-i-prof prof)))


;;; fonction de verification de la duree des cours
;;; cette fonction n'est pas definie en predicat sur un unique cours
;;; car elle fait appel a (duree-max)
;;; qui serait calculee a chaque fois lors d'un controle global
;;; renvoie la liste de cours concernes
;;; MOD - mise en commentaire: cette rct est def dans le fichier i-control.ll

; (defun cours-duree-incompatibles (l-cours)
;   (let ((maximum (duree-max)))
;     (sous-liste
;       (lambda (cours)
;         (gt (duree-i-cours cours) maximum))
;       l-cours)))



;;; passage (tentative de...) en Mode resolution
;;; AJT - FD - 31/08/95 - traitement du curseur avec <change-curseur>
;;; MOD RM - 03/05/96 : on ne passe pas en resolution si aucun cours n'est
;;; defini 

;;; AJT RM - 12/02/98 : detection de cycle

(defun mod-res (a)
  (let
    ((cycle))
    (change-curseur a Varuna-Curseur-Sablier)
    (setq cycle (graphe-cyclique))
    (cond
      ((not Liste-I-Cours)
	(change-curseur a Varuna-Curseur-Fleche)	
	(afficher-erreur #Mv_aucun-cours))
      (cycle
	(change-curseur a Varuna-Curseur-Fleche)	
	(erreur-graphe-cyclique cycle))
      (t
	(change-curseur a Varuna-Curseur-Sablier)
	(controle-integrite)
	(ifn *erreurs*
	  (resolution a)
	  (change-curseur a Varuna-Curseur-Fleche)
	  (add-and-select-application Input-App)
	  (interface-erreur))))))



;;; Passage en mode resolution (cette fois c'est sur)
;;; AJT - FD - 31/08/95 - traitement du curseur avec <change-curseur>

(defun resolution (a)
  (setq Mode 'resolution)
  (mise-a-jour-reste)
  (gc)
  (donner-titre ())
  (update-grid-titles)
  (change-curseur a Varuna-Curseur-Fleche)
  (add-and-select-application Gen-App)
  (remove-application Input-App)
  (aff-refresh Gen-App))
       
;;; passage en Mode saisie
;;;  AJT - FD - 31/08/95 - traitement du curseur avec <change-curseur>

(defun mod-sai (a)
  (change-curseur a Varuna-Curseur-Sablier)
  (gc)
  (setq Mode 'saisie)
  ;; (setq Dirty ()) MOD RM FD 08/02/96 les donnees ne sont pas enregistre
  (add-and-select-application Input-App)
  (change-curseur a Varuna-Curseur-Fleche)
  (remove-application Gen-App))


;;; le mode analyse, sans commentaires ...  

(defun mod-ana (a)
  (nyi #Mv_mod-analyse))


