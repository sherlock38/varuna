;;; --------------------------------------------------------------------------
;;; - Noyau.ll                                                  VARUNA 1.2   -
;;; - Moteur Varuna                                                          -
;;; - Noyau du moteur                                                        -
;;; --------------------------------------------------------------------------
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; - Creation:  Mars 93                                                     -
;;; - Derniere mise a jour pour version 1.13:: Aout 94                       -
;;; - Build 1.22.1 01.07.99 / MO                                             -
;;; --------------------------------------------------------------------------

;;; AJT (25/01/95) MO
;;; ajout (ou changement pour antez et post) des 4 liberations de memoire



;;; INITIALISATIONS


;;; la primitive <initialisation>: 
;;;     - initialisation des domaines
;;;     - construction des domaines de la variable contrainte associee pour les
;;;       rects a-resoudre et figes
;;;     - calcul du coeff du rect
;;;     - conversion de la duree du rect en duree interne pour la resolution

(defun initialisation ()
  (setq liste-rect-instancies ())           
  (setq Nombre-Echecs 0)
  (mapc 'initialiser-rect liste-rects-actifs)
  (gc))


;;; initialisation d'un rect (rem: ils sont tous actifs)
;;; rect fige -> on instancie - pas de coeff - pas de domaine
;;; rect non fige a-resoudre -> on calcule le domaines, le coeff

;;; REM FD MO dans le cas ou le rect n'est pas instancie mais fige
;;; on doit le considerer passif....


(defun initialiser-rect (r)
  (cond
    ((fige-rect r) 
     (newl liste-rect-instancies r) (donner-valeur r (instant-trouve-rect r)))
    ((a-resoudre-rect r)     
     (init-domaine-rect r)
     (init-coeff-rect r)
     (faire-domaine r))
    
    (t (cons-erreur 'ERR-NON-ACTIF r)))
  (vider-rect r)
  (duree-rect r (div (duree-rect r) Unite-Temps)))    

;;;  TRV MO FD 29/06/95
;;; Calcul du coeff de rect
;;; coeff 0 si pas de domaine -> rem: est-ce poosible que des rects n'est pas
;;;                              de domaines a ce niveau la
;;; coeff calcule sinon

(defun init-coeff-rect (c)
  (if 
    (or 
      (null (semaines-rect c))
      (null (moments-rect c)))
    (progn 
      (coeff-rect c 0)
      (cons-erreur 'errnodom (de-prefixer-code-moteur (code-rect c))))
    (coeff-rect c (calcul-coeff-rect c))))


;;; Calcul des moments et semaines de debut possible pour le rect 
;;; en temps interne
;;;  29/06/95 IL faut construire une erreur si faire-moments...
;;;  renvoie () ainsi que faire-semaine
;;; MOD - FD - 15/04/96 ajout du param de chevauchement

(defun init-domaine-rect (rect)
  ;; TRV
  (moments-rect rect
    ;; TRV - 15/04/96 type-rect => type-cours-rect
    (mapcar
      'moment-cumule2moment-interne
      (faire-moments-debut (dispo-rect rect) (duree-rect rect) (type-cours-rect rect))))
  (semaines-rect rect
    (faire-semaines-debut
      (dispo-rect rect)
      (nb-seances-rect rect)
      (type-cours-rect rect)
      (param-chevauchement-max-rect rect))))


;;; ERRREURS pendant la POSE DES CONTRAINTES OU LA RESOLUTION

;;; REM (21/10/94) JH
;;; 1-> pourquoi sortir du moteur lorsque un seuil d'echec 
;;; arbitrairement choisi est depasse
;;; 2-> pourquoi pas une routine d'erreur specifique pour la 
;;; pose des contraintes ou la il faut sortir du moteur
;;; (pendant la pose des contrainte, je me demande
;;; sur quoi va pouvoir bien pointer le tag running)

;;; AJT (17/01/95) MO
;;; 2) est fait: la remarque e'tait judicieuse est repond au bug med 2
;;; du repor51.v94


(defun echec-resolution (var constraint)
  (setq Nombre-echecs (add1 Nombre-echecs))   
  (when 
    (> Nombre-echecs Seuil-Echec)
    (cons-erreur 'ERRSEUILECHEC 
      (mapcar (lambda (rect)
        (de-prefixer-code-moteur (code-rect rect)))
      (firstn 3 Liste-Rect-Instancies)))   
    (exit running)))

(defun echec-pose-contrainte (var constraint)
  (let 
    ((type-contr (car *contrainte*))
     (rect1 
       (de-prefixer-code-moteur (cadr *contrainte*)))
     (rect2 
       (de-prefixer-code-moteur (caddr *contrainte*))))
    (cons-erreur 'ERRINCCONTR (list type-contr rect1 rect2))
    (exit pose-contr)))


;;; RESOLUTION

;;; l'algorithme relancable
;;; CHG (16/11/94) MO et JH
;;; on a enleve le ct-restore           
;;; en effet, les donnees etant ecrites sur le disque, on a pas besoin de 
;;; cette copie d'environnement
;;; 


(defun resoudre ()
  (ct-breadth-first t)
  (tag resoudre (algorithme)))


(defun resoudre-liste-rect (liste)
  (gc)
  (ct-solve 
    (ct-enumerate (ct-bag liste) 'variable-choisie 'valeur-choisie)
    ))

(defun resolution-par-paquet ()
  (gc)
    (let
      ((nombre-iter (div (length liste-rects-actifs) Nombre-Rect)))
      (for 
        (i 0 1 nombre-iter)
        (resoudre-liste-rect
          (firstn Nombre-Rect (nthcdr (* i Nombre-Rect) liste-rects-actifs))))))


;;; l'algorithme complet

(defun algorithme ()
  ;; TRV - 15/04/96
  (propagation-param-chevauchement)
  (propagation-dispo-groupes)
  (when 
      (not *erreurs*) ;; a remplacer par (exit resoudre)
      (cons-info () 10 () ())

      (initialiser-listes-rects)  ; on se dote de deux listes: liste-rects-actifs et 
                              ; liste-rects-passifs liste-rect etant a () 
      (liberer-memoire-rects-passifs)
      (initialisation)
      (when *erreurs* 
          ;; ceci est necessaire pour <ecrire-resultat-moteur> qui
          ;; utilise liste-rect
	  (setq liste-rect 
                (append Liste-rects-actifs liste-rects-passifs))
          (exit resoudre))
      (cons-info () 20 () ())
      (ct-set-fail-hook 'echec-pose-contrainte)
          (print "contraindre")
      (tag pose-contr  (contraindre))
      (cons-info () 40 () ())
      (liberer-memoire-m-objets)
      (when
          (not *erreurs*)
	            (print "résolution");; !!TRV 30/05/96
          (setq Liste-Rects-Actifs (sort 'moins-libre Liste-Rects-Actifs))
          (ct-set-fail-hook 'echec-resolution)
          (tag running (resolution-par-paquet)) 
          (sauver-rect-instancies)
          (setq liste-rect (append Liste-rects-actifs liste-rects-passifs)))))
  

;;; Sauvegarde des rect instancies par propagation

(defun sauver-rect-instancies ()
  (mapc 
    (lambda (rect)
      (when
        (rect-place-p rect)
        (instant-trouve-rect rect (instant-rect rect))))
    liste-rects-actifs))


;;; Fonction appellee chaque fois que l'on place un rect

(defun sauver-rect (rect)
  (instant-trouve-rect rect (instant-rect rect))   ;on sauve l'instanciation
  (cons-info 
    () 
    (+ 40 
      (* (/ (length liste-rect-instancies) (length liste-rects-actifs)) 55))
    (de-prefixer-code-moteur (code-rect rect))
    (instant-rect rect)))





;;; Le choix de variable : on prend la plus contrainte

(defun variable-choisie (liste)
  (ct-choose-object liste 'rect-place-p 'liberte))


;;; Le choix de l'instant : on prend le moins charge
;;; construction d'une info: cons-info 

(defun valeur-choisie (rect)
  (ct-setq Liste-Rect-Instancies (cons rect Liste-Rect-Instancies))
  (ct-closed-and
    ()
    ;;(print (code-rect rect) "- " (liberte rect) " ")
    (ct-instantiate (position-rect rect) 'nb-chevauchements-rect)
    (sauver-rect rect)))


;;; LE PROGRAMME PRINCIPAL DU MOTEUR

(defun noyau ()
  (preinit)
  (lire-donnees-moteur)
  (initialiser-rects-a-resoudre '())
  (resoudre)
  (ecrire-resultat-moteur)     
  (liberer-memoire-moteur))


;;; LE PROGRAMME PRINCIPAL APPELLE DE L'INTERFACE AIDA

;;; <lancer-moteur>
;;; rem: la liste des cours est une liste de symboles de cours interface

(defun lancer-moteur (liste-cours-a-resoudre)
  (cons-info 'resolution 0 () ())
  (lire-donnees-moteur)
  (cons-info () 5 () ())
  (initialiser-rects-a-resoudre liste-cours-a-resoudre)
  (resoudre)
  (cons-info () 100 () ())
  ;   (when (not *erreurs*)
  (ecrire-resultat-moteur)
  (liberer-memoire-moteur))
