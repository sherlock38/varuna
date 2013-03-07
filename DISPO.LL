;;; --------------------------------------------------------------------------
;;; - nouveau dispo.ll                                          VARUNA 1.2   -
;;; - Utilitaires                                                            -
;;; - Module de definition des dispo                                         -
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; --------------------------------------------------------------------------
;;; - Creation:  Aout 93 MO                                                  -
;;; - Derniere mise a jour pour version 1.13: Aout 93 MO                     -
;;; - Build 1.22.1 01.07.99 / JH  MO                                         -
;;; --------------------------------------------------------------------------

;;; Une dispo represente des intervalles de temps de libre dans l'annee
;;; dans un format isomorphe au temps habituel minute, heure, jour, annee...

;;; Une dispo est un paire dont le      
;;;               car est une zone de moments cumules
;;;               cdr une zone de semaines (annuelles)
;;;
;;; ex:    ( ((0 . 510)(1140 . 2279))   ((1 . 5) (7 . 15)) )
;;;


;;; 1. les predicats de base
;;; reconnaissance du type, dispo vide 

(defun dispop (objet)
  (and 
    (paire-p objet)
    (zonep (car objet))
    (zonep (cdr objet))))


(defun dispo-vide-p (dispo)
  (or (null (trouver-dispo-moment dispo))
    (null (trouver-dispo-semaine dispo))))

(defvar DISPO-PLEINE '(((0 . 10080)) . ((1 . 53))))

(defun dispo-pleine-p (dispo)
  (and
    (equal (trouver-dispo-moment dispo) '((0 . 10080)))
    (equal (trouver-dispo-semaine dispo) '((1 . 53)))))

;;; 2. accesseurs et constructeur
;;;

;;; accede a la dispo moment d'une dispo

(defun trouver-dispo-moment (dispo)
  (when (dispop dispo) 
    (car dispo)))


;;; accede a la dispo semaine d'une dispo

(defun trouver-dispo-semaine (dispo)
  (when (dispop dispo)
    (cdr dispo)))



;;; construit une dispo a partir d'une dispo moment et d'une dispo semaine 
;;; rem (07/09/94) MO

(defun cons-dispo (dispo-moment dispo-semaine)
  (cons dispo-moment dispo-semaine))


;;; ecrase la dispo moment d'une dispo avec la dispo moment donnee en argument
;;; rem (7/09/94) PR
;;; il faut traiter un vrai rajout

(defun rajouter-dispo-moment (dispo-moment dispo)
  (cons-dispo dispo-moment (trouver-dispo-semaine dispo)))


;;; idem que precedente avec une dispo semaine

(defun rajouter-dispo-semaine (dispo-semaine dispo)
  (cons-dispo (trouver-dispo-moment dispo) dispo-semaine))


;;; 3. operations sur les dispo
;;;

;;; Intersection
;;;

(defun intersection-dispo (dispo1 dispo2)
  (cons-dispo
    (intersection-dispo-moment
      (trouver-dispo-moment dispo1)
      (trouver-dispo-moment dispo2))
    (intersection-dispo-semaine
      (trouver-dispo-semaine dispo1)
      (trouver-dispo-semaine dispo2))))

;;; la meme version n-aire (JH 4/10/94)

(defun intersection-n-dispo (l-dispo)
  (if (null (cdr l-dispo))
    (car l-dispo)
    (intersection-dispo 
      (car l-dispo)
      (intersection-n-dispo (cdr l-dispo))))) 

;;; pour plus de proprete, nous avons defini les deux primitives suivantes
;;; il faudra voir si on n'implemente pas une dispo TOUT  

(defun intersection-dispo-moment (dispo1 dispo2)
  (intersection-zone dispo1 dispo2))

(defun intersection-dispo-semaine (dispo1 dispo2)
  (intersection-zone dispo1 dispo2))



;;;  CONVERSION D'UNE DISPO d'une dispo en moments et semaines de debut de cours 
;;;    possibles

;;; reste a faire
;;; 1. filtre du cru PR: sur modele de faire semaine avec liste nombre inclus
;;; 2. intervalle-inclus-zone-p
;;; 3. moments-debut, semaines-debut et implementer la dispo ecole et les
;;;    accesseurs



;;; primitive calculant les moments de debut possible a l'interieur d'une
;;; dispo connaissant une duree et un type de cours


(defun faire-moments-debut (dispo duree type-rect)
  (if
    (dispo-vide-p dispo)
    ()     
    (sous-liste
      moment-debut-rect-possible-p
      (moments-debut type-rect)
      duree
      (trouver-dispo-moment dispo))))


;;; primitive calculant les semaines de debut possible a l'interieur d'une
;;; dispo connaissant un nombre de seances et un type de cours
;;; en passant par le fameux filtre du cru PR, qui permet de reduire le
;;; nombre de semaines en cas de trop grand nombre
;;; MOD - FD - 15/04/96 ajout de chevauchement comme arg

(defun faire-semaines-debut (dispo nb-seances type-rect chevauchement)
  (if (dispo-vide-p dispo) ()
    (filtre-du-cru-PR
      (sous-liste
        semaine-debut-rect-possible-p
        (semaines-debut type-rect)
        nb-seances
        (trouver-dispo-semaine dispo))
      chevauchement)))

;;; !! a inventer du genre de liste-nombres-inclus

(defun filtre-du-cru-pr (liste-semaines-debut chevauchement)
  (let
    ((nombre-elements                 
      (div
	(* Param-Chevauchement (length liste-semaines-debut))
	chevauchement)))
    (firstn
      (max 1 nombre-elements)
      (trier-mo-liste-entiers liste-semaines-debut))))

(defun trier-mo-liste-entiers (liste)
  (let ((liste-travail (copy liste)))
    (sort
      (lambda (x y) (plus-espace-p x y liste))
      liste-travail)))


(defun plus-espace-p (x y liste-element)
  (if (ge (coeff-espacement-dans-liste x liste-element)
    (coeff-espacement-dans-liste y liste-element))
      t ()))



;;; calcul du coefficient d'espacement d'un element dans une liste

(defun coeff-espacement-dans-liste (element liste)
  (coeff-espacement element 
    (element-precedent element liste) 
    (element-suivant element liste)))


;;; donne l'element precedent de l'argumetn dans la liste
;;; il y a surement moins couteux !!

(defun element-precedent (element liste)
  (if (eq element (car liste))
    (car (last liste))
    (nth (sub1 (numero-dans-liste element liste)) liste)))


;;; donne l'element suivant de l'argument dans la liste
;;; !! meme remarque que la precedente

(defun element-suivant (element liste)
  (or
    (cadr (member element liste))
    (car liste)))

(defun coeff-espacement (element element-precedent element-suivant)
  (add (abs (sub element element-precedent)) (abs (sub element-suivant element))))




;;; predicats verifiant qu'un moment (resp. une semaine) de debut est possible
;;; connaissant une duree (resp. un nombre de seances) et une dispo

(defun moment-debut-rect-possible-p (moment duree dispo-moment)
  (intervalle-inclus-zone-p
    (creer-intervalle moment (+ moment duree))
    dispo-moment))

;;; attention, semaine-figee peut etre a nil (JH 02/10/94)
;;; 

(defun semaine-debut-rect-possible-p (semaine nb-seances dispo-semaine)
  (and 
    (ge semaine (or semaine-figee 0))
    (if 
      (eq nb-seances 1)
      (appartenir-zone-p semaine dispo-semaine)    
      (intervalle-inclus-zone-p
;;;  FD - 14/06/95 pourquoi soustraire 1
;;;  correction du bug Evry:  !! A VERIFIER !!
;;;     intervalle ouvert a droite ?
;;;     6 cours en non-chevauchement qui ont une dispo de la semaine 1 a 14
;;;     et un nb de seances = 14
;;;     4 de ces cours etaient places de la semaine 1 a 14
;;;     les 2 autres de la semaine 2 a 15
;;;         (creer-intervalle semaine (sub1 (add semaine nb-seances)))
        (creer-intervalle semaine (add semaine nb-seances))
        dispo-semaine))))


;;; teste si un rectangle (ie un moment cumule, une duree cumulee, une semaine de 
;;; debut et un nombre de semaine) appartient à une dispo donnee


(defun appartenir-rectangle-dispo-p (moment duree semaine nb-semaines dispo)
  (and
    (moment-debut-rect-possible-p moment duree (trouver-dispo-moment dispo))
    (semaine-debut-rect-possible-p 
      semaine nb-semaines (trouver-dispo-semaine dispo))))
