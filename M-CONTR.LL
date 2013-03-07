;;; --------------------------------------------------------------------------
;;; - m-contr.ll                                                VARUNA 1.2   -
;;; - MOTEUR                                                                 -
;;; - Module de definition des contraintes, pose et primitive contraindre    -
;;; --------------------------------------------------------------------------
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; - Creation:  Septembre 94                                                -
;;; - Derniere mise a jour pour version 1.11:: Septembre                     -
;;; - Build 1.22.1 01.07.99 / JH MO                                          -
;;; --------------------------------------------------------------------------

;;; chg (23/11/94) MO
;;; j'ai remplace la condition vrai dans les poses de contrainte par une
;;; condition contrainte-a-poser qui verifie que les deux rects ne sont
;;; pas deja instancies 
;;; chg (17/01/95) MO
;;; ou autres acas pour supporter la resolution partielle (cf primitive)
;;; rem: il faudrait egalement mettre une globale dans cela, histoire 
;;; d'activer a souhait cette condition

;;; I. DEFINITIONS, POSE DES CONTRAINTES DU PROBLEME

;;; 1. Definition des contraintes

;;; 1.1 Contrainte de non chevauchement

(defctconstraint non-chevauchement
  ((instant-rect1 ct-fix-var) 
    duree1 nbsem1 
    (instant-rect2 ct-fix-var) duree2 nbsem2)
  schemas
  ((when (instant-rect1 = a)
    call (progn
      (nb-contraintes-rect (ct-object instant-rect1) 0)
      (retirer-contrainte (ct-object instant-rect2))
      (retire-instant-chevauchant 
        a duree1 nbsem1 instant-rect2 duree2 nbsem2)))
  (when (instant-rect2 = b)
    call (progn
      (nb-contraintes-rect (ct-object instant-rect2) 0)
      (retirer-contrainte (ct-object instant-rect1))
      (retire-instant-chevauchant 
        b duree2 nbsem2 instant-rect1 duree1 nbsem1)))))

;;; 1.2 Contrainte de proximite de semaines (elle est orientee)

(defctconstraint proximite-semaine
  ((dx ct-fix-var) (dy ct-fix-var))
  schemas
  ((when (dx >= a) assert (dy >= a))
    (when (dx <= a) assert (dy <= (call (add a (* 3 Nb-Moments)))))
    (when (dy >= a) assert (dx >= (call (sub a (* 3 Nb-Moments)))))
    (when (dy <= a) assert (dx <= a))))

;;; 1.3 Contrainte de precedence

(defctconstraint precedence
  ((dx ct-fix-var) nbsemx (dy ct-fix-var) nbsemy)
  schemas
  ((when (dx = a) assert (dy >= (call (add a (* Nb-Moments nbsemx)))))
    (when (dx >= a) assert (dy >= (call (add a (* Nb-Moments nbsemx)))))
    (when (dy = a) assert (dx <= (call (sub a (* Nb-Moments nbsemy)))))
    (when (dy <= a) assert (dx <= (call (sub a (* Nb-Moments nbsemy)))))))

;;; 1.4 Contrainte de simultaneite stricte de semaines

(defctconstraint meme-semaine ((instant1 ct-fix-var) (instant2 ct-fix-var))
  schemas
  ((when 
    (instant1 = a) 
    call
    (retire-instants-semaine-differente a instant2))
      (when 
        (instant2 = a) 
        call 
        (retire-instants-semaine-differente a instant1))))


;;; 1.5 Contrainte de jour different
;;; a venir

(defctconstraint jours-differents ((instant1 ct-fix-var) (instant2 ct-fix-var))
  schemas
  ((when 
    (instant1 = a) 
    call
    (retire-instants-meme-jour a instant2))
      (when 
        (instant2 = a) 
        call 
        (retire-instants-meme-jour a instant1))))


;;; 1.6 Contrainte de simultanéité de jour
;;; FD - 08/06/95
(defctconstraint meme-jour ((instant1 ct-fix-var) (instant2 ct-fix-var))
  schemas
  ((when 
    (instant1 = a) 
    call
    (retire-instants-jour-different a instant2))
   (when 
     (instant2 = a) 
     call 
     (retire-instants-jour-different a instant1))))

;;; 1.7 Contrainte de simultanéité stricte
;;; FD - 08/06/95
(defctconstraint simultaneite ((instant1 ct-fix-var) (instant2 ct-fix-var) )
  schemas
  ((when (instant1 = a) assert (instant2 = a))
   (when (instant2 = a) assert (instant1 = a))))

;;; 1.8 Contrainte de simultanéité d'horaire
;;; FD - 08/06/95
(defctconstraint meme-horaire ((instant1 ct-fix-var) (instant2 ct-fix-var))
  schemas
  ((when 
    (instant1 = a) 
    call
    (retire-instants-horaire-different a instant2))
   (when 
     (instant2 = a) 
     call 
     (retire-instants-horaire-different a instant1))))

;;; 1.9 Contrainte de k-écart-simultanéité
;;; FD RM - 21/03/96

(defctconstraint k-ecart-simultaneite
  ((instant1 ct-fix-var) (instant2 ct-fix-var) ecart)
  schemas
   ((when (instant1 = a)
      assert (instant2 = (call (add a (* ecart Nb-Moments)))))
    (when (instant2 = a)
      assert (instant1 = (call (sub a (* ecart Nb-Moments)))))))

;;; 1.10 Contrainte de simultaneite sur la fin du cours (dans la semaine)
;;; RM - 06/09/96

(defctconstraint simultaneite-fin-seance
  ((instant1 ct-fix-var) (instant2 ct-fix-var) duree1 duree2)
  schemas
  ((when (instant1 = a)
    assert (instant2 = (call (sub (add a duree1) duree2))))
   (when (instant2 = a)
     assert (instant1 = (call (sub (add a duree2) duree1))))))

;;; 2. Utilitaires associes aux contraintes

;;; On ajoute une contrainte au total

(defun ajouter-contrainte (x)
  (total-contraintes-rect x
    (add1 (total-contraintes-rect x))))

;;; On retire une contrainte en rect de processus (different)

(defun retirer-contrainte (x)
  (nb-contraintes-rect x
    (sub1 (nb-contraintes-rect x))))


;;; 3. Pose des contraintes

;;; 3.1 Non-chevauchement

(defun poser-non-chevauchement (x y)
  (if
    (eq x y) 
    (cons-erreur 'ERRMCNC x)
    (progn  
      (ajouter-contrainte x)
      (ajouter-contrainte y)
      (ct-post-constraint
        'non-chevauchement
        (position-rect x) (duree-rect x) (nb-seances-rect x)
        (position-rect y) (duree-rect y) (nb-seances-rect y)))))


;;; 3.2 Proximite de semaines

(defun poser-proximite-semaine (x y)
  (if 
    (eq x y)     
    (cons-erreur 'ERRMCPS x)    
    (ct-post-constraint
      'proximite-semaine (position-rect x) (position-rect y))))

;;; 3.3 Precedence

(defun poser-precedence (x y)
  (if 
    (eq x y)     
    (cons-erreur 'ERRMCP x)    
    (ct-post-constraint
      'precedence (position-rect x) (nb-seances-rect x)
        (position-rect y) (nb-seances-rect y))))


;;; 3.4 Simultaneite stricte de semaines

(defun poser-meme-semaine (rect1 rect2)
  (ct-post-constraint 'meme-semaine 
    (position-rect rect1) (position-rect rect2)))

;;; 3.5 jours differents

(defun poser-jours-differents (rect1 rect2)
  (if 
    (eq rect1 rect2)     
    (cons-erreur 'ERRMCJD rect1)    
    (ct-post-constraint 'jours-differents 
      (position-rect rect1) (position-rect rect2))))

;;; 3.6 meme jour
;;; FD - 08/06/95

(defun poser-meme-jour (rect1 rect2)
  (if 
    (eq rect1 rect2)     
    (cons-erreur 'ERRMEMEJOUR rect1)    
    (ct-post-constraint 'meme-jour
      (position-rect rect1) (position-rect rect2))))

;;; 3.7 simultaneite stricte
;;; FD - 08/06/95

(defun poser-simultaneite (rect1 rect2)
  (if 
    (eq rect1 rect2)     
    (cons-erreur 'ERRSIMULTANEITE rect1)    
    (ct-post-constraint 'simultaneite
      (position-rect rect1) (position-rect rect2))))

;;; 3.8 meme horaire
;;; FD - 08/06/95

(defun poser-meme-horaire (rect1 rect2)
  (if 
    (eq rect1 rect2)     
    (cons-erreur 'ERRMEMEHORAIRE rect1)    
    (ct-post-constraint 'meme-horaire
      (position-rect rect1) (position-rect rect2))))

;;; 3.9 k-ecart-simultaneite
;;; FD RM - 21/03/96

(defun poser-k-ecart-simultaneite (rect1 rect2 ecart)
  (if 
    (eq rect1 rect2)     
    (cons-erreur 'ERRKECARTSIM rect1)    
    (ct-post-constraint 'k-ecart-simultaneite
      (position-rect rect1) (position-rect rect2) ecart)))

;;; 3.10 simultaneite-fin-seance
;;; RM - 06/09/96

(defun poser-simultaneite-fin-seance (rect1 rect2)
  (if 
    (eq rect1 rect2)     
    (cons-erreur 'ERRSIMULFINSEANCE rect1)    
    (ct-post-constraint 'simultaneite-fin-seance
      (position-rect rect1) (position-rect rect2)
      (duree-rect rect1) (duree-rect rect2))))


;;; II. PRIMITIVE GENERALE DE POSE DES CONTRAINTES

(defun contraindre ()
  (when compter-contraintes-p
    (setq compteur-contraintes-posees 0.))
  (poser-m-contraintes-explicites)
  (poser-m-contraintes-groupes)
  (maj-nb-contrainte))


;;; maj-nb-contrainte
;;; apres la pose des contraintes, on met a jour nb-contrainte

(defun maj-nb-contrainte ()
  (mapc
    (lambda (r)
      (nb-contraintes-rect r (total-contraintes-rect r))) liste-rect))


;;; pose les contraintes de la Liste-m-Contraintes

(defun poser-m-contraintes-explicites ()
  (mapc 
    (lambda (contr)
      (pose-contrainte-liste-non-transitive
        (car contr)
        (evlis1 (cdr contr))
        'contrainte-a-poser-p)) 
    Liste-m-Contraintes))

;;; pose des contraintes de groupes 

(defun poser-m-contraintes-groupes ()
  (mapc
    (lambda (x)
      (pose-m-contraintes-m-groupe (eval x)))
    Liste-m-Groupes))

;;; pose des contraintes sur un groupe

(defun pose-m-contraintes-m-groupe (m-groupe)
  (if
    (null (contraintes-m-groupe m-groupe))
    ()     
    (pose-contraintes-rects-fils 
      (contraintes-m-groupe m-groupe)
      (rects-m-groupe m-groupe)
      (nb-td-m-groupe m-groupe))
    (pose-contraintes-groupes-fils
      (contraintes-m-groupe m-groupe) 
      (rects-m-groupe m-groupe)
      (groupes-m-groupe m-groupe))))

;;; MOD FD RM - 21/03/96 (le printemps ...)
;;; On diff‚rencie la pose de contrainte en fonction du type de contrainte
;;; - symetrique non transitive (nc)
;;; - symetrique transitive (simultaneite...)

(defun pose-contraintes-rects-fils (liste-noms-contraintes rects ecart)
  (mapc 
    (lambda
      (contr)
      (cond
	((appartenir-liste contr
	  '(poser-non-chevauchement poser-jours-differents))
	  (pose-contrainte-liste-non-transitive contr rects
	    'contrainte-a-poser-p))
	((appartenir-liste contr
	  '(poser-proximite-semaine poser-precedence poser-meme-semaine
	    poser-meme-jour poser-simultaneite poser-meme-horaire
	    poser-simultaneite-fin-seance))
	  (pose-contrainte-liste-transitive contr rects 'contrainte-a-poser-p))
	((eq contr 'poser-k-ecart-simultaneite)
	  (pose-contrainte-liste-k-ecart contr rects 'contrainte-a-poser-p ecart))
	(t ())))
    liste-noms-contraintes))

;;; dbg (28/09/94) MO
;;; je ne posais les contraintes qu'entre les rects et les rects des
;;; sous-groupes. Il faut aussi les poser entre les sous-groupes

;;; ajt (23/01/95) MO
;;; ajout de la carac du groupe construit dans cons-m-groupe
;;; groupe actif par defaut

;;; MED (22/06/95) MO RM: quand il y a plus de deux contraintes, la deuxieme
;;; n'est pas posee
;;; DBG (22/06/95) MO RM: un nextl dans une lambda.... superbe manfred....
;;; courage pascal

(defun pose-contraintes-groupes-fils 
  (liste-noms-contraintes rects symboles-groupes)
  (let*
    ((sous-groupe-rects
      (when rects (cons-m-groupe () () rects () 'actif 0))) ;; AJT 0 04/96
     (liste-sous-groupe 
       (if
         rects
         (cons sous-groupe-rects (evlis1 symboles-groupes))
         (evlis1 symboles-groupes)))
     (sous-groupe))
    (while
      liste-sous-groupe
      (setq sous-groupe (nextl liste-sous-groupe))
      (mapc 
        (lambda (contr) 
          (pose-contrainte-entre-deux-listes
            contr 
            (tous-les-rects-fils (list sous-groupe))
            ;; -> !! c'etait la !! snif....!!!!
            ;; (nextl liste-sous-groupe)
            (tous-les-rects-fils liste-sous-groupe)
            'contrainte-a-poser-p))
        liste-noms-contraintes))))

;;; TRV 28/06/95 MO FD
;;; ??? tous les rects fils ne faitil pas des doublons ?


(defun tous-les-rects-fils (liste-m-groupes)
  (if
    (null liste-m-groupes)
    ()
    (let ((premier (car liste-m-groupes))
      (reste (cdr liste-m-groupes)))
        (append (rects-m-groupe premier)
          (tous-les-rects-fils (evlis1 (groupes-m-groupe premier)))
          (tous-les-rects-fils reste)))))



;;; III. PRIMITIVES DE POSE DE CONTRAINTES ENTRE DES LISTES

;; Pose d'une contrainte entre elements d'une liste deux a deux

(defun pose-contrainte-liste-non-transitive (contrainte liste condition)
  (when (cdr liste)
    (let ((x (car liste)))
      (mapc
        (lambda (y)
          (when 
            (funcall condition x y) 
	    (setq *contrainte* 
               (list contrainte (code-rect x) (code-rect y)))
	    ;; AJT RM : 3/01/97 compteur de contraintes
	    (when compter-contraintes-p
	      (incr compteur-contraintes-posees))
	    (funcall contrainte x y)))
          (cdr liste)))
    (pose-contrainte-liste-non-transitive contrainte (cdr liste) condition)))

(defun pose-contrainte-liste-transitive (contrainte liste condition)
  (let
    ((x (car liste))
     (y (car (last liste)))) 
    (when (funcall condition x y) 
      (setq *contrainte* 
	(list contrainte (code-rect x) (code-rect y)))
      ;; AJT RM : 3/01/97 compteur de contraintes
      (when compter-contraintes-p
	(incr compteur-contraintes-posees))
      (funcall contrainte x y))
    (pose-contrainte-liste-transitive-recursive contrainte liste condition)))

(defun pose-contrainte-liste-transitive-recursive (contrainte liste condition)
  (let
    ((x (car liste))
     (y ())) 
    (when (cdr liste)
      (setq y (cadr liste))
      (when (funcall condition x y) 
	(setq *contrainte* 
	  (list contrainte (code-rect x) (code-rect y)))
	;; AJT RM : 3/01/97 compteur de contraintes
	(when compter-contraintes-p
	  (incr compteur-contraintes-posees))
	(funcall contrainte x y))
      (pose-contrainte-liste-transitive-recursive
	contrainte (cdr liste) condition))))

(defun pose-contrainte-liste-k-ecart (contrainte liste condition ecart)
  (let
    ((x (car liste))
     (y (car (last liste)))) 
    (when (funcall condition x y) 
      (setq *contrainte* 
	(list contrainte (code-rect x) (code-rect y)))
      ;; AJT RM : 3/01/97 compteur de contraintes
      (when compter-contraintes-p
	(incr compteur-contraintes-posees))
      (funcall contrainte x y (* ecart (length (cdr liste)))))
    (pose-contrainte-liste-k-ecart-recursive
      contrainte liste condition ecart)))

(defun pose-contrainte-liste-k-ecart-recursive
  (contrainte liste condition ecart)
  (let
    ((x (car liste))
     (y ())) 
    (when (cdr liste)
      (setq y (cadr liste))
      (when (funcall condition x y) 
	(setq *contrainte* 
	  (list contrainte (code-rect x) (code-rect y)))
	;; AJT RM : 3/01/97 compteur de contraintes
	(when compter-contraintes-p
	  (incr compteur-contraintes-posees))
	(funcall contrainte x y ecart))
      (pose-contrainte-liste-k-ecart-recursive
	contrainte (cdr liste) condition ecart))))

;;; (21/04/94) MO
;;; cette primitive pose les contraintes entre deux listes de cours et permet 
;;; donc de traiter les cours de frequences multiples 

(defun pose-contrainte-entre-deux-listes (contrainte liste1 liste2 condition) 
  (if (or (null liste1) (null liste2))
    ()
    (mapc
      (lambda (y)
        (when 
          (funcall condition (car liste1) y)
	  ;; RM - 11/07/95
           (setq *contrainte* 
             (list contrainte (code-rect (car liste1))
                 (code-rect y)))
	   ;; AJT RM : 3/01/97 compteur de contraintes
	   (when compter-contraintes-p
	     (incr compteur-contraintes-posees))
	   (funcall contrainte (car liste1) y)))
      liste2)
    (pose-contrainte-entre-deux-listes 
      contrainte (cdr liste1) liste2 condition)))

;;; (21/04/94) MO
;;; la primitive la plus generale qui soit, n-liste est une liste de n listes

(defun pose-contrainte-entre-n-listes (contrainte n-liste condition)
  (when (cdr n-liste)
    (mapc
      (lambda (y)
        (pose-contrainte-entre-deux-listes 
          contrainte
          (car n-liste)
          y
          condition))
      (cdr n-liste))
    (pose-contrainte-entre-n-listes 
      contrainte (cdr n-liste) condition)))





