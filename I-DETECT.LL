;;; --------------------------------------------------------------------------
;;; - I-Detect.ll                                               VARUNA 1.2   -
;;; - Interface Graphique                                                    -
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; - Une application qui gere les evenements souris pour la saisie des      -
;;; - contraintes                                                            -
;;; --------------------------------------------------------------------------
;;; - Creation:    Juin 94                                                   -
;;; - Build 1.22.1 01.07.99 / JH                                             -
;;; --------------------------------------------------------------------------




(eval-when (load eval local-compile)
  (defabbrev detector {application}:detector))

(eval-when (load eval local-compile)
  (defstruct {detector}
    color
    first-rect
    last-rect
    last-x
    last-y
    fantome
    mouse-still-down))

(in-package '{detector})


(defun detector (x y w h image zones)
  (:initialise-structure x y w h 
	 (new '{detector}) image zones))

;; initialisation : 
;; on n'oublie pas d'agrandir la grille !!
;; et de la mettre dans un view

(defun :initialise-structure (x y w h appli image zones)
  (:mouse-still-down appli ())
  (add-component appli 'grille image)
  (send 'grow image (sub w 30) h)
  (add-component appli 'user appli)
  (send-super '{detector} 'initialise-structure
    appli x y w h (view image))
  appli)


(de :draw-rect (appli rect-aida)
  (send 'window-clear-region (send 'window appli)
    (send 'x rect-aida)
    (send 'y rect-aida)
    (send 'width rect-aida)
    (send 'height rect-aida))
  (send 'redisplay appli rect-aida))

;;; cette procedure ajoute simultanement n rectangles
;;; elle est preferable a l'ajout d'un seul rectangle
;;; car elle permet d'optimiser l'affichage
;;; en calculant le rectangle englobant tous les rectangles
;;; <voir union-rect dans i-util.ll">

(de :add-multi-rect (appli liste-rect event)
  (let ((grille (component 'grille appli)))
    (:draw-rect appli
      (union-rect 
	(mapcar (lambda (rect)
	  (cond
	    ((eq ({event}:detail event) 0)
	      (send 'ajouter-rectangle grille rect))
	    ((eq ({event}:detail event) 1)
	      (send 'inverser-rectangle grille rect))
	    ((eq ({event}:detail event) 2)
	      (send 'retirer-rectangle grille rect))))
	liste-rect)))))
	
(de :en-marges (appli)
  ;; GRILLE
  (let* ((grille (component 'grille appli))
    (nb-grille (sub1 (send 'jours grille)))
    (sem (send 'semaines grille))
    (nb-lig (if sem 0 (length (send 'heures grille))))
    (nb-col (or
      (and sem (sub1 sem))
      (sub1 (length (send 'heures grille))))))
  (when
    (or 
      (eq (grille-rect (:last-rect appli)) -1)
      (eq (grille-rect (:first-rect appli)) -1))
    (:last-rect appli
      (cons-rectangle
	0
	(col1-rect (:last-rect appli))
	(col2-rect (:last-rect appli))
	(ligne1-rect (:last-rect appli))
	(ligne2-rect (:last-rect appli))))
    (:first-rect appli
      (cons-rectangle
	nb-grille
	(col1-rect (:first-rect appli))
	(col2-rect (:first-rect appli))
	(ligne1-rect (:first-rect appli))
	(ligne2-rect (:first-rect appli)))))
  (when
    (or
      (eq (col1-rect (:first-rect appli)) -1)
      (eq (col1-rect (:last-rect appli)) -1))
    (:last-rect appli
      (cons-rectangle
	(grille-rect (:last-rect appli))
	(sub1 nb-col)
	nb-col
	(ligne1-rect (:last-rect appli))
	(ligne2-rect (:last-rect appli))))
    (:first-rect appli
      (cons-rectangle
	(grille-rect (:first-rect appli))
	0
	1
	(ligne1-rect (:first-rect appli))
	(ligne2-rect (:first-rect appli)))))
  (when
    (or
      (eq (ligne1-rect (:first-rect appli)) -1)
      (eq (ligne1-rect (:last-rect appli)) -1))
    (:first-rect appli
	(cons-rectangle
	  (grille-rect (:first-rect appli))
	  (col1-rect (:first-rect appli))
	  (col2-rect (:first-rect appli))
	  0
	  1))
    (:last-rect appli
      (cons-rectangle
	(grille-rect (:last-rect appli))
	(col1-rect (:last-rect appli))
	(col2-rect (:last-rect appli))
	nb-lig
	(add1 nb-lig))))))

      

(de :gestion-rects (appli event)
  (let* ((grille (component 'grille appli))
    (ignore (:en-marges appli))
    (last-rect (:last-rect appli))
    (first-rect (:first-rect appli))
    (g1 (min (grille-rect last-rect) (grille-rect first-rect)))
    (g2 (max (grille-rect last-rect) (grille-rect first-rect)))
    (ngrille (sub g2 g1))
    (c1 (min (col1-rect last-rect) (col1-rect first-rect)))
    (c2 (max (col1-rect last-rect) (col1-rect first-rect)))
    (ncol (sub c2 c1))
    (l1 (min (ligne1-rect last-rect) (ligne1-rect first-rect)))
    (l2 (max (ligne1-rect last-rect) (ligne1-rect first-rect)))
    (nlig (sub l2 l1))
    (liste-rect ()))
      (for (i g1 1 g2)
	(for (j c1 1 c2)
	  (for (k l1 1 l2)
	    (newl liste-rect 
	      (cons-rectangle i j (add1 j) k (add1 k))))))
      (:add-multi-rect appli liste-rect event)))
      
      


;; gestion de l'evenement mouse-down
;; on calcule le creneau 
;; on ajoute ou on retire le rectangle du creneau

(de :down-event (appli event)
  (let* ((grille (component 'grille appli))
         (x ({event}:x event))
	 (y ({event}:y event))
	 (rectangle (send 'coordonnees2rectangle grille x y)))
	(when (and rectangle (not (:mouse-still-down appli)))
	  (:mouse-still-down appli t)
	  (:first-rect appli rectangle)
	  (:last-rect appli ())
	  (:last-x appli x)
	  (:last-y appli y)
	  (:fantome appli (rectangle x y 0 0)))))
	      
(de :drag-event (appli event)
  (let* ((grille (component 'grille appli))
         (x ({event}:x event))
	 (y ({event}:y event))
	 (fantome (:fantome appli)))
	   (when (:mouse-still-down appli)
	     (when
	       (or
		 (neq x (:last-x appli))
		 (neq y (:last-y appli)))
	       (with ((current-window (send 'window appli)))
		 (send 'erase-grow fantome
		   (sub (:last-x appli) (send 'x fantome))
		   (sub (:last-y appli) (send 'y fantome)))
		 (send 'display-grow fantome
		   (sub x (send 'x fantome))
		   (sub y (send 'y fantome)))
		 (:last-x appli x)
		 (:last-y appli y))))))
  
;; gestion de l'evenement drag

(de :up-event (appli event)
  (let* ((grille (component 'grille appli))
         (x ({event}:x event))
	 (y ({event}:y event))
	 (fantome (:fantome appli))
	 (last-rect (send 'coordonnees2rectangle grille x y)))
	(when (:mouse-still-down appli)
	  (with ((current-window (send 'window appli)))
	    (send 'erase-grow fantome
	      (sub (:last-x appli) (send 'x fantome))
	      (sub (:last-y appli) (send 'y fantome))))
	  (when last-rect
	    (:last-rect appli last-rect)
	    (:gestion-rects appli event))
	  (:mouse-still-down appli ()))))
  

(de :rectangles2zones (appli liste-index)
  (let ((grille (component 'grille appli)))
    (send 'liste-rectangles grille)
    (mapcar (lambda (rect)
      (list (grille-rect rect)
	(if liste-index
	  (cons
	    (nth (col1-rect rect) liste-index)
	    (nth (col2-rect rect) liste-index))
	  (cons
	    (add1 (col1-rect rect))
	    (add1 (col2-rect rect))))))
	      (send 'liste-rectangles grille))))

	

(defun :zone2rectangle1 (appli zone liste-index)
  (let* ((grille (car zone))
    (horaire1 (cadr zone))
    (horaire2 (caddr zone))
    (pos (if liste-index 
             (numero-dans-liste horaire1 liste-index)
	       (sub1 horaire1))))
   (if liste-index
     (when 
       (and 
	 (member horaire1 liste-index)
	   (member horaire2 liste-index))
	     (cons-rectangle grille pos (add1 pos) 0 1))
     (cons-rectangle grille pos (add1 pos) 0 1))))
      

(defun :faire-liste-rectangles (appli zones liste-index)
 (supprimer-occurences-vides
   (mapcar (lambda (z) (:zone2rectangle1 appli z liste-index)) zones)))


(defun :zones2rectangle (appli zones liste-index)
  (let ((liste-rects (:faire-liste-rectangles appli zones liste-index)))
    (mapc (lambda (rect)
      (let ((lrect ({vgrid}:liste-rects (component 'grille appli))))
	({vgrid}:liste-rects (component 'grille appli)
	  (cons rect lrect))))
	    liste-rects)))
	      
	    