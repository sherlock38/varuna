;;; --------------------------------------------------------------------------
;;; - I-vtype                                                   VARUNA 1.2   -
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; - Interface Graphique                                                    -
;;; - Creation de la grille proportionnelle Emploi du Temps                  -
;;; --------------------------------------------------------------------------
;;; - Creation:  Aout 94                                                     -
;;; - Mise a jour pour version 1.8:                                          -
;;; - Build 1.22.1 01.07.99 / JH MO                                          -
;;; --------------------------------------------------------------------------

;;; MO trv 19/09/95

(eval-when (load eval local-compile)
  (defabbrev vgrid {rectangle}:vgrid))

(eval-when (load eval local-compile) 
  (defstruct {vgrid} 
    heures jours semaines
    ofset
    legend-h legend-v
    type
    ys-grilles
    ys-heures
    xs-heures
    xs-semaines
    liste-rects
    pattern
    ))

(in-package '{vgrid})

;;; vgrid renvoie une image reprsesentant une grille
;;; gere les deux types de grille porportionnelle verticale
;;; et proportionnelle horizontale.
;;; positionnee en x et y de largeur w et de hauteur h
;;; les-heures est une liste d'entier qui repr‚sentent la proportionalite
;;; entre les lignes ou les colonnes
;;; legend-h et legend-v sont les legendes verticales et horizontales,
;;; nb-jours est le nombre de Jours a afficher
;;; nb-semaines est le nombre de semaines a afficher

(defun vgrid (x y w h les-heures nb-jours nb-sem legend-h legend-v)
  (:initialise-structure (new '{vgrid}) x y w h 
    les-Heures nb-jours nb-sem legend-h legend-v))

(defun :initialise-structure
  (grid x y w h les-heures nb-Jours nb-sem legend-h legend-v)
  ({rectangle}:initialise-structure grid x y w h)
  (:legend-h grid legend-h)
  (:legend-v grid legend-v)
  (:ofset grid 10)
  (:jours grid nb-Jours)
  (:heures grid les-heures)
  (:semaines grid nb-sem)
  (if nb-sem
    (:type grid 'planing)
    (:type grid 'semaine))
  (:pattern grid DISPO-PAT)
  grid)

(defun :prin (grid)
  (prin "<vgrid;" (:type grid) "x" ";" (:heures grid) ">"))
    
(defun :grow (grid w h)
  (setq grid ({rectangle}:grow grid w h)))
    
;;; positionne les rectangles a griser
;;;
    
(defun :ajouter-retirer-rectangle (grille rect ajout)
  (let* ((pos (:rectangle2position grille rect))
    (lr (:liste-rects grille)))
      (ifn ajout
        (:liste-rects grille (delete rect lr))
        (:liste-rects grille (cons rect lr)))
      (rectangle
        (x-pos-rect pos)
        (y-pos-rect pos)
        (w-pos-rect pos)
        (h-pos-rect pos))))

(defun :rect-in (grille rect)
  (member rect (:liste-rects grille)))

(defun :retirer-rectangle (grille rect)
  (when (:rect-in grille rect)
    (:ajouter-retirer-rectangle grille rect ())))

(defun :ajouter-rectangle (grille rect)
  (when (not (:rect-in grille rect))
    (:ajouter-retirer-rectangle grille rect t)))

(defun :inverser-rectangle (grille rect)
  (:ajouter-retirer-rectangle grille rect (not (:rect-in grille rect))))
      
(defun :liste-rectangles (grille)
  (:liste-rects grille))

;;; renvoie la hauteur d'une grille

(defun :taille-verticale-grille-jour (grille)
  (div
    (sub
      (send 'height grille)
      (add
        #2hchar
        (mul
          (:jours grille)
          (:ofset grille))))
    (:jours grille)))

;;; renvoie la largeur d'un grille

(defun :taille-horizontale-grille-jour (grille)
  (sub
    (send 'width grille)
    (mul #2wchar (max-string-len (:legend-h grille)))))
    

;;; renvoie la coordonee y de la grille no-grille

(defun :y-grille (grille no-grille)
  (add #2hchar
    (mul no-grille
      (add
        (:ofset grille)
        (:taille-verticale-grille-jour grille)))))

;;; construit le vecteur des coordonnees des grilles 

(defun :vecteur-jours-y (grille dy)
  (let ((v (makevector (:jours grille) ())))
    (for (i 0 1 (sub1 (:jours grille)))
      (vset v i (add dy (:y-grille grille i))))
    v))
 

;;; "grise" les rectangles passes en vecteurs en utilisant le pattern
;;; defini en globale DISPO-PAT
;;; renvoie une structure de type rectangle aida a reafficher en coordonnees
;;; locales (l'appli hote doit eventuellement les corriger
;;; avant le redisplay)
      
(defun :remplir-rectangles (grille)
  (let* ((nrect (length (:liste-rects grille)))
    (vx (makevector nrect ()))
    (vy (makevector nrect ()))
    (vw (makevector nrect ()))
    (vh (makevector nrect ())))
      (for (i 0 1 (sub1 nrect))
        (let* (
          (rect (nth i (:liste-rects grille)))
          (pos (:rectangle2position grille rect)))
            (vset vx i (add1 (x-pos-rect pos)))
            (vset vy i (add1 (y-pos-rect pos)))
            (vset vw i (sub1 (w-pos-rect pos)))
            (vset vh i (sub1 (h-pos-rect pos)))))
      (with ((current-pattern (:pattern grille)))
        (fill-rectangles nrect vx vy vw vh))))

;;; affiche les grilles pour une semaine

(defun :afficher-semaine (dx dy grille)
  (let*
    ((nrect (:jours grille))
      (nseg (length (:heures grille)))
      (x (add dx (mul
        #wchar
        (add1 (max-string-len (:legend-v grille))))))
      (y (add dy #2hchar))
      (w (sub1 (:taille-horizontale-grille-jour grille)))
      (h (:taille-verticale-grille-jour grille))
      (vy (:vecteur-jours-y grille dy))
      (weq (sub (car (last (:heures grille))) (car (:heures grille))))
      (unite (/ w weq))
      (xs (makevector nseg ()))
      (y1 (makevector nseg ()))
      (y2 (makevector nseg ())))
    
;;; LES JOURS
  (draw-rectangles nrect x vy w h)
  
;;; LES HEURES
  (for (i 0 1 (sub1 (:jours grille)))
    (for (j 0 1 (sub1 nseg))
      (vset xs j (add x
        (floor (* unite (sub
          (nth j (:heures grille))
          (car (:heures grille)))))))
      (vset y1 j (vref vy i))
      (vset y2 j (add h (vref vy i))))
    (draw-string
      (add1 dx)
      (add #hchar (vref vy i))
      (nth i (:legend-v grille)))
    (draw-segments nseg xs y1 xs y2))

  
;;; LES LEGENDES (les heures) 
  (for (j 0 1 (- nseg 2))
    (draw-string
      (- (vref xs j) 6)
      (add dy (sub #hchar 2))
      (nth j (:legend-h grille))))
        
;;; VARIABLES INTERNES - COORDONNEE
;;; on sauve
  (:ys-grilles grille vy)
  (:xs-heures grille xs)
  (:ys-heures grille ())
  (:xs-semaines grille ())
  
;;; ON GRISE LES EVENTUELS RECTANGLES
  (:remplir-rectangles grille)))

;;; affiche les grilles pour plusieurs semaines
;;; c'est sensiblement la meme chose.
;;; il faut en plus dessiner les semaines
;;; et transposer l'affichage des heures

(defun :afficher-planing (dx dy grille)
  (let*
    ((nrect (:jours grille))
      (nseg (length (:heures grille)))
      (nbsem (:semaines grille))
      (x (add dx (mul
        #wchar
        (add1 (max-string-len (:legend-v grille))))))
      (y (add dy #2hchar))
      (w (sub (:taille-horizontale-grille-jour grille) 50))
      (h (:taille-verticale-grille-jour grille))
      (vy (:vecteur-jours-y grille dy))
      (weq (sub (car (last (:heures grille))) (car (:heures grille))))
      (unite-heure (/ h weq))
      (unite-semaine (/ w (sub1 nbsem)))
      (yss ())
      (x1 (makevector nseg ()))
      (x2 (makevector nseg ()))
      (ys (makevector nseg ()))
      (xs (makevector nbsem ()))
      (y1 (makevector nbsem ()))
      (y2 (makevector nbsem ())))
    
;;; LES JOURS
  (draw-rectangles nrect x vy w h)
  
  (for (i 0 1 (sub1 (:jours grille)))

;;; LES HEURES
    (for (j 0 1 (sub1 nseg))
      (vset ys j
        (add
          (mul i (add h (:ofset grille)))
          (add y (floor (* unite-heure
            (sub
              (nth j (:heures grille))
              (car (:heures grille))))))))
      (vset x1 j x)
      (vset x2 j (add w x)))

;;; LES SEMAINES
    (for (k 0 1 (sub1 nbsem))
      (vset xs k
        (add x (floor (* unite-semaine k))))
      (vset y1 k (vref vy i))
      (vset y2 k (add (vref vy i) h)))
      
    (draw-string (add1 dx) (add #hchar (vref vy i))
      (nth i (:legend-v grille)))
    (draw-segments nseg x1 ys x2 ys)
    (draw-segments nbsem xs y1 xs y2)
    (when (eq i 0)
      (setq yss (copy ys))))
    (for (i 0 1 (- nbsem 2))
      (draw-string (vref xs i) #hchar (nth i (:legend-h grille))))
   
;;; VARIABLES INTERNES - COORDONNEE
;;; on sauve
  (for (j 0 1 (sub1 nseg))
    (vset ys j (floor (* unite-heure (nth j (:heures grille))))))
  (:ys-grilles grille vy)
  (:xs-heures grille ())
  (:ys-heures grille yss)
  (:xs-semaines grille xs)
  (:remplir-rectangles grille)))


;;; Le display - branche suivant le type de la grille
;;; !!penser un jour a enlever les makevector dans le display
;;; !!je pense que c'est assez gourmand en memoire ?


(defun :display (grid dx dy region)
  (if (eq (:type grid) 'semaine)
    (:afficher-semaine dx dy grid)
    (:afficher-planing dx dy grid)))

;;; ACCESSEURS POUR LE SYSTEME DE COORDONNES

;;; renvoie le vecteur contenant les coordonnees du rectangle
;;; defini par :
;;; grille, colone1, colone2, lc1, lc2 -> <x1,y1,w,h>  
;;; pour un affichage de type semaine

(defun :rect2pos-semaine (grille gr c1 c2)
  (cons-position-rectangle
    (vref (:xs-heures grille) (or (lt c1 (vlength (:xs-heures grille))) 0))
    (vref (:ys-grilles grille) gr)
    (sub
      (vref (:xs-heures grille) (or (lt c2 (vlength (:xs-heures grille))) 1))
      (vref (:xs-heures grille) (or (lt c1 (vlength (:xs-heures grille))) 0)))
    (:taille-verticale-grille-jour grille)))
   
;;; meme chose pour une planning

(defun :rect2pos-planning (grille gr c1 c2 lc1 lc2)
  (cons-position-rectangle
    (vref (:xs-semaines grille) c1)
    (add
      (vref (:ys-grilles grille) gr)
      (sub
        (vref (:ys-heures grille) lc1)
        (vref (:ys-heures grille) 0)))
    (sub
      (vref (:xs-semaines grille) c2)
      (vref (:xs-semaines grille) c1))
    (sub
      (vref (:ys-heures grille) lc2)
      (vref (:ys-heures grille) lc1))))
   
;;; renvoie la position du rectangle suivant le type de la grille

(defun :rectangle2position (grille rect)
  (if (eq (:type grille) 'semaine)
    (:rect2pos-semaine grille
      (grille-rect rect)
      (col1-rect rect)
      (col2-rect rect))
    (:rect2pos-planning grille
      (grille-rect rect)
      (col1-rect rect)
      (col2-rect rect)
      (ligne1-rect rect)
      (ligne2-rect rect))))

            
    
;;; renvoie l'ordre le plus proche de val dans le vecteur vect+min[]

(defun :trouver-index-inferieur (grille vect min val)
  (let* ((n (if vect (vlength vect) 0))
    (i 0))
      (if (and vect (lt val (add min (vref vect 0))))
        -1
        (while
          (and
            (lt i n)
            (gt val (add min (vref vect i))))
          (incr i))
        (max 0 (min (sub1 i) (sub1 n))))))
        
          
;;; renvoie le numero de la grille correspondant a la coordonnee y

(defun :trouver-grille (grille y)
  (:trouver-index-inferieur grille (:ys-grilles grille) 0 y))

;;; meme chose avec numero de ligne

(defun :trouver-ligne (grille no-grille y)
  (let ((n (if (:ys-heures grille) (vlength (:ys-heures grille)) 2)))
    (min 
      (:trouver-index-inferieur
        grille
        (:ys-heures grille)
        (if 
          (ge no-grille 0)
          (sub
            (vref (:ys-grilles grille) no-grille)
            (if 
              (:ys-heures grille)
              (vref (:ys-heures grille) 0)
              0))
          0)
        y)
     (sub n 2))))

;;; meme chose avec colonne

(defun :trouver-colonne (grille no-grille x)
  (let ((n (vlength (or (:xs-semaines grille) (:xs-heures grille)))))
    (min
        (:trouver-index-inferieur
          grille
          (or (:xs-semaines grille) (:xs-heures grille))
          0
          x)
        (sub n 2))))
  
;;; revoie une structure du type rectangle varuna 
;;; englobant le point x y

(defun :coordonnees2rectangle (grille x y)
  (let* ((gr (:trouver-grille grille y))
    (line (when gr (:trouver-ligne grille gr y)))
    (col (when gr (:trouver-colonne grille gr x))))
      (cons-rectangle gr col (add1 col) line (add1 line))))



