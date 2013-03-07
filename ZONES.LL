;;; --------------------------------------------------------------------------
;;; - Zones.ll                                                 VARUNA 1.2    -
;;; - Utilitaires                                                            -
;;; - Utilitaire specifique de Varuna lie a la notion de zones               -
;;; --------------------------------------------------------------------------
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; - Creation: 93 - 94   MO                                                 -
;;; - Derniere mise a jour pour version 1.13::   07.11.94                    -
;;; - Build 1.22.1 01.07.99 / MO JH                                          -
;;; --------------------------------------------------------------------------

;;;  rem (24/05/95) MO
;;;  deux primitives aux noms etrangement ressemblants
;;;  intervalle-inclus-zone-p et inclusion-intervalle-zone


;;; INTERVALLES ET ZONES
;;;
;;; On definit ici un intervalle comme une paire pointee de car la borne inferieure
;;; de l'intervalle et de cdr la borne superieure
;;; l'intervalle [2;4[ se note (2 . 4)
;;; REMARQUE:    Tous les intervalles seront consideres semi-ouverts a droite

;;; On definit une zone comme une liste d'intervalles tous disjoints representant
;;; l'union de ces intervalles

;;; Voici quelques operateurs bien utiles sur ces notions d'intervalles et de zones



;;; I. PRIMITIVES SUR LA NOTION D'INTERVALLE
;;;


;;; 0. primitive de creation d'un intervalle
;;;
;;; cette primitive rend l'intervalle [borne-inf;borne-sup[ lorsque ces arguments
;;; sont precises () sinon.

(defun creer-intervalle l
  (when (eq (length l) 2)
    (let ((objet (cons (car l) (cadr l))))
      (when (intervallep objet) objet))))


;;; 1. predicats de base
;;;

(defun intervallep (objet)
  (or 
    (null objet)                ; intervalle vide
    (and  
      (consp objet) 
      (numberp (car objet))
      (numberp (cdr objet))
      (< (car objet) (cdr objet))
      t)))                   ; histoire que le predicat rende t

(defun intervalle-vide-p (intervalle)
  (null intervalle))

(defun appartenir-intervalle-p (valeur intervalle)
  (and (>= valeur (borne-inf-intervalle intervalle))
    (< valeur (borne-sup-intervalle intervalle))
    t))

(defun egalite-intervale-p (intervalle1 intervalle2)
  (and (eq (borne-inf-intervalle intervalle1)
    (borne-inf-intervalle intervalle2))
      (eq (borne-sup-intervalle intervalle1)
        (borne-sup-intervalle intervalle2))
      t))

(defun connexite-intervalle-p (intervalle1 intervalle2)
  (let ((inf1 (borne-inf-intervalle intervalle1))
    (inf2 (borne-inf-intervalle intervalle2))
    (sup1 (borne-sup-intervalle intervalle1))
    (sup2 (borne-sup-intervalle intervalle2)))
      (and (<= (max inf1 inf2) (min sup1 sup2))
        t)))


;;; 2. accesseurs et operateurs sur un intervalle
;;;

(defun borne-sup-intervalle (intervalle)
  (cdr intervalle))

(defun borne-inf-intervalle (intervalle)
  (car intervalle))

(defun longueur-intervalle (intervalle)
  (- (borne-sup-intervalle intervalle) (borne-inf-intervalle intervalle)))


;;; 3. operateurs et autres sur deux ou plusieurs intervalles
;;;


;;; intersection

(defun intersection-intervalle (intervalle1 intervalle2)
  (let
    ((borne-inf
      (max
	(borne-inf-intervalle intervalle1)
	(borne-inf-intervalle intervalle2)))
     (borne-sup
       (min
	 (borne-sup-intervalle intervalle1)
	 (borne-sup-intervalle intervalle2))))
    (when (> borne-sup borne-inf)
      (creer-intervalle borne-inf borne-sup))))

;;; union connexe

(defun union-intervalle-connexe (intervalle1 intervalle2)
  (when (connexite-intervalle-p intervalle1 intervalle2)
    (creer-intervalle 
      (min (borne-inf-intervalle intervalle1)
        (borne-inf-intervalle intervalle2))
      (max (borne-sup-intervalle intervalle1)
        (borne-sup-intervalle intervalle2)))))


;;; cette difference est un peu speciale, en effet, en sortie on a une zone 

(defun difference-intervalle (intervalle1 intervalle2)
  (difference-intervalle-inclus
    intervalle1
    (intersection-intervalle
      intervalle1
      intervalle2)))


;;; on est certain que intervalle2 est inclus dans intervalle1
;;; cette primitive renvoie bien une liste triee

(defun difference-intervalle-inclus (intervalle1 intervalle2)
  (if (intervalle-vide-p intervalle2)
    (creer-zone intervalle1)
    (creer-zone
      (creer-intervalle
        (borne-inf-intervalle intervalle1)
        (borne-inf-intervalle intervalle2))
      (creer-intervalle
        (borne-sup-intervalle intervalle2)
        (borne-sup-intervalle intervalle1)))))


;;; predicat d'inclusion

(defun inclusion-intervalle-p (intervalle1 intervalle2)
  (and (>= (borne-inf-intervalle intervalle1)
    (borne-inf-intervalle intervalle2))
      (<= (borne-sup-intervalle intervalle1)
        (borne-sup-intervalle intervalle2))
      t))


;;; inclusion au sens { int1 C interieur (int2)}

(defun inclusion-stricte-intervalle-p (intervalle1 intervalle2)
  (and (> (borne-inf-intervalle intervalle1)
    (borne-inf-intervalle intervalle2))
      (<= (borne-sup-intervalle intervalle1)
        (borne-sup-intervalle intervalle2))
      t))


;;; predicats operateurs plus petit et ordre inferieur

(defun intervalle-plus-petit-p (intervalle1 intervalle2)
  (and (< (borne-sup-intervalle intervalle1)
    (borne-inf-intervalle intervalle2))
      t))

(defun intervalle-ordre-inferieur-p (intervalle1 intervalle2)
  (and (< (borne-sup-intervalle intervalle1)
    (borne-sup-intervalle intervalle2))
      t))


;;; <trier-liste-intervalles> - CRE - FD - 02/08/96
;;; fonction de tri d'une liste d'intervalles disjoint en prenant en compte
;;; le debut (qui doit correspondre a la borne inf d'un intervalle de la
;;; liste

(defun trier-liste-intervalles (lst debut)
  (sort
      (lambda (x y)
        (let
          ((inf-x (borne-inf-intervalle x))
           (inf-y (borne-inf-intervalle y)))
          (if
            (or
                (and
		  (< inf-x debut)
		  (or (> inf-y debut) (= inf-y debut)))
                (and
		  (or (> inf-x debut) (= inf-x debut))
		  (< inf-y debut)))
            (> inf-x inf-y)
            (< inf-x inf-y))))
      lst))



;;; II. OUTILS SUR LES LISTES D'INTERVALLES OU ZONES
;;;

;;; Une zone est une liste finie d'intervalles disjoints classes par ordre
;;; croissant du type precedent.


;;; primitive de creation d'une zone: accepte en argument rien ou intervalle

(defun creer-zone l
  (when l
    (letn
      boucle
      ((liste l))
      (cond 
        ((null liste) ())
        ((intervallep (car liste))
          (ajouter-intervalle-zone
            (car liste)
            (boucle (cdr liste))))
        (t ())))))                     ;! ici, pourrait generer une
;! erreur  


;;; O. les accesseurs usuels sur les listes 
;;;


(defun premier-intervalle-zone (zone)
  (car zone))

(defun reste-zone (zone)
  (cdr zone))


;;; cette primitive doit traiter la fusion de l'intervalle ajoute avec d'eventuels 
;;; intervalles non disjoints
;;; on suppose que la zone est une liste triee et on renvoie une liste triee
;;; !! BOF: on doit pouvoir faire mieux que de d'abord faire l'intersection 

;;; chg (10/10/94) MO 
;;; (list intervalle) -> (creer-zone intervalle)
;;; Dbg (11/10/94) MO
;;; (creer-zone intervalle) -> (list intervalle)
;;; et oui sinon on se mord la queue: creer-zone appelle ajouter-intervalle qui 
;;; appelle creer-zone

(defun ajouter-intervalle-zone (intervalle zone)
  (cond ((null intervalle) zone)
    ((null zone) (list intervalle))
    (t 
      (let* ((premier (premier-intervalle-zone zone))
        (inf-pre (borne-inf-intervalle premier))
        (sup-int (borne-sup-intervalle intervalle)))

          (cond 
            ((< sup-int inf-pre)              ; debut de liste
              (cons intervalle zone))
            ((connexite-intervalle-p intervalle premier)
              (ajouter-intervalle-zone
                (union-intervalle-connexe
                  intervalle
                  premier)
                (reste-zone zone)))
            (t  (cons premier
              (ajouter-intervalle-zone
                intervalle
                (reste-zone zone)))))))))


;;; concatenation brute de zone (on est sur que les intervalles resultants sont
;;; disjoints)  rem: rapidite par rapport a union-zone qui fait une fusion controlee
;;; rem : la liste rendue est une liste triee, comme celles en entree
;;; rem (10/10/94) MO
;;; bof, ((1 . 2)) et ((2 . 3)) se concatene mal et le resultat est relativement 
;;; affreux dans tous les mauvais cas !!


(defun concatener-zone (zone1 zone2)
  (let
    ((zone-travail-1 zone1)
      (zone-travail-2 zone2)
      (zone-resultat (creer-zone)))

    (while zone-travail-1
      (let ((intervalle (nextl zone-travail-1)))
        (while (and zone-travail-2
          (pp (premier-intervalle-zone zone-travail-2)
            intervalle))
              (newl
                zone-resultat                              
                (nextl zone-travail-2)))
        (newl zone-resultat intervalle)))
    (while zone-travail-2 (newl zone-resultat (nextl zone-travail-2)))
    (reverse zone-resultat)))

;;; outil pour <concatener-zone>
(defun pp (int1 int2)
  (< (borne-sup-intervalle int1)
    (borne-inf-intervalle int2)))


;;; 1. quelques predicats
;;;

(defun zone-vide-p (zone)
  (null zone))

; !! il faudra rajouter le caractere "disjoint" des intervalles
; !! attention ( (510 . 600) () ) est une zone pour le moment

(defun zonep (objet)
  (or (null objet)
    (and
      (consp objet)
      (intervallep (car objet))
      (zonep (cdr objet)))))

(defun appartenir-zone-p (element zone)
  (if (zone-vide-p zone)
    ()
    (or (appartenir-intervalle-p 
      element 
      (premier-intervalle-zone zone))
        (appartenir-zone-p 
          element
          (reste-zone zone)))))

(defun intervalle-inclus-zone-p (intervalle zone)
  (cond 
    ((zone-vide-p zone)())
    ((intervalle-vide-p intervalle) t)
    (t (or 
      (inclusion-intervalle-p 
        intervalle 
        (premier-intervalle-zone zone))
      (intervalle-inclus-zone-p 
        intervalle
        (reste-zone zone))))))


;;; 2. les operateurs usuels
;;;

;;; rem (29/03/94)
;;; la difference, l'union et l'intersection mettent pas mal de temps 
;;; cela provient de l'appel au filtrage qui n'est pas excellent
;;; il faudrait songer a une primitive plus directe, peut-etre sur des listes
;;; triees.
;;; chg (09/05/94) MO: travail sur liste triee fait 


;;; 2.1 l'intersection
;;;
;;;  <intersection-zone> <intersection-n-zones> <intersection-liste-zones>

(defun intersection-zone (zone1 zone2)
  (if (zone-vide-p zone1)
    ()
    (concatener-zone
      (premier-filtrage-zone          ; le resultat du filtrage
        zone2
        (lambda 
          (intervalle)
          (intersection-intervalle 
            intervalle 
            (premier-intervalle-zone zone1))))

      (intersection-zone             ; le resultat de cette 2nde 
        (reste-zone zone1)     ; intersection est d'inter
        zone2))))             ; vide avec la precedente
                             
;;; <intersection-n-zones> est n-aire
                             
(defun intersection-n-zones l
  (if 
    (null (cdr l))
      (car l)
      (intersection-zone
        (car l)
        (apply 'intersection-n-zones  (cdr l)))))

;;; <intersection-liste-zones> accepte une liste de zones pour argument
;;; a la difference de la precedente

(defun intersection-liste-zones (liste-zones)
  (if
    (null (cdr liste-zones))
    (car liste-zones)
    (intersection-zone
       (car liste-zones)
       (intersection-liste-zones (cdr liste-zones)))))
                      

(defun union-zone (zone1 zone2)
       (if (zone-vide-p zone1)
           zone2
           (union-zone
                   (reste-zone zone1)
                   (ajouter-intervalle-zone
                           (premier-intervalle-zone zone1)
                           zone2))))


;;; difference intervalle \ zone
;;; on ne parcourt les deux zones qu'une seule fois en faisant des tests
;;; intervalle par intervalle

(defun difference-zone (zone1 zone2)
       (if (zone-vide-p zone2)
           zone1

           (let ((zone-t1 (cdr zone1))           ; zone de travail 1
                 (zone-t2 (cdr zone2))           ; zone de travail 2
                 (int1 (car zone1))              
                 (int2 (car zone2))
                 (zone-resultat ()))             ; zone resultat
                
                (while (and int1 int2)           
                       (cond

                             ((intervalle-plus-petit-p int2 int1)
                              (setq int2 (nextl zone-t2)))

                             ((intervalle-plus-petit-p int1 int2)
                              (newl zone-resultat int1)                                      
                              (setq int1 (nextl zone-t1)))

                             ((inclusion-intervalle-p int1 int2)
                              (setq int1 (nextl zone-t1)))

                             ((inclusion-stricte-intervalle-p int2 int1) 
                              (newl zone-resultat
                                      (creer-intervalle
                                              (borne-inf-intervalle int1)
                                              (borne-inf-intervalle int2)))
                              (setq int1
                                    (creer-intervalle
                                            (borne-sup-intervalle int2)
                                            (borne-sup-intervalle int1)))
                              (when (not int1) (setq int1 (nextl zone-t1)))
                              (setq int2 (nextl zone-t2)))

                             ((intervalle-ordre-inferieur-p int2 int1)
                              (setq int1
                                    (creer-intervalle
                                            (borne-sup-intervalle int2)
                                            (borne-sup-intervalle int1)))
                              (setq int2 (nextl zone-t2)))

                             ((intervalle-ordre-inferieur-p int1 int2)
                              (newl zone-resultat
                                    (creer-intervalle 
                                            (borne-inf-intervalle int1)
                                            (borne-inf-intervalle int2)))
                              (setq int1 (nextl zone-t1)))
   
                             (t (print "difference-zone: erreur"))))

                (when int1 (newl zone-resultat int1))
                
                (append
                        (reverse zone-resultat)
                        zone-t1))))


;;; inclusion d'un intervalle dans une zone

(defun inclusion-intervalle-zone-p (intervalle zone)
       (if (zone-vide-p zone)
           ()
           (or (inclusion-intervalle-p intervalle (premier-intervalle-zone zone))
               (inclusion-intervalle-zone-p intervalle (reste-zone zone))))) 

;;; inclusion d'une zone dans une autre

(defun inclusion-zone-p (zone1 zone2)
       (cond 
           ((zone-vide-p zone1) t)
           ((zone-vide-p zone2) ())
           (t (and (inclusion-intervalle-zone-p (premier-intervalle-zone zone1) zone2)
                   (inclusion-zone-p (reste-zone zone1) zone2)))))

;;; 3. quelques utilitaires bien "utiles" sur la notion de zones


;;; calcul de la longueur d'une zone

(defun longueur-zone (zone)
  (if (zone-vide-p zone)
      0
      (+ (longueur-intervalle (premier-intervalle-zone zone)) 
         (longueur-zone (reste-zone zone)))))
     

;;; garder-intervalles-zone renvoie la zone filtree sur les intervalles
;;; par filtre-intervalle
;;; attention: cette fonction depend de l'implementation des zones

(dmd garder-intervalles-zone (zone filtre-intervalle)
  `(sous-liste ,filtre-intervalle ,zone))
      




;;; transformation de la zone par un filtre d'intervalle
;;; !! ces trois primitives pourront etre habilement transformer en une seule

(defun premier-filtrage-zone
       (zone filtre-intervalle)
       (if (zone-vide-p zone)
           ()
           (ajouter-intervalle-zone
                   (funcall
                           filtre-intervalle
                           (premier-intervalle-zone zone))
                   (premier-filtrage-zone 
                           (reste-zone zone) 
                           filtre-intervalle
                           ))))




; transformation de la zone par deux filtres de bornes (borne-inf et sup)

(defun deuxieme-filtrage-zone 
       (zone filtre-borne-inf filtre-borne-sup)
       (let 
            ((intervalle-travail (premier-intervalle-zone zone)))
            (if (zone-vide-p zone)
                ()
                (ajouter-intervalle-zone
                        (creer-intervalle
                                (funcall 
                                        filtre-borne-inf 
                                        (borne-inf-intervalle intervalle-travail))
                                (funcall
                                        filtre-borne-sup
                                        (borne-sup-intervalle intervalle-travail)))
                        (deuxieme-filtrage-zone
                                (reste-zone zone)
                                filtre-borne-inf
                                filtre-borne-sup)))))



(defun troisieme-filtrage-zone
       (zone filtre-intervalle)
       (if (zone-vide-p zone)
           ()
           (union-zone
                   (funcall
                           filtre-intervalle
                           (premier-intervalle-zone zone))
                   (troisieme-filtrage-zone 
                           (reste-zone zone) 
                           filtre-intervalle
                           ))))


