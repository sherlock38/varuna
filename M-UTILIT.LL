;;; --------------------------------------------------------------------------
;;; - m-utilit.ll                                              VARUNA 1.2    -
;;; - Utilitaires                                                            -
;;; - Utilitaires generaux du moteur                                         -
;;; --------------------------------------------------------------------------
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; - Creation: 93 - 94                                        PR JH MO OJ   -
;;; - Maj 1.10: Janvier 95                                           MO      -
;;; - Derniere mise a jour pour version 1.13::   07.11.94                    -
;;; - Build 1.22.1 01.07.99 / JH                                             -
;;; --------------------------------------------------------------------------

;;; ajt FD - 21/06/95
;;;  ajout de la fonction <retire-instants-horaire-different> car on a ajouter
;;;  la contrainte <meme jour>

;;; AJT (34/01/95) MO
;;; ajout d'un element dans la structure d'un m-groupe: la caracteristique
;;; cf cons-m-groupe, carac-m-groupe
;;; pour l'instant, je ne m'en sers pas


;;; I. CALCUL DU COEFF DU RECT 

;;; Mesures initiales : divers calculs sur la surface du rect
;;; qui ne necessitent pas d'avoir fabrique le domaine de la variable


;;; la surface de rect est l'aire du rectangle de rect en minutes

(defun surface-rect (rect)
  (* (duree-rect rect) (nb-seances-rect rect)))

;;; la taille de domaine initial est le nombre d'heure de debut de rect 
;;; possible pour le rect

(defun taille-domaine-initial (rect)
  (*
    (length (moments-rect rect))       ; domaine de moments possibles
    (length (semaines-rect rect))))    ; domaine de semaines possibles


;;; calcul de l'espace initial reel possible pour le rect en surface

(defun espace-initial-possible (rect)
  (*
    (longueur-zone
      (trouver-dispo-moment (dispo-rect rect)))     
    (+ 1 (longueur-zone
      (trouver-dispo-semaine (dispo-rect rect))))))


;;; calcul du coeff de rect qui servira a l'elaboration du degre de liberte
;;; valeurs particulieres du coeff
;;;
;;;      coeff = 1         quand il n'y a pas superposition des possibilites
;;;                        initiales pour le rect
;;;      coeff < 1         sinon
;;;     
;;;      coeff -> 0        lorsque le nombre d'horaires de debut de rect 
;;;                        augmente et que la surface du rect est plus grande.

;;; DBG (08/06/94) JH (corrections dans le noyau)
;;; pour les rect figes par <figer emploi du temps> c'est a dire, l'emploi du 
;;; temps est fige jusqu'a une semaine donnee, le coefficient du rect ne peut 
;;; etre calcule. En effet, il peut arriver que la liste des semaines 
;;; possibles pour ce rect soit vide.
;;; (donc surface rect = 0) donc coeff indetermine.

;;; REM (28/03/95) MO
;;; Attention aux cas ou on a:  divided by zero !
;;; sont-ils bien tous traites en amont. N'est-ce pas ici qu'il faut faire le 
;;; traitement ???

(defun calcul-coeff-rect (rect)
  (/
    (espace-initial-possible rect)
    (*
      (taille-domaine-initial rect)
      (surface-rect rect))))




;;; II. FABRICATION DU DOMAINE DE LA VARIABLE CONTRAINTE ASSOCIEE AU RECT

(defun faire-domaine (rect)
  (ct-set-domain
    (position-rect rect)
    (apply
      'ct-fix-var
      (mapcar2 (lambda (x y) (cons-instant x y)) 
        (semaines-rect rect) (moments-rect rect)))))


;;; Taille du domaine de cette variable contrainte
(defun taille-domaine (rect) (ct-domain-size (position-rect rect)))

;;; Nombre de surfaces elementaires disponibles pour la variable
(defun aire-unitaire (rect)
  (floor 
    (* (coeff-rect rect) (taille-domaine rect))))

;;; Mesure de la liberte de placement d'un rect, i.e. de la variable
;;;  ... ceci nous fournit l'inverse du niveau de contrainte

(defun liberte (rect)
  (let*
    ((n (aire-unitaire rect))
      (p (1+ (nb-contraintes-rect rect))))
    (cond
      ((< (* 2 n) p) 
        (/ n p)) 
      (t (- n (* 0.5 (1- p)))))))

;(defun liberte (rect)
; (if 
;(eq (nb-contraintes-rect rect) 0)
;1 
; (/ 1 (nb-contraintes-rect rect))))



(defun moins-libre (rect1 rect2)
  (cond
    ((null (coeff-rect rect1)) ())
    ((null (coeff-rect rect2)) rect1)     
    ((< (liberte rect1) (liberte rect2)) rect1)
    (t ())))


;;; III. REDUCTIONS DE DOMAINES

;;; Reduction du domaine de rect2 lorsque rect1 a ete place et qu'ils ne
;;;  doivent pas se chevaucher

(defun retire-instant-chevauchant (instant1 duree1 nbsem1 position-rect2 duree2 nbsem2)
  (ct-restrict-domain position-rect2
    (lambda (instant2) 
      (not (chevauchement-rect-p instant1 duree1 nbsem1 instant2 duree2 nbsem2)))))

;;; Reduction du domaine de rect2 afin que tous ses instants correspondent
;;;  a la meme semaine, celle de instant1

(defun retire-instants-semaine-differente (instant1 position-rect2)
  (ct-restrict-domain position-rect2
    (lambda (x) 
      (eq (semaine-instant instant1) (semaine-instant x)))))

;;; Reduction du domaine de rect2 afin que tous ses instants correspondent
;;; a des semaines différentes que celle de l'instant1

(defun retire-instants-meme-semaine (instant1 position-rect2)
  (ct-restrict-domain position-rect2
    (lambda (x) 
      (neq (semaine-instant instant1) (semaine-instant x)))))


;;; Reduction du domaine de rect2 afin que tous ses instants correspondent
;;; a des jours differents du rect place en instant1

(defun retire-instants-meme-jour (instant1 position-rect2)
  (ct-restrict-domain position-rect2
    (lambda (x)
      (neq (jour-instant instant1) 
        (jour-instant x)))))

;;; Reduction du domaine de rect2 afin que tous ses instants correspondent
;;; au meme jour que le rect place en instant1

(defun retire-instants-jour-different (instant1 position-rect2)
  (ct-restrict-domain position-rect2
    (lambda (x)
      (eq (jour-instant instant1) 
        (jour-instant x)))))

;;; Reduction du domaine de rect2 afin que tous ses instants correspondent
;;; au meme horaire que le rect place en instant1
;;; FD - 08/06/95

(defun retire-instants-horaire-different (instant1 position-rect2)
  (ct-restrict-domain position-rect2
    (lambda (x)
      (eq 
        (horaire-instant instant1) 
        (horaire-instant x)))))

;;; IV. QUELQUES TESTS DE BASE POUR LE NOYAU

;;; Teste si un rect a deja ete place

(defun rect-place-p (rect) (ct-bound-p (position-rect rect)))


;;; Teste si deux rectangles ont une intersection non-vide
;;; dbg (03/05/94) MO: ai remplac‚ les calculs faux sur les instants par des
;;; calculs sur les semaine-instant

(defun chevauchement-rect-p 
  (instant1 duree1 nbsem1 instant2 duree2 nbsem2)
  (and
    (let
      ((semaine1 (semaine-instant instant1))
        (semaine2 (semaine-instant instant2))) 
      (and
        (lt semaine1 (add semaine2 nbsem2))
        (lt semaine2 (add semaine1 nbsem1))))

    ;; a ce moment, nous savons que les semaines se chevauchent

    (let ((moment1 (moment-instant instant1))
      (moment2 (moment-instant instant2)))
        (and
          (lt moment1 (add moment2 duree2))
          (lt moment2 (add moment1 duree1))))))


;;; Teste si deux rect doivent avoir lieu les memes semaines
;;;  ... pour l'instant, cela n'est jamais le cas

(defun egalite-semaines-obligatoire-p (rect1 rect2)
  ())


;;; V. MESURE DE LA CHARGE D'UN RECTANGLE :
;;;  nombre de rect deja instancies chevauchant un certain rectangle

(defun nb-chevauchements-rect (var instant) 
  (let* ((rect (ct-object var))
    (duree (duree-rect rect))
    (nbsem (nb-seances-rect rect))
    (lc (cdr Liste-Rect-instancies)) ; !! PR: laid
    (nb 0))
      (while lc
        (let ((rect2 (nextl lc)))
          (when
            (chevauchement-rect-p
              instant duree nbsem
              (instant-rect rect2) (duree-rect rect2) (nb-seances-rect rect2))
            (incr nb))))
      nb))


;;; VI. PROPAGATION DES DISPO DE GROUPES

;;; primitive generale de propagation
;;; en fait, il y a surement un moyen de s'en sortir autrement en gardant les 
;;; groupes d'un cours dans le cours: ainsi, on peut directement intersecter
;;; les dispo: cela demande quand meme deux lectures du fichier groupe pour
;;; que les objets existent et il faut quand meme garder la structure de 
;;; groupe telle quelle pour les contraintes car la propagation se passe dans
;;; l'autre sens
;;; il y a peut-etre moyen de faire les deux a la fois
;;; bref !! a discuter !!

(defun propagation-dispo-groupes ()
  (mapc
    (lambda (x)
      (let ((tampon (eval x)))
        (propager-dispo 
          (dispo-m-groupe tampon) 
          (rects-m-groupe tampon)
          (mapcar 'eval (groupes-m-groupe tampon)))))
    Liste-m-Groupes))


;;; primitive propageant une dispo dans une liste d'objets rect ou groupes
;;; si on connaissait la structure de l'arbre, ce serait mieux, plus optimal
;;; !! pour l'instant l'essentiel est que ca marche ? MO

(defun propager-dispo (dispo liste-rects liste-groupes-evalues)
  (when liste-rects
    (mapc (lambda (x)
      (rajouter-dispo-rect dispo x)) liste-rects))
  (when liste-groupes-evalues
    (mapc 
      (lambda (x)
        (setq  x
          (rajouter-dispo-m-groupe 
            dispo x))
        (propager-dispo 
          (dispo-m-groupe x) 
          (rects-m-groupe x)
          (mapcar 'eval (groupes-m-groupe x))))
      liste-groupes-evalues)))


;;; primitive de construction de l'instance m-groupe
;;;

;;; AJT (23/01/95) MO
;;; rajout de la caracteristique d'un m-groupe: actif, passif, pas-vu

(defun cons-m-groupe (l-contraintes l-groupes l-cours la-dispo carac nb-td)
  (if 
    (and (null l-cours) (null l-groupes))
    ()
    (list l-contraintes l-groupes  l-cours la-dispo (if carac carac 'pas-vu) nb-td)))


;;; primitive de rajout d'une dispo sur un m-groupe

(defun rajouter-dispo-m-groupe (la-dispo m-groupe)
  (list 
    (contraintes-m-groupe m-groupe) 
    (groupes-m-groupe m-groupe)
    (rects-m-groupe m-groupe) 
    (intersection-dispo la-dispo (dispo-m-groupe m-groupe))
    (carac-m-groupe m-groupe)))


;;; accesseurs de l'instance m-groupe
;;;

(defun contraintes-m-groupe (m-groupe)
  (car m-groupe))

(defun groupes-m-groupe (m-groupe)
  (cadr m-groupe))

(defun rects-m-groupe (m-groupe)
  (caddr m-groupe))

(defun dispo-m-groupe (m-groupe)
  (cadddr m-groupe))

(defun carac-m-groupe (m-groupe)
  (car (cddddr m-groupe)))

;;; CRE - 18/04/96 - FD
(defun nb-td-m-groupe (m-groupe)
  (cadr (cddddr m-groupe)))

;;; constructeur de l'instance moteur m-contrainte

(defun cons-m-contrainte (type-contrainte liste-rects)
  (cons type-contrainte liste-rects))


;;; accesseur aux differents champs de l'instance moteur m-contrainte

(defun type-m-contrainte (m-contrainte)
  (car m-contrainte))

(defun rects-m-contrainte (m-contrainte)
  (cdr m-contrainte))


;;; traiter-dispo fait l'intersection entre la dispo du rectangle et la dispo
;;; envoyee en argument et affecte cette valeur au champ dispo du rectangle
;;; si le resultat est la dispo vide alors on construit l'erreur ERRNODISP
;;; JH 04/10/94 : l'erreur est construite avec le code du rect

(defun rajouter-dispo-rect (dispo rect)
  (let
    ((la-dispo-finale (intersection-dispo dispo (dispo-rect rect))))
    (when
      (dispo-vide-p la-dispo-finale)
      (cons-erreur 'ERRNODISP (de-prefixer-code-moteur (code-rect rect))))
    (dispo-rect rect la-dispo-finale)))


;;; PAKCAGE DES CODES - MOTEUR

(defun prefixer-code-moteur (symbole)
  (symbol Nom-Package-Moteur symbole)))
               
(defun de-prefixer-code-moteur (symbole-package)
  (string2symbol (string symbole-package)))     


;;; MEMOIRE: LIBERATION DE LA PLACE PRISE INUTILEMENT 
;;;

;;; rem (25/01/95) MO
;;; il y a 4 re'cupe'rations de me'moire dans cette version du moteur
;;; a` savoir des recuperations de la place memoire prise par:

;;; 1. les rects passifs
;;; 2. les champs moements-rect semaines-rect et dispo-rects des rects actifs
;;; 3. les objets m-groupes et m-contraintes
;;; 4. toute la place utilise'e par les objets moteur


;;; 1. liberation de la place memoire prise par les rects passifs

(defun liberer-memoire-rects-passifs ()
  (mapc (lambda (s) (setq s ())) (mapcar 'code-rect liste-rects-passifs))
  (setq liste-rects-passifs ())
  (gc))


;;; 2. liberation de la place prise en memoire par les champs moments-rect, 
;;;    semaines-rect et dispo-rect d'un rect actif
;;;
;;; la primitive vider-rect est appele'e explicitement dans initialiser-rect pour
;;; chaque rect actif

(defun vider-rect (rect)
  (moments-rect rect ())
  (dispo-rect rect ())
  (semaines-rect rect ()))     


;;; 3. primitive liberant la place inutilement utilisee pendant la resolution

(defun liberer-memoire-m-objets ()
  (mapc 'remob liste-m-groupes)
  (setq liste-m-groupes ())
  (setq liste-m-contraintes ())
  (gc))


;;; 4. on libere toute la place prise par le moteur, place memoire solver comprise


(defun liberer-memoire-moteur ()
  (mapc (lambda (rect) (remob (code-rect rect))) liste-rect)
  (setq liste-rect ()))

 
(defun donner-valeur (rect instant)
  (ct-set-value (position-rect rect) instant))



;;; Rects Actifs/passifs
;;;
;;; (23/01/95) MO PR
;;;
;;; un rect actif est un rect 
;;;     soit a resoudre, soit fige (r ou f)
;;; un rect passif etant un rect non fige non a-resoudre (nf et nr)
  

(defun rect-actif-p (r)
  (or
    (fige-rect r)
    (a-resoudre-rect r)))


(defun rect-passif-p (r)
  (not (rect-actif-p r)))

    
;;; contrainte-a-poser-p 

;;; condition devant etre verifiee pour pouvoir poser une contrainte
;;; table de verite de ce predicat
;;;
;;;            x       f/r   f/nr   nf/r   nf/nr
;;;       y   
;;;    
;;;      f/r           ()     ()     t      ()         f   fige  
;;;      f/nr          ()     ()     t      ()        nf   non fige 
;;;     nf/r           t      t      t      ()         r   a resoudre
;;;     nf/nr          ()     ()     ()     ()        nr   ne pas resoudre
;;;
;;; ceci est equivalent a:
;;;     (actif-rect x) et (actif-rect y) et (non-fige x ou non fige y)

(defun contrainte-a-poser-p (x y)
  (and 
    (not (eq x y))
    (rect-actif-p x)
    (rect-actif-p y)
    (not 
      (and 
        (fige-rect x)    ; -> on peut faire un booleen binaire 
        (fige-rect y)))
    t))


;;; primitive qui renvoie la liste des rects actifs

(defun initialiser-listes-rects ()
    (print "nb de rect: " (length liste-rect))
    (setq liste-rects-actifs (sous-liste rect-actif-p liste-rect))
    (setq liste-rects-passifs (sous-liste rect-passif-p liste-rect))
    (setq liste-rect ())
    (print "nb de rects actifs " (length liste-rects-actifs))
    (print "nb de rects passifs " (length liste-rects-passifs)))


 
;;; <initialiser-rects-a-resoudre>
;;; primitive qui met a t les champs des cours a resoudre
;;; cela sert pour la resolution partielle
;;; rem: liste est une liste de symboles interface 

(defun initialiser-rects-a-resoudre (liste)
  (if 
    liste
    (mapc
      (lambda (c)
	(a-resoudre-rect (eval (prefixer-code-moteur c)) t))
      liste)
    (mapc
      (lambda (c)
	(a-resoudre-rect c t))
      liste-rect)))



;;; <propagation-param-chevauchement>
;;; fixe le paramtre de chevauchement pour tous les rects en fonction
;;; des contraintes qui pesent sur les groupes

(defun propagation-param-chevauchement ()
  (mapc
    (lambda (gr)
      (let
	((tampon (eval gr)))
	(when (param-chevauchement-vaut-1-p tampon)
	  (propager-param-chevauchement-1
	    (rects-m-groupe tampon)
	    (mapcar 'eval (groupes-m-groupe tampon))))))
    Liste-m-Groupes))

;;; <propager-param-chevauchement-1>
;;; propage a tous les rects appartenant a liste-rects et liste-groupes
;;; un parametre de chevauchement egal a 1

(defun propager-param-chevauchement-1 (liste-rects liste-groupes)
  (when liste-rects
    (mapc
      (lambda (x) (param-chevauchement-max-rect x 1)) liste-rects))
  (when liste-groupes
    (mapc 
      (lambda (x)
	(propager-param-chevauchement-1
	  (rects-m-groupe x)
	  (mapcar 'eval (groupes-m-groupe x))))
      liste-groupes)))

;;; <param-chevauchement-vaut-1-p>
;;; predicat permettant de savoir si pour les cours inclus dans cette famille
;;; nous devaons utiliser 1 comme parametre de chevauchement max
;;; quand nous avons une contrainte de k-ecart-simultane le parametre de
;;; chevauchement max doit etre egal a 1

(defun param-chevauchement-vaut-1-p (groupe)
  (appartenir-liste
    'poser-k-ecart-simultaneite
    (contraintes-m-groupe groupe))))

;;; <type-cours-rect>
;;; accesseur au type de cours d'un rectangle
;;; 

(defun type-cours-rect (rect . val)
  (when val
    (type-rect
      rect
      (cons-type-rect val (param-chevauchement-type-rect rect))))
  (type-cours-type-rect (type-rect rect)))

;;; <param-chevauchement-max-rect>
;;; accesseur au parametre de chevauchement max associe a un rectangle
;;; si type-rect est superieur a PARAM-CHEVAUCHEMENT-1, alors le parametre de
;;; chevauchement max qui sera utilise sera 1
;;; sinon on prend Param-Chevauchement-Max

(defun param-chevauchement-max-rect (rect . val)
  (let ((type-rect (type-rect rect)))
    ;; affectation de la valeur de parametre de chevauchement max pour rect
    (when val
      (selectq (car val)
	(1
	  (when (not (eq (param-chevauchement-max-rect rect) 1))
	    (type-rect rect (cons-type-rect (type-cours-type-rect type-rect) 1))))
	(t
	  ;; dans les autres cas on prendra la valeur par defaut du param de
	  ;; chevauchement max (globale: Param-Chevauchement-Max)
	  (param-chevauchement-max-i-ecole i-ecole))))
    ;; on retourne le parametre de chevauchement max
    (if (eq (param-chevauchement-type-rect (type-rect rect)) 1)
      1
      (param-chevauchement-max-i-ecole i-ecole))))

;;; fonctions de manipulation du champ type-rect d'un rect
;;; ATTENTION 
;;; le champ type-rect regroupe deux info : le type de cours et le parametre de
;;; chevauchement max a utilise pour le rect
;;; type-rect est construit de la maiere suivante:
;;;  type-rect = param-chevauchement-max * PARAM-CHEVAUCHEMENT-1 + type-cours

(defun cons-type-rect (type-cours param-chevauchement)
  (add
    (mul param-chevauchement PARAM-CHEVAUCHEMENT-1)
    type-cours))

(defun type-cours-type-rect (type-rect)
  (rem type-rect PARAM-CHEVAUCHEMENT-1))

(defun param-chevauchement-type-rect (type-rect)
  (div type-rect PARAM-CHEVAUCHEMENT-1))

;;; CRE RM : 06/12/96

(defun dispo-avec-inter-cours (dispo inter-cours)
  (ifn (dispop dispo)
    ()
    (cons
      (mapcar
	(lambda (element)
	  (cons (car element) (+ (cdr element) inter-cours)))
	(car dispo))
      (cdr dispo))))

;;; CRE RM : 10/06/97
;;; On retrouve un type de cours a partir d'un rectangle

(defun rechercher-type-cours (code)
  (let
    ((lst Liste-Rect)
     (r))
    (setq r (car lst))
    (until (and lst (not (equal code (code-rect r))))
      (setq r (nextl lst)))
    (type-rect r)))

(defun construire-dispo-rect (instant duree)
  (let
    ((moment
      (moment-instant-cumule (instant-interne2instant-cumule instant)))
     (semaine
      (semaine-instant-cumule (instant-interne2instant-cumule instant))))
    (cons-dispo
      (cons (cons moment (+ moment duree)) ())
      (cons (cons semaine (+ semaine 1)) ()))))

;;; CRE RM 11/06/97
;;; Fonction qui retourne la liste des ajouts de seances d'un cours

(defun liste-ajouts-cours (cours)
  (let
    ((lst ()))
    (mapc
      (lambda (pair)
	(when (eq cours (car pair))
	  (newl lst (cdr pair))))
      Liste-M-Cours-Ajouts)
    lst))

;;; Fonction qui retourne la liste des ajouts de seances d'un prof

(defun liste-ajouts-prof (prof)
  (let
    ((lst ()))
    (mapc
      (lambda (pair)
	(when (eq prof (car pair))
	  (newl lst (cdr pair))))
      Liste-M-Prof-Ajouts)
    lst))

;;; Fonction qui retourne la liste des rects d'un prof
;;; Cours + Ajouts si RESOLUTION-CONTRAINTES-MAX = t
;;; Cours sinon

(defun liste-rects-cours (lst-cours)
  (if (not RESOLUTION-CONTRAINTES-MAX)
    (mapcar 'prefixer-code-moteur lst-cours)
    (let
      ((lst (mapcar 'prefixer-code-moteur lst-cours))
       (lst-ajouts ()))
      (mapc
	(lambda (c)
	  (setq lst-ajouts (liste-ajouts-cours (prefixer-code-moteur c)))
	  (when lst-ajouts
	    (setq lst (append lst lst-ajouts))))
	lst-cours)
      lst)))
