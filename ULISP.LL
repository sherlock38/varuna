;;; --------------------------------------------------------------------------
;;; - Ulisp.ll                                                  VARUNA 1.2   -
;;; - Utilitaires                                                            -
;;; - utilitaires de Lisp purs, independants de Varuna                       -
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; --------------------------------------------------------------------------
;;; - Creation:   1993 PR                                                    -
;;; - Derniere mise a jour pour version 1.13: Septembre 94 MO                -
;;; - Build 1.22.1 01.07.99 / JH                                             -
;;; --------------------------------------------------------------------------

;;; rem chg (09/05/94) MO: 
;;; modification des outils sur les zones -> listes triees

;;; rem chg (01/09/94) MO:
;;; j'ai rajoute dans iterer-champs-fichier un () pour que cela rende bien ()



;;; FONCTIONS
;;;

;;; fonction toujours vraie
(defun vrai l t)


;;; OPERATIONS SUR LES LISTES
;;;

;;; <maximum> determine le maximum des elements d'une liste

(dmd maximum (f l)
  `(let ((m 0) (liste ,l))
    (while liste
      (setq m (max m (,f (nextl liste)))))
    m))

;;; macro generale d'iteration

(dmd iterer-liste (x laliste . body)
  `(let ((ll ,laliste))
    (while ll
      (let ((,x (nextl ll))) . ,body))))

;;; liste des f(x,y), oó x et y parcourent deux listes

(dmd mapcar2 (f l1 l2)
  `(let ((l3 ()))
    (iterer-liste x1 ,l1
      (iterer-liste x2 ,l2
        (newl l3 (,f x1 x2))))
    (nreverse l3)))

;;; recherche du premier element d'une liste satisfaisant une condition

(dmd premier (bool l . lpar)
  `(let ((liste ,l) (continue t) (result ()))
    (while (and liste continue)
      (let ((x (nextl liste)))
        (when
          (funcall ,bool x . ,lpar)
          (setq result x continue nil))))
    result))

;;; sous-liste des elements d'une liste verifiant une condition

(dmd sous-liste (f l . lpar)
  `(let ((sl ()) (liste ,l))
    (while liste
      (let ((x (nextl liste)))
        (when (,f x . ,lpar) (setq sl (cons x sl)))))
    (nreverse sl)))

;;; liste de nombres entre n1 et n2, avec une longueur de base de n
;;;  (longueur du cours), et un paramätre d'accroissement de alpha

(defun liste-nombres-inclus (n1 n2 n . alpha)
  (cond ((<= (car alpha) 0)
    ())
      ((< (1+ (- n2 n1)) n)
        ())
      ((= (1+ (- n2 n1)) n) 
        (list n1))
      (t 
        (let*
          ((m (1- n1))
            (p (- n2 m))          ; nb d'entiers entre n1 et n2 compris
            (imax (- p (1- n)))   ; quantite maxi de nombres inclus
            (param (if alpha (car alpha) 1))
            (a (* param (1+ (div (1- imax) n))))) ; en gros, le nombre d'Çlts de la liste
          (cond
            ((= a 1) (list n1 (- n2 (1- n))))
            (t
              (cons n1
                (liste-nombres-iter
                  imax 1 (1- (min a imax)) m))))))))

;;; liste de nombres, sans compter m+1, jusqu'Ö m+imax

(defun liste-nombres-iter (imax k n m)
  (if
    (= k n)
    (list (+ m imax))
    (cons
      (+ m (div-sup (* k imax) n))
      (liste-nombres-iter imax (1+ k) n m))))

;;; teste si deux listes ont des elements communs
;;; jerome : member->appartenir-liste (a cause de la pile)

(defun inter-non-vide-p (l1 l2)
  (cond ((null l1) ())
    ((appartenir-liste (car l1) l2) t)
    (t (inter-non-vide-p (cdr l1) l2))))

;;; donne la liste des elements en commun de deux listes

(defun elements-communs (liste1 liste2)
  (if 
    (or (null liste1) (null liste2)) 
    ()
    (letn 
      boucle
      ((liste-travail liste1) (liste-resultat ()))
      (cond
        ((null  liste-travail) (reverse liste-resultat))
        ((appartenir-liste (car liste-travail) liste2)
          (boucle 
            (cdr liste-travail) 
            (cons (car liste-travail) liste-resultat)))
        (t (boucle (cdr liste-travail) liste-resultat)))))) 

;;; teste si un objet appartient a une liste d'objet (member ?)

(defun appartenir-liste (objet liste)
  (cond ((null liste) ())
    ((eq objet (car liste)) objet)
    (t (appartenir-liste objet (cdr liste)))))

;;; FD 11/04/95
;;; fct identique a la precedente sf qu'elle retourne le reste de 
;;; la liste

(defun appartenir-liste2 (objet liste)
  (cond 
    ((null liste) ())
    ((eq objet (car liste)) liste)
    (t (appartenir-liste2 objet (cdr liste)))))

;;;  CRE (23/05/95) MO
;;;  <mono-element-p> teste si une liste a un cdr nul

(defun mono-element-p (liste)
  (and (consp liste) (null (cdr liste))))


;; Renvoie l'ordre d'un element dans une liste

; vieille version 
;(defun numero-dans-liste (element liste)
;       (sub (length liste) (length (member element liste))))

; (02/05/94) MO: redefinie
(defun numero-dans-liste (element liste)
  (letn 
    boucle
    ((l liste) (compteur 0))
    (cond ((null l) ())
      ((eq (car l) element) compteur)
      (t (boucle (cdr l) (add1 compteur))))))


;; Plus-Proche-Index : Renvoie l'ordre de l'entier d'une liste ordonnee le plus plus proche de 
;;                       l'element (entier) donne en argument

(defun plus-proche-index (nombre-entier liste-entiers)
  (let ((element (tag trouve
    (mapc (lambda (v)
      (when (le nombre-entier v)
        (exit trouve v)))
          liste-entiers))))
            (numero-dans-liste element liste-entiers)))


;; Extract supprime les doublons dans une liste triee
;;  precedent et temp, sont des variables temporaires.

(defun extract (liste precedent temp)
  (if (null liste)
    temp
    (if (equal (car liste) precedent)
      (extract (cdr liste) (car liste) temp)
      (extract (cdr liste) (car liste) (cons (car liste) temp)))))


;;; Renvoie la taille maximale d'une liste de chaine de caractere
;;; on s'en sert dans les editeurs

(defun max-string-len (liste)
  (let ((maxs 0))
    (mapc (lambda (chaine)
      (let ((taille (slength chaine)))
        (when (gt taille maxs)
          (setq maxs (slength chaine)))))
            liste)
    maxs))


;; (sommer liste) : fait la somme des elements de la liste

(defun somme-partielle (item liste)
  (let ((temp 0))
    (mapc (lambda (x) (setq temp (add temp x)))
      (firstn item liste))
    temp))

(defun sommer (liste)
  (somme-partielle (length liste) liste))

;; (max-string-width (liste-chaine)) : calcule
;; la largeur maximale des chaines de la liste (pour la legende)

(defun max-string-width (liste-chaine)
  (let ((le-max 0))
    (mapc (lambda (chaine) (setq le-max (max 0 (slength chaine))))
      liste-chaine)
    le-max))

;; applique f(x,y) a chaque couple
(defun mapcouple (func l1 l2)
  (if (not l1) ()
    (funcall func (car l1) (car l2))
    (mapcouple func (cdr l1) (cdr l2))))

;; renvoie le premier et le dernier element d'une liste

(defun premier-element (liste)
  (car liste))

(defun dernier-element (liste)
  (car (last liste)))


(defun supprimer-occurences-vides (liste)
  (let
    ((premier (car liste))
      (reste (cdr liste)))
    (cond ((null liste) ())
      ((null premier) (supprimer-occurences-vides reste))
      (t (cons premier (supprimer-occurences-vides reste)))))) 


;;; rem: il n'enleve pas les () multiples

(defun supprimer-occurences-multiples (liste)
  (if (null liste)
    ()
    (if (appartenir-liste (car liste) (cdr liste))
      (supprimer-occurences-multiples (cdr liste))
      (cons 
        (car liste)
        (supprimer-occurences-multiples (cdr liste))))))

;;; idem sur les chaine: utilise un member soit un equal 

(defun supprimer-chaines-multiples (liste)
  (if (null liste)
    ()
    (if (member (car liste) (cdr liste))
      (supprimer-chaines-multiples (cdr liste))
      (cons 
        (car liste)
        (supprimer-chaines-multiples (cdr liste))))))

(defun supprimer-occurence (objet liste)
  (if (null liste)
    ()
    (if (eq (car liste) objet)
      (supprimer-occurence objet (cdr liste))
      (cons
        (car liste)
        (supprimer-occurence objet (cdr liste))))))

;;; evlis1 rend la liste des elements evalues d'une liste en enlevant les elements
;;; non evluables 

(defun evlis1 (liste)
  (let ((liste-travail ()))
    (mapc
      (lambda (code)
        (and (boundp code) (newl liste-travail (eval code))))
      liste)
    (reverse liste-travail)))
        
;;; OPERATIONS SUR LES NOMBRES
;;;

;; Division entiere en prenant le sup

(defun div-sup (x y)
  (if
    (zerop (modulo x y))
    (div x y)
    (1+ (div x y))))


;;; OPERATIONS SUR LES CHAINES
;;;

;; Passage chaine a symbole

(defun string2symbol (ch)
  (with-input-from-string ch (read)))


;; Passage chaine a nombre (c'est pareil ...)

(defun string2number (ch)
  (with-input-from-string ch (read)))


;;; <minuscule> transforme la chaine en minuscule
;;;             renvoie nil si l'argument n'est pas une chaine  

(defun minuscule (chaine)
  (if (stringp chaine)
    (if (eqstring chaine "")
      chaine
      (catenate 
        (ascii (lowercase (sref chaine 0)))
        (minuscule (substring chaine 1))))))

;;; <avec-guillemet-p>
;;; CRE - 29/02/96 - FD
;;; verifie q'une chaine ne comprend pas de guillemet

(defun avec-guillemet-p (chaine)
  (chrpos (cascii GUILLEMET) chaine))

;;; <separer-chaine>
;;; creer une liste de chaines de longueur max <longueur-max> a partir de la
;;; chaine <chaine>

(defun separer-chaine (chaine longueur-max)
  (couper-chaine chaine longueur-max 0 0))

;;; <couper-chaine> - 07/05/96 - FD

(defun couper-chaine (chaine longueur-max pos-courante pos-suivante)
  (let*
    ((blanc " ")
     (index-courant) 
     (index-suivant)
     (chaine-a-traitee))

    (cond
      ((and
	(numberp pos-suivante)
	(= pos-courante 0)
	(> pos-suivante longueur-max))
      ;; cas ou le mot est plus long que la longueur max
      (setq
	chaine-a-traitee
	(substring chaine (+ pos-suivante 1) (slength chaine)))
      (cons
	(substring chaine pos-courante longueur-max)
	(couper-chaine
	  chaine-a-traitee
	  longueur-max
	  0
	  0)))
      
      ((and (numberp pos-suivante)(> pos-suivante longueur-max))
	(setq
	  chaine-a-traitee
	  (substring chaine (+ pos-courante 1) (slength chaine)))
	(cons
	  (substring chaine 0 pos-courante)
	  (couper-chaine
	    chaine-a-traitee
	    longueur-max
	    0
	    0)))
      ((not (numberp pos-suivante)) 	;; on a atteind la fin de la chaine
	(if (> (slength chaine) longueur-max)
	  (list
	    (substring chaine 0 (+ pos-courante 1))
	    (substring chaine (+ pos-courante 1) (slength chaine)))
	  (list chaine)))
      (t
	(setq index-courant pos-suivante)
	(setq index-suivant (index blanc chaine (+ pos-suivante 1)))
	(couper-chaine chaine longueur-max index-courant index-suivant)))))

;;; OPERATIONS SUR LES FICHIERS
;;;

;; On itere la meme operation ligne a ligne sur tous les champs d'un fichier
;;  apres l'avoir ouvert, et jusqu'a la fin de ce fichier
;;; ??? rem FD MO (26/06/95)
;;; on a essaye de faire un (close (inchan)) sans succes - pourquoi ???


(dmd iterer-champs-fichier (fichier lchamps . body)
  `(when
    (probefile ,fichier)
    (with
      ((inchan (openi ,fichier)))
      (untilexit EOF
	(let* ,(mapcar (lambda (x) (list x '(read))) `,lchamps)
	  . ,body)))
    ()))


;;; MACRO DE DEFINITION DES CONCEPTS
;;;

;;; On a une macro pour la definition des concepts
;;; On definit la variable globale liste-xxx, la classe, 
;;;  la fonction de creation et les accesseurs, AUTOMATIQUEMENT.

(dmd defconcept (nom . l)
  `(progn
  (defvar ,(concat 'liste- nom) ())
  (defctclass ,nom . ,(mapcar 'faire-champ l))
  (defun ,(concat 'creer- nom) ,(mapcar 'car (filtrer l))
    (let ((y (ct-make-instance ,nom . 
      ,(mapcan (lambda (x) (list (car x) (car x))) (filtrer l)))))
        (set code y)
        (newl ,(concat 'liste- nom) y)))
  . ,(mapcar
  (lambda (x) 
    (list 'ct-make-accessor nom (car x) (concat (car x) '- nom))) l)))

;; Utilitaires pour la macro precedente

(defun filtrer (l) (sous-liste cadr l))

(defun faire-champ (sl)
  (if (cddr sl) (cons (car sl) (cddr sl)) (car sl)))

(defun faire-champ2 (sl)
  (if (cddr sl)
    (cons (car sl) (cddr sl))
    (car sl)))

(defun filtrer-champ2 (l) (sous-liste cadr l))



(dmd defsousaccesseurs (classe sous-classe . l-champs)
  `(progn
  .,(mapcar (lambda (champ)
    `(defun ,(concat champ '- classe) (ob . valeur)
      (let ((s-class (,(concat sous-classe '- classe) ob)))
        (ifn valeur
          (and s-class (,(concat champ '- sous-classe) s-class))
          (and s-class (,(concat champ '- sous-classe) s-class (car valeur)))))))
            l-champs)))


;;; Utilitaires pour trouver des successions dans des listes

(defun precede-immediatement (a b l)
  (eq (length (memq a l)) (add1 (length (memq b l)))))

(defun se-succedent (l1 l2)
  (if (cdr l1)
    (if (precede-immediatement (car l1) (cadr l1) l2)
      (se-succedent (cdr l1) l2)
      ())
    t))

(defun premiere-succession (l1 l2)
  (if (se-succedent l1 l2)
    l1
    (premiere-succession (firstn (sub1 (length l1)) l1) l2)))


(defun successions (l1 l2)
  (when l1
    (let ((suc (premiere-succession l1 l2)))
      (cons
        suc
        (successions (nthcdr (length suc) l1) l2)))))

;; macro permetant de supprimer un objet dans le champ d'un autre objet

(dmd supprimer-objet (objet liste accesseur)
  `(mapc (lambda (o)
    (when (memq ,objet (,accesseur o))
      (,accesseur o (delq ,objet (,accesseur o)))))
        ,liste))


(de allcar (e)
  (if (null e) () (cons (caar e) (allcar (cdr e)))))

(de allcdr (e)
  (if (null e) () (cons (cdar e) (allcdr (cdr e)))))

(defun map-n-listes (f . l)
  (when (any 'consp l)
    (apply f (allcar l))
    (apply 'map-n-listes f (allcdr l))))




;;; DEFINITION ET ACCESSEURS SUR LISTE DE PAIRES
;;;


;;; a. predicats et accesseurs de base

(defun liste-paires-vide-p (liste-paire)
  (null liste-paire))

(defun liste-paire-p (objet)
  (and
    (paire-p (car objet))
    (liste-paire-p (cdr objet))))

(defun paire-p (objet)
  (or (null objet)
    (and (consp objet)
      (not (null (cdr objet))))))

(defun premiere-paire (liste-paire)
  (car liste-paire))

(defun liste-sans-premiere-paire (liste-paire)
  (cdr liste-paire))

(defun tete-paire (paire)
  (car paire))

(defun queue-paire (paire)
  (cadr paire))

(defun premiere-tete (liste-paire)
  (tete-paire (premiere-paire liste-paire)))

(defun premiere-queue (liste-paire)
  (queue-paire (premiere-paire liste-paire)))

(defun n-paire (liste-paire n)
  (nth n liste-paire))


;;; 1. Quelques complements sur les a-listes (a un seul niveau)
;;;    avec gestion de l'unicite des clefs

;;; <modifier-aliste>
;;; renvoie une a-liste dotee de la paire clef-valeur si celle-ci
;;; n'existait pas, ou remplace la valeur par la nouvelle si la clef 
;;; existe <remplacer-aliste> ne fait que remplacer la valeur

(defun modifier-aliste (aliste clef valeur)
       (acons clef valeur (enlever-element aliste clef)))

(dmd modifier-aliste-d (aliste clef valeur)
     `(setq ,aliste (modifier-aliste ,aliste ,clef ,valeur)))

;;; <remplacer-aliste>
;;; renvoie une aliste dans laquelle la valeur de la paire clef-valeur
;;; a ete remplacee par la nlle <valeur> passee en parametre
;;;  ne cree pas de paire clef-valeur si elle n'existait pas

(defun remplacer-aliste (aliste clef valeur)
       (if (null (assq clef aliste))
           aliste
           (acons clef valeur (enlever-element aliste clef))))       

(dmd remplacer-aliste-d (aliste clef valeur)
     `(setq ,aliste (remplacer-aliste ,aliste ,clef ,valeur)))

;;; <enlever-element>
;;; renvoie une aliste privee de la paire clef-valeur 

(defun enlever-element (aliste clef)
       (if (null aliste)
           aliste
           (if (eq (caar aliste) clef)
               (cdr aliste)
               (cons (car aliste) (enlever-element (cdr aliste) clef)))))

(dmd enlever-element-d (aliste clef)
     `(setq ,aliste (enlever-element ,aliste ,clef)))

;;; <liste-clefs>
;;; retourne la liste des clefs d'une aliste de (clef.valeur)

(defun liste-clefs (aliste)
       (if (null aliste)
           ()
           (cons (tete-paire (premiere-paire aliste)) 
                 (liste-clefs (liste-sans-premiere-paire aliste)))))

;;; <incrementer-aliste>
;;; incremente la valeur associee a une clef de <incr> (incr = 1 par defaut)
;;; si la clef n'est pas presente dans la aliste on l'ajoute avec la valeur
;;; <incr>

(defun incrementer-aliste (aliste clef . incr)
  (let ((valeur (if incr 
                  (car incr)
                  1)))
    (if (null (assq clef aliste))
      (acons clef valeur aliste)
      (acons 
          clef 
          (+ (cassq clef aliste) valeur)
          (enlever-element aliste clef)))))

(dmd incrementer-aliste-d (aliste clef . incr)
  `(setq ,aliste 
         (if ,(null incr)
           (incrementer-aliste ,aliste ,clef)
           (incrementer-aliste ,aliste ,clef ,(car incr)))))

