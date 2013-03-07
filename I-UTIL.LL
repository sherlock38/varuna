;;; ------------------------------------------------------------------------
;;; - I-util.ll                                               VARUNA 1.2   -
;;; - Copyright Abstract, France 1993-2006                                 -
;;; - under licence MIT Expat since 2006                                   -
;;; - Interface                                                            -
;;; - Module d'utilitaires de l'interface                                  -
;;; - Aida & Lisp                                                          -
;;; ------------------------------------------------------------------------
;;; - Creation:  Mars 1993                                    JH           -
;;; - Mise a jour pour version 1.13: Juin 1994 MO                          -
;;; - Build 1.22.1 01.07.99 / JH MO                                        -
;;; ------------------------------------------------------------------------

;;; Plan du fichier
;;;
;;; 0. Les variables globales de ce fichier 
;;; 1. Operations sur les chaines
;;; 2. Les procedures d'alerte
;;; 3. Objets Aida
;;; 4. Utilitaires pour les editeurs
;;; 5. Resources Aida
;;; 6. Macros de definition des concepts de l'interface


;;; 0. Les variables globales de ce fichier d'utilitaire

;;; les variables de polices

(defvar time5)
(defvar time5b)
(defvar time6b)
(defvar time8)
(defvar time8b)
(defvar time10)
(defvar time10b)
(defvar arial10)
(defvar arial10b)
(defvar arial12b)
(defvar arial8)
(defvar arial8b)
;;; AJT FD - RM 23/05/96
(defvar courier8)
(defvar courier12)
(defvar courier12b)
(defvar liste-couleurs)
(defvar liste-couleur-varuna)

;;icones et leurs fichiers

(defvar Icon-varuna) 
(defvar Icon-ungrow)
(defvar Icon-grow)
(defvar Icon-size) 
(defvar Icon-scale)
;; AJT - FD - 03/07/96
(defvar Icon-Suivant)
(defvar Icon-Precede)
;; AJT - RM 05/05/97
(defvar Icon-Exclamation)
(defvar Icon-Question)
(defvar Icon-Information)
(defvar Icon-Stop)

;;; 19/09/95
;;; les curseurs
(defvar Varuna-Curseur-Fleche)
(defvar Varuna-Curseur-Sablier)

(defvar DISPO-PAT)


;;; 1. les operations sur les chaines:
;;;      <nullstrp>       
;;;      ->  <chaine-vide-p> CRE (09/06/95) MO  
;;;      <est-un-symbole>
;;;      <sans-espace>
;;;      ->  <chaine-avec-blanc-p> CRE (09/06/95) MO 
;;;      <sans-nombre-en-premier >
;;;      <est-un-symbole>
;;;      <ajouter-espaces>
;;;      <cadrer-centrer>
;;;      <couper-chaine>
;;;      <chaine2image>
;;;      <imprimer-liste-chaine>
;;;      <taille-chaine-image>
;;;      <dernier-espace>
;;;      <espace-apres>
;;;      <split-string1>
;;;      <split-string>
;;;      <avec-lettre-en-premier>

;;; teste si une chaine est vide

(defun nullstrp (string)
  (equal (slen string) 0))

;;; rem (07/06/95) MO !!!
;;; pourquoi de l'anglais -> on pense que c'est une fonction standard lisp
;;; ce n'est pas un booleen en sortie -> alors pourquoi un p ????
;;; d'ou la primitive suivante:

(defun chaine-vide-p (chaine)
  (and
    (stringp chaine)
    (eq (slen chaine) 0)
    t))

;;; verifie que la chaine passe en parametre ne comporte pas d'espace

(defun sans-espace (chaine)
  (and
    (stringp chaine)
    (let ((esp (index " " chaine)))
      (if esp () t))))

;;; rem (08/06/95) MO
;;; ceci est un booleen... et la norme d'ecriture... ed plus, il y a mieux
;;; que de regarder la presence d'une chaine " " dans la chaine de depart
;;; nom contestable espace <> blanc

(defun chaine-avec-blanc-p (objet)
  (and 
    (stringp objet)
    (any 
      (lambda (code-interne) (eq code-interne 32))
      (pname objet))
    t))  


;;; verifie que chaine est une chaine et ne commence pas par un nombre

(defun sans-nombre-en-premier (chaine)
  (cond 
    ((not (stringp chaine)) ())
    ((nullstrp chaine) t)
    (t (not (digitp (sref chaine 0))))))

;;; CRE - FD - 21/07/95
;;; <caracteres-autorises-p>
;;; verifie qu'une chaine de caracteres n'est composee que de 
;;; lettres et chiffres et de caracteres autorises 
;;; (Liste-Caracteres-Autorises)

(defun caracteres-autorises-p (chaine)
  (and
    (stringp chaine)
    (every
      (lambda (c)
        (or 
          (chrpos c Liste-Caracteres-Autorises)
          (letterp c)
          (digitp c)))
      (pname chaine))))

;;; CRE - FD - 25/07/95
;;; <avec-lettre-en-premier>
;;; verifie qu'une chaine de caracteres commence par une lettre
;;; 
(defun avec-lettre-en-premier-p (chaine)
  (and
    (stringp chaine)
    (not (nullstrp chaine))
    (letterp (sref chaine 0))))

;;; verifie que la chaine passee en parametre est un symbole
;;; <au sens de l'interface, un symbole ne comporte pas de blancs
;;; et ne doit pas commencer par un chiffre>
;;; FD - 25/07/95
;;; avant le 25/07/95
; (defun est-un-symbole (chaine)
;   (if 
;     chaine
;     (and 
;         (sans-nombre-en-premier chaine)
;         (sans-espace chaine)
;     ()))

(defun est-un-symbole (chaine)
  (and
    chaine
    (sans-nombre-en-premier chaine)
    (sans-espace chaine)
    (avec-lettre-en-premier-p chaine)
    (caracteres-autorises-p chaine)))

;;; ajoute des espace a une chaine

(defun ajouter-espace (chaine avant apres)
  (let ((result chaine))
    (when (gt apres 0)
      (repeat apres
        (setq result (catenate result " "))))
    (when (gt avant 0)
      (repeat avant
        (setq result (catenate " " result))))
    result))

;;; centre une chaine sur n-c caracteres

(defun cadrer-centrer (chaine nombre-cadre)
  (let* 
    ((n-esp (max 0 (- nombre-cadre (slength chaine))))
     (espace-avant (div n-esp 2))
     (espace-apres (div-sup n-esp 2)))
    (ajouter-espace chaine espace-avant espace-apres)))

;;; coupe une chaine en plusieurs lignes (pour les alertes)
;;; selon la taille du display en cours
;;; renvoie une image aida

(defun couper-chaine (chaine taille)
  (let 
    ((nblig (div (send 'width chaine) taille)))
    (if (le nblig 1)
      chaine
      (let* ((posespace
                 (or
                     (index ESP chaine (div taille #wchar))
                     (slength chaine)))
             (car-chaine (substring chaine 0 posespace))
             (cdr-chaine (substring chaine (add1 posespace) (slength chaine))))
        (column
            car-chaine
            (couper-chaine cdr-chaine taille))))))

;;; renvoie une image de la chaine chaine cadree sur un tier de l'ecran

(defun chaine2image (chaine)
  (couper-chaine chaine (div (bitxmax) 3)))

;;; imprime sur un le tty courrant les elements de la liste
;;; avec sauts de ligne a chaque element

; (defun imprimer-liste-chaines (liste)  
;   (if (null liste)
;     (print "")
;     (print (car liste))
;     (imprimer-liste-chaines (cdr liste))))

(defun imprimer-liste-chaines (liste-chaines)
  (mapc
    (lambda (str)
    (when (not (chaine-vide-p str))
      (print str)))
    liste-chaines))

(defun imprimer-liste-chaines-avec-vide (liste-chaines)
  (mapc 'print liste-chaines))

;;; renvoie la taille en pixel d'une chaine de caractere
;;; (plus precis que #wchar * taille-chaine car #wchar est
;;; la taille moyenne d'un caractere. Cette fonction renvoie
;;; la taille exacte.

(defun taille-chaine-image (chaine)
  (let ((taille 0))
    (for (i 0 1 (sub1 (slength chaine)))
      (setq taille
        (add taille (send 'width (string (ascii (sref chaine i)))))))
    taille))

;;; renvoie le rang de dernier espace (en partant de la gauche) 
;;; ou le premier espace (en partant de la droite)
;;; d'un chaine de caractere

(defun dernier-espace (chaine)
  (let ((pos-espace1 ()) (pos-espace2 ()) (pos 0))
    (while (setq  pos-espace1 (index ESP chaine pos))
      (setq pos-espace2 pos-espace1)
      (incr pos))
    pos-espace2))

;;; renvoie la sous chaine de chaine a partir
;;; du premier espace (recherche a partir de position) 
;;; trouve dans la chaine chaine. Si la chaine ne comporte pas
;;; d'espace, renvoie la chaine entiere.

(defun espace-apres (chaine position)
  (let ((pos-esp (index ESP chaine position)))
    (if pos-esp
      (substring chaine 0 pos-esp)
      chaine)))

;;; split-string1
;;; renvoie la sous chaine de the-string comptee a partir du debut de longueur width
;;; coupee apres un mot
;;; dbg (17/12/94) MO
;;; cette primitive renvoyait un seul mot, ce n'est plus le cas
;;; rem (17/12/94) MO
;;; je n'aime pas le nom car:
;;;     il veut pas dire grand chose: suggestion ?
;;;     le chiffre un "1" prete a confusion avec la lettre "l" tres suggestive en lisp
 

(defun split-string1 (the-string width)
  (let ((index 0)
    (buffer "")
    (fin ())
    (string-len (slength the-string)))
      (while
        (and
          (lt index string-len)
          (le (taille-chaine-image buffer) width))
        (setq buffer (catenate buffer (ascii (sref the-string index))))
        (incr index))
      (if (eq index string-len)
        buffer
        (if (setq fin (dernier-espace (substring buffer 1)))
          (substring buffer 0 (add1 fin))
          (espace-apres the-string (slength buffer))))))
        
;;; renvoie la liste de chaines affichables...

(defun split-string (the-string nlig width)
  (let ((liste-chaine ())
    (buffer ""))
      (while (and (lt (length liste-chaine) nlig) (not (nullstrp the-string)))
        (setq buffer (split-string1 the-string width))
        (newl liste-chaine
          (if (eq (length liste-chaine) (sub1 nlig)) 
            the-string
            buffer))
        (setq the-string (substring the-string (slength buffer))))
      (reverse liste-chaine)))



;;; 2. Les procedures d'alerte:
;;;


;;; Primitive d'alerte
;;; Elle envoit une fenetre de message du type demande (ex: prompt-warn)
;;; avec le ou les messages donnes en argument
;;;

(defun alerte-type (type message1 . reste-messages)
  (funcall 
    type           
    (letn boucle 
      ((M1 message1)
       (M2 reste-messages))
      (cond 
        ((null M2) M1)
        (t 
          (boucle (catenate m1 (car M2))(cdr M2)))))))

;;; une alerte pour un champ obligatoire
;;; le champ <champ> est obligatoire...
;;; petite bidouille :
;;; attendu qu'un champ est defini par son accesseur
;;; et qu'un accesseur est defini comme type-objet+champ
;;; et que le libelle du champ est defini comme un message pointe par 
;;; champ+type-objet. Cette fonction retrouve le libelle du champ
;;; et imprime le bon message 


(defun alerte (champ)
  (alerte-type 
      'afficher-attention
      #Mv_champ
      champ
      #Mv_obligatoire))

;;; Objets Aida:
;;;
;;; <hypersel>   un hyperselectionneur pour choix simples
;;; <hyperselmultiple> un hyperselectionneur pour choix multiples
;;;
;;; demande-type : fonction de demande d'une chaine avec un hypersel
;;; demandes-type : foncion de demande de chaines avec hypersel
;;; demande-simple-type : demande d'un chaine sans hypersel
;;; demandes-simples-type : demande d'une liste de chaine sans hypersel
;;; demande-valeur-type : routine generale de demande d'un valeur typee
;;; demande-chaine : demande d'un chaine
;;; demande-entier : demande d'un nombre entier


;;; definition d'un hyperselectonneur standard
;;; le composant hyper de l'application renvoyee
;;; pointe sur l'objet aida hyperselector
;;; lorsque cette application est utilisee dans un dialogue
;;; l'action de l'hyperselectionneur met a jour la variable
;;; answer du dialogue

(defun hypersel (w h strings)
  (let*
    ((hy
      (hyperselector 1 1 (- w 2) (- h 2) ()))
      (l (simplestringedit 0 0 w))
      (vv (verticalscrollbar 0 0
	{scroller}:scrollbarwidth h 0 1 0 1 0))
      (a (application '{application} 0 0 0 0
	(column 
	  (row vv (boxedimage 0 0 (add1 w) (add1 h) hy))
	  (boxedimage 0 1 
	    (+ 1 {scroller}:scrollbarwidth w) 
	    #3wchar (translation 0 0 l))))))
    (set-action hy
      (lambda (a)
	(let
	  ((is-dialogue (componentp 'asker a)))
	  (when is-dialogue
	    ({asker}:set-answers (component 'asker a)
	      (send 'get-selected-strings a))))))

    (send 'link-filter hy l)
    (send 'link-vscrollbar hy vv)
    (send 'set-strings hy strings)
    (send 'fit-to-contents a)
    (add-component a 'hyper hy)
    (current-keyboard-focus-window (send 'window l))
    a))

;;; meme chose, mais permet de selectionner plusieurs chaines
;;; MOD FD-11/05/95 Le <stringmenu> a ete remplacer par la fonction
;;; <liste-avec-vscroller> qui retourne un hyperselecteur avec
;;; un scroller vertical qui lui est associe -> permet de resoudre
;;; le problème des ascenseurs qui ne fonctionnaient pas

(defun hyperselmultiple (w h strings)
  (let
    ((h (hypersel w h strings))
     (selection (liste-avec-vscroller 57 9 ())))
    (set-action (component 'hyper h)
      (lambda (a)
	(let*
	  ((friend (component 'friend a))
	    (selectionnees (send 'get-strings friend))
	    (selection (send 'get-selected-string a))
	    (is-dialogue (componentp 'asker a)))
	  (when (and selection (not (member selection selectionnees)))
	    (send 'set-strings friend
	      (cons selection selectionnees))
	    (when is-dialogue
	      ({asker}:set-answers (component 'asker a)
		(send 'get-strings friend)))))))         
    (set-action (component 'liste selection)
      (lambda (s)
	(let ((is-dialog (componentp 'asker s))
	  (sels (send 'get-strings s))
	  (sel (send 'get-selected-string s)))
	(when is-dialog
	  ({asker}:set-answers (component 'asker s)
	    (delete sel ({asker}:get-answers (component 'asker s)))))
	(when sel 
	  (send 'set-strings s
	    (delete sel sels)))))) 
    (add-component h 'friend (component 'liste selection))
    (column
      (standard-list-input 0 0 10 130 " " selection)
      (rectangle 0 0 10 10)
      h)))



;;; l'action standard : renvoie la chaine selectionnee dans 
;;; une application contenant l'hyperselecteur

(defun get-hyper-string (hyper-appli)
  (send 'get-selected-string (component 'hyper hyper-appli)))


;;; routine standard de saisie dans une liste triee 
;;;cette routine integre l'hyperselectionneur défini ci-dessus
;;;- message est la question a poser a l'utilisateur
;;;- liste, la liste des chaines a demander,
;;;- une fonction de tri
;;;- une fonction d'ajout pour saisir
;;;- une valeur qui n'est pas présente dans la liste
;;;- optionnellement l'option tous (permet de tout selectionner)
;;;  attention, dans ce cas la fonction renvoie une liste

(defun demande-type (message liste-libelles fonction-tri fonction-ajout tous)
  (if liste-libelles
    (progn
      (setq liste-libelles
	(supprimer-chaines-multiples (sort fonction-tri liste-libelles)))
      (let
	((result
	  (afficher-demande
	    (column
	      message 
	      (hypersel 403 126 liste-libelles)
	      (filledbox 0 0 1 1 0))
	    ()
	    (supprimer-occurences-vides
	      (list
		(when fonction-ajout
		  (bouton-ajout-dialogue fonction-ajout))
		(when tous
		  (bouton-tous-dialogue)))))))
	(if (equal result 'tous) liste-libelles result)))
    (if fonction-ajout (funcall fonction-ajout))))



;;; meme chose mais renvoie une liste
;;; ajt (09/12/94) MO
;;; je mets l'option tous, histoire de pouvoir tous les selectionner ensemble


(defun demandes-type (message liste-objets fonction-tri fonction-ajout tous)
  (if liste-objets
    (progn
      (setq liste-objets
	(supprimer-chaines-multiples (sort fonction-tri liste-objets)))
      (let 
	((result
	  (afficher-demandes
	    (column
	      message 
	      (hyperselmultiple 403 126 liste-objets)
	      (filledbox 0 0 1 1 0))
	    ()
	    (supprimer-occurences-vides
	      (list
		(when fonction-ajout
		  (bouton-ajout-dialogue fonction-ajout))
		(when tous
		  (bouton-tous-dialogue)))))))
	(if (equal result '(tous)) liste-objets result)))
    (if fonction-ajout (list (funcall fonction-ajout)))))


;;; fonction de demande d'une chaine dans une liste (sans hyperselectionneur)

(defun demande-simple-type (message liste boutons)
  (when liste
    (afficher-demande-liste-simple
      message
      (supprimer-chaines-multiples liste)
      boutons)))

;;; fonction de demande d'une liste de chaines 
;;; dans une liste (sans hyperselectionneur)

(defun demandes-simples-type (message liste boutons)
  (when liste
    (afficher-demandes-liste-simple
      message
      (supprimer-chaines-multiples liste)
      boutons)))


;;; routine standard de saisie d'une valeur typee éventuellement bornée
;;; message est la question a poser a l'utilisateur
;;; min et max, les valeurs (nombres) minimales et maximales autorisées
;;; fonction-libelle : une fonction qui transforme min et max en libellé
;;; fonction-filtre : une fonction de filtre qui recevra comme argument
;;; la chaine, le min et le max.

(defun demande-valeur-type (message min max fonction-libelle fonction-filtre)
  (let 
    ((label-min (funcall fonction-libelle min))
      (label-max (funcall fonction-libelle max))
      (cont 1)
      (result ()))
    (while 
      (neq cont 0)
      (let*
	((prompt (catenate message" ? [" label-min "-" label-max "]"))
	 (val (afficher-demande-valeur prompt () t ())))
	(if val
	  (when (not (funcall fonction-filtre val min max))
	    (setq result val)
	    (setq cont 0))
	  (setq cont 0))))
    result))

;;; routine standard de saisie d'un chaine de caracterers

(defun demande-chaine (message)
  (afficher-demande-valeur message () t ()))

;;; CRE RM - 23/08/96 : demande d'une chaine avec reaffichage d'une
;;; ancienne valeur dans l'editeur

(defun demande-chaine-avec-ancienne-valeur (titre message old-value)  
  (let
    ((dialogue
      (strings-asker message ()
	(list (string old-value)) 1 1 t ())))
    (with ((current-background (make-named-color "white")))
      (send 'set-title dialogue titre)
      (grab-and-select-application dialogue)
      ({asker}:get-answer dialogue))))


;;; 4. Utilitaires pour les editeurs
;;;


;;; Encadre l'image
;;; ajoute le prompt dans prompt dans le rectangle
;;; en met tout ca en x et y … la bonne taille w
;;; h est ingnore par cette fonction mais doit figurer
;;; dans la liste des arguments pour des raisons de compatibilite
;;; avec les autres fonctions d'affichage

(defun standard-input (x y w h prompt image)
  (send 'resize image
    (- w (send 'width (font arial8b prompt)) 4)
    ())
  (view 
    (box x y w (+ 4 (send 'height (font arial8b prompt))))
    (translation
      (+ x 3) (+ y 2) (font arial8b prompt))
    (translation
      (+ x 3 (send 'width (font arial8b prompt)))
      (+ y 2)
      image)))

;;; construit une liste avec un scrollbar 
;;; on peut forcer la largeur en donnant w > taille initiale
;;; n-vis est le nombre d'elements qui doivent etre visibles
;;; 20 est la taille horizontalle d'un scroller
;;; devrait s'appeler <image-ascenceur image>

(defun standard-list (x y n-vis w image)
  (let*
    ((wscroll {scroller}:scrollbarwidth)
      (w-image (send 'width image))
      (width (if (eq w-image 0) (bitxmax) w-image))
      (haut (max #hchar (send 'height image))) 
      (scroller
	(verticalscroller 0 0 (add wscroll width) haut image))
      (sc
	(elasticboxedimage x y
	  (add wscroll width) haut scroller))
      (appli (application '{application} x y w (* n-vis #hchar) 
	(elasticview sc))))
    (send 'reinitialise scroller)
    (add-component appli 'scroller scroller)
    appli))


;;; renvoie une application avec un scroller
;;; (de nom de composant 'scroller)

(defun list-and-scroll (image)
  (let*
    ((image2 (send 'resize image 50 50))
      (scroller (verticalscroller 0 0 20 50 image2))
      (sc (elasticboxedimage 0 0 20 50 scroller)))
    (send 'reinitialise scroller)
    (add-component image 'scroller scroller)
    (application '{application} 0 0 50 50 sc)))

;;;FD-11/05/95 pour remplacement de list-and-scroll

(defun liste-avec-vscroller (w h liste)
  (with ((current-font FONT-EDITEUR))
    (let*
      ((selecteur
	(hyperselector 0 0 (mul #wchar w) (mul #hchar h) liste))
      (scrolleur
	(verticalscrollbar
	  0 0 (add 3 {scroller}:scrollbarwidth)
	  (send 'h selecteur)
	  0 (max 1 (length liste)) 0 1 0)))
      (send 'link-vscrollbar selecteur scrolleur)
      (mixed-applicationq
	liste selecteur
	image
	(boxedimage 0 0 
	  (add 2 
	    (add
	      (send 'w selecteur)
	      (send 'w scrolleur)))
	  (add 2
	    (send 'h selecteur))
	  (row scrolleur selecteur))))))


;;; methode d'affichage standard d'une application
;;; en x y et de la bonne taille
;;; avec le libelle <prompt> au dessus de l'appli


(defun standard-list-input (x y w h prompt appli)
  (let*
    ((pr (font arial8b (catenate " " prompt " "))))
    (send 'resize appli w h)
    (translation x y
      (column pr appli))))

(defun standard-box (x y w h prompt appli)
  (send 'resize appli w h)
  (translation x y appli))

(defun standard-rect (x y w h prompt appli)
  (send 'resize appli (sub w 2) (sub h 2))
  (translation x y
    (column
      (font arial8b prompt)
      (view (box 0 0 w h) (translation 1 1 appli)))))

;;;... str not yet implemented...

(defun nyi (str)
  (afficher-attention
    (column str "" #Mv_nyi "")))
  

;;; met a jour un stringmenu
;;; si chaine est dans la liste, on le retire (si dete est vrai)
;;; sinon, on l'ajoute

(defun update-stringmenu (menu chaine delete)
  (let
    ((chaines (send 'get-strings menu)))
    (send 'set-strings menu
      (if (member chaine chaines)
	(if delete (delete chaine chaines) chaines)
	(cons chaine chaines)))))




;;; liste des application qui contiennent un composant type
;;; FD - 25/07/95 - MOD
;;; test si le composant est une application (aida-type-checked-p)
;;; chg (11/04/95) FD
;;; change deux <let>s en serie par un seul <let>


(defun user-appli (appli type)
  (let 
    ((out ())
     (lc (all-components appli))
     (co))
    (while lc
      (setq co (nextl lc))
      (when 
        (and
            (aida-type-checked-p (component co appli) 'aidaapplication)
            (componentp type (component co appli)))
        (newl out (component type (component co appli)))))
    out))


;;; gestion du focus, renvoie l'appli suivante dans la liste de composants 
;;; varuna d'une appli.
;;; 
;;; cette primitive a un comportement bizarre, si les deux premieres 
;;; sous-applis sont en "readonly-flag" car elle n'est pas recursive
;;; cela vient peut-e^tre de la gestion du code qui etait en readonly-flag ???

;;; d'ou une nouvelle ecriture de cette primitive 
;;; !! (a tester largement comme il se doit

;;; vieille version ante (11/04/95)

;;;(defun next-user-appli (appli type)
;;; (let*                             ; pourquoi un let* ?? 
;;;   ((lcv (user-appli appli type)))
;;;   (or 
;;;   (cadr (member appli lcv ))     
;;;     (if 
;;;     (send 'readonly-flag (car lcv)) 
;;;     (cadr lcv) 
;;;     (car lcv)))))

;;; rq.: <member> remplacer par <appartenir-liste2> pour eviter des pb
;;;      de pile pleine...
(defun next-user-appli (appli type)
  (let 
    ((lcv (user-appli appli type))
     (appli-suiv ))
    (setq appli-suiv (cadr (appartenir-liste2 appli lcv )))
    (if appli-suiv
      ;; action si on est pas a la derniere appli
      (if (send 'readonly-flag appli-suiv)
        ;; non selectionnable
        (next-user-appli appli-suiv type)
        ;; appli-suiv est selectionnable
        appli-suiv)
      (if        ; actions pour la derniere appli  
        (send 'readonly-flag (car lcv)) 
        (cadr lcv) 
        (car lcv)))))

;;; iterateur sur les zones de donnees (action du bouton annuler d'un editeur 
;;; en modification)

;;; iterateur sur les zones de donnees (action du bouton OK)
;;; MOD RM - 27/02/97 : reecriture de la fonction

(defun editeur-ok (edi)
  (every
    (lambda (e)
      (send 'i-valider e))
    (user-appli edi 'user)))

; (defun editeur-ok (edi)
;   (letn 
;     boucle
;     ((comp (user-appli edi 'user)))
;     (cond
;       ((null (car comp)) t)
;       ((not (send 'i-valider (car comp))) ())
;       (t (boucle (cdr comp))))))

;;; routine appelee lorsque l'utilisateur clique sur OK
;;; dans un editeur
;;; on verifie avant de sauver que
;;; tous les champs sont valides avec la fonction editeur-ok
;;; appelle la fonction de controle d'integrite
;;; stockee dans le champ userdata de l'appli ed (le bouton)

(defun sauver-editeur (ed)
  (when (editeur-ok ed)
    (mapc
      (lambda (c) (send 'i-sauver c))
      (user-appli ed 'user))
    (when
      (funcall
	({application}:userdata ed)
	({application}:userdata (component 'appli ed)))
      (remove-application (component 'appli ed))
      (setq Dirty t))))

(defun annuler-editeur (ed)
  (remove-application (component 'appli ed))
  (mapc
    (lambda (c) (send 'i-restore c))
    (user-appli ed 'user))
  ({application}:userdata (component 'appli ed) ()))


;;; Definition des rectangles

;;; un rectangle dans une grille est definie par 5 elements
;;; le numero de grille, les lignes et colones

(defun cons-rectangle (grille c1 c2 l1 l2)
  (list grille c1 c2 l1 l2))

;;; accesseurs
(defun grille-rect (rect)
  (car rect))
(defun col1-rect (rect)
  (cadr rect))
(defun col2-rect (rect)
  (caddr rect))
(defun ligne1-rect (rect)
  (cadddr rect))
(defun ligne2-rect (rect)
  (car (last rect)))

;;; une position de rectangle est definie
;;; par ses corrdonees du point haut gauche (x et y)
;;; par sa largeur et sa hauteur

(defun cons-position-rectangle (x y w h)
  (list x y w h))

;;; des accesseurs

(defun x-pos-rect (pos)
  (car pos))
(defun y-pos-rect (pos)
  (cadr pos))
(defun w-pos-rect (pos)
  (caddr pos))
(defun h-pos-rect (pos)
  (cadddr pos))


;;; renvoie le rectangle englobant tous les rectangles de liste rects

(defun union-rect1 (r1 list-of-aida-rects)
  (if (null list-of-aida-rects)
    (or r1 (rectangle 0 0 0 0))
    (union-rect1
      (if (null r1)
        (car list-of-aida-rects)
        ({rectangle}:join-rectangle
          r1
          (or (car list-of-aida-rects) r1)))
      (cdr list-of-aida-rects))))
      
(defun union-rect (list-aida-rects)
  (if (null list-aida-rects)
    (rectangle 0 0 0 0)
    (if (null (cdr list-aida-rects))
      (or (car list-aida-rects) (rectangle 0 0 0 0))
      (union-rect1 (car list-aida-rects) (cdr list-aida-rects)))))
    

;;; cette macro definit une application pour l'edition d'un champ liste
;;; d'objets. L'objet doit avoir deux accesseurs code et libelle.
;;; norme pour les procedures d'edition
;;; ajout <ajouter-objet>
;;; edtion <editer-objet (objet evalue) (t)>
;;; + des selectioneurs du type
;;; <demandes-objet (prompt) (procedure d'ajout)>
;;; parametres ...
;;; TRV - FD 08/12/95
;;; ajout de l'argument <sup-func> nom de la fonction de suppression (cette fct
;;; a un seul argument le code de l'objet a supprimer)

;;; TRV 29/04/96 - remplacer <demandes-objet> par <demandes-filtree-objet>
;;; idem pour <demande-objet>

(dmd def-selecteur-objet (objet)
  `(defun ,(concat 'selecteur- objet)
    (l-objet accesseur accesseur-objet ajout-mul ajout-creer oblig sup-func fonc-ajout)
    (selecteur-objet
      l-objet
      oblig
      accesseur
      accesseur-objet
      ',(concat 'code- objet)
      ',(concat 'libelle- objet)
     (lambda (o)
       ;; MOD RM - 22/04/96 : ajout de <creationp> dans <editer-...>
       (,(concat 'editer- objet) (eval o) ()))
     (if fonc-ajout
       fonc-ajout
     (if ajout-creer 
       (lambda () 
         (list
           (car
             (last
               (,(concat 'libelle- objet '2codes- objet)
		 (,(concat 'ajouter- objet)))))))
       (if ajout-mul
         (lambda () 
           (,(concat 'demandes- objet) 
             (get-message ',(concat 'v_ objet))
             ',(concat 'ajouter- objet) 
             ()))
         (lambda () (list (,(concat 'demande- objet)
           (get-message ',(concat 'v_ objet)) 
           ',(concat 'ajouter- objet)))))))
     sup-func
    )))


;;; cette macro genere pour un type d'objet des selecteurs = dialogues
;;; pour selectionner les objets par leur libelles ou par leur codes.
;;; la fonction generee est du type (demande-objets prompt func)
;;; pour une demande simple et (demandes-objet prompt func) pour une demande
;;; de plusieurs objets. 
;;; la fonction func est une fonction d'ajout d'objet. elle doit exister sous forme
;;; (ajouter-objet ()) et doit renvoyer () ou le libelle de l'objet.
;;; Les objets doivent repondre aux deux accesseurs code et libelle
;;; le code assurant l'unicite. 
;;; 


;;; gestion des reponses multiples
;;; FD - 05/01/95 - pourquoi avoir un parametre sortie, si il n'est pas utilise

(defun reponses-multiples (liste-objets selecteur prompt sortie multi)
  (cond
    ((not liste-objets) ())
    ((eq (length liste-objets) 1) (eval (car liste-objets)))
    (t (let ((code (demande-type prompt
      (mapcar (lambda (obj)
        (string (funcall selecteur obj))) liste-objets)
      'alphalessp () multi)))
    (when code
      (if (consp code)
        (evlis (mapcar (lambda (chaine) (symbol () chaine)) code))
        (eval (symbol () code))))))))

;;; CRE - FD - 05/01/96
(defun reponses-multiples-choix-multiple (liste-objets selecteur prompt sortie multi)
  (cond
    ((not liste-objets) ())
    ((eq (length liste-objets) 1) (eval (car liste-objets)))
    (t (let ((code (demandes-type prompt
      (mapcar (lambda (obj)
        (string (funcall selecteur obj))) liste-objets)
      'alphalessp () multi)))
    (when code
      (if (consp code)
        (evlis (mapcar (lambda (chaine) (symbol () chaine)) code))
        (eval (symbol () code))))))))


(dmd def-demandeur-objet (objet)
  `(progn
  (defun ,(concat 'demande- objet) (prompt nouveau-p)
    (let* (
      (str
        (demande-type
          prompt
	  (mapcar ',(concat 'libelle- objet) ,(concat 'liste- objet))
          'alphalessp
          (and nouveau-p ',(concat 'ajouter- objet))
          ()))
      (les-objets (,(concat 'libelle- objet '2 'codes- objet) str)))
        (reponses-multiples les-objets ',(concat 'code- objet)
          (column (get-message 'v_plusieurs-reponses) (string str))
          ',(concat 'code- objet) ())))
  
  (defun ,(concat 'demande-filtree- objet)
    (prompt prompt-null nouveau-p filtre)
    (let ((sub-list 
      (mapcar ',(concat 'libelle- objet)
        (sous-liste 
          (lambda (x) (funcall filtre x))
          ,(concat 'liste- objet)))))
            (ifn sub-list
              (not
                (afficher-attention prompt-null))
              (let* (
                (str (demande-type prompt sub-list 
                  'alphalessp
                  (and nouveau-p ',(concat 'ajouter- objet))
                  ()))
		;;(les-objets (,(concat 'libelle- objet '2 'codes- objet) str)))
		;; MOD FD - 24/01/96 - il faut aussi filtrer les codes
		;; => ajout de l'appel a <filtre>
                (les-objets
                  (sous-liste
                    (lambda (x) (funcall filtre x))
                    (,(concat 'libelle- objet '2 'codes- objet) str))))

	      (reponses-multiples les-objets ',(concat 'code- objet)
                    (column (get-message 'v_plusieurs-reponses)
                      (string str)) ',(concat 'code- objet) ())))))
  
  (defun ,(concat 'demandes- objet) (prompt nouveau-p tous)
    (let* 
      ((strs
	(supprimer-chaines-multiples
	  (demandes-type 
	    prompt
	    (mapcar ',(concat 'libelle- objet)
	      ,(concat 'liste- objet))
	    'alphalessp 
	    (and nouveau-p ',(concat 'ajouter- objet))
	    tous)))
      (les-objets (mapcar ',(concat 'libelle- objet '2codes- objet) strs)))
      (when les-objets
        (supprimer-occurences-vides
          (apply 'cons-or-append
            (mapcar
	      (lambda (lo str)
		(reponses-multiples lo ',(concat 'code- objet)
		  (column #Mv_plusieurs-reponses (string str))
		  ',(concat 'code- objet) t))
	      les-objets strs))))))

;;; AJT RM - 29/06/95
;;; Pour la liste des cours instancies sans salles

  (defun ,(concat 'demandes-filtree- objet) (prompt nouveau-p tous filtre)
    (let* 
      ((strs
	(supprimer-chaines-multiples	
	  (demandes-type 
	    prompt
	    (mapcar ',(concat 'libelle- objet)
	      (sous-liste
		(lambda (x) (funcall filtre x))
		,(concat 'liste- objet)))
	    'alphalessp 
	    (and nouveau-p ',(concat 'ajouter- objet))
	    tous)))
      (les-objets (mapcar ',(concat 'libelle- objet '2codes- objet) strs)))

      ;; MOD FD - 24/01/96 - il faut aussi filtrer les codes
      ;; => ajout de l'appel a <filtre>
      (when les-objets
        (supprimer-occurences-vides
          (apply 'cons-or-append
            (mapcar (lambda (lo str)
	      ;; MOD - FD - 05/01/96 - remplace <reponses-multiples>
	      ;; par <reponses-multiples-choix-multiple>
              (reponses-multiples-choix-multiple
		; lo avt le 24/01/96
		(sous-liste
		  (lambda (o)
		    (funcall filtre o))
		  lo)
		',(concat 'code- objet)
                (column (get-message 'v_plusieurs-reponses) (string str))
                ',(concat 'code- objet) t))
            les-objets strs))))))))


;;; 5. Resources Aida:
;;;
;;; gestion des resources Aida suivant le systeme d'exploitation
;;; polices, couleurs, fonctions d'impressions des couleurs en niveau de gris
;;; icones, ...

;;; les polices
;;; rem (07/06/95) MO
;;; changement du nom: <make-fonts> -> <initialiser-polices>

(defun initialiser-polices ()
  (cond
    ((eq (system) 'msdos)
      (progn
        (setq time5 (load-font "Times New Roman-5-TT"))
        (setq time5b (load-font "Times New Roman-5-TT-Bold"))
	(setq time6b (load-font "Times New Roman-6-TT-Bold"))
        (setq time8 (load-font "Times New Roman-8-TT"))
        (setq time8b (load-font "Times New Roman-8-TT-Bold"))
        (setq time10 (load-font "Times New Roman-10-TT"))
        (setq time10b (load-font "Times New Roman-10-TT-Bold"))	
        (setq courier8 (load-font "Courier New-8-TT"))
        (setq courier12 (load-font "Courier New-12-TT"))
        (setq courier12b (load-font "Courier New-12-TT-Bold"))
        (setq arial8 (load-font "Arial-8-TT"))
        (setq arial8b (load-font "Arial-8-TT-Bold"))
        (setq arial10 (load-font "Arial-10-TT"))
        (setq arial10b (load-font "Arial-10-TT-Bold"))
	(setq arial12b (load-font "Arial-12-TT-Bold"))	
	(setq FONT-EDITEUR courier8)))
    ((eq (system) 'unix)
      (progn
        (setq time5 0)
        (setq time5b 0)
	(setq time6b 0)
        (setq time8 0)
        (setq time8b 0)
	(setq courier8 0)
        (setq courier12 0)
        (setq courier12b 0)
        (setq arial10b 0)
        (setq arial10b 0)
	(setq arial12b 0)
	(setq FONT-EDITEUR 0)))))


;;; Les couleurs

;;; fonction de conversion de couleur en niveau de gris
;;; pour l'impression
;;; ??? (08/03/95) MO
;;; d'ou viennent ces chiffres ???

(defun niveau-gris (color)
  (if color
    (+
      (/ (#:color:red color) 106666.7)    
      (/ (#:color:green color) 56478.)
      (/ (#:color:blue color) 290909.1))
    0))


;;; CRE (08/01/95) MO
;;; creation de cette primitive servant pour les transformations de couleurs
;;; en gris pour les applications impressions graphiques
;;; rem (09/06/95) mo
;;; et ta bonne vieille habitude de nommer les primitives en francais ??? 

(defun color2grey (color)
  (let* 
    ((niveau (niveau-gris color))
     (composante (truncate (min 32767 (* niveau 32767)))))
    (make-color composante composante composante)))


;;; CRE (08/09/95) MO
;;; creation d'une primtive filtrant les gris noirs en les eclaircissant et
;;; mettant en blanc les gris trop clairs
;;; !! constantes dans la primitive 


(defun filtre-gris (niv-gris)
  (if niv-gris
    (cond
      ((< niv-gris 0.95) 0.95)
      ((> niv-gris 0.95) 0.95)
      (t niv-gris))
    0.95))


;;; CRE (22/03/95) MO
;;; cre'ation d'une liste de couleur nomme'e pour varuna
;;; pour le moment nous nous contentons de mettre une couche developpeur
;;; devant la liste-couleur actuelle mais l'avenir nous permettra, je
;;; l'espe`re de ge'rer les couleur par leur nom varuna dans tout le code
;;; pour cela, il faudra peut-e^tre se de'finir une table d'e'quivalence
;;; type de salle - nom de couleur et non prendre le nth de la liste-couleur


;;; accesseur <ajouter> une couleur nomme'e a la liste de couleur
;;; on pourrait implementer un pourcentage au lieu d'un chiffre nominal 
;;; pour les composantes rgb. -> il faut e^tre rapidement clair la-dessus

(defun ajouter-couleur-varuna (nom rouge vert bleu)
  (newl 
    liste-couleur-varuna 
    (cons
      nom
      (make-color rouge vert bleu)))
   t)    


;;; accesseur <trouver> qui renvoie la couleur aida pour un nom de couleur
;;;  varuna

(defun trouver-couleur-varuna (nom)
   (letn
     boucle
     ((liste-travail liste-couleur-varuna))
     (cond
       ((null liste-travail) ())
       ((equal (caar liste-travail) nom) (cdar liste-travail))
       (t (boucle (cdr liste-travail))))))

;;; primitive qui permet facilement de construire plus lisiblement la liste
;;; couleur -> est amene'e a` disparai^tre en me^me temps que liste-couleur
;;; atttention: il peut y avoir des listes vides dans cette liste

(defun construire-liste-couleur liste-nom-couleur
  (mapcar 'trouver-couleur-varuna liste-nom-couleur))


;;; primitive enlever qui ne se contente pas d'enlever la couleur-varuna
;;; de la liste de ces couleurs mais qui "kill" en me^me temps le vecteur
;;; couleur, libe'rant ainsi de la place me'moire, conseil donne' par INRIA
;;; cahpitre 20 manuel LISP

(defun enlever-couleur-varuna (nom)
   (letn
     boucle
     ((liste-travail liste-couleur-varuna)(liste-resultat))
     (cond
       ((null liste-travail) ())
       ((equal (caar liste-travail) nom) 
        (progn
          (cons (reverse liste-resultat) (cdr liste-travail))
          (kill-color (cdar liste-travail))))
       (t (boucle 
            (cdr liste-travail)
            (cons (car liste-travail) liste-resultat))))))

;;; primitive qui "fait" les couleurs de la liste-couleur
;;; a terme devra e^tre revu pour enlever la liste-couleur et la 
;;; remplacer par la liste des couleurs varuna nomme'es

;;; CHG (07/06/95) MO
;;; changement de nom de primitive
;;; <make-colors> -> <initialiser-couleurs>

(defun initialiser-couleurs ()
  (defllresource-named-color 'foreground system.inhibit "blue")
  (setq couleur-item-inhibe (make-named-color "blue"))
  (setq couleur-item-actif (make-named-color "black"))
  (setq couleur-separateur-inhibe (make-named-color "black"))
  (setq COULEUR-RESA (make-named-color "red"))
  (setq COULEUR-AJOUT (make-named-color "cyan"))
  (ajouter-couleur-varuna "gris" 24576 24576 24576)
  (ajouter-couleur-varuna "rouge-pale" 24575 0 0)
  (ajouter-couleur-varuna "jaune-pale" 24575 24575 0)
  (ajouter-couleur-varuna "vert-pale" 0 24575 0)
  (ajouter-couleur-varuna "cyan-pale" 0 24575 24575)
  (ajouter-couleur-varuna "bleu-pale" 0 0 24575)
  (ajouter-couleur-varuna "violet-pale" 24575 0 24575)
  ;   (ajouter-couleur-varuna "rouge-fonce" 16383 0 0);; FD
  (ajouter-couleur-varuna "jaune-fonce" 16383 16383 0)
  (ajouter-couleur-varuna "vert-fonce" 0 16383 0)
  (ajouter-couleur-varuna "cyan-fonce" 0 16383 16383)
  ;   (ajouter-couleur-varuna "bleu-fonce" 0 0 16383);; FD
  ;   (ajouter-couleur-varuna "violet-fonce" 16383 0 16383);; FD
  (ajouter-couleur-varuna "rouge-pastel" 32767 16383 16383)
  (ajouter-couleur-varuna "jaune-pastel" 32767 32767 16383)
  (ajouter-couleur-varuna "vert-pastel" 16383 32767 16383)
  (ajouter-couleur-varuna "cyan-pastel" 16383 32767 32767)
  (ajouter-couleur-varuna "bleu-pastel" 16383 16383 32767)
  (ajouter-couleur-varuna "violet-pastel" 32767 16383 32767)
  (ajouter-couleur-varuna "rose" 32767 0 16383)

  (setq LISTE-COULEURS
    (construire-liste-couleur
      "vert-pastel"              ;;  "jaune-pastel" "rouge-pastel"
      "jaune-pale"  "cyan-pale" "vert-pale" 
      "jaune-fonce"  "cyan-fonce" "rose" "violet-pale"
      "bleu-pastel" "violet-pastel"))) 



;;; chargement des icones                       
;;; si les fichiers n'existent pas, 
;;; l'icone est creee avec un bitmap vide
;;; il n'apparaitera donc pas a l'ecran mais 
;;; ca ne bloque pas...

(dmd charger-icone (icon-global-var icon-file alternative)
  `(when (not ,icon-global-var)
    (if (probefile ,icon-file)
      (setq ,icon-global-var (libloadicon ,icon-file))
      (setq ,icon-global-var ,alternative))))

;;; <make-icons> -> <initialiser-icones> (07/06/95)
;;; CHG (07/06/95) MO
;;; on fait le (with ((current-directory))) dans <initialiser-icones>

(defun initialiser-icones ()
  (with
    ((current-directory REP-VARUNA))
    (let 
      ((icone-nulle (create-bytemap 0 0)))
      (charger-icone Icon-Varuna Fichier-Icon-Varuna icone-nulle)
      (charger-icone Icon-Ungrow Fichier-Icon-Ungrow icone-nulle)
      (charger-icone Icon-Grow Fichier-Icon-Grow icone-nulle)
      (charger-icone Icon-Size Fichier-Icon-Size icone-nulle)
      (charger-icone Icon-Scale Fichier-Icon-Scale icone-nulle)
      ;; AJT - FD 02/07/96
      (charger-icone Icon-Suivant Fichier-Icon-Suivant icone-nulle)
      (charger-icone Icon-Precede Fichier-Icon-Precede icone-nulle)
      ;; AJT - RM 05/05/97 (boites de dlg)
      (charger-icone Icon-Exclamation Fichier-Icon-Exclamation icone-nulle)
      (charger-icone Icon-Question Fichier-Icon-Question icone-nulle)
      (charger-icone Icon-Information Fichier-Icon-Information icone-nulle)
      (charger-icone Icon-Stop Fichier-Icon-Stop icone-nulle)

      ;; AJT RM 02/10/97 : icone pour les demandes
      ({asker}:asker-icon (icon 0 0 Icon-Question))
      
      (when (not Dispo-Pat)
	(if (probefile Fichier-Pat)
	  (setq DISPO-PAT (make-pattern (libloadicon Fichier-Pat)))
	  (setq DISPO-PAT (pattern-max)))))))


;;; LES CURSEURS

;;; <initialiser-curseurs>
;;; CRE - FD - 19/09/95
;;; primitive qui "cre" les curseur utiliser par Varuna

(defun initialiser-curseurs ()
  (setq Varuna-Curseur-Fleche (make-named-cursor "left-ptr"))
  (setq Varuna-Curseur-Sablier (make-named-cursor "watch")))

;;; <change-curseur>
;;; CRE - FD - 31/08/95
;;; change l'aspect du curseur
;;; arg: b composant d'une application pour laquelle on doit modifier le
;;;        curseur
;;;      curseur : curseur (ah bon ? - FD), les valeurs

(defun change-curseur (elt curseur)
  (let
    ((appli))
    (cond
      ((eq elt Gen-App)
	(setq appli Gen-App))
      ((eq elt Input-App)
	(setq appli Input-App))
      (t
	(setq appli ({application}:father (send 'appli (send 'window elt))))))
    (when (activatedp appli)
      (with
        ((current-window (send 'window appli)))
	(current-cursor curseur)
        (bitmap-sync)(bitmap-flush)(process-pending-events)))))


;;; 6. MACRO DE DEFINITION DES CONCEPTS DE L'INTERFACE
;;;


;;; listing renvoie la variables globale : liste des objets 

(defun listing (classe)
  (concat 'Liste- classe))
  
;;; GENERATEUR DE SYMBOLES UNIQUES

;;; MOD RM - 05/09/95 : recherche du code dans le ficher <varcode.trs>
;;; pour les donnees en reseau
;;; 23/02/96 - FD - MultiP

(defun next-value (c index)
  (let
    ((i index))
    (while (boundp (symbol () (concat PREFIXE c i)))
      (setq i (add1 i)))
    i))
    
(defun gencode (caractere index)
  (symbol () (concat PREFIXE caractere index))))

;;;  MOD RM - 29/08/95 : utilisation de compteur de code pour la generation 

(dmd generer-fonction-code (classe car)
  `(defun ,(concat 'gencode- classe) ()
    (let
      ((code))
      (lire-fichier-codes)
      (setq ,(concat 'compteur- classe)
	(next-value ,car ,(concat 'compteur- classe)))
      (setq code (gencode ,car ,(concat 'compteur- classe)))
      (setq ,(concat 'compteur- classe)
	(add ,(concat 'compteur- classe) 1))      
      (ecrire-fichier-codes)
      code)))

;;; CRE RM - 05/09/95 : mise a jour des compteurs de code
;;; MOD - FD - 29/08/96 gestion du EOF

(defun lire-fichier-codes ()
  (with ((current-directory REP-DONNEES))
    (if (probefile FICHIER-VARCODE)
      (progn
	;; intialisation dans le cas ou le fichier-varcode n'est pas comple
	(mapc
	  (lambda
	    (compteur) (set compteur 0))
	  '(compteur-i-cours compteur-i-prof compteur-i-salle
	    compteur-i-groupe compteur-i-resa compteur-i-ajout
	    compteur-i-annulation compteur-i-site))
	(with ((inchan (openi FICHIER-VARCODE)))
	  (untilexit EOF
	    (mapc
	      (lambda
		(compteur) (set compteur (read)))
	      '(compteur-i-cours compteur-i-prof compteur-i-salle
		compteur-i-groupe compteur-i-resa compteur-i-ajout
		compteur-i-annulation compteur-i-site)))
	  (close)))
      (progn
        (setq compteur-i-cours (length liste-i-cours))
        (setq compteur-i-prof (length liste-i-prof))                    
        (setq compteur-i-salle (length liste-i-salle))
        (setq compteur-i-groupe (length liste-i-groupe))
        (setq compteur-i-resa (length liste-i-resa))
        (setq compteur-i-ajout (length liste-i-ajout))
        (setq compteur-i-annulation (length liste-i-annulation))
	(setq compteur-i-site (length liste-i-site))))))

	
(defun ecrire-fichier-codes ()
  (when (not LECTURE-SEULE?)
    (with ((current-directory REP-DONNEES))
      (with ((outchan (openo FICHIER-VARCODE)))
	(print compteur-i-cours)
	(print compteur-i-prof)
	(print compteur-i-salle)
	(print compteur-i-groupe)
	(print compteur-i-resa)
	(print compteur-i-ajout)
	(print compteur-i-annulation)
	(print compteur-i-site)
	(close)))))
  
;;; Recherche dans une liste de l'element dont
;;; ayant pour libelle (selecteur element)

(defun libelle2codes (libelle selecteur liste)
  (let ((result ()))
    (mapc (lambda (inst)
      (when (equal (funcall selecteur inst)  libelle)
        (newl result inst)))
          liste)
    result))

;;; Renvoie le code de l'objet dont le libelle est libelle

(dmd libelle2code (classe)
  `(defun ,(concat 'libelle- classe '2codes- classe) (libelle)
      (libelle2codes libelle ',(concat 'libelle- classe) ,(listing classe))))


;;; On a une macro pour la definition des concepts
;;; On definit la variable globale liste-xxx, la classe, 
;;;  la fonction de creation et les accesseurs, AUTOMATIQUEMENT.
;;; pour un objet d'interface, les champs code et libelle
;;; sont obligatoires et automatiquements crees.
;;; chaque objet d'interface possede aussi un champ
;;; editeur qui est le symbole de la fonction de creation de la fenetre
;;; d'edition

;;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;;; generation d'un accesseur. Si l'objet vaut nil
;;; la fonction renvoie le libelle du champ sous forme de message
;;; construit par concatenation du nom du champ et de l'objet.
;;; et du prefixe v_
;;; rem (02/05/95) MO 
;;; c'est assez interessant comme astuce, mais il FAUT VRAIMENT LE SAVOIR 
;;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


(dmd def-i-accesseur (type-objet champ nom-accesseur)
  `(defun ,nom-accesseur (objet . val)
    (if objet
      (ifn val
        (and objet (funcall 'ogetq (get-abbrev ',(symbol () type-objet))
          ',champ objet))
        (and objet (funcall 'oputq (get-abbrev ',(symbol () type-objet))
          ',champ objet '(car val))))
      (get-message ',(concat 'v_ type-objet '- champ)))))


(dmd def-iclasse (nom car . l)
  `(progn
    
    ;; la globale
    (defvar ,(concat 'liste- nom) ()) 
    
    ;; la classe
    (deftclass ,nom code libelle editeur . ,(mapcar 'faire-champ2 l))
    
    ;; un generateur de code pour cette classe
    (generer-fonction-code ,nom ,car)
    
    ;; AJT RM - 29/08/95 : le compteur de codes (contient la valeur max des
    ;; codes crees

    (defvar ,(concat 'compteur- nom) 0)
    
    ;; un accesseur par libelle
    (libelle2code ,nom)
    
    ;; les fonctions de demande
    (def-demandeur-objet ,nom)
    
    ;; un selecteur
    (def-selecteur-objet ,nom)
    
    ;; une fonction de creation avec tous les champs en parametres
    (defun ,(concat 'creer- nom)
      (code libelle . ,(mapcar 'car (filtrer-champ2 l)))
      (let ((y (funcall 'omakeq
        (get-abbrev ',(symbol () nom))))) 
      (prog1 y
        (,(concat 'code- nom) y code)
        (,(concat 'libelle- nom) y libelle)
        (set code y)
        .,(mapcar (lambda (x)
          (list (concat (car x) '- nom) 'y (car x)))
        (filtrer-champ2 l)))))
    
    ;; Une fonction de creation sans parametres
    (defun ,(concat 'nouveau- nom) ()
      (funcall 'omakeq (get-abbrev ',(symbol () nom))))
    
    ;; les accesseurs 
    (def-i-accesseur ,nom code ,(concat 'code- nom))
    (def-i-accesseur ,nom libelle ,(concat 'libelle- nom))
    (def-i-accesseur ,nom editeur ,(concat 'editeur- nom))
    . ,(mapcar
      (lambda (x) 
        (list 'def-i-accesseur nom (car x)
          (concat (car x) '- nom)))
      l)))


(dmd def-i-sous-accesseurs (classe sous-classe . l-champs)
  `(progn
    .,(mapcar (lambda (champ)
      `(defun ,(concat champ '- classe) (ob . valeur)
        (let ((s-class (,(concat sous-classe '- classe) ob)))
          (ifn valeur
            (and s-class (,(concat champ '- sous-classe) s-class))
            (and s-class (,(concat champ '- sous-classe) s-class
              (car valeur)))))))
    l-champs)))



;;; utilitaires pour la saisie des dispos


(defun faire-zone-speciale (liste-horaires liste-jours) 
  (let ((lj liste-jours)
        (lh liste-horaires)
        (lt ()))
       (while lj
         (while (cdr lh)
           (newl lt 
             (creer-intervalle 
               (cons-moment-cumule (car lj) (nextl lh))
               (cons-moment-cumule (car lj) (car lh)))))
         (setq lh liste-horaires) (nextl lj))
       (reverse lt)))


(defun expanser-dispo-moment
  (dispo-moments liste-horaires-cumules liste-jours)
  (mapcar 
    (lambda (int)
      (list
        (jour-moment-cumule (borne-inf-intervalle int))
        (horaire-moment-cumule (borne-inf-intervalle int))
        (horaire-moment-cumule (borne-sup-intervalle int))))
    (intersection-zone
      (faire-zone-speciale liste-horaires-cumules liste-jours)
        dispo-moments)))       


(defun expanser-dispo-semaine (dispo-semaine debut fin)
  (let ((liste-semaines
    (faire-zone-speciale
      (liste-nombres-inclus debut fin 1 1) '(0))))
        (mapcar
          (lambda (int)
            (list
              0
              (borne-inf-intervalle int)
              (borne-sup-intervalle int)))
          (intersection-zone
            liste-semaines
            dispo-semaine))))
              
          
;;; dans l'autre sens

(defun concatener-i-dispo-moment (i-dispo-moment)
  (let ((liste-intervalles-moments
    (mapcar (lambda (i-moment)
      (let* ((jour (car i-moment))
        (i-inter (cadr i-moment))
        (h1 (car i-inter))
        (h2 (cdr i-inter)))
      (creer-intervalle
        (cons-moment-cumule jour h1)
        (cons-moment-cumule jour h2))))
    i-dispo-moment)))
  (apply 'creer-zone liste-intervalles-moments)))

;;; attention, la derniere semaine est exclue
;;; a cause du creer-intervalle

(defun concatener-i-dispo-semaine (i-dispo-semaine)
  (let ((liste-intervalles-semaines
    (mapcar (lambda (i-moment)
      (let* ((jour (car i-moment))
        (i-inter (cadr i-moment))
        (s1 (car i-inter))
        (s2 (cdr i-inter)))
          (creer-intervalle s1 s2)))
            i-dispo-semaine)))
              (apply 'creer-zone liste-intervalles-semaines)))

(defun concatener-i-dispo (i-dispo-sem i-dispo-mom)
  (cons
    (concatener-i-dispo-moment i-dispo-mom)
    (concatener-i-dispo-semaine i-dispo-sem)))


;;; copie d'un objet structure et partage
;;; CHG FD/MO 15/09/95
;;; ajout de l'arg l-champs-inutiles, liste de champs que l'on ne veut pas
;;; recopies. Par defaut, il auront donc la valeur par defaut ????

(defun circopy-struct (objet . l-champs-inutiles)
  (when
    (structurep objet)
    (let*
      ((type (type-of objet))
	(copie (funcall 'omakeq type)))
      (mapc
	(lambda (champ)
	  (let ((valeur (funcall 'ogetq type champ objet)))
	    (ifn
	      (appartenir-liste champ l-champs-inutiles)
	      (funcall 'oputq type champ copie `',valeur)
	      ;; dans l'ideal, il faudrait peut-etre ici une valeur par defaut
	      )))
	(field-list type))
      copie)))

;;; ajoute et selectionne une fenetre
;;; (over add-and-select-application)

;;; !!! ??? (07/03/95) MO
;;; je ne suis pas sur de cette syntaxe
;;; un add-application est une modification physique et renvoie ?????

(defun add-and-select-application (appli)
  (let 
    ((app (add-application appli)))
     (when app
       (send 'window-state (send 'window app) 'activate)
       ;; (current-keyboard-focus-window (send 'window app))
      app)))
      
;;; grabe et selectionne une application
;;; (over grab-and-select-application)

(defun grab-and-select-application (appli)
  (let ((app (add-and-select-application appli)))
    (when app
      (grab-application app))))

;;; !!!!!
(defun grab-and-select-application (appli)
  (let ((app (add-and-select-application appli)))
    (when app
      (grab-application app))))

;;; un append qui accepte indiferement
;;; des listes ou des atomes.

(defun cons-or-append args              
  (when args
    (if (consp (car args))
      (append (car args) (apply 'cons-or-append (cdr args)))
      (cons (car args) (apply 'cons-or-append (cdr args))))))

;;; FD Mai 1995
;;; a classer au bon endroit dans ce .... de fichier (MO)

;;; permet de tracer une boite autour d'un ensemble de champs et d'ajouter
;;; un titre en haut a gauche
;;; arguments
;;;     hauteur  -> hauteur de la fanetre dans laquelle s'insere le cadre
;;;     largeur  -> largeur de la fenetre
;;;     lignes   -> nombre de lignes de la fenetre
;;;     titre    -> le titre associe au cadre
;;;     liste-champs -> une liste de champs, chaque champ etant une liste 
;;;                     composee de deux liste l1 et l2 
;;;                     format de l1:
;;;                      - nom du champ
;;;                      - son type
;;;                      - attributs (dans l'ordre de def. de l'objet aida)
;;;                     l2 liste de coordonnees (no-ligne, no-colonne,
;;;                      nb-lignes, nb-colonnes)
;;;
(defun encadre (hauteur largeur lignes liste-champs titre li-avt li-apr)
   (let* ((ligne-premier-champ (sub (apply 'min (mapcar 'caadr liste-champs))
                                    li-avt))
          (ligne-dernier-champ (add (apply 'max 
                                    (mapcar (lambda (champ)
                                           (let ((place (cadr champ)))
                                                 (sub1 (add (nth 0 place)
                                                            (nth 2 place)))))
                                           liste-champs))
                                     li-apr))
          (hauteur-ligne (div hauteur lignes))
          (hauteur-cadre 
            (mul hauteur-ligne
              (add (sub ligne-dernier-champ
                        (sub1 ligne-premier-champ))
                   2)))
            (decalage (div hauteur-ligne 2)))
         (translation 
               0
               (sub (mul hauteur-ligne
                         (sub ligne-premier-champ 2))
                    decalage)
               (view
                 titre
                 (box 
                   0
                   0
                   (add largeur 10)
                   (add(add decalage
                         hauteur-cadre)5))
                   ))))
       
       

;;; permet de tracer un cadre comprenant un titre et permettant d'encadrer
;;; une liste de champs (cluster)
;;; arguments
;;;     hauteur  -> hauteur de la fanetre dans laquelle s'insere le cadre
;;;     largeur  -> largeur de la fenetre
;;;     lignes   -> nombre de lignes de la fenetre
;;;     colonnes -> nombre de colonne
;;;     titre    -> le titre associe au cadre
;;;     liste-champs -> une liste de champs, chaque champ etant une liste 
;;;                     composee de deux liste l1 et l2 
;;;                     format de l1:
;;;                      - nom du champ
;;;                      - son type
;;;                      - attributs (dans l'ordre de def. de l'objet aida)
;;;                     l2 liste de coordonnees (no-ligne, no-colonne,
;;;                      nb-lignes, nb-colonnes)
;;;
(defun cadre-avec-titre (hauteur largeur lignes colonnes liste-champs titre)  
   (let*
     ((ligne-premier-champ
       (apply 'min (mapcar 'caadr liste-champs)))
     (ligne-dernier-champ
       (apply 'max (mapcar (lambda (champ)
	 (let ((place (cadr champ)))
	   (sub1 (add (nth 0 place)
	     (nth 2 place)))))
       liste-champs)))
     (premiere-colonne (apply 'min (mapcar (lambda (champ)
       (let ((place (cadr champ)))
	 (nth 1 place)))
     liste-champs)))
     (derniere-colonne (apply 'max 
       (mapcar (lambda (champ)
	 (let ((place (cadr champ)))
	   (add (nth 1 place)
	     (nth 3 place))))
       liste-champs)))
     (hauteur-ligne (div hauteur lignes))
     (largeur-colonne (div largeur colonnes))
     (hauteur-cadre (add (mul hauteur-ligne
       (add (sub ligne-dernier-champ
	 ligne-premier-champ)
       3))
     5))
     (largeur-cadre (mul largeur-colonne
       (sub derniere-colonne
	 premiere-colonne))))
     (translation 
       (mul largeur-colonne
	 (sub1 premiere-colonne))
       (sub (mul hauteur-ligne
	 (sub ligne-premier-champ 2))
       5)
       (cluster 
	 5
	 0 ;; 5
	 largeur-cadre
	 hauteur-cadre
	 titre))))


;;; CRE RM - 13/06/96
;;; Fonction permettant d'encapsuler une liste avec une police de caracteres

(defun encapsuler-font-liste (liste police)
  (mapcar
    (lambda (x)
      (font police x))
    liste))

;;; CRE RM - 13/06/96
;;; Fonction permettant de desencapsuler une liste 

(defun desencapsuler-font-liste (liste)
  (mapcar
    (lambda (x)
      (send 'image x))
    liste))

;;; <encapsuler-chaine> - CRE - FD - 31/07/96
;;; macro permettant d'ancpaulser une chaine dans une police de taille
;;; determiner

(dmd encapsuler-chaine (chaine font)
  `(with ((current-font ,font))
    ,chaine))

;;; routine standard de saisie d'une valeur typee appartenant … une liste
;;; d'intervalle
;;; message est la question a poser a l'utilisateur
;;; fonction-libelle : une fonction qui transforme min et max en libellé
;;; fonction-filtre : une fonction de filtre qui recevra comme argument
;;; la chaine, le min et le max.

(defun demande-valeur-type-intervalles (message liste fonction-libelle fonction-filtre)
  (let 
    ((cont 1)
     (result ()))
    (while 
      (neq cont 0)
      (let*
        ((prompt  
           (catenate
	     message" ? "
	     (liste-intervalles2chaine liste)))
	(val (afficher-demande-valeur prompt () t ())))
        (if 
          val
          (when 
            (not (funcall fonction-filtre val liste))
            (setq result val)
            (setq cont 0))
	  (setq cont 0))))
      result))

;;; <liste-intervalles2chaine> - CRE - FD - 30/07/96
;;; transforme une liste d'intervalle de la forme ((1 . 3)(7 . 40)) en une
;;; chaine de la forme "[1-3][7-40]

(defun liste-intervalles2chaine (liste-intervalles)
  (let ((chaine ""))
    (mapc
      (lambda (intervalle)
	(setq chaine
	  (catenate
	    chaine
	    "["
	    (borne-inf-intervalle intervalle)
	    "-"
	    (- (borne-sup-intervalle intervalle) 1)
	    "]")))
      liste-intervalles)
    chaine))

;;; CRE RM 15/10/96 : type d'un objet

(defun type-objet (objet)
  (selectq (type-of objet)
    ('#:tclass:i-cours  'cours)
    ('#:tclass:i-groupe 'groupe)
    ('#:tclass:i-prof   'prof)
    ('#:tclass:i-salle  'salle)    
    ('#:tclass:i-resa   'resa)
    ('#:tclass:i-annul  'annul)
    ('#:tclass:i-ajout  'ajout)))

;;; CRE RM 18/08/98 : verification complete de l'acces a une donnee.
;;; voir la fonction debut-modification

(defun autoriser-acces-complet (site donnee type)
  (let
    ((data donnee)
     (strdata))
    (ifn (symbolp donnee)
      (setq strdata (send 'objet2string donnee))
      (setq data (eval donnee))
      (setq strdata (string donnee)))
    (ifn (equal (type-of data) '#:tclass:i-cours)
      (funcall 'debut_modif SITE strdata type)
      (if
	(and
	  (every
	    (lambda (p)
	      (if (equal (funcall 'debut_modif SITE (send 'objet2string p) MODIFICATION) -1)
		()
		t))
	    (profs-i-cours data))
	  (every
	    (lambda (f)
	      (if (equal (funcall 'debut_modif SITE (send 'objet2string f) MODIFICATION) -1)
		()
		t))
	    (groupes-i-cours data)))
	(funcall 'debut_modif SITE strdata type)
	ACCES-REFUSE))))
