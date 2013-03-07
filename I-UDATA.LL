;;; --------------------------------------------------------------------------
;;; - I-udata.ll                                                VARUNA 1.2   -
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; - Le mode saisie                                                         -
;;; - Définition des applications de la base de donees                       -
;;; - des actions d'edition                                                  -
;;; --------------------------------------------------------------------------
;;; - Creation:  Juillet 94                                                  -
;;; - Derniere mise a jour pour version 1.13: Mai 1995                       -
;;; - Build 1.22.1 01.07.99 / JH                                             -
;;; --------------------------------------------------------------------------

;;; FD-13/05/95 ajout d'un editeur <hmedit>


;;; 1. La base de donnee
;;; --------------------
;;; macro data-base-application
;;; construit une application avec 
;;; le titre <titre>
;;; une barre de menu <menubar> a definir dans l'appel de la macro
;;; des boutons radio pour chaque concept passe dans liste concept
;;; un hyperselectionneur pour filtrer les libelles de chaque concept 
;;; Les 4 boutons modifier/supprimer/ajouter/copier
;;; l'action des boutons radio provoque le reafffichage
;;; du listing des objets par libelle
;;; l'action des 4 boutons add/edit/new/del provoque l'appel
;;; a une fonction <edition-concept> (cette fonction peut etre generee avec
;;; la macro actions-edition -> voir plus bas)
;;; avec deux arguments argument
;;; 1- une paire dont le car est un symbole correspondant a l'action choisie
;;; par l'utilisateur (ajouter/copier/modifier/supprimer)
;;; et dont le cdr est le libelle selectionne dans le selectionneur
;;; 2- le selectionneur a mettre a jour apres edition


;;; construit un selectionneur (hyper) pour la liste liste
;;; et une chaine de car prompt
;;; le resultat renvoye est une liste dont le premier element
;;; est l'action (ajout/suppression/modification)
;;; le deuxieme est l'element selectione sous forme de chaine

(defun mod-action (appli action selection liste)
  (cond
    ((and LECTURE-SEULE? (neq action 'modifier)) ;; MOD - FD - 22/02/96 MultiP
      (afficher-attention #Mv_lecture-seule))
    (t
      (funcall
	({application}:userdata appli)
	(cons action selection)
	liste))))

(defun get-item (liste)
  ;; chg (29/05/95) mo: ai remplace #50 par #51 pour que le simplestring
  ;; ai la meme taille que le selecteur avec ascenseur -> !! peut etre mieux
  (let*
    ((edit
      (with ((current-font FONT-EDITEUR))
	(simplestringedit 0 0 #51wchar)))
     (b-mod
       (standardbutton
	 (font FONT-EDITEUR 
	   (cadrer-centrer #Mv_editer 10))))
     (b-del
       (standardbutton
	 (font FONT-EDITEUR
	   (cadrer-centrer #Mv_supprimer 10))))
     (b-nou
       (standardbutton
	 (font FONT-EDITEUR
        ;;FD-02/05/95 v_creer remplace v_nouveau
	   (cadrer-centrer #Mv_creer 10)))) 
     (b-dup
       (standardbutton
	 (font FONT-EDITEUR
	   (cadrer-centrer #Mv_dupliquer 10))))
      
     (select
       (with ((current-font FONT-EDITEUR))
	 (hyperselector 0 0 (mul #wchar 50) 200 liste)))

     (scroller
        (verticalscrollbar
          0 0 (add 3 {scroller}:scrollbarwidth)
          (send 'h select)
          0 (max 1 (length liste)) 0 1 0)))
    
    (send 'link-vscrollbar select scroller)
    (send 'link-filter select edit)             
    
    (set-action b-mod (lambda (b)
      (let
        ((selection (send 'get-selected-string (component 'liste b))))
        (when selection
          (mod-action
            (component 'get b)
            'modifier
            selection
            (component 'liste b))))))
    
    (set-action b-nou (lambda (b) 
      (mod-action
        (component 'get b)
        'ajouter
        (send 'get-line (component 'selection b))
        (component 'liste b))))
    
    (set-action b-del (lambda (b)
      (let
        ((selection (send 'get-selected-string (component 'liste b))))
        (when selection
          (mod-action
            (component 'get b)
            'supprimer
            selection
            (component 'liste b))))))
    
    (set-action b-dup (lambda (b)
      (let
        ((selection (send 'get-selected-string (component 'liste b))))
        (when selection
          (mod-action
            (component 'get b)
            'dupliquer
            selection
            (component 'liste b))))))
    
    (set-action edit
      #'(lambda (e)
        (let
          ((selection (send 'get-line e))
            (chaines (send 'get-filtered-strings (component 'liste e))))
          (when (equal 1 (length chaines))
            (send 'set-line e (car chaines))
            (setq selection (car chaines)))
          (if (member selection chaines)
            (mod-action e)
            (current-keyboard-focus-window (send 'window  e))))))

    ;; l'application
    (setq appli
      (mixed-applicationq 
        modifier b-mod
        liste select
        filtre edit
        image  
        (view 
          (rectangle 0 0 350 350)
          (boxedimage 40 50 (add 2 (add (send 'w select)
            (send 'w scroller)))
          (add 2 (send 'h select)) (row scroller select))
          (translation 40 250
            (column (rectangle 0 0 0 (div #1wchar 2))
              (boxedimage 0 0
                (add 10 (send 'width edit))
                (add 2 (send 'height edit)) edit)))
          (translation 40 300
            (row
              (translation 10 0 b-mod) 
              (translation 10 0 b-del)
              (translation 10 0 b-nou)
              (translation 10 0 b-dup))))
        selection edit                    
        modifier b-mod
        supprimer b-del
        nouveau b-nou
        dupliquer b-dup
        name 'get))
    appli))


(defvar Last-Selection ())

(dmd data-base-application (titre-appli menubar thermo . liste-concepts)
  `(let* (
    ;;  l'hyperselectionneur
    (select (get-item ()))              
    ;; les boutons radio
    (b1
      (radiobutton 0 0 120 20
	(font FONT-EDITEUR
	  (get-message ',(concat 'v_ (car liste-concepts))))))
    (panel-buttons (selectorapplication 
      0 0 120 ,(mul 20 (length liste-concepts))
      (elasticcolumn
        b1
        .,(mapcar
          (lambda (concept)
            `(radiobutton
              0 0 120 20
	      (font FONT-EDITEUR
		(get-message ',(concat 'v_ concept)))))
          (cdr liste-concepts))))))

  ;; actions des boutons
  (set-action panel-buttons
    (lambda (p)
      (let* ((select (send 'get-selection p))
        (image-selectionnee
          (if select (send 'title select) Last-Selection))
        (appli-select (component 'type p))
        (hyper-sel (component 'liste appli-select)))
      (setq Last-Selection image-selectionnee)
      (send 'set-line (component 'filtre appli-select) "")
      (cond 
        .,(mapcar 
          (lambda (concept)
            `((equal image-selectionnee
	      (font FONT-EDITEUR
		(get-message ',(concat 'v_ concept))))
            ({application}:userdata appli-select
              ',(concat 'edition- concept))
            (send 'set-strings hyper-sel
              (supprimer-chaines-multiples
		(sortl (mapcar ',(concat 'libelle- concept)
		  ,(listing concept)))))))
          liste-concepts)))))
  (setq Last-Selection
    (font FONT-EDITEUR (get-message ',(concat 'v_ (car liste-concepts)))))

  ;; l'appli
  (let
    ((appli
      (mixed-applicationq
        radio panel-buttons
        type select
        concepts liste-concepts
        image 
        (view
          (box 0 0 (bitxmax) (bitymax))
          (column 
            ,menubar
            (translation 100 20 ,titre-appli)
            (row
              (translation 20 50 panel-buttons)
              select)
            (rectangle 0 0 627 30)    
            (row
              (rectangle 0 0 105 15)
              (boxedview ,thermo)
              (translation 70 1 
                (font time8 #Mv_Bande-varuna))))))))
    
    (send 'initialise appli) 
; update appli's components list now
; otherwise, the next instruction
; is lost
  (send 'set-selection panel-buttons b1)
  appli)))



;;; cette fonction met a jour la liste de l'hyperselectionneur
;;; en appelant l'action du composant 'radio de l'application select
;;; (un selectionneur) Ensuite, elle demande au selectionneur
;;; de selectionner le libelle (l'action du bouton 'radio perd la
;;; selection en cours)

(defun mise-a-jour-listing (select libelle)
  (let
    ((action-bouton
      ({application}:action
	(component 'radio select))))
    (when (and action-bouton libelle)
      (funcall action-bouton (component 'radio select))
      (when libelle
	(send 'set-selected-strings select (list libelle))
	(send 'ensure-visible select libelle)))))




;;; 2. Utilitaires pour les objets
;;; ----------------------------------
;;; <copier-structure-varuna>
;;; CRE - FD - 15/09/95
;;; fonction retourne une copie d'un objet structure et ne copiant pas
;;; certains champs de cette structure en fonction du type de l'objet
;;; MOD - FD - 07/05/96 - ajout de fige et salle-figee dans les champs qui ne
;;; sont pas a copier

(defun copier-structure-varuna (objet)
  (let ((type-objet (type-of objet)))
    (selectq
      type-objet
      ('#:tclass:i-cours
	;; on ne copie pas l'instant du cours et la salle trouvee
	(circopy-struct objet
	  'instant-trouve 'salle-trouvee 'fige
	  'salle-figee 'salle-figee-reelle))
      (t
	;; dans les autre cas on copie tous
	(circopy-struct objet)))))

;;; 3. Les editeur
;;;----------------

;;; 3.1 Definition d'un "generateur" d'editeur de concepts (classe)


;;; une classe contient des objets types
;;; types -> standards (entiers, chaines, symboles, listes...)
;;; dans la version actuelle tous les types sont consideres comme standards
;;; 
;;; a chaque type correspond un objet aida, fonction type2objet
;;; cet objet aida defini toutes les actions de l'objet
;;; a chaque objet aida correspond une methode d'image (l'affichage quoi)
;;; c'est cette fonction qui est retournee par la  fonction objet2image

;;; les deux tables de correspondances
;;; entre <type>-<objet aida>-<methode d'affichage>

;;; !!essayer separer types standards de types utilisateurs??

(defun type2objet (type)
  (selectq type
    
    ;; types standard
    ('symbole 'sedit)
    ('chaine  'vedit)
    ('horaire 'hedit)
    ('duree   'hedit)
    ('entier  'iedit)
    ('enum    'enum)
    ('bool    'bool)
    ('liste   'ledit)
    ('listefd1 'hmedit) ;;;;;FD 23/03/95
    ('texte   'tedit)
    
    ;; type utilisateur
    ('disponibilite   'i-dispo)
    ('liste-i-prof   'selecteur-i-prof)
    ('liste-i-groupe 'selecteur-i-groupe)
    ('liste-i-cours  'selecteur-i-cours)
    ('liste-i-type-cours 'selecteur-i-type-cours)
    ('liste-i-salle 'selecteur-i-salle)))  ;;; ajout FD 21/03/95


;;; routines d'affichage pour chaque type d'objet

(defun objet2image (objet)
  (selectq objet
    ('vedit 'standard-input)
    ('sedit 'standard-input)
    ('hedit 'standard-input)
    ('iedit 'standard-input)
    ('enum  'standard-input)
    ('bool  'standard-input)
    ('ledit 'standard-list-input)
    ('hmedit 'standard-list-input) ;;;; FD 23/03/95
    ('tedit 'standard-rect)
    ('selecteur-i-prof   'standard-list-input)
    ('selecteur-i-groupe 'standard-list-input)
    ('selecteur-i-cours  'standard-list-input)
    ('selecteur-i-type-cours 'standard-list-input)
    ('i-dispo 'standard-box)
    ('selecteur-i-salle 'standard-list-input)))  ;;; ajout FD 21/03/95
      

;;;
;;; macro de construction d'une grille de saisie
;;; pour une classe d'objets
;;; classe est un objet <ct-class> ou <tclass>
;;; le parametre nom permet de nommer de facon specifique la fonction generee
;;; le nom de la fonction generee est
;;; fenetre-classe-<nom-editeur>
;;; ex : fenetre-cours-standard
;;; controle est un nom de fonction de controle d'integrite global
;;; (appelee lorsque l'utilisateur clique sur le bouton OK de l'editeur)
;;; cette fonction doit informer l'utilisateur des eventuelles erreurs,
;;; effectuer ce qu'elle a a faire (par exemple d'autres actions d'edition)
;;; et renvoyer un booleen.
;;; la grille est decoupee en cases : 
;;; lignes est le nombre de lignes de la grille
;;; nombre de colones de la grille 
;;; largeur et hauteur : la largeur et la hauteur de la fenetre
;;;
;;; l est une liste qui contient deux listes l1 et l2
;;; format de l1
;;;     -> nom du champ
;;;     -> son type <passer directement le type d'objet aida permet 
;;;                  de rester general ?>
;;;     -> attributs dans l'ordre dans lequel ils
;;;        apparaissent dans la definition de l'objet aida correspondant.
;;; l2 est une liste de cordonees <no-grille,no-colonne,nb-lignes,nb-colones>
;;;     
;;; ne pas oublier d'exporter la fonction generee dans le module 
;;; d'appel de la macro

;;; MOD RM - 23/04/96 : ajout d'un parametre <creationp> permettant
;;; d'afficher dans la barre de titre CREATION ou MODIFICATION


(dmd def-editeur (classe nom controle lignes colones largeur hauteur . l)
  `(defun ,(concat 'fenetre- classe '- nom) (inst creationp)

;;; construit la liste des zones
;;; en appelant pour chaque type l'objet aida correpondant
;;; un objet aida prend a au moins deux paramettres
;;; 1 - la valeur initiale
;;; 2 - un accesseur sur l'objet en cours d'edition
;;; par convention, l'accesseur est constitue par concatenation
;;; du champ et de la classe (ex code-i-cours)
    
    (let 
      ((appli
        (let* (
        (zone-boutons (ok-annuler-editeur ',controle))
        . ,(mapcar
        (lambda (paire)
          (let* ((x (car paire))
            (champ (car x))
            (accesseur (concat champ '- classe))
            (valeur (list accesseur 'inst)))
          (list (concat 'z- (car x))
            (cons
              (type2objet (cadr x)) 
              (cons valeur (cons `',accesseur (cddr x)))))))    
        l))
      
;;; constuction de l'image de l'appli et de ses composants graphiques
      
      ,(cons 'mixed-applicationq 
        
        (append
;;; les zones definies ci-dessus sont ajoutees comme constituant 
;;; de l'image de l'application car on defini en meme temps
;;; un accesseur sur ces zone <par la fonction component>
          
          (mapcan
            (lambda (paire)
              (let ((x (car paire)))
                (list
                  (car x)                 ;;nom du composant
                  (concat 'z- (car x))))) ;; la zone definie dans le let
            l)
;;; l'image de l'appli
;;; pour chaque zone (objet aida), on ivoque la methode d'affichage
;;; definie pour l'objet en question (fonction objet2image)
;;; on ajoute egalement ici les boutons ok et annuler
;;; et un rectangle englobant toutes les zonnes
          
          (list
            'name `'appli
            'image
            `(view
              (rectangle 0 0 ,(add 10 largeur) ,(add 10 hauteur))
              (translation
                10
                (sub ,hauteur (send 'height zone-boutons))
                zone-boutons)
              
;;; on calcule les coordonnees de l'objet
;;; en fonction des cellules passes dans l'appel de la macro (place)
              
              
              .,(mapcar
                (lambda (paires)
                  (let* ((taille-ligne (div hauteur lignes))
                    (taille-colone (div largeur colones))
                    (champ (caar paires))
		    (chaine (caddr paires))
                    (objet-aida (type2objet (cadar paires)))
                    (place (cadr paires))
                    (ligne (nth 0 place))
                    (colone (nth 1 place))
                    (nblig (nth 2 place))
                    (nbcol (nth 3 place))
                    (x (add 10 (mul (sub1 colone) taille-colone)))
                    (y (mul (sub1 ligne) taille-ligne))
                    (w (sub (mul nbcol taille-colone) 10))
                    (h (mul nblig taille-ligne)))
                  `(,(objet2image objet-aida) ,x ,y ,w ,h
		      (if ,chaine
			(get-message ',(concat 'v_ chaine))
			(get-message ',(concat 'v_ classe '- champ)))
                    ,(concat 'z- champ))))
                l))))))))
;;; il ne reste qu'a ajouter le titre
;;; et mettre garder un pointeur sur l'objet en cours d'edition
;;; dans le champ {application}:userdata
;;; qui pourra etre consulte et modifie par chaque zone de l'appli
    
;;; XXX Bug 3 il faut ici que quand le nom est autre que standard ou owner
;;; ce ne soit pas la classe seule mais la classe et le nom qui genere le
;;; titre
;;; MOD - FD - 13/09/95  
      (send 'set-title appli
	(catenate
	  (get-message ',(concat 'v_ 'titre-saisie- 
	    (if 
	      (and
		(neq nom 'standard)
		(neq nom 'owner))
	      (concat classe '- nom) 
	      classe)))
	  (if LECTURE-SEULE?
	    #Mv_titre-lecture-seule
	    (if creationp #Mv_titre-creation #Mv_titre-modification))))

;;;     (send 'set-title appli (get-message ',(concat 'v_ 'titre-saisie- classe)))
    ({application}:userdata appli inst)
    appli)))



;;; 3.1 generations des routines d'edition
;;;



;;; la macro actions-edition genere les principales actions
;;; d'edition d'un objet
;;; nom est le nom de l'editeur standard pour les objets
;;; de la classe classe <nom passe dans le def-editeur>

;;; cette macro supporte des routines personnalises pour
;;; des objets specifiques (pour varuna : les groupes)
;;; pour chaque ection edition/ajout/suppression/copie
;;; on peut passer la fonction a appeler par la routine
;;; de gestion d'edition (fonction edition-classe appellee)
;;; par le gestionnaire de liste.
;;; dans ce cas, c'est la fonction passee en parametre de la
;;; macro qui est appelee (cette fonction peut aussi utiliser les
;;; routines standard generees par cette macro)

;;; les fonctions generees par cette macro sont
;;; editer-classe, ajouter-classe, supprimer-classe, copier-classe
;;; edition-classe
;;; ne pas oublier de les exporter dans le module d'appel de la macro
  
  
(dmd actions-edition (classe nom edition ajout suppression copie)  
  `(progn    
    
;;; routine d'edition standard : appel de l'editeur editeur-concept
;;; gere le listing
;;; et l'affichage de la liste dans l'application data-base
;;; cette fonction renvoie le libelle de l'objet edite
;;; ou null si l'utilisateur a annule l'edition

;;; MOD RM - 22/04/96 : ajout d'un parametre <creationp> permettant de
;;; savoir si l'objet passe a l'editeur est en phase de creation ou
;;; de modification    
    
    (defun ,(concat 'editer- classe) (instance creationp)
      (let* ((fenetre
        (or
          (,(concat 'editeur- classe) instance)
          ',(concat 'fenetre- classe '- nom)))
      (appli (grab-and-select-application
	(funcall fenetre instance creationp)))
      (nouvelle-instance ({application}:userdata appli)))
      ;; MOD RM - 22/05/96 : prise en compte du reseau
      (when
	(and
	  (not MODULE-RESEAU)
	  (not nouvelle-instance)
	  creationp)
	(setq ,(concat 'compteur- classe)
	  (sub ,(concat 'compteur- classe) 1))
	(ecrire-fichier-codes))
      (when nouvelle-instance
        (when (not (appartenir-liste nouvelle-instance
          ,(listing classe)))
        (newl ,(listing classe) nouvelle-instance)) 
        (,(concat 'libelle- classe) nouvelle-instance))))
    
;;; routine d'ajout
;;; creee l'objet et appelle l'edition standard
;;; MOD RM - 29/08/95 : incrementation du compteur de code
    
    (defun ,(concat 'ajouter- classe) ()
      (let ((inst (,(concat 'nouveau- classe))))
        (when inst
          (,(concat 'code- classe) inst (,(concat 'gencode- classe)))
	  (when MODULE-RESEAU
	    (debut-modification SITE inst CREATION))
	  (,(concat 'editer- classe) inst t))))
    
;;;!!reecrire delq!!
;;; routine de suppression
;;; gere le listing
;;; !!probleme de genre avec le messages de confirmation
    
    (defun ,(concat 'supprimer- classe) (inst confirm)
      (when
        (if confirm
          (confirmer-supprimer (,(concat 'libelle- classe) inst) 'genre)
          t)
        (setq ,(listing classe) (delq inst ,(listing classe)))
        ;; DBG (22/05/95) MO: on supprime le groupe de l'affichage
        ;; ou la salle si il y a lieu
        ;; on confond les etats et les actions
        ;; !! on devrait faire une maj-constantes-globales a cet endroit
        ;; DBG - FD - 21/06/95 , les donnees sont modifiees donc
        ;; il faut modifier DIRTY
        (setq DIRTY t)
        (setq Liste-Groupes-Aff (delq inst Liste-Groupes-Aff))
        (setq Liste-Salles-Aff (delq inst Liste-Salles-Aff))
	;; AJT RM - 04/12/95 : Affichage des profs
	(setq Liste-Profs-Aff (delq inst Liste-Profs-Aff))
        (makunbound (,(concat 'code- classe) inst))
         t ))

;;; routins de copie
;;; creee une copie de l'objet et appelle la routine d'edition
;;; FD - 15/09/95
;;; rem: copie aussi l'instant d'un cours si celui-ci est instancie
;;; DBG : utilisation de la fonction copier-structure-varuna
    
    (defun ,(concat 'copier- classe) (inst)
      (let
	((copie (copier-structure-varuna inst)))

        (when
	  copie
	  ;; /// !!! ///
	  ;; REM FD/MO 15/09/95 au bon endroit: idem module-reseau
	  ;; il faut me mettre en amont au niveau de l'appel a cette prim
          ;; DBG - FD - 21/06/95 , les donnees sont modifiees donc
          ;; il faut modifier DIRTY
          (setq DIRTY t)
          (,(concat 'code- classe) copie (,(concat 'gencode- classe)))

	  (if MODULE-RESEAU
	    (debut-modification SITE copie CREATION))
	  (,(concat 'index- classe) copie ())
          (,(concat 'editer- classe) copie t))))
    
;;; Branch...
;;; c'est la routine qui est invoquee dans les actions des boutons
;;; de l'application data-base
    
;;; AJT RM - 23/08/95 : tests reseau pour la modification de donnees
    
    (defun ,(concat 'edition- classe) (message liste)
      (let* ((action (car message))
        (libelle-instance (cdr message))
        (les-instances 
          (,(concat 'libelle- classe '2 'codes- classe)
            libelle-instance))
        (inst (reponses-multiples
          les-instances 
          ',(concat 'code- classe)
          (column (get-message 'v_plusieurs-reponses)
            libelle-instance)
          ',(concat 'code- classe) ())))

      (cond
	((not (tester-acces-objet inst))
	  (traiter-erreur))

	((and (eq action 'modifier) inst)
	  (if MODULE-RESEAU
	    (if (eq (debut-modification SITE inst MODIFICATION)
	      ACCES-AUTORISE)
	    ,(or
	      (and edition (list edition 'inst ()))
	      (list (concat 'editer- classe) 'inst ()))
	    (afficher-attention
	      (column
		#Mv_modif-acces-refuse
		(row #Mv_donnee (send 'objet2string inst) #Mv_modifiee)
		#Mv_autre-utilisateur)))
	    ,(or
	      (and edition (list edition 'inst ()))
	      (list (concat 'editer- classe) 'inst ()))))

	((and (eq action 'dupliquer) inst)
	  (if MODULE-RESEAU
	    (if (eq (debut-modification SITE inst MODIFICATION)
	      ACCES-AUTORISE)
	    ,(or
	      (and copie (list copie 'inst))
	      (list (concat 'copier- classe) 'inst))
	    (afficher-attention
	      (column
		#Mv_modif-acces-refuse
		(row #Mv_donnee (send 'objet2string inst) #Mv_modifiee)
		#Mv_autre-utilisateur)))
	    ,(or
	      (and copie (list copie 'inst))
	      (list (concat 'copier- classe) 'inst))))
	    
	((and (eq action 'supprimer) inst)
	  (if MODULE-RESEAU
	    (if (eq (debut-modification SITE inst SUPPRESSION)
	      ACCES-AUTORISE)
	    ,(or
	      (and suppression (list suppression 'inst t))
	      (list (concat 'supprimer- classe) 'inst t))
	    (afficher-attention
	      (column
		#Mv_sup-acces-refuse
		(row #Mv_donnee (send 'objet2string inst) #Mv_modifiee)
		#Mv_autre-utilisateur)))
	    ,(or
	      (and suppression (list suppression 'inst t))
	      (list (concat 'supprimer- classe) 'inst t))))

	((eq action 'ajouter)
	  ,(or
            (and ajout (list ajout))
            (list (concat 'ajouter- classe)))))
	(mise-a-jour-listing liste libelle-instance)))))

;;; FD - 21/06/95 
;;; predicat indiquant si c'est une famille qui genere des cours
;;; automatiquement
(defun famille-generatrice-p (groupe)
  (let 
    ((type (type-macro-i-groupe groupe)))

    (or
        (equal type #Mv_groupe-enseignement)
 	;; MOD RM - 22/04/96 : test de l'alternance
	(equal type #Mv_groupe-alternance)
        (equal type #Mv_groupe-td-a)
	(equal type #Mv_groupe-td-b)
	;; compatibilite avec la version avt 19/01/96
	(equal type #Mancien-v_groupe-td-a)
	(equal type #Mancien-v_groupe-td-b))))

;;; FD - 21/06/95
;;; predicat permettant de savoir si un cours est issu d'une famille
;;; generatrice ou non

(defun cours-genere-p (cours)
  (any 'famille-generatrice-p (groupes-i-cours cours)))
    
(defun cours-non-genere-p (cours)
  (not (cours-genere-p cours)))

;;;  MODULE-RESA
;;;  definition de l'editeur pour le module de reservation

(dmd def-editeur-resa 
  (classe nom controle fct-recherche 
   champs-lignes-a-effacer champs-listes-a-effacer
   lignes colones largeur hauteur l l-rech l-reserv)
  `(defun ,(concat 'fenetre- classe '- nom) (inst)

;;; construit la liste des zonnes
;;; en appelant pour chaque type l'objet aida correpondant
;;; un objet aida prend a au moins deux paramettres
;;; 1 - la valeur initiale
;;; 2 - un accesseur sur l'objet en cours d'edition
;;; par convention, l'accesseur est constitue par concatenation
;;; du champ et de la classe (ex code-i-cours)
    
    (let ((appli-demande)
          (appli-recherche)
          (appli-reserve)
          (appli
      (let* (
        (bt-rechercher (bouton-rechercher ',fct-recherche))
        (zone-boutons (ok-annuler-reservation ',controle))
       . ,(mapcar

        (lambda (paire)
          (let* ((x (car paire))
            (champ (car x))
            (accesseur (concat champ '- classe))
            (valeur (list accesseur 'inst)))
          (list (concat 'z- (car x))
            (cons
              (type2objet (cadr x)) 
              (cons valeur (cons `',accesseur (cddr x)))))))    
        (append (cdadr l)(cdaddr l) (cdr l-rech)(cdr l-reserv))))

;;; construction de l'image de l'appli-demande
        (setq appli-demande
         ,(cons 'mixed-applicationq
            (list
             'name `'appli-demande
             'image
             `(view 
                  ;;; FD !!! cdadr !!! cdaddr !!!! CRRRR...
                  ,(cadre-avec-titre hauteur largeur lignes colones (cdadr l) (caadr l))
                  ,(cadre-avec-titre hauteur largeur lignes colones (cdaddr l) (caaddr l))
                  ,(encadre hauteur largeur lignes  
                       (append (cdadr l) (cdaddr l))
                       (car l);;;"DEMANDE"
                       1 1)
                  (translation
                      (sub ,largeur 80)
                      150
                      bt-rechercher)
                  .,(mapcar
                        (lambda (paires)
                          (let* ((taille-ligne (div hauteur lignes))
                                 (taille-colone (div largeur colones))
                                 (champ (caar paires))
                                 (objet-aida (type2objet (cadar paires)))
                                 (place (cadr paires))
                                 (ligne (nth 0 place))
                                 (colone (nth 1 place))
                                 (nblig (nth 2 place))
                                 (nbcol (nth 3 place))
                                 (x (add 10 (mul (sub1 colone) taille-colone)))
                                 (y (mul (sub1 ligne) taille-ligne))
                                 (w (sub (mul nbcol taille-colone) 10))
                                 (h (mul nblig taille-ligne)))
                            `(,(objet2image objet-aida) ,x ,y ,w ,h
                                (get-message ',(concat 'v_ classe '- champ))
                                ,(concat 'z- champ))))
                        (append (cdadr l)(cdaddr l)))))))
;;;        (add-application appli-demande)
;;; construction de l'image de l'appli-recherche
        (setq appli-recherche
         ,(cons 'mixed-applicationq
            (list
             'name `'appli-demande
             'image
             `(view 
                  ,(encadre hauteur largeur lignes  
                       (cdr l-rech) 
                       (car l-rech);;; "RECHERCHE"
                       0 1)
                  .,(mapcar
                        (lambda (paires)
                          (let* ((taille-ligne (div hauteur lignes))
                                 (taille-colone (div largeur colones))
                                 (champ (caar paires))
                                 (objet-aida (type2objet (cadar paires)))
                                 (place (cadr paires))
                                 (ligne (nth 0 place))
                                 (colone (nth 1 place))
                                 (nblig (nth 2 place))
                                 (nbcol (nth 3 place))
                                 (x (add 10 (mul (sub1 colone) taille-colone)))
                                 (y (mul (sub1 ligne) taille-ligne))
                                 (w (sub (mul nbcol taille-colone) 10))
                                 (h (mul nblig taille-ligne)))
                            `(,(objet2image objet-aida) ,x ,y ,w ,h
                                (get-message ',(concat 'v_ classe '- champ))
                                ,(concat 'z- champ))))
                        (cdr l-rech))))))
;;;        (add-application appli-recherche)

;;; construction de l'image de l'appli-reserve
        (setq appli-reserve
         ,(cons 'mixed-applicationq
            (list
             'name `'appli-reserve
             'image
             `(view 
                  ,(encadre hauteur largeur lignes  
                       (cdr l-reserv) 
                       (car l-reserv);;; "RESERVATION"
                       0 0)
                  (translation
                      (sub ,largeur 80)
                       ,(div hauteur lignes);;20
                      zone-boutons)
                  .,(mapcar
                        (lambda (paires)
                          (let* ((taille-ligne (div hauteur lignes))
                                 (taille-colone (div largeur colones))
                                 (champ (caar paires))
                                 (objet-aida (type2objet (cadar paires)))
                                 (place (cadr paires))
                                 (ligne (nth 0 place))
                                 (colone (nth 1 place))
                                 (nblig (nth 2 place))
                                 (nbcol (nth 3 place))
                                 (x (add 10 (mul (sub1 colone) taille-colone)))
                                 (y (mul (sub1 ligne) taille-ligne))
                                 (w (sub (mul nbcol taille-colone) 10))
                                 (h (mul nblig taille-ligne)))
                            `(,(objet2image objet-aida) ,x ,y ,w ,h
                                (get-message ',(concat 'v_ classe '- champ))
                                ,(concat 'z- champ))))
                        (cdr l-reserv))))))

        ({application}:event-action appli-demande 'ascii-event
            (lambda (appl ev)(effacer-recherche appl ev)))
        ({application}:event-action appli-recherche 'down-event
            (lambda (appl ev)()))
        ({application}:event-action appli-reserve 'down-event
            (lambda (appl ev)()))

;;; constuction de l'image de l'appli et de ses composants graphiques
      
      ,(cons 'mixed-applicationq 
        
        (append
;;; les zonnes definies ci-dessus sont ajoutees comme constituant 
;;; de l'image de l'application car on defini en meme temps
;;; un accesseur sur ces zone <par la fonction component>
          
          (mapcan
            (lambda (paire)
              (let ((x (car paire)))
                (list
                  (car x)                 ;;nom du composant
                  (concat 'z- (car x))))) ;; la zone definie dans le let
            (append (cdadr l)(cdaddr l)(cdr l-rech)(cdr l-reserv)))
;;; l'image de l'appli
;;; pour chaque zone (objet aida), on ivoque la methode d'affichage
;;; definie pour l'objet en question (fonction objet2image)
;;; on ajoute egalement ici les boutons ok et annuler
;;; et un rectangle englobant toutes les zonnes
          
          (list
            'demande 'appli-demande
            'recherche 'appli-recherche
            'reservation 'appli-reserve
            'lignes-a-effacer `,champs-lignes-a-effacer
            'listes-a-effacer `,champs-listes-a-effacer
            'name `'appli
            'image
            `(column
                 appli-demande
                 appli-recherche
                 appli-reserve
                 (rectangle 0 0 ,largeur #1hch);; laisser une ligne en bas
                     )))))))

;;; il ne reste qu'a ajouter le titre
;;; et mettre garder un pointeur sur l'objet en cours d'edition
;;; dans le champ {application}:userdata
;;; qui pourra etre consulte et modifie par chaque zone de l'appli
    
    (send 'set-title appli (get-message ',(concat 'v_ 'titre-saisie- classe)))
    ({application}:userdata appli inst)
    appli)))


;;; bouton permettant d'effectuer la reservation ou de l'annuler

(defun ok-annuler-reservation (fonct)
  (let 
    ((liste-actions-ok  (list  'sauver-editeur fonct))
     (liste-actions-annuler (list 'annuler-editeur)))
    (boutons-groupes #1wchar ()      ;;; () pour bt verticaux
        (list "OK" liste-actions-ok)
        (list "Annuler" liste-actions-annuler))))



;;; efface la recherche precedente i.d. la liste des salles dispo
;;; et la salle choisie

(defun effacer-recherche (a ev)
;  (setq fd a)
;  (setq franck (component 'lignes-a-effacer a))
  ;; effacecement des champs de type chaine, entier, horaire, ...
  (mapc 
      #'(lambda (ligne)
          (send 'set-line (component ligne a) ""))
      (component 'lignes-a-effacer a))
  ;;; efface les listes 
  (mapc
      #'(lambda (liste)
          (send 
              'i-set-values 
              (component liste a)
              () () ()))
      (component 'listes-a-effacer a)))

;; avant le 25/07/95
;(defun effacer-recherche (a ev)
;  (setq fd a)
;  (send 'set-line (component 'salle a) "")
;  (send 'i-set-values 
;      (component 'salles-disponibles a) 
;      () () ())
;  (send 'set-line (component 'heure-resa a) ""))

