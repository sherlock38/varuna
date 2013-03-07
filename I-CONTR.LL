;;; --------------------------------------------------------------------------
;;; - I-Contr                                                   VARUNA 1.2   -
;;; - Interface Graphique                                                    -
;;; - Actions associees au Menu "Contraintes"                                -
;;; --------------------------------------------------------------------------
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; - Creation:  Mars 93                                                     -
;;; - Mise a jour pour version 1.07: Decembre 93                             -
;;; - Build 1.22.1 01.07.99 / JH MO                                          -
;;; --------------------------------------------------------------------------

;;; TRV MO 15/09/95
;;; je travaille sur les primitives du type <defiger...>

;;; MOD FD-02/05/95 les boutons + et - ont ete remplaces par Ajouter/Supprimer

;;; globales pour la gestion de l'editeur contraintes
;;; !!! pourquoi avoir des glob pour un editeur !!!

(defvar lctype ())              
(defvar index 0)


;;; editeur de contraintes de type contrainte non simultaneite avec 
;;; une procédure d'ajout - contrainte n-aire symetrique

(defun con-nsimul (a)
  (contr-type
    #Mv_non-simultaneite
    'poser-non-chevauchement
    #'(lambda ()
      (let ((lc (demandes-i-cours #Mv_pro-nsc2 () ())))
        (when lc
          (if (gt (length lc) 1)
            lc
            (not (afficher-attention #Mv_warn-2cours))))))))

;;; precedence                          

(defun con-preced (a)
  (contr-type 
    #Mv_precedence
    'poser-precedence
    #'(lambda ()
      (let* ((c1 (demande-i-cours #Mv_pro-1co ()))
        (c2 (if c1
          (demande-i-cours
            (catenate
              (string (libelle-i-cours c1))
              #Mv_pro-prec) ())
          ())))
            (cond
              ((not c2) ())
              ((not c1) ())
              ((eq c1 c2)
                (not (afficher-attention
                  (font time8 #Mv_war-ide))))
              (t (list c1 c2)))))))
                  
;;; editeur de contraintes de type simultaneite semaine avec procedure d'ajout

(defun con-memesem (a)
  (contr-type
    #Mv_meme-semaine
    'poser-meme-semaine
    #'(lambda ()
      (let*
	((lc (demandes-i-cours #Mv_i-cours () ())))
	(when lc
          (if (gt (length lc) 1)
            lc
            (not (afficher-attention #Mv_warn-2cours))))))))

;;; proximite de semaine

(defun con-proxsem (a)
  (contr-type
    #Mv_proximite-semaine
    'poser-proximite-semaine
    #'(lambda ()
      (let* ((c1 (demande-i-cours #Mv_pro-1co ()))
        (c2 (if c1
          (demande-i-cours
            (catenate
              (string (libelle-i-cours c1))
              #Mv_pro-sis) ())
          ())))
            (cond
              ((not c1) ())
              ((not c2) ())
              ((eq c1 c2)
                (not (afficher-attention
                  (font time8 #Mv_waride))))
              (t (list c1 c2)))))))


;;; jours differents

(defun con-jourdif (a)
  (contr-type
    #Mv_jours-differents
    'poser-jours-differents
    #'(lambda ()
      (let* ((lc (demandes-i-cours #Mv_i-cours () ())))
        (when lc
          (if (gt (length lc) 1)
            lc
            (not (afficher-attention #Mv_warn-2cours))))))))

;;; l'editeur de contrainte de type type et un prompt
;;; definition de l'image et des actions
;;; MOD FD-11/05/95 remplacement de list-and-scroll par
;;;     liste-avec-vscroller pour resoudre le pb des 
;;;     ascenseurs qui ne fonctionnaient pas

(defun edit-contraintes (type prompt)
  (let* ((zl-liste-contr
          ;;;FD-11/05/95  (stringmenu
             (liste-avec-vscroller
                 47 10 ;;;AJT pour liste-avec-scroller
               (mapcar 'code-libelle
                 (evlis 
                   (car (contraintes-de-type type))))))
         (b-ok (standardbutton "   OK    "))
         (b-annuler (standardbutton #Mv_annuler))
         (taille-bouton 
             (max
                 (slength #Mv_ajouter)
                 (slength #Mv_supprimer)))
         (b-plus-contr (standardbutton ;;;" + ")) FD-02/05/95
                           (cadrer-centrer #Mv_ajouter taille-bouton)))
         (b-moins-contr (standardbutton ;;;" - ")) FD-02/05/95
                            (cadrer-centrer #Mv_supprimer taille-bouton)))
         (b-next (standardbutton " >> "))
         (b-prev (standardbutton " << ")))
             
   (setq lctype (contraintes-de-type type))
   (setq index 0)
            
   (set-action b-next
     #'(lambda (b)
       (setq index (add1 index))
       (send 'set-strings (component 'liste-contr b)
         (mapcar 'code-libelle (evlis (nth index lctype))))
       (gestion-next-prev (component 'appl b))))
             
   (set-action b-prev
     #'(lambda (b)
       (setq index (sub1 index))
       (send 'set-strings (component 'liste-contr b)
         (mapcar 'code-libelle (evlis (nth index lctype))))
       (gestion-next-prev (component 'appl b))))
   
   (set-action b-annuler 
     #'(lambda (b)
       (setq Saisie-Ok ())
       (remove-application (component 'appl b))))
   
   (set-action b-ok
     #'(lambda (b)
       (setq Saisie-Ok t)
       (remove-application (component 'appl b))))

   (mixed-applicationq
     liste-contr (component 'liste zl-liste-contr);;;FD-11/05/95zl-liste-contr
     name 'appl
     plus b-plus-contr
     moins b-moins-contr
     next b-next
     prev b-prev
     image
      (view (rectangle 50 50 400 300)
            (translation 10 10 (font time8b prompt))
            (standard-list-input 20 40 350 150
              #Mv_i-cours
              ;;FD-11/05/95 remplacement de list-and-scroll par
              ;; liste-avec-vscroller pour resoudre le pb des 
              ;; ascenseurs qui ne fonctionnaient pas
              ;;(list-and-scroll zl-liste-contr)) 
              zl-liste-contr)
            (translation 10 230
              (row b-prev b-plus-contr b-moins-contr b-next))
            (translation 50 300 (row b-annuler b-ok))))))


;;; gestion de l'editeur de contraintes
;;; (boutons precedent suivant)

(defun gestion-bouton-next (appli)
  (if (ge index (sub1 (length lctype)))
      (inhibit-application (component 'next appli))
      (authorize-application (component 'next appli))))

(defun gestion-bouton-prev (appli)
  (if (eq index 0)
      (inhibit-application (component 'prev appli))
      (authorize-application (component 'prev appli))))

(defun gestion-next-prev (appli)
  (gestion-bouton-next appli)
  (gestion-bouton-prev appli))

;;; gestion de l'editeur de contraintes

(defun contr-type (prompt type proc-ajout)
  (let ((editeur (edit-contraintes type prompt)))
    (gestion-next-prev editeur)
    
    (set-action (component 'plus editeur)
      (lambda (b)
        (let ((lcl (funcall proc-ajout)))
          (if (member (mapcar 'code-i-cours lcl) lctype)
            (afficher-erreur #Mv_contrainte-existe)
            (when lcl
              (send 'set-strings
                (component 'liste-contr b)
                (mapcar 'code-libelle lcl))
              (setq index 0)
              (setq lctype (cons (mapcar 'code-i-cours lcl) lctype))
              (gestion-next-prev editeur))))))
      
    (set-action (component 'moins editeur)
      (lambda (b)
        (setq lctype (delete (nth index lctype) lctype))
        (setq index 0)
        (gestion-next-prev editeur)
        (send 'set-strings (component 'liste-contr b)
          (mapcar 'code-libelle (evlis (car lctype))))))
    
    (send 'set-title editeur #Mv_con)
    (grab-and-select-application editeur))
  
  (when Saisie-Ok
    (let ((lc-sans-type ()))
      (mapc (lambda (contr)
        (when (not (equal (car contr) type))
          (setq lc-sans-type (cons contr lc-sans-type))))
            Liste-Contraintes)
      (setq lctype (mapcar (lambda (contr)
        (cons type contr))
          lctype))
      (setq Dirty t)
      (setq Liste-Contraintes (append lc-sans-type lctype)))))

;;; liste des contraintes du type type

(defun contraintes-de-type (type)
  (mapcar 'cdr
    (sous-liste (lambda (c) (eq (car c) type)) Liste-Contraintes)))


;;; FIGER/DEFIGER
;;; -------------

;;; l'action de figer un cours est possible des qu'un cours est instancie.
;;; le flag fige-i-cours est alors mis a t est l'instant de ce cours n'est plus
;;; modifiable par une resolution tant que le cours reste fige.
;;; que le cours soit instancie ou non on peut figer la salle, mais cela n'a
;;; pas la meme signification:

;;; primtives dans cette partie
;;;
;;;       <defiger-cours>
;;;       <defiger-salle-cours>
;;;       <figer-cours>
;;;       <figer-salle-cours>

;;; <defiger-cours>
;;; primitive fondamentale inverse de figer-cours

(defun defiger-cours (cours)
  (fige-i-cours cours ()))

;;; <figer-salle-cours>

;;; ne fige la salle du cours cours 
;;; que lorsque ce dernier est place
;;; FD - 18/07/95 - MAJ 
;;; si le cours n'a pas de salle, on fige quand meme sa salle!!!
;;; le champ salle-fige contient alors "Code : "
;;; => il ne faut pas figer la salle si le cours n'a pas de salle

;;; MOD RM - 21/05/96 : lorsque l'on fige les salles, on doit modifier
;;; le champ <salle-figee-reelle> en fonction de <salle-trouvee>

;;; MOD RM - 23/09/98

(defun figer-salle-cours (cours) 
  (when (instant-trouve-i-cours cours)
    (if (not (salle-trouvee-i-cours cours))
      (salle-figee-reelle-i-cours cours ())
      (salle-figee-reelle-i-cours cours
        (code-i-salle (salle-trouvee-i-cours cours))))))
  
;;; fige le cours cours

(defun figer-cours (cours)
  (fige-i-cours cours t))

;;; defige la salle du cours cours
;;; MOD RM 23/09/98

(defun defiger-salle-cours (cours)
  (salle-figee-reelle-i-cours cours ()))

;;; desinstancie le cours cours (instant et salle trouvee)

(defun desinstancier (cours)
  (instant-trouve-i-cours cours ())
  (salle-trouvee-i-cours cours ()))

;; desaffecter la salle du cours (salle trouvee)

(defun desaffecter (cours)
  (salle-trouvee-i-cours cours ()))

;; macro permettant de figer des cours avec une fonction filtre 
;; et l'option salle figee
;; REM : si f vaut nil, on utilise pas sous-liste (economie de liste)

;;; DBG FD - 29/06/95 figeait les cours meme ceux qui n'ont pas d'instant
;;; ce qui fait que si on lancait une resolution ensuite cela plantait

;;; MOD RM - 27/09/96 : modification RESEAU

(dmd figer-les-cours (salle-p filtre)
  `(mapc
    (lambda (cours)
      (if MODULE-RESEAU
	(if (eq (debut-modification SITE cours MODIFICATION) ACCES-AUTORISE)
	  (when (i-instanciep cours)
	    (figer-cours cours)
	    (when ,salle-p (figer-salle-cours cours)))
	  (afficher-attention
	    (column
	      #Mv_modif-acces-refuse
	      (row #Mv_donnee (send 'objet2string cours) #Mv_modifiee)
	      #Mv_autre-utilisateur)))
	(when (i-instanciep cours)
	  (figer-cours cours)
	  (when ,salle-p (figer-salle-cours cours)))))
    ,(if (null filtre)
      `Liste-I-Cours
      `(sous-liste ,filtre Liste-I-Cours))))

;; idem pour defiger
;;; MOD RM - 27/09/96 : modification RESEAU

(dmd defiger-les-cours (salle-p filtre)
  `(mapc
    (lambda (cours)
      (if MODULE-RESEAU
	(if (eq (debut-modification SITE cours MODIFICATION) ACCES-AUTORISE)
	  (progn
	    (defiger-cours cours)
	    (when ,salle-p (defiger-salle-cours cours)))
	  (afficher-attention
	    (column
	      #Mv_modif-acces-refuse
	      (row #Mv_donnee (send 'objet2string cours) #Mv_modifiee)
	      #Mv_autre-utilisateur)))
	(defiger-cours cours)
	(when ,salle-p (defiger-salle-cours cours))))
    ,(if (null filtre)
      `Liste-I-Cours
      `(sous-liste ,filtre Liste-I-Cours))))
  
;;; macro permettant de desinstancier des cours
;;; MOD RM - 27/09/96 : modification RESEAU

(dmd desinstancier-les-cours (filtre)
  `(mapc
    (lambda (cours)
      (if MODULE-RESEAU
	(if (eq (debut-modification SITE cours MODIFICATION) ACCES-AUTORISE)
	  (desinstancier cours)
	  (afficher-attention
	    (column
	      #Mv_modif-acces-refuse
	      (row #Mv_donnee (send 'objet2string cours) #Mv_modifiee)
	      #Mv_autre-utilisateur)))
	(desinstancier cours)))
    ,(if (null filtre)
      `Liste-I-Cours
      `(sous-liste ,filtre Liste-I-Cours))))

;;; macro permettant denlever les salles non figees
;;; CRE RM - 05/12/96

(dmd desaffecter-les-salles (filtre)
  `(mapc
    (lambda (cours)
      (if MODULE-RESEAU
	(if (eq (debut-modification SITE cours MODIFICATION) ACCES-AUTORISE)
	  (desaffecter cours)
	  (afficher-attention
	    (column
	      #Mv_modif-acces-refuse
	      (row #Mv_donnee (send 'objet2string cours) #Mv_modifiee)
	      #Mv_autre-utilisateur)))
	(desaffecter cours)))
    ,(if (null filtre)
      `Liste-I-Cours
      `(sous-liste ,filtre Liste-I-Cours))))


;;; interface utilisateur - figer des groupe
;;; a est l'appli a redessiner
;;; gere les sous-groupes et les salles
;;; MOD RM - 27/09/96 : reecriture de la fonction

(defun figer-groupes (a)
  (let*
    ((groupes (demandes-i-groupe #Mv_i-groupes () ()))
     (fige-salles-p
       (when groupes (demande-figer-salle () t)))
     (groupes-a-figer ()))
    (when groupes
      (if (not (any 'groupes-i-groupe groupes))
	(setq groupes-a-figer groupes)
	(ifn (prompt-oui-non "Voulez-vous aussi figer les sous-familles ?")
	  (setq groupes-a-figer groupes)
	  (setq groupes-a-figer
	    (append groupes (liste-tous-les-sous-groupes groupes)))))
      (when groupes-a-figer
	(change-curseur a Varuna-Curseur-Sablier)
	(figer-les-cours
	  fige-salles-p
	  (lambda (cours)
	    (and
	    (donnee-modifiable-p cours)
	    (inter-non-vide-p (groupes-i-cours cours) groupes-a-figer))))
	(change-curseur a Varuna-Curseur-Fleche)
	(setq DIRTY t)
	(aff-refresh a)))))

;;; interface utilisateur - figer des salle
;;; on fige les cours qui ont lieu dans une salle donnee et les salles
;;; DBG (01/06/95) MO - bug fd3105
;;; on ne redemande pas si on veut figer les salles aprŠs en avoir deja fait
;;; la demande
;;; rem (01/06/95) MO
;;; je ne pense pas que cela soit trŠs clair, est-ce les cours ou les salles
;;;  (ou eventuellement les deux -> cas ici) que l'on veut figer
;;; il faut alors que le libelle de la demande soit tres clair sur l'operation
;;; que cela va faire -> ce n'est pas le cas pour le moment 

(defun figer-salles (a)
  (let
    ((salles (demandes-i-salle #Mv_i-salles () ())))
    (when salles
      (change-curseur a Varuna-Curseur-Sablier)
      (figer-les-cours
	t
	(lambda (cours)
	  (and
	    (donnee-modifiable-p cours)
	    (memq (salle-trouvee-i-cours cours) salles))))
      (change-curseur a Varuna-Curseur-Fleche)
      (setq DIRTY t)
      (aff-refresh a))))

;;; interface utilisateur - figer tous les cours

(defun figer-tout (a)
  (let
    ((fige-salles-p (demande-figer-salle () t)))
    (change-curseur a Varuna-Curseur-Sablier)    
    (figer-les-cours fige-salles-p donnee-modifiable-p)
    (change-curseur a Varuna-Curseur-Fleche)
    (setq DIRTY t)
    (aff-refresh a)))

;;; fonction d'aiguillage pour l'option figer
;;; fonction appelee par le menu avec l'appli hote du menu
;;; en param a passer a tout ce qui suit

(defun con-fig (a)
  (let
    ((figer-quoi (demande-choix v_i-groupes v_i-salles v_tout)))
    (cond
      ((equal figer-quoi #Mv_i-groupes) (figer-groupes a))
      ((equal figer-quoi #Mv_i-salles) (figer-salles a))
      ((equal figer-quoi #Mv_tout) (figer-tout a))
      (t ()))))


;;; AJT RM - 30/06/95
;;; possibilite de defiger les cours instancies sans salles

(defun con-defig (a)
  (let
    ((figer-quoi (demande-choix v_i-groupes v_i-cour-ss-salle v_tout)))
    (cond
      ((equal figer-quoi #Mv_i-groupes) (defiger-groupes a))
      ((equal figer-quoi #Mv_i-cour-ss-salle) (defiger-cour-ss-salle a))
      ((equal figer-quoi #Mv_tout) (defiger-tout a))
      (t ()))))

;;; CRE RM - 27/09/96 : defiger des familles

(defun defiger-groupes (a)
  (let*
    ((groupes (demandes-i-groupe #Mv_i-groupes () ()))
     (defige-salles-p
       (when groupes (prompt-oui-non #Mv_defiger-toutes-les-salles)))
     (groupes-a-defiger ()))
  (when groupes
    (if (not (any 'groupes-i-groupe groupes))
      (setq groupes-a-defiger groupes)
      (ifn (prompt-oui-non "Voulez-vous aussi defiger les sous-familles ?")
	(setq groupes-a-defiger groupes)
	(setq groupes-a-defiger
	  (append groupes (liste-tous-les-sous-groupes groupes)))))
    (when groupes-a-defiger
      (change-curseur a Varuna-Curseur-Sablier)      
      (defiger-les-cours
        defige-salles-p
        (lambda (cours)
	  (and
	    (donnee-modifiable-p cours)
	    (inter-non-vide-p (groupes-i-cours cours) groupes-a-defiger))))
      (change-curseur a Varuna-Curseur-Fleche)
      (setq DIRTY t)
      (aff-refresh a)))))

;;; defiger les cours sans salle

(defun defiger-cour-ss-salle (a)
  (if
    (every (lambda (c) (not (fige-i-cours c))) Liste-I-Cours)
    (afficher-attention #Mv_aucun-cours-figes)
    (let
      ((confirm-cours
	(prompt-oui-non #Mv_defiger-cours-ss-salle)))
      (when confirm-cours
	(change-curseur a Varuna-Curseur-Sablier)	
	(defiger-les-cours () i-instancie-sans-salle-modifiable-p)
	(change-curseur a Varuna-Curseur-Fleche)
	(setq DIRTY t)
	(aff-refresh a)))))

;;; defiger tous les cours
;;; MOD RM 23/09/98 : salle-figee -> salle-figee-reelle

(defun defiger-tout (a)
  (if
    (every (lambda (c) (not (fige-i-cours c))) Liste-I-Cours)
    (afficher-attention #Mv_aucun-cours-figes)
    (let*
      ((confirm-cours
	(prompt-oui-non #Mv_defiger-tous-les-cours))
       (confirm-salle 
	 (when
	   (and
	     confirm-cours
	     (any 'salle-figee-reelle-i-cours Liste-I-Cours))
	   (prompt-oui-non #Mv_defiger-toutes-les-salles))))
      (when confirm-cours
	(change-curseur a Varuna-Curseur-Sablier)	
	(defiger-les-cours confirm-salle donnee-modifiable-p)
	(change-curseur a Varuna-Curseur-Fleche)
	(setq DIRTY t)
	(aff-refresh a)))))

;;; supprime les contraintes associees a un cours

(defun supprimer-contraintes-cours (code-i-cours)
  (let ((nouvelles-contraintes ()))
    (mapc
      (lambda (contrainte)
        (if (member code-i-cours (cdr contrainte))
          (let ((nouveaux-cours (delete code-i-cours (cdr contrainte))))
            (when (gt (length nouveaux-cours) 1)
              (newl nouvelles-contraintes 
                (cons (car contrainte) nouveaux-cours))))
          (newl nouvelles-contraintes contrainte)))
      Liste-Contraintes)
    (setq Liste-Contraintes nouvelles-contraintes)))


;;; place un cours dans la grille
;;; seuls les cours non instancies sont proposes (si il y en a)

;;; rem (24/05/95) MO !!!
;;; le <rendre-visibilite> affiche toutes les familles du cours -> specif ???
;;; rem (30/05/95) MO !!!
;;; et l'affectation de la salle, elle est ou -> on peut l'avoir par le
;;; resultat de l'‚valuation de <deplacement-possible>

;;; MOD RM - 050995 : Reecriture de cette fonction et integration
;;; du traitement reseau
;;; AJT - FD - 19/09/95 - la salle n'etait pas affectee !!!
;;;                       on recherchait une salle pour le cours avec
;;;                       construction d'erreur si on n'en trouvait pas mais
;;;                       le champs salle-trouvee restait () dans tous les cas

;;; MOD RM 09/02/96 : on factorise cette fonction pour eviter la duplication
;;; de code

(defun con-placer (appli)
  (let
    ((cours
      (demande-filtree-i-cours
        #Mv_i-cours
        #Mv_pas-cours-non-instancies
        () 'i-pas-instancie-modifiable-p)))
    (when cours
      (if MODULE-RESEAU
        (if (eq (debut-modification SITE cours MODIFICATION) ACCES-REFUSE)
          (afficher-attention
            (column
              #Mv_modif-acces-refuse
              (row #Mv_donnee (send 'objet2string inst) #Mv_modifiee)
              #Mv_autre-utilisateur))
          (placer-un-cours appli cours 'confirmer-placement-cours ()))
        (placer-un-cours appli cours 'confirmer-placement-cours ())))))

;;; CRE RM 04/04/97 : deplacement d'un cours

(defun con-deplacer (appli)
  (let
    ((cours
      (demande-filtree-i-cours
        #Mv_i-cours
        #Mv_aucun-cours-modifiable
        () 'donnee-modifiable-p)))
    (when cours
      (if MODULE-RESEAU
        (if (eq (debut-modification SITE cours MODIFICATION) ACCES-REFUSE)
          (afficher-attention
            (column
              #Mv_modif-acces-refuse
              (row #Mv_donnee (send 'objet2string inst) #Mv_modifiee)
              #Mv_autre-utilisateur))
          (placer-un-cours appli cours 'confirmer-placement-cours ()))
        (placer-un-cours appli cours 'confirmer-placement-cours ())))))

;;; MOD RM - 14/02/96 : ajout d'une fonction de confirmation
;;; MOD FD - 16/02/96 : si la semaine est passee en parametre nous ne la 
;;; demandons pas
;;; MOD RM - 14/08/96 : placement d'un cours en tenant compte des contraintes

(defun placer-un-cours (appli cours fonct-confirm semaine-fixee)   
  (let
    ((mom)
     (semaine semaine-fixee)
     (instant)
     (place)
     (salle ())
     (confirm ())
     (mouvement-impossible)
     (mouvement-ok)
     (continuerp t))
    
    (setq mom
      (when cours (demande-moment-cumule (type-i-cours cours))))
    (when (not semaine)
      (setq semaine
	(when mom (demande-semaine #Mv_semaine Semaine-Debut Semaine-Fin))))
    (setq instant
      (when (and semaine mom) (cons-instant-cumule semaine mom)))
    (setq place
      (when instant
	(setq instant (instant-cumule2instant-interne instant))
	(setq instant (choisir-instant-deplacement cours instant))))
    
    (setq salle
      (when place
	(change-curseur appli Varuna-Curseur-Sablier)
	(nouveau-deplacement-possible cours place)))

    (when (and MODULE-SALLES (equal salle t))
      (setq continuerp
	(prompt-continuer
	  (column
	    #Mv_continuer-deplacement
	    #Mv_lever-contrainte-salle))))

    (setq mouvement-impossible *erreurs*)
    (setq mouvement-ok
      (or
	(and mouvement-impossible (not (traiter-erreur)))
	(and
	  (not mouvement-impossible)
	  salle
	  continuerp)))

    (when mouvement-ok
      (when
	(or
          (not fonct-confirm)
          (setq confirm
            (funcall fonct-confirm cours place salle)))
	(setq DIRTY t)
        (instant-trouve-i-cours cours place)
	(if (appartenir-liste salle LISTE-I-SALLE)
	  (progn
	    (salle-trouvee-i-cours cours salle)
	    (when (salle-figee-reelle-i-cours cours)
	      (salle-figee-reelle-i-cours cours salle)))
	  (salle-trouvee-i-cours cours ()))
	
        (figer-cours cours)
	;;MOD FD - 19/02/96 (rendre-visibilite appli cours)
	;; remplacer par maj-affichage 
	;; AJT - FD - 19/09/95 - mise a jour de l'affichage
	;; pourquoi une liste
	(when (appartenir-liste salle LISTE-I-SALLE)
	  (maj-affichage 'salle (list salle) ()))))
    (change-curseur appli Varuna-Curseur-Fleche)
    confirm))

(defun confirmer-placement-cours (cours instant salle)
  (if (appartenir-liste salle liste-i-salle)
    (afficher-confirmation
      (column
	#Mv_placer-le-cours	
	(row
	  (string (libelle-i-cours cours))
	  " (" (string (code-i-cours cours)) ")")
	(row #Mv_info-le (libelle-instant instant))
	#Mv_deplacer-avec-la-salle
	(libelle-i-salle salle)))
    (afficher-confirmation
      (column
	#Mv_placer-le-cours
	(row
	  (string (libelle-i-cours cours))
	  " (" (string (code-i-cours cours)) ")")
	(row #Mv_info-le (libelle-instant instant))))))

;;; retire (desinstancie) les cours qui ne sont pas figes

(defun con-desinstancier (a)
  (change-curseur a Varuna-Curseur-Sablier)  
  (desinstancier-les-cours i-pas-fige-modifiable-p)
  (change-curseur a Varuna-Curseur-Fleche)
  (setq DIRTY t)
  (delete-erreur)
  (aff-refresh a))

;;; retire (desaffecte) les salles non figees

(defun con-desaffecter (a)
  (change-curseur a Varuna-Curseur-Sablier)  
  (desaffecter-les-salles i-salle-pas-fige-modifiable-p)
  (change-curseur a Varuna-Curseur-Fleche)
  (setq DIRTY t)
  (delete-erreur)
  (aff-refresh a))

;;; resolution partielle de groupes

;;; MOD FD 29/06/95
;;; MOD RM - 14/05/96 : avertissement en cas d'impossibilite

(defun resoudre-groupes (sallesp)
  (let
    ((groupes (demandes-i-groupe #Mv_i-groupes () ())))
    (cond
      ((not Liste-i-Groupe)
	(afficher-erreur #Mv_resolution-imp-groupe))	
      (groupes
	(setq compter-contraintes-p ())
	(edt-run
	  Gen-App
	  (mapcan 'tous-les-i-cours-fils groupes) t sallesp))
      (t ()))))


;;; resolution partielle de cours
;;; MOD RM - 14/05/96 : avertissement en cas d'impossibilite

(defun resoudre-cours (sallesp)
  (let 
    ((cours (demandes-i-cours #Mv_i-cours () () )))
    (cond
      ((not Liste-i-Cours)
	(afficher-erreur #Mv_resolution-imp-cours))	
      (cours
	(setq compter-contraintes-p ())	
	(edt-run
	  Gen-App
	  (mapcar 'code-i-cours cours) t sallesp))
      (t ()))))

;;; resolution partielle a partir d'une semaine

; (defun resoudre-semaine-figee ()
;   (nyi "La résolution partielle à partir d'une semaine"))

;;; resolution partielle par types de cours

(defun resoudre-type-cours (sallesp)
  (let* 
    ((tc (demande-i-type-cours #Mv_type-cours ()))
     (code-tc (when tc (code-i-type-cours tc)))
     (l-codes 
         (mapcar 
             'code-i-cours
             (sous-liste
                 (lambda (c code) 
                   (eq (type-i-cours c) code))
                 Liste-I-Cours code-tc)))) 
    (when l-codes
      (setq compter-contraintes-p ())
      (edt-run Gen-App l-codes t sallesp))))

;;; interface utilisateur - resolution partielle
;;; propose le choix entre groupes, cours, types de cours, et semaine

;;;  REM - FD 27/10/95 - il faut tester si la resolution est possible avant
;;;        de laisser choisir les groupes, cours, ... sur lesquels on souhaite
;;;         lancer la resolution

;;; MOD RM - 06/12/96 : resolution partielle avec ou sans salle

(defun con-resoudre (sallesp)
  (let
    ((resoudre-quoi))
    (if (and MODULE-RESEAU (not (resolution-p)))
      (afficher-attention #Mv_resolution-impossible)
      (setq resoudre-quoi
	(demande-choix v_i-groupe v_i-cours v_type-cours)))
    (cond
      ((equal resoudre-quoi #Mv_i-groupe)
	(resoudre-groupes sallesp))
      ((equal resoudre-quoi #Mv_i-cours)
	(resoudre-cours sallesp))
      ((equal resoudre-quoi #Mv_type-cours)
	(resoudre-type-cours sallesp)))))

(defun resol-partielle-avec-salles (a)
  (con-resoudre t))

(defun resol-partielle-sans-salles (a)
  (con-resoudre ()))

