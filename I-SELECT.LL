;;; --------------------------------------------------------------------------
;;; - I-select                                                  VARUNA 1.2   -
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; - Interface Graphique                                                    -
;;; - Bibliotheque d'editeur : editeur de liste d'objets                     -
;;; --------------------------------------------------------------------------
;;; - Creation:  Mars 93                                                     -
;;; - derniere mise a jour pour version 1.13: Avril 95 FD                    -
;;; - Build 1.22.1 01.07.99 / JH MO FD                                       -
;;; --------------------------------------------------------------------------

;;; - FD - 12/95
;;; - ajout du champ <suppression> qui permet d'indiquer si on utilise une fct
;;; - particuliere pour supprimer un objet
;;; - FD-13/04/95 :modification pour la gestion du focus


;;; cet objet gere les actions de l'objet obmenu 
;;; definition des boutons et des actions associcees

(eval-when (load eval local-compile)
  (defabbrev select {application}:select))

(eval-when (load eval local-compile) 
  (defstruct {select} 
    backup
    accesseur
    accesseur-objet
    obli))

(in-package '{select})

; select renvoie une application compose d'un selectionneur a double entres
; les parametres sont :
; list : la liste des objets a editer
; accesseur, accesseur de l'objet en cours de modif
; acc-id : accesseur de l'identifiant (doit renvoyer un symbole)
; acc-lib : accesseur de libelle (doit renvoyer une chaine)
; func-edition : procedure d'edition (recoit l'objet selectionne en param)
; func-ajout : fonction d'ajout (doit renvoyer l'objet)
; titre : titre de l'editeur


(defun selecteur-objet
  (liste obli accesseur accesseur-objet ident libelle edition ajout-codes supprimer)
  (:initialise-structure (new '{select}) 
    liste obli accesseur accesseur-objet ident libelle edition
    ajout-codes supprimer))


(defun :initialise-structure
  (appli liste obli accesseur accesseur-objet ident libelle edition ajout suppression)
  (with ((current-font FONT-EDITEUR))
    (let*
      ((zone-codes (doublestringmenu liste ident libelle))
	(taille-bouton
	  (max (slength #Mv_modifier) (slength #Mv_ajouter) (slength #Mv_supprimer)))
	(bouton-ajouter
	  (standardbutton
	    (font FONT-EDITEUR (cadrer-centrer #Mv_ajouter taille-bouton))))
	(bouton-retirer
	  (standardbutton
	    (font FONT-EDITEUR (cadrer-centrer #Mv_supprimer taille-bouton))))
	(bouton-editer
	  (standardbutton
	    (font FONT-EDITEUR (cadrer-centrer #Mv_modifier taille-bouton))))
	(view-buttons (elasticrow bouton-ajouter bouton-retirer bouton-editer))
	(view-code (standard-list 0 0 0 (send 'width view-buttons) zone-codes)))
   
      (:backup appli (copylist liste))
      (:accesseur appli accesseur)
      (:accesseur-objet appli accesseur-objet)
      (:obli appli obli)
      ({application}:userdata bouton-ajouter ajout)
      ({application}:userdata bouton-editer edition)
      ({application}:userdata bouton-retirer suppression)

      (set-action bouton-ajouter
	#'(lambda (b)
	  (let
	    ((lobj
	      (when (not LECTURE-SEULE?)
		(funcall ({application}:userdata b)))))
	    (when LECTURE-SEULE? (afficher-attention #Mv_lecture-seule))
	    (and
	      lobj 
	      (post-event 'ascii-event
		({application}:father (component 'appl b))
		0)
	      (post-event 'keyboard-focus-event (component 'appl b) 0)
	      
	      (mapc (lambda (c)
		(send 'o-add-object (component 'codes b) c)) lobj)))))
      
      (set-action bouton-retirer 
	#'(lambda (b)
	  (let*
	    ((menu (component 'codes b))
	      (str (send 'get-selected-string menu))
	      (code (send 'extraire-code menu str)))
	    (if ({application}:userdata b)
	      (when
		(and
		  (tester-acces-objet code)
		  (funcall ({application}:userdata b) code))
		(post-event 'ascii-event
		  ({application}:father (component 'appl b))
		  0)
		(post-event 'keyboard-focus-event (component 'appl b) 0)
		(send 'o-delete-object (component 'codes b)))
	      (post-event 'ascii-event
		({application}:father (component 'appl b))
		0)
	      (post-event 'keyboard-focus-event (component 'appl b) 0)
	      (if
		(or
		  LECTURE-SEULE?
		  (not (tester-acces-objet code)))
		(afficher-attention #Mv_lecture-seule)
		(send 'o-delete-object menu))))))
      
      (set-action bouton-editer  
	#'(lambda (b)
	  (let*
	    ((menu (component 'codes b))
	      (ligne (send 'get-selected-string menu))
	      (code-ob (and ligne (send 'extraire-code menu ligne))))
	    (when
	      (and ligne (tester-acces-objet code-ob))
	      (if MODULE-RESEAU
		(if (eq (debut-modification SITE (eval code-ob) MODIFICATION)
		  ACCES-AUTORISE)
		(progn
		  (post-event 'ascii-event
		    ({application}:father (component 'appl b))
		    0)
		  (post-event 'keyboard-focus-event (component 'appl b) 0)      
		  (funcall ({application}:userdata b) code-ob))
		(afficher-attention
		  (column
		    #Mv_modif-acces-refuse
		    (row #Mv_donnee (string code-objet) #Mv_modifiee)
		    #Mv_autre-utilisateur)))
		(progn
		  (post-event 'ascii-event
		    ({application}:father (component 'appl b))
		    0)
		  (post-event 'keyboard-focus-event (component 'appl b) 0)
		  (funcall ({application}:userdata b) code-ob)))))))
      
      (add-component appli 'user appli)
      (add-component appli 'codes zone-codes)
      (add-component appli 'appl appli)
      (add-component appli 'b-ajout bouton-ajouter)
      (add-component appli 'b-retire bouton-editer)
      (let
	((view (constrainedview view-code view-buttons)))
	(send 'update-constraint view view-code
	  (stretchconstraint 1 1 1 1))
	(send 'update-constraint view view-buttons
	  (stretchconstraint 1 1 -1 1))
	({application}:initialise-structure appli 0 0 0 0
	  view)))))

;;; FD-12/04/95
;;; pas de focus possible sur ce type d'application donc on passe le focus
;;; a la suivante 
(defun :current-keyboard-focus-window (ed)
  (:i-next ed))

;;; FD-02/05/95, on ne donne jamais le controle du clavier a ce type
;;; d'appli
(defun :readonly-flag (ed)
  t)

(defun :i-next (ed)
  (current-keyboard-focus-window
    (send 'window
      (if (send 'i-valider ed)
	(next-user-appli ed 'user)
	ed))))

(defun :i-valider (ed)
  (let*
    ((val (send 'o-get-objects (component 'codes ed)))
      (erreur 
	(and
	  (:obli ed)
	  (null val)
	  (alerte (funcall (:accesseur ed) ())))))
    (not erreur)))

;; sauvegarde en trois etapes :
;; 1. on supprime l'objet courant dans la liste de depart
;; 2. on ajoute l'objet courant dans la nouvelle liste
;; 3. enfin on modifier le champ de l'objet courant
;; ici, le member provoque des erreurs de pile  -> repare
;; on prefere delq a delete car il utilise le predicat eq et non equal

(defun :i-sauver (ed)
  (let
    ((liste-depart (:backup ed))
      (nouvelle-liste (send 'o-get-objects (component 'codes ed)))
      (objet-courant ({application}:userdata (component 'appli ed))))
    (when objet-courant
      (when (:accesseur-objet ed)
	(mapc
	  (lambda (o)
	    (funcall (:accesseur-objet ed) o
	      (delq objet-courant 
		(funcall (:accesseur-objet ed) o))))
	  liste-depart)
	(mapc
	  (lambda (o)
	    (funcall (:accesseur-objet ed) o
	      (cons objet-courant 
		(funcall (:accesseur-objet ed) o))))
	  nouvelle-liste))
      (funcall (:accesseur ed) objet-courant nouvelle-liste))
    t))


;;; pour l'annulation d'un editeur de ce type
;;; on doit
;;; 1 - supprimer l'objet edite (si il existe) dans les nouveaux
;;; 2 - ajouter l'objet edite dans les objets de backup
;;; 3 - modifier le champ de l'objet (i-restore)
;;; les points 1 et 2 n'ont de sens que si 
;;; l'accesseur de deuxieme niveau est defini

(defun :i-restore-objets (ed)
  (when (:accesseur-objet ed)
    (let
      ((les-nouveaux (send 'o-get-objects (component 'codes ed)))
	(objet ({application}:userdata (component 'appli ed)))
	(les-anciens (:backup ed)))
      (mapc
	(lambda (sous-objet)
	  (let
	    ((les-objets (funcall (:accesseur-objet ed) sous-objet)))
	    (when (memq objet les-objets)
	      (funcall (:accesseur-objet ed) sous-objet
		(delete objet les-objets)))))
	les-nouveaux)
      (mapc
	(lambda (sous-objet)
	  (let
	    ((les-objets (funcall (:accesseur-objet ed) sous-objet)))
	    (when (not (memq objet les-objets))
	      (funcall (:accesseur-objet ed) sous-objet
		(cons objet les-objets)))))
	les-anciens))))


;;; plus complique,  il faut aussi traiter les autre objets

(defun :i-restore (ed)
  (let
    ((objet-edite ({application}:userdata (component 'appli ed))))
    (when (and objet-edite (:accesseur ed))
      (:i-restore-objets ed)
      (funcall (:accesseur ed) objet-edite (:backup ed)))))

