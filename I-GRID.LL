;;; --------------------------------------------------------------------------
;;; - I-Grid.ll                                                 VARUNA 1.2   -
;;; - Interface Graphique                                                    -
;;; - Routines de gestions des grilles et rectangles                         -
;;; --------------------------------------------------------------------------
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; - Creation:  Juin 93                                                     -
;;; - Mise a jour pour version 1.07:  Décembre 93                            -
;;; - Build 1.22.1 01.07.99 / JH MO                                          -
;;; --------------------------------------------------------------------------

;;; Routines de gestion des grilles et rectangles
;;; 
;;; Il y a deux points d'entree differents selon que nous sommes 
;;; resolution ou non, a savoir:
;;;   - maybe-show-cours
;;;   - afficher-cours-en-resolution
;;;
;;; ces primitives aiguillent l'affichage selon la demande (annuelle/hebdo
;;; complete/partielle, famille/salle) vers les primitives adequates:
;;;
;;;    - affichage annuel -> show-cours-year
;;; 
;;;    - affichage hebdo  
;;;            - affichage famille -> show-cours-week-groupe
;;;            - affichage salle   -> show-cours-week-salle     
;;;


;;; DBG (22/03/95) MO - show-cours-week-salle -  bug report12.v95-1 
;;; DBG (09/03/95) MO - show-cours-year


;;; point d'entree des routines d'affichage des rectangle de cours.
;;; l'affichage ne s'effectue que dans certaines conditions (cours instancie,
;;; groupe d'affichage etc...

(defun maybe-show-cours (cours)
  (when (cours-affichable-p cours)
    (show-cours (component 'grid Gen-App) cours)))

;;; meme fonction, mais celle-ci permet de traiter les evenements et 
;;; les erreurs en attente (process-pending-events)
;;; typiquement, elle est appellee en cours de resolution (d'ou le nom)

(defun afficher-cours-en-resolution (code-cours instant)
  (let ((cours (eval code-cours)))
    (instant-trouve-i-cours cours instant)
    (maybe-show-cours cours)
    (interface-erreur)
    (process-pending-events)))

      
;;; affiche le cours dans la fenetre principale
;;; suivant le type de la grille: annuelle, hebdo salle ou hebdo groupe
;;; MOD RM - 04/12/95 : Affichage des profs

(defun show-cours (appli cours)
  (if (eq Grid-Type 'year)
    (show-cours-year appli cours)
    (cond
      ((and Liste-Groupes-Aff t)
	(show-cours-week-groupe appli cours))
      ((and Liste-Salles-Aff t)
	(show-cours-week-salle appli cours))
      (t
	(show-cours-week-prof appli cours)))))

;;; Affiche les cours figes

(defun afficher-cours-figes ()
  (mapc
    (lambda (cours)
      (and
        (fige-i-cours cours)
        (cours-affichable-p cours)
        (show-cours (component 'grid Gen-App) cours)))
    Liste-I-Cours))

;;; affichage d'un cours dans un display de type planing
;;; on sait que le cours doit e^tre affiche'

(defun show-cours-year (appli cours)
  (let* 
    ((instant (instant-trouve-i-cours cours))
     (horaire-debut (interne2cumule (horaire-instant instant)))
     (horaire-fin (add horaire-debut (duree-i-cours cours)))
     (index-debut (plus-proche-index horaire-debut I-Heures))
     (index-fin (plus-proche-index horaire-fin I-Heures))
     (sem-debut (semaine-instant instant))
     (sem-fin (add sem-debut (nb-seances-i-cours cours)))
     (col-fin (sub (add1 Semaine-Fin-Aff) Semaine-Debut-Aff))
     (col1 (max 0 (sub sem-debut Semaine-Debut-Aff)))
     (col2 (min col-fin (sub sem-fin Semaine-Debut-Aff))))
    (add-cours-but
      appli
      (couleur-de-salle (type-salle-i-cours cours))
      cours
      (numero-dans-liste (jour-instant instant) Jours-Aff)
      col1
      col2
      index-debut
      (or index-fin (sub1 (length i-heures)))
      0 1 1)))                        ; pas d'affichage multi en planning


;;; REM (29/09/94) MO
;;; TOUTE LA PARTIE SUIVANTE EST A REVOIR, J'AI IMPLEMENTE LE VIEUX AFFICHAGE 
;;; DES SALLES

;;; affichage d'un cours dans un display de type semaine
;;; avec rectangles tronques suivant les groupes du cours
;;; et les groupes d'affichage

(defun show-cours-week-groupe (appli cours)
  (let* 
    ((instant (instant-trouve-i-cours cours))
     (horaire-debut (interne2cumule (horaire-instant instant)))
     (horaire-fin (add horaire-debut (duree-i-cours cours)))
     (index-debut (plus-proche-index horaire-debut I-Heures))
     (index-fin (plus-proche-index horaire-fin I-Heures))
     (valeur))
    (if Aff-Avec-Chevauchement
      (add-cours-but
	appli
	(couleur-de-salle (type-salle-i-cours cours))
	cours
	(numero-dans-liste (jour-instant instant) Jours-Aff)
	index-debut
	index-fin
	0 1 0 1 1)
      (setq valeur (valeur-max-cours-chevauchement cours Liste-Cours-Aff))
      (add-cours-but
	appli
	(couleur-de-salle (type-salle-i-cours cours))
	cours
	(numero-dans-liste (jour-instant instant) Jours-Aff)
	index-debut
	index-fin
	0 1
	(min
	  (numero-dans-liste (code-i-cours cours)
	    (liste-cours-chevauchement cours ()))
	  (- valeur 1))
	1
	valeur))))

;;; affichage d'un cours dans un display de type semaine et salle

;;; DBG (22/03/95) MO -  bug report12.v95-1 - show-cours-week-salle 
;;; il faut que le numero de grille soit le numero du jour dans la liste
;;; des jours affichables et non le simple numero du jour, ce qui revient
;;; au numero du jour dans les jours ouvrables
;;; pourquoi show-cours prend un numero de grille comme argument et non un
;;; jour -> !!!! une barriere d'abstraction violee ???

(defun show-cours-week-salle (appli cours)
  (let* 
    ((instant (instant-trouve-i-cours cours))
      (horaire-debut (interne2cumule (horaire-instant instant)))
      (horaire-fin (add horaire-debut (duree-i-cours cours)))
      (index-debut (plus-proche-index horaire-debut I-Heures))
      (index-fin (plus-proche-index horaire-fin I-Heures))
      (salle-aff (car liste-salles-aff)))     
      (add-cours-but
        appli
        (couleur-de-salle (type-salle-i-cours cours))
        cours
        (numero-dans-liste (jour-instant instant) Jours-Aff)
        index-debut
        index-fin
        0 1 0 1 1)))

;;; CRE RM - 04/12/95
;;; Affichage des profs (affichage hebdo)

(defun show-cours-week-prof (appli cours)
  (let* 
    ((instant (instant-trouve-i-cours cours))
      (horaire-debut (interne2cumule (horaire-instant instant)))
      (horaire-fin (add horaire-debut (duree-i-cours cours)))
      (index-debut (plus-proche-index horaire-debut I-Heures))
      (index-fin (plus-proche-index horaire-fin I-Heures))
      (prof-aff (car liste-profs-aff)))     
      (add-cours-but
        appli
        (couleur-de-salle (type-salle-i-cours cours))
        cours
        (numero-dans-liste (jour-instant instant) Jours-Aff)
        index-debut
        index-fin
        0 1 0 1 1)))

;;; addition d'un bouton de cours dans l'appli la
;;; couleur est liee au type de salle,
;;; utilise l'objet chaine pour rendre visible le maximum
;;; d'info sur le cours

;;; AJT 28/06/95 MO
;;; pour l'application impression ajout des horaires de debut et de fin
;;; DBG - FD - 18/09/95
;;; instant n'etait pas initialise !!!!
;;;   instant -> (instant-i-cours cours)

(defun image-rectangle-cours (cours info)
  (let
    ((police (police-cours cours))
     (horaire-debut
       (interne2cumule (horaire-instant (instant-trouve-i-cours cours))))
     (horaire-fin (add (duree-i-cours cours) horaire-debut)))

    (ifn info
      (view (translation 1 1 (libelle-i-cours cours)))
      (column
        (chaine 0 0 1 police 2 (libelle-i-cours cours))
        (chaine 0 0 1 (police-salle-cours cours) 1
          (if (salle-trouvee-i-cours cours)
            (libelle-i-salle (salle-trouvee-i-cours cours))
            ""))
        (chaine 0 0 1 police 1
          (liste-libelles-i-profs (profs-i-cours cours)))
        (if 
          impressionp
          (chaine 0 0 1 police 1
            (catenate
              #Mv_info-de
              (funcall 'cumule2naturel horaire-debut)
              #Mv_info-a
              (funcall 'cumule2naturel horaire-fin)))
          "")))))

;;; application associee a un rectangle de cours
;;; le type de l'application depend du contexte
;;; l'appli est deplacable uniquement lorsqu'on est pas en
;;; cours de resolution.

(defun appli-rectangle-cours (cours)
  (let*
    ((image (image-rectangle-cours cours Week-Aff))
    (the-box (box 0 0 (send 'width image) (send 'height image)))
    (the-view (elasticview image the-box))
    (image-appli (if Week-Aff the-view image)))
  (send 'fit-to-contents
    (ifn I-Running
      (moveableapplication image-appli)
      (application '{application} 0 0
        (send 'width image-appli)
        (send 'height image-appli)
        image-appli)))))

;;; ajout d'une appli de cours de couleur color, associee au cours cours,
;;; positionne en no grille jour, de col1 a col2, de lig1 a lig2,
;;; si ce rectangle est divise en total-sublig sous ligne. L'appli
;;; est ajoute a la position sublig est sa hauteur est calculee en fonction
;;; de n-sublig.

(defun add-cours-but
  (appli color cours jour col1 col2 lig1 lig2 sublig n-sublig total-sublig)
  (let* 
    ((local-appli (appli-rectangle-cours cours)))
    (widget-set-args local-appli
      'background color  
      'font (police-cours cours))
    (add-component local-appli 'le-cours cours)
    (set-action local-appli 'deplacement-rectangle)
    ({application}:event-action local-appli
      'drag-event
      (lambda (appli event)
	(traitement-chevauchement (component 'le-cours appli))))	
    (send
      'v-add-application
      appli
      local-appli
      (code-i-cours cours)
      (cons-rectangle jour col1 col2 lig1 lig2)
      sublig
      n-sublig
      total-sublig
      Week-Aff)))

          

;;; mise a jour du titre (composant titre) de la grille

(defun update-grid-titles ()
  (maj-mon-titre 
    (component 'titre Gen-App)
    (texte-titre-gen-app)))


;;; !!! (02/05/95) MO
;;; c'est tres crad, il faudrait vraiment mettre des variables pour les
;;; libelles

;;;  <texte-mon-titre> renvoie une chaine
 
(defun texte-titre-gen-app ()
  (let
    ((composante1)(composante2)(composante3))
    (setq composante1 #Mv_Affichage) 
    (setq composante2
      (if
        week-aff
	;; MOD - FD - 29/07/96 semaines calendaires
        (catenate #Mv_aff-semnum ESP (ecrire-numero-semaine week-aff))
        #Mv_aff-pla))
    (setq composante3
      (or 
        (and
          Liste-Groupes-Aff
          (catenate #Mv_aff-gro1 ESP
        ;; MOD RM - 07/12/95 : libelle du 1er groupe choisi      
	    (libelle-i-groupe (car Liste-Groupes-Aff))))
        ;; AJT RM - 04/12/95
        (and
          Liste-Profs-Aff
          (catenate #Mv_aff-pro1 ESP
            (libelle-i-prof (car Liste-Profs-Aff))))
        (and
          Liste-Salles-Aff
          (catenate #Mv_aff-sal1 ESP
            (libelle-i-salle (car Liste-Salles-Aff))))))
    (catenate composante1 ESP composante2 ESP composante3)))


;;; construit la grille puis la "colle" dans l'image du composant grid
;;; de l'application grille

(defun add-grid (grille)
  (let* ((new-gr-im (if (eq Grid-Type 'year) (year-grid) (week-grid))))
    ({v-application}:v-image grille new-gr-im)
    (send 'set-image grille (elasticview new-gr-im))
    ))
    
;;; gestion du deplacement des rectangles de cours
;;; sauvegarde les coordonnees d'origine du rectangle (en x et y)
;;; dans le champ user-data de l'appli
;;; met a jour la fonction appellee lorsque l'utilisateur 
;;; lache la souris (au moyen de la fonction event-action)

(defun deplacement-rectangle (appli)
  (let
    ((first-x (send 'x (send 'window appli)))
     (first-y (send 'y (send 'window appli)))
     (cours (component 'le-cours appli)))

    (when (not ({application}:userdata appli))
      ({application}:userdata appli (list first-x first-y)))
    (if
      (or
	(donnee-lecture-seule-p cours t)  ; LECTURE-SEULE? 27/02/96 MultiP
	(not (tester-types-cours cours))) ;; AJT - FD - 02/04/96
      ({application}:event-action appli 'up-event 'deplacement-interdit)
      ({application}:event-action appli 'up-event 'fin-deplacement))))

;;; replace le cours cours (retire le rectangle, puis le rajoute)

(defun replacer-rectangle-cours (cours)
  (send 'v-remove-application
    (component 'grid Gen-App)
    (code-i-cours cours))
  (show-cours (component 'grid Gen-App) cours))

;;; renvoie l'instant associe a rect avec une semaine d'affichage sem

(defun nouvel-instant (rect sem)
  (let* ((no-grille (grille-rect rect))
    (jo (jour-grille no-grille))
    (col1 (col1-rect rect))
    (lc1 (ligne1-rect rect)))
  (when jo
    (if Week-Aff
      (cons-instant
        sem
        (cons-moment jo (cumule2interne (nth col1 I-Heures))))
      (cons-instant
        sem
        (cons-moment jo (cumule2interne (nth lc1 I-Heures))))))))

;;; fonction appellee en fin de deplacement
;;; verifie que le nouvel instant est ok
;;; demande confirmation du deplacement
;;; puis appelle replacer-rect (puisque le rectangle n'est 
;;; 'jamais' place exactement dans la grille)

;;; chg (26/05/95) mo
;;; si new-instant = old-instant alors on replace sans controle

;;; rem (26/05/95) MO
;;; pour moi, ces primitives sont illisibles et absolument pas lispienne
;;; affectation de variables temporaires et construction de listes pour rien
;;; modularite absente et imbrication de primitives sans visualisation claire
;;; des couches:
;;; grille / rectangle graphique / appli rectangle-cours avec actions
;;; conversion entre les notions de temps / coordonnees-grille / coord. fenetre


;;; !!! rem (25/06/95) MO
;;; on s"etait convenu avec jerome sur 'rectangle' pour l'interface, 'rect'
;;; pour le moteur -> dommage

;;; MOD RM - 05/09/95 : traitement reseau pour le deplacement d'un cours

(defun fin-deplacement (appli event)
  (let*
    ((grille (send 'v-image (component 'grid Gen-App)))
     (cours (component 'le-cours appli))
     (old-x (car ({application}:userdata appli)))
     (old-y (cadr ({application}:userdata appli)))
     (new-x (send 'x (send 'window appli)))
     (new-y (send 'y (send 'window appli)))
     (new-rect (send 'coordonnees2rectangle grille new-x new-y))
     (sem1 (semaine-instant (instant-trouve-i-cours cours)))
     (ofset
       (if Week-Aff
	 sem1
	 (if (lt sem1 Semaine-Debut-Aff)
	   (add sem1 (col1-rect new-rect))
	   (add1 (col1-rect new-rect)))))
     (new-instant (nouvel-instant new-rect ofset))
     (la-salle))

    (if (eq new-instant (instant-trouve-i-cours cours))
      (progn
	(if MODULE-RESEAU
	  (if (eq (debut-modification SITE cours MODIFICATION) ACCES-REFUSE)
	    (afficher-attention
	      (column #Mv_modif-acces-refuse
		(row #Mv_donnee (send 'objet2string cours) #Mv_modifiee)
		#Mv_autre-utilisateur))
	    (setq la-salle
	      (choisir-salle cours new-instant (salle-trouvee-i-cours cours)))
	    (when (appartenir-liste la-salle liste-i-salle)
	      (salle-trouvee-i-cours cours la-salle)))
	  (setq la-salle
	    (choisir-salle cours new-instant (salle-trouvee-i-cours cours)))
	  (when (appartenir-liste la-salle liste-i-salle)
	    (salle-trouvee-i-cours cours la-salle)
	    (when (salle-figee-reelle-i-cours cours)
	      (salle-figee-reelle-i-cours cours la-salle))))
	(show-edt Gen-App))
      (if MODULE-RESEAU
	(if (eq (debut-modification SITE cours MODIFICATION) ACCES-REFUSE)
	  (progn
	    (afficher-attention
	      (column #Mv_modif-acces-refuse
		(row #Mv_donnee (send 'objet2string cours) #Mv_modifiee)
		#Mv_autre-utilisateur))
	    (replacer-rectangle-cours cours))
	  (validation-fin-deplacement appli cours))
	(validation-fin-deplacement appli cours)))))

(defun validation-fin-deplacement (appli cours)
  (let
    ((mouvement-ok)
     (mouvement-impossible)
     (la-salle)
     (continuerp t))

    ;; MOD RM - 16/08/96 : changement du curseur en sablier
    (change-curseur appli Varuna-Curseur-Sablier)

    ;; MOD RM - 07/10/97
    ; (setq la-salle (deplacement-possible cours new-instant))
    (setq new-instant (choisir-instant-deplacement cours new-instant))
    (ifn new-instant
      (setq mouvement-ok ())
      (setq la-salle (nouveau-deplacement-possible cours new-instant))    
      (when (and MODULE-SALLES (equal la-salle t))
	(setq continuerp
	  (prompt-continuer
	    (column
	      #Mv_continuer-deplacement
	      #Mv_lever-contrainte-salle))))
      (setq mouvement-impossible *erreurs*)
      (setq mouvement-ok
	(and
	  instant
	  (or
	    (and mouvement-impossible (not (interface-erreur)))
	    (and
	      (not mouvement-impossible)
	      la-salle
	      continuerp
	      (if (appartenir-liste la-salle liste-i-salle)
		(afficher-confirmation
		  (column
		    #Mv_deplacer-au
		    (libelle-instant new-instant)
		    #Mv_deplacer-avec-la-salle
		    (libelle-i-salle la-salle)
		    (if
		      (or (liste-annulations cours) (liste-ajouts cours))
		      (column
			#Mv_msg-sup-annul-ajouts1 
			#Mv_msg-sup-annul-ajouts2
			#Mv_msg-sup-annul-ajouts3)
		      "")))
		(afficher-confirmation
		  (column
		    #Mv_deplacer-au
		    (libelle-instant new-instant)
		    (if
		      (or (liste-annulations cours) (liste-ajouts cours))
		      (column
			#Mv_msg-sup-annul-ajouts1
			#Mv_msg-sup-annul-ajouts2
			#Mv_msg-sup-annul-ajouts3)
		      "")))))))))
    (ifn mouvement-ok
      (replacer-rectangle-cours cours)
      (setq DIRTY t)
      (instant-trouve-i-cours cours new-instant)
      (if (appartenir-liste la-salle liste-i-salle)
	(progn
	  (salle-trouvee-i-cours cours la-salle)
	  (when (salle-figee-reelle-i-cours cours)
	    (salle-figee-reelle-i-cours cours la-salle)))
	(salle-trouvee-i-cours cours ()))
      (figer-cours cours)
      (supprimer-annulations-ajouts-cours cours)
      (show-edt Gen-App))
    (change-curseur appli Varuna-Curseur-Fleche)))

;;; <deplacement-interdit>
;;; CRE - 27/02/96
;;; MOD - 29/02/96
(defun deplacement-interdit (appli event)
  (let ((cours (component 'le-cours appli)))
    (traiter-erreur)
    (replacer-rectangle-cours cours)))

;;; TRAITEMENT DES RESAS
;;; AJT (10/04/95) MO
;;; Module des reservations des salles


;;; primitive d'affichage des resas
;;; on est FORCEMENT dans le cas Affichage Salle
;;; annuel ou hebdo


(defun afficher-resa (appli resa)
  (when 
    (resa-affichable-p resa)
    (or
      (and (eq Grid-Type 'year) (afficher-resa-annuel appli resa))
      (afficher-resa-hebdo appli resa))))


;;; affichage d'une resa en affichage salle annuel

(defun afficher-resa-annuel (appli resa)
  (let* 
    ((instant (instant-i-resa resa))
     (horaire-debut (interne2cumule (horaire-instant instant)))
     (horaire-fin (add horaire-debut (duree-i-resa resa)))
     (index-debut (plus-proche-index horaire-debut I-Heures))
     (index-fin (plus-proche-index horaire-fin I-Heures))
     (semaine (semaine-instant instant))
     (col (max 0 (sub semaine Semaine-Debut-Aff))))
    (ajouter-bouton-resa
      appli
      (couleur-de-resa resa)
      resa
      (numero-dans-liste (jour-instant instant) Jours-Aff)
      col
      (add1 col)
      index-debut
      (or index-fin (sub1 (length i-heures))))))
 

;;; affichage d'une resa en affichage salle hebdo

(defun afficher-resa-hebdo (appli resa)
  (let* 
    ((instant (instant-i-resa resa))
      (horaire-debut (interne2cumule (horaire-instant instant)))
      (horaire-fin (add horaire-debut (duree-i-resa resa)))
      (index-debut (plus-proche-index horaire-debut I-Heures))
      (index-fin (plus-proche-index horaire-fin I-Heures))
      (salle-aff (car liste-salles-aff)))     
      (ajouter-bouton-resa
        appli
        (couleur-de-resa resa)
        resa
        (numero-dans-liste (jour-instant instant) Jours-Aff)
        index-debut
        index-fin
        0 1)))



;;; ajout d'une appli de resa de couleur color, associee a la resa resa,
;;; positionne en no grille jour, de col1 a col2, de lig1 a lig2,

(defun ajouter-bouton-resa (appli couleur resa jour col1 col2 lig1 lig2)
  (let 
    ((local-appli (appli-rectangle-resa resa)))
    (widget-set-args local-appli
      'background couleur
      'font (police-resa resa))
    (add-component local-appli 'la-resa resa)
    ({application}:event-action 
      local-appli 'drag-event 
      (lambda 
        (appli event)
        (info-resa (component 'la-resa appli))))
    (send
      'v-add-application
      appli
      local-appli
      (code-i-resa resa)
      (cons-rectangle jour col1 col2 lig1 lig2)
      0 1 1 week-aff)))


;;; application associee a un rectangle de resa
;;; le type de l'application depend du contexte
;;; l'appli n'est pas deplacable
;;; pour le moment, on n'affiche rien, meme en hebdo info = nil


(defun appli-rectangle-resa (cours)
    (let* 
      ((image (image-rectangle-resa resa week-aff))
        (the-box (box 0 0 (send 'width image) (send 'height image)))
        (the-view (elasticview image the-box))
        (image-appli (if Week-Aff the-view image)))
      (send 'fit-to-contents
        (application 
          '{application} 0 0
          (send 'width image-appli)
          (send 'height image-appli)
          image-appli))))


;;; addition d'un bouton de resa dans l'appli. La couleur est couleur-resa

(defun image-rectangle-resa (resa info)
  (let
    ((police (police-resa resa))
     (horaire-debut (interne2cumule (horaire-instant (instant-i-resa resa))))
     (horaire-fin (add (duree-i-resa resa) horaire-debut)))

  (ifn
    info
    (view "")   ;;; !!! pourquoi ""
      (ifn
        impressionp
        (cadrer-centrer "Resa" 6) 
        (column
          (chaine 0 0 1 police 2 (libelle-i-resa resa))
          (chaine 0 0 1 police 1
            (if (salle-i-resa resa)
              (libelle-i-salle (salle-i-resa resa))
              ""))
          ""
          (chaine 0 0 1 police 1
            (catenate
              #Mv_info-de
              (funcall 'cumule2naturel horaire-debut)
              #Mv_info-a
              (funcall 'cumule2naturel horaire-fin)))
          ;; rem le funcal est important, il y a des pb avec cumule2naturel
          ;; sinon
          ;; les "" ne sont pas d'un proprete ireprochable
          ;; mais je ne sais comment reagit column avec ()
          "")))))




;;; une info sur les salles
;;; on fait un <with> avant la construction des applis, et notamment des 
;;; boutons pour que le <centeredimage> se fasse bien.
;;; rq: FD pour bouton2 s'inspirer de ce qui est fait dans <info-cours> 
;;; (i-appli.ll). !!

(defun info-resa (resa)
  (with
    ((current-font time8)
     (current-background COULEUR-RESA))
    (let* 
      ((bouton1 (standard-ok-button 10))
        (image (image-info-resa resa))
	; MOD FD - 02/04/96 
	;         (bouton2 (standardbutton (cadrer-centrer "Supprimer" 22)))
	(bouton2 (bouton-standard "      Supprimer      " 'supprimer-resa))
        (appl
          (application
            '{application} 0 0 0 0 
            (column image (row bouton1 bouton2)))))
      (send 'set-title appl #Mv_aff-resa)
      (send 'fit-to-contents appl)
      (add-component bouton1 'appli appl)
      (add-component bouton2 'la-resa resa)
      (add-component bouton2 'appli appl)
      (grab-and-select-application appl))))


(defun image-info-resa (resa)
    (let*
      ((instant (instant-i-resa resa))
        (jour (string (libelle-jour (jour-instant instant))))
        (horaire-debut (interne2cumule (horaire-instant instant)))
        (horaire-fin (add (duree-i-resa resa) horaire-debut))
	;; MOD - FD - 29/07/96 semaine calendaire
	(sem (ecrire-numero-semaine (semaine-instant (instant-i-resa resa))))
        (salle (libelle-i-salle (salle-i-resa resa)))
        (marge 10))
      (translation 10 0
        (column
          (catenate
            (libelle-i-resa resa)
            " (" (string (code-i-resa resa)) ")")
          (catenate
            #Mv_info-semaine
            (string sem))
          (catenate #Mv_info-le jour #Mv_info-de
            (funcall 'cumule2naturel horaire-debut)
            #Mv_info-a (funcall 'cumule2naturel horaire-fin))
          (row
            #Mv_i-salle
            (font time8b (libelle-i-salle (salle-i-resa resa))))))))


;;; TRAITEMENT DES AJOUTS DE SEANCES
;;; AJT (27/07/95) RM
;;; Module des ajouts de seances


;;; primitive d'affichage des ajouts de seance
;;; on est FORCEMENT dans le cas Affichage Salle
;;; annuel ou hebdo ---> FAUX donc a modifier !!!

(defun afficher-ajout (appli ajout)
  (when 
    (ajout-affichable-p ajout)
    (or
      (and (eq Grid-Type 'year) (afficher-ajout-annuel appli ajout))
      (afficher-ajout-hebdo appli ajout))))

;;; affichage d'un ajout en affichage salle annuel

(defun afficher-ajout-annuel (appli ajout)
  (let* 
    ((instant (instant-i-ajout ajout))
     (horaire-debut (interne2cumule (horaire-instant instant)))
     (horaire-fin (add horaire-debut (duree-i-ajout ajout)))
     (index-debut (plus-proche-index horaire-debut I-Heures))
     (index-fin (plus-proche-index horaire-fin I-Heures))
     (semaine (semaine-instant instant))
     (col (max 0 (sub semaine Semaine-Debut-Aff))))
    (ajouter-bouton-ajout
      appli
      (couleur-de-ajout ajout)
      ajout
      (numero-dans-liste (jour-instant instant) Jours-Aff)
      col
      (add1 col)
      index-debut
      (or index-fin (sub1 (length i-heures))))))
 

;;; affichage d'un ajout en affichage salle hebdo
(defun afficher-ajout-hebdo (appli ajout)
  (let* 
    ((instant (instant-i-ajout ajout))
      (horaire-debut (interne2cumule (horaire-instant instant)))
      (horaire-fin (add horaire-debut (duree-i-ajout ajout)))
      (index-debut (plus-proche-index horaire-debut I-Heures))
      (index-fin (plus-proche-index horaire-fin I-Heures))
      (salle-aff (car liste-salles-aff)))     
      (ajouter-bouton-ajout
        appli
        (couleur-de-ajout ajout)
        ajout
        (numero-dans-liste (jour-instant instant) Jours-Aff)
        index-debut
        index-fin
        0 1)))



;;; ajout d'une appli de ajout de couleur color, associee a l' ajout,
;;; positionne en no grille jour, de col1 a col2, de lig1 a lig2,

(defun ajouter-bouton-ajout (appli couleur ajout jour col1 col2 lig1 lig2)
  (let 
    ((local-appli (appli-rectangle-ajout ajout)))
    (widget-set-args local-appli
      'background couleur
      'font (police-ajout ajout))
    (add-component local-appli 'lajout ajout)
    ({application}:event-action 
      local-appli 'drag-event 
      (lambda 
        (appli event)
        (info-ajout (component 'lajout appli))))
    (send
      'v-add-application
      appli
      local-appli
      (code-i-ajout ajout)
      (cons-rectangle jour col1 col2 lig1 lig2)
      0 1 1 week-aff)))


;;; application associee a un rectangle de ajout
;;; le type de l'application depend du contexte
;;; l'appli n'est pas deplacable
;;; pour le moment, on n'affiche rien, meme en hebdo info = nil


(defun appli-rectangle-ajout (cours)
    (let* 
      ((image (image-rectangle-ajout ajout week-aff))
        (the-box (box 0 0 (send 'width image) (send 'height image)))
        (the-view (elasticview image the-box))
        (image-appli (if Week-Aff the-view image)))
      (send 'fit-to-contents
        (application 
          '{application} 0 0
          (send 'width image-appli)
          (send 'height image-appli)
          image-appli))))


;;; addition d'un bouton de ajout dans l'appli. La couleur est couleur-ajout
;;; AJT - FD - 18/09/95 - texte pour l'impression

(defun image-rectangle-ajout (ajout info)
  (let
    ((police (police-ajout ajout))
      (horaire-debut
        (interne2cumule (horaire-instant (instant-i-ajout ajout))))
     (horaire-fin (add (duree-i-ajout ajout) horaire-debut)))
    (ifn info
      (view "")
      (ifn
        impressionp
        (view (cadrer-centrer "Ajout" 6))
        (column
          (chaine 0 0 1 police 2 (libelle-i-ajout ajout))
          (chaine 0 0 1 police 1
            (if (salle-i-ajout ajout)
              (libelle-i-salle (salle-i-ajout ajout))
              ""))
          (chaine 0 0 1 police 1
            (liste-libelles-i-profs (profs-i-ajout ajout)))
          (chaine 0 0 1 police 1
            (catenate
              #Mv_info-de
              (funcall 'cumule2naturel horaire-debut)
              #Mv_info-a
              (funcall 'cumule2naturel horaire-fin)))
          ;; rem le funcal est important, il y a des pb avec cumule2naturel
          ;; sinon
          ;; les "" ne sont pas d'un proprete ireprochable
          ;; mais je ne sais comment reagit column avec ()
        "")))))
  



;;; une info sur les salles
;;; on fait un <with> avant la construction des applis, et notamment des 
;;; boutons pour que le <centeredimage> se fasse bien.

(defun info-ajout (ajout)
  (with
    ((current-font time8)
     (current-background COULEUR-AJOUT))
    (let* 
      ((bouton1 (standard-ok-button 10))
        (image (image-info-ajout ajout))
	; MOD - FD - 02/04/96
	; (bouton2 (standardbutton (cadrer-centrer "Supprimer" 22)))
	(bouton2 (bouton-standard "      Supprimer      " 'supprimer-ajout))
        (appl
          (application
            '{application} 0 0 0 0 
            (column image (row bouton1 bouton2)))))
      ; MOD - FD - 02/04/96
      ; (set-action bouton2 'supprimer-ajout)
      (send 'set-title appl #Mv_aff-ajout)
      (send 'fit-to-contents appl)
      (add-component bouton1 'appli appl)
      (add-component bouton2 'lajout ajout)
      (add-component bouton2 'appli appl)
      (grab-and-select-application appl))))

  
(defun image-info-ajout (ajout)
    (let*
      ((instant (instant-i-ajout ajout))
        (jour (string (libelle-jour (jour-instant instant))))
        (horaire-debut (interne2cumule (horaire-instant instant)))
        (horaire-fin (add (duree-i-ajout ajout) horaire-debut))
	;; MOD - FD - 29/07/96 semaines calendaires
        (sem (ecrire-numero-semaine (semaine-instant (instant-i-ajout ajout))))
        (salle (libelle-i-salle (salle-i-ajout ajout)))
        (marge 10))
      (translation 10 0
        (column
          (catenate
            (libelle-i-ajout ajout)
            " (" (string (code-i-ajout ajout)) ")")
          (catenate
            #Mv_info-semaine
            (string sem))
          (catenate
            #Mv_info-le
            jour
            #Mv_info-de
            (funcall 'cumule2naturel horaire-debut)
            #Mv_info-a
            (funcall 'cumule2naturel horaire-fin))
          (row
            #Mv_i-salle
            (font time8b (libelle-i-salle (salle-i-ajout ajout))))
          (catenate
            #Mv_info-profs
            (liste-libelles-i-profs (profs-i-ajout ajout)))))))

  
   
