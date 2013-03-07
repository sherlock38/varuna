;;; --------------------------------------------------------------------------
;;; - I-Appli.ll                                                VARUNA 1.2   -
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; - Interface Graphique                                                    -
;;; - Definition des applications principales de VARUNA                      -
;;; - et des comportements simples                                           -
;;; --------------------------------------------------------------------------
;;; - Creation:  Mars 93                                                     -
;;; - Mise a jour pour version 1.2:  Juin 1994                               -
;;; - Build 1.22.1 01.07.99 / JH MO                                          -
;;; --------------------------------------------------------------------------


;;; 1. L'APPLICATION RESOLUTION
;;;

;;; Mise a jour du bandeau de la fenetre Gen-App (devrait etre dans actions)

(defun donner-titre (str)
  (if str
    (send 'set-title Gen-App (catenate App-Name " -- " str))
    ;; MOD - FD - 27/02/96 MultiP ajout du test LECTURE-SEULE?
    (send 'set-title Gen-App
      (catenate App-Name (when LECTURE-SEULE? #Mv_mode-consultation)))))


;;; les boutons de modification de taille de la grille

(defun sizer-panel ()
  (let 
    ((b1 (button (elasticicon 0 0 16 16 Icon-ungrow)))
     (b2 (button (elasticicon 0 0 16 16 Icon-grow)))
     (b3 (button (elasticicon 0 0 16 16 Icon-size)))
     (b4 (button (elasticicon 0 0 16 16 Icon-scale))))
    (set-action b2 'agrandir-grille)
    (set-action b1 'rapetir-grille)
    (set-action b3 'retailler-grille)
    (set-action b4 'echelle-grille) 
    (row
      b1 
      (translation 5 0 b2)
      (translation 5 0 b3)
      (translation 5 0 b4))))


;;; l'application resolution

(defun application-resolution ()
  (let* 
    ((menu (make-resolution-menubar))
     (th (mon-thermo 100 11 2))                 ; objet i-objets.ll
     (br (bouton-run-stop t))                   ; bouton i-boutons.ll

     (image (if (eq Grid-Type 'Year) (year-grid) (week-grid)))

     (gr-image (v-application 0 0
       (send 'width image)
       (send 'height image)
       image))

     (gr (scroller 0 0 627 390 gr-image t t)) 

     (le-titre (mon-titre (send 'width menu) ""))  ; objet i-objets.ll

     (size-manager (sizer-panel))                  ; objet i-appli.ll

     (bouton-next (button (elasticicon 0 0 16 16 Icon-Suivant)))
       ; (standardbutton ">>"))           ; pas dans l'appli
     (bouton-prev (button (elasticicon 0 0 16 16 Icon-Precede)))
       ; (standardbutton "<<"))           ; pas dans l'appli

     (appli 
       (mixed-applicationq

         ;; nommage des composants
         menu-bar menu
         grid gr-image
         defileur gr
         bouton br
         titre le-titre
         thermo th               
         name Gen-App

         ;; definition de l'image
         image
         (elasticview
           (column
             (row 
               (column 
                 menu 
                 le-titre)
               (translation 5 5 br)
               (rectangle 0 0 5 37))
             (rectangle 0 0 627 3)    
             (boxedview gr)
             (rectangle 0 0 627 3)    
             (row
               (translation 5 0 size-manager)
               (rectangle 0 0 20 15)
               (boxedview th)
               (rectangle 0 0 45 15)
	       bouton-prev
               (rectangle 0 0 5 15)
	       bouton-next
               (translation 70 1 
                 ;; devrait etre un message  
                 (font time8 #M v_copyright-resolution)))
             (rectangle 0 0 627 2)      
             )))))             
             
    (set-action bouton-next 'aff-suivant)     ; pas dans cette version
    (set-action bouton-prev 'aff-precedent)   ; pas dans cette version

    (set-action br
      (lambda (b)
	(setq compter-contraintes-p t)
	(funcall 'edt-run b () t t)))

    (send 'initialise appli)

    (send 'resize
      (component 'grid appli) 
      (- (send 'width (component 'defileur appli))
	200 {scroller}:scrollbarwidth)
      (- (send 'height (component 'defileur appli))
        200 {scroller}:scrollbarheight))

    (send 'reinitialise (component 'defileur appli))
    appli))


;;; mise a jour du thermometre du display (devrait etre dans les actions)

(defun mise-a-jour-thermometre (nombre total)
  (maj-thermo (component 'thermo Gen-App) (* (/ nombre total) 100))
  (maj-thermo (component 'thermo Input-App) (* (/ nombre total) 100)))




;;; 2. Application Mode Saisie
;;;
;;; application du Mode saisie - juste une petite image et des boutons
;;; pour chaque type
;;; mettre une variable globale pour le liste des concepts a passer
;;; a data-base-application (cf cette fonction dans i-udata.ll


(defun application-saisie ()
  (let*
    ((menu (make-input-menubar))
     (th (mon-thermo 100 11 2))                 ; objet i-objets.ll
     (title
       (owner 0 0 #2wchar 500
	 (font time8b (libelle-i-ecole I-ECOLE)) 'edit-owner-ecole)) 
     (appli
       (data-base-application 
	 title menu th i-cours i-prof i-salle i-groupe)))
    (send 'set-title appli #Mv_titreModesaisie)
    (add-component appli 'menu-bar menu)
    (add-component appli 'thermo th)
    ;; MOD RM 12/06/96 : definition de la taille de la fenetre 
    (send 'resize appli 630 470)
    appli))


;;; 3. APPLICATION INFO-COURS

;;; actions de l'application - info-cours

;;; <acces-figer-defiger-interdit>
;;; CRE - FD - 27/02/96

(defun acces-figer-defiger-interdit (bouton)
  ;; doit retourner ()
  (and (traiter-erreur) ()))

;;; CRE RM - 28/11/96 : fonction Figer / Defiger

(defun info-cours-figer-defiger (but)
  (let
    ((cours (component 'le-cours but)))
    (if MODULE-RESEAU
      (if (eq (debut-modification SITE cours MODIFICATION) ACCES-AUTORISE)     
	(figer-defiger-element cours but)
	(afficher-attention
	  (column
	    #Mv_modif-acces-refuse
	    (row #Mv_donnee (send 'objet2string cours) #Mv_modifiee)
	    #Mv_autre-utilisateur)))
      (figer-defiger-element cours but))))

;;; CRE RM - 28/11/96 : Figer / Defiger un cours

(defun figer-defiger-element (cours but)
  (let
    ((result (appli-figer-defiger cours))
     (coursp)
     (sallep))
    (when result
      (setq coursp (car result))
      (setq sallep (cdr result))
      (if coursp
	(figer-cours cours)
	(defiger-cours cours))
      (if sallep
	(figer-salle-cours cours)
	(defiger-salle-cours cours))
      (when but
	(reafficher-infos but cours))
      (setq DIRTY t))))

;;; CRE RM - 28/11/96 : appli pour Figer / Defiger

(defun appli-figer-defiger (cours)
  (let*
    ((coursp (fige-i-cours cours))
     (sallep (salle-figee-reelle-i-cours cours))
     (bc-fige (radiobutton 0 0 250 15 #Mv_cours-fige))
     (bc-defige (radiobutton 0 0 250 15 #Mv_cours-defige))
     (bs-fige (radiobutton 0 0 250 15 #Mv_salle-fige))
     (bs-defige (radiobutton 0 0 250 15 #Mv_salle-defige))
     (p-cours (selectorapplication 0 0 250 30 (column bc-fige bc-defige)))
     (p-salle (selectorapplication 0 0 250 30 (column bs-fige bs-defige))))
    (send 'set-selection p-cours (if coursp bc-fige bc-defige))
    (send 'set-selection p-salle (if sallep bs-fige bs-defige))
    (when
      (afficher-confirmation
	(column #Mv_selection-fige "" p-cours "" p-salle ""))
      (cons
	(equal (send 'title (send 'get-selection p-cours)) #Mv_cours-fige)
	(equal (send 'title (send 'get-selection p-salle)) #Mv_salle-fige)))))

;;; CRE RM - 28/11/96 : mise a ajour de la boite d'infos de cours

(defun reafficher-infos (but cours)
  (send 'set-image (component 'zone-cours but)
    (chaine 0 0 1 (police-cours cours) 1
      (catenate
	(libelle-i-cours cours)
	" (" (string (code-i-cours cours)) ")")))
  (send 'v-remove-application
    (component 'grid Gen-App) (code-i-cours cours))
  (when (salle-trouvee-i-cours cours)
    (send 'set-image (component 'zone-salle but)
      (chaine 0 0 1 (police-salle-cours cours) 1
	(libelle-i-salle (salle-trouvee-i-cours cours)))))
  (show-cours (component 'grid Gen-App) cours))

;;; CRE - FD - 01/03/96
;;; MOD - FD - 27/03/96 - ajout de tester-types-cours
;;; MOD - RM - 28/11/96 : une seule fonction pour Figer / Defiger

(defun autoriser-info-cours-fige (bouton)
  (let 
    ((cours (component 'le-cours bouton)))
    (if
      (or
	(donnee-lecture-seule-p cours t) ; LECTURE-SEULE?
	(not (tester-types-cours cours)))
      (acces-figer-defiger-interdit cours)
      (info-cours-figer-defiger bouton))))

;;; AJT RM - 250795
;;; Bouton Annuler une séance

(defun bouton-annuler-seance (but)
  (let
    ((cours (component 'le-cours but))
     (rattraperp))
    ;; AJT - FD - 02/04/96 type de cours et salle accessibles
    (if (not (tester-types-cours cours))
      (traiter-erreur)
      ;; AJT RM 17/08/95 : Fermeture de la fenetre <info-cours>
      (remove-application (component 'appli but))
      (setq rattraperp (annuler-seance cours))
      (when rattraperp
	(bouton-ajouter-seance but)))))

(defun bouton-ajouter-seance (but)
  (let 
    ((cours (component 'le-cours but))
     (Raco-App))
    (if (not (tester-types-cours cours))
      (traiter-erreur)
      (remove-application (component 'appli but))
      (ajouter-seance cours () () ()))))

;;; renvoie une liste de professeurs avec leur civilites
;;; et separes par des virgules 

(defun liste-libelles-i-profs (profs)
  (let 
    ((libs (mapcar 'civilite-libelle-i-prof profs))
     (chaine ""))
  (while libs
    (setq chaine
      (catenate
        chaine
        (car libs)
        (if (cdr libs) ", " "")))
    (nextl libs))
  chaine))

;;; application info-cours
;;; AJT RM - 250795 : Boutons annulation et ajout de seances
;;; AJT - FD - 20/09/95 - mise en gras de libelle du cours lorsque
;;;                       celui-ci est fige

(defun info-cours (cours)
  (let* 
    ((but (standard-ok-button 8))
     (but-fig
       (bouton-standard
	 (cadrer-centrer "Figer / Défiger" 10) 'autoriser-info-cours-fige))
     (but-annul (bouton-standard "Annuler une séance" 'bouton-annuler-seance))
     (but-ajout (bouton-standard "Ajouter une séance" 'bouton-ajouter-seance))
     (instant (instant-trouve-i-cours cours))
     (jour (string (libelle-jour (jour-instant instant))))
     (horaire-debut (interne2cumule (horaire-instant instant)))
     (horaire-fin (add (duree-i-cours cours) horaire-debut))
     (sem (semaine-instant (instant-trouve-i-cours cours)))
     (salle (salle-trouvee-i-cours cours))
     (salle-lib (or (and salle (libelle-i-salle salle)) " "))
     (view-salle
         (application '{application} 0 0 0 0
             (view
                 (chaine 0 0 1 (police-salle-cours cours) 1 salle-lib))))
     (view-cours
         (application '{application} 0 0 0 0
             (view
                 (chaine 0 0 1 (police-cours cours) 1
                     (catenate 
                         (libelle-i-cours cours)
                         " (" (string (code-i-cours cours)) ")")))))
     (appl (application '{application} 0 0 0 0
         (column
             (row view-cours)
	     ;;  MOD - FD - 29/07/96 semaines calendaires
             (catenate
                 #Mv_info-semaines
                 (string (ecrire-numero-semaine sem))
                 #Mv_info-a
                 (string (ecrire-numero-semaine (sub1 (add sem (nb-seances-i-cours cours))))))
        (catenate
          #Mv_info-le jour #Mv_info-de
          (funcall 'cumule2naturel horaire-debut)
          #Mv_info-a
          (funcall 'cumule2naturel horaire-fin)) 
        (row #Mv_i-salle view-salle)
        (catenate
          #Mv_info-profs
          (liste-libelles-i-profs (profs-i-cours cours)))
        (if MODULE-RACO
            (column
                (catenate
                    #Mv_info-annuls
                    (liste-seances-i-annulation cours))
                (catenate
                    #Mv_info-ajouts
                    (liste-seances-i-ajout cours))
                (row but but-fig)
                (row but-annul but-ajout))
            (column
                (row but but-fig)))))))
    (send 'set-title appl #Mv_aff-cou)
    (widget-set-args appl
        'font time8
        'background (couleur-de-salle (type-salle-i-cours cours)))
    (send 'fit-to-contents view-cours)
    (send 'fit-to-contents view-salle)
    (send 'fit-to-contents appl)
    (send 'resize
        appl (max (mul 2 (send 'width but)) (send 'width appl)) ())
    (add-component but 'appli appl)
    (add-component appl 'zone-cours view-cours)
    (add-component appl 'zone-salle view-salle)
    (add-component but-fig 'le-cours cours)
    (when MODULE-RACO
      (add-component but-annul 'le-cours cours)
      (add-component but-annul 'appli appl)  
      (add-component but-ajout 'le-cours cours)
      (add-component but-ajout 'appli appl))
    (grab-and-select-application appl)))


;;; 4. L'APPLICATION A-PROPOS
;;;


(defun about-application ()
  (let* 
    ((but (standard-ok-button 5))
     (logo (icon 0 0 Icon-Varuna))
     (appl (application '{application} 12 12 50 50
        (header 
          logo  
          (font time8b (ajouter-espace #Mvaruna 0 20))
          but
          (filledbox 0 0 20 100 2)
          (column
            "" 
            (font time8 #Mv_version)
            (font time8 #Mv_build)
            ""
            (font time8 #Mv_support-technique)
            (font time8 #Mv_telephone)
            (font time8 #Mv_fax)
            (font time8 #Mv_email)
            ""
            (font time8 #Mv_copyright)) t t))))
    (send 'set-title appl #Mv_apropos)
    (send 'fit-to-contents appl)
    (add-component but 'appli appl)
    appl))


;;; 5. TERMINAL POUR LES DIVERSES EDITIONS

;;; AJT (27/03/95) MO
;;; ajout d'un deletefile pour pallier l'imcompatibilite de la 15.26 sur le 
;;; copyfile
;;; CHG (29/03/95) MO
;;; release ILOG debuggant -a priori- cette icompatibilite

(defun make-ttyapp ()
  (let* ((tty (medite 0 0 500 2000))
    (but (standardbutton (font time8b (ajouter-espace  #Mv_fermer 10 10))))
    (clear (standardbutton (font time8b (ajouter-espace #Mv_aff-eff 10 10))))
    (sav (standardbutton (font time8b (ajouter-espace #Mv_dir-sav 10 10))))
    (prin (standardbutton (font time8b (ajouter-espace #Mv_dir-pri 10 10))))
    (scroller (verticalscrollbar
      0 0 {scroller}:scrollbarwidth 0 0 0 0 0 0))) 
  (medite-attach-scrollbar tty scroller)
  (setq Tty-App (application '{application} 50 50 200 200 
    (column
      (elasticrow but sav clear prin)
      (row scroller tty))))
  (send 'set-title Tty-App #Mv_titre-terminal)
  (medite-cur-top tty)
  (edit-command-redisplay ({medite}:edit tty) ())
  (send 'fit-to-contents Tty-App)
  (add-component Tty-App 'tty tty)
  (add-component clear 'b-clear tty)
  (add-component but 'topappli Tty-App)
  (add-component clear 'topappli Tty-App)
  (add-component sav 'b-sav Tty-App)
  (add-component Tty-App 'scroller scroller)

  (set-action clear
    (lambda (b)
      (let
	((fichier (catenate REP-VARUNA "\" FICHIER-TEMPORAIRE)))
	(send 'set-text (component 'tty b) ())
	(remove-application (component 'topappli b))
	(when (probefile fichier)
	  (deletefile fichier)))))

  (set-action but
    (lambda (b)
      (let
	((fichier (catenate REP-VARUNA "\" FICHIER-TEMPORAIRE)))
	(when (probefile fichier)
	  (deletefile fichier))
	(remove-application (component 'topappli b)))))

  (set-action sav 
    (lambda (b) 
      (let
	((fichier (demande-chaine #Mv_nom-fichier))
	 (rm (rmargin)))
        (when fichier
	  (rmargin 1024)
          (with ((outchan (openo fichier)))
	    (imprimer-liste-chaines-avec-vide
	      (send 'get-text (component 'tty b)))
	    (close (outchan)))
	  (rmargin rm)))))

  (set-action prin
    (lambda (b)
      (let
	((fichier (catenate REP-VARUNA "\" FICHIER-TEMPORAIRE))
	 (rm (rmargin)))
	(rmargin 1024)
	(with ((outchan (openo fichier)))
	  (imprimer-liste-chaines-avec-vide
	    (send 'get-text (component 'tty b)))
	  (close (outchan)))
	(rmargin rm)
	(when (probefile fichier)
	  (imprimer-fichier-texte fichier)))))))


;;; 6. APPLICATION IMPRESSION
;;;


;;; une zone de titre
;;; ne sert plus que dans l'application impression

(defun gridtitle ()
  (let ((ed (stringedit 0 0 500 "")))
    (widget-set-args ed
      'font time8b)
    (send 'readonly-flag ed t)
    ed))

;;; CRE RM : une zone pour la date de derniere modif

(defun datetitle ()
  (let
    ((ed (stringedit 0 0 40 "")))
    (widget-set-args ed 'font time6b)
    (send 'readonly-flag ed t)
    ed))
  
;;; construction de l'application impression

(defun application-impression ()
  (let* 
    ((gr-image (if (eq Grid-Type 'year) (year-grid) (week-grid)))
     (gr (v-application 0 0 640 400 gr-image))
     (le-titre1 (gridtitle))
     (le-titre2 (gridtitle))
     (date-modification (datetitle))
     (im
       (elasticview
         (column
             (row le-titre1 date-modification)
             le-titre2
             (font time5b COPYRIGHT)    ; DBG (06/05/95) MO faute de frappe
             gr))))      
    (mixed-applicationq 
       grid gr 
       grid1-title le-titre1 
       grid2-title le-titre2
       date-title date-modification
       image im)))



;;; LA GRILLE

;;; renvoie la liste d'horaires pour la construction d'une grille
;;; prenant en compte les horaires d'ouverture et de fermeture et
;;; de pause de l'ecole.

(defun horaires-proportionnels ()
  (let*
    ((les-heures (cons
      Horaire-Cumule-Fermeture
      (cons
        Horaire-Cumule-Pause
        (copy Heures-Debut-Aff)))))
  (setq I-Heures (sortn (supprimer-occurences-vides les-heures)))))

;;; image de la grille annuelle

;;; taille originelle

(defun hauteur-grille ()
  (ifn Gen-App
    (- (bitymax) {scroller}:scrollbarheight 300)
    (send 'height (component 'grid Gen-App))))

(defun largeur-grille ()
  (ifn Gen-App
    (- (bitxmax) {scroller}:scrollbarwidth 300)
    (send 'width (component 'grid Gen-App))))

;;; la grille annuelle
;;; MOD - FD - 29/07/96 semaines calendaires
(defun year-grid ()
  (let* 
    ((les-heures (horaires-proportionnels))
     (les-semaines
       (ecrire-liste-numeros-semaine
	 (liste-nombres-inclus
	    Semaine-Debut-Aff (add 1 Semaine-Fin-Aff) 1 1))))
    (vgrid 0 0 (largeur-grille) (hauteur-grille)
      les-heures (length Jours-Aff)
      (add 2 (sub Semaine-Fin-Aff Semaine-Debut-Aff))
      (mapcar 'string les-semaines)
      (mapcar 'string (mapcar 'libelle-jour Jours-Aff)))))


;;; la grille hebdomadaire

(defun week-grid ()
  (let* 
    ((les-heures (horaires-proportionnels)))
    (vgrid 0 0 (largeur-grille) (hauteur-grille) les-heures
      (length Jours-Aff) ()
      (mapcar 'heure-entiere les-heures)
      (mapcar 'string (mapcar 'libelle-jour Jours-Aff)))))

;;; renvoie la liste d'horaires pour une grille
;;; de type dispo (de zero a 24 heures) avec un parametre
;;; grain-dispo (globale) qui exprime le nombre de minutes entre deux 
;;; horaire
;;; MOD FD - RM 20/09/96

(defun heures-semaines-dispo ()
  (let 
    ((lh ())
      (count (div HORAIRE-CUMULE-OUVERTURE (grain-dispo-i-ecole I-ECOLE)))
      (max (div HORAIRE-CUMULE-FERMETURE (grain-dispo-i-ecole I-ECOLE))))     
;       (max (div (mul NB-MINUTES NB-HEURES) (grain-dispo-i-ecole I-ECOLE))))
    (while (le count max)
      (newl lh (mul count (grain-dispo-i-ecole I-ECOLE)))
      (incr count))
    (reverse lh)))

;;; image de la grille de disponibilite en semaine

(defun grille-semaines-dispo ()
  (let* 
    ((les-heures (heures-semaines-dispo)))
    (vgrid 0 0 0 0 les-heures (length JOURS-OUVRABLES) () 
      (mapcar 'libelle-horaire-dispo les-heures)
      (mapcar 'string JOURS-OUVRABLES))))

;;; construit la liste des semaines pour une grille de type dispo
;;; en semaines
;;; MOD RM - 25/09/96 : dispo des semaines = nb semaines ecole

(defun semaines-dispo ()
  (let 
    ((ls ())
      (count 1)
      (max (+ (length semaines-debut-ecole) 1)))
    (while (le count max)
      (newl ls count)
      (incr count))
    (reverse ls)))

;;; image de la grille de saisie des dispo en semaine
;;; MOD - FD - 29/07/96 semaines calendaires
;;; MOD RM - 25/09/96 : dispo des semaines = nb semaines ecole

(defun grille-planning-dispo ()
  (let* 
    ((les-semaines (ecrire-liste-numeros-semaine (semaines-dispo))))
    (vgrid 0 0 0 0 '(0 1) 1 (+ (length semaines-debut-ecole) 1)
      (mapcar 'libelle-semaines-dispo les-semaines)
      (list #Mv_semaines))))

;;;  CRE RM 310795
;;;  Demande d'une annulation de seance dans le menu
;;;  <rattrapages de cours>
;;;  MOD - FD - 09/05/96 - filtrage pour gestion de sites

(defun demander-annulation-seance (s)
  (let
    ((cours
      (demande-filtree-i-cours	#Mv_pro-cou #Mv_pas-cours-non-instancies
	() 'donnee-modifiable-p))
     (rattraperp))
    (when cours
      (setq rattraperp (annuler-seance cours))
      (when rattraperp
	(ajouter-seance cours () () ())))))


;;;  Demande d'une suppression d'annulation de seance dans le menu
;;;  <rattrapages de cours>
;;; MOD - RM - 16/08/95
;;;  MOD - FD - 09/05/96 - filtrage pour gestion de sites

(defun supprimer-annulation-seance (s)
  (let
    ((annul (demande-filtree-i-annulation
              #Mv_pro-ann
	      #Mv_aucune-annulation
	      ()
	      'annul-site-p)))
      (when annul
      (if MODULE-RESEAU
	(if (eq (debut-modification SITE annul SUPPRESSION) ACCES-AUTORISE)
          (supprimer-i-annulation annul t)
	  (afficher-attention
	    (column
	      #Mv_sup-acces-refuse
	      (row #Mv_donnee (send 'objet2string annul) #Mv_modifiee)
	      #Mv_autre-utilisateur)))
	(supprimer-i-annulation annul t))
      (show-edt s))))

;;; Demande d'un ajout de seance dans le menu <rattrapages de cours>
;;;  MOD - FD - 09/05/96 - filtrage pour gestion de sites

(defun demander-ajout-seance (s)
  (let 
    ((cours
      (demande-filtree-i-cours #Mv_pro-cou #Mv_pas-cours-non-instancies
	       () 'donnee-modifiable-p))
     (Raco-App))
    (when cours
      (ajouter-seance cours () () ()))))

;;;  Demande d'une suppression d'ajout de seance dans le menu
;;;  <rattrapages de cours>
;;;  MOD RM - 16/08/95

(defun supprimer-ajout-seance (s)
  (let
    ((ajout (demande-filtree-i-ajout
              #Mv_pro-ajout1
	      #Mv_aucun-ajout
	      ()
	      'ajout-site-p)))
      (when ajout
	(if MODULE-RESEAU
	  (if (eq (debut-modification SITE ajout SUPPRESSION) ACCES-AUTORISE)
	    (supprimer-i-ajout ajout t)
	    (afficher-attention
	      (column
		#Mv_sup-acces-refuse
		(row #Mv_donnee (send 'objet2string ajout) #Mv_modifiee)
		#Mv_autre-utilisateur)))
	  (supprimer-i-ajout ajout t))
	(show-edt s))))

;;; AJT RM - 250795
;;; Bouton Annuler une séance
;;; AJT RM - 29/08/95 : traitement de la donnee en reseau

(defun annuler-seance (cours)
  (let
    ((retour ()))
    (if MODULE-RESEAU
      (if (eq (debut-modification SITE cours MODIFICATION) ACCES-AUTORISE)     
	(setq retour (executer-annulation-seance cours))
	(afficher-attention
	  (column
	    #Mv_modif-acces-refuse
	    (row #Mv_donnee (send 'objet2string cours) #Mv_modifiee)
	    #Mv_autre-utilisateur)))
      (setq retour (executer-annulation-seance cours)))
    retour)))

(defun executer-annulation-seance (cours)
  (let
    ((annul (nouveau-i-annulation)))
    (code-i-annulation annul (gencode-i-annulation))
    (when MODULE-RESEAU
      (debut-modification SITE annul CREATION))
    (libelle-i-annulation annul
      (catenate "Annulation de " (libelle-i-cours cours)))
    (code-cours-i-annulation annul (code-i-cours cours))
    (cours-i-annulation annul cours)
    (libelle-cours-i-annulation annul (libelle-i-cours cours))
    (duree-i-annulation annul (duree-i-cours cours))
    (if (i-pas-instanciep cours)
      (afficher-erreur #Mv_cours-non-place)
      (when (editer-i-annulation annul t)
	(update-grid-titles)
	(show-edt Gen-App)
	(rattrapage-i-annulation annul)))))

(defun ajouter-seance (cours jour sem flag)
  (if MODULE-RESEAU
    (if (eq (debut-modification SITE cours MODIFICATION) ACCES-AUTORISE)     
      (executer-ajout-seance cours jour sem flag)
      (afficher-attention
	(column
	  #Mv_modif-acces-refuse
	  (row #Mv_donnee (send 'objet2string cours) #Mv_modifiee)
	  #Mv_autre-utilisateur)))
    (executer-ajout-seance cours jour sem flag)))

(defun executer-ajout-seance (cours jour semaine flag)
  (let
    ((ajout (nouveau-i-ajout)))
    (code-i-ajout ajout (gencode-i-ajout))
    (when MODULE-RESEAU
      (debut-modification SITE ajout CREATION))
    (libelle-i-ajout ajout (catenate "Ajout de " (libelle-i-cours cours)))
    (when jour (jour-i-ajout ajout (libelle-jour jour)))
    (when semaine (semaine-i-ajout ajout semaine))
    (flag-i-ajout ajout flag)
    (duree-i-ajout ajout (duree-i-cours cours))
    (audience-i-ajout ajout (audience-i-cours cours))
    (type-salle-i-ajout ajout (type-salle-i-cours cours))
    (cours-rattrape-i-ajout ajout cours)
    (instant-cours-rattrape-i-ajout ajout (instant-trouve-i-cours cours))
    (profs-i-ajout ajout (profs-i-cours cours))
    (contraintes-familles-i-ajout ajout t)
    (dispo-cours-familles-i-ajout ajout t)
    (dispo-profs-i-ajout ajout t)
    (setq Raco-App (fenetre-i-ajout-standard ajout))
    (grab-and-select-application Raco-App)
    (when (not (chaine-vide-p (salle-i-ajout ajout)))
      (maj-affichage
	'salle
	(list (salle-i-ajout ajout))
	(semaine-i-ajout ajout)))))
  
;;; CRE RM 010895
;;; renvoie une liste de seances annulees pour un cours 

(defun liste-seances-i-annulation (cours)
  (let ((liste-seances-annulees ())
        (chaine ""))
     (mapc
       (lambda (annul)
         ; MOD FD 13/02/96
	 ; (when (eq (code-cours-i-annulation annul) (code-i-cours cours))
	 (when (eq (cours-i-annulation annul) cours)
           (setq liste-seances-annulees
             (cons 
                 (cons
                     (ecrire-numero-semaine
		       (string2number (semaine-i-annulation annul)))
                     (code-i-annulation annul))
                 liste-seances-annulees))))
       liste-i-annulation)
     (while liste-seances-annulees
        (setq chaine
          (catenate
            chaine
            (string (caar liste-seances-annulees))
            " (" (cdar liste-seances-annulees) ")" 
           (if (cdr liste-seances-annulees) ", " "")))
       (nextl liste-seances-annulees))
     chaine))

;;; CRE RM 020895
;;; renvoie une liste de seances annulees pour un cours 

(defun liste-seances-i-ajout (cours)
  (let ((liste-seances-ajoutees ())
        (chaine ""))
     (mapc
       (lambda (ajout)
	 ; MOD FD 13/02/96
	 ; (when (eq (cours-rattrape-i-ajout ajout) (code-i-cours cours))
	 (when (eq (cours-rattrape-i-ajout ajout) cours)
           (setq liste-seances-ajoutees
             (cons 
                 (cons
		   (ecrire-numero-semaine (semaine-i-ajout ajout))
		   (code-i-ajout ajout))
                 liste-seances-ajoutees))))
       liste-i-ajout)
     (while liste-seances-ajoutees
        (setq chaine
          (catenate
            chaine
            (string (caar liste-seances-ajoutees))
            " (" (cdar liste-seances-ajoutees) ")" 
           (if (cdr liste-seances-ajoutees) ", " "")))
       (nextl liste-seances-ajoutees))
     chaine))
