;;; --------------------------------------------------------------------------
;;; - I-Menu.ll                                                VARUNA 1.2    -
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; - Interface Graphique                                                    -
;;; - Routines associees aux sous-menus                                      -
;;; --------------------------------------------------------------------------
;;; - Creation:  Mars 93                                                     -
;;; - Mise a jour pour version 1.07: Decembre 93                             -
;;; - Build 1.22.1 01.07.99 / JH MO                                          -
;;; --------------------------------------------------------------------------

;;; Place des menus (mode resolution)

;; Fichier

(defvar PLACE-MENU-NOUVEAU 0)
(defvar PLACE-MENU-OUVRIR 1)
(defvar PLACE-MENU-ENREGISTRER 2)
(defvar PLACE-MENU-ENREGISTRER-SOUS 3)
(defvar PLACE-MENU-MODE 5)
(defvar PLACE-MENU-APROPOS 7)
(defvar PLACE-MENU-QUITTER 8)

;; Resolution

(defvar PLACE-MENU-RES-TOTALE 0)
(defvar PLACE-MENU-RES-PARTIELLE 1)
(defvar PLACE-MENU-RES-COURS-TOTAL 3)
(defvar PLACE-MENU-RES-COURS-PARTIELLE 4)
(defvar PLACE-MENU-RES-SALLES 5)

;; Gestion

(defvar PLACE-MENU-CONTRAINTES 0)
(defvar PLACE-MENU-FIGER 2)
(defvar PLACE-MENU-DEFIGER 3)
(defvar PLACE-MENU-PLACER 5)
(defvar PLACE-MENU-DEPLACER 6)
(defvar PLACE-MENU-RETIRER-NON-FIGER 7)
(defvar PLACE-MENU-RETIRER-SALLES 8)

;; Afficher

(defvar PLACE-MENU-AFFICHER-FAMILLES 0)
(defvar PLACE-MENU-AFFICHER-SALLE 1)
(defvar PLACE-MENU-AFFICHER-PROF 2)
(defvar PLACE-MENU-OPTIONS 4)
(defvar PLACE-MENU-ECHELLE 5)
(defvar PLACE-MENU-SEM-PREC 6)
(defvar PLACE-MENU-SEM-SUIV 7)
(defvar PLACE-MENU-REAFFICHER 9)

;; Donnees

(defvar PLACE-MENU-DONNEES-ETABLISSEMENT 0)
(defvar PLACE-MENU-DONNEES-COURS 1)
(defvar PLACE-MENU-DONNEES-PROF 2)
(defvar PLACE-MENU-DONNEES-SALLES 3)
(defvar PLACE-MENU-DONNEES-FAMILLES 4)
(defvar PLACE-MENU-DONNEES-RESA 5)
(defvar PLACE-MENU-DONNEES-ANNUL 6)
(defvar PLACE-MENU-DONNEES-AJOUT 7)
(defvar PLACE-MENU-DONNEES-ERREUR 8)
(defvar PLACE-MENU-DONNEES-STATS 9)

;; Outils

(defvar PLACE-MENU-FORMAT-IMPRESSION 0)
(defvar PLACE-MENU-IMPRIMER 1)
(defvar PLACE-MENU-GESTION-TD 3)
(defvar PLACE-MENU-RESA 4)
(defvar PLACE-MENU-RACO 5)
(defvar PLACE-MENU-EDITION 6)
(defvar PLACE-MENU-PERMUT 7)
(defvar PLACE-MENU-MANIP 8)
(defvar PLACE-MENU-FERIE 9)

;;;   sous-menu Rattrapage

(defvar PLACE-MENU-SUPPRIMER-ANNUL 1)
(defvar PLACE-MENU-SUPPRIMER-AJOUT 3)

;;; Place des menus (mode saisie)

;; Fichier

(defvar PLACE-MENU-ETABLISSEMENT 7)
(defvar PLACE-MENU-SEMAINE 8)
(defvar PLACE-MENU-PAUSE 9)
(defvar PLACE-MENU-SITE 10)

;; Contraintes

(defvar PLACE-MENU-NON-CHEVAUCHEMENT 0)
(defvar PLACE-MENU-PRECEDENCE 1)
(defvar PLACE-MENU-MEME-SEMAINE 2)
(defvar PLACE-MENU-JOURS-DIFFERENTS 3)
(defvar PLACE-MENU-PROXIMITE-SEMAINE 4)
(defvar PLACE-MENU-ANALYSE-DISPO 6)
(defvar PLACE-MENU-ANALYSE-CONTR 7)
(defvar PLACE-MENU-ERREUR 9)

; barre de menu du mode resolution

(defun make-resolution-submenus ()
  (let 
    ((menubar (component 'menu-bar Gen-App))

     ;; Fichier
      
     (menuv0 
       (create-aidamenu ()
         (encapsuler-menu-chaine #Mv_dir-nou) 'dir-new
         (encapsuler-menu-chaine #Mv_dir-dir) 'dir-load
	 (encapsuler-menu-chaine #Mv_dir-sav) 'dir-sav
	 (encapsuler-menu-chaine #Mv_dir-sav-as) 'dir-sav-as
         (encapsuler-menu-chaine #Mv_aff-lig) ()
	 (encapsuler-menu-chaine #Mv_mod-saisie) 'mod-sai
         (encapsuler-menu-chaine #Mv_aff-lig) ()	 
         (encapsuler-menu-chaine #Mv_dir-a-propos) 'dir-apropos
         (encapsuler-menu-chaine #Mv_dir-qui) 'dir-quit))

     ;; Resolution
     
     (menuv1 
       (create-aidamenu ()
	 (encapsuler-menu-chaine #Mv_res-totale) 'resolution-complete
	 (encapsuler-menu-chaine #Mv_res-partiel)
	 'resol-partielle-avec-salles
	 (encapsuler-menu-chaine #Mv_aff-lig4) ()
	 (encapsuler-menu-chaine #Mv_res-cours-total) 'resolution-cours
	 (encapsuler-menu-chaine #Mv_res-cours-partiel)
	 'resol-partielle-sans-salles
	 (encapsuler-menu-chaine #Mv_res-salle) 'affectation-salles))	

     ;; Gestion
     
     (menuv2 
       (create-aidamenu ()
	 (encapsuler-menu-chaine #Mv_con) ()
	 (encapsuler-menu-chaine #Mv_aff-lig3) ()
	 (encapsuler-menu-chaine #Mv_coc-fig) 'con-fig
	 (encapsuler-menu-chaine #Mv_coc-defig) 'con-defig
	 (encapsuler-menu-chaine #Mv_aff-lig3) ()
	 (encapsuler-menu-chaine #Mv_coc-placer) 'con-placer
	 (encapsuler-menu-chaine #Mv_coc-deplacer) 'con-deplacer
	 (encapsuler-menu-chaine #Mv_coc-effacer) 'con-desinstancier
	 (encapsuler-menu-chaine #Mv_coc-desaffecter) 'con-desaffecter))

     ;; Gestion : Contraintes explicites

     (sous-menuv2-1
       (create-aidamenu ()
	 (encapsuler-menu-chaine #Mv_coc-non) 'con-nsimul
	 (encapsuler-menu-chaine #Mv_coc-pre) 'con-preced
	 (encapsuler-menu-chaine #Mv_coc-meme) 'con-memesem
	 (encapsuler-menu-chaine #Mv_coc-jour) 'con-jourdif
	 (encapsuler-menu-chaine #Mv_coc-prox) 'con-proxsem))
     
     ;; Affichage
     
     (menuv3 
       (create-aidamenu ()
	 (encapsuler-menu-chaine #Mv_aff-gro) 'afficher-familles
	 (encapsuler-menu-chaine #Mv_aff-sal) 'afficher-salles
	 (encapsuler-menu-chaine #Mv_aff-pro) 'afficher-profs
	 (encapsuler-menu-chaine #Mv_aff-lig) ()
	 (encapsuler-menu-chaine #Mv_aff-option) ()
	 (encapsuler-menu-chaine #Mv_aff-echelle) ()
	 (encapsuler-menu-chaine #Mv_aff-sem-prec) 'aff-precedent
	 (encapsuler-menu-chaine #Mv_aff-sem-suiv) 'aff-suivant
	 (encapsuler-menu-chaine #Mv_aff-lig) ()  
	 (encapsuler-menu-chaine #Mv_aff-rea) 'aff-refresh))

     ;; Affichage : Option
     
     (sous-menuv3-1
       (create-aidamenu ()
	 (encapsuler-menu-chaine #Mv_aff-sem) 'aff-week
	 (encapsuler-menu-chaine #Mv_aff-pla) 'aff-pla
	 (encapsuler-menu-chaine #Mv_aff-lig2) ()
	 (encapsuler-menu-chaine #Mv_aff-partiel) 'aff-partiel
	 (encapsuler-menu-chaine #Mv_aff-complet) 'aff-complet
	 (encapsuler-menu-chaine #Mv_aff-lig2) ()
	 (encapsuler-menu-chaine #Mv_aff-sans-chevauchement)
	 'aff-sans-chevauchement
	 (encapsuler-menu-chaine #Mv_aff-avec-chevauchement)
	 'aff-avec-chevauchement))

     ;; Affichage : Echelle
     
     (sous-menuv3-2
       (create-aidamenu ()
	 (encapsuler-menu-chaine #Mv_aff-reduire) 'rapetir-grille
	 (encapsuler-menu-chaine #Mv_aff-agrandir) 'agrandir-grille
	 (encapsuler-menu-chaine #Mv_aff-ecran) 'retailler-grille
	 (encapsuler-menu-chaine #Mv_aff-pourcent) 'echelle-grille))

     ;; Donnees
     
     (menuv4 
       (create-aidamenu ()
	 (encapsuler-menu-chaine #Mv_don-eco) 'aff-ecole
	 (encapsuler-menu-chaine #Mv_don-cou) ()
	 (encapsuler-menu-chaine #Mv_don-pro) ()
	 (encapsuler-menu-chaine #Mv_don-sal) ()
	 (encapsuler-menu-chaine #Mv_don-gro) 'aff-groupes
	 (encapsuler-menu-chaine #Mv_don-res) 'aff-resas
	 (encapsuler-menu-chaine #Mv_don-ann) 'aff-annulations
	 (encapsuler-menu-chaine #Mv_don-ajt) 'aff-ajouts
	 (encapsuler-menu-chaine #Mv_last-error) 'aff-derniere-erreur
	 (encapsuler-menu-chaine #Mv_stats) 'afficher-statistiques))

     ;; Donnees : Cours

     (sous-menuv4-1
       (create-aidamenu ()
	 (encapsuler-menu-chaine #Mv_don-tous) 'aff-cours
	 (encapsuler-menu-chaine #Mv_don-csal) 'aff-cours-sans-salle
	 (encapsuler-menu-chaine #Mv_don-cours-souhait)
	 'aff-cours-souhaits-non-respectes
	 (encapsuler-menu-chaine #Mv_don-cours-sans-prof)
	 'aff-cours-sans-prof
	 (encapsuler-menu-chaine #Mv_don-cours-type) 'aff-cours-type))

     ;; Donnees : Professeurs

     (sous-menuv4-2
       (create-aidamenu ()
	 (encapsuler-menu-chaine #Mv_don-tous) 'aff-profs
	 (encapsuler-menu-chaine #Mv_don-prof-sans-cours)
	 'aff-profs-sans-cours))

     ;; Donnees : Salles

     (sous-menuv4-3
       (create-aidamenu ()
	 (encapsuler-menu-chaine #Mv_don-tous) 'aff-salles
	 (encapsuler-menu-chaine #Mv_don-type-salle) 'aff-salles-type))
     
     ;; Outils
     
     (menuv5  
       (create-aidamenu ()
	 (encapsuler-menu-chaine #Mv_dir-device) 'dir-device
	 (encapsuler-menu-chaine #Mv_dir-print) ()
	 (encapsuler-menu-chaine #Mv_aff-lig2) ()
	 (encapsuler-menu-chaine #Mv_outils-td) ()
	 (encapsuler-menu-chaine #Mv_outils-resa) ()
	 (encapsuler-menu-chaine #Mv_outils-raco) ()
	 (encapsuler-menu-chaine #Mv_outils-edition) ()
	 (encapsuler-menu-chaine #Mv_outils-permut) 'permuter-seances
	 (encapsuler-menu-chaine #Mv_outils-manip) ()
	 (encapsuler-menu-chaine #Mv_outils-ferie) ()))


     ;; Outils : Imprimer

     (sous-menuv5-0
       (create-aidamenu ()
	 (encapsuler-menu-chaine #Mv_outils-imp-ecran) 'dir-print
	 (encapsuler-menu-chaine #Mv_aff-lig2) ()	 
	 (encapsuler-menu-chaine #Mv_outils-imp-salles) 'dir-lsalles
	 (encapsuler-menu-chaine #Mv_outils-imp-profs) 'dir-lprofs))

     
     ;; Outils : Gestion des TD / TP

     (sous-menuv5-1
       (create-aidamenu ()
	 (encapsuler-menu-chaine #Mv_outils-td-creation) 'creer-groupe-td
	 (encapsuler-menu-chaine #Mv_outils-td-suppression)
	 'supprimer-groupe-td))

     ;; Outils : Reservation de salle
     
     (sous-menuv5-2
      (create-aidamenu ()
	(encapsuler-menu-chaine #Mv_outils-resa-config) 'configurer-resa
	(encapsuler-menu-chaine #Mv_outils-resa-demander) 'demande-reservation
	(encapsuler-menu-chaine #Mv_outils-resa-afficher) 'aff-res))

     ;; Outils : Rattrapage de cours

     (sous-menuv5-3
      (create-aidamenu ()
        (encapsuler-menu-chaine #Mv_outils-raco-demander-annulation)
	'demander-annulation-seance
        (encapsuler-menu-chaine #Mv_outils-raco-supprimer-annulation)
	'supprimer-annulation-seance
        (encapsuler-menu-chaine #Mv_outils-raco-demander-ajout)
	'demander-ajout-seance
        (encapsuler-menu-chaine #Mv_outils-raco-supprimer-ajout)
	'supprimer-ajout-seance 
        (encapsuler-menu-chaine #Mv_outils-raco-afficher-ajout) 'aff-ajt))

     ;; Outils : Editions
          
     (sous-menuv5-4
       (creer-sous-menu-editions))

     ;; Outils : Traitement de donnees

     (sous-menuv5-5
       (create-aidamenu ()
	 (encapsuler-menu-chaine #Mv_outils-manip-2standard)
	 'transformer-famille2famille-standard
	 (encapsuler-menu-chaine #Mv_aff-lig5) ()	 
	 (encapsuler-menu-chaine #Mv_outils-manip-raz-dispo-cours)
	 'raz-dispo-cours
	 (encapsuler-menu-chaine #Mv_outils-manip-raz-dispo-prof)
	 'raz-dispo-profs
	 (encapsuler-menu-chaine #Mv_outils-manip-raz-dispo-famille)
	 'raz-dispo-familles
	 (encapsuler-menu-chaine #Mv_aff-lig5) ()	 
	 (encapsuler-menu-chaine #Mv_outils-manip-sup-resas)
	 'supprimer-toutes-reservations
	 (encapsuler-menu-chaine #Mv_outils-manip-sup-annuls)
	 'supprimer-toutes-annulations
	 (encapsuler-menu-chaine #Mv_outils-manip-sup-ajouts)
	 'supprimer-tous-ajouts
	 (encapsuler-menu-chaine #Mv_outils-manip-sup-liens-prof)
	 'supprimer-liens-prof))

     (sous-menuv5-6
       (create-aidamenu ()
	 (encapsuler-menu-chaine #Mv_annuler-journee) 'annuler-journee
	 (encapsuler-menu-chaine #Mv_dep_journee) 'deplacer-journee
	 (encapsuler-menu-chaine #Mv_dep-cours-journee) 'deplacer-cours-journee
	 (encapsuler-menu-chaine #Mv_aff-lig5) ()	 
	 (encapsuler-menu-chaine #Mv_sup-annul-journee)
	 'supprimer-annuler-journee
	 (encapsuler-menu-chaine #Mv_sup-ajt-journee)
	 'supprimer-ajouter-journee)))
    
    ;; Raccordement des menus
    
    (aidamenu-attach-submenu menubar 0 menuv0)
    (aidamenu-attach-submenu menubar 1 menuv1)
    (aidamenu-attach-submenu menubar 2 menuv2)
    (aidamenu-attach-submenu menuv2 0 sous-menuv2-1)
    (aidamenu-attach-submenu menubar 3 menuv3)
    (aidamenu-attach-submenu menuv3 4 sous-menuv3-1)
    (aidamenu-attach-submenu menuv3 5 sous-menuv3-2)
    (aidamenu-attach-submenu menubar 4 menuv4)
    (aidamenu-attach-submenu menuv4 1 sous-menuv4-1)
    (aidamenu-attach-submenu menuv4 2 sous-menuv4-2)
    (aidamenu-attach-submenu menuv4 3 sous-menuv4-3)    
    (aidamenu-attach-submenu menubar 5 menuv5)
    (aidamenu-attach-submenu menuv5 1 sous-menuv5-0)
    (aidamenu-attach-submenu menuv5 3 sous-menuv5-1)
    (aidamenu-attach-submenu menuv5 4 sous-menuv5-2)
    (aidamenu-attach-submenu menuv5 5 sous-menuv5-3)
    (aidamenu-attach-submenu menuv5 6 sous-menuv5-4)
    (aidamenu-attach-submenu menuv5 8 sous-menuv5-5)
    (aidamenu-attach-submenu menuv5 9 sous-menuv5-6)        
   
    (add-component Gen-App 'menu-direct menuv0)
    (add-component Gen-App 'menu-resol menuv1)
    (add-component Gen-App 'menu-gestion menuv2)
    (add-component Gen-App 'menu-affich menuv3)
    (add-component Gen-App 'menu-data menuv4)
    (add-component Gen-App 'menu-outils menuv5)
    (add-component Gen-App 'sous-menu-affich-options sous-menuv3-1)
    (add-component Gen-App 'sous-menu-outils-manip sous-menuv5-5)
    (add-component Gen-App 'sous-menu-outils-ferie sous-menuv5-6)
    (add-component Gen-App 'sous-menu-outils-td sous-menuv5-4)
    (add-component Gen-App 'sous-menu-outils-reservation sous-menuv5-2)
    (add-component Gen-App 'sous-menu-outils-raco sous-menuv5-3)
    (add-component Gen-App 'sous-menu-outils-edi sous-menuv5-1)))
    
; barre de menu du mode saisie

(defun make-input-submenus ()
  (let 
    ((menubar (component 'menu-bar Input-App))

      ;; Fichier
      
      (menuv0 
        (create-aidamenu ()
          (encapsuler-menu-chaine #Mv_dir-nou) 'dir-new
          (encapsuler-menu-chaine #Mv_dir-dir) 'dir-load
	  (encapsuler-menu-chaine #Mv_dir-sav) 'dir-sav
          (encapsuler-menu-chaine #Mv_dir-sav-as) 'dir-sav-as
          (encapsuler-menu-chaine #Mv_aff-lig) ()
	  (encapsuler-menu-chaine #Mv_mod-resolution) 'mod-res
          (encapsuler-menu-chaine #Mv_aff-lig) ()	  
          (encapsuler-menu-chaine #Mv_don-eco) 'don-ecole
	  (encapsuler-menu-chaine #Mv_semaines-calendaires)
	  'don-semaines-calendaires
	  (encapsuler-menu-chaine #Mv_pause) 'don-pause
	  (encapsuler-menu-chaine #Mv_sites) ()
          (encapsuler-menu-chaine #Mv_aff-lig) ()
          (encapsuler-menu-chaine #Mv_dir-a-propos) 'dir-apropos
          (encapsuler-menu-chaine #Mv_dir-qui) 'dir-quit))

     ;; Fichier : Sites

      (sous-menuv0-1
	(create-aidamenu ()
	  (encapsuler-menu-chaine #Mv_creer-site) 'don-creer-site
      	  (encapsuler-menu-chaine #Mv_modif-site) 'don-modif-site
	  (encapsuler-menu-chaine #Mv_suppr-site) 'don-suppr-site))
      
      ;; Contraintes
      
      (menuv1
        (create-aidamenu ()
          (encapsuler-menu-chaine #Mv_coc-non) 'con-nsimul
          (encapsuler-menu-chaine #Mv_coc-pre) 'con-preced
          (encapsuler-menu-chaine #Mv_coc-meme) 'con-memesem
          (encapsuler-menu-chaine #Mv_coc-jour) 'con-jourdif
          (encapsuler-menu-chaine #Mv_coc-prox) 'con-proxsem
	  (encapsuler-menu-chaine #Mv_aff-lig3) ()
	  (encapsuler-menu-chaine #Mv_analyse-dispo)
	  'analyse-propagation-dispo
	  (encapsuler-menu-chaine #Mv_analyse-contraintes)
	  'analyse-pose-contraintes
	  (encapsuler-menu-chaine #Mv_aff-lig3) ()	  
	  (encapsuler-menu-chaine #Mv_last-error) 'aff-derniere-erreur)))

    (aidamenubar-attach-submenu menubar 0 menuv0)
    (aidamenu-attach-submenu menuv0 10 sous-menuv0-1)    
    (aidamenubar-attach-submenu menubar 1 menuv1)

    (add-component Input-App 'menu-direct menuv0)
    (add-component Input-App 'menu-contr menuv1)))


;; construction de la barre de menu principale

(defun make-resolution-menubar ()
  (create-aidamenubar
      (encapsuler-menu-chaine #Mv_dir) ()
      (encapsuler-menu-chaine #Mv_resolution) ()
      (encapsuler-menu-chaine #Mv_gestion) ()      
      (encapsuler-menu-chaine #Mv_aff) ()
      (encapsuler-menu-chaine #Mv_don) ()
      (encapsuler-menu-chaine #Mv_outils) ()))
  
(defun make-input-menubar ()
  (create-aidamenubar
    (encapsuler-menu-chaine #Mv_dir) ()
    (encapsuler-menu-chaine #Mv_con) ()))


(defun place-varuna-menu (appli menu)
  (cond
    ((eq appli gen-app)
     (selectq menu
      ('menu-direct 0)
      ('menu-resol 1)
      ('menu-gestion 2)
      ('menu-affich 3)
      ('menu-data 4)
      ('menu-outils 5)
      (t ())))
    ((eq appli input-app)
     (selectq menu
       ('menu-direct 0)
       ('menu-contr 2)
       (t ())))
    (t ())))


;;; LES UTILITAIRES GENERAUX SUR LES MENUS

;; quelques opérations sur les menus
;; inhibition d'un item a faire...

(defun inhibit-menu (menu)
  (let ((i 0))
    (inhibit-application menu)
    (mapc (lambda (item)
      (authorize-menu-item menu item i t)
      (setq i (add1 i)))
    (send 'items menu))))

(defun authorize-menu-item (menu item rank . inhibit)
  (aidamenu-modify-item menu () rank ()
    (if (car inhibit) 0 1)
    (send 'value item)))


(defun authorize-menu (menu)
  (let ((i 0))
    (authorize-application menu)
    (mapc (lambda (item) (authorize-menu-item menu item i)
      (setq i (add1 i)))
    (send 'items menu))))


;;; AJT (27/01/95) MO
;;; primitive donnant la derniere image ou string de la structure objet
;;; ie
;;; on regarde l'image de l'objet-image ; alors:
;;; si cette image n'existe pas, on renvoie l'objet
;;; si cette image est une string, on s'arrete en renvoyant la chaine
;;; sinon, on re-itere sur cette image pris comme objet
;;; REM (27/01/95) MO
;;; primitive a mettre dans uaida a terme

(defun derniere-image-objet (objet)
  (let 
    ((type (type-of objet)))
    (cond
      ((eq type 'null)())
      ((eq type 'string) objet)
      ((member 'image (field-list type)) 
       (derniere-image-objet (send 'image objet)))
      (t objet))))

;;; primitive qui renvoie la chaine representant l'item

(defun capter-chaine-item (item)
  (derniere-image-objet item))


;;; encapsule une chaine de type string par une font et un foreground

(defun v-chaine (chaine couleur police)
  (foreground couleur (font police chaine)))


;;; n-item renvoie le <num-item> item de <menu>

(defun n-item (menu num-item)
  (nth num-item (send 'items menu)))


;;; primitive permettant d'inhiber le <item>-eme item de <menu> en le grisant et

(defun inhiber-item (menu item)
  (aidamenu-modify-item 
     menu () item 
     (v-chaine 
       (capter-chaine-item (n-item menu item)) 
       couleur-item-inhibe
       arial8b)
      0 
      (send 'value (n-item menu item))))

;;;  ajout du 02/02/96
;;;  cette fonction doit remplacer la precedente

(defun inhiber-item-couleur (menu item couleur)
  (aidamenu-modify-item 
     menu () item 
     (v-chaine 
       (capter-chaine-item (n-item menu item)) 
       couleur
       arial8b)
      0 
      (send 'value (n-item menu item))))


;;; primitive activant le <item>-ieme item de <menu>

(defun activer-item (menu item)
  (aidamenu-modify-item 
     menu () item 
     (v-chaine 
       (capter-chaine-item (n-item menu item)) 
       couleur-item-actif
       arial8b)
     1 
     (send 'value (n-item menu item))))



;;; PRIMITIVES PARTICULIERES A VARUNA

;;;

(defun inhiber-varuna-menu (appli nom-menu liste-item)
  (if
    liste-item
    (let 
      ((menu (send 'component nom-menu appli))
       (liste-travail liste-item))
      (while liste-travail
        (inhiber-item menu (nextl liste-travail))))
    (inhiber-item 
      (send 'component 'menu-bar appli) 
      (place-varuna-menu appli nom-menu)))) 

;;; AJT 02/02/96
;;;  cette fonction devrait remplacer la precedente pour pouvoir avoir
;;; differentes couleurs d'inhibition

(defun inhiber-varuna-menu-couleur (appli nom-menu liste-item couleur)
  (if
    liste-item
    (let 
      ((menu (send 'component nom-menu appli))
       (liste-travail liste-item))
      (while liste-travail
	;; modif par rapport a la fct precedente: couleur
        (inhiber-item-couleur menu (nextl liste-travail) couleur)))
    (inhiber-item 
      (send 'component 'menu-bar appli) 
      (place-varuna-menu appli nom-menu)))) 

;;;

(defun activer-varuna-menu (appli nom-menu liste-item)
  (if
    liste-item
    (let 
      ((menu (send 'component nom-menu appli))
       (liste-travail liste-item))
      (while liste-travail
        (activer-item menu (nextl liste-travail))))
    (activer-item 
      (send 'component 'menu-bar appli) 
      (place-varuna-menu appli nom-menu))))         

;;; !!! (22/06/95) MO a mettre dans frontal... voire vars.ll
(defvar Fichier-menu "menu-edi.cfg")

;;; CRE (15/06/95) MO <creer-sous-menu-editions>
;;; il faudra utiliser REP-DONNEES quand cela sera a niveau

(defun creer-sous-menu-editions ()
  (with
    ((current-directory (string (getenv "VARUNA"))))
    
    (if
      ;; FD - 08/09/95 - il y a "plantage" si le fichier menu est vide
      (and
        (probefile FICHIER-MENU)
        (not (fichier-vide-p FICHIER-MENU)))
      (sous-menu-editions FICHIER-MENU)
      (setq MODULE-EDITIONS ()))))


(defun sous-menu-editions (fichier)
  (let
    ((menu (create-aidamenu ()))
     (tamp1)
     (tamp2))
    (with
      ((inchan (openi fichier)))
      (untilexit EOF
	(setq tamp1 (encapsuler-menu-chaine (read)))
	(setq tamp2 (read))
	(aidamenu-insert-item menu () 0 tamp1 1 tamp2))
      (close))
    menu))

;;; CRE RM - 23/05/96 : encapsulation des chaines pour les menus
;;; meme chose en 640x480 qu'en 800x600

(defun encapsuler-menu-chaine (chaine)
  (font arial8b chaine))
