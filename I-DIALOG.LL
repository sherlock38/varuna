;;; --------------------------------------------------------------------------
;;; - I-Dialog.ll                                               VARUNA 1.2   -
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; - Interface Graphique                                                    -
;;; - Bibliotheque de dialogues                                              -
;;; --------------------------------------------------------------------------
;;; - Creation: Janvier 94                                      MO           -
;;; - Mise a jour pour version 1.13:                                         -
;;; - Build 1.22.1 01.07.99 / JH MO                                          -
;;; --------------------------------------------------------------------------
;;; FD - RM - 04/04/96 
;;;       remplacement de get-message par #M
;;;       pb impression:
;;;         ajout d'un Ctrl D pour reinitialiser l'imprimante (si on envoyait
;;;         plusieurs impression a la suite a partir de la 2eme, nous avions
;;;         des decalages, chgmnt orientation, taille ...)
;;;         Nous etions oblige de cliquer deux fois sur paysage pour avoir
;;;         une impression en paysage (pb variable locale different de var
;;;         globale).

;; constantes pour les dialogues
;; ces definitions doivent figurer dans une fonction
;; sinon, problemes a la compilation et valeurs par defaut
;; minvisibleobjects = 6 et ok = d'accord
;; RM FD - MOD - 23/08/95
;; ajout de la redefinition du l'image du bouton cancel d'un "asker"
;; remplacement de "OK" par #Mv_ok

(defun init-dialog ()
  (setq {asker}:minvisibleobjects 15)
  (setq {asker}:ok (font FONT-EDITEUR (cadrer-centrer #Mv_ok 10)))
  (setq {asker}:cancel (font FONT-EDITEUR (cadrer-centrer #Mv_annuler 10))))

;;; Pour pouvoir avoir des dialogues (Promp-warn...) avec le titre
;;; de l'application , il  
;;; est necessaire de redefinir des dialogues propres a l'appli.

;;;
;;; I. Macro generale d'ecriture de dialogues munis de titres
;;;
;;; la liste est du type
;;;   (titre-fenetre type-dialogue premier_argument - autres_arguments)
;;; AJT - FD - 19/09/95 - ajout d'une couleur de fond

(dmd afficher-dialogue (couleur . liste)
  `(let
    ((dialogue ,(cdr liste)))
    (with ((current-background ,couleur))
      (send 'set-title dialogue ,(car liste))
      (with ((current-font FONT-EDITEUR))
        (grab-and-select-application dialogue))
      ({asker}:get-answers dialogue))))

;;;
;;; II Primitives specifiques a une application
;;;

;;; dialogue <dialogue-confirmation> assimile a <prompt-confirm>
;;; MOD - FD RM - 16/08/95 - encapsulation de la font
; (defun afficher-confirmation (image)
;   (with
;     ((current-font FONT-EDITEUR))
;     (afficher-dialogue
;       (trouver-couleur-varuna "jaune-pastel")
;       #Mv_titre-confirmation
;       confirmer
;     image
;       ())))

;;; dialogue <dialogue-warning> assimile a <prompt-warn>

; (defun afficher-warning (image)
;   (with
;     ((current-font FONT-EDITEUR))
;     (afficher-dialogue
;       (trouver-couleur-varuna "jaune-pastel")
;       #Mv_titre-warning
;       warner
;       image
;       ())))

;; dialogue de demande de chaine de caractere

(defun afficher-demande-valeur (image chaines o liste-boutons)
  (car
    (with ((current-font FONT-EDITEUR))
      (afficher-dialogue
        (make-named-color "white")
        #Mv_titre-demande
        string-asker
        image
        chaines
        o
        liste-boutons))))

;; dialogue de demande standard
;; la gestion du dialogue doit etre implementee dans l'appli
;; la fonction renvoie le champ <answers>

(defun afficher-demandes (appli o liste-boutons)
  (with ((current-font FONT-EDITEUR))
    (afficher-dialogue
      (make-named-color "white")
      #Mv_titre-demande
      object-asker
      appli
      ()
      liste-boutons)))

;;; meme chose, mais
;;; la fonction renvoie uniquement la premiere reponse

(defun afficher-demande (image o liste-boutons)
  (car (afficher-demandes image o liste-boutons)))

;; dialogue de demande dans une liste (sans hyperselectionneur)

(defun afficher-demande-liste-simple (image liste liste-boutons)
  (car
    (with ((current-font FONT-EDITEUR))
      (afficher-dialogue
        (make-named-color "white")
        #Mv_titre-demande
        string-asker
        image
        liste
        ()
        liste-boutons))))

;; dialogue de demande d'une liste dans une liste (sans hypersel)
(defun afficher-demandes-liste-simple (image liste liste-boutons)
  (with ((current-font FONT-EDITEUR))
    (afficher-dialogue
      (make-named-color "white")
      #Mv_titre-demande
      strings-asker
      image
      liste
      ()
      1
      (length liste)
      ()
      liste-boutons)))

;;; dialogue <oui-non-annuler> utilise pour enregistrement... !!re-utiliser la macro
;;; affiche un dialogue avec le prompt
;;; et les trois choix OUI, NON, ANNULER
;;; AJT - FD - 19/09/95 - couleur de fond
;;; MOD RM - 20/08/96 : encapsulation de la font

(defun oui-non-annuler (prompt)
  (let*
    ((b-annuler (standardbutton
      (font FONT-EDITEUR (cadrer-centrer #Mv_annuler 10))))
     ({asker}:ok (font FONT-EDITEUR (cadrer-centrer #Mv_oui 10)))
     ({asker}:cancel (font FONT-EDITEUR (cadrer-centrer #Mv_non 10)))
     (dialogue (confirmer prompt (list b-annuler)))) 
     ; !! presence de list
    
    ;; action du bouton annuler: normalement la prim doit renvoyer zero
    (set-action b-annuler 
        (lambda (b)
          (let 
            ((asker (component 'asker b)))
            ;; rem FD/MO 15/9/95... il faut une liste dans le set-answer[s]
            ;; on n'y crois pas.... Amen ILOG
            ({asker}:set-answers asker `,'(0)) 
            (remove-application asker))))
    
    (with ((current-background (trouver-couleur-varuna "jaune-pastel")))
      (send 'set-title dialogue #Mv_titre-confirmation)
      (grab-and-select-application dialogue)

      ;; rem FD MO 15/09/95
      ;; il semblerait que la ligne suivante existe pour ne pas avoir
      ;; de message du bouton ok du type "oui".... dans un let BOFBOFBOFBOF
      ;;(setq {asker}:ok nil {asker}:cancel nil)
      ;; on recette un coup sans cette ligne

      ({asker}:get-answer dialogue))))


;;; dialogue <prompt-confirm-string>, !!a re-ecrire avec la macro
;;; Affiche une dialogue avec le chois entre deux chaines
;;; (dans des boutons radio)
;;; renvoie la chaine choisie ou ()
;;; AJT - FD - 19/09/95 - couleur de fond
;;; MOD RM - 20/08/96 : encapsulation de la font

(defun prompt-confirm-string (prompt titre string1 string2)
  (let*
    (({asker}:ok (font FONT-EDITEUR (cadrer-centrer string1 10)))
     ({asker}:cancel (font FONT-EDITEUR (cadrer-centrer string2 10)))
     (dialogue (confirmer prompt ())))
    
    (with ((current-background (trouver-couleur-varuna "jaune-pastel")))
      (send 'set-title dialogue titre)
      (grab-and-select-application dialogue)
      (setq {asker}:ok nil {asker}:cancel nil)
      ({asker}:get-answer dialogue))))
        
;;; affiche un dialogue avec image et deux bouton oui et non
        
(defun prompt-oui-non (image)
  (prompt-confirm-string image #Mv_titre-confirmation #Mv_oui #Mv_non))

;;; affiche un dialogue avec image et deux boutons continuer et annuler

(defun prompt-continuer (image)
  (prompt-confirm-string
    image #Mv_titre-confirmation #Mv_continuer #Mv_annuler))
      
      
;;; CRE RM - 15/09/95
;;; Boite de dialogue a trois boutons (CONFIRMER - ANNULER - 
;;; CONFIRMER et IMPRIMER)
;;; Retourne : t si OUI, nil si NON, 0 si OUI ET IMPRIMER
;;; AJT - FD - 19/09/95 - couleur de fond

(defun ok-annuler-imprimer (prompt)
  (let*
    ((b-autre 
      (standardbutton 
        (font FONT-EDITEUR (cadrer-centrer #Mv_conf-imp 20))))
    ({asker}:ok (font FONT-EDITEUR (cadrer-centrer #Mv_oui 20)))
    ({asker}:cancel (font FONT-EDITEUR(cadrer-centrer #Mv_non 20)))
    (dialogue (confirmer prompt (list b-autre))))
    (set-action b-autre
      (lambda (b)
        (let 
          ((asker (component 'asker b)))
          ({asker}:set-answers asker `,'(0))
          (remove-application asker))))
    (with ((current-background (trouver-couleur-varuna "jaune-pastel")))
      (send 'set-title dialogue #Mv_titre-confirmation)
      (grab-and-select-application dialogue)
      ({asker}:get-answer dialogue))))


;;; cree un file-selector en modifiant quelques comportements standards
;;; appelle validate-ask-directory et la fonction utilisateur
;;; tester-repertoire avec comme parametre le nom du repertoire

;;; MOD RM - 12/03/97 : possibilite de creer un rep.

(defun get-directory (prompt tester-repertoire)
  (let 
    ((selecteur
      (let*
     
        ((appli (fileselector 0 0 300 300))
          (t-chb (div 100 #wchar))
          (b-new (standardbutton (cadrer-centrer #Mv_dir-nou t-chb)))
          (b-ok (standardbutton (cadrer-centrer #Mv_ok t-chb)))
          (b-annuler (standardbutton (cadrer-centrer #Mv_annuler t-chb))))

        (set-action b-new
          (lambda (a)
            (let
              ((nom-rep (demande-chaine "Entrez le nom du répertoire : "))
               (rep-courant
                 (get-fileselector-current-directory
                   (component 'ask-file a)))
               (status))
              (when nom-rep
                (setq status
                  (funcall 'creer_repertoire (catenate rep-courant nom-rep)))
                (if (eq status -1)
                  (afficher-erreur "Création du repertoire impossible.")
                  (remove-application (component 'app a))
                  ({application}:userdata (component 'ask-file a)
                    (catenate rep-courant nom-rep "\")))))))
                  
        (set-action b-annuler
          (lambda (a) (remove-application (component 'app a)) ()))
        
        (set-action b-ok
          (lambda (a)
            (let*
              ((valid-dir
                (funcall
                  tester-repertoire
                  (get-fileselector-current-directory
                    (component 'ask-file a)))))
              (when
                valid-dir
                (remove-application (component 'app a))
                ({application}:userdata (component 'ask-file a)
                  valid-dir)))))
        
        (set-fileselector-current-directory 
          appli 
          (current-directory))
        
        (set-fileselector-filter appli "*.edt")
        (mixed-applicationq 
          name 'app
          ask-file appli
          image
          (column appli
            (elasticrow b-ok b-annuler b-new))))))
    (send 'set-title selecteur prompt)
    (grab-and-select-application selecteur)
    ({application}:userdata (component 'ask-file selecteur))))


;;; demande le choix entre deux images
;;; avec deux boutons radio (passer a n choix)

(dmd demande-choix (choix1 . l-choix) 
  `(let*
    ((bouton1 (radiobutton 0 0 250 #1hchar (get-message ',choix1)))
      (liste-boutons (cons bouton1
        (mapcar (lambda (message)
          (radiobutton 0 0 250 #1hchar (get-message message))) ',l-choix)))
      (panneau
        (selectorapplication 0 0 250 ,(mul #1hchar (add1 (length l-choix)))
          (apply 'column liste-boutons))))
    (send 'set-selection panneau bouton1)
    (and (afficher-confirmation panneau)
      (let ((select (send 'get-selection panneau)))
        (when select
          (send 'title select))))))

;;; TRV - FD - 19/09/95
;;; boite de dialogue permettant d'initialiser des globales booleennes
;;; arg:
;;;     - titre : titre de la fenetre
;;;     - choix : liste d'options (booleens) sous de la forme de
;;;               paire (libelle variable)
;;;               variable etant une globale


(dmd afficher-options (titre . choix)
  `(let
    ((appli)
     (boite-a-cocher)
     (valeur))
      
    (setq
      liste-options
      (mapcar
        (lambda (option)
          (setq
            boite-a-cocher 
            (checkbox 0 0 150 20 (get-message (car option))))
          (send 'checked boite-a-cocher
            (eval (cadr option)))               ;; eval !!!
          ;       (setq valeur (cadr option))
          ;       (set-action boite-a-cocher
          ;         (lambda (cb)
          ;           (set valeur (send 'checkedp boite-a-cocher))))
          boite-a-cocher)
        ',choix))
    
    (setq appli
          (mixed-applicationq 
              name 'app
              image
              (row
                (apply 'column liste-options)
                (bouton-standard "OK" ()))))
               
    (send 'set-title appli ',titre)
    (add-application appli)))
      
  
;;; definition d'un dialogue de mise en page
;;; !!l'editeur utilise des utilitaires base de donnee 

(dmd make-device-accessor (champ)
  `(defun ,(concat champ '- 'device) (device . val)
     (ifn val 
       (and device (send ',champ device))
         (and device val (send ',champ device (car val))))))

(make-device-accessor nbrows)
(make-device-accessor nbcolumns)
(make-device-accessor orient)
(make-device-accessor scale-factor)
(make-device-accessor paper-width)
(make-device-accessor paper-height)
(make-device-accessor copies)

(defun boutons-portrait-paysage (orient)
  (with ((current-font FONT-EDITEUR))
    (let*
      ((portrait (radiobutton 0 0 100 #hchar #Mv_portrait))
        (paysage (radiobutton 0 0 100 #hchar #Mv_paysage))
        (appli
          (selectorapplication
            0 0
            (add 100 (add (send 'width portrait) (send 'width paysage)))
            #hchar
            (row portrait (translation 20 0 paysage)))))
    
      (send 'set-selection appli 
        (if (eq orient 90) paysage portrait))
      (set-action appli
        (lambda (a)
          (let
            ((device ({application}:userdata (component 'appli a)))
              (sel (send 'get-selection a)))
            (when sel
              (cond 
                ((eq (send 'title sel) #Mv_portrait)
                  (orient-device device 0))
                ((eq (send 'title sel) #Mv_paysage)
                  (orient-device device 90)))))))
      appli)))

;;; CRE RM - 19/08/96 : traitement des boutons radio de la boite
;;; <format d'impression> pour les champs couleur et postscript

(defun boutons-radio-couleur ()
  (with ((current-font FONT-EDITEUR))
    (let*
      ((bouton1 (radiobutton 0 0 100 #hchar #Mv_device-couleur-color))
        (bouton2 (radiobutton 0 0 100 #hchar #Mv_device-couleur-grey))
        (appli
          (selectorapplication
            0 0
            (add 100 (add (send 'width bouton1) (send 'width bouton2)))
            #hchar
            (row bouton1 (translation 20 0 bouton2)))))
      
      (send 'set-selection appli 
        (if COULEUR-DEVICE bouton1 bouton2))
      (set-action appli
        (lambda (a)
          (let
            ((sel (send 'title (send 'get-selection a))))
            (when sel
              (cond 
                ((eq sel #Mv_device-couleur-color)
                  (setq COULEUR-DEVICE t))
                ((eq sel #Mv_device-couleur-grey)
                  (setq COULEUR-DEVICE ())))))))
      appli)))

(defun boutons-radio-ps ()
  (with ((current-font FONT-EDITEUR))
    (let*
      ((bouton1 (radiobutton 0 0 100 #hchar #Mv_device-ps-yes))
        (bouton2 (radiobutton 0 0 100 #hchar #Mv_device-ps-no))
        (appli
          (selectorapplication
            0 0
            (add 100 (add (send 'width bouton1) (send 'width bouton2)))
            #hchar
            (row bouton1 (translation 20 0 bouton2)))))
      
      (send 'set-selection appli 
        (if POSTSCRIPT-DEVICE bouton1 bouton2))
      (set-action appli
        (lambda (a)
          (let
            ((sel (send 'title (send 'get-selection a))))
            (when sel
              (cond 
                ((eq sel #Mv_device-ps-yes)
                  (setq POSTSCRIPT-DEVICE t))
                ((eq sel #Mv_device-ps-no)
                  (setq POSTSCRIPT-DEVICE ())))))))
      appli)))

(defun device-asker (device)
  (let
    ((b-ok (standardbutton " OK "))
     (b-annuler (standardbutton #Mv_annuler))
     (b-nonps
       (standardbutton
         (cadrer-centrer "Commande non PS" 15)))
     (z-copies
       (iedit (or (copies-device device) 1)
         'copies-device 1 () t t t () ()))
     (z-paperw
       (iedit (or (paper-width-device device) 210)
         'paper-width-device 1 () t t t () ()))
     (z-paperh
       (iedit (or (paper-height-device device) 297)
         'paper-height-device 1 () t t t () ()))
     (z-row
       (iedit (or (nbrows-device device) 1)
         'nbrows-device 1 () t t t () ()))
     (z-col
       (iedit (or (nbcolumns-device device) 1)
         'nbcolumns-device 1 () t t t () ()))
     (z-scale
       (iedit (or (scale-factor-device device) 0)
         'scale-factor-device 0 () t t t () ()))
     (z-boutons (boutons-portrait-paysage (or (orient-device device) 90)))
     (z-couleur (boutons-radio-couleur))
     (z-postscript (boutons-radio-ps)))

    (set-action b-annuler 'annuler-editeur)
    ({application}:userdata b-ok
      (lambda (device) t))
    (set-action b-ok 'sauver-editeur)
    (set-action b-nonps 'demander-ligne-commande)
    (mixed-applicationq
      copies z-copies
      row z-row
      col z-col
      pph z-paperh
      ppw z-paperw
      scale z-scale
      name 'appli
      image
      (view
        (standard-input 10 20 150 () #Mv_copies z-copies)
        (standard-input 10 40 350 () #Mv_device-orient z-boutons)
        (standard-input 10 60 350 () #Mv_device-couleur z-couleur)
        (standard-input 10 80 350 () #Mv_device-postscript z-postscript) 
        (standard-input 10 120 210 () #Mv_device-nbrow z-row)
        (standard-input 10 140 210 () #Mv_device-nbcol z-col)
        (standard-input 240 120 210 () #Mv_paperh z-paperh)
        (standard-input 240 140 210  () #Mv_paperw z-paperw)
        (standard-input 10 160 150 () #Mv_echelle z-scale)
        (translation 380 70 b-nonps)
        (translation 10 200 (row b-ok b-annuler))))))

(defun demander-ligne-commande (b)
  (let
    ((chaine
      (demande-chaine-avec-ancienne-valeur
        #Mv_titre-demande
        #Mv_ligne-commande CMDLINE-NON-PS)))
    (when chaine
      (setq CMDLINE-NON-PS (string chaine)))))


;;; MOD - attention variable locale diifferente de la variable globale
;;; AJT - RM - 23/08/96 : on sauvegarde les options d'impression

(defun get-device (format)
  (let
    ((appli))
    (ifn format
      (setq format (ps-device FICHIER-POSTSCRIPT)))
    (lire-options-impression format)
    (setq appli (device-asker format))

    ({application}:userdata appli format)
    (send 'grow appli 510 230)
    (send 'set-title appli #Mv_mise-en-page)
    (grab-and-select-application appli)

;     et echelle pour laquelle il faut interpreter 0 comme ()

    (let* 
      ((dev ({application}:userdata appli))
       (orient-b (orient-device dev))
       (scale (scale-factor-device dev)))
      (when dev
        ; (orient-device dev (if orient-b 0 90))
        (scale-factor-device dev (if (eq scale 0) () scale))
        ;; MOD RM - 19/08/96 : on remplace grey par color
        (send 'colortype dev 'color)
        ;; MOD RM - 20/08/96 : ecran virtuel pour l'impression
        (send 'xmin dev 0)
        (send 'ymin dev 0)
        (send 'xmax dev 680)
        (send 'ymax dev 420)
        (send 'layout dev 'centered)
        (send 'margin dev 15)
        (ecrire-options-impression format)
        dev))))

;;; CRE RM - 23/08/96 : sauvegarde des options d'impression dans
;;; le fichier <printer.ini>

(defun ecrire-options-impression (device)
  (let ((rm (rmargin)))
    (rmargin 1024)
    (with ((current-directory REP-VARUNA))
      (with ((outchan (openo FICHIER-PRINTER)))
        (print "(copies " (copies-device device) ")")
        (print "(orientation " (orient-device device) ")")
        (print "(lignes " (nbrows-device device) ")")
        (print "(colonnes " (nbcolumns-device device) ")")
        (print "(hauteur " (paper-height-device device) ")")
        (print "(largeur " (paper-width-device device) ")")
        (print "(echelle " (scale-factor-device device) ")")
        (print "(coloration " COULEUR-DEVICE ")")
        (print "(postscript " POSTSCRIPT-DEVICE ")")
        (print "(cmdline " GUILLEMET CMDLINE-NON-PS GUILLEMET ")")
        (close)))
    (rmargin rm)))


(defun lire-options-impression (device)
  (with ((current-directory REP-VARUNA))
    (if (probefile FICHIER-PRINTER)
      (with ((inchan (openi FICHIER-PRINTER)))
        (copies-device device (cadr (read)))
        (orient-device device (cadr (read)))
        (nbrows-device device (cadr (read)))
        (nbcolumns-device device (cadr (read)))
        (paper-height-device device (cadr (read)))
        (paper-width-device device (cadr (read)))
        (scale-factor-device device (cadr (read)))
        (setq COULEUR-DEVICE (cadr (read)))
        (setq POSTSCRIPT-DEVICE (cadr (read)))
        (setq CMDLINE-NON-PS (cadr (read)))
        (close)))))

;;; CRE RM - 15/12/95
;;; Boite de dialogue permettant de saisir les options d'affichage
;;; pour la grille (cette boite remplacera les differents items du
;;; menu <Afficher>

(defun get-aff-options (appli-gen)
  (let
    ((appli (aff-options-asker appli-gen)))
    (send 'grow appli 430 170)
    (send 'set-title appli #Mv_aff-titre)
    (grab-and-select-application appli)))


(defun aff-options-asker (appli-gen)
  (let
    ((b-ok 
      (standardbutton (cadrer-centrer #Mv_ok 10)))
    (b-annuler 
      (standardbutton (cadrer-centrer #Mv_annuler 10)))
    (b-periode 
      (groupe-boutons-radios #Mv_aff-pla #Mv_aff-sem (not Week-Aff))) 
    (b-detail 
      (groupe-boutons-radios #Mv_aff-complet #Mv_aff-partiel
        (aff-complet-p)))
    (b-chevauchement 
      (groupe-boutons-radios #Mv_aff-sans-chevauchement 
        #Mv_aff-avec-chevauchement (not Aff-Avec-Chevauchement)))
    editeur)
    
    (set-action b-annuler 'annuler-editeur)
    ({application}:userdata b-ok appli-gen)
    (set-action b-ok 'fermer-editeur)
    (setq editeur 
      (mixed-applicationq
        name 'appli
        image
        (view
          (standard-groupe 10 10 200 70 #Mv_aff-periode b-periode)  
          (standard-groupe 10 90 200 70 #Mv_aff-detail b-detail)   
          (standard-groupe 220 10 200 70 #Mv_aff-chevauchement
            b-chevauchement)  
          (translation 250 130 (row b-ok b-annuler)))))
    (add-component editeur 'b-periode b-periode)
    (add-component editeur 'b-detail b-detail)
    (add-component editeur 'b-chevauchement b-chevauchement)
    editeur))


(defun groupe-boutons-radios (premiere-chaine deuxieme-chaine premier-bouton-p)
  (with ((current-font FONT-EDITEUR))
  (let*
    ((bouton1
      (radiobutton 0 0 175 20 premiere-chaine))
    (bouton2 
      (radiobutton 0 0 175 20 deuxieme-chaine))
    (appli 
      (selectorapplication 0 0 200 40
        (column bouton1 bouton2))))
    (if premier-bouton-p
      (send 'set-selection appli bouton1)
      (send 'set-selection appli bouton2))
    (set-action appli
      (lambda (a) t))
    appli)))


;;; A deplacer dans i-util.ll
(defun standard-groupe (x y w h prompt image)
  (view 
    (box x y w h)
    (translation (+ x 8) (- y 8) (catenate ESP prompt ESP))
    (translation (+ x 10) (+ y 20) image)))

;;; Prédicat testant si l'on se trouve en affichage complet
;;; (tous les jours et toutes les semaines)

(defun aff-complet-p ()
  (and
    (every
      (lambda (j)
        (appartenir-liste j Jours-Aff))
      Jours-Ouvres)
    (eq Semaine-Debut-Aff Semaine-Debut)
    (eq Semaine-Fin-Aff Semaine-Fin)))

(defun fermer-editeur (ed)
  (remove-application (component 'appli ed))
  (mise-a-jour-affichage ({application}:userdata ed) ed))


(defun mise-a-jour-affichage (appli-gen appli)
  (let
    ((periode 
        (send 'title 
        (send 'get-selection (component 'b-periode appli))))
    (detail 
      (send 'title 
        (send 'get-selection (component 'b-detail appli))))
    (chevauchement 
      (send 'title 
        (send 'get-selection (component 'b-chevauchement appli)))))
    
    (if (equal periode #Mv_aff-sem)
      (aff-week appli-gen)
      (aff-pla appli-gen))
    (if (equal detail #Mv_aff-partiel)
      (aff-partiel appli-gen)
      (aff-complet appli-gen))
    (if (equal chevauchement #Mv_aff-avec-chevauchement)
      (setq Aff-Avec-Chevauchement t)
      (setq Aff-Avec-Chevauchement ()))
    (update-grid-titles)
    (aff-refresh appli-gen)))

;;; CRE RM - 01/09/97 : relookage des boites de dlg standards

(defun afficher-confirmation (image)
  (with
    ((current-font FONT-EDITEUR))
    (afficher-dialogue
      (trouver-couleur-varuna "jaune-pastel")
      #Mv_titre-confirmation
      confirmer
      image
      ())))

(defun info-application (icone titre-app titre image)
  (let* 
    ((but (standard-ok-button 5))
     (logo (icon 0 0 icone))
     (appl (application '{application} 12 12 100 100
        (header 
          logo  
          (font arial12b (ajouter-espace titre 0 20))
          but
	  (filledbox 0 0 20 50 0)
	  (font time10 image) t t))))
    (send 'set-title appl titre-app)
    (send 'fit-to-contents appl)
    (add-component but 'appli appl)
    appl))

(defun afficher-attention (image)
  (with ((current-background (trouver-couleur-varuna "jaune-pastel")))
    (grab-and-select-application
      (info-application
	Icon-Exclamation "Varuna - Attention" "ATTENTION" image))))

(defun afficher-information (image)
  (with ((current-background (trouver-couleur-varuna "jaune-pastel")))
    (grab-and-select-application
      (info-application
	Icon-Information "Varuna - Information" "INFORMATION" image))))

(defun afficher-erreur (image)
  (with ((current-background (trouver-couleur-varuna "jaune-pastel")))
    (grab-and-select-application
      (info-application
	Icon-Stop "Varuna - Erreur" "ERREUR" image))))

