;;; --------------------------------------------------------------------------
;;; - I-Err.ll                                                  VARUNA 1.2   -
;;; - Interface Graphique                                                    -
;;; - Routines de gestion des erreurs                                        -
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; --------------------------------------------------------------------------
;;; - Creation:  Janvier 94                                     JH           -
;;; - Mise a jour pour version 1.07: Decembre 93                             -
;;; - Build 1.22.1 01.07.99 / JH MO                                          -
;;; --------------------------------------------------------------------------

;;; MODIF (21/03/95) FD - erreur-seuil-echec - 
;;;                     - erreur-cours-non-places -
;;;              ces 2 fonctions ont ete modifiees pour que la liste des
;;;              cours s'affiche dans un selectionneur (cela evite de voir
;;;              disparaitre le bouton OK ...)
;;; AJT FD ajout du traitement des messages d'erreur se produisant lors de
;;; la reservation d'une salle


;; les erreurs possibles sont :
;; errEcole : pas de fichier ecole.edt (->edition de l'ecole)
;; errNoDate : pas de date pour une liste de cours
;; errNoType : pas de salle du type pour un cours
;; errNoRoom : plus de salle disponible pour une liste de cours
;; errResol : erreur en voulant placer un cours
;; errSeuil : seuil d'echecs atteints
;; errGene : erreur generale
;; errContraintes : erreur en posant une contrainte 
;; errDureeCours : la duree des cours est incompatibles avec l'ecole
;; errNoDom : pas de domaine pour le cours
;; errDep : deplacement rectangle interdit 


;; errMCNC : non chevauchement sur le meme cours (pose des containtes) 
;; errMCPS : proximite semaines sur le meme cours (pose des contraintes)
;; errMCP : precedence sur un meme cours (pose des contrintes)
;; errMCJD : jours differents sur un meme cours (pose des contraintes)

;; AJT - 04/03/96 - MulitP N7
;; erreur-lecture-seule : donnee est en lecture seule

;; erreur-suivante renvoie une liste de deux elements 
;; 1 - un code d'erreur parmis ceux cites ci-dessus
;; 2 - une liste d'objets
;; pour chaque type d'erreur,
;; on connait a priori le type des objets passes dans erreur-suivante
;; ! il serait interessant de recuperer le nom de la contrainte ? type-contrainte
;; renvoie ct-constraint dont je ne sais que faire
;;; AJT - FD -10/10/95 - ajout du code dans le message

(defun erreur-domaine-cours (code-erreur cours)
  (afficher-erreur
    (column
      (get-message code-erreur)
      (get-message 'errNoDomRem)
      (chaine2image
        (catenate
          (libelle-i-cours (eval (symbol () cours)))
          " (" cours ")")))))

(defun erreur-duree-cours (code-erreur code-cours)
  (afficher-erreur
    (column
      (get-message code-erreur)
      (chaine2image
        (libelle-i-cours (eval code-cours)))))
  ;; MOD RM - 22/04/96 : ajout de <creationp> dans <editer-i-cours>
  (editer-i-cours (eval code-cours) ()))

(defun erreur-deplacement-cours (code-erreur code-cours infos)
  (let ((message1 (get-message code-erreur)))
    (afficher-erreur
      (apply 'column
        (cons
          message1
          (mapcar 'get-message infos))))))


;;; CHG (05/04/95) MO
;;; cette primitive n'et pas utile dans cette version

;;;(defun confirmer-deplacement-cours (code-erreur liste-objets)
;;;  (afficher-confirmation
;;;    (column 
;;;      (get-message code-erreur)
;;;     (get-message (car liste-objets))
;;;     (get-message 'v_cours)
;;;     (stringmenu (mapcar 'libelle-i-cours (cdr liste-objets)))
;;;     (catenate (get-message 'v_continuer) " ?"))))
                                        
                                        
(defun erreur-contraintes (code-erreur cours type-contrainte)
  (alerte-type 'afficher-erreur
    (column
      (get-message code-erreur)
      (column
        (get-message 'v_cours)
        (chaine2image (libelle-i-cours cours))
        (row "(" (string (code-i-cours cours)) ")")))))

;;; CRE - FD - 21/07/95
;;; <erreur-inc-contr>
;;; cas ou il n'est pas possible de poser une contrainte entre
;;; deux rectangles (cours ou ajout de seance)
;;; MOD RM 11/06/97

(defun erreur-inc-contr (code-erreur liste)
  (let 
    ((contrainte (car liste))
     (rect1 (eval (cadr liste)))
     (rect2 (eval (caddr liste))))
        
  (alerte-type 'afficher-erreur
      (column
         (get-message code-erreur)
          (row
	    (code-contrainte2erreur contrainte)
	    " entre")
          (row
	    (if (equal (type-of rect1) '#:tclass:i-cours)
              (string (libelle-i-cours rect1))
	      (string (libelle-i-ajout rect1)))
              " - "
	      (if (equal (type-of rect1) '#:tclass:i-cours)
		(string (code-i-cours rect1))
		(string (code-i-ajout rect1))))
	  (row
	    (if (equal (type-of rect2) '#:tclass:i-cours)
              (string (libelle-i-cours rect2))
	      (string (libelle-i-ajout rect2)))
              " - "
	      (if (equal (type-of rect2) '#:tclass:i-cours)
		(string (code-i-cours rect2))
		(string (code-i-ajout rect2))))))))

(defun erreur-type-salle (code-erreur cours)
  (alerte-type 'afficher-erreur
    (column
      (row (get-message code-erreur) (string (type-salle-i-cours cours)))
      (column (get-message 'v_i-cours)
        (chaine2image (libelle-i-cours cours)))))
  ;; MOD RM - 22/04/96 : ajout de <creationp> dans <editer-i-cours>  
  (editer-i-cours (eval cours) ()))


(defun erreur-cours-non-places (code lc)
  (with ((current-font FONT-EDITEUR))
    (let
      ((hy))
      (setq 
	hy
	(hypersel 400 
	  (min (add (* #hchar (length lc))5) 300) 
	  (mapcar (lambda (code)
	    (libelle-i-cours (eval code)))
	  lc)))
      (send 'inhibit-application (component 'hyper hy))
      (alerte-type 'afficher-attention
	(column
	  (get-message code)
	  hy)))))


(defun erreur-cours-incomplet (code cours)
  (alerte-type 'afficher-erreur
    (column
      (get-message 'v_le-cours)
      (chaine2image (libelle-i-cours cours))
      (get-message code)))
  ;; MOD RM - 22/04/96 : ajout de <creationp> dans <editer-i-cours>  
  (editer-i-cours cours ()))

(defun erreur-seuil-echec (code liste-codes-cours)
  (with ((current-font FONT-EDITEUR))
    (let
      ((hy
	(hypersel 400 
	  (min (* #hchar (length liste-codes-cours)) 300) 
	  (mapcar (lambda (code)
	    (catenate
	      (string code)
	      " - "
	      (libelle-i-cours (eval code))))
	  liste-codes-cours))))
      (send 'inhibit-application (component 'hyper hy))
      (afficher-erreur
	(column 
	  (row (string Seuil-Echec) (get-message code))
	  #Mv_i-cours
	  hy)))))

(defun erreur-dispo-cours (code-cours)
  (let ((cours (eval code-cours)))
    (afficher-erreur
      (column
        (row #Mv_i-cours (libelle-i-cours cours))
        #Mv_dispo-incompatibles
        #Mv_verifier-dispo))
    ;; MOD RM - 22/04/96 : ajout de <creationp> dans <editer-i-cours>    
    (editer-i-cours cours ())))

(defun erreur-def-groupe (code-groupe)
  (let ((groupe (eval code-groupe)))
    (afficher-erreur 
      (column
        (row #Mv_i-groupe (libelle-i-groupe groupe))
        #Mv_groupe-incomplet))
    (when
      ;; MOD RM - 22/04/96 : ajout de <creationp> dans <editer-i-groupe>      
      (editer-i-groupe groupe ())
      (mise-a-jour-groupe-special groupe))))

;;; CRE RM - 04/06/96 : gestion des erreurs des familles alternance

(defun erreur-dispo-groupe-alternance (code-groupe)
  (let ((groupe (eval code-groupe)))
    (afficher-erreur 
      (column
        (row #Mv_i-groupe (libelle-i-groupe groupe))
        #Mv_groupe-alternance-dispo-insuffisante))
    (when
      ;; MOD RM - 22/04/96 : ajout de <creationp> dans <editer-i-groupe>      
      (editer-i-groupe groupe ())
      (mise-a-jour-groupe-special groupe))))

(defun erreur-dispo-alternance (code-groupe)
  (let ((groupe (eval code-groupe)))
    (afficher-erreur 
      (column
        (row #Mv_i-groupe (libelle-i-groupe groupe))
        #Mv_groupe-alternance-dispo-insuffisante))))

(defun erreur-alias (objet)
  (afficher-erreur
      (column
          #Merralias
          (print-to-string objet))))
  

;;; FD-24/04/95 traitement des messages d'erreurs lies … la resa
(defun erreur-resa (code-erreur liste)
  (afficher-erreur
      (column
          ;;;"Echec dans la recherche" 
          (get-message code-erreur)
          (couper-chaine
              (print-to-string liste-objets)
              100))))

;;;  CRE FD - 03/07/95
(defun erreur-dispo-insuffisante (code)
  (let ((cours (eval code)))
    (afficher-attention
      (column
	#Merrdispo-Insuffis
	(row 
	  (libelle-i-cours cours)
	  " ("
	    (print-to-string (code-i-cours cours))
	    ")")))))

;;;  CRE FD - 03/07/95
(defun erreur-dispo-ecole (code)
  (let ((cours (eval code)))
    (afficher-erreur
      (column
          #Merrdispo-Insuffis
          #Merrdispo-Ecole
          (row 
              (libelle-i-cours cours)
              " ("
              (print-to-string (code-i-cours cours))
              ")")))))

;;;  CRE FD - 03/07/95
(defun erreur-dispo-groupe (code)
  (let ((cours (eval code)))
    (afficher-erreur
      (column
          #Merrdispo-Insuffis
          #Merrdispo-Groupe
          (row 
              (libelle-i-cours cours)
              " ("
              (print-to-string (code-i-cours cours))
              ")")))))

;;;  CRE FD - 03/07/95
(defun erreur-dispo-prof (code)
  (let ((cours (eval code)))
    (afficher-erreur
      (column
          #Merrdispo-Insuffis
          #Merrdispo-Prof
          (row 
              (libelle-i-cours cours)
              " ("
              (print-to-string (code-i-cours cours))
              ")")))))

;;;  CRE FD - 03/07/95
(defun erreur-nodispo (code)
  (let ((cours (eval code)))
    (afficher-erreur
      (column
          #Merrdispo-Insuffis
          (row 
              (libelle-i-cours cours)
              " ("
              (print-to-string (code-i-cours cours))
              ")")))))

;;; CRE - FD - 04/05/95
;;; erreur produit lors de la recherche d'un creneau horaire pour un ajout de
;;; seance
(defun erreur-dispo-ajout ()
  (afficher-erreur
    (column
      "Il n'existe pas de creneau horaire suffisant.")))

;;; CRE - FD - 11/10/95
(defun erreur-arret-resolution ()
  (afficher-information
    (column
      "La résolution a été interrompue.")))

(defun erreur-lecture-seule (objet)
  (afficher-erreur
    (column
      "Donnée en lecture seule"
      (chaine2image
        (catenate
          (send 'libelle objet)
          " (" (send 'code objet) ")")))))

(defun erreur-type-cours (objet)
  (afficher-erreur #Mv_err-type-cours))

(defun erreur-type-interdit-salle (objet)
  (afficher-erreur
    (column
      #Mv_type-interdit
      (row "(type de salle : " (print-to-string objet) ")"))))

(defun erreur-type-interdit-cours (objet)
  (let
    ((type (eval objet)))
    (afficher-erreur
      (column
        #Mv_type-interdit
        (row
          "(type de cours : "
          (print-to-string (code-i-type-cours type))  
          " - "
          (print-to-string (libelle-i-type-cours type))
          ")")))))

;;; CRE RM 15/01/97 : permutation de seances

(defun erreur-permut-cours (objet)
  (let
    ((cours (eval objet)))
    (afficher-erreur
      (column
        "Permutation de séances :"
        "Vous avez sélectionné le même cours"
        (row
          (print-to-string (libelle-i-cours cours))
          " ("
          (print-to-string (code-i-cours cours)) ")")))))

(defun erreur-permut-duree (objet)
  (afficher-erreur
    (column
      "Permutation de séances :"      
      "La durée des séances est différente.")))

(defun erreur-permut-capa-salle (objet)
  (afficher-erreur
    (column
      "Permutation de séances :"      
      "La capacité d'une des salles est insuffisante"
      "pour effectuer la permutation.")))

(defun erreur-permut-no-annul (liste-objets)
  (let
    ((cours (eval (car liste-objets)))
     (sem (cadr liste-objets))) 
    (afficher-erreur
      (column
        "Permutation de séances :"      
        (row
          "La séance de la semaine "
          (ecrire-numero-semaine (print-to-string sem)))
        (row "du cours "
          (print-to-string (libelle-i-cours cours))
          " ("
          (print-to-string (code-i-cours cours)) ")")
        "a déjà été annulée."))))

(defun traiter-erreur ()
  (let* ((erreur (erreur-suivante))
    (code-erreur (car erreur))
    (liste-objets (cdr erreur))
    (objet (if (consp liste-objets) (car liste-objets) liste-objets)))
      (delete-erreur)
      (cond 
        ((eq code-erreur 'errContraintes)
          (erreur-contraintes code-erreur objet (cadr liste-objets)))
        
        ((eq code-erreur 'errEcole)
          (don-ecole Input-App))
                
        ((eq code-erreur 'errGene) 
          (end))
        
        ((eq code-erreur 'errNoRoom)
          (erreur-cours-non-places code-erreur liste-objets))
        
        ((eq code-erreur 'errNoDate)
          (erreur-cours-non-places code-erreur liste-objets))
        
        ((or (eq code-erreur 'errNoFili ) (eq code-erreur 'errNoPro))
          (erreur-cours-incomplet code-erreur objet))
        
        ((eq code-erreur 'errNoType)
          (erreur-type-salle code-erreur objet))
        
        ((eq code-erreur 'errNoDom)
          (erreur-domaine-cours code-erreur objet))
        
        ((eq code-erreur 'errResol) ())
        
        ((eq code-erreur 'errSeuilEchec)
          (erreur-Seuil-Echec code-erreur liste-objets))
        
        ((eq code-erreur 'errDepl)
          (erreur-deplacement-cours code-erreur objet (cdr liste-objets))) 
        
        ((eq code-erreur 'errMCNC)
          (erreur-contraintes code-erreur objet (cadr liste-objets)))
        
        ((eq code-erreur 'errMCPC)
          (erreur-contraintes code-erreur objet (cadr liste-objets)))
        
        ((eq code-erreur 'errMCP)
          (erreur-contraintes code-erreur objet (cadr liste-objets)))
        
        ((eq code-erreur 'errMCJD)
          (erreur-contraintes code-erreur objet (cadr liste-objets)))
        
        ;;;((eq code-erreur 'errNoVal)
        ;;;  (erreur-lecture-fichier code-erreur () liste-objets))

        ;;;((eq code-erreur 'errTypeEnr)
        ;;;  (erreur-lecture-fichier code-erreur () liste-objets))

        ;;;((eq code-erreur 'errGrpNoFils)
        ;;;  (erreur-groupe code-erreur () ()))
        
        ((eq code-erreur 'errDefGroupe)
          (erreur-def-groupe objet))

        ;; RM 04/06/96 
        ((eq code-erreur 'errAlternanceGroupe)
          (erreur-dispo-groupe-alternance objet))

        ((eq code-erreur 'errGroupeAlternance)
          (erreur-dispo-alternance objet))
        
        ((eq code-erreur 'errDispoCours)
          (erreur-dispo-cours objet))
        
        ((eq code-erreur 'errDureeCours)
          (erreur-duree-cours code-erreur objet))
        
        ((eq code-erreur 'erralias)
          (erreur-alias objet))

        ;; FD - 03/07/95 
        ((eq code-erreur 'errdispo-insuffi)
          (erreur-dispo-insuffisante objet))

        ;; FD - 03/07/95 
        ((eq code-erreur 'errdispo-ecole)
          (erreur-dispo-ecole objet))
        
        ;; FD - 03/07/95 
        ((eq code-erreur 'errdispo-groupe)
          (erreur-dispo-groupe objet))
        
        ;; FD - 03/07/95 
        ((eq code-erreur 'errdispo-prof)
          (erreur-dispo-prof objet))

        ((eq code-erreur 'errnodisp)
          (erreur-nodispo objet))
        
        ((eq code-erreur 'errinccontr)
          (erreur-inc-contr code-erreur liste-objets))

        ((or (eq code-erreur 'err-Capa-Aud)
             (eq code-erreur 'err-Type-Salle))
          (erreur-resa code-erreur liste-objets))

        ((eq code-erreur 'err-dispo-ajout)
          (erreur-dispo-ajout))
        
        ;; AJT - FD - 11/10/95
        ((eq code-erreur 'arret-resolution)
          (erreur-arret-resolution))

        ((eq code-erreur 'erreur-lecture-seule)
          (erreur-lecture-seule objet))
        
        ((eq code-erreur 'err-type-cours-depl)
          (erreur-type-cours objet))

        ((eq code-erreur 'err-type-interdit-salle)
          (erreur-type-interdit-salle objet))

        ((eq code-erreur 'err-type-interdit-cours)
          (erreur-type-interdit-cours objet))
        
        ((eq code-erreur 'errpermutcours)
          (erreur-permut-cours objet))

        ((eq code-erreur 'errpermutduree)
          (erreur-permut-duree objet))
        
        ((eq code-erreur 'errpermutcapa)
          (erreur-permut-capa-salle objet))

        ((eq code-erreur 'errpermutnoannul)
          (erreur-permut-no-annul liste-objets))

        ((not (null code-erreur))
          (afficher-erreur
            (column
              "Erreur Inconnue" (string code-erreur)
                (couper-chaine
                  (print-to-string liste-objets)
                  100))))
        (t ()))))


(defun interface-erreur ()
  (when *erreurs* (traiter-erreur)))

;;; CRE RM 31/10/97 : affichage des erreurs de pose de contrainte lors
;;; d'un deplacement de cours

(defun erreur-contraintes-non-respectees (cours)
  (with ((current-font FONT-EDITEUR))
    (let
      ((hy
	(hypersel 500
	  (min (add (* #hchar (length lst-err-contraintes)) 5) 300) 
	  (mapcar
	    (lambda (err)
	      (catenate
		(code-contrainte2erreur (car err)) " : "
		(if (eq (type-objet (cdr err)) 'cours)
		  (libelle-i-cours (cdr err))
		  (libelle-i-ajout (cdr err)))
		" (" (send 'objet2string (cdr err)) ")"))
	    (reverse lst-err-contraintes)))))
      (send 'inhibit-application (component 'hyper hy))
      (afficher-attention
	(column
	  #MInfo-Dep-Contraintes-Non-Respectees
	  (catenate
	    "avec le cours "
	    (libelle-i-cours cours)
	    " (" (code-i-cours cours) ")")
	  ESP hy)))))

;;; CRE RM 12/02/98 : affichage d'un cycle

(defun erreur-graphe-cyclique (liste)
  (with ((current-font FONT-EDITEUR))
    (let
      ((hy
	(hypersel 500
	  (min (add (* #hchar (length liste)) 5) 300) 
	  (mapcar
	    (lambda (f)
	      (catenate
		(libelle-i-groupe f)
		" (" (send 'objet2string f) ")"))
	    liste))))
      (send 'inhibit-application (component 'hyper hy))
      (afficher-erreur
	(column
	  #MInfo-Cycle
	  ESP hy)))))
