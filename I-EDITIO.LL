;;; --------------------------------------------------------------------------
;;; - i-editio.ll                                                VARUNA 1.2  -
;;; - description de l'interface d'e'dition                                  -
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; --------------------------------------------------------------------------
;;; - Cre'ation:  Juin 1995  MO                                              -  
;;; - Mise a jour pour version 1.13 : Juin 1995 MO                           -
;;; - Build 1.22.1 01.07.99 / MO FD                                          -
;;; --------------------------------------------------------------------------


;;; CRE (01/06/95) MO
;;; pour le moment, je prefere un selectq a une macro
;;; on peut avoir comme objet
;;;               - cours
;;;               - familles
;;;               - professeurs
;;;               - salles
;;;               - reservation
;;;               - rien -> ()


(defun action-sous-menu-edition (objet commande)
  (if
    objet
    (let 
      ((objets-demandes)
       (premier-libelle))

      (if (liste-objet-vide-p objet)
        (afficher-erreur (chaine-erreur-objet objet))
        (setq objets-demandes
          (selectq objet
            (cours (demandes-i-cours #Mv_pro-cours () t))
            (profs  (demandes-i-prof #Mv_pro-profs () t))
            (familles (demande-i-groupe #Mv_pro-group ()))
            (salles (demandes-i-salle #Mv_pro-salles () t))
	    (resa (demandes-i-resa #Mv_pro-resa () t))
            (raco (demande-i-cours #Mv_pro-cou ()))
            (annul (demandes-i-annulation #Mv_pro-annulation () t))
            (ajout (demandes-i-ajout #Mv_pro-ajout () t))
            (t ())))
        (if (and (eq objet 'familles) objets-demandes)
          (setq objets-demandes
            (cons objets-demandes (tous-les-sous-groupes objets-demandes))))
        (if (and (eq objet 'raco) objets-demandes)
          (setq objets-demandes (rattrapage-cours-liste objets-demandes)))
        
        (when objets-demandes
          (setq premier-libelle (libelle-objet objet (car objets-demandes)))
	  (imprimer commande premier-libelle objets-demandes))))))

  
(defun commande-edition (commande)
  (let
    ((erreur)
     (format (ps-device FICHIER-POSTSCRIPT)))
    (if
      (and MODULE-RESEAU (not (interdire-acces-attente EDT-SEM)))
      (afficher-attention #Mv_acces-edt-interdit)

      (lire-options-impression format)
      (with 
	((current-directory (string (getenv "VAREDIT"))))
	(setq erreur
	  (funcall 'vexecute "vedit editions.edi" (current-directory)))
	(if (< erreur 33)
	  (afficher-erreur
	    (column
	      "Une erreur d'impression est survenue"
	      "dans le module vedit.exe"))
	  (setq erreur (appeler-vprint commande))
	  (if (< erreur 33)
	    (afficher-attention
	      (column
		"Une erreur d'impression est survenue"
		"dans le module vprint.exe"))
	    (if postscript-device
	      (imprimer-fichier-ps FICHIER-POSTSCRIPT)
	      (setq erreur
		(funcall 'vexecute CMDLINE-NON-PS (current-directory)))) )))
      (when MODULE-RESEAU
	(autoriser-acces EDT-SEM)))))

(defun appeler-vprint (commande)
  (let
    ((ligne-commande "vprint.exe ")
     (infile "-i vedit.txt ")
     (outfile "-o psdump.ps"))
    (selectq commande
      (veprof
	(setq ligne-commande
	  (catenate
	    ligne-commande
	    "-p " commande ".pos " commande ".prn "
	    "-p " commande "2.pos " commande "2.prn "
	    infile
	    outfile)))
      ((veenseig vevacanc)
	(setq ligne-commande
	  (catenate
	    ligne-commande
	    "-p " commande ".pos " commande ".prn "
	    "-p veensei2.pos veensei2.prn "
	    infile
	    outfile)))
      (vevacnou
	(setq ligne-commande
	  (catenate
	    ligne-commande
	    "-p " commande ".pos " commande ".prn "
	    "-p veensei2.pos veensei2.prn "
	    "-p vevacno2.pos vevacno2.prn "
	    infile
	    outfile)))
      (t
	(setq ligne-commande
	  (catenate
	    ligne-commande
	    "-p " commande ".pos " commande ".prn "
	    "-p " commande ".pos " commande ".prn "
	    infile
	    outfile))))
    (funcall 'vexecute ligne-commande (current-directory))))

;;; ancienne version de la fonction 
; (defun ecrire-fichier-editions (symb liste-objets)
;   (let ((chemin-donnees (current-directory)))
;     (with ((current-directory (string (getenv "VAREDIT"))))
;       (let ((liste-travail liste-objets))
;  
;         (with  
;           ((outchan (openo "editions.edi")))
;           (print "(path " chemin-donnees")")
;           (print "(type-edition " `',symb ")")
;           (print "(unite-temps " unite-temps")")
;           (print "(horaire-cumule-ouverture " horaire-cumule-ouverture")")
;           (print "(horaire-cumule-fermeture " horaire-cumule-fermeture")")
;           (while 
;             liste-travail 
;             (print "(objet " `',(nextl liste-travail) ")"))
;           (close (outchan)))))))
 
(defun ecrire-fichier-editions (symb premier-libelle liste-objets)
  (let
    ((chemin-donnees (current-directory))
     (rm (rmargin)))
    (with ((current-directory (string (getenv "VAREDIT"))))
      (let ((liste-travail liste-objets))

	(rmargin 1024)
        (with  
          ((outchan (openo "editions.edi")))
          (print "(path " chemin-donnees")")
          (print "(type-edition " `',symb ")")
          (print "(unite-temps " unite-temps")")
          (print "(horaire-cumule-ouverture " horaire-cumule-ouverture")")
          (print "(horaire-cumule-fermeture " horaire-cumule-fermeture")")
          (print "(libelle (" premier-libelle "))")
          (while 
            liste-travail 
            (print "(objet " `',(nextl liste-travail) ")"))
          (close (outchan)))
	(rmargin rm)))))

;;; CRE RM - 04/09/95 : retourne une liste comprenant une annulation et
 ;;; un ajout de seance pour l'edition du rattrapage de cours

(defun rattrapage-cours-liste (cours)
  (let ((liste)
        (annul)
        (ajout))
    (mapc
      (lambda (annul)
	; MOD FD 13/02/96
	; (when (eq (code-cours-i-annulation annul) (code-i-cours cours))
	(when (eq (cours-i-annulation annul) cours)
          (setq liste (cons (code-i-annulation annul) liste))))  
      liste-i-annulation)
    (cond
      ((not liste)
        (afficher-attention #Mv_aucune-annulation)
        (setq annul ()))
      ((> (length liste) 1)
        (setq annul (demande-i-annulation #Mv_pro-ann ())))
      (t
        (setq annul (car liste))))    
    (setq liste ())
    (mapc
      (lambda (ajout)
	; MOD FD 13/02/96
	; (when (eq (cours-rattrape-i-ajout ajout) (code-i-cours cours))
	(when (eq (cours-rattrape-i-ajout ajout) cours)
          (setq liste (cons (code-i-ajout ajout) liste))))  
      liste-i-ajout)
    (cond
      ((not liste)
        (afficher-attention #Mv_aucun-ajout)
        (setq ajout ()))
      ((> (length liste) 1)
        (setq ajout (demande-i-ajout #Mv_pro-ajout1 ())))
      (t
        (setq ajout (car liste))))
    (when (and annul ajout)
      (list annul ajout))))

;;; CRE RM - 15/09/95 : Impression d'une reservation de salle lors de
;;; sa reservation
;;; nouvelle fonction dans <imprim.ll> 15/02/96
; (defun imprimer-reservation (resa)
;   (if
;     (and MODULE-RESEAU (not (interdire-acces EDT-SEM)))
;     (afficher-warning #Mv_acces-edt-interdit)
;     
;     (ecrire-fichier FICHIER-RESA 'interface 'resa ())
;     (ecrire-fichier-editions
;       'veresa
;       (libelle-objet 'resa resa)
;       `(,resa))
;     (when MODULE-RESEAU
;       (autoriser-acces EDT-SEM))
;     (commande-edition)))

;;; CRE RM - 15/09/95 : Impression d'un ajout de seance lors de
;;; sa reservation
;;; la nouvelle fonction se trouve dans <imprim.ll> 15/02/96
    
; (defun imprimer-ajout (ajout)
;   (if
;     (and MODULE-RESEAU (not (interdire-acces EDT-SEM)))
;     (afficher-warning #Mv_acces-edt-interdit)
;     
;     (ecrire-fichier FICHIER-AJOUT 'interface 'ajout ())
;     (ecrire-fichier-editions
;       'veajout
;       (libelle-objet 'ajout ajout)
;       `(,ajout))
;     (when MODULE-RESEAU
;       (autoriser-acces EDT-SEM))
;     (commande-edition)))

;;; CRE RM - 15/09/95 : Impression d'une annulationde seance lors de
;;; sa reservation
;;; la nouvelle fonction se trouve dans <imprim.ll> 15/02/96
; (defun imprimer-annulation (annul)
;   (if
;     (and MODULE-RESEAU (not (interdire-acces EDT-SEM)))
;     (afficher-warning #Mv_acces-edt-interdit)
;     
;     (ecrire-fichier FICHIER-ANNUL 'interface 'annulation ())
;     (ecrire-fichier-editions
;       'veannul
;       (libelle-objet 'annul annul)
;       `(,annul))
;     (when MODULE-RESEAU
;       (autoriser-acces EDT-SEM))
;     (commande-edition)))

;;; CRE RM - 11/10/95 : retourne le libelle d'un objet

(defun libelle-objet (type objet)
  (selectq type
    (cours
      (libelle-i-cours objet))
    (profs
      (libelle-i-prof objet))
    (familles
      (libelle-i-groupe objet))
    (salles
      (libelle-i-salle objet))
    (resa
      (libelle-i-resa objet))
    (annul
      (libelle-i-annulation objet))
    (ajout
      (libelle-i-ajout objet))
    (raco
      (libelle-i-annulation objet))
    (t ())))
    
;;; CRE RM - 12/01/96 : retourne une chaine pour traiter l'erreur

(defun chaine-erreur-objet (type)
  (let ((erreur))
    (selectq type
      (cours (setq erreur #Mv_aucun-cours))
      (profs (setq erreur #Mv_aucun-prof))
      (familles (setq erreur #Mv_no-groups))
      (salles (setq erreur #Mv_aucune-salle))
      (resa (setq erreur #Mv_aucune-resa))
      (annul (setq erreur #Mv_aucune-annulation))
      (ajout (setq erreur #Mv_aucun-ajout))
      (raco (setq erreur #Mv_aucun-cours))
      (t ()))
    erreur))

(defun liste-objet-vide-p (objet)
  (let ((retour))
    (selectq objet
      (cours (setq retour (and liste-i-cours t)))
      (profs (setq retour (and liste-i-prof t)))
      (familles (setq retour (and liste-i-groupe t)))
      (salles (setq retour (and liste-i-salle t)))
      (resa (setq retour (and liste-i-resa t)))
      (annul (setq retour (and liste-i-annulation t)))
      (ajout (setq retour (and liste-i-ajout t)))
      (raco (setq retour (and liste-i-cours t)))
      (t ()))
    (not retour)))
  
