;;; ------------------------------------------------------------------------
;;; - imprim.ll                                               VARUNA 1.2   -
;;; - Copyright Abstract, France 1993-2006                                 -
;;; - under licence MIT Expat since 2006                                   -
;;; -                                                                      -
;;; - Module d'utilitaires pour les impressions specifiques                -
;;; -                                                                      -
;;; - Aida & Lisp                                                          -
;;; ------------------------------------------------------------------------
;;; - Creation:  Fevrier 1996                                      FD      -
;;; - Build 1.22.1 01.07.99                                                -
;;; ------------------------------------------------------------------------

;;; <ecrire-fichier-impression>
;;; permet d'ecrire dans le repertoire VARUNA\tmp le fichier de donnees passe
;;; en argument et contenant uniquement les objets passes en arguments.
;;; Le fichier ainsi cree ne contiendra que les donnees necessaire a
;;; l'impression

(defun ecrire-fichier-impression (fichier type-ecriture type-enregistrement liste-objets)
  (setq LISTE-ENR liste-objets)
  (let
    ((pl (printlevel))
     (rm (rmargin)))
    (printlevel 100)
    (rmargin 1024)
    (with
      ((current-directory (string (getenv "VAREDIT")))
	(outchan (openo fichier)))
      (when LISTE-ENR
	;; MOD - FD/RM 12/09/96 : on n'indexe plus les donnees !!!	
	(ecrire-nouveaux type-enregistrement type-ecriture ()))
      (close))
    (printlevel pl)
    (rmargin rm)
    ()))

;;; <imprimer>
;;; impressions textuelles.
;;; Pour ces impressions, nous enregistrons dans le repertoire VARUNA\tmp les
;;; donnees necessaires a celles-ci (et uniquement celles qui sont necessaires)
;;; rem: Il faut creer une fonction pour chacune des partie ddu <cond>
;;; !! creer des sous fonctions

;;; DBG RM 01/07/97 : lorsqu'un prof dispense plusieurs cours, on ne l'ajoute
;;; pas n fois !!! (merci pour la lecture generique de vedit)
;;; pareille pour les salles ...

(defun imprimer (commande premier-libelle objets-demandes)
  (let
    ((rep-donnees-impression (string (getenv "VAREDIT")))
     (liste-cours)
     (liste-groupes)
     (liste-profs)
     (liste-salles)
     (liste-reservations)
     (liste-ajouts)
     (liste-annuls))

    (cond
      ((eq commande 'vecfcour)	;; confirmation de cours
	(setq liste-cours objets-demandes)
	(mapc
	  (lambda (cours)
	    (setq liste-profs
	      (append liste-profs (profs-i-cours cours)))
	    (when (salle-trouvee-i-cours cours)
	      (setq
		liste-salles
		(cons (salle-trouvee-i-cours cours) liste-salles))))
	  objets-demandes))
      
      ((eq commande 'veprof)   ;; cours d'un prof
	(setq liste-profs objets-demandes)
	(mapc
	  (lambda (prof)
	    (setq liste-cours (append (cours-i-prof prof) liste-cours)))
	  liste-profs)
	(mapc
	  (lambda (cours)
	    (when (salle-trouvee-i-cours cours)
	      (setq liste-salles
		(cons (salle-trouvee-i-cours cours) liste-salles))))
	  liste-cours))
      
      ((eq commande 'veresa)
	;; reservation de salle
	(setq liste-reservations objets-demandes)
	(mapc
	  (lambda (resa)
	    (when (salle-i-resa resa)
	      (setq liste-salles (cons (salle-i-resa resa)  liste-salles))))
	  liste-reservations))

      ((eq commande 'veannul)
	;; annulation 
	(setq liste-annuls objets-demandes)
	(setq liste-cours
	  (mapcar 'cours-i-annulation liste-annuls))
	(mapc
	  (lambda (annul)
	    (let*
	      ((cours (cours-i-annulation annul))
		(salle (salle-trouvee-i-cours cours)))
	      
	      (setq liste-profs
		(append (profs-i-cours cours) liste-profs))
	      (when salle
		(setq liste-salles (cons salle  liste-salles)))))
	  liste-annuls))
      
      ((eq commande 'veajout)
	;; ajout
	(setq liste-ajouts objets-demandes)
	(mapc
	  (lambda (ajout)
	    (let
	      ((salle (salle-i-ajout ajout))
		(cours (cours-rattrape-i-ajout ajout))) 
	      (setq liste-profs
		(append (profs-i-ajout ajout) liste-profs))
	      (when salle
		(setq liste-salles (cons salle liste-salles)))
	      (when cours
		(setq liste-cours (cons cours liste-cours)))))
	  liste-ajouts))
      
      (t
	;; on travaille sur les donnees v‚ritables
	(and dirty (ecrire-donnees-interface))
	(setq rep-donnees-impression (current-directory))))

    (ifn (probefile FICHIER-DATES)
      (afficher-erreur "Le fichier de dates est absent du répertoire de données.")

      (copier-fichier-dates (current-directory) rep-donnees-impression)
      (ecrire-donnees-impression
	(supprimer-occurences-multiples liste-cours)
	(supprimer-occurences-multiples liste-groupes)
	(supprimer-occurences-multiples liste-profs)
	(supprimer-occurences-multiples liste-salles)
	(supprimer-occurences-multiples liste-reservations)
	(supprimer-occurences-multiples liste-ajouts)
	(supprimer-occurences-multiples liste-annuls))
      (with
	((current-directory rep-donnees-impression))
	(ecrire-fichier-editions commande premier-libelle objets-demandes)
	(commande-edition commande)))))

;;; <copier-fichier-dates>
;;; on copie le fichier DATES.EDT dans le rep Vedit (si besoin)

(defun copier-fichier-dates (rep-donnees rep-imp)
  (when (not (equal rep-donnees rep-imp))
    (copyfile
      (catenate rep-donnees FICHIER-DATES)
      (catenate rep-imp "\" FICHIER-DATES))))

;;; <ecrire-donnees-impression>
;;; ecriture des donnees passees en argument et qui sont necessaires aux
;;; impressions
  
(defun ecrire-donnees-impression (liste-cours liste-groupes liste-profs liste-salles liste-reservations liste-ajouts liste-annuls)
  (ecrire-fichier-impression FICHIER-COURS 'interface 'cours liste-cours)
  (ecrire-fichier-impression FICHIER-GROUPES 'interface 'groupe liste-groupes)
  (ecrire-fichier-impression FICHIER-PROFS 'interface 'prof liste-profs)
  (ecrire-fichier-impression FICHIER-SALLES 'interface 'salle liste-salles)
  (ecrire-fichier-impression FICHIER-RESA 'interface 'resa liste-reservations)
  (ecrire-fichier-impression FICHIER-AJOUT 'interface 'ajout liste-ajouts)
  (ecrire-fichier-impression FICHIER-ANNUL 'interface 'annulation liste-annuls))
  
;;; <imprimer-reservation>
;;; imprimer une reservation de salle lors de sa reservation
;;; impression d'une reservation sans enregistrer toutes les donnees mais
;;; uniquement celles dont nous avons besoin
;;; rq: ancienne fct etait dans <i-editio.ll>

(defun imprimer-reservation (resa)
  (let
    ((rep-donnees (current-directory)))
    
    (ecrire-fichier-impression
      FICHIER-RESA 'interface 'resa (list resa))
    (ecrire-fichier-impression
      FICHIER-SALLES 'interface 'salle (list (salle-i-resa resa)))

    (ifn (probefile FICHIER-DATES)
      (afficher-erreur "Le fichier de dates est absent du répertoire de données.")
   
      (with
	((current-directory (string (getenv "VAREDIT"))))
	(copier-fichier-dates rep-donnees (current-directory))      
	(ecrire-fichier-editions
	  'veresa (libelle-objet 'resa resa) `(,resa)))
      (commande-edition 'veresa))))

;;; <imprimer-ajout>
;;; imprimer un ajout de seance lors de l'ajout
;;; impression d'un ajout sans enregistrer toutes les donnees mais
;;; uniquement celles dont nous avons besoin
;;; rq: ancienne fct etait dans <i-editio.ll>

(defun imprimer-ajout (ajout)
  (let
    ((rep-donnees (current-directory)))

    (ecrire-fichier-impression
      FICHIER-AJOUT 'interface 'ajout (list ajout))
    (ecrire-fichier-impression
      FICHIER-COURS 'interface 'cours (list (cours-rattrape-i-ajout ajout)))
    (ecrire-fichier-impression
      FICHIER-SALLES 'interface 'salle (list (salle-i-ajout ajout)))
    (ecrire-fichier-impression
      FICHIER-PROFS 'interface 'prof (profs-i-ajout ajout))

    (ifn (probefile FICHIER-DATES)
      (afficher-erreur "Le fichier de dates est absent du répertoire de données.")
   
      (with
	((current-directory (string (getenv "VAREDIT"))))
	(copier-fichier-dates rep-donnees (current-directory))      
	(ecrire-fichier-editions
	  'veajout (libelle-objet 'ajout ajout) `(,ajout)))
      (commande-edition 'veajout))))

;;; <imprimer-annulation>
;;; imprimer une annulation de seance lors de son annulation
;;; impression d'une annulation sans enregistrer toutes les donnees mais
;;; uniquement celles dont nous avons besoin
;;; rq: ancienne fct etait dans <i-editio.ll>

(defun imprimer-annulation (annul)
  (let
    ((rep-donnees (current-directory)))

    (ecrire-fichier-impression
      FICHIER-ANNUL 'interface 'annulation (list annul))
    (ecrire-fichier-impression
      FICHIER-COURS 'interface 'cours (list (cours-i-annulation annul)))
    (ecrire-fichier-impression
      FICHIER-SALLES 'interface 'salle
      (list (salle-trouvee-i-cours (cours-i-annulation annul))))
    (ecrire-fichier-impression
      FICHIER-PROFS 'interface 'prof (profs-i-cours (cours-i-annulation annul)))

    (ifn (probefile FICHIER-DATES)
      (afficher-erreur "Le fichier de dates est absent du répertoire de données.")

      (with
	((current-directory (string (getenv "VAREDIT"))))
	(copier-fichier-dates rep-donnees (current-directory))      
	(ecrire-fichier-editions
	  'veannul (libelle-objet 'annul annul) `(,annul)))
      (commande-edition 'veannul))))

;;; <imprimer-modif-td>
;;; impression d'une suppression ou d'une creation de TD
;;; sup = t suppression
;;; sup = () creation

(defun imprimer-modif-td (cours famille sup)
  (let
    ((rep-donnees (current-directory)))
  
    (ecrire-fichier-impression
      FICHIER-COURS 'interface 'cours (list cours))
    (ecrire-fichier-impression
      FICHIER-GROUPES 'interface 'groupe (list famille))
    (ecrire-fichier-impression
      FICHIER-SALLES 'interface 'salle
      (when (salle-trouvee-i-cours cours)
	(list (salle-trouvee-i-cours cours))))

    (ifn (probefile FICHIER-DATES)
      (afficher-erreur "Le fichier de dates est absent du répertoire de données.")
    
    (with
      ((current-directory (string (getenv "VAREDIT"))))
      (copier-fichier-dates rep-donnees (current-directory))      
      (ecrire-fichier-editions
	(if sup 'vesuptd 'vecretd)
	(libelle-objet 'cours cours)
	`(,cours)))
    (if sup
      (commande-edition 'vesuptd)
      (commande-edition 'vecretd)))))
