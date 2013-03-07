;;; --------------------------------------------------------------------------
;;; - I-Dir.ll                                                  VARUNA 1.2   -
;;; - Interface Graphique                                                    -
;;; - Actions associees au menu "Fichier"                                    -
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; --------------------------------------------------------------------------
;;; - Creation:  Mars 93                                                     -
;;; - Mise a jour pour version 1.07: Decembre 93                             -
;;; - Build 1.22.1 01.07.99 / JH MO                                          -
;;; --------------------------------------------------------------------------


;;; demande confirmation avant de quitter
;;; si Dirty vaut t, propose d'enregistrer les donnees
;;; (on quitte avec un end)

(defun dir-quit (a)
  (if DIRTY
    (when (not (eq (enregistrer-donnees-modifiees) 0))
      (fermer-varuna))
    (when (prompt-oui-non (font time8 #Mv_quit))
      (fermer-varuna))))

(defun fermer-varuna ()
  (when MODULE-RESEAU
    (fin-transaction SITE))
  (end))

;;; Fichier / Nouveau

(defun dir-new (a)
  (nouvelle-session t))

;;; Fichier / Ouvrir
;;; enregistrer-donnees-modifiees = 0 <=> on annule !

(defun dir-load (a)
  (when (not (eq (enregistrer-donnees-modifiees) 0)) 
    (nouvelle-session ())))


;;; CRE - FD - 14/09/95
;;; <enregistrer-donnees-modifiees>
;;; propose d'enregistrer les donnees si elle ont ete modifier

(defun enregistrer-donnees-modifiees ()
  (when Dirty
    (let
      ((choix
	(when (not LECTURE-SEULE?)
	  (oui-non-annuler #Mv_enregistrer))))
      (when (eq choix t)
	(dir-sav a))
      choix)))

;;; chargement de nouveaux fichiers de donnees (eventuellement vides)
;;; DBG : JH 22/11 : ajout de messages pour repertoire invalide
;;;                  soit parce qu'il n'y a pas de donnees dans le rep
;;;                  et que l'utilisateur veut ouvrir
;;;                  soit par qu'il y en a deja et que l'utilisateur
;;;                  veut faire nouveau

(defun ouvrir-repertoire (nouveaup)
  (with ((current-font FONT-EDITEUR))
    (get-directory #Mv_dir-dir
      (lambda (dir)
	(if
	  (and
	    (directoryp dir)
	    (if nouveaup
	      (not (varuna-directory dir))
	      (varuna-directory dir))
	    (current-directory dir))
	  dir
	  (afficher-erreur
	    (column
	      #Mv_invalid-dir
	      (if nouveaup
		#Mv_effacer-donnees
		#Mv_pas-de-donnees)))
	  ())))))

(defun nouvelle-session (nouveau)
  (let
    ((ancien-repertoire (current-directory))
     (dir-ok (ouvrir-repertoire nouveau)))
    (when dir-ok
      (when MODULE-RESEAU 
	(fin-transaction SITE))
      (maj-rep-donnees dir-ok)
      (repertoire-donnees dir-ok)
      (current-directory dir-ok)
      (when MODULE-RESEAU
	(init-semaphores))
      (initialiser-lecture-seule)
      (initialiser-trace)
      (remove-application Gen-App)
      (remove-application Input-App)
      (if (interface-varuna dir-ok nouveau)
	(maj-site-connecte)
	(interface-varuna ancien-repertoire ())))))
	  
;;; copie le fichier file de old-dir vers new-dir (chemins d'accces
;;; a des repertoires DOS)

; (defun copier-fichiers-donnees (old-dir new-dir file)
; (copyfile
;   (full-file-name file old-dir)
;   (full-file-name file new-dir)))

;;; actions de l'option enregistrer sous
;;; demande un nouveau repertoire
;;; appelle copier-fichiers-donnees pour chaque fichier de l'interface

;;; AJT (10/04/95) MO
;;; ajout du fichier resa dans les fichiers a sauvegarder
;;;  AJT - FD - 31/08/95 - traitement du curseur avec <change-curseur>

(defun dir-sav-as (a)
  (let
    ((old-dir (current-directory))
     (dir
       (get-directory #Mv_dir-sav-as
      (lambda (dir)
	(if (and (directoryp dir) (not (probefile (catenate dir "varuna.trs"))))
	  dir
	  (afficher-confirmation
	    (column
	      #Mv_invalid-dir
	      #Mv_donnees-utilisees-reseau)))))))
    (when dir
      (change-curseur a Varuna-Curseur-Sablier)
      (when MODULE-RESEAU 
	(fin-transaction SITE))
      (maj-rep-donnees dir)
      (repertoire-donnees dir)
      (current-directory dir)
      (when MODULE-RESEAU
	(init-semaphores))
      (ecrire-donnees-interface)      
      (initialiser-lecture-seule)
      (initialiser-trace)
      (when MODULE-RESEAU
	(debut-transaction SITE))
      (change-curseur a Varuna-Curseur-Fleche))))

;;; appel des routines de sauvegarde

(defun dir-sav (a)
  (change-curseur a Varuna-Curseur-Sablier)
  (ecrire-donnees-interface)
  (change-curseur a Varuna-Curseur-Fleche))

;;; actions de l'option a Propos de Varuna

(defun dir-apropos (a)
  (add-and-select-application (about-application)))

;;; action de l'option mise en page
;;; mise a jour de la globale Format-Impression

(defun dir-device (a)
  (let
    ((save-dirty DIRTY)
     (device (get-device FORMAT-IMPRESSION)))
    (setq DIRTY save-dirty)
    (when device
      (setq FORMAT-IMPRESSION device))))

(defun dir-print (a)
  (let
    ((the-device (initialiser-impression))
     (titre)
     (impression-multi))
    
    (when the-device
      (setq titre (demande-chaine #Mv_add-comment))
      (when titre
	(if (eq Grid-Type 'Year)
	  (progn
	    (aff-pri () titre 0)
	    (imprimer-fichier ()))
	  (setq impression-multi
	    (demande-choix v_imp-une-semaine v_imp-plusieurs-semaines))
	  (cond
	    ((equal impression-multi #Mv_imp-une-semaine)
	      (aff-pri () titre 0)
	      (imprimer-fichier ()))
	    ((equal impression-multi #Mv_imp-plusieurs-semaines)
	      (impression-hebdo titre))))))))

;;; CRE RM 26/01/98 : impression de listes

(defun dir-lsalles (a)
  (let
    ((device)
     (titre) 
     (liste-salles ())
     (num 0))

    (ifn liste-i-salle
      (afficher-attention #Mv_aucune-salle)
      (change-curseur a Varuna-Curseur-Sablier)      
      (setq liste-salles
	(demandes-filtree-i-salle #Mv_pro-sals () t 'salle-week-p))
      (change-curseur a Varuna-Curseur-Fleche))

    (ifn liste-salles
      (afficher-attention "Aucune salle ne contient de cours cette semaine.")
      (setq device (initialiser-impression))
      (when device
	(setq titre (demande-chaine #Mv_add-comment))
	(when titre
	  (mapc
	    (lambda (salle)
	      (maj-liste-salles-aff (list salle))
	      (update-grid-titles)
	      (aff-pri num titre (length liste-salles))
	      (incr num))
	    liste-salles)
	  (if (> (length liste-salles) 1)
	    (imprimer-fichier num)
	    (imprimer-fichier ())))))))
  
(defun dir-lprofs (a)
  (let
    ((device)
     (titre) 
     (liste-profs ())
     (num 0))

    (ifn liste-i-prof
      (afficher-attention #Mv_aucun-prof)
      (change-curseur a Varuna-Curseur-Sablier)      
      (setq liste-profs
	(demandes-filtree-i-prof #Mv_pro-profs () t 'prof-week-p))
      (change-curseur a Varuna-Curseur-Fleche))

    (ifn liste-profs
      (afficher-attention "Aucun professeur ne dispense de cours cette semaine.")
      (setq device (initialiser-impression))
      (when device
	(setq titre (demande-chaine #Mv_add-comment))
	(when titre
	  (mapc
	    (lambda (prof)
	      (maj-liste-profs-aff (list prof))
	      (update-grid-titles)
	      (aff-pri num titre (length liste-profs))
	      (incr num))
	    liste-profs)
	  (if (> (length liste-profs) 1)
	    (imprimer-fichier num)
	    (imprimer-fichier ())))))))

(defun salle-week-p (s)
  (let
    ((liste ())
     (salle-ok ()))
    (ifn Week-Aff
      t
      (mapc
	(lambda (ajt)
	  (when
	    (and
	      (eq s (salle-i-ajout ajt))
	      (= (semaine-instant (instant-i-ajout ajt)) Week-Aff)
	      (setq salle-ok t))))
	liste-i-ajout)
      (when (not salle-ok)
	(mapc
	  (lambda (r)
	    (when
	      (and
		(eq s (salle-i-resa r))
		(= (semaine-instant (instant-i-resa r)) Week-Aff)
		(setq salle-ok t))))
	  liste-i-resa))
      (when (not salle-ok)
	(mapc
	  (lambda (c)
	    (when (eq s (salle-trouvee-i-cours c))
	      (newl liste c)))
	  liste-i-cours))
      (if salle-ok
	t
	(if liste
	  (any 'cours-week-p liste)
	  ())))))

(defun prof-week-p (p)
  (ifn Week-Aff
    t
    (or
      (any 'cours-week-p (cours-i-prof p))
      (and t
	(supprimer-occurences-vides
	  (mapcar
	    (lambda (ajt)
	      (and
		(= (semaine-instant (instant-i-ajout ajt)) Week-Aff)
		(appartenir-liste p (profs-i-ajout ajt))))
	    liste-i-ajout))))))
    	
(defun cours-week-p (c)
  (let
    ((instant (instant-trouve-i-cours c)))
    (ifn instant
      ()
      (and
	(<= (semaine-instant instant) Week-Aff)
	(< Week-Aff (+ (semaine-instant instant) (nb-seances-i-cours c)))))))

(defun initialiser-impression ()
  (let
    ((the-device)
     (new-devicep))
    
    (if (not Format-Impression)
      (setq the-device (dir-device a))
      (setq new-devicep (oui-non-annuler #Mv_pro-device))
      (cond
	((eq new-devicep t) (setq the-device (dir-device a)))
	((eq new-devicep ()) (setq the-device Format-Impression))
	((eq new-devicep 0) (setq the-device ()))))
     the-device))
       

;;; CRE RM - 26/08/97 : impression sur plusieurs semaines (en mode hebdo)

(defun impression-hebdo (titre)
  (let
    ((i 0)
     (num 0)
     (semaine ())
     (sem-deb ())
     (sem-fin ())
     (lsite-sem ()))

    (setq semaine Week-Aff)
    (setq sem-deb
      (demande-semaine #Mv_premiere-semaine Semaine-Debut Semaine-Fin))
    (when sem-deb
      (setq sem-fin
	(demande-semaine #Mv_deuxieme-semaine sem-deb Semaine-Fin))
    
      (for (i sem-deb 1 sem-fin t)
	(setq Week-Aff i)
	(aff-pri num titre (+ (- sem-fin sem-deb) 1))
	(incr num))
      (imprimer-fichier num))
    (setq Week-Aff semaine)))
      

;;; DESINSTANCIATION DES DONNEES DE L'INTERFACE
;; FD - 13/03/96 - CRRR il existe dans i-contr.ll une fonction desinstancier
;; mais qui prend en argument un cours
;; celle-ci n'est pas utilisee
;; (defun desinstancier ()
;;   (mapc (lambda (i-c)
;;     (instant-trouve-i-cours i-c ())
;;     (salle-trouvee-i-cours i-c ()))
;;   Liste-I-Cours))

  
;;; edition des parametre de l'ecole
;;; dans le menu fichier du mode saisie
;;; recalcule toutes le globales de l'ecole
;;; (voir calculer-champs-i-ecole ds i-ecole.ll)
;;; desinstancie tous les cours de l'ecole
;;; MOD - FD - 13/03/96 
;;;(desinstancier-les-cours vrai)=>(desinstancier-les-cours 'desinstanciable-p)

(defun don-ecole (a)
  (let*
    ((liste (component 'liste (component 'type a)))
     (selection (send 'get-selected-string liste)))
    
    (when (not I-ECOLE)
      (ecole-defaut))
    ;; MOD RM - 22/04/96 : ajout de <creationp> dans <editer-i-ecole>
    (when (editer-i-ecole I-ECOLE ())
      (calculer-champs-i-ecole)
      ;; AJT - 13/03/96
      (change-curseur a Varuna-Curseur-Sablier)
      (defiger-les-cours t desinstanciable-p)
      (desinstancier-les-cours desinstanciable-p)
      (change-curseur a Varuna-Curseur-Fleche))      
    (mise-a-jour-listing liste selection)))

;;; edition des parametres "caches" a l'utilisateur
;;; (cf l'editeur owner dans i-datab.ll")
;;; cette fonction renvoie une image aida (le nom de l'ecole)
;;; MOD - FD - 13/03/96 
;;;(desinstancier-les-cours vrai)=>(desinstancier-les-cours 'desinstanciable-p)
;;; MOD RM - 23/04/96 : ajout de <creationp>

(defun edit-owner-ecole ()
  (let* ((appli
    (grab-and-select-application
      (fenetre-i-ecole-owner (car Liste-I-Ecole) ())))
  (new-ecole ({application}:userdata appli)))
  (when new-ecole
    (calculer-champs-i-ecole)
    ;; AJT - 13/03/96
    (defiger-les-cours t desinstanciable-p)
    (desinstancier-les-cours desinstanciable-p)
    (font time8b (libelle-i-ecole new-ecole)))))

;;; <desinstanciable-p>
;;; CRE - FD - 13/03/96
;;; indique si on peut desinstancier un cours
;;; i.e. un cours est instancie a un horaire non compatible.

(defun desinstanciable-p (i-cours)
  (and
    (i-instancie-modifiable-p i-cours)
    (not
      (horaire-debut-possible-p
	i-cours
	(moment-instant-cumule
	  (instant-interne2instant-cumule
	    (instant-trouve-i-cours i-cours)))))))

;;; <don-semaines-calendaires> - CRE 31/07/96
;;; saisir la correspondance entre semaines internes Varuna et semaines
;;; calendaires si nous travaillons en semaines calendaires sinon saisie
;;; du nombre de semaines

(defun don-semaines-calendaires (a)
  (let
    ((nb-sem))
    (if (calendaire-i-ecole I-ECOLE)
      (don-periodes-calendaires)
      (setq nb-sem
	(demande-entier #Mv_i-ecole-semaine-fin 1 NB-SEMAINES))
      (when nb-sem
	(semaine-fin-i-ecole I-ECOLE nb-sem)
	(calculer-champs-i-ecole)))))

;;; <don-periodes-calendaires> CRE - FD - 01/08/96

(defun don-periodes-calendaires ()
  (let
    ((nb-sem 0))
    (editer-i-calendaire SEM-CALENDAIRES ())
    ;; trier la liste des intervalles de semaines en fonction de la semaine de
    ;; debut des cours : premiere-sem-i-calendaire
    (periodes-i-calendaire SEM-CALENDAIRES
      (trier-liste-intervalles
	(periodes-i-calendaire SEM-CALENDAIRES)
	(premiere-sem-i-calendaire SEM-CALENDAIRES)))
	
    ;; mise a jour du nombre de semaines
    (mapc
      (lambda (periode)
	(setq nb-sem
	  (add
	    nb-sem
	    (- (borne-sup-intervalle periode)
	       (borne-inf-intervalle periode)))))
      (periodes-i-calendaire SEM-CALENDAIRES))
    (semaine-fin-i-ecole I-ECOLE nb-sem)
    (periodes-calendaires-i-ecole I-ECOLE (periodes-i-calendaire SEM-CALENDAIRES))
    (calculer-champs-i-ecole)))

;;; CRE RM - 17/12/96 : Gestion de la pause pour l'etablissement

(defun don-pause (a)
  (let
    ((pause
      (demande-entier-pause
	(catenate #Mv_ancienne-pause (pause-i-ecole i-ecole))
	#Mv_valeur-pause 0 60)))
    (when pause
      (setq DIRTY t)
      (pause-i-ecole I-ECOLE pause))))

;;; CRE RM 28/02/97 : Gestion des sites (ameliorations des fonctionnalites)
;;; mise … jour des acces pour les differents sites

;;; Toutes creation d'un site a partir du menu est forcement de
;;; type SECONDAIRE (le site de type CENTRAL utilise le menu)

(defun don-creer-site (a)
  (let
    ((nouveau (nouveau-i-site)))
    (code-i-site nouveau (gencode-i-site))
    (type-i-site nouveau 'secondaire)
    (when (editer-i-site nouveau t)
      (if (and MODULE-RESEAU (not (interdire-acces EDT-SEM)))
	(afficher-attention #Mv_acces-edt-interdit)
	(change-curseur a Varuna-Curseur-Sablier)
	(ecrire-fichier FICHIER-SITES 'interface 'site ())
	(lire-fichier FICHIER-SITES 'interface)
	(autoriser-acces EDT-SEM)	
	(change-curseur a Varuna-Curseur-Fleche)))))

(defun don-modif-site (a)
  (let
    ((courant (demande-i-site "Quel site ?" ())))
    (when (and courant (editer-i-site courant ()))
      (if (and MODULE-RESEAU (not (interdire-acces EDT-SEM)))
	(afficher-attention #Mv_acces-edt-interdit)
	(change-curseur a Varuna-Curseur-Sablier)      
	(ecrire-fichier FICHIER-SITES 'interface 'site ())
	(lire-fichier FICHIER-SITES 'interface)	
	(autoriser-acces EDT-SEM)
	(change-curseur a Varuna-Curseur-Fleche)))))

;;; On ne supprime jamais le site CENTRAL !!!

(defun don-suppr-site (a)
  (let
    ((courant (demande-i-site "Quel site ?" ())))
    (when courant
      (if (eq (type-i-site courant) 'central)
	(afficher-erreur "Vous ne pouvez pas supprimer le site central.")
	(when (supprimer-i-site courant t)
	  (if (and MODULE-RESEAU (not (interdire-acces EDT-SEM)))
	    (afficher-attention #Mv_acces-edt-interdit)
	    (change-curseur a Varuna-Curseur-Sablier)      	  
	    (ecrire-fichier FICHIER-SITES 'interface 'site ())
	    (lire-fichier FICHIER-SITES 'interface)	    
	    (autoriser-acces EDT-SEM)	    
	    (change-curseur a Varuna-Curseur-Fleche)))))))
