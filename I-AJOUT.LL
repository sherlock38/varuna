;;; --------------------------------------------------------------------------
;;; - I-ajout.ll                                               VARUNA 1.2    -
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; - Interface Graphique                                                    -
;;; - Fonctions utilisees pour l'ajout de seances                          -
;;; --------------------------------------------------------------------------
;;; - Creation:  Juillet 95 (FD)                                             -
;;; - Build 1.22.1 01.07.99 / FD                                             -
;;; --------------------------------------------------------------------------

;;; recherche la liste des salles disponibles en fonction de la demande
;;; effectuee (action declenchee par le bouton <RECHERCHER> de la fenetre 
;;; d'ajout de seance.

(defun rechercher-ajout (b)
  (let
    ((objet-ajout)
     (editeur (component 'salles-disponibles b))
     (dispo-ajout))

    (change-curseur b Varuna-Curseur-Sablier)
    (current-keyboard-focus-window ()) ;; il n'y a plus de proprio du clavier
    ;; mise a vide de la salle choisie
    (send 'set-line (component 'salle b) "")
    (ifn
      (demande-valide-p
          b 
          '(duree semaine jour type-salle 
            audience horaire1 horaire2))

      (send 'i-set-values editeur () () ())

      (sauver-demande 
          b 
          '(semaine jour duree type-salle audience 
            horaire1 horaire2 dispo-cours-familles dispo-profs
            contraintes-familles profs))
      (setq objet-ajout 
        ({application}:userdata (component 'appli b)))

      (setq dispo-ajout
	(cons-dispo
	  (trouver-dispo-moment DISPO-ECOLE)
	  (creer-zone
	    (creer-intervalle
	      (semaine-i-ajout objet-ajout)
	      (+ (semaine-i-ajout objet-ajout) 1)))))
      
      ;; AJT - FD - 01/08/95
      ;; calcule de la dispo de l'ajout en fonction du flag sur la
      ;; dispo du COURS
      (when (dispo-cours-familles-i-ajout objet-ajout)
        (setq dispo-ajout
	  (intersection-dispo
	    dispo-ajout
	    (reduire-dispo-cours (cours-rattrape-i-ajout objet-ajout)))))
         
      ;; calcule de la dispo de l'ajout en fonction du flag sur la
      ;; dispo des PROFS
      ;;; 
      (when (dispo-profs-i-ajout objet-ajout)
          (mapc 
	    (lambda (prof)
	      (setq dispo-ajout
		(intersection-dispo dispo-ajout (dispo-i-prof prof))))
	    (profs-i-ajout objet-ajout)))

      ;; AJT - FD - 24/08/95
      ;; tenir compte des contraintes qui pesent sur le cours auquel on ajoute
      ;; une seance

      (setq in dispo-ajout)
      
      (when (contraintes-familles-i-ajout objet-ajout)
        (setq
          dispo-ajout
          (intersection-dispo
            dispo-ajout
            (reduction-par-contraintes
              dispo-ajout
              (semaine-i-ajout objet-ajout)
	      (duree-i-ajout objet-ajout)
	      1				; nb de seances
	      (cours-rattrape-i-ajout objet-ajout)
              '(poser-non-chevauchement
		poser-meme-semaine
                poser-jours-differents
		poser-meme-jour
		poser-simultaneite
		poser-simultaneite-fin-seance
		poser-meme-horaire
		poser-precedence
		poser-proximite-semaine)
	      ()))))
      
      ;; verifier que la dispo restante est suffisante
      ;; FD - 17/08/95
      (when 
        (not
	  (dispo-ajout-suffisante-p
	    dispo-ajout
	    (semaine-i-ajout objet-ajout)
	    (jour-libelle (jour-i-ajout objet-ajout))
	    (duree-i-ajout objet-ajout)
	    (cours-rattrape-i-ajout objet-ajout)))
	(cons-erreur 'err-dispo-ajout ()))

      ;; recherche des salles disponibles
      (when
        (not *erreurs*)
	(setq Salles-Disponibles
            (salles-libres 
	      ;; obligatoires
	      (semaine-i-ajout objet-ajout)
	      (jour-libelle (jour-i-ajout objet-ajout))
	      (duree-i-ajout objet-ajout)
	      ;; facultatif salle
	      ()
	      (type-salle-i-ajout objet-ajout) 
	      (audience-i-ajout objet-ajout)
	      ;; facultatif temps
	      (horaire1-i-ajout objet-ajout) 
	      (horaire2-i-ajout objet-ajout) 
	      ()
	      dispo-ajout
	      1))) ;; AJT - FD - 01/08/95 
      (cond
        (*erreurs*
            ;; salles-libres -> t => *erreurs*
            (send 'i-set-values editeur () () ())
            (traiter-erreur))
        
        ((not Salles-Disponibles)
            ;; salles-libres -> () => pas de salle
            (send 'i-set-values editeur () () ())
            (afficher-attention "Pas de salle disponible."))

        ((aucun-creneau-p Salles-Disponibles)
             (send 'i-set-values editeur () () ())
             (afficher-attention "Pas de créneau horaire possible."))
        
        (t
	  ;; mise a jour de la liste des salles disponibles
           ;; rq FD 01/08/95 car, cadr, ... ???? (accesseurs)
           (send 'i-set-values 
              editeur
              (mapcar 'libelle-i-salle (car Salles-Disponibles))
              (mapcar 'string (cadr Salles-Disponibles))
              (mapcar (lambda (x)
                        (if (consp (car x))
                          (intervalles-cumules2intervalles-naturels x)
                          (horaires-cumules2horaires-naturels x)))
          
                  (caddr Salles-Disponibles))) ;;definir un accesseur
           (when (eq (length (car Salles-Disponibles)) 1)
               ;; on ne peut choisir qu'une salle
               (selection-automatique b)))))
    (change-curseur b Varuna-Curseur-Fleche)))


;;; <sauver-demande>
;;; 
(defun sauver-demande (ed champs-a-sauver)
    (mapc (lambda (c)
            (and 
            ;;;!!!    (send 'i-valider (component c ed))
                (send 'i-sauver (component c ed))))
        champs-a-sauver))

;;; <demande-valide-p>
;;; verifie que tous les champs pouvant etre utilises pour la 
;;; recherche ont bien une valeur 'valide'
;;;
(defun demande-valide-p (ed liste-champs-a-verifier)
  (let ((resultat t)
        (objet ({application}:userdata 
                                  (component 'appli ed))))

    (mapc (lambda (c)
            (and resultat
                (setq resultat
                      (send 'i-valider (component c ed)))))
        liste-champs-a-verifier)
    (and resultat
        (or (not(nullstrp (send 'get-line 
                              (component 'semaine ed))))
            (and (alerte #Mv_i-resa-semaine) ()))
        (verifier-creneau ed))))


;;; <ajouter>
;;; effectue l'ajout d'une séance apres verification que les donnees
;;; saisies sont bien les bonnes
;;; MAJ
;;; prendre le car de (libelle-i-salles2code-i-salles salle)
;;; fait tout planter si la salle correspondant au car est occupee
;;;
(defun ajouter (ajout)
  (let* 
    ((copie)
     (confirm) 
     (salle (salle-i-ajout ajout))
     ;;; deux salles peuvent avoir le meme nom mais des codes
     ;;; differents -> on prend le car
     (code-salle (car (libelle-i-salle2codes-i-salle salle)))
     (pos (position-element code-salle (car Salles-Disponibles)))
     (intervalles (nth pos (caddr Salles-Disponibles)))) ;;; intervalle ou heure
    ;;; verification de l'heure de debut
    (cond
      ((not (horaire-resa-possible-p
                 (heure-ajout-i-ajout ajout)
                 (duree-i-ajout ajout)
                 intervalles))
       (afficher-attention
	 "L'ajout de séance ne peut pas se faire à cet horaire.")
       ())
      ((not (horaire-type-cours-p (heure-ajout-i-ajout ajout)))
       (afficher-erreur "Ce n'est pas un horaire de début de cours...")
       ())
      ;; AJT - 04/03/96 - MultiP
      ((not MODULE-SALLES)
	(afficher-attention #Mv_enregistrement-impossible)
	())
      ((donnee-lecture-seule-p (cours-rattrape-i-ajout ajout) t)
	;; 01/03/96 - AJT - MultiP
	(traiter-erreur)
	())

      (t
        ;; FD !! (car ...)!!
       (salle-i-ajout ajout 
            (car (libelle-i-salle2codes-i-salle salle)))
        (instant-i-ajout ajout 
            (cons-instant 
                (semaine-i-ajout ajout) 
                (cons-moment 
                    (jour-libelle (jour-i-ajout ajout))
                    (cumule2interne (heure-ajout-i-ajout ajout)))))
         (setq confirm (confirm-ajout ajout))
         (when confirm
             (newl Liste-I-Ajout ajout)
             (when (eq confirm 0)
                 (imprimer-ajout ajout))
             liste-i-ajout)))))


;;; demande confirmation de la reservation a effectuer
(defun confirm-ajout (ajout)
  (let* (
         (instant (instant-i-ajout ajout))
         (duree (duree-i-ajout ajout))
         (horaire-debut (heure-ajout-i-ajout ajout))
         (horaire-fin (add horaire-debut duree)))
    
    ;; rq: mettre les message dans des 
    (ok-annuler-imprimer
        (column 
            #Mv_i-ajout-confirm
	    ;; MOD - 09/05/96 ajout du "split"
            (apply 'column (separer-chaine (libelle-i-ajout ajout) 40))
            (row 
                #Mv_pourle
                (string (libelle-jour (jour-instant instant)))
                #Mv_de
                (cumule2naturel 
                    (horaire-moment-cumule horaire-debut))
                #Mv_a
                (cumule2naturel 
                    (horaire-moment-cumule horaire-fin)))
            (row 
                #Mv_delasem
		;; AJT - FD - 02/08/96 semaines calendaires
                (string (ecrire-numero-semaine (semaine-instant instant))))
            (row #Mv_ensalle
                (libelle-i-salle
                    (eval (salle-i-ajout ajout))))))))

;;; FONCTIONS A METTRE DANS i-util.ll

;;; <aucun-creneau-p>
;;;
(defun aucun-creneau-p (salles-libres)
  (null (creneaux-salles-libres salles-libres)))

;;; <creneaux-salles-libres>
;;;
(defun creneaux-salles-libres (salles-avec-creneaux)
  (car salles-avec-creneaux))


;;; <supprimer-ajout>
;;; FD - CRE - 31/07/95
;;; RM - MOD - 16/08/95
;;; suppression d'un ajout de seance
(defun supprimer-ajout (b)
  (let ((ajout (component 'lajout b)))
    (if
      (or
	(donnee-lecture-seule-p ajout t)
	(not (ajout-site-p ajout)))
      (traiter-erreur)
    
      (when (supprimer-i-ajout ajout t)
	(setq Liste-Ajouts-Aff (supprimer-occurence ajout Liste-Ajouts-Aff))
	(remove-application (component 'appli b))
	(show-edt b)))))

;;; <salle-annulation>
;;; CRE - FD - 21/07/95
;;; permet de connaitre la salle liberee par l'annulation d'une seance
(defun salle-annulation (annul)
  (salle-trouvee-i-cours (cours-i-annulation annul)))


;;; <reduire-dispo-cours>
;;; CRE - FD - 02/08/95
;;; reduit la dispo passee en parametre en fonction de la dispo globale
;;; du cours (dispo du cours plus celle de ses familles)

(defun reduire-dispo-cours (cours)
  (let ((dispo (dispo-i-cours cours)))
    ;; prendre en compte les dispo des familles
    (mapc
        (lambda (groupe)
          (setq dispo
                (intersection-dispo dispo (dispo-i-groupe groupe))))
        (tous-les-i-groupes-peres cours))
    dispo))

;;;  CRE RM 080895
;;;  il serait peut etre interessant d'eliminer physiquement ces objets !!!
;;;  MOD RM 17/08/95
;;;  Effacement physique des objets
;;;  MOD RM - 10/06/97 : on ne supprime les annulations et les ajouts
;;;  ques des cours non figes pris en compte dans la resolution

(defun supprimer-annulations-ajouts ()
   (mapc
     (lambda (ann)
       (when (i-pas-figep (cours-i-annulation ann))
	 (supprimer-i-annulation ann ())))
     liste-i-annulation)
   (mapc
     (lambda (ajt)
       (when (i-pas-figep (cours-rattrape-i-ajout ajt))
	 (supprimer-i-ajout ajt ())))
     liste-i-ajout)
   t)

;;; AJT RM 08/08/95 : effacement des annulations et des ajouts
;;; MOD RM 21/08/95 : simplification de code
;;; MOD RM 11/06/97 : on ne signale que les annulations et les ajouts
;;; des cours non figes

(defun declencher-resolutionp (coursp)
  (let
    ((resol t))

    (when coursp
      (if (existe-raco-pas-fige-p) 
	(ifn
	  (afficher-confirmation
	    (column
	      #Mv_i-sup-annul-ajout-run1
	      #Mv_i-sup-annul-ajout-run2
	      #Mv_i-run))
	  (setq resol ())
	  (supprimer-annulations-ajouts)))
      (when (and resol liste-i-ajout)
	(setq resol (demander-niveau-contraintes))))
    resol))

(defun existe-raco-pas-fige-p ()
  (or
    (any
      (lambda (ajt) (i-pas-figep (cours-rattrape-i-ajout ajt)))
      liste-i-ajout)
    (any
      (lambda (ann) (i-pas-figep (cours-i-annulation ann)))
      liste-i-annulation)))

(defun demander-niveau-contraintes ()
  (let
    ((niveau (appli-niveau-contraintes))
     (retour))
    (cond
      ((equal niveau #Mv_toutes-contraintes)
	(setq RESOLUTION-CONTRAINTES-MAX t)
	(setq retour t))
      ((equal niveau #Mv_aucune-contrainte)
	(setq RESOLUTION-CONTRAINTES-MAX ())
	(setq retour t))
      (t (setq retour ())))
    retour))
           
(defun appli-niveau-contraintes ()
  (let*
     ((tous (radiobutton 0 0 250 15 #Mv_toutes-contraintes))
       (aucune (radiobutton 0 0 250 15 #Mv_aucune-contrainte))
       (p-contraintes
	 (selectorapplication 0 0 250 30 (column tous aucune)))
       (retour ()))
     (send 'set-selection p-contraintes tous)
     (when
       (afficher-confirmation
	 (column
	   #Mv_niveau-contraintes #Mv_niveau-contraintes2
	   "" p-contraintes ""))
       (setq retour (send 'title (send 'get-selection p-contraintes))))
     retour))
       

;;; CRE RM 16/08/95
;;; <groupes-i-ajout> retourne la liste des familles auxquelles
;;; appartiennent le cours dont on ajoute une seance

(defun groupes-i-ajout (ajout)
  (groupes-i-cours (cours-rattrape-i-ajout ajout)))

;;; <dispo-duree-insuffisante-p> verifie que la dispo passee en parametre
;;; CRE - FD - 17/08/95
;;; rq on tient compte des heures de debut qui sont fonction du type de 
;;; cours auquel on ajoute une seance.

(defun dispo-ajout-suffisante-p (dispo sem jour duree cours)
  (dispo-suffisante-duree-p 
      (dispo-moment-ce-jour dispo sem jour)
      duree
      (type-i-cours cours)))

;;; CRE RM - 19/09/95 : fonction de controle de l'editeur d'annulation
;;; MOD - RM FD - 02/11/95 - l'instant doit etre calcule en premier pour
;;;     pouvoir l'afficher dans la fenetre de confirmation

(defun annuler (annul)
  (let ((confirm))
    ;; MOD RM - 14/08/96 : semaine calendaire
    (when (calendaire-i-ecole I-ECOLE)
      (semaine-i-annulation annul
	(string
	  (lire-numero-semaine
	    (string2number (semaine-i-annulation annul))))))
    ;; MOD - FD RM : 30/10/95 : calcul de l'instant avant la confirmation
    (instant-i-annulation 
      annul 
      (cons-instant 
	(string2number (semaine-i-annulation annul))
	(moment-instant 
	  (instant-trouve-i-cours (cours-i-annulation annul)))))
  
    ;; AJT - 04/03/96 - MultiP
    (if (not MODULE-SALLES)
      (afficher-attention "L'enregistrement n'est possible sur ce poste.")
      (setq confirm (confirm-annulation annul)))
    
    (when confirm
      (newl Liste-I-Annulation annul)
      (when (eq confirm 0)
	(imprimer-annulation annul))
      liste-i-annulation)))


;;; demande confirmation de l'annulation a effectuer

(defun confirm-annulation (annul)
  (let*
    ((instant (instant-i-annulation annul))
      (duree (duree-i-annulation annul))
      (horaire-debut (interne2cumule (horaire-instant instant)))
      (horaire-fin (add horaire-debut duree)))
    
    (ok-annuler-imprimer
      (column 
        #Mv_i-annul-confirm
	(libelle-i-annulation annul)
        (row 
          #Mv_pourle
          (string (libelle-jour (jour-instant instant)))
          #Mv_de
          (cumule2naturel 
            (horaire-moment-cumule horaire-debut))
          #Mv_a
          (cumule2naturel 
            (horaire-moment-cumule horaire-fin)))
        (row 
          #Mv_delasem
          (string
	    (ecrire-numero-semaine
	      (string2number (semaine-i-annulation annul)))))
        (row #Mv_ensalle
	  (if
	    (salle-trouvee-i-cours 
	      (cours-i-annulation annul))
	    (libelle-i-salle
	      (salle-trouvee-i-cours 
		(cours-i-annulation annul)))
	    ""))))))

;;; Permutation de deux seances de cours
;;; CRE RM - 08/01/97 

(defun permuter-seances (a)
  (let
    ((cours1) (sem1)
     (cours2) (sem2)
     (continuerp t))
    
    (setq cours1 (demander-cours #Mv_permut-cours1))
    (when cours1
      (setq sem1 (demander-semaine #Mv_permut-sem1 cours1))
      (when sem1
	(setq cours2 (demander-cours #Mv_permut-cours2))
	(when cours2
	  (setq sem2 (demander-semaine #Mv_permut-sem2 cours2)))))
    (when sem2
      (when MODULE-RESEAU
	(when (not (donnee-autorisee-reseau-p cours1))
	  (setq continuerp ()))
	(when (not (donnee-autorisee-reseau-p cours2))
	  (setq continuerp ())))
      (when continuerp
	(analyser-permutation cours1 cours2 sem1 sem2)
	(if *erreurs*
	  (traiter-erreur)
	  (change-curseur a Varuna-Curseur-Sablier)	
	  (executer-permutation cours1 cours2 sem1 sem2)
	  (setq DIRTY t)
	  (show-edt a)
	  (change-curseur a Varuna-Curseur-Fleche)	
	  (afficher-information #Mv_permut-ok))))))

(defun donnee-autorisee-reseau-p (cours)
  (if (eq (debut-modification SITE cours MODIFICATION) ACCES-AUTORISE)
      t
      (afficher-attention
	(column
	  #Mv_modif-acces-refuse
	  (row #Mv_donnee (send 'objet2string cours) #Mv_modifiee)
	  #Mv_autre-utilisateur))
      ()))

(defun analyser-permutation (cours1 cours2 sem1 sem2)
  (cond
    ((eq cours1 cours2)
      (cons-erreur 'ERRPERMUTCOURS cours1))
    ((not (eq (duree-i-cours cours1) (duree-i-cours cours2)))
      (cons-erreur 'ERRPERMUTDUREE cours1))
    ((not (capacite-salle-suffisante-p cours1 cours2))
      (cons-erreur 'ERRPERMUTCAPA cours1))
    ((annulation-impossible-p cours1 sem1)
      (cons-erreur 'ERRPERMUTNOANNUL (list cours1 sem1)))
    ((annulation-impossible-p cours2 sem2)
      (cons-erreur 'ERRPERMUTNOANNUL (list cours2 sem2)))
    (t t)))

(defun executer-permutation (cours1 cours2 sem1 sem2)
  (creer-annulation cours1 sem1 ())
  (creer-annulation cours2 sem2 ())
  (creer-ajout-seance
    cours1 sem2
    (cons-instant sem2
      (moment-instant (instant-trouve-i-cours cours2)))
    (salle-trouvee-i-cours cours2) ())
  (creer-ajout-seance
    cours2 sem1
    (cons-instant sem1
      (moment-instant (instant-trouve-i-cours cours1)))
    (salle-trouvee-i-cours cours1) ()))

(defun demander-cours (prompt)
  (demande-filtree-i-cours
    prompt
    #Mv_aucun-cours-place
    ()
    'cours-modifiable-instancie-salle-p))

(defun demander-semaine (prompt cours)
  (let
    ((sem-debut (semaine-instant (instant-trouve-i-cours cours))))
    (demande-semaine prompt
      sem-debut
      (- (+ sem-debut (nb-seances-i-cours cours)) 1))))

;;; Booleen retournant VRAI si le cours est une donnee modifiable (site),
;;; et instancie et dans une salle

(defun cours-modifiable-instancie-salle-p (cours)
  (and
    (i-instanciep cours)
    (salle-trouvee-i-cours cours)
    (donnee-modifiable-p cours)))

(defun capacite-salle-suffisante-p (cours1 cours2)
  (let
    ((capa1 (capacite-i-salle (salle-trouvee-i-cours cours1)))
     (capa2 (capacite-i-salle (salle-trouvee-i-cours cours2)))
     (aud1 (audience-i-cours cours1))
     (aud2 (audience-i-cours cours2)))
    (and
      (>= (min capa1 capa2) (max aud1 aud2))
      t)))

(defun annulation-impossible-p (cours sem)
  (let
    ((value ()))
    (mapc
      (lambda (annul)
	(when (eq (cours-i-annulation annul) cours)
	  (when (equal (semaine-i-annulation annul) (string sem))
	    (setq value t))))
      Liste-I-Annulation)
    value))

;;; On cree les annulations de facon automatique
;;; flag = t (permet le UNDO pour les jours feries)

(defun creer-annulation (cours sem flag)
  (let
    ((annul (nouveau-i-annulation)))
    (code-i-annulation annul (gencode-i-annulation))
    (set (code-i-annulation annul) annul)
    (when MODULE-RESEAU
      (debut-modification SITE annul CREATION))
    (libelle-i-annulation annul
      (catenate "Annulation de " (libelle-i-cours cours)))
    (code-cours-i-annulation annul (code-i-cours cours))
    (cours-i-annulation annul cours)
    (libelle-cours-i-annulation annul (libelle-i-cours cours))
    (duree-i-annulation annul (duree-i-cours cours))
    (semaine-i-annulation annul (string sem))
    (instant-i-annulation annul 
      (cons-instant 
	(string2number (semaine-i-annulation annul))
	(moment-instant 
	  (instant-trouve-i-cours (cours-i-annulation annul)))))
    (flag-i-annulation annul flag)
    (newl Liste-I-Annulation annul)    
    annul))

;;; On cree les ajouts de facon automatique
;;; flag = t (permet le UNDO pour les jours feries)

(defun creer-ajout-seance (cours sem instant salle flag)
  (let
    ((ajout (nouveau-i-ajout)))
    (code-i-ajout ajout (gencode-i-ajout))
    (set (code-i-ajout ajout) ajout)
    (when MODULE-RESEAU
      (debut-modification SITE ajout CREATION))
    (libelle-i-ajout ajout
      (catenate "Ajout de " (libelle-i-cours cours)))
    (cours-rattrape-i-ajout ajout cours)
    (instant-cours-rattrape-i-ajout ajout (instant-trouve-i-cours cours))
    (duree-i-ajout ajout (duree-i-cours cours))
    (semaine-i-ajout ajout sem)
    (profs-i-ajout ajout (profs-i-cours cours))
    (salle-i-ajout ajout salle)
    (instant-i-ajout ajout instant)
    (flag-i-ajout ajout flag)
    (newl Liste-I-ajout ajout)    
    ajout))

;;; CRE RM 04/11/97
;;; Fonction qui retourne la liste des ajouts de seances d'un cours

(defun liste-ajouts-i-cours (cours)
  (let
    ((lst ()))
    (when (consp cours)
      (setq cours (car cours)))
    (mapc
      (lambda (ajt)
	(when (eq (cours-rattrape-i-ajout ajt) cours)
	  (newl lst ajt)))
      liste-i-ajout)
    lst))
