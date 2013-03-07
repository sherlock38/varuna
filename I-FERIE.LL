;;; ------------------------------------------------------------------------
;;; - I-Ferie.ll                                              VARUNA 1.2   -
;;; - Copyright Abstract, France 1993-2006                                 -
;;; - under licence MIT Expat since 2006                                   -
;;; - Interface                                                            -
;;; - Module d'utilitaires pour la gestion des jours feries                -
;;; - Aida & Lisp                                                          -
;;; ------------------------------------------------------------------------
;;; - Creation:  Oct 1997                                                  -
;;; - Build 1.22.1 01.07.99 / RM                                           -
;;; ------------------------------------------------------------------------

;;; Trois methodes implementees :
;;;
;;;    1. Annulation complete d'une journee
;;;    2. Deplacement complet de la journee
;;;    3. Deplacement de certains cours d'une journee vers une autre

;;; 1. Annulation complete d'une journee

(defun annuler-journee (a)
  (let
    ((jour)
     (jtmp)
     (semaine)
     (lcours)
     (lpossible))
    
    (if (not (existe-cours-modifiable-instancie-p))
      (afficher-attention #Mv_aucun-cours-place)
      (setq jtmp (demande-jour-ouvres))
      (setq jour (if jtmp (jour-libelle jtmp) ()))
      (when jour
	(setq semaine
	  (demande-semaine #Mv_pro-sem Semaine-Debut Semaine-Fin))
	(when semaine
	  (setq lcours
	    (sous-liste
	      (lambda (c) (semaine-jour-cours-p c jour semaine))
	      Liste-I-Cours))
	  (if (not lcours)
	    (afficher-attention
	      (row
		"Aucun cours n'est placé le " (string (libelle-jour jour))
		" de la semaine " (string semaine) "."))

	    (change-curseur a Varuna-Curseur-Sablier)		
	    (setq lpossible (annulation-journee-possible lcours jour semaine))
	    (change-curseur a Varuna-Curseur-Fleche)		
	    (when lpossible
	      (change-curseur a Varuna-Curseur-Sablier)		
	      (annulation-journee lpossible jour semaine)
	      (setq DIRTY t)
	      (show-edt a)
	      (change-curseur a Varuna-Curseur-Fleche)	
	      (afficher-information "Annulation de la journée terminée."))))))))
    
;;; Fonction retournant la liste des cours dont l'annulation est possible
;;; (verifie que l'annulation de cette seance n'a pas encore ete faite).

(defun annulation-journee-possible (lcours jour semaine)
  (let
    ((liste-possible
      (sous-liste
	(lambda (c)
	  (not (annulation-impossible-p c semaine)))
	lcours)))
    (when (not liste-possible)
      (afficher-attention
	(column
	  (row "Tous les séances de cours du " (string (libelle-jour jour)))
	  (row "de la semaine " (string semaine) " sont deja annulées."))))
    (when MODULE-RESEAU
      (setq liste-possible
	(sous-liste
	  (lambda (c)
	    (donnee-autorisee-reseau-p c))
	  liste-possible)))
    liste-possible))

;;; Fonction effectuant physiquement les annulations

(defun annulation-journee (lcours jour semaine)
  (mapc
    (lambda (c)
      (creer-annulation c semaine t))
    lcours))

;;; On implemente un premier UNDO dans Varuna
;;; Suppression des annulations automatiques (flag a t) d'une journee
;;; d'une semaine
;;; Utile si l'utilisateur s'est trompe lors d'une annulation d'une journee

(defun supprimer-annuler-journee (a)
  (let
    ((jour)
     (jtmp)
     (semaine)
     (lannuls))
    (if (not Liste-I-Annulation)
      (afficher-attention #Mv_aucune-annulation)
      (setq jtmp (demande-jour-ouvres))
      (setq jour (if jtmp (jour-libelle jtmp) ()))
      (when jour
	(setq semaine
	  (demande-semaine #Mv_pro-sem Semaine-Debut Semaine-Fin))
	(when semaine
	  (setq lannuls
	    (sous-liste
	      (lambda (ann)
		(semaine-jour-annul-p ann jour semaine))
	      Liste-I-Annulation))
	  (if (not lannuls)
	    (afficher-attention
	      (row
		"Aucune annulation générée le " (string (libelle-jour jour))
		" de la semaine " (string semaine) "."))
	    (when lannuls
	      (suppression-liste-annulations lannuls)
	      (setq DIRTY t)
	      (show-edt a)
	      (change-curseur a Varuna-Curseur-Fleche)	
	      (afficher-information "La suppression des annulations est terminée."))))))))

;;; <semaine-jour-cours-p>
;;; retourne t si le cours possede une seance la semaine n, le jour j

(defun semaine-jour-cours-p (cours jour semaine)
  (when (instant-trouve-i-cours cours)
    (let*
      ((instant-cours (instant-trouve-i-cours cours))
	(jour-cours (jour-instant instant-cours))
	(semaine-debut (semaine-instant instant-cours))
	(semaine-fin (add semaine-debut (nb-seances-i-cours cours))))
      (and
	(eq jour-cours jour)
	(precede-semaine-p semaine-debut semaine)
	(precede-semaine-strictement-p semaine semaine-fin)))))

;;; <semaine-jour-annul-p>
;;; retourne t si l'annulation a lieu le jour j de la semaine n
;;; et si le flag vaut t

(defun semaine-jour-annul-p (ann jour semaine)
  (and
    (flag-i-annulation ann)
    (eq (semaine-instant (instant-i-annulation ann)) semaine)
    (eq (jour-instant (instant-i-annulation ann)) jour)))
    

;;; Booleen retournant VRAI s'il existe au moins un cours modifiable
;;; instancie

(defun existe-cours-modifiable-instancie-p ()
  (any 'cours-modifiable-instancie-p liste-i-cours))

;;; Booleen retournant VRAI si le cours est une donnee modifiable (site),
;;; et instancie 

(defun cours-modifiable-instancie-p (cours)
  (and 
    (i-instanciep cours)
    (donnee-modifiable-p cours)))

;;; 2. Deplacement complet d'une journee

(defun deplacer-journee (a)
  (deplacement-journee t))

;; tousp = t si on veut deplacer tous les cours de la journee

(defun deplacement-journee (tousp)
  (let
    ((jour-orig) (semaine-orig)
     (jour-dest) (semaine-dest)
     (jtmp)
     (lcours) (lpossible))
    
    (if (not (existe-cours-modifiable-instancie-p))
      (afficher-attention #Mv_aucun-cours-place)
      (setq jtmp (demande-jour-ouvres))
      (setq jour-orig (if jtmp (jour-libelle jtmp) ()))
      (when jour-orig
	(setq semaine-orig
	  (demande-semaine #Mv_pro-sem Semaine-Debut Semaine-Fin))
	(when semaine-orig
	  (setq jtmp (demande-jour-ouvres))
	  (setq jour-dest (if jtmp (jour-libelle jtmp) ()))
	  (when jour-dest
	    (setq semaine-dest
	      (demande-semaine #Mv_pro-sem Semaine-Debut Semaine-Fin))
	    (when semaine-dest
	      (ifn (journees-differentes-p jour-orig semaine-orig jour-dest semaine-dest)
		(afficher-erreur "Les deux journées sélectionnées sont identiques.")
		(if tousp
		  (setq lcours
		    (sous-liste
		      (lambda (c)
			(semaine-jour-cours-p c jour-orig semaine-orig))
		      Liste-I-Cours))
		  (setq lcours
		    (demandes-filtree-i-cours #Mv_pro-cours () t
		      (lambda (c)
			(semaine-jour-cours-p c jour-orig semaine-orig)))))
		(if (not lcours)
		  (afficher-attention
		    (row
		      "Aucun cours n'est placé le " (string (libelle-jour jour-orig))
		      " de la semaine " (string semaine-orig) "."))
		  
		  (change-curseur a Varuna-Curseur-Sablier)		
		  (setq lpossible (annulation-journee-possible lcours jour-orig semaine-orig))
		  (change-curseur a Varuna-Curseur-Fleche)		
		  (when lpossible
		    (change-curseur a Varuna-Curseur-Sablier)				    
		    (annulation-journee lpossible jour-orig semaine-orig)
		    (ajout-journee lpossible jour-dest semaine-dest)
		    (setq DIRTY t)
		    (show-edt a)
		    (change-curseur a Varuna-Curseur-Fleche)	
		    (afficher-information "Le déplacement de la journée est terminé.")))))))))))
  
;;; Fonction effectuant les ajouts de seances

(defun ajout-journee (lcours jour semaine)
  (let
    ((lst-ajouts-possibles)
     (lst-ajouts-utilisateur)
     (horaire-cumule)
     (instant))
    (mapc				
      (lambda (c)
	(if (ajout-possible-p c jour semaine)
	  (newl lst-ajouts-possibles c)
	  (newl lst-ajouts-utilisateur c)))
      lcours)

    ;; On ajoute les seances de facon automatique (pour les possibles)
    
    (mapc
      (lambda (c)
	(setq horaire-cumule
	  (interne2cumule (horaire-instant (instant-trouve-i-cours c))))
	(setq instant
	  (instant-cumule2instant-interne
	    (cons-instant-cumule semaine
	      (cons-moment-cumule jour horaire-cumule))))
	(creer-ajout-seance c semaine instant (salle-trouvee-i-cours c) t))
      lst-ajouts-possibles)

    ;; On demande a l'utilisateur pour les autres
    (when lst-ajouts-utilisateur
      (afficher-attention
	(column
	  "Certains ajouts de séance ne respectent pas les contraintes posées."
	  "Vous allez pouvoir lever ou non certaines contraintes."))
      (mapc
	(lambda (c)
	  (ajouter-seance c jour semaine t))
	lst-ajouts-utilisateur))))
	

;;; Fonction retournant VRAI si les deux journees sont differentes

(defun journees-differentes-p (jour1 sem1 jour2 sem2)
  (not (and (eq jour1 jour2) (eq sem1 sem2))))


;;; Fonction qui retourne vrai si un ajout de seance du cours <cours>
;;; peut etre fait le jour <jour> de la semaine <sem>

(defun ajout-possible-p (cours jour sem)
  (let
    ((dispo)
     (duree (duree-i-cours cours))
     (instant)
     (moment)
     (horaire-cumule)
     (salle (salle-trouvee-i-cours cours))
     (possible ()))

    (setq horaire-cumule
      (interne2cumule (horaire-instant (instant-trouve-i-cours cours))))
    (setq instant
      (instant-cumule2instant-interne
	(cons-instant-cumule sem (cons-moment-cumule jour horaire-cumule))))
    (setq moment
      (moment-instant-cumule (instant-interne2instant-cumule instant)))
      
    (setq dispo (construire-dispo-ajout instant (duree-i-cours cours)))
    
    ;; On test les disponibilites

    (setq dispo (intersection-dispo dispo (reduire-dispo-cours cours)))
    (when (dispo-ajout-suffisante-p dispo sem jour duree cours)
      (mapc
	(lambda (pr)
	  (setq dispo (intersection-dispo dispo (dispo-i-prof pr))))
	(profs-i-cours cours))
      
      (when (dispo-ajout-suffisante-p dispo sem jour duree cours)

	;; On test les contraintes
	(setq dispo
	  (intersection-dispo dispo
	    (reduction-par-contraintes
	      dispo (semaine-instant instant) (duree-i-cours cours)
	      (nb-seances-i-cours cours) cours
	      '(poser-non-chevauchement poser-meme-semaine poser-jours-differents
		poser-meme-jour poser-simultaneite poser-simultaneite-fin-seance
		poser-meme-horaire poser-precedence poser-proximite-semaine)
	      ())))
	
	;; On verifie la salle
	(when
	  (and
	    (dispo-ajout-suffisante-p dispo sem jour duree cours)
	    (salle-disponible-p salle moment duree sem 1 cours))
	  (setq possible t))))

    possible))

;;; On implemente un premier UNDO dans Varuna
;;; Suppression des ajouts automatiques (flag a t) d'une journee
;;; d'une semaine
;;; Utile si l'utilisateur s'est trompe lors d'un deplacement de journee

(defun supprimer-ajouter-journee (a)
  (let
    ((jour)
     (jtmp)
     (semaine)
     (lajouts))
    (if (not Liste-I-Ajout)
      (afficher-attention #Mv_aucun-ajout)
      (setq jtmp (demande-jour-ouvres))
      (setq jour (if jtmp (jour-libelle jtmp) ()))
      (when jour
	(setq semaine
	  (demande-semaine #Mv_pro-sem Semaine-Debut Semaine-Fin))
	(when semaine
	  (setq lajouts
	    (sous-liste
	      (lambda (ajt)
		(semaine-jour-ajout-p ajt jour semaine))
	      Liste-I-Ajout))
	  (if (not lajouts)
	    (afficher-attention
	      (row
		"Aucun ajout généré le " (string (libelle-jour jour))
		" de la semaine " (string semaine) "."))
	    (when lajouts
	      (suppression-liste-ajouts lajouts)
	      (setq DIRTY t)
	      (show-edt a)
	      (change-curseur a Varuna-Curseur-Fleche)	
	      (afficher-information "La suppression des ajouts est terminée."))))))))

;;; <semaine-jour-ajout-p>
;;; retourne t si l'ajout a lieu le jour j de la semaine n
;;; et si le flag vaut t

(defun semaine-jour-ajout-p (ajt jour semaine)
  (and
    (flag-i-ajout ajt)
    (eq (semaine-i-ajout ajt) semaine)
    (eq (jour-instant (instant-i-ajout ajt)) jour)))


;;; 3. Deplacement d'un ensemble de cours

(defun deplacer-cours-journee (a)
  (deplacement-journee ()))
