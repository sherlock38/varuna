;;; --------------------------------------------------------------------------
;;; - I-resa.ll                                                VARUNA 1.2    -
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; - Interface Graphique                                                    -
;;; - Definition de l'application du module de reservation de VARUNA         -
;;; --------------------------------------------------------------------------
;;; - Creation:  Mars 95 (FD)                                                -
;;; - Build 1.22.1 01.07.99 / MO FD                                          -
;;; --------------------------------------------------------------------------


;;; recherche la liste des salles disponibles en fonction de la demande
;;; effectuee (action declenchee par le bouton <RECHERCHER> de la fenetre de
;;; reservation d'une salle.

(defun rechercher (b)
  (let*
    ((objet-resa)
     (editeur (component 'salles-disponibles b))
     (annulation ())
     (nb-semaines 1))

    (change-curseur b Varuna-Curseur-Sablier)
    (current-keyboard-focus-window ())
    (send 'set-line (component 'salle b) "")
    (ifn (i-valider-demande b)
      (send 'i-set-values editeur () () ())
      (i-sauver-demande b)
      (setq objet-resa 
	({application}:userdata (component 'appli b)))

      (if RESA-N-SEMAINES
	(setq nb-semaines
	  (demande-entier "Nombre de semaines " 1
	    (add1
	      (-
		(semaine-fin-i-ecole i-ecole)
		(semaine-i-resa objet-resa)))))
	(setq nb-semaines 1)
	(setq nb-seances-resa 1))
    
      (ifn nb-semaines
	(setq annulation t)
	(setq Salles-Disponibles
	  (salles-libres 
	    (semaine-i-resa objet-resa)
	    (jour-libelle (jour-i-resa objet-resa))
	    (duree-i-resa objet-resa)
	    (eval (salle-resa-i-resa objet-resa))
	    (type-salle-i-resa objet-resa) 
	    (audience-i-resa objet-resa)
	    (horaire1-i-resa objet-resa) 
	    (horaire2-i-resa objet-resa) 
	    (type-cours-i-resa objet-resa)
	    DISPO-ECOLE
	    nb-semaines))
	(setq nb-seances-resa nb-semaines))

      (if
	(or
	  annulation
	  *erreurs* 
	  (not Salles-Disponibles)
	  (not (car Salles-Disponibles)))
	(send 'i-set-values editeur () () ()))

      (when (not annulation)
	(and
	  (not (and *erreurs* (traiter-erreur)))
	  (not (and (not Salles-Disponibles)
	    (or (afficher-attention "Pas de salle disponible.") t)))
	  (not (and (not (car Salles-Disponibles))
	    (or (afficher-attention "Pas de créneau horaire possible.") t)))

	  (send 'i-set-values 
	    editeur
	    (mapcar 'libelle-i-salle (car Salles-Disponibles))
	    (mapcar 'string (cadr Salles-Disponibles))
	    (mapcar
	      (lambda (x)
		(if (consp (car x))
		  (intervalles-cumules2intervalles-naturels x)
		  (horaires-cumules2horaires-naturels x)))
                                   
	      (caddr Salles-Disponibles)))
	  (if (eq (length (car Salles-Disponibles)) 1)
	    (selection-automatique b)) )))
      (change-curseur b Varuna-Curseur-Fleche)))

;;; fonction permettant d'enregistrer tous les champs necessaires
;;; a la demande de reservation ont bien ete saisi
;;;
;;; rq mettre dans le {resa}

(defun i-sauver-demande (ed)
    (mapc
      (lambda (c)
	(and 
	  (send 'i-sauver (component c ed))))
      '(semaine jour duree 
	salle-resa type-salle audience
	horaire1 horaire2 type-cours)))

;;; verification du creneau horaire
;;; 
(defun verifier-creneau (ed)
  (let* ((heure-debut (send 'get-line (component 'horaire1 ed)))
         (hd)
         (heure-fin (send 'get-line (component 'horaire2 ed)))
         (hf)
         (duree (send 'get-line (component 'duree ed)))
         (d (naturel2cumule duree)))
    (if 
      (or (nullstrp heure-debut)
          (nullstrp heure-fin))
      t
      (and
          (setq hd (naturel2cumule heure-debut))
          (setq hf (naturel2cumule heure-fin))
          ;;; verifier que les le creneau n'est pas incompatible
          ;;; avec la dispo de l'ecole
          ;;;(dispo-ecole-ce-jour sem j)
          (ifn (ge hf hd)
            (and 
                (afficher-erreur "Début de créneau et fin de créneau incompatibles.")
                ())
            t)
          (ifn (ge hf (add hd d))
            (and 
                (afficher-erreur "Créneau horaire insuffisant.")
                ())
            t)))))
        

;;; verifie que tous les champs pouvant etre utilises pour la 
;;; recherche ont bien une valeur 'valide'
(defun i-valider-demande (ed)
  (let ((resultat t)
        (objet-resa ({application}:userdata 
                                  (component 'appli ed))))

    (mapc (lambda (c)
            (and resultat
                (setq resultat
                      (send 'i-valider (component c ed)))))
        '(duree semaine jour 
             salle-resa type-salle audience
             horaire1 horaire2 type-cours))
    (and resultat
        (or (not(nullstrp (send 'get-line 
                              (component 'semaine ed))))
            (and (alerte #Mv_i-resa-semaine) ()))
        (verifier-creneau ed)
        )))



;;; creation de l'application de demande de reservation
;;; FD - 31/05/95
;;; lancement de l'editeur de reservation
;;; rem FD 31/05/95 a quoi sert code ???
;;; AJT RM - 29/08/95 : traitement des donnees en reseau

(defun demande-reservation (s)
  (let
    ((code)
     (resa (nouveau-i-resa))
     (Resa-App))

    (commentaires-i-resa resa '(""))

    (when CONFIGURER-RESA
      (configurer-resa t)
      (setq CONFIGURER-RESA ()))
    
    ;; MOD - FD - 18/06/96 - prendre uniquement les salles de type autorise
    (ifn (liste-types-salle-autorises)		;; Liste-I-Salle
      ;; il n'est pas possible de faire de reservation car il n'existe pas
      ;; de salles 
      (afficher-erreur "Il faut définir des salles avant de pouvoir effectuer des réservations")
      (code-i-resa resa (gencode-i-resa))
      (when MODULE-RESEAU
	(debut-modification SITE resa CREATION))
      (setq code resa)
      ;;; changement de la couleur d'inhibition
      ;;; !!!FD a encapsuler ???
      ;;;(defllresource-named-color 'foreground system.inhibit "blue")
      ;;; !!!FD Resa-App  uniquement pour les tests 
      (setq Resa-App (fenetre-i-resa-standard resa))
      (add-and-select-application Resa-App)
      (send 'i-set-values (component 'salles-disponibles Resa-App) () ()())
      )))

;;; verification de l'heure de la reservation par rapport a une liste
;;; de creneaux horaires ou une liste d'horaires possibles

(defun horaire-resa-possible-p (heure duree liste)
  (cond
    ((zone-vide-p liste) ())
    (t
      (cond 
        ((consp (car liste)) ;;; on a un intervalle ex (480 . 1250)
         ;; rem MO (06/06/95) Gnarf: intervallep
         (or (inclusion-intervalle-p 
                   (cons heure (add heure duree)); rem MO (06/06/95) Gnarf: cons-intervalle
                   (premier-intervalle-zone liste))
             (horaire-resa-possible-p 
                 heure duree (reste-zone liste))))
        (t ;;; on a un horaire
           (or (eq heure (car liste))
               (horaire-resa-possible-p
                   heure duree (reste-zone liste))))))))
           


;;; effectue la reservation apres verification que les donnees
;;; saisies sont bien les bonnes

;;; MAJ (06/06/95) 
;;; a mon avis, prendre le car de (libelle-i-salles2code-i-salles salle)
;;; fait tout planter si la salle correspondant au car est occupee

(defun reserver (resa)
  (let* 
    ((copie)
     (confirm)
     (semaine)
     (i 1)
     (salle (salle-i-resa resa))
     ;;; deux salles peuvent avoir le meme nom mais des codes
     ;;; differents -> on prend le car
     (code-salle (car (libelle-i-salle2codes-i-salle salle)))
     (pos (position-element code-salle (car Salles-Disponibles)))
     (intervalles (nth pos (caddr Salles-Disponibles)))) ;;; intervalle ou heure
    ;;; verification de l'heure de debut
    (cond
      ((not (horaire-resa-possible-p
                 (heure-resa-i-resa resa)
                 (duree-i-resa resa)
                 intervalles))
       (afficher-attention "La réservation ne peut pas avoir lieu à cet horaire.")
       ())
      ((not (horaire-type-cours-p (heure-resa-i-resa resa)))
       (afficher-attention "Ce n'est pas un horaire de début de cours...")
       ())

      ;; AJT - 04/03/96 - MultiP
      ((not MODULE-SALLES)
	(afficher-attention #Mv_enregistrement-impossible)
	())
      
      (t
        (salle-i-resa resa 
            (car (libelle-i-salle2codes-i-salle salle)))
        (instant-i-resa resa 
            (cons-instant 
                (semaine-i-resa resa) 
                (cons-moment 
                    (jour-libelle (jour-i-resa resa))
                    (cumule2interne (heure-resa-i-resa resa)))))
	(commentaires-i-resa resa (list (string nb-seances-resa)))	  
	(setq confirm (confirm-resa resa))
        (when confirm
	  (setq copie (circopy-struct resa))
	  (newl Liste-I-Resa copie)
	  (setq semaine (semaine-i-resa resa))

	  (for (i 1 1 (sub1 nb-seances-resa) ())
	    (setq copie (copier-structure-varuna resa))
	    (code-i-resa copie (gencode-i-resa))
	    (set (code-i-resa copie) copie)
	    (when MODULE-RESEAU
	     (debut-modification SITE copie CREATION))
	    (instant-i-resa copie
	      (cons-instant 
		(+ semaine i) 
		(cons-moment 
		  (jour-libelle (jour-i-resa resa))
		  (cumule2interne (heure-resa-i-resa resa)))))
	    (index-i-resa copie ())
	    (commentaires-i-resa copie (list (string nb-seances-resa)))
	    (newl Liste-I-Resa copie))
	  
	  (setq Liste-Salles-Aff (list (salle-i-resa copie)))
	  (setq Liste-Groupes-Aff ())
	  (setq Liste-Profs-Aff ())
	  (if Week-Aff
	    (setq Week-Aff (semaine-i-resa copie)))
	  (update-grid-titles)
	  (show-edt Gen-App)
	  (when (eq confirm 0)
	    (imprimer-reservation resa))
	  Liste-I-Resa)))))

;;; selection automatique de la salle quand il y en a qu'un choix possible 
;;; dans la liste

(defun selection-automatique (b)
  (send 
      'set-line 
      (component 'salle b)
      (libelle-i-salle (caar Salles-Disponibles))))



;;; demande confirmation de la reservation a effectuer
(defun confirm-resa (resa)
  (let*
    ((instant (instant-i-resa resa))
      (duree (duree-i-resa resa))
      (horaire-debut (heure-resa-i-resa resa))
      (horaire-fin (add horaire-debut duree)))
    
;;; MOD RM - 15/09/95 : lancement possible de l'impression lors de la
;;; reservation
    (ok-annuler-imprimer
      (column 
	#Mv_resa-confirm
	(libelle-i-resa resa)
	(row 
	  #Mv_pourle
	  (string (libelle-jour (jour-instant instant)))
	  #Mv_de
	  (cumule2naturel 
	    (horaire-moment-cumule horaire-debut))
	  #Mv_a
	  (cumule2naturel 
	    (horaire-moment-cumule horaire-fin)))
	(if (> nb-seances-resa 1)
	  (row 
	    #Mv_delasem
	    (string (ecrire-numero-semaine (semaine-instant instant)))
	    #Mv_alasem
	    (string (ecrire-numero-semaine
	      (sub1 (+ nb-seances-resa (semaine-instant instant))))))
	  (row 
	    #Mv_delasem
	    (string (ecrire-numero-semaine (semaine-instant instant)))))
	(row #Mv_ensalle
	  (libelle-i-salle
	    (eval (salle-i-resa resa)))))
	)))

;;; MOD RM 16/08/95
;;; MOD RM 11/05/99 : resas sur plusieurs semaines

(defun supprimer-resa (b)
  (let
    ((resa (component 'la-resa b))
     (liste-resas ())
     (delete-all ()))

    (if
      (or
	(donnee-lecture-seule-p resa t)
	(not (resa-site-p resa)))
      (traiter-erreur)

      (mapc
	(lambda (r)
	  (when (resa-multiple-p resa r)
	    (setq liste-resas (cons r liste-resas))))
	liste-i-resa)

      (when liste-resas
	(when (afficher-confirmation "Supprimer toutes les séances ?")
	  (setq delete-all t)))

      (if delete-all
	(setq liste-resas (cons resa liste-resas))
	(setq liste-resas (cons resa ())))
      
      (mapc
	(lambda (r)

	  (if MODULE-RESEAU
	    (if (eq (debut-modification SITE r SUPPRESSION) ACCES-AUTORISE)
	      (progn
		(when (supprimer-i-resa r (if delete-all () t))
		  (setq Liste-Resas-Aff
		    (supprimer-occurence r Liste-Resas-Aff))
		  (remove-application (component 'appli b))))
	      (afficher-attention
		(column
		  #Mv_sup-acces-refuse
		  (row #Mv_donnee (send 'objet2string r) #Mv_modifiee)
		  #Mv_autre-utilisateur)))
	    (when (supprimer-i-resa r (if delete-all () t))
	      (setq Liste-Resas-Aff
		(supprimer-occurence r Liste-Resas-Aff))
	      (remove-application (component 'appli b)))))
	liste-resas)
      (show-edt b))))

;;; retourne la 1ere position d'un element d'une liste
;;; rq: on suppose que l'element appartient effectivement a la 
;;; liste
(defun position-element (valeur liste)
  (sub (length liste)
    (length (appartenir-liste2 valeur liste))))



(defun consulter-reservation (b)
  (nyi #Mv_outils-resa-consulter))

(defun afficher-reservation (b)
  (nyi #Mv_outils-resa-afficher))

;;; comparaison alphabetique par rapport au champ libelle de deux salles
(defun precede-alpha-p (obj1 obj2)
  (alphalessp 
      (libelle-i-salle obj2)
      (libelle-i-salle obj1)))


;;; CRE RM 05/05/99 : configuration des reservations

(defun configurer-resa (b)
  (let*
    ((b-one (radiobutton 0 0 150 15 #Mv_resa-simple))
     (b-multi (radiobutton 0 0 150 15 #Mv_resa-multiple))
     (choix (selectorapplication 0 0 150 30 (column b-one b-multi))))

    (send 'set-selection choix (if RESA-N-SEMAINES b-multi b-one))
    (when
      (afficher-confirmation
	(column #Mv_conf-resa "" choix ""))
      (setq RESA-N-SEMAINES
	(equal (send 'title (send 'get-selection choix)) #Mv_resa-multiple)))))


(defun resa-multiple-p (r1 r2)
  (and
    (not (eq r1 r2))
    (not (equal (car (commentaires-i-resa r1)) "1"))
    (not (equal (car (commentaires-i-resa r2)) "1"))    
    (equal (libelle-i-resa r1) (libelle-i-resa r2))
    (eq (salle-i-resa r1) (salle-i-resa r2))
    (eq (duree-i-resa r1) (duree-i-resa r2))
    (eq (jour-instant (instant-i-resa r1))
        (jour-instant (instant-i-resa r2)))
    (eq (horaire-instant (instant-i-resa r1))
        (horaire-instant (instant-i-resa r2)))))
