;; ---------------------------------------------------------------------------
;;; - I-Stats.ll                                                VARUNA 1.2   -
;;; - Statistiques Varuna (niveau 1)                                         -
;;; -                                                                        -
;;; --------------------------------------------------------------------------
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; - Creation: Decembre 1996                                                -
;;; - Derniere mise a jour pour version 1.13:: Decembre 1996                 -
;;; - Build 1.22.1 01.07.99 / RM                                             -
;;; --------------------------------------------------------------------------

(defun afficher-statistiques (a)
  (let
    ((rm (rmargin)))
    (rmargin 1024)
    (change-curseur a Varuna-Curseur-Sablier) 
    (with-medite-output (component 'tty Tty-App) (imprimer-stats))
    (change-curseur a Varuna-Curseur-Fleche)     
    (add-and-select-application Tty-App)
    (rmargin rm)))

(defun imprimer-stats ()
  (let
    ((nb-cours (calculer-nb-cours))
     (nb-cours-sans-salle (calculer-nb-cours-sans-salle))
     (nb-cours-non-instancies (calculer-nb-cours-non-instancies))
     (nb-cours-sans-profs (calculer-nb-cours-sans-profs))
     (nb-cours-par-prof (calculer-nb-cours-par-prof))
     (volume-donnees (calculer-volume-donnees)))

    (print "Statistiques du jeu de données : " (current-directory))
    (terpri)
    (print "Nombre de cours                   : " nb-cours)
    (print "Nombre de familles                : " (calculer-nb-familles))
    (print "Nombre de salles                  : " (calculer-nb-salles))
    (print "Nombre de professeurs             : " (calculer-nb-profs))
    (when MODULE-RESA
      (print "Nombre de réservations de salles  : " (calculer-nb-resas)))
    (when MODULE-RACO
      (print "Nombre d'annulations de séances   : " (calculer-nb-annulations))
      (print "Nombre d'ajouts de séances        : " (calculer-nb-ajouts)))
    (terpri)
    (print "Nombre de cours non instanciés    : " nb-cours-non-instancies
      " (" (pourcentage nb-cours-non-instancies nb-cours) "%)")
    (print "Nombre de cours sans salle        : " nb-cours-sans-salle
      " (" (pourcentage nb-cours-sans-salle nb-cours) "%)")
    (print "Nombre de cours sans professeur   : " nb-cours-sans-profs
      " (" (pourcentage nb-cours-sans-profs nb-cours) "%)")
    (terpri)
    (when nb-cours-par-prof
      (print "Nombre de cours par professeur    : " nb-cours-par-prof))
    (terpri)
    (print "Nombre de contraintes (saisies)   : "
      (calculer-nb-contraintes-saisies))
    (when compter-contraintes-p
      (print "Nombre de contraintes (posées)    : "
	(round compteur-contraintes-posees 1)))
    (terpri)
    (print "Nombre de types de cours          : " (calculer-nb-types-cours))
    (mapc
      (lambda (type)
        (print "   " (libelle-i-type-cours type) " : "
          (calculer-nb-cours-type-cours type) " cours"))
      (reverse (liste-type-cours)))
    (terpri)
    (print "Nombre de type de salles          : " (calculer-nb-types-salles))
    (mapc
      (lambda (type)
        (print "   " type " : "
          (calculer-nb-salles-type-salle type) " salles - "
          (calculer-nb-cours-type-salle type) " cours"))
      (reverse (liste-type-salle)))
    (terpri)
    (ifn volume-donnees
      (print #Mv_acces-edt-interdit)
      (print "Volume des données                : " volume-donnees " Koctets")
      (when dirty
	(print "(Données modifiées depuis le dernier enregistrement)")))))

;;; Fonctions de bas niveau : calculs directs

;; Retourne le nombre de cours definis

(defun calculer-nb-cours ()
  (length liste-i-cours))

;; Retourne le nombre de familles definies

(defun calculer-nb-familles ()
  (length liste-i-groupe))

;; Retourne le nombre de salles definies

(defun calculer-nb-salles ()
  (length liste-i-salle))

;; Retourne le nombre de professeurs definis

(defun calculer-nb-profs ()
  (length liste-i-prof))

;; Retourne le nombre de reservations de salles definies

(defun calculer-nb-resas ()
  (length liste-i-resa))

;; Retourne le nombre d'annulations de seances definies

(defun calculer-nb-annulations ()
  (length liste-i-annulation))

;; Retourne le nombre d'ajouts de seances definis

(defun calculer-nb-ajouts ()
  (length liste-i-ajout))

;; Retourne le nombre de cours sans salle definis

(defun calculer-nb-cours-sans-salle ()
  (let
    ((count 0))
    (mapc
      (lambda (c)
        (when (not (salle-trouvee-i-cours c))
          (incr count)))
      liste-i-cours)
    count))

;; Retourne le nombre de cours non instancies

(defun calculer-nb-cours-non-instancies ()
  (let
    ((count 0))
    (mapc
      (lambda (c)
        (when (not (instant-trouve-i-cours c))
          (incr count)))
      liste-i-cours)
    count))

;; Retourne le nombre de cours sans profs

(defun calculer-nb-cours-sans-profs ()
  (let
    ((count 0))
    (mapc
      (lambda (c)
        (when (not (profs-i-cours c))
          (incr count)))
      liste-i-cours)
    count))

;; Retourne le nombre de cours ayant au moins un prof

(defun calculer-nb-cours-avec-profs ()
  (- (calculer-nb-cours) (calculer-nb-cours-sans-profs)))

;; Retourne le nombre de profs ayant au moins un cours

(defun calculer-nb-profs-avec-cours ()
  (let
    ((count 0))
    (mapc
      (lambda (p)
        (when (cours-i-prof p)
          (incr count)))
      liste-i-prof)
    count))

;; Retourne le nb de cours par prof

(defun calculer-nb-cours-par-prof ()
  (let
    ((nb-profs-avec-cours (calculer-nb-profs-avec-cours)))
    (if (not (eq nb-profs-avec-cours 0))
      (/
	(round
	  (* 100
	    (/ (calculer-nb-cours-avec-profs) nb-profs-avec-cours))
	  1)
	100)
      ())))

;; Retourne le nombre de contraintes (implicites et explicites)
;; saisies dans le jeu de donnees

(defun calculer-nb-contraintes-saisies ()
  (let
    ((count 0))
    (mapc
      (lambda (f)
        (when (contraintes-i-groupe f)
          (setq count (add count (length (contraintes-i-groupe f))))))
      liste-i-groupe)
    (add count (length liste-contraintes))))

;; Retourne le pourcentage d'un sous-ensemble par rapport a la totalite

(defun pourcentage (sous-ensemble totalite)
  (if (not (eq totalite 0))
    (round (/ (* sous-ensemble 100) totalite) 1)
    0))

;; Retourne le nombre de types de cours

(defun calculer-nb-types-cours ()
  (length (liste-type-cours)))

;; Retourne le nombre de types de salles

(defun calculer-nb-types-salles ()
  (length (liste-type-salle)))

;; Retourne le nombre de cours d'un type de cours donne

(defun calculer-nb-cours-type-cours (type)
  (let
    ((count 0))
    (mapc
      (lambda (c)
        (when (eq type (type-i-cours c))
          (incr count)))
      liste-i-cours)
    count))

;; Retourne le nombre de salles d'un type de salle donne

(defun calculer-nb-salles-type-salle (type)
  (let
    ((count 0))
    (mapc
      (lambda (s)
        (when (eq (string2symbol type) (type-i-salle s))
          (incr count)))
      liste-i-salle)
    count))

;; Retourne le nombre de cours d'un type de salle donne

(defun calculer-nb-cours-type-salle (type)
  (let
    ((count 0))
    (mapc
      (lambda (c)
        (when (eq (string2symbol type) (type-salle-i-cours c))
          (incr count)))
      liste-i-cours)
    count))

;; Retourne la somme des tailles des fichiers edt

(defun calculer-volume-donnees ()
  (let
    ((count 0.))
  (if (and MODULE-RESEAU (not (interdire-acces EDT-SEM)))
    ()
    (setq count
      (+
	(calculer-taille-fichier FICHIER-COURS)
	(calculer-taille-fichier FICHIER-PROFS)
	(calculer-taille-fichier FICHIER-SALLES)
	(calculer-taille-fichier FICHIER-GROUPES)
	(calculer-taille-fichier FICHIER-CONTRAINTES)
	(calculer-taille-fichier FICHIER-ECOLE)
	(ifn MODULE-RESA 0. (calculer-taille-fichier FICHIER-RESA))
	(ifn MODULE-RACO 0.
	  (+ (calculer-taille-fichier FICHIER-ANNUL)
	    (calculer-taille-fichier FICHIER-AJOUT)))
	(ifn MODULE-GESTSITE 0. (calculer-taille-fichier FICHIER-SITES))))
    (when MODULE-RESEAU (autoriser-acces EDT-SEM))
    (ceiling (/ count 1024)))))

(defun calculer-taille-fichier (fichier)
  (let
    ((taille (file-size fichier)))
    (if taille
      taille
      0.)))
	  