;;; --------------------------------------------------------------------------
;;; - I-Sortie.ll                                               VARUNA 1.2   -
;;; - Interface Graphique                                                    -
;;; - Sorties textes dans un terminal                                        -
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; --------------------------------------------------------------------------
;;; - Creation:  Mars 93                                                     -
;;; - Mise a jour pour version 1.07: Decembre 93                             -
;;; - Build 1.22.1 01.07.99 / JH MO                                          -
;;; - Mise a jour pour version 1.075:07.11.94                                -
;;; - Ajout de la macro affichage et reecriture des differentes routines     -
;;; --------------------------------------------------------------------------

;;; MOD RM - 13/02/97 : Remise en forme !!!

;;; Execute l'expression affiche dans l'appli tty lorsque
;;; condition est verifie

(dmd affichage (condition . affiche)
  `(let ((rm (rmargin)))
    (when ,condition
      (rmargin 1024)
      (add-and-select-application Tty-App)
      (with-medite-output (component 'tty Tty-App) .,affiche)
      (rmargin rm))))

;;; Affichage de l'ETABLISSEMENT
;;; Tous les cours de l'ecole (peut etre long, d'ou la confirm).

(defun aff-ecole (a)
  (change-curseur a Varuna-Curseur-Sablier)
  (let*
    ((ok
      (afficher-confirmation (chaine2image #Mv_tous-les-cours)))
    (comp (and ok (prompt-oui-non #Mv_avec-commentaires))))
    (affichage ok
      (print #Mv_ecole libelle-ecole)
      (terpri)
      (mapc
	(lambda (c)
	  (imprimer-cours c t t ())
	  (when comp (imprimer-liste-chaines (commentaires-i-cours c)))
	  (terpri))
	Liste-I-Cours)))
  (change-curseur a Varuna-Curseur-Fleche))

;;; Affichage de COURS

;;; COURS / TOUS

(defun aff-cours (a)
  (change-curseur a Varuna-Curseur-Sablier)
  (let
    ((cours (demandes-i-cours #Mv_pro-cours () t))
     (comp))
    (if (not Liste-I-Cours)
      (afficher-attention #Mv_aucun-cours)
      (setq comp (and cours (prompt-oui-non #Mv_avec-commentaires)))
      (mapc
	(lambda (c)
	  (affichage c
	    (imprimer-cours c t t ())
	    (when comp (imprimer-liste-chaines (commentaires-i-cours c)))
	    (terpri)))
	cours)))
  (change-curseur a Varuna-Curseur-Fleche))

;;; COURS / COURS SANS SALLE
  
(defun aff-cours-sans-salle (a)
  (change-curseur a Varuna-Curseur-Sablier)
  (let
    ((cours
      (demandes-filtree-i-cours
	#Mv_pro-cours () t 'i-instancie-sans-salle-p)))
    (if (not cours)
      (afficher-attention #Mv_aucun-cours-sans-salle)
      (mapc
	(lambda (c)
	  (affichage c (imprimer-cours c t () ()) (terpri)))
	cours)))
  (change-curseur a Varuna-Curseur-Fleche))

;;; CRE RM 24/09/98
;;; liste des cours dont les souhaits de salle non pas ete respectes

(defun salle-souhaitee-non-affectee-p (cours)
  (and
    (salle-trouvee-i-cours cours)
    (salle-figee-i-cours cours)
    (not
      (eq
	(salle-trouvee-i-cours cours)
	(eval (salle-figee-i-cours cours))))))

(defun aff-cours-souhaits-non-respectes (a)
  (change-curseur a Varuna-Curseur-Sablier)
  (let
    ((cours
      (demandes-filtree-i-cours
	#Mv_pro-cours () t 'salle-souhaitee-non-affectee-p)))
    (if (not cours)
      (afficher-attention #Mv_souhaits-salle-ok)
      (mapc
	(lambda (c)
	  (affichage c (imprimer-cours c t () t) (terpri)))
	cours)))
  (change-curseur a Varuna-Curseur-Fleche))

;;; COURS / COURS SANS PROF

(defun aff-cours-sans-prof (a)
  (change-curseur a Varuna-Curseur-Sablier)
  (let
    ((cours
      (demandes-filtree-i-cours
	#Mv_pro-cours () t 'i-cours-sans-prof-p)))
    (ifn cours
      (afficher-attention #Mv_aucun-cours-sans-prof)
      (mapc
	(lambda (c)
	  (affichage c (imprimer-cours c () t ()) (terpri)))
	cours)))
  (change-curseur a Varuna-Curseur-Fleche))

;;; COURS / COURS D'UN TYPE DONNE
  
(defun aff-cours-type (a)
  (change-curseur a Varuna-Curseur-Sablier)  
  (let*
    ((type-choisi
      (demande-filtree-i-type-cours
	#Mv_choix-type-cours () () (lambda (x) t)))
    (cours
      (sous-liste
	 (lambda (c type)
	   (eq (code-i-type-cours type) (type-i-cours c)))
	 liste-i-cours type-choisi)))
    (ifn cours
      (afficher-attention #Mv_aucun-cours-type)
      (mapc
	(lambda (c)
	  (affichage c (imprimer-cours c t t ()) (terpri)))
	cours)))
  (change-curseur a Varuna-Curseur-Fleche))

;;; Fonction d'affichage d'un cours

(defun imprimer-cours (cours profp sallep souhaitp)
  (let*
    ((instant (instant-trouve-i-cours cours))
     (duree (duree-i-cours cours)) 
     (horaire-debut (when instant (interne2cumule (horaire-instant instant))))
     (horaire-fin (when instant (add duree horaire-debut))) 
     (nbsem (nb-seances-i-cours cours)))
    (print
      #Mv_i-cours (libelle-i-cours cours)
      " (" (code-i-cours cours) ")")
    (if (not instant)
      (print "Non instancié.")
      (print "Le " (libelle-jour (jour-instant instant))
	" de " (cumule2naturel horaire-debut) " à "
	(cumule2naturel horaire-fin))
      (print
	"De la semaine " 
	(ecrire-numero-semaine (semaine-instant instant))
	" à la semaine "
	(ecrire-numero-semaine (sub1 (add nbsem (semaine-instant instant)))))
      (when profp
	(prin "Professeur(s) : ")
	(mapc
	  (lambda (p)
	    (prin (catenate (civilite-i-prof p) " " (libelle-i-prof p) " ")))
	  (profs-i-cours cours))
	(terpri))
      (when sallep
	(if (salle-trouvee-i-cours cours)
	  (print "En salle : " (libelle-i-salle (salle-trouvee-i-cours cours)))
	  (print "Aucune salle d'affectée")))
      (when souhaitp
	(print "Salle souhaitée : "
	  (libelle-i-salle (salle-figee-i-cours cours)))
	(print "Salle affectée : "
	  (libelle-i-salle (salle-trouvee-i-cours cours)))))))


;;; Affichage de PROFESSEURS

;;; PROFESSEURS / TOUS

(defun aff-profs (a)
  (change-curseur a Varuna-Curseur-Sablier)      
  (let
    ((profs (demandes-i-prof #Mv_pro-profs () t))
     (comp))
    (if (not Liste-I-Prof)
      (afficher-attention #Mv_aucun-prof)
      (setq comp (and profs (prompt-oui-non #Mv_avec-commentaires)))
      (mapc
	(lambda (p)
	  (affichage p
	    (print #Mv_i-prof (libelle-i-prof p) " (" (code-i-prof p) ")")
	    (mapc
	      (lambda (c)
		(print "   " (libelle-i-cours c) " (" (code-i-cours c) ")"))
	      (cours-i-prof p))
	    (when comp (imprimer-liste-chaines (commentaires-i-prof p)))
	    (terpri)))
	profs)))
  (change-curseur a Varuna-Curseur-Fleche))

;;; PROFESSEURS / PROFESSEURS SANS COURS

(defun aff-profs-sans-cours (a)
  (change-curseur a Varuna-Curseur-Sablier)  
  (let*
    ((profs (demandes-filtree-i-prof #Mv_pro-profs () t 'i-prof-sans-cours-p))
     (comp (and profs (prompt-oui-non #Mv_avec-commentaires))))
    (if (not profs)
      (afficher-attention #Mv_aucun-prof-sans-cours)
      (mapc
	(lambda (p)
	  (affichage p
	    (print #Mv_i-prof (libelle-i-prof p) " (" (code-i-prof p) ")")
	    (when comp (imprimer-liste-chaines (commentaires-i-prof p)))
	    (terpri)))
	profs)))
  (change-curseur a Varuna-Curseur-Fleche))

;;; Affichage de SALLES

;;; SALLES / TOUS

(defun aff-salles (a)
  (change-curseur a Varuna-Curseur-Sablier)
  (let
    ((salles (demandes-i-salle #Mv_pro-sals () t))
     (comp))
    (if (not Liste-I-Salles)
      (afficher-attention #Mv_aucune-salle)
      (setq comp (and salles (prompt-oui-non #Mv_avec-commentaires)))
      (mapc
	(lambda (s)
	  (affichage s
	    (print #Mv_i-salle (libelle-i-salle s) " (" (code-i-salle s) ")")
	    (print "Capacité : " (capacite-i-salle s) " places.")
	    (when comp (imprimer-liste-chaines (commentaires-i-salle s)))
	    (terpri)))
	salles)))
  (change-curseur a Varuna-Curseur-Fleche))

;;; SALLES / SALLE D'UN TYPE DONNE

(defun aff-salles-type (a)
  (change-curseur a Varuna-Curseur-Sablier)  
  (let*
    ((type-choisi
      (symbol ()
	(demande-type
	  #Mv_choix-type-salle (liste-type-salle) 'alphalessp()())))
    (comp (and type-choisi (prompt-oui-non #Mv_avec-commentaires))))
    (mapc
      (lambda (s)
	(affichage (and s (eq type-choisi (type-i-salle s)))
	  (print  #Mv_i-salle  (libelle-i-salle s) " (" (code-i-salle s) ")")
	  (print "Capacité : " (capacite-i-salle s) " places.")
	  (when comp (imprimer-liste-chaines (commentaires-i-salle s)))
	  (terpri)))
      liste-i-salle))
  (change-curseur a Varuna-Curseur-Fleche))
      

;;; Affichage de FAMILLES

;;; On n'affiche pas l'ensemble des sous familles sinon multiplicite de
;;; l'info lorsque l'on choisit tous !

(defun aff-groupes (a)
  (change-curseur a Varuna-Curseur-Sablier)
  (let
    ((groupes (demandes-i-groupe #Mv_pro-groups () t))
     (comp))
    (if (not Liste-I-Groupe)
      (afficher-attention #Mv_no-groups)
      (setq comp (and groupes (prompt-oui-non #Mv_avec-commentaires)))
      (mapc
	(lambda (g) (aff-sub-groups g comp))
	groupes)))
  (change-curseur a Varuna-Curseur-Fleche))

;;; Affiche tous les cours d'un groupe et de ses sous-groupes

(defun aff-sub-groups (g comp)
  (affichage g
    (print #Mv_i-groupe (libelle-i-groupe g) " (" (code-i-groupe g) ")")
    (terpri)
    (when (cours-i-groupe g)
      (mapc
	(lambda (c)
	  (imprimer-cours c t t ())
	  (terpri))
	(cours-i-groupe g)))
    (when (groupes-i-groupe g)
	(print "Sous-familles : ")
	(mapc
	  (lambda (g)
	    (print "   " (libelle-i-groupe g) " (" (code-i-groupe g) ")"))
	  (groupes-i-groupe g))
	(terpri))
    (when comp (imprimer-liste-chaines (commentaires-i-groupe g)) (terpri))))

;;; Affichage de RESERVATIONS

(defun aff-resas (a)
  (change-curseur a Varuna-Curseur-Sablier)  
  (let
    ((resas (demandes-i-resa #Mv_pro-resa () t)))
    (ifn Liste-I-Resa
      (afficher-attention #Mv_aucune-resa)
      (mapc
	(lambda (r)
	  (affichage r
	    (imprimer-resa r)
	    (terpri)))
	resas)))
  (change-curseur a Varuna-Curseur-Fleche))

;;; Fonction d'affichage d'une reservation de salle

(defun imprimer-resa (resa)
  (let*
    ((instant (instant-i-resa resa))
     (duree (or (duree-i-resa resa) 0))
     (horaire-debut (interne2cumule (horaire-instant instant)))
     (horaire-fin (add horaire-debut duree)))
    (print
      #Mv_i-resa (libelle-i-resa resa)
      " (" (code-i-resa resa) ")")
    (print "Le " (libelle-jour (jour-instant instant))
      " de " (cumule2naturel horaire-debut)
      " à " (cumule2naturel horaire-fin))
    (print "La semaine " (ecrire-numero-semaine (semaine-instant instant)))
    (print "En salle : " (libelle-i-salle (eval (salle-i-resa resa))))))


;;; Affichage des ANNULATIONS DE SEANCES

(defun aff-annulations (a)
  (change-curseur a Varuna-Curseur-Sablier)
  (let
    ((liste-ann (demandes-i-annulation #Mv_pro-annulation () t))
     (comp))
    (ifn liste-i-annulation
      (afficher-attention #Mv_aucune-annulation)
      (setq comp (and liste-ann (prompt-oui-non #Mv_avec-commentaires)))
      (mapc
	(lambda (ann)
	  (affichage ann
	    (imprimer-annulation-seance ann)
	    (when comp (imprimer-liste-chaines (commentaires-i-annulation ann)))
	    (terpri)))
	liste-ann)))
  (change-curseur a Varuna-Curseur-Fleche))

;;; Affiche une annulation de seance

(defun imprimer-annulation-seance (ann)
  (let*
    ((cours (cours-i-annulation ann))
     (instant (instant-trouve-i-cours cours))
     (duree (duree-i-cours cours)) 
     (horaire-debut
       (when instant (interne2cumule (horaire-instant instant))))
     (horaire-fin (when instant (add duree horaire-debut))) 
     (nbsem (nb-seances-i-cours cours)))
    (print (libelle-i-annulation ann) ' "(" (code-i-annulation ann) ")")
    (print "Pour le cours : "
      (libelle-i-cours cours) " (" (code-i-cours cours) ")")
    (print "Le " (libelle-jour (jour-instant instant))
      " de la semaine "
      (ecrire-numero-semaine (string2number (semaine-i-annulation ann)))
      ", de " (cumule2naturel horaire-debut)
      " à " (cumule2naturel horaire-fin))
    (when (profs-i-cours cours)
      (prin "Professeur(s) : ")
      (mapc
	(lambda (p) (prin (catenate (libelle-i-prof p) " ")))
	(profs-i-cours cours))
      (terpri))
    (if (salle-trouvee-i-cours cours)
      (print "En salle : " (libelle-i-salle (salle-trouvee-i-cours cours)))
      (print "Aucune salle d'affectée"))))


;;; Affichage des AJOUTS DE SEANCES

(defun aff-ajouts (a)
  (change-curseur a Varuna-Curseur-Sablier)
  (let
    ((liste-ajouts (demandes-i-ajout #Mv_pro-ajout () t)))
    (ifn liste-i-ajout
      (afficher-attention #Mv_aucun-ajout)
      (mapc
	(lambda (ajt)
	  (affichage ajt
	    (imprimer-ajouts-seance ajt)
	    (terpri)))
	liste-ajouts)))
  (change-curseur a Varuna-Curseur-Fleche))

;;; Affiche un ajout de seance

(defun imprimer-ajouts-seance (ajout)
  (let*
    ((cours (cours-rattrape-i-ajout ajout))
     (instant (instant-trouve-i-cours cours))
     (duree (duree-i-cours cours)) 
     (horaire-debut
       (when instant (interne2cumule (horaire-instant instant))))
     (horaire-fin (when instant (add duree horaire-debut))) 
     (nbsem (nb-seances-i-cours cours)))
    (print (libelle-i-ajout ajout) " (" (code-i-ajout ajout) ")")
    (print "Pour le cours : "
      (libelle-i-cours cours) " (" (code-i-cours cours) ")")
    (print
      "Le " (libelle-jour (jour-instant instant))
      " de la semaine "
      (ecrire-numero-semaine (semaine-instant (instant-i-ajout ajout)))
      ", de " (cumule2naturel horaire-debut)
      " à " (cumule2naturel horaire-fin))
    (when (profs-i-ajout ajout)
      (prin "Professeur(s) : ")
      (mapc
	(lambda (p) (prin (catenate (libelle-i-prof p) " ")))
	(profs-i-ajout ajout))
      (terpri))
    (if (salle-i-ajout ajout)
      (print "En salle : " (libelle-i-salle (salle-i-ajout ajout)))
      (print "Aucune salle d'affectée"))))

