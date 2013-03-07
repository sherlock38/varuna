;;; --------------------------------------------------------------------------
;;; - I-Manip                                                    VARUNA 1.2  -
;;; - Interface Graphique                                                    -
;;; - Manipulation des donnees (raz dispo, raz resas ...)                    -
;;; --------------------------------------------------------------------------
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; - Creation:  Juin 97                                                     -
;;; - Build 1.22.1 01.07.99 / RM                                             -
;;; --------------------------------------------------------------------------

;;; TRANSFORMATION DU TYPE DE FAMILLE

;;; Transformer une famille en famille standard

(defun transformer-famille2famille-standard (a)
  (let
    ((fam
      (demande-filtree-i-groupe
	#Mv_changer-type-fam
	#Mv_toutes-fam-standard
	()
	'famille-non-standard-p)))
    (when fam
      (change-curseur a Varuna-Curseur-Sablier)
      (famille2famille-standard fam)
      (change-curseur a Varuna-Curseur-Fleche)
      (afficher-information #Mv_traitement-termine))))
  
;; Attention : les familles alternances ne peuvent etre transformees
;;; en familles standards (gestion de la contrainte k-ecart-simultaneite)

(defun famille-non-standard-p (fam)
  (and
    (type-macro-i-groupe fam)
    (not (equal (type-macro-i-groupe fam) "STANDARD"))
    (not (equal (type-macro-i-groupe fam) "ALTERNANCE"))))
    
(defun famille2famille-standard (fam)
  (editeur-i-groupe fam 'fenetre-i-groupe-standard)
  (type-macro-i-groupe fam "STANDARD")
  (type-cours-i-groupe fam ())
  (type-salle-i-groupe fam ())
  (nb-seances-i-groupe fam ())
  (audience-i-groupe fam ())
  (durees-i-groupe fam ())
  (profs-i-groupe fam ())
  (nb-td-i-groupe fam ())
  (duree-i-groupe fam ()))

;;; SUPPRESSION D'UN ENSEMBLE D'ELEMENTS

;;; Elimination des resas d'un jeu de donnees

(defun supprimer-toutes-reservations (a)
  (ifn liste-i-resa
    (afficher-attention #Mv_aucune-resa)
    (change-curseur a Varuna-Curseur-Sablier)
    (suppression-liste-resas liste-i-resa)
    (aff-refresh a)
    (change-curseur a Varuna-Curseur-Fleche)    
    (afficher-information #Mv_suppressions-effectuees)))

(defun suppression-liste-resas (lresas)
  (mapc
    (lambda (resa)
      (if MODULE-RESEAU
	(if (eq (debut-modification SITE resa SUPPRESSION) ACCES-AUTORISE)
	  (supprimer-i-resa resa ())
	  (afficher-attention
	    (column
	      #Mv_sup-acces-refuse
	      (row #Mv_donnee (send 'objet2string resa) #Mv_modifiee)
	      #Mv_autre-utilisateur)))
	(supprimer-i-resa resa ())))
    lresas))

;;; Elimination des annulations de seances d'un jeu de donnees

(defun supprimer-toutes-annulations (a)
  (ifn liste-i-annulation
    (afficher-attention #Mv_aucune-annulation)
    (change-curseur a Varuna-Curseur-Sablier)
    (suppression-liste-annulations liste-i-annulation)
    (aff-refresh a)
    (change-curseur a Varuna-Curseur-Fleche)
    (afficher-information #Mv_suppressions-effectuees)))    

(defun suppression-liste-annulations (lannuls)
  (mapc
    (lambda (ann)
      (if MODULE-RESEAU
	(if (eq (debut-modification SITE ann SUPPRESSION) ACCES-AUTORISE)
	  (supprimer-i-annulation ann ())
	  (afficher-attention
	    (column
	      #Mv_sup-acces-refuse
	      (row #Mv_donnee (send 'objet2string ann) #Mv_modifiee)
	      #Mv_autre-utilisateur)))
	(supprimer-i-annulation ann ())))
    lannuls))

;;; Elimination des ajouts de seances d'un jeu de donnees

(defun supprimer-tous-ajouts (a)
  (ifn liste-i-ajout
    (afficher-attention #Mv_aucun-ajout)
    (change-curseur a Varuna-Curseur-Sablier)
    (suppression-liste-ajouts liste-i-ajout)
    (aff-refresh a)
    (change-curseur a Varuna-Curseur-Fleche)
    (afficher-information #Mv_suppressions-effectuees)))    

(defun suppression-liste-ajouts (lajouts)
  (mapc
    (lambda (ajt)
      (if MODULE-RESEAU
	(if (eq (debut-modification SITE ajt SUPPRESSION) ACCES-AUTORISE)
	  (supprimer-i-ajout ajt ())
	  (afficher-attention
	    (column
	      #Mv_sup-acces-refuse
	      (row #Mv_donnee (send 'objet2string ajt) #Mv_modifiee)
	      #Mv_autre-utilisateur)))
	(supprimer-i-ajout ajt ())))
    lajouts))

;;; REMISE A ZERO DES DISPONIBILITES

(defun raz-dispo-cours (a)
  (let
    ((lst))
    (setq lst
      (demandes-filtree-i-cours #Mv_pro-cours () t 'cours-non-genere-p))
    (when lst
      (change-curseur a Varuna-Curseur-Sablier)
      (initialiser-dispo-cours lst)
      (change-curseur a Varuna-Curseur-Fleche)
      (afficher-information #Mv_traitement-termine))))

(defun raz-dispo-profs (a)
  (let
    ((lst))
    (ifn liste-i-prof
      (afficher-erreur #Mv_aucun-prof)
      (setq lst (demandes-i-prof #Mv_pro-profs () t))
      (when lst
	(change-curseur a Varuna-Curseur-Sablier)	
	(initialiser-dispo-prof lst)
	(change-curseur a Varuna-Curseur-Fleche)
	(afficher-information #Mv_traitement-termine)))))

(defun raz-dispo-familles (a)
  (let
    ((lst
      (demandes-i-groupe #Mv_pro-groups () t)))
    (cond
      ((any 'groupes-i-groupe lst)  
	(setq lst (construire-liste-famille lst)))
      (t lst))
    (when lst
      (change-curseur a Varuna-Curseur-Sablier)
      (initialiser-dispo-familles lst)
      (change-curseur a Varuna-Curseur-Fleche)
      (afficher-information #Mv_traitement-termine))))
      
(defun construire-liste-famille (liste)
  (let
    ((sf (afficher-confirmation #Mv_manip-sous-fam)))
    (when sf
      (setq liste (construire-arbres-familles liste)))
    liste))

(defun construire-arbres-familles (lst-familles)
  (let
    ((lst lst-familles))
    (mapc
      (lambda (f)
        (setq lst (append (tous-les-sous-groupes f) lst)))
      lst-familles))
  (setq lst (supprimer-occurences-multiples lst))
  lst)
	
;;; Remise a zero de la dispo d'un ensemble de cours
;;; DISPO-ECOLE = omega

(defun initialiser-dispo-cours (lst-cours)
  (mapc
    (lambda (c)
      (if MODULE-RESEAU
	(if (eq (debut-modification SITE c MODIFICATION) ACCES-AUTORISE)
	  (dispo-i-cours c DISPO-ECOLE)	  
	  (afficher-attention
	    (column
	      #Mv_sup-acces-refuse
	      (row #Mv_donnee (send 'objet2string c) #Mv_modifiee)
	      #Mv_autre-utilisateur)))
	(dispo-i-cours c DISPO-ECOLE)))
    lst-cours))

;;; Remise a zero de la dispo d'un ensemble de profs
;;; DISPO-ECOLE = omega

(defun initialiser-dispo-prof (lst-profs)
  (mapc
    (lambda (p)
      (if MODULE-RESEAU
	(if (eq (debut-modification SITE p MODIFICATION) ACCES-AUTORISE)
	  (dispo-i-prof p DISPO-ECOLE)	  
	  (afficher-attention
	    (column
	      #Mv_sup-acces-refuse
	      (row #Mv_donnee (send 'objet2string p) #Mv_modifiee)
	      #Mv_autre-utilisateur)))
	(dispo-i-prof p DISPO-ECOLE)))
    lst-profs))

;;; Remise a zero de la dispo d'un ensemble de familles
;;; DISPO-ECOLE = omega
;;; S'il s'agit d'une famille generatrice, on reinitialise les
;;; dispos des cours generes

(defun initialiser-dispo-familles (lst-familles)
  (mapc
    (lambda (f)
      (if MODULE-RESEAU
	(if (eq (debut-modification SITE f MODIFICATION) ACCES-AUTORISE)
	  (progn
	    (dispo-i-groupe f DISPO-ECOLE)
	    (when (famille-generatrice-p f)
	      (mapc 'initialiser-dispo-cours (cours-i-groupe f))))
	  (afficher-attention
	    (column
	      #Mv_sup-acces-refuse
	      (row #Mv_donnee (send 'objet2string f) #Mv_modifiee)
	      #Mv_autre-utilisateur)))
	(progn
	  (dispo-i-groupe f DISPO-ECOLE)
	  (when (famille-generatrice-p f)
	    (mapc 'initialiser-dispo-cours (cours-i-groupe f))))))
    lst-familles))


;;; BRISER LIENS COURS / PROFS

(defun supprimer-liens-prof (a)
  (let
    ((lst))
    (ifn liste-i-prof
      (afficher-erreur #Mv_aucun-prof)
      (setq lst (demandes-i-prof #Mv_pro-profs () t))
      (when lst
	(change-curseur a Varuna-Curseur-Sablier)	
	(effectuer-suppression-liens-prof lst)
	(aff-refresh a)	
	(change-curseur a Varuna-Curseur-Fleche)
	(afficher-information #Mv_traitement-termine)))))

(defun effectuer-suppression-liens-prof (lst-prof)
  (if (eq (length lst-prof) (length liste-i-prof))
    (progn
      (supprimer-lien-prof-cours () t)
      (supprimer-lien-prof-familles () t)
      (supprimer-lien-prof-ajouts () t))
    (mapc
      (lambda (p)
	(supprimer-lien-prof-cours p ())
	(supprimer-lien-prof-familles p ())
	(supprimer-lien-prof-ajouts p ()))
      lst-prof))
  (mapc
    (lambda (p) (cours-i-prof p ()))
    liste-i-prof))
    
;;; Suppression des liens prof / cours

(defun supprimer-lien-prof-cours (prof tous)
  (if tous
    (mapc
      (lambda (c) (profs-i-cours c ()))
      liste-i-cours)
    (mapc
      (lambda (c) (profs-i-cours c (remq prof (profs-i-cours c))))
      (cours-i-prof prof))))

;;; Suppression des liens prof / famille

(defun supprimer-lien-prof-familles (prof tous)
  (mapc
    (lambda (f)
      (profs-i-groupe f
	(if tous () (remq prof (profs-i-groupe f)))))
    liste-i-groupe))

;;; Suppression des liens prof / ajout de seance

(defun supprimer-lien-prof-ajouts (prof tous)
  (mapc
    (lambda (ajt)
      (profs-i-ajout ajt
	(if tous () (remq prof (profs-i-ajout ajt)))))
    liste-i-ajout))
