;;; ------------------------------------------------------------------------
;;; - I-Reduc.ll                                              VARUNA 1.2   -
;;; - Copyright Abstract, France 1993-2006                                 -
;;; - under licence MIT Expat since 2006                                   -
;;; - Interface                                                            -
;;; - Module d'utilitaires pour la reduire les dispo en fonction des       -
;;; - differents types de contraintes qui peuvent peser sur un cours       -
;;; - Aida & Lisp                                                          -
;;; ------------------------------------------------------------------------
;;; - Creation:  Aout 1995                                    FD           -
;;; - Build 1.22.1 01.07.99 / FD                                           -
;;; ------------------------------------------------------------------------

;;; Remise a jour de tout ce bout de code pour prendre en compte toutes
;;; les contraintes et traiter les ajouts de seances et les deplacements
;;; de cours. Prise en compte des contraintes explicites et des ajouts
;;; de seances deja definis !

;;; Attention : detailp = t pour un deplacement de cours (on veut tous
;;; les conflits)
;;; detailp = () pour un ajout de seance (on reduit la dispo pour proposer
;;; des creneaux possibles)


;;; <semaine-cours-p>
;;; arg: semaine
;;;      cours
;;; indique si un cours a lieu pendant une semaine determinnee
;;; rq: il est preferable de tester si <semaine-debut> precede <semaine>
;;;     et si <semaine> precede <semaine-fin> => evite de creer un intervalle

(defun semaine-cours-p (semaine cours)
  (let*
    ((instant-cours (instant-trouve-i-cours cours))
     (semaine-debut (semaine-instant instant-cours))
     (semaine-fin (add semaine-debut (nb-seances-i-cours cours))))

    (and
      (precede-semaine-p semaine-debut semaine)
      (precede-semaine-strictement-p semaine semaine-fin))))


;;; <retire-moment-cours>
;;; arg: dispo
;;;      cours
;;; retire a une dispo les moments pendant lesquels le cours a lieu

(defun retire-moments-cours (dispo cours)
  (let*
    ((dispo-semaines (trouver-dispo-semaine dispo))
     (instant-cours (instant-trouve-i-cours cours))
     (jour (jour-instant instant-cours))
     (horaire-debut (interne2cumule(horaire-instant instant-cours)))
     (horaire-fin (add horaire-debut (duree-i-cours cours))))

    (cons-dispo
      (difference-zone
	(trouver-dispo-moment dispo)
	(creer-zone
	  (creer-intervalle
	    (cons-moment-cumule jour horaire-debut)
	    (cons-moment-cumule jour horaire-fin))))
      dispo-semaines)))

;;; <retire-semaines-cours>
;;; arg: dispo
;;;      cours
;;; retire a une dispo les semaines pendant lesquelles le cours a lieu

(defun retire-semaines-cours (dispo cours)
  (let*
    ((instant-cours (instant-trouve-i-cours cours))
     (semaine-debut (semaine-instant instant-cours))
     (semaine-fin (add semaine-debut (nb-seances-i-cours cours)))
     (intervalle-semaines-cours
       (creer-zone (creer-intervalle semaine-debut semaine-fin))))

    (cons-dispo
      (trouver-dispo-moment dispo)
      (difference-zone
	(trouver-dispo-semaine dispo)
	intervalle-semaines-cours))))


;;; <retire-periode-cours>
;;; supprime de la dispo la periode pendant laquelle le cours a lieu

(defun retire-periode-cours (dispo cours semaine detailp)
  (if detailp
    (let
      ((dispo-commune
	(intersection-dispo
	  dispo
	  (construire-dispo cours (instant-trouve-i-cours cours)))))
      (when (not (dispo-vide-p dispo-commune))
	(setq dispo
	  (retire-moments-cours
	    (retire-semaines-cours dispo cours)
	    cours))))
    (when (semaine-cours-p semaine cours) 
      (setq dispo (retire-moments-cours dispo cours))))
  dispo)
  
;;; <fixe-dispo-avec-cours-debut>
;;; reduit la dispo a la periode pendant laquelle le cours a lieu
;;; lors d'une simultaneite de debut de seance

(defun fixe-dispo-avec-cours-debut (dispo duree cours)
  (let*
    ((instant-cours (instant-trouve-i-cours cours))
     (jour (jour-instant instant-cours))
     (duree-cours (duree-i-cours cours))
     (horaire-debut (interne2cumule(horaire-instant instant-cours)))
     (horaire-fin (add horaire-debut (duree-i-cours cours)))
     (semaine-debut (semaine-instant instant-cours))
     (semaine-fin (add semaine-debut (nb-seances-i-cours cours))))

    (intersection-dispo
      dispo
      (cons-dispo
	(creer-zone
	  (creer-intervalle
	    (cons-moment-cumule jour horaire-debut)
	    (cons-moment-cumule jour
	      (cond
		((> duree duree-cours) (+ horaire-fin (- duree duree-cours)))
		((> duree-cours duree) (- horaire-fin (- duree-cours duree)))
		(t horaire-fin)))))
	(creer-zone
	  (creer-intervalle semaine-debut semaine-fin))))))

;;; <fixe-dispo-avec-cours-fin>
;;; reduit la dispo a la periode pendant laquelle le cours a lieu
;;; lors d'une simultaneite de fin de seance

(defun fixe-dispo-avec-cours-fin (dispo duree cours)
  (let*
    ((instant-cours (instant-trouve-i-cours cours))
     (jour (jour-instant instant-cours))
     (duree-cours (duree-i-cours cours))
     (horaire-debut (interne2cumule(horaire-instant instant-cours)))
     (horaire-fin (add horaire-debut (duree-i-cours cours)))
     (semaine-debut (semaine-instant instant-cours))
     (semaine-fin (add semaine-debut (nb-seances-i-cours cours))))

    (intersection-dispo
      dispo
      (cons-dispo
	(creer-zone
	  (creer-intervalle
	    (cons-moment-cumule jour
	      (cond
		((> duree duree-cours) (- horaire-debut (- duree duree-cours)))
		((> duree-cours duree) (+ horaire-debut (- duree-cours duree)))
		(t horaire-debut)))
	    (cons-moment-cumule jour horaire-fin)))
	(creer-zone
	  (creer-intervalle semaine-debut semaine-fin))))))

;;; <fixe-jour-dispo-cours>
;;; reduit la dispo a la dispo du jour pendant lequel a lieu le cours

(defun fixe-jour-dispo-cours (dispo cours)
  (cons-dispo
    (dispo-moment-ce-jour dispo ()
      (jour-instant (instant-trouve-i-cours cours)))
    (trouver-dispo-semaine dispo)))

;;; <retire-jour-dispo-cours>
;;; retire a une dispo la dispo du jour pendant lequel a lieu le cours

(defun retire-jour-dispo-cours (dispo cours)
  (cons-dispo
    (difference-zone
      (trouver-dispo-moment dispo)
      (dispo-moment-ce-jour dispo ()
	(jour-instant (instant-trouve-i-cours cours))))
    (trouver-dispo-semaine dispo)))

;;; <fixe-horaire-dispo-cours>
;;; reduit la dispo a la dispo du jour pendant lequel a lieu le cours
;;; et en fonction d'une duree (pour fixer l'heure de debut)

(defun fixe-horaire-dispo-cours (dispo duree cours)
  (let*
    ((instant-cours (instant-trouve-i-cours cours))
     (horaire-debut (interne2cumule (horaire-instant instant-cours)))
     (horaire-fin (add horaire-debut duree)))
    (cons-dispo
      (mapcar
	(lambda (intervalle)
	  (let
	    ((jour (jour-moment-cumule (borne-inf-intervalle intervalle))))
	    (creer-intervalle
	      (cons-moment-cumule jour horaire-debut)
	      (cons-moment-cumule jour horaire-fin))))
	(trouver-dispo-moment dispo))
      (trouver-dispo-semaine dispo))))

;;; <fixe-semaine-dispo-cours>
;;; reduit la dispo pour que la premiere semaine soit celle du cours

(defun fixe-semaine-dispo-cours (dispo seances cours)
  (let
    ((sem-deb (semaine-instant (instant-trouve-i-cours cours))))
    (cons-dispo
      (trouver-dispo-moment dispo)
      (intersection-zone
	(trouver-dispo-semaine dispo)
	(creer-zone
	  (creer-intervalle sem-deb (add sem-deb seances)))))))

;;; <fixe-precedence-dispo-cours>
;;; reduit la dispo pour la contrainte de precedence avec le cours

(defun fixe-precedence-dispo-cours (dispo cours seances firstp) 
  (let*
    ((sem-deb (semaine-instant (instant-trouve-i-cours cours))))
    (cons-dispo
      (trouver-dispo-moment dispo)
      (intersection-zone
	(trouver-dispo-semaine dispo)
	(creer-zone
	  (if firstp
	    (creer-intervalle (semaine-debut-i-ecole i-ecole) sem-deb)
	    (creer-intervalle
	      (+ sem-deb seances) (semaine-fin-i-ecole i-ecole))))))))

;;; <fixe-proxy-sem-dispo-cours>
;;; reduit la dispo pour la contrainte de proximite de semaines avec le cours

(defun fixe-proxy-sem-dispo-cours (dispo seances cours firstp) 
  (let*
    ((sem-deb (semaine-instant (instant-trouve-i-cours cours))))
    (cons-dispo
      (trouver-dispo-moment dispo)
      (intersection-zone
	(trouver-dispo-semaine dispo)
	(creer-zone
	  (if firstp
	    (creer-intervalle
	      (max (semaine-debut-i-ecole i-ecole) (- sem-deb 3))
	      (min (semaine-fin-i-ecole i-ecole) (+ sem-deb seances)))
	    (creer-intervalle
	      sem-deb
	      (min (semaine-fin-i-ecole i-ecole)
		(+ sem-deb 3 seances)))))))))
 
;;; <reduire-dispo-avec-cours>
;;; reduit la dispo passee en parametre en fonction de la contrainte et de la 
;;; liste des cours auxquels elle s'applique (si ces cours on lieu a pendant la
;;; passee en parametre)
;;; duree a ete ajoute pour le traitement de la contrainte meme horaire .
     
(defun reduire-dispo-avec-cours
  (dispo cours-deplace semaine duree seances contrainte liste-cours detailp)
  (let
    ((dispo-resultat dispo)
     (dispo-tmp)
     (firstp))
    (mapc
      (lambda (cours)
	(when (consp cours)
	  (setq firstp (cdr cours))	; gestion des contraintes explicites
	  (setq cours (car cours)))

	(when (instant-trouve-i-cours cours)

	  (cond
	    ;; contrainte de NON-CHEVAUCHEMENT
	    ((equal contrainte 'poser-non-chevauchement)
	     (setq dispo-tmp
	       (retire-periode-cours
		 (if detailp dispo dispo-resultat) cours semaine detailp))
	     (if detailp
	       (when (not (dispo-cours-suffisante-p dispo-tmp cours-deplace))
		 (newl lst-err-contraintes (cons contrainte cours)))
	       (setq dispo-resultat dispo-tmp)))

	    ;; contrainte de SIMULTANEITE (debut de seance)
	    ((equal contrainte 'poser-simultaneite)
	     (setq dispo-tmp
	       (fixe-dispo-avec-cours-debut
		 (if detailp dispo dispo-resultat) duree cours))
	     (if detailp
	       (when (not (dispo-cours-suffisante-p dispo-tmp cours-deplace))
		 (newl lst-err-contraintes (cons contrainte cours)))
	       (setq dispo-resultat dispo-tmp)))
	    
	    ;; contrainte de SIMULTANEITE (fin de seance)
	    ((equal contrainte 'poser-simultaneite-fin-seance)
	     (setq dispo-tmp
	       (fixe-dispo-avec-cours-fin
		 (if detailp dispo dispo-resultat) duree cours))
	     (if detailp
	       (when (not (dispo-cours-suffisante-p dispo-tmp cours-deplace))
		 (newl lst-err-contraintes (cons contrainte cours)))
	       (setq dispo-resultat dispo-tmp)))
	    
	    ;; contrainte de MEME JOUR
	    ((eq contrainte 'poser-meme-jour)
	     (setq dispo-tmp
	       (fixe-jour-dispo-cours
		 (if detailp dispo dispo-resultat) cours))
	     (if detailp
	       (when (not (dispo-cours-suffisante-p dispo-tmp cours-deplace))
		 (newl lst-err-contraintes (cons contrainte cours)))
	       (setq dispo-resultat dispo-tmp)))
	    
	    ;; contrainte de JOURS DIFFERENTS
	    ((equal contrainte 'poser-jours-differents)
	      (setq dispo-tmp
	       (retire-jour-dispo-cours
		 (if detailp dispo dispo-resultat) cours))
	     (if detailp
	       (when (not (dispo-cours-suffisante-p dispo-tmp cours-deplace))
		 (newl lst-err-contraintes (cons contrainte cours)))
	       (setq dispo-resultat dispo-tmp)))
	    
	    ;; contrainte de MEME HORAIRE
	    ((equal contrainte 'poser-meme-horaire)
	     (setq dispo-tmp
	       (fixe-horaire-dispo-cours
		 (if detailp dispo dispo-resultat) duree cours))
	     (if detailp
	       (when (not (dispo-cours-suffisante-p dispo-tmp cours-deplace))
		 (newl lst-err-contraintes (cons contrainte cours)))
	       (setq dispo-resultat dispo-tmp)))

	    ;; contrainte de MEME SEMAINE
	    ((equal contrainte 'poser-meme-semaine)
	     (setq dispo-tmp
	       (fixe-semaine-dispo-cours
		 (if detailp dispo dispo-resultat) seances cours))
	     (if detailp
	       (when (not (dispo-cours-suffisante-p dispo-tmp cours-deplace))
		 (newl lst-err-contraintes (cons contrainte cours)))
	       (setq dispo-resultat dispo-tmp)))

	    ;; contrainte de PRECEDENCE
	    ((equal contrainte 'poser-precedence)
	     (setq dispo-tmp
	       (fixe-precedence-dispo-cours
		 (if detailp dispo dispo-resultat) cours seances firstp))
	     (if detailp
	       (when (not (dispo-cours-suffisante-p dispo-tmp cours-deplace))
		 (newl lst-err-contraintes (cons contrainte cours)))
	       (setq dispo-resultat dispo-tmp)))

	    ;; contrainte de PROXIMITE DE SEMAINES
	    ((equal contrainte 'poser-proximite-semaine)
	     (setq dispo-tmp
	       (fixe-proxy-sem-dispo-cours
		 (if detailp dispo dispo-resultat) seances cours firstp))
	     (if detailp
	       (when (not (dispo-cours-suffisante-p dispo-tmp cours-deplace))
		 (newl lst-err-contraintes (cons contrainte cours)))
	       (setq dispo-resultat dispo-tmp)))

	    ;; autre contrainte
	    (t dispo-resultat))))
      liste-cours)
    
    dispo-resultat))

;;; <appartenir-liste-groupes-p>
;;; verifie si un cours appartient a une liste de groupes ou a un de leurs
;;; sous-groupes

(defun appartenir-liste-groupes-p (groupes cours)
  (let*
    ((liste-groupes groupes)
     (groupe (nextl liste-groupes)))

    (until
      (or
	(null groupe)
	(member cours (cours-i-groupe groupe))
	(appartenir-liste-groupes-p (groupes-i-groupe groupe) cours))
      (nextl liste-groupes groupe))
    (not (null groupe))))

;;; <appartenir-sous-groupes-de-groupe-p>
;;; teste si un cours appartient a l'une des sous-famille d'une famille

(defun appartenir-sous-groupes-de-groupe-p (groupe cours)
  (appartenir-liste-groupes-p (groupes-i-groupe groupe) cours))

;;; <appartenir-groupe-p>
;;; teste si un cours appartient a l'une des sous-famille d'une famille

(defun appartenir-groupe-p (groupe cours)
  (or
    (member cours (cours-i-groupe groupe))
    (appartenir-liste-groupes-p (groupes-i-groupe groupe) cours)))

;;; <groupe-contrainte-cours-p>
;;; verifie su un groupe a dans la liste de ses cours <cours> et a une
;;; contrainte <contrainte>

(defun groupe-contrainte-cours-p (groupe contrainte cours)
  (and
    (member cours (cours-i-groupe groupe))
    (member contrainte (contraintes-i-groupe groupe))))

;;; Fonction fournissant la liste des cours subissant la contrainte
;;; <contrainte> avec le cours <cours>

(defun liste-cours-contraints (cours contrainte)
  (let
    ((liste-groupes
      (sous-liste groupe-contrainte-cours-p Liste-i-Groupe contrainte cours))
    (liste-groupes-peres
      (sous-liste
	(lambda (g)
	  (and
	    (member contrainte (contraintes-i-groupe g))
	    (appartenir-sous-groupes-de-groupe-p g cours)))
	Liste-I-Groupe))
    (liste-groupes-freres)
    (liste-cours))

    (setq liste-groupes-freres
      (mapcan
	(lambda (g)
	  (mapcan
	    (lambda (f)
	      (when (not (appartenir-sous-groupes-de-groupe-p f cours))
		(cons f (tous-les-sous-groupes f))))
	    (groupes-i-groupe g)))
	(append liste-groupes liste-groupes-peres)))
    (setq liste-cours
      (sous-liste
	(lambda (c)
	  (and
	    (instant-trouve-i-cours cours)
	    (not (eq (code-i-cours cours) (code-i-cours c)))
	    (or
	      (inter-non-vide-p (groupes-i-cours c) liste-groupes)
	      (inter-non-vide-p (groupes-i-cours c) liste-groupes-peres)
	      (inter-non-vide-p (groupes-i-cours c) liste-groupes-freres))))
	Liste-i-Cours))

    (when (eq contrainte 'poser-non-chevauchement)
      (mapc
	(lambda (p)
	  (setq liste-cours
	    (append liste-cours (copylist (cours-i-prof p)))))
	(profs-i-cours cours)))
    (setq liste-cours (delq cours liste-cours))
    (supprimer-occurences-multiples liste-cours)))

;;; On construit la liste des cours qui subissent la contrainte explicite
;;; <contrainte> avec le cours <cours>
;;; Dans le cas de PRECEDENCE ou PROXIMITE DE SEMAINES la liste de cours
;;; devient une liste (cours . firtsp) avec firstp = t si le cours
;;; <cours> est le premier dans liste (contraintes non reciproques !)

(defun liste-cours-contraints-explicitement (cours contrainte)
  (let
    ((lst-cours ())
     (lst-relations (contraintes-de-type contrainte)))
    (mapc
      (lambda (relation)
	(when (appartenir-liste (code-i-cours cours) relation)
	  (if
	    (or
	      (eq contrainte 'poser-precedence)
	      (eq contrainte 'poser-proximite-semaine))
	    (if (eq (code-i-cours cours) (car relation))
	      (setq lst-cours
		(append lst-cours (list (cons (cadr relation) t))))
	      (setq lst-cours
		(append lst-cours (list (cons (car relation) ())))))
	    (setq lst-cours (append lst-cours relation)))))
      lst-relations)
    (setq lst-cours (supprimer-occurences-multiples lst-cours))
    (setq lst-cours (delete (code-i-cours cours) lst-cours))
    (mapcar
      (lambda (elt)
	(if (consp elt)
	  (cons (eval (car elt)) (cdr elt))
	  (eval elt)))
      lst-cours)))

;;; <reduire-dispo>
;;; reduit une dispo <dispo> en fonction de la contrainte <contrainte> passee
;;; en parametre et qui pese sur les groupe auxquels est lie le cours.

(defun reduire-dispo (dispo semaine duree seances cours contrainte detailp)
  (let*
    ((liste-cours (liste-cours-contraints cours contrainte))
     (liste-cours-exp (liste-cours-contraints-explicitement cours contrainte))
     (liste-ajouts (liste-ajouts-contraints cours liste-cours contrainte))
     (liste-ajouts-exp
       (liste-ajouts-contraints cours liste-cours-exp contrainte))     
     (dispo-tmp dispo))

    ;; traitement des contraintes posees par les familles
    (when (and (not detailp) (eq contrainte 'poser-non-chevauchement))
      (newl liste-cours cours))
    (when liste-cours
      (setq dispo-tmp
	(reduire-dispo-avec-cours dispo-tmp cours semaine duree seances
	  contrainte liste-cours detailp)))
    
    ;; traitement des contraintes explicites
    (when (and (not detailp) (eq contrainte 'poser-non-chevauchement))
      (newl liste-cours-exp cours))
    (when liste-cours-exp
      (setq dispo-tmp
	(reduire-dispo-avec-cours dispo-tmp cours semaine duree seances
	  contrainte liste-cours-exp detailp)))
    
    ;; traitement des ajouts de seances (contraintes posees par les familles)
     (when liste-ajouts
       (setq dispo-tmp
	(reduire-dispo-avec-ajout dispo-tmp cours semaine duree seances
	  contrainte liste-ajouts detailp)))

     ;; traitement des ajouts de seances (contraintes explicites)
     (when liste-ajouts-exp
       (setq dispo-tmp
	(reduire-dispo-avec-ajout dispo-tmp cours semaine duree seances
	  contrainte liste-ajouts-exp detailp)))
     
    dispo-tmp))
    
;;; Analyse des contraintes.
;;; detailp = t lors d'un deplacement de cours pour construire la liste des
;;; erreurs
;;; detailp = () lors d'un ajout de seancepour une reduction de la dispo

(defun reduction-par-contraintes (dispo semaine duree seances cours contraintes detailp)
  (setq lst-err-contraintes ())
  (if detailp
    (mapc				; liste des erreurs
      (lambda (contrainte)
	(reduire-dispo dispo semaine duree seances cours contrainte detailp))
      contraintes)
    (mapc				; reduction de le dispo
      (lambda (contrainte)
	(setq dispo
	  (reduire-dispo dispo semaine duree seances cours contrainte detailp)))
      contraintes))
  dispo)

;;; CRE RM 04/11/97
;;; Les memes fonctions de reductions pour les ajouts de seances ...

;;; <retire-moment-ajout>
;;; arg: dispo
;;;      ajout
;;; retire a une dispo les moments pendant lesquels l'ajout a lieu

(defun retire-moments-ajout (dispo ajout)
  (let*
    ((dispo-semaines (trouver-dispo-semaine dispo))
     (instant-ajout (instant-i-ajout ajout))
     (jour (jour-instant instant-ajout))
     (horaire-debut (interne2cumule (horaire-instant instant-ajout)))
     (horaire-fin (add horaire-debut (duree-i-ajout ajout))))

    (cons-dispo
      (difference-zone
	(trouver-dispo-moment dispo)
	(creer-zone
	  (creer-intervalle
	    (cons-moment-cumule jour horaire-debut)
	    (cons-moment-cumule jour horaire-fin))))
      dispo-semaines)))

;;; <retire-semaine-ajout>
;;; arg: dispo
;;;      cours
;;; retire a une dispo la semaine pendant laquelle l'ajout a lieu

(defun retire-semaine-ajout (dispo ajout)
  (let*
    ((instant-ajout (instant-i-ajout ajout))
     (semaine-debut (semaine-instant instant-ajout))
     (semaine-fin (add semaine-debut 1))
     (intervalle-semaines-cours
       (creer-zone (creer-intervalle semaine-debut semaine-fin))))

    (cons-dispo
      (trouver-dispo-moment dispo)
      (difference-zone
	(trouver-dispo-semaine dispo)
	intervalle-semaines-cours))))


;;; <retire-periode-ajout>
;;; supprime de la dispo la periode pendant laquelle le cours a lieu

(defun retire-periode-ajout (dispo ajout semaine detailp)
  (if detailp
    (let
      ((dispo-commune
	(intersection-dispo
	  dispo
	  (construire-dispo-ajout
	    (instant-i-ajout ajout) (duree-i-ajout ajout)))))
      (when (not (dispo-vide-p dispo-commune))
	(setq dispo
	  (retire-moments-ajout
	    (retire-semaine-ajout dispo ajout)
	    ajout))))
    (when (eq semaine (semaine-i-ajout ajout))
      (setq dispo (retire-moments-ajout dispo ajout))))
  dispo)
  
;;; <fixe-dispo-avec-ajout-debut>
;;; reduit la dispo a la periode pendant laquelle l'ajout a lieu
;;; lors d'une simultaneite de debut de seance

(defun fixe-dispo-avec-ajout-debut (dispo duree ajout)
  (let*
    ((instant-ajout (instant-i-ajout ajout))
     (jour (jour-instant instant-ajout))
     (duree-ajout (duree-i-ajout ajout))
     (horaire-debut (interne2cumule (horaire-instant instant-ajout)))
     (horaire-fin (add horaire-debut (duree-i-ajout ajout)))
     (semaine-debut (semaine-instant instant-ajout))
     (semaine-fin (add semaine-debut 1)))

    (intersection-dispo
      dispo
      (cons-dispo
	(creer-zone
	  (creer-intervalle
	    (cons-moment-cumule jour horaire-debut)
	    (cons-moment-cumule jour
	      (cond
		((> duree duree-ajout) (+ horaire-fin (- duree duree-ajout)))
		((> duree-ajout duree) (- horaire-fin (- duree-ajout duree)))
		(t horaire-fin)))))
	(creer-zone
	  (creer-intervalle semaine-debut semaine-fin))))))

;;; <fixe-dispo-avec-ajout-fin>
;;; reduit la dispo a la periode pendant laquelle l'ajout a lieu
;;; lors d'une simultaneite de fin de seance

(defun fixe-dispo-avec-ajout-fin (dispo duree ajout)
  (let*
    ((instant-ajout (instant-i-ajout ajout))
     (jour (jour-instant instant-ajout))
     (duree-ajout (duree-i-ajout ajout))
     (horaire-debut (interne2cumule(horaire-instant instant-ajout)))
     (horaire-fin (add horaire-debut (duree-i-ajout ajout)))
     (semaine-debut (semaine-instant instant-ajout))
     (semaine-fin (add semaine-debut 1)))

    (intersection-dispo
      dispo
      (cons-dispo
	(creer-zone
	  (creer-intervalle
	    (cons-moment-cumule jour
	      (cond
		((> duree duree-ajout) (- horaire-debut (- duree duree-ajout)))
		((> duree-ajout duree) (+ horaire-debut (- duree-ajout duree)))
		(t horaire-debut)))
	    (cons-moment-cumule jour horaire-fin)))
	(creer-zone
	  (creer-intervalle semaine-debut semaine-fin))))))

;;; <fixe-jour-dispo-ajout>
;;; reduit la dispo a la dispo du jour pendant lequel a lieu l'ajout

(defun fixe-jour-dispo-ajout (dispo ajout)
  (cons-dispo
    (dispo-moment-ce-jour dispo ()
      (jour-instant (instant-i-ajout ajout)))
    (trouver-dispo-semaine dispo)))

;;; <retire-jour-dispo-ajout>
;;; retire a une dispo la dispo du jour pendant lequel a lieu l'ajout

(defun retire-jour-dispo-ajout (dispo ajout)
  (cons-dispo
    (difference-zone
      (trouver-dispo-moment dispo)
      (dispo-moment-ce-jour dispo ()
	(jour-instant (instant-i-ajout ajout))))
    (trouver-dispo-semaine dispo)))

;;; <fixe-horaire-dispo-ajout>
;;; reduit la dispo a la dispo du jour pendant lequel a lieu l'ajout
;;; et en fonction d'une duree (pour fixer l'heure de debut)

(defun fixe-horaire-dispo-ajout (dispo duree ajout)
  (let*
    ((instant-ajout (instant-i-ajout ajout))
     (horaire-debut (interne2cumule (horaire-instant instant-ajout)))
     (horaire-fin (add horaire-debut duree)))
    (cons-dispo
      (mapcar
	(lambda (intervalle)
	  (let
	    ((jour (jour-moment-cumule (borne-inf-intervalle intervalle))))
	    (creer-intervalle
	      (cons-moment-cumule jour horaire-debut)
	      (cons-moment-cumule jour horaire-fin))))
	(trouver-dispo-moment dispo))
      (trouver-dispo-semaine dispo))))

;;; <fixe-semaine-dispo-ajout>
;;; reduit la dispo pour que la premiere semaine soit celle de l'ajout

(defun fixe-semaine-dispo-ajout (dispo seances ajout)
  (let
    ((sem-deb (semaine-instant (instant-i-ajout ajout))))
    (cons-dispo
      (trouver-dispo-moment dispo)
      (intersection-zone
	(trouver-dispo-semaine dispo)
	(creer-zone
	  (creer-intervalle sem-deb (add sem-deb seances)))))))

;;; <fixe-precedence-dispo-ajout>
;;; reduit la dispo pour la contrainte de precedence avec l'ajout

(defun fixe-precedence-dispo-ajout (dispo ajout seances firstp) 
  (let*
    ((sem-deb (semaine-instant (instant-i-ajout ajout))))
    (cons-dispo
      (trouver-dispo-moment dispo)
      (intersection-zone
	(trouver-dispo-semaine dispo)
	(creer-zone
	  (if firstp
	    (creer-intervalle (semaine-debut-i-ecole i-ecole) sem-deb)
	    (creer-intervalle
	      (+ sem-deb seances) (semaine-fin-i-ecole i-ecole))))))))

;;; <fixe-proxy-sem-dispo-ajout>
;;; reduit la dispo pour la contrainte de proximite de semaines avec l'ajout

(defun fixe-proxy-sem-dispo-ajout (dispo seances ajout firstp) 
  (let*
    ((sem-deb (semaine-instant (instant-i-ajout ajout))))
    (cons-dispo
      (trouver-dispo-moment dispo)
      (intersection-zone
	(trouver-dispo-semaine dispo)
	(creer-zone
	  (if firstp
	    (creer-intervalle
	      (max (semaine-debut-i-ecole i-ecole) (- sem-deb 3))
	      (min (semaine-fin-i-ecole i-ecole) (+ sem-deb seances)))
	    (creer-intervalle
	      sem-deb
	      (min (semaine-fin-i-ecole i-ecole)
		(+ sem-deb 3 seances)))))))))

;;; <reduire-dispo-avec-ajout>
;;; reduit la dispo passee en parametre en fonction de la contrainte et de la 
;;; liste des ajouts auxquels elle s'applique.
     
(defun reduire-dispo-avec-ajout
  (dispo cours-deplace semaine duree seances contrainte liste-ajout detailp)
  (let
    ((dispo-resultat dispo)
     (dispo-tmp)
     (firstp))
    (mapc
      (lambda (ajout)
	(when (consp ajout)
	  (setq firstp (cdr ajout))	; gestion des contraintes explicites
	  (setq ajout (car ajout)))

	(cond
	  ;; contrainte de NON-CHEVAUCHEMENT
	  ((equal contrainte 'poser-non-chevauchement)
	    (setq dispo-tmp
	      (retire-periode-ajout
		(if detailp dispo dispo-resultat) ajout semaine detailp))
	    (if detailp
	      (when (not (dispo-cours-suffisante-p dispo-tmp cours-deplace))
		(newl lst-err-contraintes (cons contrainte ajout)))
	      (setq dispo-resultat dispo-tmp)))
	  
	  ;; contrainte de SIMULTANEITE (debut de seance)
	  ((equal contrainte 'poser-simultaneite)
	    (setq dispo-tmp
	      (fixe-dispo-avec-ajout-debut
		(if detailp dispo dispo-resultat) duree ajout))
	    (if detailp
	      (when (not (dispo-cours-suffisante-p dispo-tmp cours-deplace))
		(newl lst-err-contraintes (cons contrainte ajout)))
	      (setq dispo-resultat dispo-tmp)))
	  
	  ;; contrainte de SIMULTANEITE (fin de seance)
	  ((equal contrainte 'poser-simultaneite-fin-seance)
	    (setq dispo-tmp
	      (fixe-dispo-avec-ajout-fin
		(if detailp dispo dispo-resultat) duree ajout))
	    (if detailp
	      (when (not (dispo-cours-suffisante-p dispo-tmp cours-deplace))
		(newl lst-err-contraintes (cons contrainte ajout)))
	      (setq dispo-resultat dispo-tmp)))
	  
	  ;; contrainte de MEME JOUR
	  ((eq contrainte 'poser-meme-jour)
	    (setq dispo-tmp
	      (fixe-jour-dispo-ajout
		(if detailp dispo dispo-resultat) ajout))
	    (if detailp
	      (when (not (dispo-cours-suffisante-p dispo-tmp cours-deplace))
		(newl lst-err-contraintes (cons contrainte ajout)))
	      (setq dispo-resultat dispo-tmp)))
	  
	  ;; contrainte de JOURS DIFFERENTS
	  ((equal contrainte 'poser-jours-differents)
	    (setq dispo-tmp
	      (retire-jour-dispo-ajout
		(if detailp dispo dispo-resultat) ajout))
	    (if detailp
	      (when (not (dispo-cours-suffisante-p dispo-tmp cours-deplace))
		(newl lst-err-contraintes (cons contrainte ajout)))
	      (setq dispo-resultat dispo-tmp)))
	  
	  ;; contrainte de MEME HORAIRE
	  ((equal contrainte 'poser-meme-horaire)
	    (setq dispo-tmp
	      (fixe-horaire-dispo-ajout
		(if detailp dispo dispo-resultat) duree ajout))
	    (if detailp
	      (when (not (dispo-cours-suffisante-p dispo-tmp cours-deplace))
		(newl lst-err-contraintes (cons contrainte ajout)))
	      (setq dispo-resultat dispo-tmp)))
	  
	  ;; contrainte de MEME SEMAINE
	  ((equal contrainte 'poser-meme-semaine)
	    (setq dispo-tmp
	      (fixe-semaine-dispo-ajout
		(if detailp dispo dispo-resultat) seances ajout))
	    (if detailp
	      (when (not (dispo-cours-suffisante-p dispo-tmp cours-deplace))
		(newl lst-err-contraintes (cons contrainte ajout)))
	      (setq dispo-resultat dispo-tmp)))
	  
	  ;; contrainte de PRECEDENCE
	  ((equal contrainte 'poser-precedence)
	    (setq dispo-tmp
	      (fixe-precedence-dispo-ajout
		(if detailp dispo dispo-resultat) ajout seances firstp))
	    (if detailp
	      (when (not (dispo-cours-suffisante-p dispo-tmp cours-deplace))
		(newl lst-err-contraintes (cons contrainte ajout)))
	      (setq dispo-resultat dispo-tmp)))
	  
	  ;; contrainte de PROXIMITE DE SEMAINES
	  ((equal contrainte 'poser-proximite-semaine)
	    (setq dispo-tmp
	      (fixe-proxy-sem-dispo-ajout
		(if detailp dispo dispo-resultat) seances ajout firstp))
	    (if detailp
	      (when (not (dispo-cours-suffisante-p dispo-tmp cours-deplace))
		(newl lst-err-contraintes (cons contrainte ajout)))
	      (setq dispo-resultat dispo-tmp)))
	  
	  ;; autre contrainte
	  (t dispo-resultat)))
      liste-ajout)
    dispo-resultat))

;;; Retourne la liste des ajouts de seances en contrainte avec le cours
;;; deplace
;;; Dans le cas d'un non-chevauchement, on ajoute les ajouts du cours

(defun liste-ajouts-contraints (cours-dep liste-cours contrainte)
  (let 
    ((lst ()))
    (mapc
      (lambda (cours)
	(setq lst (append lst (liste-ajouts-i-cours cours))))
      liste-cours)
    (when (eq contrainte 'poser-non-chevauchement)
      (setq lst (append lst (liste-ajouts-i-cours cours-dep))))
    (supprimer-occurences-vides lst)))

