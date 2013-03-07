;;; --------------------------------------------------------------------------
;;; - I-Input.ll                                                VARUNA 1.2   -
;;; - Interface Graphique                                                    -
;;; - Routines de saisie generiques et controles                             -
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; --------------------------------------------------------------------------
;;; - Creation:  Mars 93                                                     -
;;; - Mise a jour pour version 1.07: Decembre 93                             -
;;; - Build 1.22.1 01.07.99 / JH MO                                          -
;;; --------------------------------------------------------------------------


;; marco generale pour la saisie d'une valeur typee
;; libelle est la fonction qui transforme la valeur type en chaine de car
;; sortie est la fonction qui transforme la chaine en valeur type
;; la fonction de filtre doit s'ecrire <filtre-demande>-type

(dmd def-demandeur-valeur-type (type libelle sortie)
  `(defun ,(concat 'demande- type) (prompt min max)
    (let ((resultat (demande-valeur-type
      prompt min max ',libelle
      ',(concat 'filtre-demande- type))))
        (and
          resultat
          (,sortie resultat)))))

;;; demande d'entiers

(def-demandeur-valeur-type entier string string2number)

;;; demande de symbole

(defun demande-symbole (prompt)
  (let
    ((resultat
      (demande-valeur-type
        prompt () ()
        (lambda (u) "")
        'filtre-demande-symbole)))
    (and 
      resultat
      (symbol () resultat))))

;;; demande d'horaire

(defun demande-heure (prompt debut fin etab)
  (let 
    ((resultat 
      (demande-valeur-type
        prompt
	(if etab
	  (max debut HORAIRE-CUMULE-OUVERTURE)
	  debut)
	(if etab
	  (min fin HORAIRE-CUMULE-FERMETURE)
	  fin)
        'cumule2naturel 'filtre-demande-heure)))
    (and
      resultat
      (naturel2cumule resultat))))

;;; demande de duree

(defun demande-duree (prompt min max)
  (let 
    ((resultat 
      (demande-valeur-type
        prompt min max 'cumule2naturel 'filtre-demande-duree)))
    (and
      resultat
      (naturel2cumule resultat))))

;;; les filres

;;; filtre de demande d'une duree

(defun filtre-demande-duree (h duree-mini-c duree-maxi-c)
  (let ((label-hmini
    (and duree-mini-c (funcall 'cumule2naturel duree-mini-c)))
  (label-hmaxi
    (and duree-maxi-c (funcall 'cumule2naturel duree-maxi-c))))
; h est une chaine duree au format naturel 2h30 
  (cond
    ((or (null h)
      (not (horaire-valide-p h 'naturel)))
    (alerte-type 'afficher-erreur #Mmust-be-hour))             
    ((and duree-mini-c
      (not (horaire-naturel-plus-grand-egal-p 
        h
        label-hmini)))
    (alerte-type 
      'afficher-erreur
      (catenate #Mhmust-be-more label-hmini)))
    
    ((and duree-maxi-c
      (not (horaire-naturel-plus-petit-egal-p
        h
        label-hmaxi)))
    (alerte-type 
      'afficher-erreur
      (catenate #Mhmust-be-less label-hmaxi)))
    ((not (integerp (naturel2interne h)))
      (alerte-type
        'afficher-erreur
        (catenate #Mv_duree-non-valide Unite-Temps)))
    (t ()))))

;;; filtre de demande d'un horaire

(defun filtre-demande-heure (h horaire-mini-c horaire-maxi-c)
  (let ((label-hmini
    (and horaire-mini-c (funcall 'cumule2naturel horaire-mini-c)))
  (label-hmaxi
    (and horaire-maxi-c (funcall 'cumule2naturel horaire-maxi-c))))
; h est une chaine horaire au format naturel 8h30 
  (cond
    ((or (null h)
      (not (horaire-valide-p h 'naturel)))
    (alerte-type 'afficher-erreur #Mmust-be-hour))             
    ((and horaire-mini-c
      (not (horaire-naturel-plus-grand-egal-p 
        h
        label-hmini)))
    (alerte-type 
      'afficher-erreur
      (catenate #Mhmust-be-more label-hmini)))
    ((and horaire-maxi-c
      (not (horaire-naturel-plus-petit-egal-p
        h
        label-hmaxi)))
    (alerte-type 
      'afficher-erreur
      (catenate #Mhmust-be-less label-hmaxi)))
    (t ()))))


;;; filtre un nombre entier

(defun filtre-demande-entier (val min max)
  (let ((num (string2number val)))
    (cond 
      ((not (numberp num))
        (afficher-erreur #Mmust-be-int))
      ((or (and min (< num min)) (and max (> num max)))
        (afficher-erreur
          (row
            #Mv_valeur
            #Mv_war-inter
            (catenate min " - " max))))
      (t ()))))

;;; filtre un symbole

(defun filtre-demande-symbole (val min max)
  (if (est-un-symbole val)
    ()
    (afficher-erreur (row #Mv_valeur #Mv_war-symbol))))


;;; autres fonctions de demande

;;; demande un jour ouvres

(defun demande-jour-ouvres ()
  (symbol ()
    (demande-simple-type
      #Mv_pro-jour
      (mapcar 'string (mapcar 'libelle-jour Jours-Ouvres)) ())))

;;; demande de plusieurs jours ouvres

(defun demande-jours-ouvres ()
  (mapcar'string2symbol
    (demandes-simples-type
      #Mv_pro-jours
      (mapcar 'string (mapcar 'libelle-jour Jours-Ouvres)) ()))) 

;;; demande d'un jour ouvrable

(defun demande-jour-ouvrable ()
  (symbol ()
    (demande-simple-type
      #Mv_pro-jour
      (mapcar 'string Jours-Ouvrables) ())))

;;; demande de plusieurs jours ouvrables

(defun demande-jours-ouvrables ()
  (mapcar'string2symbol
    (demandes-simples-type
      #Mv_pro-jours
      (mapcar 'string Jours-Ouvrables) ())))

;;; demande d'une liste d'horaires compris entre min et max (en cumule)
;;; et chacun separes de pas

(defun demande-horaires (pas min max)
  (let
    ((liste-horaires ())
     (liste-libelles-horaires ())
     (hor min))
    (when (and (integerp pas) (gt pas 0) (lt min max))  
      (while (le hor max)
        (newl liste-horaires hor)
        (setq hor (add hor pas))))
    (setq liste-libelles-horaires
      (mapcar 'cumule2naturel (reverse liste-horaires)))
    (mapcar 'naturel2cumule
      (demandes-simples-type
        #Mv_horaires
        liste-libelles-horaires
        ()))))


;;; il faut maintenir une liste de toutes les contraintes possibles
;;; contraintes n-aires ou contraintes binaires ?

;;; demande d'un contrainte parmi LISTE-LIBELLES-CONTRAINTES
;;; renvoie le code de la contrainte (# du libelle)

(defun demande-contrainte ()
  (libelle-contrainte2code-contrainte
    (demande-simple-type
      #Mv_quelle-contrainte
      LISTE-LIBELLES-CONTRAINTES
      ())))

;;; demande d'un moment cumule
;;; si un type de cours est precise, on propose les horaires possibles

(defun demande-horaire-type (type jour)
  (let
    ((heure
      (demande-simple-type
        #Mv_horaire
	(supprimer-occurences-vides
	  (mapcar
	    (lambda (m)
	      (when (eq (jour-moment-cumule m) jour)
		(cumule2naturel (horaire-moment-cumule m))))
	    (liste-moments-cumules-i-type-cours type)))
	())))
    (and heure (naturel2cumule heure))))

(defun demande-moment-cumule (type)
  (let
    ((jour (demande-jour-ouvres))
     (horaire))
    (when jour
      (if type
        (setq horaire (demande-horaire-type type (jour-libelle jour)))
        (setq horaire (demande-heure #Mv_horaire 0 (* NB-MINUTES NB-HEURES) t)))
      (when horaire
        (cons-moment-cumule (jour-libelle jour) horaire)))))

;;; demande de plusieurs moments cumule
;;; factorise les jours

(defun demande-moments-cumules ()
  (let*
    ((l-jours (demande-jours-ouvrables))
     (l-horaires
       (and l-jours
;        (demande-horaires Unite-Temps 0 (* NB-MINUTES NB-HEURES))))
         ;; MOD FD - RM : 15/05/96 horaires proposes en fct de l'etablissement 
         (demande-horaires Unite-Temps
           HORAIRE-CUMULE-OUVERTURE
           HORAIRE-CUMULE-FERMETURE)))   
     (l-moments ()))
    (when (and l-jours l-horaires)
      (mapc
        (lambda (jour)
          (mapc
            (lambda (horaire)
              (newl l-moments
                (cons-moment-cumule (jour-libelle jour) horaire)))
            l-horaires))
        l-jours))
    l-moments))

;;; demande d'un intervalle de moments
;;; factorise avec les jours

(defun demande-intervalles ()
  (let*
    ((l-jours (demande-jours-ouvrables))
     (max (* NB-MINUTES NB-HEURES))
     (horaire1 (and l-jours (demande-heure #Mv_heure-debut 0 max ())))
     (horaire2 (and horaire1 (demande-heure #Mv_heure-fin horaire1 max ())))
     (l-inter ()))
    (when horaire2
      (mapc
        (lambda (jour)
          (newl l-inter
            (creer-intervalle
              (cons-moment-cumule (jour-libelle jour) horaire1)
              (cons-moment-cumule (jour-libelle jour) horaire2))))
        l-jours)
      ;; MOD FD - RM 15/05/96 : maj des horaires cumules ouverture et fermeture
      (setq HORAIRE-CUMULE-OUVERTURE (min HORAIRE-CUMULE-OUVERTURE horaire1))
      (setq HORAIRE-CUMULE-FERMETURE (max HORAIRE-CUMULE-FERMETURE horaire2)))  
    l-inter))
 
;;; demande de semaine (prise en compte des semaines calendaires)
;;; <demande-semaine> - CRE - FD - 29/07/96
;;; min et max sont des semaines interne Varuna
 
(defun demande-semaine (prompt min max)
  (let
    ((resultat))
    (ifn (calendaire-i-ecole I-ECOLE)
      (setq resultat
        (demande-valeur-type-intervalles
          prompt (list (cons min (+ max 1)))
          'string
          'filtre-demande-semaine))
      (setq resultat
        (demande-valeur-type-intervalles
          prompt
          (intersection-periodes-calendaires
            (cons
              (ecrire-numero-semaine min)
              (+ (ecrire-numero-semaine max) 1)))
          'string
          'filtre-demande-semaine)))
    (if
      resultat
      (lire-numero-semaine (string2number resultat)))))

;;; <filtre-demande-semaine> - CRE - FD - 29/07/96
;;; min et max en semaine interne Varuna

(defun filtre-demande-semaine (val liste-intervalles)
  (let ((num (string2number val)))
    (cond 
      ((not (numberp num))
        (afficher-erreur #Mmust-be-int))
      ((and
        (not (calendaire-i-ecole I-ECOLE))
        (not (appartenir-intervalle-p num (car liste-intervalles))))
      (afficher-erreur
        (row
          #Mv_valeur
          #Mv_war-inter
          (liste-intervalles2chaine liste-intervalles))))
      ((and
        (calendaire-i-ecole I-ECOLE)
        (not (semaine-calendaire-p num)))
      (afficher-erreur
        (row
          #Mv_valeur
          #Mv_war-inter
          (liste-intervalles2chaine liste-intervalles))))
      (t ()))))

;;; <demande-intervalle-semaines> - CRE - FD - 30/07/96

(defun demande-intervalle-semaines ()
  (let*
    ((semaine1 (demande-entier #Mv_sem-cal-debut 1 NB-SEMAINES))
     (semaine2 (when semaine1
       (demande-entier #Mv_sem-cal-fin semaine1 NB-SEMAINES))))

    (when (and semaine1 semaine2)
      (list (creer-intervalle semaine1 (add1 semaine2))))))
      
;;; CRE RM - 18/12/96

;;; demande de la valeur de pause
;;; ex : valeur dans [0 60] et valeur modulo 10 = 0
;;; => 0, 10, 20, 30, 40, 50, 60

(defun demande-entier-pause (old-msg msg min max)
  (let
    ((resultat
      (demande-valeur-pause
        old-msg msg min max (unite-temps-i-ecole i-ecole))))
    (and 
      resultat
      (string2number resultat))))

(defun demande-valeur-pause (old-msg msg min max grad)
  (let 
    ((cont 1)
     (result ())
     (val))
    (while (neq cont 0)
      (setq val
        (afficher-demande-valeur
          (column
	    old-msg
	    (catenate msg" ? [" min "-" max "]"))
	  () t ()))
      (if val
        (when (not (filtre-demande-entier-intervalle val min max grad))
          (setq result val)
          (setq cont 0))
        (setq cont 0)))
    result))


;;; filtre d'entier dans un intervalle avec une graduation
;;; ex : valeur dans [0 60] et valeur modulo 10 = 0
;;; => 0, 10, 20, 30, 40, 50, 60

(defun filtre-demande-entier-intervalle (val min max graduation)
  (let
    ((num (string2number val)))
    (cond 
      ((not (numberp num))
        (afficher-erreur #Mmust-be-int))
      ((or (and min (< num min)) (and max (> num max)))
        (afficher-erreur
          (row
            #Mv_valeur
            #Mv_war-inter
            (catenate min " - " max))))
      ((not (eq (modulo num graduation) 0))
        (afficher-erreur
          (row
            #Mv_valeur #Mv_multiple
            (string graduation))))
      (t ()))))
