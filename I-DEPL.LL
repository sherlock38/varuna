;; ---------------------------------------------------------------------------
;;; - I-Depl.ll                                                 VARUNA 1.2   -
;;; - Utilitaires Varuna pour le deplacement des rectangles                  -
;;; -                                                                        -
;;; --------------------------------------------------------------------------
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; - Creation:  Septembre 94                                                -
;;; - Derniere mise a jour pour version 1.13:: Mai 1995 MO                   -
;;; - Build 1.22.1 01.07.99 / JH MO                                          -
;;; --------------------------------------------------------------------------

;;; trv (26/05/95) MO
;;; en cours de travail pour debugger (120595), prendre en compte les resas
;;; et les contraintes explicites comme implicites. 

;;; rem (17/10/94) MO
;;; l'esprit de programmation ici est de ne fabriquer aucune liste
;;; que du predicat !


;;; primitive <deplacement-possible>
;;; chg (26/05/95) MO
;;; j'ai change l'evaluation en sortie de deplacement-possible;  en effet, il
;;; fallait que cela soit t ou () pour pouvoir distinguer les deux cas
;;; c'etait utilise dans i-contr.ll pour con-place n'importe comment !!
;;; de plus, pour pouvoir affecter une salle, il fallait la donner
;;; explicitement

;;; en entree:
;;;      - un i-cours
;;;      - un instant (plus tard un moment cumule et une semaine)

;;; en sortie 
;;;      la salle a affecter
;;;   ou
;;;      () et construction de messages


;;; predicat <deplacement-possible-p>

(defun deplacement-possible-p (i-cours instant)
  (let 
    ((moment
      (moment-instant-cumule (instant-interne2instant-cumule instant)))
     (semaine
       (semaine-instant-cumule (instant-interne2instant-cumule instant))))
    (and
      (cours-disponible-p i-cours moment semaine)
      (profs-disponibles-p i-cours moment semaine)
      (groupes-i-cours-disponibles-p i-cours moment semaine)
      (profs-non-deja-pris-p i-cours moment semaine)
      (salle-du-type-disponible-p i-cours moment semaine)
      (contraintes-explicites-respectees-p i-cours moment semaine)
      (moment-debut-possible-p i-cours moment semaine)
      (horaire-debut-possible-p i-cours moment))))
      

;;; <choisir-salle-disponible>
;;; CRE - FD - 20/02/96
;;; permet de choisir une salle parmis celle qui sont diponibles pour le
;;; cours (donne en arg) et au moment <moment> de la semaine <semaine>

;;; MOD RM - 23/07/96 : on passe l'ancienne salle en parametre
;;; pour les deplacements

(defun choisir-salle-disponible (i-cours moment semaine salle)
  (let
    ((salles-disponibles (donner-salles-disponibles i-cours moment semaine)))
    (if
      (and
	salle
	(eq (length salles-disponibles) 1)
	(eq salle (car salles-disponibles)))
      salle
      (if salles-disponibles
	(demande-filtree-i-salle
	  #Mv_choisir-salle-disponible
	  #Merrdepsal
	  ()
	  (lambda (s)
	    (member s salles-disponibles)))
	(newl liste-erreurs 'INFO-DEP-pas-de-salles)))))


;;; <donner-salles-disponibles>
;;; CRE - FD - 20/02/96

(defun donner-salles-disponibles (i-cours moment semaine)
  (sous-liste salle-possible-p LISTE-I-SALLE i-cours semaine moment))

;;; <salle-possible>

(defun salle-possible-p (salle i-cours semaine moment)
  (let 
    ((duree (duree-i-cours i-cours))
     (type-salle (type-salle-i-cours i-cours))
     (nb-seances (nb-seances-i-cours i-cours))
     (audience (audience-i-cours i-cours)))
    (and
      (eq (type-i-salle salle) type-salle)
      (le audience (capacite-i-salle salle))
      (salle-disponible-p salle moment duree semaine nb-seances i-cours))))

;;; <contraintes-explicites-respectees-p>

(defun contraintes-explicites-respectees-p (i-cours moment semaine)
  t)

;;; predicat <salle-du-type-disponible-p>

(defun salle-du-type-disponible-p (i-cours moment semaine)
  (let 
    ((la-liste liste-i-salle)
     (duree (duree-i-cours i-cours))
     (type-salle (type-salle-i-cours i-cours))
     (nb-seances (nb-seances-i-cours i-cours))
     (buffer ())
     (la-salle)
     (audience (audience-i-cours i-cours)))
    (while 
      (and (setq la-salle (nextl la-liste)) (not buffer))
      (and
        (eq (type-i-salle la-salle) type-salle)
        (le audience (capacite-i-salle la-salle))
        (setq buffer 
          (salle-disponible-p la-salle moment duree semaine nb-seances i-cours))))
    buffer))


;;; predicat <salle-disponible-p>
;;; a priori, la salle est disponible (buffer t), on regarde un a un tous 
;;; les cours (et les reservations ???) pour voir si il y a chevauchement
;;; entre un cours ayant lieu dans cette salle avec le creneau 
;;; (moment semaine duree) donne en argument

;;; l' ALGO
;;;
;;; pour tous les cours et tant que la salle est dispo (buffer t)
;;; SI
;;; ET cours instancie, cours diff cours depart, salle-cours du type de salle
;;; ALORS buffer est le resultat du chevauchement du crenau et de la seance 
;;; du cours

;;; ??? (19/05/95) MO
;;; que se passe-t-il exactement si i-cours est renseigne et
;;; que l'on demande les memes horaires en deplacement

;;; DBG (22/11/94) JH 
;;;  on verifie que les cours ne sont pas instancies avant regarder les 
;;; chevauchements
;;; ligne (when (instant-trouve-i-cours le-cours))
;;; chg (18/05/95) MO
;;; (when <pred1> (and <pred2> ..... <predn>)) en (and <pred1> <pred2>....)

;;; DBG (12/05/95) bug mo150500
;;;  deplacement des cours la correction me semble etre un car
;;; en effet dans le neq i-cours le cours, le cours est un objet et i-cours
;;; () ou une liste
;;;
;;; AJT RM 11/08/95
;;; Gestion des resas et des ajouts de seances
;;; On verifie que la salle est libre


(defun salle-disponible-p (salle moment duree semaine nb-seances . i-cours)
  (let 
    ((la-liste liste-i-cours)
     (la-liste-resa liste-i-resa) 
     (la-liste-ajout liste-i-ajout)
     (buffer t)
     (le-cours)
     (la-resa)
     (l-ajout))
    (while 
      (and (setq le-cours (nextl la-liste)) buffer)
      (and
        (instant-trouve-i-cours le-cours)       
        (if i-cours (neq (car i-cours) le-cours) t) 
        (eq salle (eval (salle-trouvee-i-cours le-cours)))
        (setq buffer              
          (not 
            (chevauchement-rectangles-p 
              moment duree semaine nb-seances
              (moment-instant-cumule 
                (instant-interne2instant-cumule
                  (instant-trouve-i-cours le-cours)))
              (duree-i-cours le-cours)
              (semaine-instant-cumule
                (instant-interne2instant-cumule
                  (instant-trouve-i-cours le-cours)))
              (nb-seances-i-cours le-cours))))))
    (while 
      (and (setq la-resa (nextl la-liste-resa)) buffer)
      (and
        (instant-i-resa la-resa)       
        (eq salle (eval (salle-i-resa la-resa)))
        (setq buffer              
          (not 
            (chevauchement-rectangles-p 
              moment duree semaine nb-seances
              (moment-instant-cumule 
                (instant-interne2instant-cumule
                  (instant-i-resa la-resa)))
              (duree-i-resa la-resa)
              (semaine-instant-cumule
                (instant-interne2instant-cumule
                  (instant-i-resa la-resa))) 1)))))
    (while 
      (and (setq l-ajout (nextl la-liste-ajout)) buffer)
      (and
        (instant-i-ajout l-ajout)       
        (eq salle (eval (salle-i-ajout l-ajout)))
        (setq buffer              
          (not 
            (chevauchement-rectangles-p 
              moment duree semaine nb-seances
              (moment-instant-cumule 
                (instant-interne2instant-cumule
                  (instant-i-ajout l-ajout)))
              (duree-i-ajout l-ajout)
              (semaine-instant-cumule
                (instant-interne2instant-cumule
                  (instant-i-ajout l-ajout))) 1)))))
    buffer))


;;; predicat <profs-disponibles-p>

(defun profs-disponibles-p (i-cours moment-cumule semaine)
  (let ((les-profs (profs-i-cours i-cours))
        (buffer t)
        (duree (duree-i-cours i-cours)) 
        (nb-seances (nb-seances-i-cours i-cours)))
    (while
      (and les-profs buffer)
      (setq buffer 
        (appartenir-rectangle-dispo-p 
          moment-cumule duree semaine nb-seances
          (dispo-i-prof (nextl les-profs)))))
    buffer))


;;; predicat <prof-non-deja-pris-p>
;;; DBG JH 25/11 : on verifie que le cours n'est pas instancie 
;;; avant de regarder les chevauchements

(defun profs-non-deja-pris-p (i-cours moment semaine)
  (let ((les-profs (profs-i-cours i-cours))
        (buffer t)
        (le-prof)
        (les-cours)
        (le-cours)
        (duree (duree-i-cours i-cours))
        (nb-seances (nb-seances-i-cours i-cours))) 
       (while 
         (and (setq le-prof (nextl les-profs)) buffer)
         (setq les-cours (cours-i-prof le-prof))
         (while 
           (and (setq le-cours (nextl les-cours)) buffer)     
           (and 
             (neq i-cours le-cours)
             (instant-trouve-i-cours le-cours)
             (setq buffer 
               (not 
                 (chevauchement-rectangles-p 
                   moment duree semaine nb-seances
                   (moment-instant-cumule 
                     (instant-interne2instant-cumule 
                       (instant-trouve-i-cours le-cours)))
                   (duree-i-cours le-cours)
                   (semaine-instant-cumule
                     (instant-interne2instant-cumule 
                       (instant-trouve-i-cours le-cours)))
                   (nb-seances-i-cours le-cours)))))))
       buffer))       



;;; predicat <groupes-i-cours-disponibles-p>
;;; MOD - FD - 18/03/96
;;; on verifiait que la dispo des sous-groupes des groupes auxquels appartient
;;; le cours etait suffisante !!!! (et pourquoi pas verifier que la dispo des
;;; autres cours est suffisante ...)
;;; Par contre, il faut verifier que la dispo des familles 'meres' d'une
;;; famille a laquelle appartient le cours est suffissante.

(defun groupes-i-cours-disponibles-p (i-cours moment-cumule semaine)
  (every
    (lambda (groupe)
      (groupe-i-cours-disponible-p groupe i-cours moment-cumule semaine))
    (groupes-i-cours i-cours)))

;;; <groupe-i-cours-disponible-p>
;;; CRE - FD - 18/03/96
;;; verifie si la dispo d'un groupe et des groupes aux quels appartient ce
;;; groupe est suffisante pour le cours si on place celui-ci a un moment
;;; donne d'une semaine donnee

(defun groupe-i-cours-disponible-p (groupe i-cours moment-cumule semaine)
  (and
    (appartenir-rectangle-dispo-p
      moment-cumule
      (duree-i-cours i-cours)
      semaine 
      (nb-seances-i-cours i-cours)
      (dispo-i-groupe groupe))
    (every
      (lambda (gr)
	(groupe-i-cours-disponible-p gr i-cours moment-cumule semaine))
      (sous-liste
	(lambda (gr)
	  (appartenir-liste groupe (groupes-i-groupe gr)))
	liste-i-groupe))))
	
;;; predicat <cours-disponible-p>

(defun cours-disponible-p (i-cours moment-cumule semaine)
  (appartenir-rectangle-dispo-p 
    moment-cumule
    (duree-i-cours i-cours)
    semaine
    (nb-seances-i-cours i-cours)
    (dispo-i-cours i-cours)))


;;; predicat <chevauchement-rectangles-p>
;;; rem (17/10/94) MO
;;; attention: l'ancien predicat de ce nom a ete rebaptise en chevauchement-rect-p

(defun chevauchement-rectangles-p (mom1 duree1 sem1 nbsem1 mom2 duree2 sem2 nbsem2)
  (and
    (< sem1 (+ sem2 nbsem2))
    (< sem2 (+ sem1 nbsem1))
    (< mom1 (+ mom2 duree2))
    (< mom2 (+ mom1 duree1))
    t))


;;; predicat <horaire-debut-possible-p>
;;; verifie que l'horaire de debut est compatible avec le type du cours
;;; (pas encore de semaine de debut pas type de cours, mais
;;; ca va tres prochainement arriver)

(defun horaire-debut-possible-p (i-cours moment-cumule)
  (let* ((type-cours (type-i-cours i-cours))
    (i-type-cours (when (boundp type-cours) (eval type-cours))))
      (member
        moment-cumule
        (liste-moments-cumules-i-type-cours i-type-cours))))



;;; predicat <moment-debut-possible-p>
;;; verifie que le moment et la semaine de debut sont compatibles
;;; avec la dispo de l'ecole


(defun moment-debut-possible-p (i-cours moment-cumule semaine)
  (appartenir-rectangle-dispo-p 
    moment-cumule
    (duree-i-cours i-cours)
    semaine
    (nb-seances-i-cours i-cours)
    Dispo-Ecole))

;;; Nouvel implementation du deplacement d'un rectangle prenant
;;; en compte les contraintes

;;; !!! ATTENTION PLEASE : ON UTILISE LE MOTEUR POUR LE DEPLACEMENT !!!
;;; Va falloir que ca change !!!

;;; CRE RM - 17/06/96 : creation des donnees moteur a partir des objets
;;; interface

;;; CRE RM - 12/06/97 : prise en compte des ajouts de seances

(defun creer-objets-moteur (cours instant)
  (setq RESOLUTION-CONTRAINTES-MAX t)		; toutes les contraintes
  (setq Liste-Rect ())     
  (setq Liste-M-Contraintes ())     
  (setq Liste-M-Groupes ())
  (setq Liste-M-Cours-Ajouts ())                ; CRE RM 12/06/97
  (setq Liste-M-Prof-Ajouts ())                 ; CRE RM 12/06/97
  (creer-rectangles-depl cours instant)
  (creer-rectangles-ajout cours instant)
  (creer-profs-depl)
  (creer-groupes-depl)
  (creer-contraintes-depl))
  
;; Creation des rectangles a partir des cours appartenant a LISTE-I-COURS

(defun creer-rectangles-depl (cours-deplace instant)
  (ifn cours-deplace
    (mapc
      (lambda (cours)
	(creer-rect
	  (prefixer-code-moteur (code-i-cours cours))
	  (duree-i-cours cours) (nb-seances-i-cours cours)
	  (fige-i-cours cours)
	  (intersection-dispo (dispo-i-cours cours) dispo-ecole)
	  (numero-dans-liste (type-i-cours cours) liste-codes-types-cours)
	  (if (fige-i-cours cours) (instant-trouve-i-cours cours) ())))
      Liste-i-Cours)
    (mapc
      (lambda (cours)
	(if (equal cours cours-deplace)
	  (creer-rect
	    (prefixer-code-moteur (code-i-cours cours))
	    (duree-i-cours cours) (nb-seances-i-cours cours)
	    ()
	    (intersection-dispo
	      (construire-dispo cours-deplace instant)
	      (dispo-i-cours cours))
	    (numero-dans-liste (type-i-cours cours) liste-codes-types-cours)
	    instant)
	  (creer-rect
	    (prefixer-code-moteur (code-i-cours cours))
	    (duree-i-cours cours)
	    (nb-seances-i-cours cours)
	    (if (instant-trouve-i-cours cours) t ())
	    (intersection-dispo (dispo-i-cours cours) dispo-ecole)
	    (numero-dans-liste (type-i-cours cours) liste-codes-types-cours)
	    (instant-trouve-i-cours cours))))
      liste-i-cours)))

(defun creer-rectangles-ajout (cours-deplace instant)
  (mapc
    (lambda (ajt)
      (creer-rect
	(prefixer-code-moteur (code-i-ajout ajt))
	(duree-i-ajout ajt) 1
	t
	(construire-dispo-ajout (instant-i-ajout ajt) (duree-i-ajout ajt))
	(numero-dans-liste
	  (type-i-cours (cours-rattrape-i-ajout ajt))
	  liste-codes-types-cours)
	(instant-i-ajout ajt))
      (newl Liste-M-Cours-Ajouts
	(cons
	  (prefixer-code-moteur (code-i-cours (cours-rattrape-i-ajout ajt)))
	  (prefixer-code-moteur (code-i-ajout ajt))))
      (mapc
	(lambda (p)
	  (newl Liste-M-Prof-Ajouts
	    (cons p (prefixer-code-moteur (code-i-ajout ajt)))))
	(profs-i-ajout ajt)))
    Liste-i-Ajout))

(defun construire-dispo (cours instant)
  (let
    ((moment
      (moment-instant-cumule (instant-interne2instant-cumule instant)))
     (semaine
      (semaine-instant-cumule (instant-interne2instant-cumule instant))))
    (cons-dispo
      (cons (cons moment (+ moment (duree-i-cours cours))) ())
      (cons (cons semaine (+ semaine (nb-seances-i-cours cours))) ()))))

(defun construire-dispo-ajout (instant duree)
  (let
    ((moment
      (moment-instant-cumule (instant-interne2instant-cumule instant)))
     (semaine
      (semaine-instant-cumule (instant-interne2instant-cumule instant))))
    (cons-dispo
      (cons (cons moment (+ moment duree)) ())
      (cons (cons semaine (+ semaine 1)) ()))))

;;; Mise a jour des dispo des rects en fonction des dispo des profs

(defun creer-profs-depl ()
  (mapc
    (lambda (pr)
      (mapc
	(lambda (c)
	  (rajouter-dispo-rect (dispo-i-prof pr) c))
	(evlis1
	  (mapcar
	    (lambda (x)
	      (prefixer-code-moteur (code-i-cours x)))
	    (cours-i-prof pr))))
      (newl Liste-m-Contraintes 
	(cons-m-contrainte
	  'poser-non-chevauchement
	  (liste-rects-cours
	    (mapcar 'code-i-cours (cours-i-prof pr))))))
    liste-i-prof))

;;; Creation des groupes moteur

(defun creer-groupes-depl ()
  (mapc
    (lambda (gr)
      (set 
	(prefixer-code-moteur (code-i-groupe gr)) 
	(cons-m-groupe 
	  (contraintes-i-groupe gr) 
	  (mapcar
	    (lambda (x)
	      (prefixer-code-moteur (code-i-groupe x)))
	    (groupes-i-groupe gr))
	  (evlis1
	    (liste-rects-cours
	      (mapcar 'code-i-cours (cours-i-groupe gr))))
	  (dispo-i-groupe gr)
	  'pas-vu
	  (nb-td-i-groupe gr)))
      (if (not (prefixer-code-moteur (code-i-groupe gr))) 
	(cons-erreur 'ERRGRPNOFILS (code-i-groupe gr))
	(newl Liste-m-Groupes (prefixer-code-moteur (code-i-groupe gr)))))
    liste-i-groupe))

;;; Prise en compte des contraintes explicites pour la pose de contrainte

(defun creer-contraintes-depl ()
  (mapc
    (lambda (contr)
      (newl Liste-m-Contraintes 
	(cons-m-contrainte 
	  (car contr)
	  (liste-rects-cours (cdr contr)))))
    liste-contraintes))

(defun poser-contraintes (rect-deplace)
  (let
    ((no-echec t))
    (setq compter-contraintes-p ())
    (initialiser-listes-rects)
    (liberer-memoire-rects-passifs)
    (initialisation)
    (ifn (ct-domain-p (position-rect rect-deplace))
      (progn
	(cons-erreur 'err-type-cours-depl rect-deplace)
	(setq no-echec ()))
      (ct-set-fail-hook 'echec-pose-contrainte)
      (tag pose-contr (contraindre)))
    (liberer-memoire-m-objets)
    (liberer-memoire-moteur)
    no-echec))

;;; Le deplacement complet

(defun deplacement-possible (cours instant)
  (let
    ((salle (salle-trouvee-i-cours cours))
     (liste-erreurs ())
     (continuerp t)
     (rect-deplace)
     (liste-codes-cours-a-resoudre
       (mapcar 'code-i-cours (sous-liste i-instanciep Liste-I-Cours))))
    (creer-objets-moteur cours instant)
    (setq rect-deplace (eval (prefixer-code-moteur (code-i-cours cours))))
    ;; MOD - FD - 30/08/96 ne prendre en compte que les cours instancies
    ; (initialiser-rects-a-resoudre ()) plus celui que nous placons ou
    ;; deplacons
    (when
      (not
	(appartenir-liste (code-i-cours cours) liste-codes-cours-a-resoudre))
      (setq liste-codes-cours-a-resoudre
	(cons (code-i-cours cours) liste-codes-cours-a-resoudre)))
    (initialiser-rects-a-resoudre liste-codes-cours-a-resoudre)
      
    (propagation-dispo-groupes)
    (setq *erreurs* ())	 ;;  Ne pas prendre en compte les erreurs globales
                         ;; ne traiter que le cours que nous deplacons
    (ifn
      (dispo-cours-suffisante-p 
	(dispo-rect rect-deplace)
	cours)
      (cons-erreur 'errdispo-insuffi cours))

    (when *erreurs*
      (traiter-erreur)
      (setq continuerp
	(prompt-continuer
	  (column
	    #Mv_continuer-deplacement
	    #Mv_lever-contrainte-dispo)))
      ;; MOD - FD - 30/08/96
      ;; Il faudrait verifier que la dispo restante est suffisante
      (when continuerp
	(dispo-rect
	  ; 	  (eval (prefixer-code-moteur (code-i-cours cours)))
	  rect-deplace
	  (construire-dispo cours instant))))

    (ifn continuerp
      ()
      (when (not (poser-contraintes rect-deplace))
	(traiter-erreur)
	(setq continuerp ()))
      (when *erreurs*
	(traiter-erreur)
	(setq continuerp
	  (prompt-continuer
	  (column
	    #Mv_continuer-deplacement
	    #Mv_lever-contraintes))))
      (ifn continuerp
	()
	(setq salle (choisir-salle cours instant salle))
	(ifn liste-erreurs
	  salle
	  (cons-erreur 'errDepl (cons cours liste-erreurs))
	  (traiter-erreur)
	  t)))))

(defun choisir-salle (cours instant salle)
  (if (not MODULE-SALLES)
    t
    (let
      ((moment
	(moment-instant-cumule (instant-interne2instant-cumule instant)))
      (semaine
	(semaine-instant-cumule (instant-interne2instant-cumule instant))))
      (choisir-salle-disponible cours moment semaine salle))))

;;; CRE RM - 06/12/96 : propagation des dispo en mode saisie
;;; AJT RM - 12/02/98 : detection de cycle

(defun analyse-propagation-dispo (a)
  (let
    ((cycle (graphe-cyclique)))
    (change-curseur a Varuna-Curseur-Sablier)
    (if cycle
      (progn
	(change-curseur a Varuna-Curseur-Fleche)  
	(erreur-graphe-cyclique cycle))
      (creer-objets-moteur () ())
      (initialiser-rects-a-resoudre ())
      (propagation-dispo-groupes)
      (liberer-memoire-m-objets)
      (liberer-memoire-moteur)
      (change-curseur a Varuna-Curseur-Fleche)  
      (if *erreurs*
	(traiter-erreur)
	(afficher-information #Mv_analyse-dispo-ok)))))

;;; CRE RM - 06/12/96 : analyse des contraintes
;;; AJT RM - 12/02/98 : detection de cycle

(defun analyse-pose-contraintes (a)
  (let
    ((cycle (graphe-cyclique)))
    (change-curseur a Varuna-Curseur-Sablier)
    (if cycle
      (progn
	(change-curseur a Varuna-Curseur-Fleche)  
	(erreur-graphe-cyclique cycle))
      (creer-objets-moteur () ())
      (initialiser-rects-a-resoudre ())
      (propagation-dispo-groupes)
      (when (not *erreurs*)
	(initialiser-listes-rects)
	(liberer-memoire-rects-passifs)
	(initialisation)
	(ct-set-fail-hook 'echec-pose-contrainte)
	(tag pose-contr  (contraindre))
	(liberer-memoire-m-objets)
	(liberer-memoire-moteur))
      (change-curseur a Varuna-Curseur-Fleche)
      (if *erreurs*
	(traiter-erreur)
	(afficher-information #Mv_analyse-contr-ok)))))
    

;;; CRE RM : Nouvel algo pour le deplacement d'un cours
;;; sans l'utilisation de PECOS !

;;; Fonction qui remplace (deplacement-possible cours instant)

(defun nouveau-deplacement-possible (cours instant)
  (let
    ((dispo)
     (dispo-tmp)
     (type-cours (type-i-cours cours))
     (moment)
     (salle (salle-trouvee-i-cours cours))
     (liste-erreurs ())
     (erreur-dispo ())
     (continuerp t))

    ;; On test les disponibilites

    (setq dispo (construire-dispo cours instant))
    (setq dispo-tmp (intersection-dispo dispo (reduire-dispo-cours cours)))
    (when (not (dispo-cours-suffisante-p dispo-tmp cours))
      (afficher-attention
	"La disponibilité du cours et de ses familles n'est pas respectée.")
      (setq erreur-dispo t))

    (setq dispo-tmp dispo)
    (mapc
      (lambda (pr)
	(setq dispo-tmp
	  (intersection-dispo dispo-tmp (dispo-i-prof pr))))
      (profs-i-cours cours))
    (when (not (dispo-cours-suffisante-p dispo-tmp cours))
      (afficher-attention
	"La disponibilité de l'enseignant n'est pas respectée.")
      (setq erreur-dispo t))     
    
    (when erreur-dispo
      (setq continuerp
	(prompt-continuer
	  (column
	    #Mv_continuer-deplacement
	    #Mv_lever-contrainte-dispo))))
    
    (ifn continuerp
      ()
    
      ;; On test les contraintes
      (setq dispo-tmp
	(intersection-dispo
	  dispo
	  (reduction-par-contraintes
	    dispo (semaine-instant instant) (duree-i-cours cours)
	    (nb-seances-i-cours cours)
	    cours
	    '(poser-non-chevauchement poser-meme-semaine poser-jours-differents
	      poser-meme-jour poser-simultaneite poser-simultaneite-fin-seance
	      poser-meme-horaire poser-precedence poser-proximite-semaine)
	    t)))

      (when lst-err-contraintes
	(erreur-contraintes-non-respectees cours)
	(setq continuerp ())
	(setq continuerp
	  (prompt-continuer
	    (column
	      #Mv_continuer-deplacement
	      #Mv_lever-contraintes))))
     
      (ifn continuerp
	()
	;; On affecte une salle
	  
	(setq salle (choisir-salle cours instant salle))
	(ifn liste-erreurs
	  salle
	  (cons-erreur 'errDepl (cons cours liste-erreurs))
	  (traiter-erreur)
	  t)))))

(defun instant-type-cours-valide-p (instant type-cours)
  (let
    ((mc (moment-instant-cumule (instant-interne2instant-cumule instant))))
    (and (member mc (liste-moments-cumules-i-type-cours type-cours)) t)))

(defun dispo-ecole-suffisante-p (dispo cours)
  (dispo-cours-suffisante-p
    (intersection-dispo dispo dispo-ecole)
    cours))


;;; On test la validite de l'horaire / type de cours
;;; et que l'instant est correct avec les heures de l'etablissement

(defun choisir-instant-deplacement (cours instant)
  (let
    ((dispo (construire-dispo cours instant))
     (sem (semaine-instant instant)) 
     (type-cours (type-i-cours cours))
     (continuerp t))

    (while
      (and
	continuerp
	(or
	  (not (instant-type-cours-valide-p instant type-cours))
	  (not (dispo-ecole-suffisante-p dispo cours))))
      
      (afficher-attention
	(column
	  "Cet horaire ne correspond pas au type du cours"
	  "ou ne respecte pas les horaires de l'établissement."))
      (setq moment (demande-moment-cumule type-cours))
      (ifn moment
	(setq continuerp ())
	(setq instant
	  (instant-cumule2instant-interne
	    (cons-instant-cumule sem moment)))
	(setq dispo (construire-dispo cours instant))))
    (if continuerp
      instant
      ())))

