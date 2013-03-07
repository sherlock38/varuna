;;; ---------------------------------------------------------------------------
;;; - i-uresa.ll                                              VARUNA 1.2      -
;;; - Utilitaires sur les libertes de salles                                  -
;;; -                                                                         -
;;; ---------------------------------------------------------------------------
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; - Creation:  Avril 95                                                     -
;;; - Build 1.22.1 01.07.99 / MO                                              -
;;; ---------------------------------------------------------------------------

;;; AJT FD-26/04/95
;;; traitement des erreurs: ex. incompatibilite entre l'audience et la salle
;;; choisie
;;; Tri alphabetique de la liste des salles (cf. <maj-vecteur-salles>)

;;; MOD FD-26/04/95
;;; on passe en argument de <salles-libres> un objet de type <i-type-cours> et ;;; non un entier

;;; CRE (13/04/95) MO
;;; ce module reunit toutes les primitives permettant de faire une recherche
;;; de salle, du point unique d'entree: salles-libres .... aux petites
;;; primtives de calcul obscures
;;;
;;; plus tard.... bien plus tard... alors que le recette sera passe' par 
;;; la`, peut-e^tre dispatcherons-nous ces primitives dans les fichiers
;;; les plus aproprie's: uvaruna, i-util, dispo,.... a voir 



;;; <salles-libres>
;;; salles libres est le point d'entree d'une recherche de liberte
;;; sur une salle, ou un type pour une semaine, un jour et un certain
;;; nombre d'options
;;; cette primitive a une forme procedurale... elle est largement commentee
;;; toutes les sous-primitives sont dans ce module, a l'exception de
;;; garder-intervalles-zone, qui est une (nouvelle) primitive de zones.ll

(defun salles-libres 
  (semaine jour duree                    ; arguments obligatoires  
   salle type-salle audience             ; facultatifs salle
   horaire-debut horaire-fin type-cours  ; facultatifs creneaux
   dispo nb-semaines)

  (let
    ((vecteur-salles)
     (vecteur-arguments))

    ;;; a ce moment de la partie on a un vecteur de salles qui vaut t
    ;;; si il n'y a pas de salles de cette audience ou le vecteur de salles

    (tag echec-erreur
        
      ;;; mise a jour du vecteur des salles
     (setq vecteur-salles
       (maj-vecteur-salles salle type-salle audience))
     (when (eq vecteur-salles t) (exit echec-erreur t))

     ;; algo de calcul des horaires pris pour chacune des salles

     (setq vecteur-arguments
       (maj-moments-pris vecteur-salles semaine jour nb-semaines))

;;; FD / MO (13/09/95)
;;; pour le moment, il nous semble que :
;;; 1. creneaux pris pour les salles interessantes
;;; 2. inversion creneaux libres sous forme de dispo et non plus
;;; dispo sa aucun type de cours et liste si type de cours
;;; 3. prise en compte d'un dispo de cours (important pour le rattrapage)
;;; 4. transformation en dispo ou liste d'horaires de debut selon le type
;;; de cours
;;; tout doit pouvoir etre fait en meme temps en donnant une intersection
;;; de dispo au moment de l'inversion pris->libre
;;; !!! a retravailler FD + MO du genre träs bientot ... !!!
     
     
     ;;; puis, on sort les crenaux possibles pour toutes les salles
     (setq vecteur-arguments 
       (maj-moments-libres
           vecteur-arguments 
	   ()	;; type-cours MOD - 28/08/95 le type de cours n'est pris en
	        ;; compte qu'a la fin 
           semaine jour
           horaire-debut horaire-fin duree))

     ;; AJT - FD - 01/08/95
     ;; pour tenir compte d'une dispo
     (setq vecteur-arguments
           (maj-moments-libres-avec-dispo 
               vecteur-arguments 
               dispo 
               type-cours semaine jour))

     ;;; enfin, on sort les deux listes resultantes

     (let 
       ((liste1)(liste2)(liste3)(i 0))
       (for 
         (i 0 1 (sub1 (vlength vecteur-salles)) ())
         (when 
           (vref vecteur-arguments i)
           (newl liste1 (vref vecteur-salles i))
           (newl liste2 (capacite-i-salle (vref vecteur-salles i)))
           (newl liste3 (vref vecteur-arguments i))))
       (list (reverse liste1) (reverse liste2) (reverse liste3))))))


        


 
;;; <maj-moments-moments-libres>
;;; cette primitive prend un vecteur de moments pris pour un jour et 
;;; une semaine et rend un vecteur de moments libres sachant une duree
;;; un eventuel type de cours et un eventuel creneau: horaire-debut et
;;; horaire-fin
;;; MOD - FD - 04/08/95
;;; calcule de borne-inf et borne-sup

(defun maj-moments-libres
  (vecteur-moments-pris type-cours  semaine jour
   horaire-debut horaire-fin duree)
  (let
    ((vecteur-resultat)(zone-travail)(n (vlength vecteur-moments-pris))
     (dispo-jour-ecole (premier-element (dispo-ecole-ce-jour semaine jour)))
     (borne-inf)
     (borne-sup)
     (zone-dispo)
     (i 0 ))

    (ifn horaire-debut
      (setq borne-inf (borne-inf-intervalle dispo-jour-ecole))
      (setq borne-inf
            (max horaire-debut (borne-inf-intervalle dispo-jour-ecole))))
    (ifn horaire-fin
      (setq borne-sup (borne-sup-intervalle dispo-jour-ecole))
      (setq borne-sup
            (min horaire-fin (borne-sup-intervalle dispo-jour-ecole))))
    (setq zone-dispo (creer-zone (creer-intervalle borne-inf borne-sup)))

    (setq vecteur-resultat (makevector n ()))
    (for 
      (i 0 1 (sub1 n) ())
      (setq zone-travail
        (difference-zone
          zone-dispo  
          (vref vecteur-moments-pris i)))
      (vset 
        vecteur-resultat 
        i 
       (liberte-sachant-duree type-cours zone-travail semaine jour duree)))
    vecteur-resultat))

;;; <liberte-sachant-duree>
;;; donne les creneaux de liberte ou la liste des horaires de debut

(defun liberte-sachant-duree (type-cours zone-horaire semaine jour duree)
  (or
    ;; si il y a un type de cours, on renvoie la liste des hc possibles  
    (and   
      type-cours
      (horaires-debut-possibles type-cours semaine jour zone-horaire duree))
    ;; sinon on ne garde que les intervalles depassant la duree
    (garder-intervalles-zone 
        zone-horaire 
        (lambda (i) (>= (longueur-intervalle i) duree)))))


;;; <horaires-debut-possibles>
;;; donne les horaires de debut possible sachant la duree et une zone 
;;; horaire

(defun horaires-debut-possibles 
  (type-cours semaine jour zone-horaire duree)
  (sous-liste
    (lambda (h)
      (intervalle-inclus-zone-p    
          (creer-intervalle h (add h duree))
          zone-horaire))
    (liste-horaires-debut-cours type-cours semaine jour)))


;;; <liste-horaires-debut-cours>
;;; donne les horaires d'un type de cours a un jour et une semaine precis
;;; pour le moment, on n'a rien a faire des semaines

(defun liste-horaires-debut-cours (type-cours semaine jour)
  (let 
    (;;; FD-27/04/95 (liste (moments-debut type-cours))
     (liste (liste-moments-cumules-i-type-cours type-cours))
     (liste-resultat)
     (tampon))
    (while liste
      (setq tampon (nextl liste))
      (when 
        (eq jour (jour-moment-cumule tampon))
        (newl liste-resultat (horaire-moment-cumule tampon))))
    (reverse liste-resultat)))


;;; <dispo-ecole-ce-jours>
;;; donne la dispo de l'ecole pour une journee precise
;;; semaine ne sert a rien pour le moment vue la dispo de l'ecole

(defun dispo-ecole-ce-jour (semaine jour)
  (let
    ((dispo-moment (trouver-dispo-moment DISPO-ECOLE))
     (dispo-jour)
     (intervalle-tampon))
    (while dispo-moment
      (setq intervalle-tampon (premier-intervalle-zone dispo-moment))
      (setq dispo-moment (reste-zone dispo-moment))  
      (when 
        (eq (jour-moment-cumule (borne-inf-intervalle intervalle-tampon)) jour)   
        (setq dispo-jour  
          (ajouter-intervalle-zone 
            (creer-intervalle
              (horaire-moment-cumule (borne-inf-intervalle intervalle-tampon))
              (horaire-moment-cumule (borne-sup-intervalle intervalle-tampon)))
            dispo-jour ))))
    dispo-jour))


;;; <maj-moments-pris>
;;; cette primitive rend le vecteur des moments pris par salle

(defun maj-moments-pris (vecteur-salle semaine jour nb-semaines)
  (let
    ((vecteur-resultat (makevector (vlength vecteur-salle) ()))
     (tampon)
     (i 0))

    (for (i semaine 1 (sub1 (+ semaine nb-semaines)) ())

      (mapc
	(lambda (cours)
	  (and
	    (setq tampon 
	      (appartenir-vecteur (salle-trouvee-i-cours cours) vecteur-salle)) 
	    (cours-ce-jour-p cours i jour)  
	    (let*
	      ((horaire-debut-cours 
		(interne2cumule (horaire-instant (instant-trouve-i-cours cours))))
	      (horaire-fin-cours (add horaire-debut-cours (duree-i-cours cours))))
	      (vset 
		vecteur-resultat 
		tampon 
		(union-zone 
		  (vref vecteur-resultat tampon) 
		  (creer-zone 
		    (creer-intervalle 
		      horaire-debut-cours 
		      horaire-fin-cours)))))))
	Liste-I-Cours)  

      (mapc
	(lambda (resa)
	  (and
	    (setq tampon 
	      (appartenir-vecteur (salle-i-resa resa) vecteur-salle)) 
	    (resa-ce-jour-p resa i jour)  
	    (let*
	      ((horaire-debut-resa 
		(interne2cumule (horaire-instant (instant-i-resa resa))))
	      (horaire-fin-resa (add horaire-debut-resa (duree-i-resa resa))))
	      (vset vecteur-resultat 
		tampon 
		(union-zone 
		  (vref vecteur-resultat tampon) 
		  (creer-zone 
		    (creer-intervalle 
		      horaire-debut-resa 
		      horaire-fin-resa)))))))
	liste-i-resa)

      (mapc
	(lambda (ajout)
	  (and
	    (setq tampon 
	      (appartenir-vecteur (salle-i-ajout ajout) vecteur-salle)) 
	    (ajout-ce-jour-p ajout i jour)  
	    (let*
	      ((horaire-debut-ajout 
		(interne2cumule (horaire-instant (instant-i-ajout ajout))))
	      (horaire-fin-ajout (add horaire-debut-ajout (duree-i-ajout ajout))))
	      (vset 
		vecteur-resultat 
		tampon 
		(union-zone 
		  (vref vecteur-resultat tampon) 
		  (creer-zone 
		    (creer-intervalle 
		      horaire-debut-ajout 
		      horaire-fin-ajout)))))))
	liste-i-ajout)

      (mapc
	(lambda (annul)
	  (and
	    (setq tampon
	      (appartenir-vecteur (salle-annulation annul) vecteur-salle))
	    (annulation-ce-jour-p annul i jour)
	    (let*
	      ((horaire-debut-annulation
		(interne2cumule (horaire-instant (instant-i-annulation annul))))
	      (horaire-fin-annulation
		(add horaire-debut-annulation (duree-i-annulation annul))))
	      (vset
		vecteur-resultat
		tampon
		(difference-zone
		  (vref vecteur-resultat tampon)
		  (creer-zone
		    (creer-intervalle
		      horaire-debut-annulation
		      horaire-fin-annulation)))))))
	Liste-I-Annulation))

    vecteur-resultat))


;;; <cours-ce-jour-p>
;;; cette primitive rend t si le cours a lieu le jour de cette semaine

(defun cours-ce-jour-p (cours semaine jour)
  (let 
    ((instant (instant-trouve-i-cours cours)))
    (and
      instant
      (eq jour (jour-instant instant))
      (ge semaine (semaine-instant instant))
      ;;; FD-19/04/95 lt et non le
      (lt semaine (add (semaine-instant instant) (nb-seances-i-cours cours)))
      t)))
  

;;; <resa-ce-jour-p>
;;; cette primitive rend t si la resa a lieu le jour de cette semaine

(defun resa-ce-jour-p (resa semaine jour)
  (let 
    ((instant (instant-i-resa resa)))
    (and
      instant
      (eq jour (jour-instant instant))
      (eq semaine (semaine-instant instant))
      t)))

;;; <ajout-ce-jour-p>
;;; CRE - FD - 31/07/95
;;; cette primitive rend t si l'ajout a lieu le jour de cette semaine

(defun ajout-ce-jour-p (ajout semaine jour)
  (let 
    ((instant (instant-i-ajout ajout)))
    (and
      instant
      (eq jour (jour-instant instant))
      (eq semaine (semaine-instant instant))
      t)))

;;; <annulation-ce-jour-p>
;;; CRE - FD - 31/07/95
;;; cette primitive rend t si l'annulation a lieu le jour de cette semaine

(defun annulation-ce-jour-p (annul semaine jour)
  (let 
    ((instant (instant-i-annulation annul)))
    (and
      instant
      (eq jour (jour-instant instant))
      (eq semaine (semaine-instant instant))
      t)))

;;; <maj-vecteur-salles>
;;; cette primitive rend un vecteur des salles possibles 

(defun maj-vecteur-salles (salle type-salle audience)
  ;; si pas d'audience alors audience nulle 
  (ifn audience (setq audience 0))

  (or      
    ;; si la demande concerne qu'une seule salle  

    ;; AJT FD-26/04/95 - traitement erreur incompatibilite type de
    ;; salle et salle choisie
    (and
      (and salle type-salle)
      (if (eq (type-i-salle salle) type-salle)
	()
	;; type-salle et salle choisis incompatibles
	(cons-erreur 'err-Type-Salle 
	  (list (libelle-i-salle salle)
	    type-salle))
	t))
    (and salle 
        (if 
          (ge (capacite-i-salle salle) audience)
          (makevector 1 salle)
          (cons-erreur 'err-Capa-Aud 
              (list (libelle-i-salle salle)
                  audience))
          t))
        
    ;; si la demande concerne plusieurs salles d'un meme type ou non 
    (let
      ((liste) 
       (liste-resultat)
       (tampon))
      ;;; FD ajout du tri
      ;;; rq un tri sur la capacite des salles ne serait-t il pas prefereable
      ;;        (setq liste (copy Liste-I-Salle))
      ;;  (setq liste (sort 'precede-alpha-p liste))
      (setq liste Liste-I-Salle)
      
      (while liste
        (setq tampon (nextl liste))
        (and 
          (if type-salle (eq (type-i-salle tampon) type-salle) t)
          (ge (capacite-i-salle tampon) audience)  
          (newl liste-resultat tampon)))

      (if liste-resultat 			;; MOD - FD - 21/06/96
	(progn
	  (setq liste-resultat
	    (sous-liste salle-site-p liste-resultat))
	  (setq tampon (apply 'vector liste-resultat)))
	  
        (cons-erreur 'err-Capa-Aud (list type-salle audience))
        (setq tampon t))

      ;; MOD - 14/05/96 - filtrer sur les types autorise pour le site
      ;; deplacmnt du bloc suivant apres le test precedent

      ;       (setq liste-resultat
      ; 	(sous-liste
         ; 	  (lambda (s) (type-salle-autorise-p (type-i-salle s)))
      ; 	  salle-site-p 				;; MOD - FD - 18/06/96
      ;        liste-resultat))
      
      (ifn liste-resultat
	t
	tampon)
      )))

;;; <appartenir-vecteur> donne la premiere position du vecteur contenant
;;; element

(defun appartenir-vecteur (element vecteur)
  (let
    ((n (vlength vecteur))
     (resultat)
     (compteur 0))
    (until
      (or resultat (ge compteur n))  
      (if
        (eq element (vref vecteur compteur))
        (setq resultat compteur)
        (setq compteur (add1 compteur))))
    resultat))

;;; <maj-moments-libres-avec-dispo>
;;; CRE - FD - 01/08/95
;;; reduit le vecteur de moments en fonction de la dispo passee en parametre
;;; FD - MOD - 17/08/95
;;; ajt de <liberte-sachant-duree> pour tenir compte de la duree de la
;;; resa ou de l'ajout

(defun maj-moments-libres-avec-dispo (vecteur-moments dispo type-cours semaine jour)
  (let 
    ((tampon 0)
     (vecteur-resultat)
     (long (vlength vecteur-moments))
     (dispo-jour (dispo-horaire-ce-jour dispo semaine jour))
     (zone-travail))

    (setq vecteur-resultat (makevector long ()))
    (mapvector
      (lambda (moments-salle)
        (setq 
          zone-travail
          (intersection-zone moments-salle dispo-jour))
        (vset
            vecteur-resultat
            tampon
            (liberte-sachant-duree 
                type-cours 
                zone-travail
                semaine
                jour
                duree))
        (setq tampon (add1 tampon)))
      vecteur-moments)
    vecteur-resultat))

;;; <dispo-moment-ce-jour>
;;; CRE - FD - 01/08/95
;;; donne la dispo pour une journee precise a partir d'une dispo
;;; semaine ne sert a rien pour le moment vue la dispo de l'ecole
;;; rq peut remplacer <dispo-ecole-ce-jour>
;;; on suppose que dispo-ecole est un ensemble de d'intervalle (aucun de ces
;;; intervalles n'etant a 'cheval' sur des jours differents
 (defun dispo-moment-ce-jour (dispo semaine jour)
   (let
     ((dispo-moment
       (trouver-dispo-moment
	 (intersection-dispo dispo (dispo-ecole-i-ecole I-ECOLE)))))
     (sous-liste
       (lambda (intervalle)
	 (let
	   ((jour-debut (jour-moment-cumule (borne-inf-intervalle intervalle))))
 	(meme-jour-p jour jour-debut)))
       dispo-moment)))

;;; <dispo-moment-jour2dispo-horaire-jour>
;;; transforme une dispo de moments cumules d'un jour en une dispo d'horaires
;;; pour ce jour
 (defun dispo-moment-jour2dispo-horaire-jour (dispo-moment)
   (mapcar
     (lambda (intervalle)
       (creer-intervalle
	 (horaire-moment-cumule (borne-inf-intervalle intervalle))
	 (horaire-moment-cumule (borne-sup-intervalle intervalle))))
     dispo-moment))
 
(defun dispo-horaire-ce-jour (dispo semaine jour)
  (dispo-moment-jour2dispo-horaire-jour (dispo-moment-ce-jour dispo semaine jour)))

;;; <horaire-type-cours-p>
;;; CRE - FD - 02/08/95
;;; verifie qu'un horaire est compatible avec un type de cours
(defun horaire-type-cours-p (moment-cumule)
  (let ((indice 0)
        (long (length liste-i-type-cours)))
  (until
      (or
          (eq indice long )
          (member
             moment-cumule
             (liste-moments-cumules-i-type-cours (nth indice liste-i-type-cours))))

      (setq indice (add1 indice)))
  (neq indice long)))

