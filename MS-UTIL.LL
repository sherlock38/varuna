;;; --------------------------------------------------------------------------
;;; - ms-utilit.ll                                             VARUNA 1.2    -
;;; - Moteur des salles                                                      -
;;; - Utilitaires spécifiques a la gestion des salles                        -
;;; --------------------------------------------------------------------------
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; - Creation: 93 - 94                                        PR JH MO OJ   -
;;; - Derniere mise a jour pour version 1.13::  11/04/95 MO                  -
;;; - Build 1.22.1 01.07.99 / JH                                             -
;;; --------------------------------------------------------------------------


;;; trv (26/05/95) mo: travail sur bug mo120501
;;; <choix-salle>

;;; CRE (11/04/95) MO
;;; creation d'une primitive <maj-salles-reservees> et de sa compagne
;;; <maj-salles-interdites-reservees> en fin de fichier
;;; l'ecriture de cette primitive m'a permis de trouver une betise
;;; dans <maj-salles-interdites>: on pouvait avoir deux fois la meme salle
;;; cela n'etait pas teste a l'ajout d'une salle interdite
;;; a recette largement, comme d'habitude !!!


;;; 1. UTILITAIRES DE <CALCULER-SALLES> et de <MOTEUR-SALLE>
;;;


;;; Teste si une salle a une capacite superieure a une autre

(defun plus-grande-capacite-p (salle1 salle2)
  (ge (capacite-salle salle1) (capacite-salle salle2)))


;;; trv (26/05/95) mo: travail sur bug mo120501
;;; <choix-salle>
;;; Choix d'une salle

;;; MOD RM - 04/10/95 : On ‚limine la liste lsauf (gain de listes !!!)

(defun choix-salle (cours)
  (let
    ((s1 (premiere-salle Liste-Salle cours)))
    (when s1
      (if (< (capacite-salle (eval s1)) (audience-cours cours))
	()
	(choix-salle-iter 
	  s1
	  (cdr (member s1 Liste-Salle))
	  (audience-cours cours)
	  cours)))))

;;; Fct iteree

(defun choix-salle-iter (salle liste-salles-restantes audience cours)
  (if (null liste-salles-restantes)
    salle
    (let
      ((nouvelle-salle (premiere-salle liste-salles-restantes cours)))
      (if
	(or
	  (null nouvelle-salle)
	  (< (capacite-salle (eval nouvelle-salle)) audience))
	salle
	(choix-salle-iter
	  nouvelle-salle
	  (cdr (member nouvelle-salle liste-salles-restantes))
	  audience
	  cours)))))


;;; Premiere salle
;;; cette primitive rend la premiere salle de l1 qui n'appartient pas a lmoins

(defun premiere-salle (liste-salles cours)
  (let
    ((salle (nextl liste-salles)))
    (while
      (and
	salle
	(or 
	  (appartenir-liste (code-salle salle)(interdites-cours cours))
	  (not (salle-bon-type-p salle cours))))
      (setq salle (nextl liste-salles)))
    salle))

;;; CRE RM - 04/10/95 : pr‚dicat sur le type d'une salle d'un cours

(defun salle-bon-type-p (salle cours)
  (eq
    (type-salle salle)
    (type-salle-cours cours)))


(defun liste-salle-triee (type . capacite)
  (sous-liste
    (lambda (salle)
      (and (eq (type-salle salle) type)
	(>= (capacite-salle salle) (or (car capacite) 0))))
    Liste-Salle))


;;; construit une erreur avec les cours sans salles
;;; si il y a deja une erreur, ne fait rien.
;; a remplacer avec des sous-liste

;;; MOD RM - 04/10/95 : remplacement par un sous-liste
;;; Fonction appel‚e par personne !!!

(defun cours-sans-salle ()
  (when (not *erreurs*)
    (let 
      ((css
	(sous-liste
	  (lambda (cours)
	    (and
	      (ms-instant-trouve-p cours)
	      (not (ms-salle-trouvee-p cours))))
	  liste-cours)))
      (when css
	(cons-erreur 'errNoRoom css)))))

;;; CRE RM - 04/10/95 : pr‚dicats sur l'instanciation et la l'affectation
;;; d'une salle pour un cours

(defun ms-instant-trouve-p (cours)
  (and (instant-trouve-cours cours) t))
    
(defun ms-salle-trouvee-p (cours)
  (and (salle-trouvee-cours cours) t))

;;; CRE RM - 04/10/95 : fonction appel‚e dans la focntion <MOTEUR-SALLE>

(defun affecter-salles-aux-cours ()
  (mapc
    (lambda (cours)
      (salle-trouvee-cours
	cours
	(de-prefixer-code-ms
	  (salle-trouvee-cours cours))))
    liste-cours)
  t)

;;; 2. UTILITAIRES A LA LECTURE DE FICHIER 

(defun prefixer-code-ms (symbole)
  (symbol 'ms symbole))
               
(defun de-prefixer-code-ms (symbole-package)
  (string2symbol (string symbole-package)))     


;;; 3. LIBERATION DE MEMOIRE
;;; JH/!je ne pense pas qu'il faille "remober" liste-cours et liste-salle
;;;   /!sinon, on ne peut pas relancer le moteur salle une deuxieme fois
;;;   /!il faut juste les mettre a null

(defun liberer-memoire-ms-post ()
  (mapc (lambda (cours) (remob (code-cours cours))) liste-cours)
  (mapc (lambda (salle) (remob (code-salle salle))) liste-salle)
  (setq liste-cours ())                 ; (remob liste-cours)
  (setq liste-salle ()))                ; (remob liste-salle)


;;; 4. CHEVAUCHEMENT DE RECTANGLE


;;; Teste si deux rectangles ont une intersection non-vide
;;; dbg (03/05/94) MO: ai remplac‚ les calculs faux sur les instants par des
;;; calculs sur les semaine-instant

(defun chevauchement-cours-p 
  (instant1 duree1 nbsem1 instant2 duree2 nbsem2)
    (and
      (let
        ((semaine1 (semaine-instant instant1))
         (semaine2 (semaine-instant instant2))) 
        (and
          (lt semaine1 (add semaine2 nbsem2))
          (lt semaine2 (add semaine1 nbsem1))))
      
      ;; a ce moment, nous savons que les semaines se chevauchent
      
      (let ((moment1 (moment-instant instant1))
            (moment2 (moment-instant instant2)))
           (and
             (lt moment1 (add moment2 duree2))
             (lt moment2 (add moment1 duree1))))))

(defun envoi-info (compteur seuil long)
  (when 
    (eq (modulo compteur seuil) 0) 
    (cons-info () (+ 20 (floor (* 60 (/ compteur long)))) () ())))        


;;; DBG (10/02/95) MO
;;; il manquait l'eval dans le let
;;; !! il faudrait vraiment evaluer tout cela a la lecture !!!! et 
;;; donc revoir un peu ces vieilles primitives croulantes interdites....

;;; CHG (12/04/95) MO
;;; mis un <and> a la place du <when> et tester l'appartenance de la salle
;;; a <interdites-cours>
;;; en changement par rapport au bug ci-dessus, je reviens en arriere: pas 
;;; besoin d'eval mais on enleve le code salle a la fin.... c'est bcp mieux
;;; !!! le re-itere la remarque sur les vieilles primtives croulantes....

;;; MOD RM - 20/05/96 : on remplace le and par un when pour augmenter la
;;; clarete du test et on ajoute la gestion des conflits pour les cours
;;; ayant lieu en meme temps dans la meme salle (salle figee identique)

(defun maj-salles-interdites (cours liste)
  (let
    ((instant (instant-trouve-cours cours))
     (duree (duree-cours cours))
     (salle (salle-trouvee-cours cours))
     (nbsem (nb-seances-cours cours))
     (modifierp ()))
    
    (mapc
      (lambda (c)
	(when
	  (chevauchement-cours-p
	    instant duree nbsem
	    (instant-trouve-cours c) (duree-cours c) (nb-seances-cours c))
	  (cond
	    ((eq salle (salle-trouvee-cours c))
	      (afficher-erreur-chevauchement cours c)
	      (salle-trouvee-cours c ())
	      (interdites-cours c (cons salle (interdites-cours c)))
	      (setq modifierp t))
	    ((not (appartenir-liste salle (interdites-cours c)))	     
	      (interdites-cours c (cons salle (interdites-cours c))))
	    (t t))))
      liste)
    (when modifierp
      (setq liste (cours-modifies-fin-liste liste)))
    liste))

;;; CRE RM - 20/05/96 : affichage d'une erreur de chevauchement de deux
;;; cours instancies dans une meme salle

(defun afficher-erreur-chevauchement (cours1 cours2)
  (let
    ((code1 (code-cours cours1))
     (code2 (code-cours cours2))) 
    (afficher-attention
      (column
	(row #Mv_les-cours)
	(row
	  (string (libelle-i-cours (de-prefixer-code-ms code1)))
	  " (" (string code1) ")")
	(row
	  "et "
	  (string (libelle-i-cours (de-prefixer-code-ms code2)))
	  " (" (string code2) ")")	  
	(row #Mv_chevauche)
	(row #Mv_varuna-cherche)
	(row #Mv_pour-le-cours " (" (string code2) ")")))))


;;; AJT (11/04/95) MO
;;; Prise en compte de la reservation des salles

(defun maj-salles-reservees (liste)
  (mapc 'maj-salles-interdites-reservees liste))

;;; CRE (11/04/95) MO
;;; <maj-salles-interdites-reservees>
;;; pour chaque cours (en moyenne il y a moins de resa que de cours), je
;;; regarde pour chaque resa si ces deux entites se chevauchent, et alors
;;; si la salle n'est pas deja interdites pour ce cours, je lastocke dans
;;; son champ interdites
;;; pour des raisons d'homogeneite avec le reste du programme, j'ai conserve
;;; la representation symbolique avec evaluation ulterieure pour les salles
;;; il faudrait un jour songer a changer cela et a pointer directement sur
;;; les objets !!!!

;;; MOD RM - 21/05/96 : on test les conflits lorsqu'un cours et une
;;; resa ont lieu en meme temps dans la meme salle (le cours ayant une
;;; salle figee apres la creation de la resa)

(defun maj-salles-interdites-reservees (cours)
  (let
    ((instant (instant-trouve-cours cours))
     (duree (duree-cours cours))
     (nbsem (nb-seances-cours cours))
     (salle (salle-trouvee-cours cours)))
    (salles-reservees-resa cours instant duree nbsem salle)
    (salles-reservees-ajout cours instant duree nbsem salle)))

(defun salles-reservees-resa (cours instant duree nbsem salle)
  (mapc
    (lambda (resa)
      (when
	(chevauchement-cours-p
	  instant duree nbsem
	  (instant-resa resa) (duree-resa resa) 1)
	(cond
	  ((eq salle (salle-resa resa))
	    (afficher-erreur-chevauchement-resa cours resa)	      
	    (salle-trouvee-cours cours ())
	    (interdites-cours cours
	      (cons (salle-resa resa) (interdites-cours cours))))
	  ((not (appartenir-liste (salle-resa resa)
	    (interdites-cours cours)))
	  (interdites-cours cours 
	    (cons (salle-resa resa) (interdites-cours cours))))
	  (t t))))
    liste-resa))

(defun salles-reservees-ajout (cours instant duree nbsem salle)
  (mapc
    (lambda (ajout)
      (when
	(chevauchement-cours-p
	  instant duree nbsem
	  (instant-ajout ajout) (duree-ajout ajout) 1)
	(cond
	  ((eq salle (salle-ajout ajout))
	    (afficher-erreur-chevauchement-ajout cours ajout)	      
	    (salle-trouvee-cours cours ())
	    (interdites-cours cours
	      (cons (salle-ajout ajout) (interdites-cours cours))))
	  ((not (appartenir-liste (salle-ajout ajout)
	    (interdites-cours cours)))
	  (interdites-cours cours 
	    (cons (salle-ajout ajout) (interdites-cours cours))))
	  (t t))))
    liste-ajout))

;;; CRE RM - 21/05/96 : affichage d'une erreur de chevauchement d'un
;;; cours instancies et d'une resa dans une meme salle
;;; CRE RM - 11/06/97 : affichage d'une erreur entre un cours et un ajout
;;; de seance

(defun afficher-erreur-chevauchement-resa (cours resa)
  (let
    ((c-cours (code-cours cours))
     (c-resa (code-resa resa))) 
    (afficher-attention
      (column
	(row
	  #Mv_le-cours
	  (string (libelle-i-cours (de-prefixer-code-ms c-cours)))
	  " (" (string c-cours) ")")
	(row
	  "et " #Mv_la-resa
	  (string (libelle-i-resa (de-prefixer-code-ms c-resa)))
	  " (" (string c-resa) ")")	  
	(row #Mv_chevauche)
	(row #Mv_varuna-cherche)
	(row #Mv_pour-le-cours " (" (string c-cours) ")")))))

(defun afficher-erreur-chevauchement-ajout (cours ajout)
  (let
    ((c-cours (code-cours cours))
     (c-ajout (code-ajout ajout))) 
    (afficher-attention
      (column
	(row
	  #Mv_le-cours
	  (string (libelle-i-cours (de-prefixer-code-ms c-cours)))
	  " (" (string c-cours) ")")
	(row
	  "et l'" 
	  (string (libelle-i-ajout (de-prefixer-code-ms c-ajout)))
	  " (" (string c-ajout) ")")	  
	(row #Mv_chevauche)
	(row #Mv_varuna-cherche)
	(row #Mv_pour-le-cours " (" (string c-cours) ")")))))

;;; CRE RM 17/05/96 - on trie les cours avec ceux ayant une salle figee
;;; en premier.
;;; MOD RM 24/09/98 : on place les salles dans l'ordre suivant :
;;; -> salles figees, salles souhaitees, autre

(defun liste-salles-figees-dabord (liste)
  (let
    ((resultat ())
     (liste-tmp ()))
    (mapc
      (lambda (cours)
	(if (salle-trouvee-cours cours)
	  (setq liste-tmp (cons cours liste-tmp))
	  (setq liste-tmp (append liste-tmp (list cours)))))
      liste)
    (mapc
      (lambda (cours)
	(if (salle-figee-cours cours)
	  (setq resultat (cons cours resultat))
	  (setq resultat (append resultat (list cours)))))
      liste-tmp)
    resultat))

;;; CRE RM 7/01/99
;;; on deplace les cours modifies en fin de liste

(defun cours-modifies-fin-liste (liste)
  (let
    ((liste-non-modifies ())
     (liste-modifies ())) 

    (mapc
      (lambda (cours)
	(if (salle-trouvee-cours cours)
	  (setq liste-non-modifies
	    (append liste-non-modifies (list cours)))
	  (setq liste-modifies
	    (append liste-modifies (list cours)))))
      liste)
    (append liste-non-modifies liste-modifies)))

