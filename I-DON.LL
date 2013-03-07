;;; --------------------------------------------------------------------------
;;; - I-Don.ll                                                  VARUNA 1.2   -
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; - Interface Graphique                                                    -
;;; - Utilitaires d'interface Varuna                                         -
;;; --------------------------------------------------------------------------
;;; - Copyright by Abstract & P. Riviere - 1993-1996                         -
;;; - Creation:  Mars 93                                                     -
;;; - Mise a jour pour version 1.07: Decembre 93                             -
;;; - Build 1.22.1 01.07.99 / JH MO                                          -
;;; --------------------------------------------------------------------------

;;; - AJT MO resa-affichable-p

;;; !! remettre un peu d'ordre dans ce fichier!!

;; III. les autres utilitaires:
;; ---------------------------------


;;; Construit la liste des differents types de salle - renvoie des string
;;; a partir des types existants dans les champs type de salle
;;; de l'objet cours et type de salle de l'objet salle.

(defun liste-type-salle ()
  (let 
    ((out ()))
    (mapc
      (lambda (salle)               ; les types de salle definis
	(when (not (memq (type-i-salle salle) out))
	  (newl out (type-i-salle salle))))
      Liste-I-Salle)
    (mapc
      (lambda (cours)               ; les types de salles utilises
	(when (not (memq (type-salle-i-cours cours) out))
	  (newl out (type-salle-i-cours cours))))
      Liste-I-Cours)
    (mapcar 'string out)))

;;; <liste-types-salle-autorises>
;;; CRE - FD - 21/03/96
;;; retourne la liste de tous les types de salle autorises

(defun liste-types-salle-autorises ()
  (sous-liste type-salle-autorise-p (liste-type-salle)))

;;; <maj-lst-types-salles-autorises>
;;; CRE - FD - 21/05/96
;;; ajoute un type de salles a la list de ceux deja autorises
;;; MOD - FD - 28/05/96 - SITE-CONNECTE

(defun maj-lst-types-salle-autorises (type-salle)
  (when
    (not (appartenir-liste (symbol () type-salle)
; TYPES-SALLE-AUTORISES
      (types-salles-i-site SITE-CONNECTE)))
    (types-salles-i-site SITE-CONNECTE
      (cons (symbol () type-salle) (types-salles-i-site SITE-CONNECTE)))))
;     (setq TYPES-SALLE-AUTORISES
;       (cons (symbol () type-salle) TYPES-SALLE-AUTORISES))))

;;; construit la liste des codes des types de cours
;;; renvoie des string
;;; MOD RM - 05/09/96 : ajout du libelle

(defun liste-type-cours ()
  (mapcar 'code-i-type-cours (types-cours-i-ecole I-Ecole)))

;;; <liste-types-cours-autorises>
;;; CRE - FD - 26/03/96
;;; retourne la liste de tous les types de cours autorises

(defun liste-types-cours-autorises ()
  (sous-liste type-cours-autorise-p (liste-type-cours)))

(defun liste-types-cours-avec-libelle ()
  (mapcar
    (lambda (tc)
      (catenate
	(string (code-i-type-cours tc)) " - "
	(string (libelle-i-type-cours tc))))
    (liste-types-cours-autorises)))
  
;;; <maj-lst-types-cours-autorises>
;;; CRE - FD - 30/04/96
;;; ajoute un type de cours a la list de ceux deja autorises
;;; MOD - FD - 04/06/96 -SITE-CONNECTE

(defun maj-lst-types-cours-autorises (type-cours)
  (when
    (not
      (appartenir-liste (code-i-type-cours type-cours)
	(types-cours-i-site SITE-CONNECTE)))
    (types-cours-i-site SITE-CONNECTE
      (cons
	(code-i-type-cours type-cours)
	(types-cours-i-site SITE-CONNECTE)))))

;;; construit la liste des civilites de profs
;;; renvoie des string

(defun liste-civilite-prof ()
  (let
    ((out ()))
    (mapc (lambda (prof)
      (when (not (member (civilite-i-prof prof) out))
	(newl out (civilite-i-prof prof))))
    Liste-I-Prof)
    (mapcar 'string out)))

;;; construit la liste des codes de salles
;;; renvoie des string
;;; MOD RM - 05/09/96 : ajout du libelle
;;; MOD RM - 18/09/96 : filtrage des salles en fonction du site

(defun liste-code-salle ()
  (let
    ((liste-salle))
    (if SITE-CENTRAL
      (setq liste-salle Liste-I-Salle)
      (mapc
	(lambda (salle)
	  (when (salle-site-p salle)
	    (newl liste-salle salle)))
	Liste-I-Salle))
    (mapcar
      (lambda (sa)
	(catenate
	  (code-i-salle sa) " - " (string (libelle-i-salle sa))))
      liste-salle)))

;;; libelle du professeur avec la civilite

(defun civilite-libelle-i-prof (prof)
  (catenate (civilite-i-prof prof) ESP (libelle-i-prof prof)))

;;; confirmation pour une suppression

(defun confirmer-supprimer (libelle x)
  (afficher-confirmation
    (column #Mv_supprimer libelle)))

;;; predicat d'instanciation d'un cours (au sens de l'interface)

(defun i-instanciep (cours)
  (when (instant-trouve-i-cours cours) t))

;;; l'inverse

(defun i-pas-instanciep (cours)
  (not (i-instanciep cours)))

;;; <i-pas-instancie-modifiable-p>
;;; CRE - 04/03/96 - MulitP
;;; le cours n'est pas instancie et il n'est pas en lecture seule

(defun i-pas-instancie-modifiable-p (cours)
  (and
    ;; AJT - FD - 02/04/96
    (cours-site-p cours)
    (not (donnee-lecture-seule-p cours ()))
    (i-pas-instanciep cours)))

(defun i-instancie-modifiable-p (cours)
  (and
    (cours-site-p cours)
    (not (donnee-lecture-seule-p cours ()))
    (i-instanciep cours)))

;;; AJT RM - 290695
;;; predicat d'instanciation d'un cours sans salle

(defun i-instancie-sans-salle-p (cours)
  (and 
      (instant-trouve-i-cours cours)
      (not (salle-trouvee-i-cours cours))))

;;; <i-cours-sans-prof-p>
;;; predicat indiquant qu'un cours n' a pas de prof d'affecte

(defun i-cours-sans-prof-p (cours)
  (not (profs-i-cours cours)))

;;; CRE RM 26/12/96

(defun i-prof-sans-cours-p (prof)
  (not (cours-i-prof prof)))

;;; <i-instancie-sans-salle-modifiable-p>
;;; CRE - 04/03/96 - MuliP

(defun i-instancie-sans-salle-modifiable-p (cours)
  (and 
      (donnee-modifiable-p cours))
      (i-instancie-sans-salle-p cours)))

;;; predicat qui renvoie t si le cours est fige

(defun i-figep (cours)
  (and (fige-i-cours cours) t))

;;; l'inverse

(defun i-pas-figep (cours)
  (not (fige-i-cours cours)))

;;; <i-pas-fige-modifiable-p>
;;; CRE - 04/03/96

(defun i-pas-fige-modifiable-p (cours)
  (and
    (donnee-modifiable-p cours)
    (i-pas-figep cours)))

;;; predicat qui renvoie t si le cours a une salle
;;; rem (30/05/95) mo: et si on respectait la specif concernant les predicats? 
;;; ie mettre un -p a la fin du nom de la primitive !!!

(defun i-salle-trouvee (cours)
  (and (salle-trouvee-i-cours cours) t))

;;; predicat renvoie t si le cours n'a pas de salle
;;; (le cours doit donc etre instancie)
;;; different de (non i-salle-trouvee cours)

(defun i-pas-de-salle-trouvee (cours)
  (and
    (i-instanciep cours)
    (not (i-salle-trouvee cours))))

;;; <i-salle-fige-p>
;;; CRE - FD - 20/09/95
;;; predicat qui renvoie t si le cours a sa salle de fige

(defun i-salle-fige-p (cours)
  (and
    (i-salle-trouvee cours)
    (salle-figee-reelle-i-cours cours)))

;;; <i-salle-pas-fige-modifiable-p>
;;; CRE - 06/12/96

(defun i-salle-pas-fige-modifiable-p (cours)
  (and
    (donnee-modifiable-p cours)
    (not (i-salle-fige-p cours))))

;;; AFFICHABILITE D'UN COURS

;;; calcule "l'affichabilite" d'un cours suivant le contexte d'affichage : 
;;;                          - il est instancie au sens de l'interface
;;;                          - il est pour Liste-Groupes-aff
;;;                          - il est dans la salle Liste-Salles-Aff
;;;                          - il est dans la semaine affichee
;;;                          - il a lieu pendant un jour de Jour-Aff
;;;                          - il a lieu dans une semaine comprise entre
;;;                            Semaine-Debut-Aff et Semaine-Fin-Aff
;;; (a venir)                - il commence a une heure de debut donnee


;;; teste si un des groupes du cours appartient
;;; a la liste Liste-Groupes-Aff

(defun afficher-groupe-p (cours)
  (inter-non-vide-p (groupes-i-cours cours) Liste-Groupes-Aff))


;;; teste si la salle trouvee du cours
;;; appartient a la liste Liste-Salles-Af

(defun afficher-salle-p (cours)
  (memq (salle-trouvee-i-cours cours) Liste-Salles-Aff))

;;; CRE RM - 04/12/95
;;; teste si un des profs du cours appartient
;;; a la liste Liste-Profs-Aff

(defun afficher-profs-p (cours)
  (inter-non-vide-p (profs-i-cours cours) Liste-Profs-Aff))

;;; teste si le cours est affichable
;;; par sa definition (groupe ou salle affichable)
;;; MOD RM - 04/12/95 : Affichage des profs

(defun afficher-cours-donnees-p (cours)
  (or
    (and Liste-Groupes-Aff (afficher-groupe-p cours))
    (and Liste-Salles-Aff (afficher-salle-p cours))
    (and Liste-Profs-Aff (afficher-profs-p cours))))


;;; teste si le cours est affichable 
;;; par la definition du display
;;; type d'affichage et intervalles affiches
;;; en particulier, traite le cas des type
;;; de cours (avec Heures-Debut-Aff)

;;; DBG (09/03/95) MO
;;; pas moins de quatre bug dans cette seule primitive
;;; cela expliquait un certain nombre de bug de l'affichage partiel et
;;; hebdomadaire


(defun afficher-cours-display-p (cours)
  (let* 
    ((instant (instant-trouve-i-cours cours))
     (semaine-debut-cours (semaine-instant instant))
     (semaine-fin-cours 
             (sub (add semaine-debut-cours (nb-seances-i-cours cours)) 1))
     ;;(semaine-fin-cours (add semaine-debut (nb-seances-i-cours cours)))
     (horaire-cumule-debut (interne2cumule (horaire-instant instant)))
     (le-jour (jour-instant instant)))
    (and
      (memq le-jour Jours-Aff)
      (memq horaire-cumule-debut Heures-Debut-Aff)
;;;      (or 
;;;        (and
;;;          (lt semaine-debut-cours Semaine-Debut-Aff)
;;;          (gt semaine-fin-cours Semaine-Debut-Aff))
;;;        (and
;;;          (ge semaine-debut-cours Semaine-Debut-Aff)
;;;          (le semaine-debut-cours Semaine-Fin-Aff))
;;;        (and
;;;          Week-Aff
;;;          (and
;;;            (ge semaine-debut-cours Week-Aff)
;;;            (le semaine-fin-cours Week-Aff))))
      (if 
        (not Week-aff)
        (or
          (and
            (ge semaine-fin-cours Semaine-Debut-Aff)  ; non strict parce que 
            (le semaine-fin-cours Semaine-Fin-Aff))   ; ferme a droite 
          (and
            (ge semaine-debut-cours Semaine-Debut-Aff) ; non strict parce que 
            (le semaine-debut-cours Semaine-Fin-Aff))  ; ferme a gauche
          (and 
            (ge semaine-fin-cours Semaine-Fin-Aff)
            (le semaine-debut-cours Semaine-Debut-Aff))) ;cours deb. 2 cotes
        (and
;;; AJT RM 020895 : non affichage des seances annul‚es
          (not (seance-annulee-p cours Week-Aff))
          (ge Week-Aff semaine-debut-cours)
          (le Week-Aff semaine-fin-cours))))))   ; attention au sens -> crucial

;;; predicat d'affichage du cours <cours>

(defun cours-affichable-p (cours)
  (and
    (i-instanciep cours)
    (afficher-cours-donnees-p cours)
    (afficher-cours-display-p cours)))



;;; AFFICHABILITE D'UNE RESA
;;; AJT (10/04/95) MO
;;; calcul de l'affichabilite d'une resa pour le module de reservation salles


;;; predicat general d'affichage de la resa <resa>
;;; il faut que par nature elle soit affichable ainsi que pour le display

(defun resa-affichable-p (resa)
  (and
    (or 
      (afficher-resa-donnees-p resa)
      (appartenir-liste resa Liste-Resas-Aff)) ;;; AJT FD-25/05/95
    (afficher-resa-display-p resa)))


;;; teste si la resa est dans la salle d'affichage 
;;; resa affichable par nature de resa

(defun afficher-resa-donnees-p (resa)
  (and Liste-Salles-Aff (afficher-resa-salle-p resa)))


;;; teste si la salle de la resa est a l'affiche 

(defun afficher-resa-salle-p (resa)
  (and 
    (memq (salle-i-resa resa) Liste-Salles-Aff)
    t))


;;; teste si la resa est affichable pour le display 

(defun afficher-resa-display-p (resa)
  (let* 
    ((instant (instant-i-resa resa))
     (semaine-resa (semaine-instant instant))
     (horaire-cumule (interne2cumule (horaire-instant instant)))
     (jour (jour-instant instant)))
    (and
      (memq jour Jours-Aff)
      (memq horaire-cumule Heures-Debut-Aff)
      (if 
        (not Week-aff)
        (and
          (ge semaine-resa Semaine-Debut-Aff)  ; non strict parce que 
          (le semaine-resa Semaine-Fin-Aff))   ; ferme a droite 
        (eq week-aff semaine-resa))
      t)))



;;; COULEURS

;;; renvoie une couleur suivant le type de la salle
;;;
;;;
;;; TST (14/03/95) MO,FD
;;; en mettant une globale a la place de l'appel a la primtive (liste-type
;;; salle), on gagne un facteur n (n nbre de cours) soit
;;; 0.07 s  en moy contre 0.
;;; ceci permet l'amelioration d'un facteur 2 de l'affichage d'une grosse 
;;; filière 
;;; (12101 + 12101-td) sur assas 22 s contre 37 s
;;; ceci signifie egalement qu'il y a du temps a gagner autre part -> a voir..
;;; construction de l'entite' rectangle ???? 


(defun couleur-de-salle (type-salle)
  (let* 
    ((numero (numero-dans-liste (string type-salle) (liste-type-salle)))
     (couleur 
       (or
         (and  numero (nth numero Liste-Couleurs))
         (car (last liste-couleurs)))))

    (if impressionp (couleur-impression couleur) couleur)))    

;;; CRE - FD - 18/09/95
;;; <couleur-de-resa>
;;; renvoie la couleur d'une resa

(defun couleur-de-resa (resa)
  (let
    ((couleur COULEUR-RESA))
    (if impressionp
      (couleur-impression couleur)
      couleur)))

;;; CRE - FD - 18/09/95
;;; <couleur-de-ajout>
;;; renvoie la couleur d'une resa

(defun couleur-de-ajout (ajout)
  (let
    ((couleur COULEUR-AJOUT))
    (if impressionp
      (couleur-impression couleur)
      couleur)))

;;; CRE (08/09/95) MO
;;; creation de cette primitive servant pour les transformations de couleurs
;;; en gris pour les applications impressions graphiques
;;; MOD RM - 19/08/96 : on ne filtre que si l'on est en niveaux de gris

(defun couleur-impression (couleur)
  (if COULEUR-DEVICE
    couleur
    (let* 
      ((niveau (filtre-gris (niveau-gris couleur)))
	(composante (truncate (min 32767 (* niveau 32767)))))
      (make-color composante composante composante))))


;;; POLICES
;;; <police-salle>
;;; CRE - FD - 20/09/95
;;; renvoie la police de caracteres associee au libelle d'une salle
;;; d'un cours
;;; salle fige -> gras
(defun police-salle-cours (cours)
  (if 
    (i-salle-fige-p cours)
    (if impressionp time5b time8b)
    (if impressionp time5 time8)))

;;; renvoie la police de carractere associee a un cours
;;; Cours fige -> gras
;;; Impression -> petite police (time5)

(defun police-cours (cours)
  (if 
    (i-figep cours)
    (if impressionp time5b time8b) 
    (if impressionp time5 time8)))

;;; renvoie la police de carractere associee a une resa
;;; resa -> gras
;;; Impression -> petite police (time5)

(defun police-resa (resa)
  (if impressionp time5b time8b)) 

;;; renvoie la police de carractere associee a une ajout
;;; ajout -> gras
;;; Impression -> petite police (time5)

(defun police-ajout (ajout)
  (if impressionp time5b time8b)) 



;;; HORAIRES

;;; renvoie la liste des horaire-cumules de debut de cours
;;; pour les cours du type contenus dans la liste ltc

(defun horaires-types-cours (ltc)
  (let
    ((lh ()))
    (mapc
      (lambda (tc)
	(mapc
	  (lambda (mc)
	    (let
	      ((hmc (horaire-moment-cumule mc)))
	      (when (not (memq hmc lh))
		(newl lh hmc))))
	  (liste-moments-cumules-i-type-cours tc)))
      ltc)
    (sortn lh)))

;;; teste si on est en Mode saisie ou non

(defun mode-saisiep ()
  (equal Mode 'saisie))

;;; chaine code+libelle d'un cours

(defun code-libelle (e)
  (catenate (code-i-cours e) " - " (libelle-i-cours e)))


;;; fonction de tri d'horaires naturels

(defun tri-horaire-naturels (h1 h2)
  (when (horaire-naturel-plus-petit-p h1 h2)
    h1))

;;; fonction de tri de jours (en symboles)

(defun tri-jours (j1 j2)
  (when 
    (le (jour-libelle (symbol () j1)) (jour-libelle (symbol () j2))) j1))



;;; FD (17/03/95) Pb: renvoie () si dir = "c:\" (racine)
;;;             remplace (catenate dir "\" FICHIER-xxxx)
;;;             par (catenate dir FICHIER-xxxx)
;;; teste si le repertoire passe en parametre
;;; contient des fichiers de donnees varuna
;;; (le fichier contrainte est optionel)

(defun varuna-directory (dir)
  (and
    ;;;  FD 17/03/95 supression du "\" entre dir et FICHIER-xxxx
    ;;; ex. (probefile (catenate dir "\" FICHIER-ECOLE))
    (probefile (catenate dir  FICHIER-ECOLE))
    (probefile (catenate dir  FICHIER-GROUPES))
    (probefile (catenate dir  FICHIER-COURS))
    (probefile (catenate dir  FICHIER-PROFS))
    (probefile (catenate dir  FICHIER-SALLES))))



;;; duree maximale des cours exprimee en horaire naturelle
;;; utilise dans i-cours
;;; dbg (04/05/94) MO : il faut soutenir Horaire-Cumule-Pause ()
;;; rem (04/05/94) MO : il faudrait calculer avec des horaires internes !!


(defun duree-max ()
  (- Horaire-Cumule-Fermeture Horaire-Cumule-Ouverture))

;;; fonction de demande d'une duree de cours
;;; comprise entre 0 et le resultat de duree-max

(defun demande-duree-cours ()
  (demande-duree #Mv_pro-duree 0 (duree-max)))

;;; calcul du nombre de seances maximum

(defun nb-seances-max ()
  (add1 (- Semaine-Fin Semaine-Debut)))


;;; FD - MODULE-RESA
;;; retourne la capacite de la salle la plus grande
;;; rem (02/06/95) MO
;;; c'est la premiere fois que l'on utilise le <nextl> avec deux 
;;; arguments... bravo !! mais prudence aux effets de bord type delq.

(defun capacite-max ()
  (let 
    ((liste Liste-I-Salle)
     (salle)
     (resultat 0))
    (while 
      (nextl liste salle)
      (and                                    ; <if> 
        (gt (capacite-i-salle salle) resultat)
        (setq resultat (capacite-i-salle salle))))
    resultat))



;;; fonction de traduction d'un horaire cumule
;;; en horaire naturel - sans chaine 0
;;; ex : (heure-entiere 120) -> 2h (heure-entieres 150)->2h30

(defun heure-entiere (hc)
  (let ((h (funcall 'cumule2naturel hc)))
    (if (integerp (/ hc NB-MINUTES))
      (substring h 0 (add1 (index (string CHAINE-HEURE) h)))
      "")))

;;; renvoie les chaine de numeros de semaines paires 
;;; (pour l'editeur dispo, c'est plus lisible)

(defun libelle-semaines-dispo (sem)
  (let ((sem1 (integerp (/ sem 2))))
    (if sem1 (string sem) "")))

;;; idem avec les heures 
;;; ne renvoie que les heures entieres

(defun libelle-horaire-dispo (hc)
  (let ((hc1 (integerp (/ hc NB-MINUTES))))
    (if hc1 (catenate hc1 "h") "")))

;;; UTILITAIRES POUR LA GESTION DES DIFFERENTS TYPES DE GROUPES

(defmessage v_groupe-standard (french "STANDARD"))
(defmessage v_groupe-filiere (french "FILIERE"))
(defmessage v_groupe-enseignement (french "ENSEIGNEMENT"))
(defmessage v_groupe-td-a (french "TD/TPs SIMULTANES"))
;; pour des raisons de compatibilites avec les anciennes versions de Varuna
;; on conserve l'ancien libelle  (19/01/96)
(defmessage ancien-v_groupe-td-a (french "TDs SIMULTANES"))
(defmessage v_groupe-td-b (french "TD/TPs MEME SEMAINE"))
;; pour des raisons de compatibilites avec les anciennes versions de Varuna
;; on conserve l'ancien libelle  (19/01/96)
(defmessage ancien-v_groupe-td-b (french "TDs MEME SEMAINE"))
(defmessage v_groupe-alternance (french "ALTERNANCE"))

;;; Definition de groupes particulier
;;; MOD RM - 26/03/96 : ajout de la fenetre de type alternance

(defvar LISTE-TYPES-GROUPES
  (list 
    #Mv_groupe-standard
    #Mv_groupe-filiere
    #Mv_groupe-enseignement
    #Mv_groupe-td-a
    #Mv_groupe-td-b
    #Mv_groupe-alternance))

;;; renvoie la fenetre (application) associee a un type de groupe
;;; MOD RM - 26/03/96 : ajout de la fenetre de type alternance

(defun type-groupe2editeur-groupe (type)
  (cond
    ((equal type #Mv_groupe-filiere) 'fenetre-i-groupe-filiere)
    ((equal type #Mv_groupe-enseignement) 'fenetre-i-groupe-enseignement)
    ((or (equal type #Mv_groupe-td-a) (equal type #Mancien-v_groupe-td-a))
    'fenetre-i-groupe-td-a)
    ((or (equal type #Mv_groupe-td-b) (equal type #Mancien-v_groupe-td-b))
      'fenetre-i-groupe-td-b)
    ((equal type #Mv_groupe-alternance) 'fenetre-i-groupe-alternance)
    (t 'fenetre-i-groupe-standard)))

;;; renvoie les contraintes associes a un type de groupe
;;; MOD RM - 26/03/96 : ajout de l'alternance

(defun type-groupe2contraintes-groupe (type)
  (let ((code-contrainte
    (cond
      ((equal type #Mv_groupe-filiere)
        (libelle-contrainte2code-contrainte #Mv_non-simultaneite))
      ((equal type #Mv_groupe-enseignement)
        (list
          (libelle-contrainte2code-contrainte #Mv_meme-semaine)
          (libelle-contrainte2code-contrainte #Mv_jours-differents)))
      ((or (equal type #Mv_groupe-td-a) (equal type #Mancien-v_groupe-td-a))
        (libelle-contrainte2code-contrainte #Mv_simultaneite))
      ((or (equal type #Mv_groupe-td-b) (equal type #Mancien-v_groupe-td-b))
        (libelle-contrainte2code-contrainte #Mv_meme-semaine))
      ((equal type #Mv_groupe-alternance)
	(libelle-contrainte2code-contrainte #Mv_k-ecart-simultaneite)))))
  (if (consp code-contrainte)
    code-contrainte
    (when code-contrainte (list code-contrainte)))))



;;; cette fonction genere un code de cours pour un groupe generateur 
;;; de cours constitue du code du groupe + numero du td.
;;; si le code generer pointe deja sur quelques chose
;;; le numero est incremente jusqua obtenir un code
;;; non utilise.

(defun gencode-i-groupe-generateur (groupe-td chaine no)
  (let* ((code-groupe (code-i-groupe groupe-td))
    (i no)
    (code-base ()))
  (while (boundp code-base)
    (setq code-base (concat code-groupe "-" chaine i))
    (incr i))
  code-base))


;;; la procedure d'edition d'un groupe
;;; appelle la procedure editer-i-groupe qui a ete generee par
;;; la macro def-actions-editions
;;; puis appelle la routine de mise a jour du groupe
;;; MOD RM - 22/04/96 : ajout d'un parametre <creationp>

(defun editer-i-groupe-special (groupe creationp)
  (let ((ok (editer-i-groupe groupe creationp)))
    (when ok
      (mise-a-jour-groupe-special groupe))))

;;;  FD - 23/06/95
;;;  MOD demande confirmation avant de supprimer les cours de la famille
(defun supprimer-i-groupe-special (groupe confirm)
  (if (supprimer-i-groupe groupe confirm)
    (suppression-groupe-special groupe)))

;;; MOD 23/06/95
;;;  on test la confirmation avant d'enlever le cours de la liste des cours des
;;;  profs et des groupes
;;; suppression de cours : va enlever physiquement les cours 
;;; des champ cours du prof et cours de groupe
;;; puis supprime le cours
;;; rem. FD 06/06/95 deemande confirmation de la suppression qu'apres avoir
;;;     supprimer le cours de la liste des cours du ou des prof(s) et de la
;;;     liste des goupes dont il faisait partie
;;; MOD - FD - 21/06/95
;;; avant de supprimer un cours on verifie qu'il n'appartient pas a une famille
;;; generatrice
;;; MOD RM - 20/09/96 : lors de la suppression d'un cours, on supprime
;;; egalement les annulations et les ajouts de ce cours ainsi
;;; que les contraintes explicites qui pesent sur ce cours

(defun supprimer-i-cours-special (cours confirm)
  (if
    (and (cours-genere-p cours) confirm)
    (alerte-type 'afficher-attention
      (column
	#Mv_limitation-temporaire
	#Mv_suppression-cours-genere
	#Mv_supprimer-famille))
    (when (supprimer-i-cours cours confirm)
      (mapc
	(lambda (p)
	  (cours-i-prof p (delq cours (cours-i-prof p))))
	(profs-i-cours cours))
      (mapc
	(lambda (groupe)
	  (cours-i-groupe groupe (delq cours (cours-i-groupe groupe))))
	(groupes-i-cours cours))
      (supprimer-annulation-cours cours)
      (supprimer-ajout-cours cours)
      (supprimer-contraintes-cours (code-i-cours cours)))))

;;; AJOUT GG (18/11/94)
;;; lorsqu'on copie un groupe
;;; on ne copies pas tous les champs : 
;;; le champ cours est copie pour les enseignements et les groupes
;;; de td. Le champ groupe est toujours copie
;;; AJT RM - 27/03/96 : Ajout de l'alternance

(defun copier-i-groupe-special (groupe)
  (let ((type (type-macro-i-groupe groupe))
    (copie-groupe (circopy-struct groupe)))
  (cond
    ((equal type #Mv_groupe-enseignement)
      (cours-i-groupe copie-groupe ()))
    ((equal type #Mv_groupe-alternance)
      (cours-i-groupe copie-groupe ()))
    ((or (equal type #Mv_groupe-td-a) (equal type #Mancien-v_groupe-td-a))
      (cours-i-groupe copie-groupe ()))
    ((or (equal type #Mv_groupe-td-b)  (equal type #Mancien-v_groupe-td-b))
      (cours-i-groupe copie-groupe ())))
  (code-i-groupe copie-groupe (gencode-i-groupe))
  (index-i-groupe copie-groupe ())
  ;; MOD RM - 23/04/96 : test pour le reseau
  (if MODULE-RESEAU
    (debut-modification SITE copie-groupe CREATION))
  ;; MOD RM - 22/04/96 : ajout de <creationp> a <editer-i-groupe-special>
  (editer-i-groupe-special copie-groupe t)))

;;;  MOD  FD - 23/06/95
;;;  on teste la confirmation avant de supprimer les profs de la liste des
;;;  des profs des cours
;;; suppression d'un prof de groupe
;;; ne sert plus a rien (pas de prof dans les groupe de td)

;;; MOD RM - 13/09/96 : certaines familles ont des profs :
;;; enseignement, alternance !!!

(defun supprimer-i-prof-special (prof confirm)
  (when (supprimer-i-prof prof confirm)
    (mapc
      (lambda (cours)
	(profs-i-cours cours (delq prof (profs-i-cours cours))))
      (cours-i-prof prof))
    (mapc
      (lambda (groupe)
	(profs-i-groupe groupe (delq prof (profs-i-groupe groupe))))
      liste-i-groupe)
    (mapc
      (lambda (ajout)
	(profs-i-ajout ajout (delq prof (profs-i-ajout ajout))))
      liste-i-ajout)))

;;; CRE RM - 13/09/96 : lors de la suppression d'une salle, il faut
;;; remettre a () les champs salle-figee, salle-figee-reelle et salle-trouvee
;;; Si une resa ou un ajout sont dans cette salle, confirmation
;;; avant d'effacer.

(defun supprimer-i-salle-special (salle confirm)
  (let
    ((liste-resa (donner-liste-resa-salle salle))
     (liste-ajout (donner-liste-ajout-salle salle))
     (continuerp t))
  (when (supprimer-i-salle salle confirm)
    (when (or liste-resa liste-ajout)
	(setq continuerp
	  (prompt-continuer
	    (column #Mv_effacer-resa-ajouts
	      #Mv_effacer-resa-ajouts2 #Mv_effacer-resa-ajouts3))))
    (when continuerp
      (mapc
	(lambda (cours)
	  (when (eq salle (salle-figee-i-cours cours))
	    (salle-figee-i-cours cours ()))
	  (when (eq salle (salle-figee-reelle-i-cours cours))
	    (salle-figee-reelle-i-cours cours ()))
	  (when (eq salle (salle-trouvee-i-cours cours))
	    (salle-trouvee-i-cours cours ())))
	liste-i-cours)
      (mapc (lambda (resa) (supprimer-i-resa resa ())) liste-resa)
      (mapc (lambda (ajout) (supprimer-i-ajout ajout ())) liste-ajout)))))

(defun donner-liste-resa-salle (salle)
  (let
    ((result ()))
    (mapc
      (lambda (resa)
	(when (eq (salle-i-resa resa) salle)
	  (newl result resa)))
      liste-i-resa)
    result))

(defun donner-liste-ajout-salle (salle)
  (let
    ((result ()))
    (mapc
      (lambda (ajout)
	(when (eq (salle-i-ajout ajout) salle)
	  (newl result ajout)))
      liste-i-ajout)
    result))
	
;;; ajout d'un groupe (macro saisie)
;;; met a jour le type du groupe demande
;;; son editeur, et ses contraintes
;;; puis appelle la fonction d'edition editer-i-groupe-special
;;; (voir cette fonction)

(defun ajouter-i-groupe-special ()
  (let ((type (demande-simple-type #Mv_type-i-groupe LISTE-TYPES-GROUPES ())))
    (when type
      (let ((groupe (nouveau-i-groupe)))
        (when groupe
          (code-i-groupe groupe (gencode-i-groupe))
          (type-macro-i-groupe groupe type)
          (editeur-i-groupe groupe (type-groupe2editeur-groupe type))
	  ;; MOD RM - 22/04/96 : ajout de <creationp>
	  ;; MOD RM - 23/04/96 : test pour le reseau
	  (if MODULE-RESEAU
	    (debut-modification SITE groupe CREATION))
          (editer-i-groupe-special groupe t))))))



;;; gestion des cours generes par la macro saisie


;;; ENSEIGNEMENT
;;; la taille de la liste des durees donne le nombre de cours de
;;; l'enseignement. Si ce dernier est different du nombre de cours
;;; deja dans le groupe (nombre de cours avant la saisie)
;;; on creee les cours correspondants.
;;; puis on met a jour tous les champs des cours
;;; ainsi que leurs champ editeur quie doit etre positionne
;;; de telle sorte qu'on ne puisse pas editer directement le cours

(defun maj-enseignement (ens)
  (map-n-listes
    (lambda (co du)
      (cond
        ((and du co)            ; mise a jour d'un cours
          (libelle-i-cours co (libelle-i-groupe ens))
          (type-i-cours co (type-cours-i-groupe ens))
          (type-salle-i-cours co (type-salle-i-groupe ens))
          (audience-i-cours co (audience-i-groupe ens))
          (nb-seances-i-cours co (nb-seances-i-groupe ens))
          (duree-i-cours co du)
; on retire les cours
; des anciens profs
          (mapc (lambda (pr)
            (when (not (appartenir-liste pr (profs-i-groupe ens)))
              (cours-i-prof pr (delq co (cours-i-prof pr)))
              (profs-i-cours co (delq pr (profs-i-cours co)))))
          (profs-i-cours co))
; on affecte les nouveaux profs
          (mapc (lambda (pr)
            (when (not (appartenir-liste co (cours-i-prof pr)))
              (cours-i-prof pr (cons co (cours-i-prof pr)))))
          (profs-i-groupe ens))
; modifie les profs du cours
          
          (profs-i-cours co (profs-i-groupe ens)))
        
        ((and (not co) du)              ; creation d'un cours
          (let ((nouveau-c (nouveau-i-cours))
            (code (gencode-i-groupe-generateur ens "c" 1)))
          (code-i-cours nouveau-c code)
          (set code nouveau-c)
          (newl Liste-I-Cours nouveau-c)
	  ;; MOD RM - 23/04/96 : test pour le reseau
	  (if MODULE-RESEAU
	    (debut-modification SITE nouveau-c CREATION))
          (groupes-i-cours nouveau-c (list ens))
          (cours-i-groupe ens (cons nouveau-c (cours-i-groupe ens)))
          (libelle-i-cours nouveau-c (libelle-i-groupe ens))
          (type-i-cours nouveau-c (type-cours-i-groupe ens))
          (type-salle-i-cours nouveau-c (type-salle-i-groupe ens))
          (audience-i-cours nouveau-c (audience-i-groupe ens))
          (nb-seances-i-cours nouveau-c (nb-seances-i-groupe ens))
          (profs-i-cours nouveau-c (profs-i-groupe ens))
          (mapc (lambda (p)
            (cours-i-prof p (cons nouveau-c (cours-i-prof p))))
          (profs-i-groupe ens))
          (duree-i-cours nouveau-c du)))
        ((and co (not du))              ; suppression d'un cours
          (cours-i-groupe ens (delq co (cours-i-groupe ens)))
          (supprimer-i-cours-special co ()))))
    (cours-i-groupe ens)
    (durees-i-groupe ens)))


;;; FILIERES

;;; rien a generer, ni a mettre a jour

;;; GROUPE DE TD
;;; meme chose avec la liste des profs, sauf qu'un prof
;;; est un objet et il peuvent avoir ete changes 
;;; tout en restant le meme nombre
;;; on doit donc parcourir une premiere fois la liste des profs,
;;; en modifiant eventuellement les cours de celui ci,
;;; ensuite, on affecte les profs avec les cours
;;; du groupe


(defun maj-groupe-td (td)
  (let* ((ancien-nombre-cours (length (cours-i-groupe td)))
    (nouveau-nombre-cours (nb-td-i-groupe td))
    (diff (sub nouveau-nombre-cours ancien-nombre-cours)))
  (when (neq diff 0)
    (if (gt diff 0)
      (for (i 0 1 (sub1 diff))
        (let ((code-c (gencode-i-groupe-generateur td "" (add1 i)))
          (cours (nouveau-i-cours)))
        (code-i-cours cours code-c)
        (set code-c cours)
        (newl Liste-I-Cours cours) 
	;; MOD RM - 23/04/96 : test pour le reseau
	(if MODULE-RESEAU
	  (debut-modification SITE cours CREATION))
        (cours-i-groupe td (cons cours (cours-i-groupe td)))))

; on ne supprime pas de cours depuis cette fentre
; car on ne sait pas quels sont les cours a supprimer
; (les cours ont pu etre modifies entre temps)
      
      (afficher-erreur
        (column
          "Pas de suppression de cours depuis cette fenetre"
          "Utilisez la fenetre cours habituelle"))))
  
; dans tous les cas on remet a jour
; les cours du groupe
  
  (mapc (lambda (cours)
    (libelle-i-cours cours (libelle-i-groupe td))
    (nb-seances-i-cours cours (nb-seances-i-groupe td))
    (duree-i-cours cours (duree-i-groupe td))
    (type-salle-i-cours cours (type-salle-i-groupe td))
    (type-i-cours cours (type-cours-i-groupe td))
    (audience-i-cours cours (audience-i-groupe td))
    (when (not (memq td (groupes-i-cours cours)))
      (groupes-i-cours cours (cons td (groupes-i-cours cours)))))
  (cours-i-groupe td))))

;;; ALTERNANCE
;;; CRE RM - 26/03/96
;;; Creation des cours espaces (1 semaine sur n)
      
(defun maj-alternance (alt)
  (let*
    ((ancien-nombre-cours (length (cours-i-groupe alt)))
     (nouveau-nombre-cours (nb-seances-i-groupe alt))
     (diff (sub nouveau-nombre-cours ancien-nombre-cours)))
    (when (neq diff 0)
      (if (gt diff 0)
	(for (i 0 1 (sub1 diff))
	  (let
	    ((code-c (gencode-i-groupe-generateur alt "" (add1 i)))
	     (cours (nouveau-i-cours)))
	    (code-i-cours cours code-c)
	    (set code-c cours)
	    (newl Liste-I-Cours cours)
	    (when MODULE-RESEAU
	      (debut-modification SITE cours CREATION))
	    (cours-i-groupe alt
	      (append (cours-i-groupe alt) (list cours)))))
	;; AJT RM - 05/09/96 : reduire le nombre de seances
	(for (i 0 1 (sub1 (- diff)))
	  (let
	    ((cours
	      (eval
		(concat
		  (code-i-groupe alt) "-"
		  (- ancien-nombre-cours i)))))
	    (mapc
	      (lambda (p)
		(cours-i-prof p (delq cours (cours-i-prof p))))
	      (profs-i-cours cours))
	    (mapc
	      (lambda (groupe)
		(cours-i-groupe groupe (delq cours (cours-i-groupe groupe))))
	      (groupes-i-cours cours))
	    (setq Liste-I-Cours (delq cours Liste-I-Cours))
	    (makunbound (code-i-cours cours))))))
    (mapc
      (lambda (cours)
	(libelle-i-cours cours (libelle-i-groupe alt))
	(nb-seances-i-cours cours 1)
	(duree-i-cours cours (duree-i-groupe alt))
	(type-salle-i-cours cours (type-salle-i-groupe alt))
	(type-i-cours cours (type-cours-i-groupe alt))
	(audience-i-cours cours (audience-i-groupe alt))
	(when (not (memq alt (groupes-i-cours cours)))
	  (groupes-i-cours cours (cons alt (groupes-i-cours cours))))
	;; suppression des profs 
	(mapc
	  (lambda (pr)
	    (when (not (appartenir-liste pr (profs-i-groupe alt)))
	      (cours-i-prof pr (delq cours (cours-i-prof pr)))
	      (profs-i-cours cours (delq pr (profs-i-cours cours)))))
	  (profs-i-cours cours))
	;; affectation du cours au profs du groupe
	(mapc
	  (lambda (pr)
	    (when (not (appartenir-liste cours (cours-i-prof pr)))
	      (cours-i-prof pr (cons cours (cours-i-prof pr)))))
	  (profs-i-groupe alt))
	;; affectation des profs de la famille au cours
	(profs-i-cours cours (profs-i-groupe alt)))
      (cours-i-groupe alt))))
  


;;; aiguille sur la bonne procedure de gestion suivant
;;; le type du groupe
;;; MOD RM - 26/03/96 : ajout de l'alternance

(defun mise-a-jour-groupe-special (groupe)
  (let
    ((type (type-macro-i-groupe groupe)))
    (contraintes-i-groupe groupe 
      (or
        (type-groupe2contraintes-groupe type)
        (contraintes-i-groupe groupe)))
    (cond
      ((equal type #Mv_groupe-enseignement)
        (maj-enseignement groupe))
      ((or (equal type #Mv_groupe-td-a) (equal type #Mancien-v_groupe-td-a))
        (maj-groupe-td groupe))
      ((or (equal type #Mv_groupe-td-b)  (equal type #Mancien-v_groupe-td-b))
        (maj-groupe-td groupe))
      ((equal type #Mv_groupe-alternance)
	(maj-alternance groupe))
      (t ()))))

;;; suppression des cours d'un groupe

(defun suppression-cours-de-groupe (groupe)
  (mapc
    (lambda (c)
      (supprimer-i-cours-special c ())
      (when MODULE-RESEAU
	(debut-modification SITE c SUPPRESSION)))
  (cours-i-groupe groupe)))

;;; suppression d'un groupe
;;; pour des groupes generateurs, cela signifie aussi, supprimer
;;; les cours generes
;;; MOD - FD - 19/02/96 - supprimer les sous fam. des familles de TD
;;; (ex. la fam. contenant les ajout et suppression de grp de TDs)
;;; MOD RM - 27/03/96 : supprimer les cours d'une famille alternance

(defun suppression-groupe-special (groupe)
  (let ((type (type-macro-i-groupe groupe)))
    (mapc (lambda (gr)
      (when (memq groupe (groupes-i-groupe gr))
        (groupes-i-groupe gr (delq groupe (groupes-i-groupe gr)))))
    Liste-I-Groupe)
    (cond
      ((equal type #Mv_groupe-enseignement)
        (suppression-cours-de-groupe groupe))
      ((equal type #Mv_groupe-alternance)
	(suppression-cours-de-groupe groupe))
      ((or (equal type #Mv_groupe-td-a) (equal type #Mancien-v_groupe-td-a))
        (suppression-cours-de-groupe groupe)
	;; MOD FD 19/02/96
	(mapc
	  (lambda (grp)
	    (supprimer-i-groupe-special grp ()))
	  (groupes-i-groupe groupe)))
      ((or (equal type #Mv_groupe-td-b)  (equal type #Mancien-v_groupe-td-b))
        (suppression-cours-de-groupe groupe)
	;; MOD FD 19/02/96
	(mapc
	  (lambda (grp)
	    (supprimer-i-groupe-special grp ()))
	  (groupes-i-groupe groupe)))
      (t (mapc (lambda (cours)
        (groupes-i-cours cours (delq groupe (groupes-i-cours cours))))
      (cours-i-groupe groupe))))))


;;; renvoie le libelle d'un instant interne
;;; MOD - FD - 29/07/96 semaines calendaires

(defun libelle-instant (x)  
  (let 
    ((horaire (horaire-instant x))
      (semaine (ecrire-numero-semaine (semaine-instant x)))
      (jour (jour-instant x)))
    (catenate
      (libelle-jour jour) " à "
      (interne2naturel horaire) " (semaine " semaine ")")))


;;; Libelles de Jours associes aux numeros

;;; !!!!! c'est dorenavant du message
;;; !!!!! bug: probleme des jours = '(lundi mercredi vendredi)
;;; !!!!! equivalent pour moment, instant a '(0 1 2) et non '(0 2 4)



(defun libelle-jour (n)
  (selectq n
    (0 'lundi)
    (1 'mardi)
    (2 'mercredi)
    (3 'jeudi)
    (4 'vendredi)
    (5 'samedi)
    (6 'dimanche)))

;;; Transformation d'un libelle en numero

(defun jour-libelle (x)
  (selectq x
    (lundi 0)
    (lun 0)
    (mon 0)
    (monday 0)
    (mardi 1)
    (mar 1)
    (tuesday 1)
    (tue 1)
    (mercredi 2)
    (mer 2)
    (wed 2)
    (wednesday 2)
    (jeudi 3)
    (jeu 3)
    (thu 3)
    (thursday 3)
    (vendredi 4)
    (ven 4)
    (fri 4)
    (friday 4)
    (samedi 5)
    (sam 5)
    (saturday 5)
    (sat 5)
    (dim 6)
    (dimanche 6)
    (sun 6)
    (sunday 6)
    (t (progn (cons-erreur 'errAlias ()) ()))))

;;; renvoie le numero du jour associe a la grille no-grille

(defun jour-grille (no-grille)
  (when (le no-grille (length Jours-Aff))
    (nth no-grille Jours-Aff)))

;;; revoie le libelle d'un moment cumule

(defun libelle-moment-cumule (moment)
  (let ((jour (jour-moment-cumule moment))
    (horaire (horaire-moment-cumule moment)))
  (catenate (libelle-jour jour) " - " (cumule2naturel horaire))))

;;; renvoie le moment cumule dont le libelle est lib-moment  

(defun libelle-moment2moment-cumule (lib-moment)
  (let* ((tiret (index " - " lib-moment))
    (lib-jour (and tiret (substring lib-moment 0 tiret)))
    (lib-horaire
      (and tiret (substring lib-moment (add 3 tiret)
        (sub (slength lib-moment) (add 3 tiret))))))
  (when (and lib-horaire lib-jour)
    (cons-moment-cumule
      (jour-libelle (symbol () lib-jour))
      (naturel2cumule lib-horaire)))))

;;; renvoie l'intervalle de moment cumules dont le libelle est lib

(defun libelle-intervalle2intervalle (lib)
  (let* ((slash (index "/" lib))
    (lib1 (and slash (substring lib 0 slash)))
    (lib2 (and slash
      (substring lib (add1 slash) (sub (slength lib) (add1 slash)))))
    (inf (libelle-moment2moment-cumule lib1))
    (moment-sup (naturel2cumule lib2)))
  (when (and inf moment-sup)
    (creer-intervalle inf
      (cons-moment-cumule (jour-moment-cumule inf) moment-sup)))))


;;; fonction de tri de moments cumules

(defun tri-moment-cumule (moment1 moment2)
  (le moment1 moment2))

;;; <tous-les-sous-groupes>
;;; renvoie une liste applatie de tous les sous-groupes d'un groupe
;;; min (22/05/95) mo: attention aux yeux si la structure du groupe boucle !!! 
;;; rem (22/05/95) mo: la liste finale peut comporter des doublons !!!

(defun tous-les-sous-groupes (groupe)
  (cond
    ((null groupe) ())
    ((null (groupes-i-groupe groupe)) ())
    (t
      (apply 'append
        (groupes-i-groupe groupe)
        (mapcar 'tous-les-sous-groupes (groupes-i-groupe groupe))))))

;;; CRE RM - 27/09/96 : liste de tous les sous-groupes d'une liste
;;; de famille (sans doublons)

(defun liste-tous-les-sous-groupes (liste)
  (let
    ((nouvelle-liste ()))
    (mapc
      (lambda (groupe)
	(setq nouvelle-liste
	  (append nouvelle-liste (tous-les-sous-groupes groupe))))
      liste)
    (supprimer-occurences-multiples nouvelle-liste)))

;;; fonction de tri d'un intervalle

(defun tri-intervalle (int1 int2)
  (le (borne-inf-intervalle int1) (borne-inf-intervalle int2)))

;;; renvoie le libelle d'un intervalle

(defun intervalle2libelle (int)
  (when int
    (catenate
      (libelle-moment-cumule (borne-inf-intervalle int))
      "/"
      (cumule2naturel (horaire-moment-cumule (borne-sup-intervalle int))))))

;;; predicat renvoie t si tous les cours sont instancies

(defun probleme-resolu-p ()
  (every 'instant-trouve-i-cours Liste-I-Cours))

;;; fonction de demande figer-salle ?

(defun demande-figer-salle (sing warn)
  (prompt-oui-non
    (if sing #Mv_figer-salle #Mv_figer-salles)))

;;; CRE RM - 24/07/95
;;; Fonction pour le choix d'une semaine dans l'annulation de cours

(defun liste-semaines-seance (cours)
  (let ((out ())
        (liste-semaines-deja-annulees ())
        (semaine-depart (semaine-instant (instant-trouve-i-cours cours))))
    (setq liste-semaines-deja-annulees
      (mapcar
        (lambda (annul)
	  (when (eq (cours-i-annulation annul) cours)
            (semaine-i-annulation annul)))
        liste-i-annulation))
    (for (i semaine-depart
          1
          (1- (+ (nb-seances-i-cours cours) semaine-depart))
          t)
         (when (not (member (string i) liste-semaines-deja-annulees))
           (newl out i)))
    (mapcar 'string (ecrire-liste-numeros-semaine (reverse out)))))

;;; AFFICHABILITE D'UN AJOUT DE SEANCE
;;; AJT (27/07/95) RM
;;; calcul de l'affichabilite d'un ajout de seance


;;; predicat general d'affichage de l'ajout de seance
;;; il faut que par nature elle soit affichable ainsi que pour le display

(defun ajout-affichable-p (ajout)
  (and
    (or 
      (afficher-ajout-donnees-p ajout)
      (appartenir-liste ajout Liste-Ajouts-Aff)) ;;; AJT FD-25/05/95
    (afficher-ajout-display-p ajout)))

;;; MOD 16/08/95 RM
;;; teste si l'ajout est dans la salle d'affichage ou la famille 
;;; MOD RM - 05/12/95 : Affichage des ajouts pour l'affichage prof

(defun afficher-ajout-donnees-p (ajout)
  (or
    (and Liste-Salles-Aff (afficher-ajout-salle-p ajout))
    (and Liste-Groupes-Aff (afficher-ajout-groupe-p ajout))
    (and Liste-Profs-Aff (afficher-ajout-prof-p ajout))))


;;; teste si la salle de l'ajout de seance est a l'affiche 

(defun afficher-ajout-salle-p (ajout)
  (and 
    (memq (salle-i-ajout ajout) Liste-Salles-Aff)
    t))

;;; AJT RM 16/08/95
;;; teste si le groupe de l'ajout de seance est a l'affiche 

(defun afficher-ajout-groupe-p (ajout)
  (let ((groupes (groupes-i-ajout ajout)))
   (and (car (supprimer-occurences-vides
                 (mapcar (lambda (g) 
                           (memq g Liste-Groupes-Aff))
                     groupes)))
       t)))

;;; CRE RM - 05/12/95 : affichage d'un ajout pour l'affichage prof

(defun afficher-ajout-prof-p (ajout)
  (let ((profs (profs-i-ajout ajout)))
   (and (car (supprimer-occurences-vides
                 (mapcar (lambda (p) 
                           (memq p Liste-Profs-Aff))
                     profs)))
       t)))

;;; teste si l'ajout de seance est affichable pour le display 

(defun afficher-ajout-display-p (ajout)
  (let* 
    ((instant (instant-i-ajout ajout))
     (semaine-ajout (semaine-instant instant))
     (horaire-cumule (interne2cumule (horaire-instant instant)))
     (jour (jour-instant instant)))
    (and
      (memq jour Jours-Aff)
      (memq horaire-cumule Heures-Debut-Aff)
      (if 
        (not Week-aff)
        (and
          (ge semaine-ajout Semaine-Debut-Aff)  ; non strict parce que 
          (le semaine-ajout Semaine-Fin-Aff))   ; ferme a droite 
        (eq week-aff semaine-ajout))
      t)))

;;; CRE RM 020895

(defun seance-annulee-p (cours semaine)
  (let* ((i 0)
         (annul (nth i liste-i-annulation)))
    (until 
        (or
          (and 
	    ; MOD FD 13/02/96
	    ; (eq (code-cours-i-annulation annul) (code-i-cours cours))
	    (eq (cours-i-annulation annul) cours)
            (eq (string2number (semaine-i-annulation annul)) semaine))
          (not (nth i liste-i-annulation)))
      (progn
        (setq i (+ i 1))
        (setq annul (nth i liste-i-annulation))))
    (and (< i (length liste-i-annulation)) t)))


;;;  <liste-annulations>
;;; CRE - FD - 20/09/95
;;; retourne la liste des annulation d'un cours
(defun liste-annulations (cours)
  (let
    ((liste-travail))

    (mapc
      (lambda (annul)
	(if
	  ; MOD FD 13/02/96
	  ; (eq (code-cours-i-annulation annul) (code-i-cours cours))
	  (eq (cours-i-annulation annul) cours)
	  (newl liste-travail annul)))
      Liste-I-Annulation)
    liste-travail))

;;; <liste-ajouts>
;;; CRE - FD - 20/09/95
;;; retourne la liste des ajouts d'un cours
(defun liste-ajouts (cours)
  (let
    ((liste-travail))

    (mapc
      (lambda (ajout)
	(if
          ; MOD FD 13/02/96
	  ; (eq (cours-rattrape-i-ajout ajout) (code-i-cours cours))
	  (eq (cours-rattrape-i-ajout ajout) cours)
	  (newl liste-travail ajout)))
      Liste-I-Ajout)
    liste-travail))
  

;;; <supprimer-annulations-ajouts-cours>
;;; supprime tous les ajouts et toutes les annulations de seance d'un cours
(defun supprimer-annulations-ajouts-cours (cours)
  (supprimer-annulations-cours cours)
  (supprimer-ajouts-cours cours))

;;; <supprimer-annulations-cours>
;;; supprime toutes les annulation d'un cours
(defun supprimer-annulations-cours (cours)
  (let
    ((annulations (liste-annulations cours)))
    (mapc
      (lambda (annul)
	(supprimer-i-annulation annul ()))
      annulations)))

;;; <supprimer-ajouts-cours>
;;; supprime toutes les ajouts de seances d'un cours
(defun supprimer-ajouts-cours (cours)
  (let
    ((ajouts (liste-ajouts cours)))
    (mapc
      (lambda (ajout)
	(supprimer-i-ajout ajout ()))
      ajouts)))


;;; <tous-les-i-cours-fils>
;;; arg: i-groupe
;;; FD A PLACER au bon endroit ...
;;; on veut tous les i-cours d'un i-groupe sans doublon d'un maniere rapide
;;; et elegante

(defun tous-les-i-cours-fils (groupe)
  (tous-les-i-cours-fils-iter groupe ()))


;;;  on considere que la structure des groupe est arborigene !!!
(defun tous-les-i-cours-fils-iter (groupe liste)
  (let
    ((liste-groupe (groupes-i-groupe groupe))
      (liste-finale liste)) 
    (mapc
      (lambda (c)
        (ifn
          (appartenir-liste (code-i-cours c) liste-finale)
          (newl liste-finale (code-i-cours c))))  ; MOD FD 29/06/95 liste des codes
                                   ; et non des objets
      (cours-i-groupe groupe))
    (while
      liste-groupe
      (setq liste-finale
        (tous-les-i-cours-fils-iter (nextl liste-groupe) liste-finale)))
    liste-finale))

;;; <tous-les-i-groupes-peres>
;;; arg: cours
;;; reult: liste des groupes 'liés' a ce cours
;;; CRE - FD - 03/08/95
;;; A PLACER au bon endroit
(defun tous-les-i-groupes-peres (cours)
  (let 
    ((liste-finale))
    (mapc
      (lambda (g)
	(if 
	  (and
	    (not (appartenir-liste (code-i-groupe g) liste-finale))
	    (appartenir-liste (code-i-cours cours) (tous-les-i-cours-fils g)))
	  (newl liste-finale (code-i-groupe g))))
      liste-i-groupe)
    liste-finale))

;;; <supprimer-i-type-cours-special> - FD CRE 07/12/95
;;; redefinition de la fonction de suppression pour les types de cours
;;; on peut, soit supprimer tous les cours associes a ce type de cours, soit
;;; changer le type de cours de ces cours
;;; 30/01/96 - FD - verifier que ce n'est pas le dernier type de cours

(defun supprimer-i-type-cours-special (code)
  (let
    ((choix))
    ;       (if (and code (> (length liste-i-type-cours) 1))
    ; 	(demande-choix v_i-remplacer-type v_i-supprimer-cours))))
    (when
      (and
	code
	(> (length liste-i-type-cours) 1)
	;; AJT - FD - 23/07/96
	(type-cours-autorise-p code))
      (setq choix (demande-choix v_i-remplacer-type v_i-supprimer-cours)))

    (cond
      ((equal choix #Mv_i-remplacer-type) (remplacer-type-cours code))
      ((equal choix #Mv_i-supprimer-cours) (supprimer-cours-type code))
      (t
	()))))

;;; <modifier-i-type-cours-special>
;;; CRE - FD - 27/03/96
(defun modifier-i-type-cours-special (code)
  t)

;;; <ajouter-i-type-cours-special>
;;; CRE - FD - 23/04/96
;;; met a jour la liste des types de cours autorises pour le site

(defun ajouter-i-type-cours-special ()
  (let*
    ((type-cours (ajouter-i-type-cours))
     (code-type-cours
       (car
	 (last
	   (libelle-i-type-cours2codes-i-type-cours type-cours)))))
    
    (when type-cours
      (maj-lst-types-cours-autorises code-type-cours))
    (list code-type-cours)))
  

;;; <supprimer-cours-type> CRE - 13/12/95
;;; supprimer tous les cours d'un type donne
;;; MOD RM - 04/12/96 : il faut aussi supprimer les familles
;;; generatrices qui ont ce type de cours !

(defun supprimer-cours-type (code-type-cours)
  (let
    ((confirm
      (afficher-confirmation
	(column
	  #Mv_supprimer-cours-type
	  (libelle-i-type-cours code-type-cours)))))
    (when confirm
      (mapc
	(lambda (famille)
	  (if (eq (type-cours-i-groupe famille) code-type-cours)
	    (supprimer-i-groupe-special famille ())))
	Liste-I-Groupe)
      (mapc
	(lambda (cours)
	  (if (eq (type-i-cours cours) code-type-cours)
	    (supprimer-i-cours cours ())))
	Liste-I-Cours)
      ;; pb i-restore retablira les ancienne valeur si on clique
      ;; sur Annuler dans la fenetre info de l'etablissement
      (setq Liste-I-Type-Cours
	(delq (eval code-type-cours) Liste-I-Type-Cours))
      ;; AJT - FD - 26/07/96 - mise a jour de types de cours pour les sites
      (mapc
	(lambda (s)
	  (types-cours-i-site s
	    (supprimer-occurence code-type-cours (types-cours-i-site s))))
	Liste-I-Site))
    confirm))

;;; <echanger-type-cours>  CRE - 13/12/95
;;; remplace un type de cours donne par un autre dans la liste des cours
;;; MOD RM - 04/12/96 : il faut aussi remplacer les familles
;;; generatrices qui ont ce type de cours !

(defun remplacer-type-cours (code-ancien-type)
  (let
    ((nouveau-type
      (demande-filtree-i-type-cours
	(catenate "remplacer le type "
	  (libelle-i-type-cours code-ancien-type) " par :")
	()
	()
	'(lambda (x) (neq code (code-i-type-cours x)))))
    (confirm))
    (setq confirm
      (and
	nouveau-type
	(afficher-confirmation
	  (column
	    "Remplacer le type "		
	    (libelle-i-type-cours code-ancien-type)
	    "par le type "
	    (libelle-i-type-cours nouveau-type)))))
    (when confirm
      (mapc
	(lambda (famille)
	  (if (eq (type-cours-i-groupe famille) code-ancien-type)
	    (type-cours-i-groupe famille (code-i-type-cours nouveau-type))))
	Liste-I-Groupe)
      (mapc
	(lambda (cours)
	  (if (eq (type-i-cours cours) code-ancien-type)
	    (type-i-cours cours (code-i-type-cours nouveau-type))))
	Liste-I-Cours)
      (setq Liste-I-Type-Cours
	(delq (eval code-ancien-type) Liste-I-Type-Cours)) ;; BEURK!! un eval
      ;; AJT - FD - 26/07/96 - mise a jour de types de cours pour les sites
      (mapc
	'(lambda (s)
	  (types-cours-i-site
	    s
	    (supprimer-occurence code-ancien-type (types-cours-i-site s))))
	Liste-I-Site))
    confirm))

;;; CRE RM - 20/09/96 : suppression des annulations et ajouts d'un cours

(defun supprimer-annulation-cours (cours)
  (mapc
    (lambda (annul)
      (when (eq cours (cours-i-annulation annul))
	(supprimer-i-annulation annul ())))
    liste-i-annulation))

(defun supprimer-ajout-cours (cours)      
  (mapc
    (lambda (ajt)
      (when (eq cours (cours-rattrape-i-ajout ajt))
	(supprimer-i-ajout ajt ())))
    liste-i-ajout))

  