;;; --------------------------------------------------------------------------
;;; - Fonctions de controle d'integrites                        VARUNA 1.2   -
;;; - Objets cours/profs/groupe/salle/ecole                                  -
;;; - Et actions associees                                                   -
;;; --------------------------------------------------------------------------
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; - Creation:  Novembre 94                                                 -
;;; - Mise a jour pour version 1.10: Novembre                                -
;;; - Build 1.22.1 01.07.99 / JH MO                                          -
;;; --------------------------------------------------------------------------

;;; DOC (22/07/95) MO FD
;;; Le controle des donnees est un sujet delicat. En effet, nous avons un ens.
;;; de concepts qui forment un graphe par leur relations
;;; les concepts: cours, familles, salles, profs et depuis peu resa, raco
;;; exemple de relations: domaine a restreindre, j'enseigne, j'ai lieu en....
;;; le controle des donnees se fait a plusieurs niveaux: 0, 1, 2 et n
;;; (franck comprendra les notations)
;;; 
;;; niveau 0
;;;
;;; controle de l'integrite d'un objet de concept cours C
;;;     . dispo-cours permettant un rectangle duree et nb-seances
;;; controle de l'integrite d'un objet de concept prof P
;;;     . pour l'instant rien
;;;     . on pourrait envisager que l'utilisateur desire que le prof soit au
;;;     dispo un certain nombre de journees/semaines
;;; controle de l'integrite d'un objet de concept famille F
;;;     . pour l'instant rien
;;;     . on pourrait envisager que l'utilisateur desire que la famille soit
;;;     au moins dispo un certain nombre de journees
;;; controle de l'integrite d'un objet de concept salle S
;;;     . pour l'instant rien
;;; controle de l'integrite d'un objet de concept ecole E
;;;     . semaines de debut appartiennent au semaines d'ouverture
;;;     . jours ouvr‚s sont des jours ouvrables...etc...
;;; 
;;; niveau 1
;;;
;;; ce sont les relations (symetriques!) d'un objet a l'autre que l'on controle
;;;
;;; pour le moment:     CE, CP, CF, PE, FF, FE
;;; a venir ?           CS, PS, FS, SE
;;; n'ont pas de sens?  CC, PP, SS, EE, PF
;;;
;;; niveau 2
;;;
;;; le controle au niveau 2 est un controle de l'integrite d'un objet
;;; connaissant toutes ses relations au niveau1
;;;
;;; a completer
;;;
;;; niveau n
;;;
;;; verification connaissant tous les objets et la structure de graphe? arbre? 
;;; les reliant
;;; 
;;; a completer


;;; CONTROLES INTEGRITE GLOBALE

;;; controle d'integrite global
;;; verifications :
;;; de l'existence d'un type de salle donnee pour un cours
;;; des compatibilites de disponibilites pour tous les objets
;;; de l'integrite d'un groupe (cf groupe-ok)
;;; MOD FD - 24/05/95
;;;    dispo-compatibles-cours -> dispo-compatibles-cours-p

(defun controle-integrite ()
  (let
    ;;  rem MO: il faut une primitive <donner-liste-type-salle> (elle existe?)
    ;;          et surtout ne pas construire inutilement une gross liste !!!
    ((types-salle
      (supprimer-occurences-multiples
        (mapcar 'type-i-salle Liste-I-Salle)))
    ;; rem MO: et une construction de liste en plus !!!
    (l-cours-faux (cours-duree-incompatibles Liste-I-Cours)))
    (mapc
      (lambda (cours)
        (when
          (not (dispo-compatibles-cours-p cours))
          (cons-erreur 'errDispoCours (code-i-cours cours)))
        (when
          ;; AJT - FD - 01/03/96 - MultiP
          ;; si on n'a pas le moteur salle on a de verification sur les salles
          (and
            MODULE-SALLES
            (not (memq (type-salle-i-cours cours) types-salle)))
          (cons-erreur 'errNoType (code-i-cours cours))))
      Liste-I-Cours)
    ;; MOD RM - 04/06/96 : prise en compte du controle d'alternance
    (mapc
      (lambda (groupe)
        (cond
          ((not (groupe-ok groupe))
            (cons-erreur 'errDefGroupe (code-i-groupe groupe)))
          ((not (dispo-alternance-ok-p groupe))
            (cons-erreur 'errAlternanceGroupe (code-i-groupe groupe)))
          (t t)))
      Liste-I-Groupe)
    (when
      l-cours-faux
      (cons-erreur 'errDureeCours
        (mapcar 'code-i-cours l-cours-faux)))))




;;; CONTROLES AUTOUR DU COURS
;;; 


;;; verifie la dispo d'un cours et propose de retourner sur la fenetre
;;; cours si les dispo ne collent pas
;;; MOD FD - 24/05/95
;;;    dispo-compatibles-cours -> dispo-compatibles-cours-p
;;; NET (22/06/95) MO
;;; MOD RM 040895 : mise a jour de la liste des annulations lors d'une
;;; modification d'un cours

(defun controle-i-cours (cours)
  (let
    ((go-on t)
     (return t))
    
    (when (donnee-lecture-seule-p cours t)
      (traiter-erreur)
      (setq go-on ())
      (setq return ()))

    (when (and go-on MODULE-RESEAU)
      (when (equal (autoriser-validation-cours-p cours) ACCES-REFUSE)
	(afficher-attention
	  (column
	    #Mv_modif-acces-refuse
	    (row #Mv_donnee (string (code-i-cours cours)) #Mv_relation)
	    (row #Mv_modifiee2 #Mv_autre-utilisateur)))
	(setq go-on ())
	(setq return ())))

    (when (and go-on (not (dispo-compatibles-cours-p cours)))
      (traiter-erreur))

    (when go-on
      (maj-listes-annuls-ajouts cours))
    return))

(defun autoriser-validation-cours-p (cours)
  (if
    (and
      (every
	(lambda (p)
	  (if (equal (funcall 'debut_modif SITE (send 'objet2string p) MODIFICATION) -1)
	      ()
	      t))
	(profs-i-cours cours))
      (every
	(lambda (f)
	  (if (equal (funcall 'debut_modif SITE (send 'objet2string f) MODIFICATION) -1)
	    ()
	    t))
	(groupes-i-cours cours)))
    ACCES-AUTORISE
    ACCES-REFUSE))

(defun maj-listes-annuls-ajouts (cours)
  (let
    ((code (code-i-cours cours))
     (ancien-code (send 'i-restore (nth 0 (user-appli ed 'user)))))
    (mapc
      (lambda (annul)
        ;  MOD FD 13/02/96
        ; (when (eq ancien-code (code-cours-i-annulation annul))
        (when (eq ancien-code (code-i-cours (cours-i-annulation annul)))
            ; MOD FD 13/02/96 (code-cours-i-annulation annul code)
            (cours-i-annulation annul cours)
            (ifn (eq (libelle-i-annulation annul)
                     (catenate "ANNULATION DE " (libelle-i-cours cours)))
              (libelle-i-annulation annul
                  (catenate "ANNULATION DE " (libelle-i-cours cours))))))
        liste-i-annulation)
  (mapc
      (lambda (ajout)
        ; MOD FD 13/02/96
        ; (when (eq ancien-code (cours-rattrape-i-ajout ajout))
        (when (eq ancien-code (code-i-cours (cours-rattrape-i-ajout ajout)))
            ; MOD FD 13/02/96
            ; (cours-rattrape-i-ajout ajout code)
            (cours-rattrape-i-ajout ajout cours)
            (ifn (eq (libelle-i-ajout ajout)
                     (catenate "AJOUT DE SEANCE: " (libelle-i-cours cours)))
              (libelle-i-ajout ajout
                  (catenate "AJOUT DE SEANCE: " (libelle-i-cours cours))))))
      liste-i-ajout)
    (code-i-cours cours code)
    t))
  
  

;;; <dispo-compatibles-cours-p>
;;; rem (22/07/95) MO: i-cours a la place de cours serait plus juste

(defun dispo-compatibles-cours-p (cours)

  (let 
    ((dispo-finale (dispo-i-cours cours))
     (suffisant t))

    ;; 0. dispo du cours
    (when
      (not (dispo-cours-suffisante-p dispo-finale cours))
      (cons-erreur 'errdispoInsuffi (code-i-cours cours))
      (setq suffisant ()))

    ;; 1.intersection de la dispo de l'ecole et du cours
    (when 
      (not (dispo-vide-p dispo-finale))
      (setq dispo-finale
            (intersection-dispo 
                dispo-finale;;;(dispo-i-cours cours) 
                (dispo-ecole-i-ecole I-Ecole)))
      (or 
        (dispo-cours-suffisante-p dispo-finale cours);;;(not(dispo-vide-p dispo-finale))
        (and
            (cons-erreur 'errdispo-ecole (code-i-cours cours))
            (setq suffisant ()))))

    ;; 2. intersection avec la dispo des groupes
    (when (not (dispo-vide-p dispo-finale))
      (setq dispo-finale (intersection-dispo-cours-groupes cours))
      (or 
        (dispo-cours-suffisante-p dispo-finale cours);;;(not(dispo-vide-p dispo-finale))
        (and 
            (cons-erreur 'errdispo-groupe (code-i-cours cours))
            (setq suffisant ()))))
    ;; 3.intersection avec la dispo des profs
    (when 
        (not (dispo-vide-p dispo-finale))
        (setq dispo-finale (intersection-dispo-cours-profs cours))
        (or 
            (dispo-cours-suffisante-p dispo-finale cours);;;(not(dispo-vide-p dispo-finale))
            (and 
                (cons-erreur 'errdispo-prof (code-i-cours cours))
                (setq suffisant ()))))
;    (if *erreurs*
;      ()
;      t)))
    suffisant))


;;; <intersection-dispo-cours-profs>
;;; calcule la dispo resultant de l'intersection de la dispo du cours
;;; et de celle des profs associés a ce cours

(defun intersection-dispo-cours-profs (cours)
  (let
    ((dispo-finale (dispo-i-cours cours)))

    (when
      (and 
        (not (dispo-vide-p dispo-finale))
        (profs-i-cours cours))
      (any 
        '(lambda (prof)
           (setq 
             dispo-finale
             (intersection-dispo (dispo-i-prof prof) dispo-finale))
           (dispo-vide-p dispo-finale))
         (profs-i-cours cours)))
    dispo-finale))


;;; <intersection-dispo-cours-groupes>
;;; calcule la dispo resultant de l'intersection de la dispo du cours
;;; et de celle des groupes associés a ce cours
;;; rq: on ne tient pas compte du fait que les groupes peuvent etre
;;; sous-groupe d'un autre groupe

(defun intersection-dispo-cours-groupes (cours)
  (let 
    ((dispo-finale (dispo-i-cours cours)))
   
    (when
      (and 
        (not (dispo-vide-p dispo-finale))
        (groupes-i-cours cours))
      (any 
        '(lambda (groupe)
           (setq 
             dispo-finale
             (intersection-dispo (dispo-i-groupe groupe) dispo-finale))
           (dispo-vide-p dispo-finale))
         (groupes-i-cours cours)))
    dispo-finale))


;;; !!! fonction pour test
;;; ... en attendant que la fonction soit ecrite...
(defun premier-moment-debut-cours (intervalle type-cours)
  (borne-inf-intervalle intervalle))


;;;  CONTROLE AUTOUR DU GROUPE

;;; <dispo-compatibles-groupe>
;;; meme chose, mais a partir d'un groupe
;;; tous les cours du groupe sont OK
;;; tous les sous-groupes du groupe sont OK
;;; MOD FD - 24/05/95 
;;;    dispo-compatibles-cours -> dispo-compatibles-cours-p
(defun dispo-compatibles-groupe (groupe)
  (and
    (every 'dispo-compatibles-cours-p (cours-i-groupe groupe))
    (dispo-compatibles-groupe-alternance groupe)
    (every 'dispo-compatibles-groupe (groupes-i-groupe groupe))))

  
;;; meme chose en partant d'un prof 
;;; (tous les cours du prof sont OK)
;;; MOD FD - 24/05/95
;;;    dispo-compatibles-cours -> dispo-compatibles-cours-p
(defun dispo-compatibles-prof (prof)
  (every 'dispo-compatibles-cours-p (cours-i-prof prof)))


;;; <dispo-cours-suffisante-p>
;;; fonction verifiant que la dispo passee en parametre est 
;;; compatible avec la duree de ce cours (nb d'heures suffisant 
;;; dans la dispo) et le nombre de seances de ce cours 

(defun dispo-cours-suffisante-p (dispo cours)
  (let
    ((type-cours (type-i-cours cours)))

    (and
      (dispo-suffisante-duree-p 
        (trouver-dispo-moment dispo)
        (duree-i-cours cours)
        type-cours)
      (dispo-suffisante-semaine-p
        (trouver-dispo-semaine dispo)
        (nb-seances-i-cours cours)
        type-cours))))

;;; <groupe-ok>
;;; un groupe est correct si il a au moins un cours ou au moins un groupe

(defun groupe-ok (groupe)
  (or
    (cours-i-groupe groupe)
    (groupes-i-groupe groupe)))

;;; le probleme du controle d'integrite
;;; est lie a la presence d'un bouton annuler
;;; pour pouvoir faire le controle sur l'objet
;;; il faut que les champs de l'objet soient a jour
;;; (c'est a dire, deja sauvegardes)
;;; C'est pourquoi l'action du bouton annuler
;;; des editeur envoie a toutes les zones 
;;; un message restore.


;;; la plupart des procedures de controle
;;; ne sont pas bloquantes
;;; on propose juste de corriger ou de continuer

;;; penser a ajouter le controle du type de salle dans le controle cours
;;; avec par exemple proposition de creer la salle du bon type.

;;; controle global des groupes

;;; AJT RM 12/02/98 : detection de cycles au moment de la validation
;;; de l'editeur de familles pour eviter les explosions de pile !

(defun controle-i-groupe (groupe)
  (let
    ((cycle (graphe-cyclique))
     (retour t))
    (cond
      ((donnee-lecture-seule-p groupe t)
	(traiter-erreur)
	(setq retour ()))
      (cycle
	(erreur-graphe-cyclique cycle)
	(setq retour ()))
      ((not (dispo-compatibles-groupe groupe))
	(traiter-erreur)
	(setq retour t)))
    retour))

;;; CONTROLE AUTOUR DES SALLES

;;; les salles sont toujours OK

(defun controle-i-salle (salle)
  ;; AJT - 29/02/96 - FD MultiP
  (if (donnee-lecture-seule-p salle t)
    (progn
      (traiter-erreur)
      ())
    t))


;;; CONTROLE AUTOUR DU PROF


(defun controle-i-prof (prof)
  ;; AJT - 29/02/96 - FD MultiP
  (if (donnee-lecture-seule-p prof t)
    (progn
      (traiter-erreur)
      ())
    (if
      (not (dispo-compatibles-prof prof))
      (and (traiter-erreur) t)
      t)))


;;; CONTROLE AUTOUR DE L'ECOLE

;;; <controle-i-ecole>
;;; verification des donnees de l'ecole :
;;; verifie tous les types de cours de l'ecole

(defun controle-i-ecole (ecole)
  (let ((heures-debut-ok (every 'type-cours-ok (types-cours-i-ecole ecole))))
    (if (not heures-debut-ok)
      (not (afficher-erreur
        (column
          #Mv_heures-debut-vs-dispo
          #Mv_heures-debut-vs-dispo2)))
      t)))

;;; CONTROLE AUTOUR DES TYPES DE COURS

;;; <controle-i-type-cours>
;;; CRE - FD - 08/01/96
;;; les type de cours sont OK
;;; par contre, il faut remettre a jour le champ <types-cours-i-ecole>

(defun controle-i-type-cours (type-cours)
  ;; AJT - 29/02/96 - FD MultiP
  (if (donnee-lecture-seule-p type-cours t)
    (progn
      (traiter-erreur)
      ())
    t))

;;; <type-cours-ok>
;;; verification des champs d'un type de cours
;;; les horaires de debut de cours doivent etre
;;; inclus dans les horaires d'ouverture de l'ecole (dispo)

(defun type-cours-ok (type-cours)
  (let ((dispo-ecole (dispo-i-ecole I-ECOLE)))
    (every (lambda (moment-cumule)
      (appartenir-zone-p moment-cumule dispo-ecole))
        (liste-moments-cumules-i-type-cours type-cours))))


;;;  UTILITAIRES FACTORISES
;;;  peut-etre pour certains a mettre dans dispo.ll i-dispo.ll

;;; fonction verifiant qu'un type de cours et une duree, la dispo
;;; (de moments) fournit est suffisante pour qu'un cours de ce type (fait ref
;;; a des heures de debut) et dont la duree est <duree>, puisse
;;; avoir lieu 

(defun dispo-suffisante-duree-p (dispo-moment duree type-cours)
  (let
    ((intervalle (premier-intervalle-zone dispo-moment)))

    (cond
      ((zone-vide-p dispo-moment) ())
      ((intervalle-suffisant-p 
         intervalle
         (premier-moment-debut-cours intervalle type-cours)
         duree)
       t)
      (t
        (dispo-suffisante-duree-p
          (reste-zone dispo-moment)
          duree
          type-cours)))))


       
;;; cette fonction verifie qu'etant donne un intervalle, un moment
;;; et une duree; l'intervalle moment,moment+duree et bien inclu dans 
;;; l'intervalle donne
;;; rq: si intervalle vide retourne ().
(defun intervalle-suffisant-p (intervalle moment duree)          
  (and 
    (not (intervalle-vide-p intervalle))
    (appartenir-intervalle-p moment intervalle)
    (appartenir-intervalle-p 
        (sub1 (add moment duree))
        intervalle)))

;;; fonction verifiant qu'un type de cours et une duree, la dispo
;;; (en semaines) fournit est suffisante pour qu'un cours de ce type (fait ref
;;; a des heures de debut) et dont la duree est <duree>, puisse
;;; avoir lieu 
;;; rq: on ne tient pas compte pour l'instant du type de cours
;;;     il pourra etre utilise utltérieurement pour détermine etant
;;;     donne un intervalle de semaines et un type de cours quel est
;;;     la premiere semaine de cet intervelle a partir duquel ce
;;;     type de cours peut commencer.
(defun dispo-suffisante-semaine-p (dispo-semaine nb-seances type-cours)
  (let
    ((intervalle (premier-intervalle-zone dispo-semaine)))
    (cond
      ((zone-vide-p dispo-semaine) ())
      ((semaine-debut-possible-p 
         intervalle
         (borne-inf-intervalle intervalle);; pour etre modifie si on
                                          ;; tient compte du type de
                                          ;; de cours
         nb-seances)
       t)
      (t
        (dispo-suffisante-semaine-p
          (reste-zone dispo-semaine)
          nb-seances
          type-cours)))))
       
;;; cette fonction verifie qu'etant donne un intervalle, une semaine
;;; et un nb de seances; l'intervalle semaine, semaine+nb-seances et 
;;; bien inclu dans l'intervalle donne
(defun semaine-debut-possible-p (intervalle semaine nb-seances)          
  (and 
    (not (intervalle-vide-p intervalle))
    (appartenir-intervalle-p semaine intervalle)
    (appartenir-intervalle-p 
        (sub1 (add semaine nb-seances))
        intervalle)))


;;; fonction de verification de la duree des cours
;;; cette fonction n'est pas definie en predicat sur un unique cours
;;; car elle fait appel a (duree-max)
;;; qui serait calculee a chaque fois lors d'un controle global
;;; renvoie la liste de cours concernes

(defun cours-duree-incompatibles (l-cours)
  (let
    ((maximum (duree-max)))
    (sous-liste
      (lambda (cours)
        (gt (duree-i-cours cours) maximum))
      l-cours)))

;;; verification des champs d'un type de cours
;;; les horaires de debut de cours doivent etre
;;; inclus dans les horaires d'ouverture de l'ecole (dispo)
;;; FD - 10/01/96 - cette fonction est deja definie ci-dessus

; (defun type-cours-ok (type-cours)
;   (let ((dispo-ecole (dispo-i-ecole I-ECOLE)))
;     (every (lambda (moment-cumule)
;       (appartenir-zone-p moment-cumule dispo-ecole))
;         (liste-moments-cumules-i-type-cours type-cours))))

;;; *****************************
;;; verification des donnees de l'ecole :
;;; verifie tous les types de cours de l'ecole

;;; FD - 05/01/96 - ach,ach cette est DEJA DEFINIE plus haut
;;; -> je la met en commentaire 
; (defun controle-i-ecole (ecole)
;   (let ((heures-debut-ok (every 'type-cours-ok (types-cours-i-ecole ecole))))
;     (if (not heures-debut-ok)
;       (not (afficher-warning 
;         (column
;           #Mv_heures-debut-vs-dispo
;           #Mv_heures-debut-vs-dispo2)))
;       t)))


;;; CRE RM - 04/06/96 : booleen controlant la dispo d'une famille alternance

(defun dispo-alternance-ok-p (groupe)
  (let
    ((dispo (dispo-i-groupe groupe))
     (type-cours (type-cours-i-groupe groupe))) 
    (or
      (not (equal (type-macro-i-groupe groupe) #Mv_groupe-alternance))
      (and
        (dispo-suffisante-duree-p 
          (trouver-dispo-moment dispo)
          (duree-i-groupe groupe)
          type-cours)
        (dispo-suffisante-semaine-p
          (trouver-dispo-semaine dispo)
          (- (* (nb-seances-i-groupe groupe) (nb-td-i-groupe groupe)) 1)
          type-cours)))))

(defun dispo-compatibles-groupe-alternance (groupe)
  (when (not (dispo-alternance-ok-p groupe))
    (cons-erreur 'errGroupeAlternance (code-i-groupe groupe)))
  (if *erreurs*
    ()
    t))

;;; AJT - FD - 03/06/96
;;; <controle-i-site>
;;; fonction de controle de l'editeur

(defun controle-i-site (site)
  (let
    ((num-dd (numero-dd-i-site site))
     (code (code-i-site site)))
    (if
      (every
	(lambda (s)
	  (or
	    (eq code (code-i-site s))
	    (not (equal (numero-dd-i-site s) num-dd))))
	Liste-I-Site)
      t
      (afficher-erreur "Un autre site est défini par ce numéro de disque.")
      ())))

;;; AJT - FD - 30/07/96
;;; <controle-i-calendaire>
;;; fonction de controle de l'editeur
(defun controle-i-calendaire (calend)
  t)

