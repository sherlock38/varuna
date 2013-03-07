;; ---------------------------------------------------------------------------
;;; - i-lire.ll                                                 VARUNA 1.2   -
;;; - Interface                                                              -
;;; - Module de lecture des donnees                                          -
;;; --------------------------------------------------------------------------
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; - Creation:  septembre 94                                                -
;;; - Mise a jour pour version 1.09: Septembre                               -
;;; - Build 1.22.1 01.07.99 / MO                                             -
;;; --------------------------------------------------------------------------


;;; supprimer un objet existant. Le code ne pointe plus sur l'objet

(defun detruire-lien (symbole)
  (and symbole (remob symbole)))

;;; supprime tous les liens code <-> objet pour toutes les donnees
;;; de l'interface et remet les liste a vide

(defun free-interface ()
  (mapc					; liberation des cours
    (lambda (c)
      (detruire-lien (code-i-cours c)))
    liste-i-cours)
  (mapc					; liberation des profs
    (lambda (p)
      (detruire-lien (code-i-prof p)))
    liste-i-prof)
  (mapc					; liberation des familles
    (lambda (g)
      (detruire-lien (code-i-groupe g)))
    liste-i-groupe)
  (mapc					; liberation des salles
    (lambda (s)
      (detruire-lien (code-i-salle s)))
    liste-i-salle)
  (mapc					; liberation des types de cours
    (lambda (c)
      (detruire-lien (code-i-type-cours c)))
    liste-i-type-cours)
  (mapc					; liberation de l'ecole
    (lambda (e)
      (detruire-lien (code-i-ecole e)))
    Liste-I-Ecole)
  (when MODULE-RESA
    (mapc				; liberation des resas
      (lambda (r)
	(detruire-lien (code-i-resa r)))
      liste-i-resa))
  (when MODULE-RACO
    (mapc				; liberation des annulations
      (lambda (ann)
	(detruire-lien (code-i-annulation ann)))
      liste-i-annulation)
    (mapc				; liberation des ajouts
      (lambda (ajt)
	(detruire-lien (code-i-ajout ajt)))
      liste-i-ajout))
  (when MODULE-GESTSITE
    (mapc				; liberation des sites
      (lambda (s)
	(detruire-lien (code-i-site s)))
      Liste-I-Site))

  (initialiser-donnees)
  (gc))

;;; Met toutes les listes de donnees a ()

(defun initialiser-donnees ()
  (setq Liste-I-Cours ())
  (setq Liste-I-Prof ())
  (setq Liste-I-Salle ())
  (setq Liste-I-Groupe ())
  (setq Liste-Contraintes ())
  (setq Liste-I-Ecole ())
  (setq Liste-I-Resa ())
  (setq Liste-I-Ajout ()) 
  (setq Liste-I-Annulation ())
  (setq Liste-I-Type-Cours ())
  (setq I-ECOLE ())
  (setq Liste-I-Site()))

;;; evaluer donnes evalue tous les champs
;;; lus comme symboles qui pointent sur des objets
;;; cours, profs ou groupe, ou salle-trouvee
;;; !! il faut faire plusieurs fonctions FD !!

(defun evaluer-donnees ()
                                        ; dabord les profs
  (mapc (lambda (p)
    (cours-i-prof p (evlis1 (cours-i-prof p)))
    (mapc (lambda (c)
      (profs-i-cours c (cons p (profs-i-cours c))))
    (cours-i-prof p)))
  Liste-i-Prof)
  ; ensuite les groupes
  (mapc (lambda (g)
    (cours-i-groupe g (evlis1 (cours-i-groupe g)))
    (groupes-i-groupe g (evlis1 (groupes-i-groupe g)))
    (profs-i-groupe g (evlis1 (profs-i-groupe g)))
    (editeur-i-groupe g
      (type-groupe2editeur-groupe (type-macro-i-groupe g)))
    (mise-a-jour-groupe-special g)
    (mapc
      (lambda (c)
	(when (not (memq g (groupes-i-cours c)))
           (groupes-i-cours c 
             (cons g (groupes-i-cours c)))))
      (cours-i-groupe g)))
  Liste-I-Groupe)

  (mapc (lambda (c)
    (when 
      (and
        (salle-trouvee-i-cours c)
        (boundp (salle-trouvee-i-cours c)))
      (when (structurep (eval (salle-trouvee-i-cours c)))
        (salle-trouvee-i-cours c (eval (salle-trouvee-i-cours c))))))
          Liste-I-Cours)
                                    ; ensuite les resas
  ;; xxx FD - 30/05/95 ajouter (when (MODULE-RESA
  (mapc 
    (lambda (r)
      (and
        (salle-i-resa r)
        (boundp (salle-i-resa r))
        (structurep (eval (salle-i-resa r)))
        (salle-i-resa r (eval (salle-i-resa r)))))
    Liste-I-Resa)

  ; ensuite les AJOUTS

  (mapc 
    (lambda (a)
      (profs-i-ajout a (evlis1 (profs-i-ajout a)))
      (and
        (salle-i-ajout a)
        (boundp (salle-i-ajout a))
        (structurep (eval (salle-i-ajout a)))
        (salle-i-ajout a (eval (salle-i-ajout a))))
      (and
        (cours-rattrape-i-ajout a)
        (boundp (cours-rattrape-i-ajout a))
        (structurep (eval (cours-rattrape-i-ajout a)))
        (cours-rattrape-i-ajout a (eval (cours-rattrape-i-ajout a)))))
    Liste-I-Ajout)

  ; ensuite les ANNULATIONS
  ; AJT FD RM 13/02/96
  (mapc
    (lambda (annul)
      (and
	(code-cours-i-annulation annul)
	(boundp (code-cours-i-annulation annul))
        (structurep (eval (code-cours-i-annulation annul)))
	(cours-i-annulation annul (eval (code-cours-i-annulation annul)))))
    Liste-I-Annulation))



;;; mise a jour du groupe reste permettant d'afficher les cours
;;; sans groupe (ces cours appartiennent a un groupe Groupe-Reste 
;;; creee pour l'occasion, ce groupe a un index -1 de sorte qu'il n'est
;;; jamais ecrit dans le fichier groupe).

(defun mise-a-jour-reste ()
  (when (not Groupe-Reste)
    (let ((gr (nouveau-i-groupe)))
      (code-i-groupe gr 'groupe-reste)
      (libelle-i-groupe gr #Mv_groupe-reste)
      (index-i-groupe gr -1)
      (setq Groupe-Rest gr)))
  (cours-i-groupe Groupe-Reste ())
  (mapc (lambda (cours)
    (when (not (groupes-i-cours cours))
      (cours-i-groupe Groupe-Reste 
        (cons (cours-i-groupe Groupe-Reste) cours))))
          Liste-I-Cours))
    
;;; initialisation des variables d'affichage donnees-dependantes.
;;; chg (02/05/95) MO
;;; je change le liste-groupe-aff pour eviter les (())
;;; AJT FD-03/05/95 
;;; initialisation de Week-Aff a () (on est en affichage annuel)
;;; MOD RM - 19/08/96 : on ouvre Varuna en hebdomadaire !

(defun initialiser-affichage ()
  (maj-liste-salles-aff (list (car Liste-I-Salle)))
  (setq Semaine-Debut-Aff (semaine-debut-i-ecole I-ECOLE))
  (setq Semaine-Fin-Aff (semaine-fin-i-ecole I-ECOLE))
  (setq Jours-Aff Jours-Ouvres)
  (setq Grid-Type 'week)
  (setq Week-Aff Semaine-Debut-Aff)
  (setq Heures-Debut-Aff (horaires-types-cours (types-cours-i-ecole I-ECOLE))))
    

;;; PRIMITIVE de LECTURE GENERALE des seules donnees utiles a l'interface
;;; a l'exception du fichier ecole.edt qui est lu par le moteur
;;; (en fait par le module d'utilitaires)
;;; func est une fonction a deux parametres. Le premier (un symbole)
;;; decrit un nouvel etat, le deuxieme un pourcentage.
;;; cette fonction est appelee entre chaque lecture de fichier


;;; primitive de lecture des donnees de l'ecole
;;; 

(dmd lire-ecole-inter (enr index)
  `(lire-ecole ,enr))

;;;  MOD - RM - FD - 01/09/96 traitement reseaux

(defun lire-donnees-interface (func)
  (if (and MODULE-RESEAU (not (interdire-acces-attente EDT-SEM)))
    (afficher-attention #Mv_acces-edt-interdit)
    (and func (funcall func 'v_init 10))
    (free-interface)
    (and func (funcall func 'v_read 20))
    (lire-fichier FICHIER-ECOLE 'interface)
    (when (car Liste-I-Ecole)
      (and func (funcall func () 30))
      (lire-fichier FICHIER-COURS 'interface)
      (and func (funcall func () 40))
      (lire-fichier FICHIER-PROFS 'interface)
      (and func (funcall func () 50))
      (lire-fichier FICHIER-GROUPES 'interface)
      (and func (funcall func () 60))
      (lire-fichier FICHIER-SALLES 'interface)
      (and func (funcall func () 70))
      (lire-fichier FICHIER-CONTRAINTES 'interface)
      (and func (funcall func () 80))
      (when MODULE-RESA
        (lire-fichier FICHIER-RESA 'interface))
      (and func (funcall func () 85))      
      (when MODULE-RACO
	(lire-fichier FICHIER-AJOUT 'interface) 
        (lire-fichier FICHIER-ANNUL 'interface))
      (and func (funcall func 'v_prep 90))
      (lire-fichier FICHIER-ECOLE 'inter)
      (when MODULE-GESTSITE
	(lire-fichier FICHIER-SITES 'interface))
      (and func (funcall func () 95))
      (evaluer-donnees)
      (and func (funcall func () 98)))
      (when MODULE-RESEAU
	(autoriser-acces EDT-SEM))))
      
;;; primitive de lecture des donnees de l'ecole
;;; 
;;; voir dans i-ecole.ll


;;; primitive de lecture specifique au cours
;;; index est l'ordre de l'enregistrement dans le fichier
;;; cette valeur est gardee en memoire pour l'ecriture

(dmd lire-cours-interface (enr index)
  `(let ,(cdr enr) 
    (when code
      (when (not (boundp code))
        (creer-i-cours
          code
          libelle
          type
          type-salle
          audience
          duree
          nb-seances
          ()
          ()
	  (or dispo DISPO-ECOLE)		;; AJT - FD - 09/07/96
          ()
          fige
          ()
          salle-figee
	  salle-figee-reelle
          index))
      (when (not (memq (eval code) Liste-I-Cours))
        (newl Liste-I-Cours (eval code)))
      (commentaires-i-cours (eval code) commentaires)
      (instant-trouve-i-cours (eval code) instant-trouve)
      (salle-trouvee-i-cours (eval code) salle-trouvee))
    (construire-erreur-valeurs-manquantes ',enr)))

;;; primitive de lecture d'une enregistrement du type prof

(dmd lire-prof-interface (enr index)
  `(let ,(cdr enr) 
    (creer-i-prof
      code
      libelle
      civilite
      (or dispo DISPO-ECOLE)		;; AJT - FD - 09/07/96
      index)
    (when code 
      (let ((prof (eval code)))
        (commentaires-i-prof prof commentaires)
        (newl Liste-I-Prof prof)
        (cours-i-prof prof cours)))
    (construire-erreur-valeurs-manquantes ',enr)))
  

;;; primitive de lecture d'un enregistrement du type groupe

(dmd lire-groupe-interface (enr index)
  `(let ,(cdr enr) 
    (creer-i-groupe
      code
      libelle
      cours
      groupes
      contraintes
      (or dispo DISPO-ECOLE)		;; AJT - FD - 09/07/96
      index)
    (when code
      (let ((groupe (eval code)))
        (contraintes-i-groupe groupe contraintes)
        (newl Liste-I-Groupe groupe)
        (type-macro-i-groupe groupe type-macro)
        (type-cours-i-groupe groupe type-cours)
        (type-salle-i-groupe groupe type-salle)
        (nb-seances-i-groupe groupe nb-seances)
        (audience-i-groupe groupe audience)
        (durees-i-groupe groupe durees)
        (profs-i-groupe groupe profs)
        (nb-td-i-groupe groupe nb-td)
        (commentaires-i-groupe groupe commentaires)
        (duree-i-groupe groupe duree)))
    (construire-erreur-valeurs-manquantes ',enr)))      

;;; lecture d'une salle

(dmd lire-salle-interface (enr index)
  `(let ,(cdr enr) 
    (creer-i-salle code libelle type capacite index)
    (when code
      (newl Liste-I-Salle (eval code))
      (commentaires-i-salle (eval code) commentaires))
    (construire-erreur-valeurs-manquantes ',enr)))

;;; lecture d'une contrainte

(dmd lire-contrainte-interface (enr index)
  `(let ,(cdr enr)
    (newl Liste-Contraintes
      (cons
        type
        cours))))

;;; AJT (07/04/95) MO
;;; module reservation des salles : primitive de lecture d'une inst. salle

;;; lecture d'une reservation

(dmd lire-resa-interface (enr index)
  `(let ,(cdr enr) 
    (creer-i-resa code libelle instant duree salle index)
    (when code
      (newl Liste-I-Resa (eval code))
      (ifn commentaires
	(commentaires-i-resa (eval code) '("1"))
	(commentaires-i-resa (eval code) commentaires)))
    (construire-erreur-valeurs-manquantes ',enr)))

;;; <lire-ajout-interface>
;;; CRE - FD - 31/07/95
;;; module de rattrapage: ajout de seance

;;; lecture d'un ajout
;;; MOD RM - 11/01/96 : il faut lire les profs

(dmd lire-ajout-interface (enr index)
  `(let ,(cdr enr) 
    (creer-i-ajout code libelle index cours-rattrape instant-cours-rattrape duree profs salle instant flag)
    (semaine-i-ajout (eval code) (semaine-instant instant))
    (when code
      (newl Liste-I-Ajout
          (eval code)))
    (construire-erreur-valeurs-manquantes ',enr)))

;;; <lire-annullation-interface>
;;; CRE - FD - 31/07/95
;;; module de rattrapage: annulation de seance

;;; lecture d'un ajout

(dmd lire-annulation-interface (enr index)
  `(let ,(cdr enr) 
    (creer-i-annulation code libelle index code-cours semaine duree instant flag)
    (when code
      (commentaires-i-annulation (eval code) commentaires)
      (newl Liste-I-Annulation
          (eval code)))
    (construire-erreur-valeurs-manquantes ',enr)))

;;; Gestion des sites AJT - FD - 03/06/96

;;; lecture d'un site

(dmd lire-site-interface (enr index)
  `(let ,(cdr enr)
    (when code
      (when (not (boundp code))
	(creer-i-site
	  code libelle index numero-dd type types-cours types-salles))
      (when (not (memq (eval code) Liste-I-Site))
	(newl Liste-I-Site (eval code))))
    (construire-erreur-valeurs-manquantes ',enr)))

