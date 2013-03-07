;;; --------------------------------------------------------------------------
;;; - i-suptd.ll                                                VARUNA 1.2   -
;;; - Interface graphique                                                    -
;;; - Module des suppressions et creations de groupes de TDs                 -
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; --------------------------------------------------------------------------
;;; - Creation: RM                                                           -
;;; - Build 1.22.1 01.07.99                                                  -
;;; --------------------------------------------------------------------------

;;; Suppression d'un groupe de TD

;; On elimine un groupe de TD :
;;    - on choisit le groupe de TD a supprimer
;;    - si le cours n'est pas instancie, on le supprime et on reduit le nb
;;      de TD de la famille
;;    - si le cours est instancie, on demande la semaine de debut de
;;      suppression
;;
;; 2 cas se presentent :
;;     - la semaine est egale a la semaine de debut de cours -> on supprime
;;     - sinon, on cree un cours de k semaines dans une sous famille de
;;     - de la famille generatrice
 
(defmessage v_outils-td
  (french "Gestion des TD/TPs")
  (english "???"))

(defmessage v_outils-td-creation
  (french "Création d'un groupe")
  (english "???"))

(defmessage v_outils-td-suppression
  (french "Suppression d'un groupe")
  (english "???"))

(defmessage v_pro-gro-gene
  (french "Quelle famille génératrice de TD/TPs :")
  (english "???"))

(defmessage v_aucune-fam-gene
  (french "Il n'existe aucune famille de TD/TP qui soit instanciée")
  (english "???"))

(defmessage v_choix-sem-debut
  (french "A partir de quelle semaine :")
  (english "???"))

(defmessage v_i-sup-td-confirm
  (french "Voulez-vous supprimer le cours :")
  (english "???"))

(defmessage v_i-cre-td-confirm
  (french "Voulez-vous ajouter le cours :")
  (english "???"))

(defmessage v_sem-debut
  (french "à partir de la semaine : ")
  (english "???"))

(defun supprimer-groupe-td (a)
  (let*
    ((choix (determiner-groupe-td t))
     (famille (car choix))
     (cours (cdr choix))
     (sem-debut))
    (when cours
      (if (instant-trouve-i-cours cours)
        (eliminer-cours cours famille t)
        (eliminer-cours cours famille ()))
      (aff-refresh Gen-App))))

;; Fonction permettant de choisir le groupe de TD que l'on veut supprimer
;; Cette fonction retourne une liste (famille cours)

(defun determiner-groupe-td (cours-p)
  (let
    ((famille)
     (cours ()))
    (setq famille
      (demande-filtree-i-groupe
        #Mv_pro-gro-gene
        #Mv_aucune-fam-gene
	()
	'famille-td-site-instanciee-p)) ;; MOD - FD - 23/07/96
    ; 'famille-td-instanciee-p)) ;; MOD famille-td-p>famille-td-instanciee-p
    (when (and famille cours-p)
      (setq cours
        (reponses-multiples
          (cours-i-groupe famille)
          'code-i-cours
          (column
            #Mv_plusieurs-reponses
            (string (libelle-i-groupe famille)))
          'code-i-cours ())))
    (cons famille cours)))

(defun eliminer-cours (cours famille-gene instant-trouve)
  (let
    ((sem-debut-choisie ())
     (confirm)) 
    (if (not instant-trouve)
      (progn
        (setq confirm (confirmer-sup-td cours sem-debut-choisie))
        (when confirm 
          (supprimer-lien-cours-famille cours famille-gene confirm)))
      (setq sem-debut (semaine-instant (instant-trouve-i-cours cours)))
      (setq sem-debut-choisie
	;; MOD RM - 16/08/96 : semaines calendaires
        (demande-semaine
          #Mv_choix-sem-debut
          sem-debut
          (sub1 (add sem-debut (nb-seances-i-cours cours)))))
      (when sem-debut-choisie
        (setq confirm (confirmer-sup-td cours sem-debut-choisie))
        (when confirm 
          (if
            (equal
              (semaine-instant (instant-trouve-i-cours cours))
              sem-debut-choisie)
            (supprimer-lien-cours-famille cours famille-gene confirm)
            (reduire-cours cours famille-gene sem-debut-choisie confirm)
	    ;; MOD FD - 19/02/96 - on fige la famille
	    (figer-les-cours
	      ()
	      (lambda (c)
		(inter-non-vide-p (groupes-i-cours c) (list famille-gene))))
	    ))))))
  
;; On elimine le cours et on reduit le nb de tds de la famille

(defun supprimer-lien-cours-famille (cours famille-gene confirm)
  (let
    ((nb-td (nb-td-i-groupe famille-gene)))
    (nb-td-i-groupe famille-gene (sub1 nb-td))
    (when (eq confirm 0)
      (imprimer-modif-td cours famille-gene t))
    (supprimer-td-special famille-gene cours)))
    
(defun reduire-cours (cours famille-gene sem-debut-choisie confirm)
  (let
    ((nb-td (nb-td-i-groupe famille-gene))
    (code)
    (copie (nouveau-i-cours))
    (cours-sup ())
    (famille-sup (car (groupes-i-groupe famille-gene))))

    (nb-td-i-groupe famille-gene (sub1 nb-td))
    (setq code (generer-code-td famille-gene "-suptd-"))
    (setq copie (circopy-struct cours 'index))
    (code-i-cours copie code)
    (set code copie)
    ;; reduction du nb de seances
    (nb-seances-i-cours copie
      (- sem-debut-choisie (semaine-instant (instant-trouve-i-cours cours))))
    ;; MOD FD 16/02/96
    ;   (- (sub1 (nb-seances-i-cours cours))
    ;     (-
    ;     (nb-seances-i-cours cours)
    ;       sem-debut-choisie)))
    (newl Liste-i-cours copie)
    ;; suppression de l'ancien
    (supprimer-td-special famille-gene cours)
    (when (not famille-sup)
      (setq famille-sup (creer-sous-famille-td famille-gene "-suptd")))
    (setq cours-sup (cours-i-groupe famille-sup))
    (cours-i-groupe famille-sup (cons copie cours-sup))
    (groupes-i-cours copie (list famille-sup))
    (figer-cours copie)
    (when (eq confirm 0)
      (imprimer-modif-td copie famille-sup t))
    ))

(defun creer-sous-famille-td (famille-pere extension)   
  (let
    ((code (concat (code-i-groupe famille-pere) extension))
     (nouv-famille (nouveau-i-groupe)))
    (setq nouv-famille
      (circopy-struct famille-pere 'nb-seances 'audience 'duree
        'nb-td 'contraintes 'type-cours 'type-salle))
    (code-i-groupe nouv-famille code)
    (set code nouv-famille)
    (newl Liste-I-Groupe nouv-famille)
    (type-macro-i-groupe nouv-famille #Mv_groupe-standard)
    ;; AJT FD 22/02/96
    (editeur-i-groupe nouv-famille
      (type-groupe2editeur-groupe (type-macro-i-groupe nouv-famille)))
    (groupes-i-groupe famille-pere (list nouv-famille))
    (cours-i-groupe nouv-famille ())
    nouv-famille))

(defun supprimer-td-special (famille cours)
  (cours-i-groupe famille (delq cours (cours-i-groupe famille)))
  (supprimer-i-cours-special cours ()))

;;; CRE - FD - 23/07/96
;;; <famille-td-site-instaniciee-p>
;;; predicat indiquant que nous avons une famille de TDs et que tous les cours
;;; de cette famille sont instancies et que le type de salle et le type de
;;; cours sont autorises pour le site connecte

(defun famille-td-site-instanciee-p (famille)
  (and
    (famille-td-instanciee-p famille)
    (type-salle-autorise-p (type-salle-i-groupe famille))
    (type-cours-autorise-p (type-cours-i-groupe famille))))
      
;;; <famille-td-instanciee-p>
;;; predicat indiquant que nous avons une famille de TDs et que tous les cours
;;; de cette famille sont instancies


(defun famille-td-instanciee-p (famille)
  (and
    (famille-td-p famille)
    (every 'i-instanciep (cours-i-groupe famille))))

;; predicat sur le type de famille (generatrice de TD)

(defun famille-td-p (famille)
  (and
    (or
      ;; famille de TD/TPs SIMULTANES
      (td-simultanes-p famille)
      ;; famille de TD/TPs MEME SEMAINE
      (td-m-semaine-p famille))
    (not (eq (nb-td-i-groupe famille) 0))))

(defun td-simultanes-p (famille)
  (or
    (equal (type-macro-i-groupe famille) #Mv_groupe-td-a)
    (equal (type-macro-i-groupe famille) #Mancien-v_groupe-td-a)))

(defun td-m-semaine-p (famille)
  (or
    (equal (type-macro-i-groupe famille) #Mv_groupe-td-b)
    (equal (type-macro-i-groupe famille) #Mancien-v_groupe-td-b)))

(defun confirmer-sup-td (cours sem-debut)
  (let*
    ((instant (instant-trouve-i-cours cours))
     (duree (duree-i-cours cours))
     (horaire-debut (interne2cumule (horaire-instant instant)))
     (horaire-fin (add horaire-debut duree)))

    (ok-annuler-imprimer
      (column
        #Mv_i-sup-td-confirm
        (row
          (string (libelle-i-cours cours))
          " (" (string (code-i-cours cours)) ")")
        (row 
          #Mv_info-le
          (string (libelle-jour (jour-instant instant)))
          #Mv_de
          (cumule2naturel 
            (horaire-moment-cumule horaire-debut))
          #Mv_a
          (cumule2naturel 
            (horaire-moment-cumule horaire-fin)))
        (row #Mv_ensalle
          (if (salle-trouvee-i-cours cours)
            (libelle-i-salle (salle-trouvee-i-cours cours))
            ESP))
        (row
          (if sem-debut
            (row #Mv_sem-debut (string (ecrire-numero-semaine sem-debut)))
            ESP))))))

;;; __________________________________________________________________

;;; Creation d'un groupe de TD

(defun creer-groupe-td (a)
  (let*
    ((choix (determiner-groupe-td ()))
      (famille (car choix)))
    (when famille 
      (ajouter-nouveau-td famille))))


(defun ajouter-nouveau-td (famille-gene)
  (let
    ((sem-debut)
     (cours-gene (car (cours-i-groupe famille-gene)))
     (sem-debut-cours))
    (setq sem-debut-cours
      (semaine-instant (instant-trouve-i-cours cours-gene)))
    (setq sem-debut
      ;; MOD RM - 16/08/96 : semaines calendaires
      (demande-semaine
        #Mv_choix-sem-debut
        (add1 sem-debut-cours)
        (sub1 (add sem-debut-cours (nb-seances-i-cours cours-gene)))))
    (when sem-debut
      (creer-nouveau-td famille-gene cours-gene sem-debut))))

(defun creer-nouveau-td (famille-gene cours-gene sem-debut)
  (let
    ((code)
     (nouveau (nouveau-i-cours))
     (famille-cre (car (groupes-i-groupe famille-gene)))
     (sem-debut-cours (semaine-instant (instant-trouve-i-cours cours-gene)))
     (type-famille (type-macro-i-groupe famille-gene))
     (confirm))

    (setq code (generer-code-td famille-gene "-cretd-"))
    (if (td-simultanes-p famille-gene)
      (setq nouveau
        (circopy-struct cours-gene 'salle-trouvee))
      (setq nouveau
        (circopy-struct cours-gene 'instant-trouve 'salle-trouvee)))
    (code-i-cours nouveau code)
    (set code nouveau)
    (newl liste-i-cours nouveau)
    (when (not famille-cre)
      (setq famille-cre (creer-sous-famille-td famille-gene "-suptd")))
    (cours-i-groupe famille-cre
      (cons nouveau (cours-i-groupe famille-cre)))
    (groupes-i-cours nouveau (list famille-cre))
    ; (nb-seances-i-cours nouveau
    ; MOD FD 16/02/96  (- (nb-seances-i-cours nouveau) (sub1 sem-debut)))
    (nb-seances-i-cours nouveau
      (- (nb-seances-i-cours nouveau)
	(- sem-debut sem-debut-cours)))
    (setq confirm
      (placer-un-cours Gen-App nouveau 'confirmer-cre-td
	sem-debut))
    (if confirm
      (progn
	;; MOD FD - 19/02/96 - on fige la famille ms pas les salles
	(figer-les-cours
	  ()
	  (lambda (c)
	    (inter-non-vide-p (groupes-i-cours c) (list famille-gene))))
	(when (eq confirm 0)
	  (imprimer-modif-td nouveau famille-cre ())))
      ;; FD - AJT - 19/02/96
      ;; il faut supprimer l'objet nouveau car il n'a pas pu etre place
      ;; ou parce qu'il y a eu annulation
      ;; on supprime aussi la sous-famille "-suptd" si elle est vide
      (supprimer-i-cours-special nouveau ())
      (when
	(and
	  (not (cours-i-groupe famille-cre))
	  (not (groupes-i-groupe famille-cre)))
      (supprimer-i-groupe-special famille-cre ())))))
    

(defun confirmer-cre-td (cours instant salle)
  (let*
    ((duree (duree-i-cours cours))
     (horaire-debut (interne2cumule (horaire-instant instant)))
     (horaire-fin (add horaire-debut duree)))

    (ok-annuler-imprimer
      (column
        #Mv_i-cre-td-confirm
        (row
          (string (libelle-i-cours cours))
          " (" (string (code-i-cours cours)) ")")
        (row 
          #Mv_info-le
          (string (libelle-jour (jour-instant instant)))
          #Mv_de
          (cumule2naturel 
            (horaire-moment-cumule horaire-debut))
          #Mv_a
          (cumule2naturel 
            (horaire-moment-cumule horaire-fin)))
        (row #Mv_ensalle
          (if salle
            (libelle-i-salle salle)
            ESP))
        (row
          (if sem-debut
            (row #Mv_sem-debut (string (ecrire-numero-semaine sem-debut)))
            ESP))))))


(defun generer-code-td (famille-gene extension)
  (let
    ((index 1)
     (code))
    (setq code
      (concat (code-i-groupe famille-gene) extension index))
    (while (boundp code)
      (setq index (add1 index))
      (setq code
        (concat (code-i-groupe famille-gene) extension index)))
    code))

