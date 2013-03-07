;;; --------------------------------------------------------------------------
;;; - ecrire.ll                                            VARUNA 1.2        -
;;; - VARUNA                                                                 -
;;; - ecriture des donnees interface                                         -
;;; --------------------------------------------------------------------------
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; - Creation:  Aout 93                                                     -
;;; - Derniere mise a jour pour version 1.13:: Aout                          -
;;; - Build 1.22.1 01.07.99 / JH                                             -
;;; --------------------------------------------------------------------------


;;; controle d'un fichier. Si sa taille est nulle et que la liste
;;; d'objets correspondants n'est pas nil
;;; propose un dialogue pour reprendre la sauvegarde (.eds)

(dmd controle-fichier (fichier liste-objets)
  `(when
    (and
      (probefile ,fichier)
      (eq (file-size ,fichier) 0)
      ,liste-objets)
    (when 
      (afficher-confirmation 
        (column
          (row #Mv_erreur-ecriture ,fichier)
          #Mv_utiliser-sauvegarde))
            (reprendre-sauvegarde ,fichier))))

;;; controle global en fin d'ecriture
;;; verifie que l'ecriture s'est correctement deroulee

(defun controle-ecriture ()
  (controle-fichier FICHIER-COURS Liste-I-Cours)
  (controle-fichier FICHIER-PROFS Liste-I-Prof)
  (controle-fichier FICHIER-SALLES Liste-I-Salle)
  (controle-fichier FICHIER-GROUPES Liste-I-Groupe)
  (controle-fichier FICHIER-CONTRAINTES Liste-Contraintes)
  (controle-fichier FICHIER-ECOLE Liste-I-Ecole)
  ;; xxx FD - 30/05/95 ajouter (when (MODULE-RESA
  (when MODULE-RESA
      (controle-fichier FICHIER-RESA Liste-I-Resa))
  ;; AJT - FD - 31/07/95
  (when MODULE-RACO
      (controle-fichier FICHIER-AJOUT Liste-I-Ajout)
      (controle-fichier FICHIER-ANNUL Liste-I-Annulation))
  )

;;; renvoie la somme des tailles des liste d'objets 
;;; (utile pour le thermometre)

(defun taille-donnees ()
  (+    
    (length Liste-I-Cours)
    (length Liste-I-Prof)
    (length Liste-I-Salle)
    (length Liste-I-Groupe)
    (length Liste-Contraintes)
    (length Liste-I-Resa)
    (length Liste-I-Ajout)
    (length Liste-I-Annulation)
    1))

;;; mise a jour du thermometre (a appeler pendant l'ecriture)
;;; utilise les globales Base et TotalFiches

(defun thermo-ecriture (index)
  (mise-a-jour-thermometre (add Base index) Total-Fiches))

;;; ecriture des donnees 
;;;  MOD - RM - FD - 01/09/98 traitement reseaux

(defun ecrire-donnees-interface ()
  (if (and MODULE-RESEAU (not (interdire-acces-attente EDT-SEM)))
    (afficher-attention #Mv_acces-edt-interdit)
    (donner-titre #Mv_ecrire)
    (setq Total-Fiches (taille-donnees))
    (setq Base 0)
    (debut-enregistrement)
    (ecrire-fichier FICHIER-COURS 'interface 'cours 'thermo-ecriture)
    (setq Base (add Base (length Liste-I-Cours)))
    (ecrire-fichier FICHIER-PROFS 'interface 'prof 'thermo-ecriture)
    (setq Base (add base (length Liste-I-Prof)))
    (ecrire-fichier FICHIER-SALLES 'interface 'salle 'thermo-ecriture)
    (setq Base (add Base (length Liste-I-Salle)))
    (ecrire-fichier FICHIER-GROUPES 'interface 'groupe 'thermo-ecriture)
    (setq Base (add Base (length Liste-I-Groupe)))
    (ecrire-fichier FICHIER-CONTRAINTES 'interface 'contrainte 'thermo-ecriture)
    (when MODULE-RESA
      (ecrire-fichier FICHIER-RESA 'interface 'resa 'thermo-ecriture))
    (when MODULE-RACO
      (ecrire-fichier FICHIER-AJOUT 'interface 'ajout 'thermo-ecriture)
      (ecrire-fichier FICHIER-ANNUL 'interface 'annulation 'thermo-ecriture))
    (ecrire-fichier FICHIER-ECOLE 'interface 'ecole ())
    (when (and MODULE-RESEAU MODULE-GESTSITE MODULE-CONCEPTION)
      (ecrire-fichier FICHIER-SITES 'interface 'site ()))

    (when MODULE-RESEAU
      (when MODULE-TRACE
	(enregistrement-donnees-tracees))
      (nettoyer-transfile)
      (autoriser-acces EDT-SEM))
    (lire-donnees-interface ())
    (setq Liste-Groupes-Aff
      (evlis1 (mapcar 'code-i-groupe Liste-Groupes-Aff)))
    (setq Liste-Salles-Aff
      (evlis1 (mapcar 'code-i-salle Liste-Salles-Aff)))
    (setq Liste-Profs-Aff
      (evlis1 (mapcar 'code-i-prof Liste-Profs-Aff)))

    (mise-a-jour-thermometre 0 1)
    (fin-enregistrement)
    (donner-titre ())
    (setq DIRTY ())))

;;; renvoie un enregistrement interface mis a jour
;;; (pret a etre imprime)
;;; pour chaque champ, on verifie qu'il existe un accesseur
;;; sur l'objet-interface du bon type
;;; si oui on renvoie la nouvelle valeur du champ
;;; sinon renvoie le couple champ-valeur tel qu'il figure
;;; dans l'enregistrement (modulo les chaines de caracteres)

;;; !!pas tres propre
;;; !!surtout que cette primitive est appelee pour
;;; !!chaque enregistrement
;;; JH /mapc->tag
;;; rem (04/10/94) MO
;;; en fait, il faudrait travailler sur des listes classees
;;; les liste sont deja triees par index decroissant.
;;; en theorie, on peut donc maper a partir de (n - le dernier index ecrit)
;;; sauf si la liste est triee quelque part dans le programme.
;;; le probleme c'est que avec lastn, on fait une copie de la liste
;;; et on ne peut se permettre de faire une copie de liste
;;; par enregistrement...
;;; avant le 27/06/95
; (defun trouver-objet-interface (type index)
;   (if (eq type 'ecole) (and (index-i-ecole I-ECOLE 1) I-ECOLE)
;     (tag trouve
;       (mapc (lambda (ob)
;         (when (eq index (funcall (concat 'index-i- type) ob))
;           (exit trouve ob)))
;       (eval (concat 'Liste-I- type))))))

;;; MOD PR FD 12/05
(defun trouver-objet-interface (type index)
  (if 
    (eq type 'ecole) 
    (and
      (index-i-ecole I-ECOLE 1)
      (or (setq LISTE-ENR ())I-ECOLE))
    (trouver-objet-liste-enr type index LISTE-ENR)))

;;; Maintenant, on itère sur les enregistrements en mettant à 
;;; jour la liste à chaque fois
;;; AJT PR FD 12/05

; (defun trouver-objet-liste-enr (type index liste)  
;   (if
;     (null liste)
;     nil
;     (let ((index-courant 
;               (funcall (concat 'index-i- type) (car liste))))
;       (cond
;        ((null index-courant) nil) ; évite de tout reparcourir
;         ((eq index index-courant)
;          (progn (setq LISTE-ENR (cdr liste)) (car liste)))
;         (t (trouver-objet-liste-enr type index (cdr liste)))))))

;;; MOD FD - RM : 07/02/96 : on va essayer d'enlever la factorielle !!!

(defun trouver-objet-liste-enr (type index liste)
  (let
    ((result liste)
     (element)
     (index-courant))
    (when liste
      (nextl result element)
      (setq index-courant
        (funcall (concat 'index-i- type) element))
      (until
        (or
          (null index-courant)
          (null result)
          (eq index index-courant))
        (nextl result element)
        (setq index-courant
          (funcall (concat 'index-i- type) element)))
      (when (eq index index-courant)
        (setq LISTE-ENR result)
        element))))


;;; AJT RM - FD 01/09/95
(defun valeur-champ-code (enr)
  (let
    ((enregistrement (cdr enr))
     (champ))

    (until (eq (car champ) 'code)
      (nextl enregistrement champ))
    (cadadr champ)))

;;; fonction d'ecriture standard de l'interface

;;; AJT FD - RM : 31/08/95 : ecriture des donnees en reseau

; (dmd ecriture-standard-interface (enr index type format)
; le car de l'enregistrement, c'est le type
;   `(let ((objet (trouver-objet-interface (car ,enr) ,index)))
;     (when objet
;       (imprimer-nouvel-enregistrement ,enr objet ',type ,format ()))))

(dmd ecriture-standard-interface (enr index type format)
;  le car de l'enregistrement, c'est le type
  `(let
    ((objet (trouver-objet-interface (car ,enr) ,index))
     (etat)) 
    (if objet
      (progn
        (if (or (eq type 'ecole) (not MODULE-RESEAU))
          (setq etat ETAT-NORMAL)
          (setq etat (etat-objet objet)))
          ;; 15/01/96 ((fin-modification SITE objet))
        (cond
          ((eq etat ETAT-NORMAL)
            (imprimer-nouvel-enregistrement ,enr objet ',type ,format ()))
          ((eq etat ETAT-MODIF)
            ;; (print ,enr) !!!
            (imprimer-nouvel-enregistrement ,enr objet ',type () ()))
          ;; ETAT-SUPPR : on ne fait rien 
          (t
            ())))
      (when MODULE-RESEAU
        (let ((code (valeur-champ-code ,enr)))
          (if
            (eq (etat-objet code) ETAT-MODIF)
            ;; cet enregistrement a ete ajoute par un autre site
            ;; (print ,enr) !!!
            (imprimer-nouvel-enregistrement ,enr objet ',type () ()))
            ;; l'enregistrement a ete supprime par le site
            ;; 15/01/96 (fin-modification SITE code)
            ))))))

;;; avant le 27/06/95
; (dmd ecriture-nouveaux (type-fichier type dernier-index format-objet)
;   `(let ((ind ,dernier-index))
;     (mapc (lambda (ob)
;       (setq ind (add1 ind))
;       (,(concat 'index- type) ob ind)
;       (imprimer-nouvel-enregistrement
;         '(,type-fichier) ob ',type ,format-objet ()))
;      (sous-liste
;        (lambda (c) (null (,(concat 'index- type) c)))
;        ,(concat 'Liste- type)))))

;;; L'astuce du 'LISTE-ENR est dûe au fait que l'on est
;;; dans une macro. D'autre part, il est obligatoire d'avoir
;;; une macro ici car on est "entre 2 macros", et cela semble
;;; gêner le compilateur (si, si)
;; C'est une modif PR FD du 12/05
(dmd ecriture-nouveaux (type-fichier type dernier-index format-objet)
  `(let
    ((ind ,dernier-index)
     (liste ,'LISTE-ENR))
    (mapc
      (lambda (ob)
	;; MOD FD/RM 12/09/96
	(when ind
	  (setq ind (add1 ind))
	  (,(concat 'index- type) ob ind))
        (imprimer-nouvel-enregistrement
          '(,type-fichier) ob ',type ,format-objet ())
        ;;  15/01/96 (when MODULE-RESEAU (fin-modification SITE ob))
        )
      liste)))


;;; Format des enregistrements

(defvar FORMAT-COURS
  '(instant-trouve code libelle duree type type-salle audience nb-seances
  dispo fige salle-figee commentaires salle-trouvee salle-figee-reelle))

(defvar FORMAT-PROF
  '(code libelle civilite cours dispo commentaires))

(defvar FORMAT-SALLE
  '(code libelle capacite type commentaires))

(defvar FORMAT-GROUPE
  '(code libelle cours groupes contraintes dispo
  type-macro type-cours type-salle commentaires
  nb-seances audience durees profs nb-td duree))

(defvar FORMAT-RESA
  '(code libelle instant duree salle commentaires))

;;; AJT - FD - 31/07/95
(defvar FORMAT-AJOUT
  '(code libelle cours-rattrape profs instant-cours-rattrape instant duree salle flag))

(defvar FORMAT-ANNULATION
  '(code libelle code-cours semaine duree instant flag commentaires))

;;;  chg 27/06/95 FD MO ajt de jours ouvrables
;;; AJT - FD - 26/07/96 chmps suplementaires calendaire et periodes calendaires

(defvar FORMAT-ECOLE
  '(libelle-ecole unite-temps param-chevauchement
  param-chevauchement-max dispo-ecole horaire-cumule-pause
  semaines-debut-ecole  
  liste-moments-debut-ecole
  liste-codes-types-cours liste-libelles-types-cours
  grain-dispo calendaire periodes-calendaires pause))


(defun ecrire-cours-interface (enr index)
  (ecriture-standard-interface enr index i-cours FORMAT-COURS))

(defun ecrire-nouveaux-cours-interface (dernier-index)
  (ecriture-nouveaux cours i-cours dernier-index FORMAT-COURS))

(defun ecrire-prof-interface (enr index)
  (ecriture-standard-interface enr index i-prof FORMAT-PROF))

(defun ecrire-nouveaux-prof-interface (dernier-index)
  (ecriture-nouveaux prof i-prof dernier-index FORMAT-PROF))

(defun ecrire-salle-interface (enr index)
  (ecriture-standard-interface enr index i-salle FORMAT-SALLE))
  

;;; ajt (10/04/95) MO
;;; ajout pour tenir compte de la notion de reservation de salle

(defun ecrire-resa-interface (enr index)
  (ecriture-standard-interface enr index i-resa FORMAT-RESA))

(defun ecrire-nouveaux-salle-interface (dernier-index)
  (ecriture-nouveaux salle i-salle dernier-index FORMAT-SALLE))

;;; ajt (10/04/95) MO
;;; ajout pour tenir compte de la notion de reservation de salle

(defun ecrire-nouveaux-resa-interface (dernier-index)
  (ecriture-nouveaux resa i-resa dernier-index FORMAT-RESA)) ;;DBG FD FORMAT-SALLE remplace par FORMAT-RESA


;;; <ecrire-ajout-interface>
;;; AJT - FD - 31/07/95
;;; ajout pour tenir compte de la notion d'ajout de seance
(defun ecrire-ajout-interface (enr index)
  (ecriture-standard-interface enr index i-ajout FORMAT-AJOUT))

;;; <ecrire-nouveaux-ajout-interface>
;;; ajout pour tenir compte de la notion d'ajout de salle
(defun ecrire-nouveaux-ajout-interface (dernier-index)
  (ecriture-nouveaux ajout i-ajout dernier-index FORMAT-AJOUT)) 

;;; <ecrire-annulation-interface>
;;; AJT - FD - 31/07/95
;;; ajout pour tenir compte de la notion d'annulation de seance
(defun ecrire-annulation-interface (enr index)
  (ecriture-standard-interface enr index i-annulation FORMAT-ANNULATION))

;;; <ecrire-nouveaux-annulation-interface>
;;; ajout pour tenir compte de la notion de 'annulation de seance
(defun ecrire-nouveaux-annulation-interface (dernier-index)
  (ecriture-nouveaux annulation i-annulation dernier-index FORMAT-ANNULATION)) 

(defun ecrire-groupe-interface (enr index)
  (ecriture-standard-interface enr index i-groupe FORMAT-GROUPE))

(defun ecrire-nouveaux-groupe-interface (dernier-index)
  (ecriture-nouveaux groupe i-groupe dernier-index FORMAT-GROUPE))

(defun ecrire-ecole-interface (enr index)
  (ecriture-standard-interface enr index i-ecole FORMAT-ECOLE))

(defun ecrire-nouveaux-ecole-interface (dernier-index)
  (ecriture-nouveaux ecole i-ecole dernier-index FORMAT-ECOLE))

(defun ecrire-contrainte-interface (enr index)
  ())

(defun ecrire-nouveaux-contrainte-interface (dernier-index)
  (mapc (lambda (contr)
    (print
      (list
        'contrainte
        (list
          'type
          (formater (car contr)))
        (list
          'cours
          (formater (cdr contr))))))
            Liste-Contraintes))


;;; rem  FD MO 26/06/95
;;; dispo codee en dur !!!
(dmd maj-dispo-type (type)
  `(let ((la-dispo '(((480 . 7980)) . ((1 . 33)))))
    (mapc (lambda (ob)
      (when (null (,(concat 'dispo-i- type) ob))
        (,(concat 'dispo-i- type) ob la-dispo)))
      ,(concat 'Liste-I- type))))

;;;  primitive appelee avant le 26/06/95 dans <ecrire-donnees-interface>
; (defun maj-dispo ()
;  (maj-dispo-type cours)
;  (maj-dispo-type prof)
;  (maj-dispo-type groupe))

  
;;; Gestion de sites AJT - FD - 03/06/96

;;; liste des champs d'un objet de type site qu'il faut enregistrer

(defvar FORMAT-SITE
  '(code libelle type numero-dd types-cours types-salles))

(defun ecrire-site-interface (enr index)
  (ecriture-standard-interface enr index i-site FORMAT-SITE))

(defun ecrire-nouveaux-site-interface (dernier-index)
  (ecriture-nouveaux site i-site dernier-index FORMAT-SITE))


;;; CRE RM - 15/10/96 : trace des donnees dans le fichier Varuna.log

(defun ecrire-trace-donnee (objet)
  (let*
    ((data (if (symbolp objet) (eval objet) objet))
     (type (type-objet data))
     (liste-champs (liste-champs-type data))
     (rm (rmargin))
     (etat (etat-objet objet)))
    (when (not (eq etat ETAT-SUPPR))
      (rmargin 1024)
      (print "(" type ESP)
	(mapc 
	  (lambda (champ) 
	    (imprimer-champ-valeur champ (send champ data)))
	  liste-champs)
	(print ")")
      (rmargin rm))))

(defun enregistrement-donnees-tracees ()
  (with ((current-directory REP-VARUNA))
    (with ((outchan (openo FICHIER-TRACE-TMP)))
      (print "*:" SITE ":*")
      (mapc 'ecrire-trace-donnee LISTE-OBJETS-TRACES)
      (print "()")
      (close)))
  (funcall 'maj-fichier-trace))

(defun liste-champs-type (objet)
  (selectq (type-of objet)
    ('#:tclass:i-cours  FORMAT-COURS)
    ('#:tclass:i-groupe FORMAT-GROUPE)
    ('#:tclass:i-prof   FORMAT-PROF)
    ('#:tclass:i-salle  FORMAT-SALLE)    
    ('#:tclass:i-resa   FORMAT-RESA)
    ('#:tclass:i-annul  FORMAT-ANNUL)
    ('#:tclass:i-ajout  FORMAT-AJOUT)))
