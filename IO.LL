;;; --------------------------------------------------------------------------
;;; - io.ll                                                VARUNA 1.2        -
;;; - VARUNA                                                                 -
;;; - nouveau format de donnees                                              -
;;; --------------------------------------------------------------------------
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; - Creation:  Aout 93                                                     -
;;; - Derniere mise a jour pour version 1.13:: Aout                          -
;;; - Build 1.22.1 01.07.99 / MO JH                                          -
;;; --------------------------------------------------------------------------


;;; I. UTILITAIRES GENERAUX SUR LES FICHIERS (EN LISP... POSSIBLE ????)
;;; 
;;; CRE (26/06/95) MO FD
;;;
;;;  liste des primitives de ce paragraphe:
;;;               <creer-fichier>
;;;               <sauvegarder-fichier>
;;;               <taille-fichier>
;;;               <fichier-vide-p>


;;; <creer-fichier>
;;;  cree un fichier VIDE (de taille 0)

(defun creer-fichier (fichier)
  (with ((outchan (openo fichier))) (close)))


;;; <sauvegarder-fichier>
;;; si le fichier existe et qu'il est non vide, il copie le fichier dans un
;;; fichier de nom idem celui de fichier avec l'extension envoyee en argument
;;; le must serait:
;;; argument ecraser/ne pas ecraser sur le fichier cible
;;; il faut supporter les noms avec pathname...

;;; Rem (27/03/95) MO
;;; pas de test que le fichier a sauvegarder est le bon (vide, non ferme...)
;;; MOD RM - 07/10/96 : lecture-seule?

(defun sauvegarder-fichier (fichier)
  (let
    ((nom (nom-sauvegarde fichier EXT-BACK))) ; FD MO 26/06/95 AJT de EXT-BACK
    (when (not LECTURE-SEULE?)
      (if
	(probefile fichier)
	(copyfile fichier nom)
	(creer-fichier nom)))
    nom))

;;; <taille-fichier>
;;; donne la taille d'un fichier en nombre de caracteres

;;; JH
;;; puisque cette fonction n'existe pas en Lisp...
;;; je n'ai pas trouve d'autre moyen que de parcourir
;;; tout le fichier en incrementant un compteur

(defun taille-fichier (fichier)
  (file-size fichier))

(defun file-size (file)
  (when
    (probefile file)
    (let
      ((compteur 0))
      (with
        ((inchan (openib file)))
        (untilexit EOF                          ; eof ferme le fichier
          (readcn)                              ; readcn + rapide readch (0.8)
          (incr compteur)))
      compteur)))


;;; <fichier-vide-p>
;;; predicat rendant t si le fichier est vide (ie taille 0 ou lignes blanches)
;;; rem: FD MO il faut se pencher sur la question des buffer
;;; pour optimiser ce precicat , au pire une DLL.

(defun fichier-vide-p (fichier)
  (eq (taille-fichier fichier) 0))

;;; autres utilitaires existant en Lisp:
;;; <probefile> <delete-file>


;;; PRIMITIVE DE LECTURE GENERALE

;;; primitive de lecture de n'importe quel fichier d'enregistrements 
;;; lecture enregistrement par enregistrement et traitement de l'enregis
;;; trement par lire-enregistrement selon le type de lecture
;;; rend () en cas de succes a la lecture ?
;;;  

(defun lire-fichier (fichier type-lecture)
  (let 
    ((index 0))
    (iterer-champs-fichier fichier 
      (enregistrement)
      (setq index (add1 index))
      (lire-enregistrement enregistrement type-lecture index))
    ()))



;;; lire-enregistrement reconnait le type de l'enregistrement et aiguille vers
;;; une primitive de lecture selon le type
;;; Met le package lecture dans les sys-package afin de pouvoir gerer les
;;; variables manquantes (switch-lecture t or ()) dans les lire-{objet}
    
;;; rem (02/09/94) MO  : il faut pouvoir reconnaitre un objet inconnu 
;;; -FAIT- on cree une erreur Varuna ERRTYPENR a gerer par la suite

(dmd lire-enregistrement (enr type-lecture index)
  `(let
    ((type (car ,enr)))
    (switch-lecture t)
    (if 
      (member type LISTE-TYPES-ENREGISTREMENTS-RECONNUS)
      (funcall (concat 'lire- (car ,enr) '- ,type-lecture) ,enr ,index)
      (cons-erreur 'ERRTYPENR ,enr))
    (switch-lecture ())))



    
;;; PRIMITIVE DE LECTURE DU FICHIER ECOLE
;;;
        
;;; primitive generale de lecture d'un fichier ecole
;;; on n'a meme pas besoin de definir les variables
;;; pour plus de clarte, nous le ferons
;;; cette primitive est appelee du moteur (lorsque seul) et de l'interface
    
(dmd lire-ecole (enr)
  `(progn
  (mapc (lambda (paire) (set (car paire) (eval (cadr paire)))) 
    ',(cdr enr))
  (calculer-globales-ecole)))
    


;;; BACKUP D'UN FICHIER
;;; "." et "eds" a remplacer par des globales
;;; 
;;; FD - 26/06/95 on a ajouter le parametre extension dans les
;;; parametres
(defun nom-sauvegarde (fichier ext) 
  (concat (substring fichier 0 (index EXTENSION fichier)) ext));;EXT-BACK))


;;; Ceci n'est pas très élégant
;;; On définit la liste des enregistrements que l'on va explorer
;;; en même temps que l'on parcourt le fichier de référence
;;; AJT PR FD 12/05
(defvar LISTE-ENR)

;;; Renvoie la liste des objets d'un type donné, dans l'ordre
;;; d'index croissants, et () si elle n'existe pas
;;; AJT PR FD 12/05
(defun faire-liste-i-objets (type-enr)
  (let ((nomliste (concat 'liste-i- type-enr)))
    (if
      (boundp nomliste)
      (reverse (eval nomliste))
      ())))


;;; PRIMITIVE GENERALE D'ECRITURE

;;; 1 - on fait une copie du fichier source
;;; 2 - on ecrase le fichier (il l'ouvre en ecriture)
;;;     (tous les print y sont rediriges)
;;; 3 - on itere le fichier sauvegarde en appelant 
;;;     pour chaque enregistrement, la fonction
;;;     ecrire+type-enregistrement+type-ecriture (moteur ou interface)
;;; 4 - appelle le fonction ecrire+type-enregistrement+type-ecriture+nouveaux
;;;     pour ecrire les nouveaux enregistrements

(defun ecrire-fichier (fichier type-ecriture type-enregistrement func-info)
  ; Une ligne d'AJT PR FD du 12/05
  (setq LISTE-ENR (faire-liste-i-objets type-enregistrement))
  (let 
    ((copie-fichier (sauvegarder-fichier fichier))
      (pl (printlevel))
      (rm (rmargin))
      (index 0))
    (printlevel 100)
    (rmargin 1024)
    (with ((outchan (openo fichier)))
      (print (ascii 59) ESP COPYRIGHT)		; on affiche le copyright
      (print) 	
      (iterer-champs-fichier copie-fichier
        (enregistrement)
        (setq index (add1 index))
        (when func-info (funcall func-info index))
	(ecrire-enregistrement enregistrement type-ecriture index))
      (ecrire-nouveaux type-enregistrement type-ecriture index)
      (close))
    (printlevel pl)
    (rmargin rm)
    ()))

(dmd ecrire-enregistrement (enr type-ecriture index)
  `(let
    ((type (car ,enr)))
    (switch-lecture t)
    (if 
      (member type LISTE-TYPES-ENREGISTREMENTS-RECONNUS)
      (funcall (concat 'ecrire- (car ,enr) '- ,type-ecriture) ,enr ,index)
      (cons-erreur 'ERRTYPENR ,enr))
    (switch-lecture ())
    (car ,enr)))

(dmd ecrire-nouveaux (type type-ecriture index)
  `(funcall (concat 'ecrire-nouveaux- ,type '- ,type-ecriture) ,index))
  

(defun entre-quote (chaine)
  (catenate GUILLEMET chaine GUILLEMET))

;;; teste si l'objet passe en parametre est une liste
;;; de chaines de cararcteres (cf. formater)

(defun cons-stringp (objet)
  (when (and (consp objet) (every 'stringp objet)) t))

;;; impression par type
;;; !!trouver un moyen d'ecrire une liste avec une quote
;;; !!sans passer par print-to-string qui creee une chaine

;;; chg (04/10/94) MO
;;; plus de print-to-string et un temps deux fois moins long

(defun formater (val)
   (cond
    ((cons-stringp val) `',(mapcar 'entre-quote val))
    ((stringp val) (entre-quote val))
    ((consp val) `',val)
    ((integerp val) val)
    ((symbolp val) `',val)
    ((structurep val)`',val)
    (t val)))

;;; La même chose que formater, sauf qu'on fait l'impression
;;; Ceci permet d'éviter de construire des listes inutilement
;;; Il s'agit de l'impression d'1 type quelconque, complexe ou non
;;; AJT PR FD du 11/05
(defun imp (val)
  (cond
    ((null val) (prin "()"))
    ((and (consp val) (eq (car val) 'quote))
     (imp (cadr val)))
    ((consp val) (prin "'(")
                 (imp0 (car val))
                 (mapc (lambda (x) (prin ESP) (imp0 x)) (cdr val))
                 (prin ")"))
    ((or (symbolp val) (structurep val)) (prin "'") (imp0 val))
    (t (imp0 val))))

;;; Impression d'un type simple
;;; AJT PR FD du 11/05
(defun imp0 (val) 
   (if
    (stringp val)
     (prin GUILLEMET val GUILLEMET)
     (prin val)))

;;; AJT PR FD du 11/05
(defun imprimer-champ-valeur (champ valeur)
  (prin ESP ESP ESP "(" champ ESP)
  (imp valeur)
  (print ")"))


;;; avant le 27/06/95
; (defun imprimer-nouvel-enregistrement
;   (e-pre objet type-objet liste-champs lc-del)
;   (print "(" (car e-pre) ESP)
;     (mapc (lambda (champs)              ; les champs de liste-champs
;       (print
;         ESP ESP ESP "(" champs ESP
;         (formater
;           (funcall (concat champs '- type-objet) objet))
;         ")"))
;           liste-champs)
;     (mapc (lambda (champ-valeur)          ; les champs de e-pre
;       (let ((champ (car champ-valeur)))
;         (when
;           (and
;             (not (member champ liste-champs))
;             (not (member champ lc-del)))
;           (print 
;             ESP ESP ESP
;             "(" champ ESP 
;             (if                         ; il n'y a qu'avec les string
;               (or                       ; que cela pose probleme
;                 (stringp (cadr champ-valeur))
;                 (cons-stringp (eval (cadr champ-valeur))))
;               (formater (eval (cadr champ-valeur)))
;               (cadr champ-valeur))           ; il faut donc eventuellement
;             ")" ESP))))                      ; les remettre entre quote
;               (cdr e-pre))
;     (print ")"))

;;; Grosses modifs PR FD du 11/05
(defun imprimer-nouvel-enregistrement
  (e-pre objet type-objet liste-champs lc-del)
  (print "(" (car e-pre) ESP)
    (mapc 
        (lambda (champ)              ; les champs de liste-champs
            (imprimer-champ-valeur champ (send champ objet)))
          liste-champs)
    (mapc 
        (lambda (champ-valeur)          ; les champs de e-pre
            (let ((champ (car champ-valeur))
                  (valeur (cadr champ-valeur)))
              (when
                  (and
                      (not (member champ liste-champs))
                      (not (member champ lc-del)))
                  (imprimer-champ-valeur champ valeur))))
          (cdr e-pre))
   (print ")"))


;;; cette fonction permet de recuperer un fichier sauvegarde


;;; AJT (27/03/95) MO
;;; ajout d'un deletefile pour pallier l'imcompatibilite de la 15.26 sur le 
;;; copyfile
;;; CHG (29/03/95) MO
;;; release ILOG debuggant -a priori- cette icompatibilite


(defun reprendre-sauvegarde (fichier)
  (let ((back (nom-sauvegarde fichier EXT-BACK))) ;; FD - 26/06/95 AJT de EXT-BACK
    (close)                             ; referme tous les cannaux ouverts
    (when (probefile back)
      (progn 
        (copyfile back fichier)))
    back))

(defun full-file-name (file directory)
  (catenate directory file))


