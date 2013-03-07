;;; --------------------------------------------------------------------------
;;; - uvaruna.ll                                                VARUNA 1.2   -
;;; - Module des Utilitaires generaux                                        -
;;; - Utilitaires specifiques a Varuna Generaux                              -
;;; --------------------------------------------------------------------------
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; - Creation:  Octobre 94                                                  -
;;; - Derniere mise a jour pour version 1.13:: Octobre 94                    -
;;; - Build 1.22.1 01.07.99 / JH MO                                          -
;;; --------------------------------------------------------------------------

;;; <maj-rep-donnees>
;;; on teste que le repertoire cible existe et alors
;;; on affecte cette valeur … la variable globale REP-DONNEES 
;;;  et sinon on genere une erreur dans l'interface d'erreur

(defun maj-rep-donnees (repertoire)
  (if (directoryp repertoire)
    (setq REP-DONNEES repertoire)
    (cons-erreur 'err-rep-donnees repertoire)
    ()))					; pour rendre () si mauvais

;;; <initialiser-rep-donnees>
;;; si le repertoire de la variable d'environnement VARDATA existe
;;; alors l'affecte a REP-DONNEES glob varuna, sinon cree le repertoire
;;; en effet, cela n'est pas "grave" si le repertoire de donnees n'existe
;;; pas... 

(defun initialiser-rep-donnees ()
  (let
    ((le-repertoire (string (getenv "VARDATA"))))
    (when
      (not (directoryp le-repertoire)) 
      (create-directory le-repertoire)
      (cons-erreur 'inf-rep-init-donnees le-repertoire))
    (setq REP-DONNEES le-repertoire)))


;;; <fichier-absolu>
;;; concatene le repertoire rep (s'il existe) et le fichier fic

(defun fichier-absolu (rep fic)
  (if
    (directoryp rep)
    (catenate rep "/" fic)
    (cons-erreur 'err-rep rep)
    ()))


;;; <donnee-lecture-seule-p>
;;; version MultiP
;;; AJT - 29/02/96 - FD
;;; verifie si objet est en lecture seule ou non

(defun donnee-lecture-seule-p (objet cons-erreur)
  (cond
    ((or LECTURE-SEULE?
      (when PREFIXE-INTERDIT
	(substring-equal
	  (slength PREFIXE-INTERDIT)
	  PREFIXE-INTERDIT
	  0
	  (string (send 'code objet))
	  0)))
    (when cons-erreur
      (cons-erreur 'erreur-lecture-seule (list objet)))
    t)
    (t
      ())))

;;; <donnee-modifiable-p>
;;; CRE - 04/03/96

(defun donnee-modifiable-p (cours)
  (or
    SITE-CENTRAL
    (and
      (cours-site-p cours)
      (not (donnee-lecture-seule-p cours ())))))

;;; <type-salle-autorise-p>
;;; CRE- FD - 21/03/96
;;; verifie qu'un type de salle est utilisable
;;; MOD - FD - 05/06/96 - SITE-CONNECTE

(defun type-salle-autorise-p (type-salle)
  (cond
    (SITE-CENTRAL
      t)
    ((null type-salle)
      t)
    ;; !!!    ((not (appartenir-liste type-salle (liste-type-salle)))
    ;;       t)
    ((symbolp type-salle)
      (appartenir-liste type-salle
	(types-salles-i-site SITE-CONNECTE)))
    ((not (symbolp type-salle))
      (if (not (member type-salle (liste-type-salle)))
	t
	(appartenir-liste
	  (symbol () type-salle)
	  (types-salles-i-site SITE-CONNECTE))))
    (t
      t)))

;;; <type-cours-autorise-p>
;;; CRE- FD - 26/03/96
;;; verifie qu'un type de cours est utilisable
;;; MOD - FD - 05/06/96 - SITE-CONNECTE

(defun type-cours-autorise-p (type-cours)
  (cond
    (SITE-CENTRAL
      t)
    ((null type-cours)
      t)
    ((symbolp type-cours)
      ;       (appartenir-liste type-cours TYPES-COURS-AUTORISES)
      (appartenir-liste type-cours (types-cours-i-site SITE-CONNECTE)))
    ((not (symbolp type-cours))
      ;       (appartenir-liste (symbol () type-cours) TYPES-COURS-AUTORISES)
      (appartenir-liste (symbol () type-cours) (types-cours-i-site SITE-CONNECTE)))))

;;; 20/05/96
(defun valider-type-salle (val)
  (let ((ok (tester-type-salle val)))
    (when ok
      ;; mettre a jour la liste des types de salle autorises
      (maj-lst-types-salle-autorises (symbol () val)))
    ok))

(defun tester-type-salle (val)
  (if (or SITE-CENTRAL (type-salle-autorise-p (symbol () val)))
    t
    (cons-erreur 'err-type-interdit-salle val)
    ()))


(defun tester-type-cours (val)
  (if (or SITE-CENTRAL (type-cours-autorise-p (symbol () val)))
    t
    (cons-erreur 'err-type-interdit-cours val)
    ()))

;;; <salle-site-p>
;;; predicat verifiant si le site courant peut modifier la salle

(defun salle-site-p (salle)
  (or
    SITE-CENTRAL
    (type-salle-autorise-p (string (type-i-salle salle)))))

;;; <cours-site-p>
;;; predicat verifiant si le site courant peut modifier le cours

(defun cours-site-p (cours)
  (or
    SITE-CENTRAL
    (and
      (type-cours-autorise-p (type-i-cours cours))
      (type-salle-autorise-p (type-salle-i-cours cours)))))

;;; <tester-types-cours>
;;; verifie que le cours peut etre modifie: i.e. si le type de cours et le type
;;; de salle sont accessible pour le site courant
;;; idem que fct precedente mais construit un msg d'erreur

(defun tester-types-cours (cours)
  (or
    SITE-CENTRAL
    (and
      (tester-type-cours (type-i-cours cours))
      (tester-type-salle (type-salle-i-cours cours)))))

;;; <resa-site-p> - 02/04/96
;;; verifie que la resa peut etre modifie: i.e. si le type de cours et le type
;;; de salle sont accessible pour le site courant

(defun resa-site-p (resa)
  (or
    SITE-CENTRAL
    (and
      (tester-type-cours (type-cours-i-resa resa))
      (tester-type-salle (type-salle-i-resa resa))
      (tester-type-salle (type-i-salle (salle-i-resa resa))))))


;;; <ajout-site-p> - 02/04/96
;;; verifie que l'ajout peut etre modifie: i.e. si le type de cours et le type
;;; de salle sont accessible pour le site courant

(defun ajout-site-p (ajout)
  (or SITE-CENTRAL
    (and
      (tester-type-salle (type-salle-i-ajout ajout))
      (tester-type-salle (type-i-salle (salle-i-ajout ajout))))))

;;; <annul-site-p> - 09/05/96
;;; verifie que l'annulation peut etre modifie: i.e. le cours est accessible
;;; pour le site

(defun annul-site-p (annul)
  (donnee-modifiable-p (code-cours-i-annulation annul)))

;;; <tester-acces-objet> - CRE 01/04/96
;;; affiche un message d'erreur dans le cas ou nous ne pouvons pas modifier,
;;; supprimer l'objet dont nous passons le code en parametre.
;;; FD : cela devrait etre une fonction de la classe de l'objet

(defun tester-acces-objet (code-objet)
  (let
    ((objet (eval code-objet))) ;ne serait t-il pas possible de se passer du eval
    (selectq (type-of objet)
      ('#:tclass:i-type-cours
	(if (or SITE-CENTRAL (tester-type-cours code-objet))
	  t
	  (traiter-erreur)
	  ()))
      ('#:tclass:i-salle
	(if (tester-type-salle (type-i-salle code-objet))
	  t))
      ('#:tclass:i-cours
	(if (tester-types-cours code-objet)
	  t))
      ('#:tclass:i-groupe
	; *(every 'tester-types-cours (cours-i-groupe objet))
	(every 'tester-acces-objet (cours-i-groupe objet)))
      (t t))))

