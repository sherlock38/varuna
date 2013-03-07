;;; --------------------------------------------------------------------------
;;; - I-Iedit.ll                                                VARUNA 1.2   -
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; - Interface Graphique                                                    -
;;; - Bibliothèque d'editeur: editeur de nombres entiers                     -
;;; --------------------------------------------------------------------------
;;; - Creation:  Mars 93                                                     -
;;; - Mise a jour pour version 1.07: Decembre 93                             -
;;; - FD-13/04/95 modification de <i-valider>                                -
;;; - Build 1.22.1 01.07.99 / JH MO                                          -
;;; --------------------------------------------------------------------------

;;; AJT FD-26/04/95 ajout de <:down-event> pour pouvoir envoyer un message
;;; <ascii-event> a l'application contenant un editeur d'entiers.
;;; - Cette focntion a ete ajoute pour que lorsque l'on clique dans un       -
;;; - editeur d'entiers on puisse dans la fenetre de reservation effacer
;;; - la liste des salles disponibles (rq la fonction d'effacement des salles-
;;; - disponibles est declenchee lorsque l'application contenant les champs  -
;;; - de la demande de reservation recoit le message <ascii-event> qui ne    -
;;; - declenche habituellement aucune reaction de la parts d'une applicaton  -
;;; - quand elle le recoit                                                   -
;;; 
;;; AJT - FD - 01/08/96
;;; ajout du champ saisissable pour indiquer si nous pouvons saisir une valeur
;;; ou non


(eval-when (load eval local-compile)
  (defabbrev iedit {stringedit}:iedit))

(eval-when (load eval local-compile)
  (defstruct {iedit}
    valeur
    accesseur
    max
    min
    must-be-intp
    obligatoire
    saisissable
    filtre-lecture
    filtre-affichage))

;;; messages specifiques

(defmessage must-be-int
  (french "La valeur doit être un nombre entier")
  (english "The value must be an integer"))

(defmessage must-be-a-number
  (french "La valeur doit être un nombre")
  (english "The value must be a number"))

(defmessage imust-be-less
  (french "La valeur doit être inférieure ou égale à ")
  (english "The value must be less than "))

(defmessage imust-be-more
  (french "La valeur doit être supérieure ou égale à ")
  (english "The value must be more than "))

(in-package '{iedit})

(defun iedit (value accesseur min-value max-value must-be-int obli saisissable filtre-lecture filtre-affichage)
  (:initialise-structure (new '{iedit}) value accesseur
    max-value
    min-value
    must-be-int
    obli
    saisissable
    filtre-lecture
    filtre-affichage))

(defun :initialise-structure (editeur value accesseur
  max-value
  min-value
  must-be-int
  obli
  saisissable
  filtre-lecture
  filtre-affichage)
  (with ((current-font FONT-EDITEUR))
    ({stringedit}:initialise-structure
      editeur
      0
      0
      0
      (string value)
      ()))

;;; DBG JH 23/11 : ajout fonction filtre
  ;; rem FD - 31/07/96 au lieu d'avoirune lambda il serait preferable
  ;; de definir un predicat
  (send 'filter-func editeur            ; cette fonction filtre
    (lambda (ed code)                   ; tout ce qui n'est pas nombre
      (if (or (digitp code) (eq code #/.)) t)))
;;;
  (add-component editeur 'user editeur)
  (:accesseur editeur accesseur)
  (:valeur editeur value)
  (:max editeur max-value)
  (:min editeur min-value)
  (:must-be-intp editeur must-be-int)
  (:obligatoire editeur obli)
  (:saisissable editeur saisissable)
  (:filtre-lecture editeur filtre-lecture)
  (:filtre-affichage editeur filtre-affichage)
  (set-action editeur #'(lambda (ed) (send 'i-next ed)))
  (when (not saisissable)
    (send 'inhibit-application editeur))
  editeur)

;;; Commentaires : cf i-hedit.ll

;;; AJT FD-28/04/95 pour pouvoir envoyer un message <ascii-event> a 
;;; l'application contenant cet editeur et ainsi declencher une 
;;; action
(defun :down-event (a ev)
  ({stringedit}:down-event a ev)
  (post-event 'ascii-event
      ({application}:father a)
      0)
  (post-event 'keyboard-focus-event a 0))

(defun :i-next (ed)

  (current-keyboard-focus-window
    (send 'window (if (send 'i-valider ed) (next-user-appli ed 'user) ed))))

(defun :i-get-nombre (ed)
  (read-from-string (send 'get-line ed)))

;;; FD-13/04/95 chg: Il faut ne faut tester la valeur que si il y a qqch
;;; dans le champ => ajout de <ligne> et tests sur cette valeur

(defun :i-valider (ed)
  (let* ((ligne (send 'get-line ed))
         (valeur
	   (if (:filtre-lecture ed)
	     (funcall (:filtre-lecture ed) (read-from-string ligne))
	     (read-from-string ligne))))
    (cond
      ((and 
           (:obligatoire ed)
           (nullstrp ligne))
        (not (afficher-erreur
	    (row #Mv_le-champ (funcall (:accesseur ed) ()) #Mv_obligatoire))))
      ((and (not (nullstrp ligne)) (:must-be-intp ed) (not (integerp valeur)))
        (not (afficher-erreur
          (column
            #Mmust-be-int
            (row "(" (funcall (:accesseur ed) ()) ")")))))
      ((and (not (nullstrp ligne)) (not (numberp valeur)))
        (not (afficher-erreur
          (column
            #Mmust-be-a-number
            (row "(" (funcall (:accesseur ed) ()) ")")))))
      ((and (not (nullstrp ligne)) (:max ed) (> valeur (:max ed)))
        (not (afficher-erreur
          (column
            (catenate
	      #Mimust-be-less
	      (string
		(if (:filtre-affichage ed)
		  (funcall (:filtre-affichage ed) (:max ed))
		  (:max ed))))
	    (row "(" (funcall (:accesseur ed) ()) ")")))))
      ((and (not (nullstrp ligne)) (:min ed) (< valeur (:min ed)))
        (not (afficher-erreur 
          (column
            (catenate
	      #Mimust-be-more
	      (string
		(if (:filtre-affichage ed)
		  (funcall (:filtre-affichage ed) (:min ed))
		  (:min ed))))
	    (row "(" (funcall (:accesseur ed) ()) ")")))))
      (t t))))

(defun :i-sauver (ed)
  (let
    ((objet-edite ({application}:userdata (component 'appli ed)))
     (nombre
       (if (:filtre-lecture ed)
	 (funcall (:filtre-lecture ed) (:i-get-nombre ed))
	 (:i-get-nombre ed))))
    (and objet-edite
      (:accesseur ed)
      (funcall
	(:accesseur ed) objet-edite
	nombre))))

(defun :i-restore (ed)
  (let ((objet-edite ({application}:userdata (component 'appli ed))))
    (and objet-edite
      (:accesseur ed)
      (funcall (:accesseur ed) objet-edite (:valeur ed)))))


