;;; --------------------------------------------------------------------------
;;; - I-Hedit.ll                                                VARUNA 1.2   -
;;; - Interface Graphique                                                    -
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; - Bibliotheque d'editeur: editeur d'horaires                             -
;;; --------------------------------------------------------------------------
;;; - Copyright by Abstract - 1993-1996                                      -
;;; - Creation:  Mars 93                                                     -
;;; - Mise a jour pour version 1.07: Decembre 93                             -
;;; - Build 1.22.1 01.07.99 / JH MO                                          -
;;; --------------------------------------------------------------------------

;;; AJT FD-26/04/95 ajout de <:down-event> pour pouvoir envoyer un message
;;; <ascii-event> a l'application contenant un editeur d'horaire.
;;; - Cette focntion a ete ajoute pour que lorsque l'on clique dans un       -
;;; - editeur de type horaire on puisse dans la fenetre de reservation effacer
;;; - la liste des salles disponibles (rq la fonction d'effacement des salles-
;;; - disponibles est declenchee lorsque l'application contenant les champs  -
;;; - de la demande de reservation recoit le message <ascii-event> qui ne    -
;;; - declenche habituellement aucune reaction de la parts d'une applicaton  -
;;; - quand elle le recoit                                                   -

;;; revision 19/01/94 MO on enleve le format 8.30 (flottant) cf temps.ll
;;;


;;; remarque: cet editeur permet d'editer des horaires du type 12h30, 8h30...
;;;           il verifie que la donnee est bien un horaire valide et qu'il
;;;           se trouve dans la fourchette donnee en parametre  
;;; !!ces editeur utilise la notion de temps definie "ailleurs"

;;; hedit est une sous-classe de stringedit

(eval-when (load eval local-compile)
  (defabbrev hedit {stringedit}:hedit))

(eval-when (load eval local-compile)
  (defstruct {hedit}
    valeur
    accesseur
    minh
    maxh
    obligatoire
    saisissable))

(in-package '{hedit})



;;; Messages specifiques a l'editeur d'horaires
;;;

(defmessage must-be-hour
  (french "Heure ou Durée non valide")
  (english "Invalid Format"))

(defmessage hour-format
  (french "Le format doit être du type HHhMM")
  (english "Must be HHhMM"))

(defmessage hmust-be-less
  (french "L'horaire doit être antérieur à ")
  (english "The value must be before "))

(defmessage hmust-be-more
  (french "L'horaire doit être postérieur à ")
  (english "The value must be after "))


;;; Defintion de l'editeur <hedit>
;;;

(defun hedit (value accesseur min-value max-value obli saisissable)
  (:initialise-structure
    (new '{hedit}) value accesseur max-value min-value obli saisissable))

(defun :initialise-structure (editeur value accesseur max-value min-value
                              obli saisissable)
  (with ((current-font FONT-EDITEUR))
    ({stringedit}:initialise-structure
      editeur
      0
      0
      0
      (or (and value (cumule2naturel value)) "")
      ()))

  (add-component editeur 'user editeur)
  (:obligatoire editeur obli)
  (:valeur editeur value)
  (:accesseur editeur accesseur)
  (:maxh editeur max-value)
  (:minh editeur min-value)
  (:saisissable editeur saisissable)
  (set-action editeur #'(lambda (ed) (send 'i-next ed)))
  (when (not saisissable)
    (send 'inhibit-application editeur))
  editeur)


;;; AJT FD-28/04/95 pour pouvoir envoyer un message <ascii-event> a 
;;; l'application contenant cet editeur et ainsi declencher une 
;;; action
(defun :down-event (a ev)
  ({stringedit}:down-event a ev)
  (post-event 'ascii-event
      ({application}:father a)
      0)
  (post-event 'keyboard-focus-event a 0))


;;; AJT FD-04/05/95
;;; gestion des touches de fonctions
;(defun :functionkey-event (appli ev)
;  (post-event 'functionkey-event
;      ({application}:father appli)
;      ({event}:detail ev))
;  (post-event 'keyboard-focus-event appli 0)
;  )

;;; accesseurs : renvoie l'horaire cumule

(defun :i-get-heure (ed)
  (let ((h (send 'get-line ed)))
    (when (not (nullstrp h))
      (naturel2cumule h))))

;;; fonction de controle - renvoie t si tout est ok 

(defun :i-valider (ed)
  (let* ((hmax (send 'maxh ed))
    (hlib (and hmax (cumule2naturel hmax)))
    (hmin (send 'minh ed))
    (mlib (and hmin (cumule2naturel hmin)))
    (obli (send 'obligatoire ed))
    (valeur (send 'get-line ed)))

  (cond
    ((and (not obli) (nullstrp valeur)) t)      
    ((not (horaire-valide-p valeur 'naturel))
      (not (afficher-erreur
        (column
          #Mmust-be-hour
          #Mhour-format
          (row "(" (funcall (:accesseur ed) ()) ")")))))
    ((and hmax (> (naturel2cumule valeur)  hmax))
      (not (afficher-erreur
        (row #Mhmust-be-less hlib))))
    ((and hmin (< (naturel2cumule valeur) hmin))
      (not (afficher-erreur
        (row #Mhmust-be-more mlib))))
    (t t))))

;;; gestion du focus

(defun :i-next (ed)
  (current-keyboard-focus-window
    (send 'window (if (send 'i-valider ed) (next-user-appli ed 'user) ed))))

;;; sauvegarde

(defun :i-sauver (ed)
  (let ((objet-edite ({application}:userdata (component 'appli ed))))
    (and objet-edite
      (:accesseur ed)
      (funcall (:accesseur ed) objet-edite (:i-get-heure ed)))))

;;; le undo 

(defun :i-restore (ed)
  (let ((objet-edite ({application}:userdata (component 'appli ed))))
    (and objet-edite
      (:accesseur ed)
      (funcall (:accesseur ed) objet-edite (:valeur ed)))))

