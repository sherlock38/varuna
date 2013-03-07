;;; --------------------------------------------------------------------------
;;; - I-Vedit.ll                                                VARUNA 1.2   -
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; - Interface Graphique                                                    -
;;; - Bibliotheque d'editeur: editeur de chaine de caracteres                -
;;; --------------------------------------------------------------------------
;;; - Creation:  Mars 93                                                     -
;;; - Mise a jour pour version 1.07: Decembre 93                             -
;;; - Build 1.22.1 01.07.99 / JH MO                                          -
;;; --------------------------------------------------------------------------

(eval-when (load eval local-compile)
  (defabbrev vedit {stringedit}:vedit))

(eval-when (load eval local-compile)
  (defstruct {vedit} 
    valeur
    obligatoire
    saisissable
    accesseur))

(defmessage v_sans-guillemet
  (french "Une chaîne ne doit pas comportée de guillemet")
  (english "Wrong format string"))

(in-package '{vedit})

(defun vedit (chaine accesseur obligatoire saisissable)
  (:initialise-structure (new '{vedit}) (or chaine "") accesseur saisissable))

(defun :initialise-structure (editeur chaine accesseur saisissable)
  (with ((current-font FONT-EDITEUR))
    ({stringedit}:initialise-structure
      editeur
      0
      0
      0 chaine))
  (add-component editeur 'user editeur)
  (:obligatoire editeur obligatoire)
  (:saisissable editeur saisissable)
  (:valeur editeur chaine)
  (:accesseur editeur accesseur)
  (set-action editeur #'(lambda (ed) (send 'i-next ed)))
  (when (not saisissable)
    (send 'inhibit-application editeur))
  editeur)

(defun :i-valider (ed)
  (let*
    ((valeur (send 'get-line ed))
     (erreur))
    ;; 28/02/96 AJT - FD controle qu'il n'y a pas de "
    (when (avec-guillemet-p valeur) 
      (setq erreur t)
      (afficher-erreur #Mv_sans-guillemet))
    (when
      (and (:obligatoire ed) (nullstrp valeur))
      ;; MOD RM - 10/04/96 : si le libelle est vide, on ne ferme pas l'editeur
      (setq erreur t)
      (alerte (funcall (:accesseur ed) ())))
    (not erreur)))

;;; commentaires : voir i-hedit.ll

(defun :i-next (ed)
  (current-keyboard-focus-window
    (send 'window (if (send 'i-valider ed) (next-user-appli ed 'user) ed))))

(defun :i-sauver (ed)
  (let ((objet-edite ({application}:userdata (component 'appli ed))))
    (and objet-edite
      (:accesseur ed)
      (funcall (:accesseur ed) objet-edite (send 'get-line ed)))))

(defun :i-restore (ed)
  (let ((objet-edite ({application}:userdata (component 'appli ed))))
    (and objet-edite
      (:accesseur ed)
      (funcall (:accesseur ed) objet-edite (:valeur ed)))))

;;; puisque'un stringedit ne gere pas un champ non saisissable
;;; on overwrite le ascii-event de l'appli 
;;; cette petite bidouille permet de s'en sortir
;;; cette fonction ne permet pas de filtrer les carracteres
;;; de commandes (DEL, CTRL+QQchose, ...)

(defun :ascii-event (appli event)
  (when
    (:saisissable appli)
    ({stringedit}:ascii-event appli event)))
