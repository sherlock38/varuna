;;; --------------------------------------------------------------------------
;;; - I-bool.ll                                                 VARUNA 1.2   -
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; - Interface Graphique                                                    -
;;; - Bibliotheque d'editeur: editeur de booleens (oui/non)                  -
;;; --------------------------------------------------------------------------
;;; - Creation:  Mars 93                                                     -
;;; - Mise a jour pour version 1.07: Decembre 93                             -
;;; - Build 1.22.1 01.07.99 / JH MO                                          -
;;; --------------------------------------------------------------------------
;;; AJT FD - 16/08/95 ajout de <:down-event> pour pouvoir envoyer un message -
;;; <ascii-event> a l'application contenant un editeur d'horaire.            -
;;; - Cette focntion a ete ajoute pour que lorsque l'on clique dans un       -
;;; -editeur de type booleen on puisse dans la fenetre de reservation effacer-
;;; - la liste des salles disponibles (rq la fonction d'effacement des salles-
;;; - disponibles est declenchee lorsque l'application contenant les champs  -
;;; - de la demande de reservation recoit le message <ascii-event> qui ne    -
;;; - declenche habituellement aucune reaction de la parts d'une applicaton  -
;;; - quand elle le recoit                                                   -
;;; --------------------------------------------------------------------------

(eval-when (load eval local-compile)
  (defabbrev bool {checkbox}:bool))

(eval-when (load eval local-compile)
  (defstruct {bool} 
    valeur
    accesseur))

(in-package '{bool})

(defun bool (val accesseur)
  (:initialise-structure (new '{bool}) val accesseur))

(defun :initialise-structure (editeur val accesseur)
  (with ((current-font FONT-EDITEUR))
    ({checkbox}:initialise-structure
      editeur
      0
      0
      #wchar                              
;(send 'height {checkbox}:on-icon)
      #1hchar ""))
  (add-component editeur 'user editeur)
  (:accesseur editeur accesseur)
  (:valeur editeur val)
  (send 'checked editeur val)  
  editeur)

(defun :i-valider (ed)
  t)
(defun :i-next (ed)
  t)

(defun :i-sauver (ed)
  (let 
    ((state (send 'checkedp ed))
      (objet-edite ({application}:userdata (component 'appli ed))))
    (and objet-edite
      (:accesseur ed)
      (funcall (:accesseur ed) objet-edite state))))


(defun :i-restore (ed)
  (let ((objet-edite ({application}:userdata (component 'appli ed))))
    (and objet-edite
      (:accesseur ed)
      (funcall (:accesseur ed) objet-edite (:valeur ed)))))

;;; FD-25/07/95, on ne donne jamais le controle du clavier a ce type
;;; d'appli
(defun :readonly-flag (ed)
  ())

;;; AJT FD-16/08/95 pour pouvoir envoyer un message <ascii-event> a 
;;; l'application contenant cet editeur et ainsi declencher une 
;;; action
(defun :down-event (a ev)
  ({checkbox}:down-event a ev)
  (post-event 'ascii-event
      ({application}:father a)
      0)
  (post-event 'keyboard-focus-event a 0))
