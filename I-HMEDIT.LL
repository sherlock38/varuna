 ;;; --------------------------------------------------------------------------
;;; - I-Hmedit.ll                                             VARUNA 1.2     -
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; - Interface Graphique                                                    -
;;; -                                                                        -
;;; --------------------------------------------------------------------------
;;; - Copyright by Abstract & P. Riviere - 1993-1996                         -
;;; - Creation: Avril 1995 (FD)                                              -
;;; - Mise a jour pour version                                               -
;;; - Build 1.22.1 01.07.99 / MO FD                                          -
;;; --------------------------------------------------------------------------


;;; definition d'un <stringmenu> auquel est lie d'autres <stringmenu>
;;; !!FD cette fonction doit etre mise dans i-util.ll
;;; arguments:
;;;    w -> largeur
;;;    h -> hauteur
;;;    liste-maitre -> liste des elements a edite dans le 1er stringmenu
;;;    listes-esclaves -> liste des elements lies a la liste precedente

(defun multi-select (w h chmp-maj liste-maitre . liste-esclaves)
   (let* ((maitre (stringmenu (mapcar 'string 
                                     liste-maitre)))
         (esclaves (mapcar (lambda (l)
                             (stringmenu (mapcar 'string l)))
                           liste-esclaves))
          (view (column ;;;constrainedview
                    (apply 'row
                        (cons maitre esclaves))))
         (appli (standard-list 0 0 6 400
                    view)))

      (add-component appli 'scroller (component 'scroller appli))

        (add-component appli
                 'maitre
                 maitre)
        (add-component appli
                 'esclaves
                 esclaves)
        ;;; action associee au <stringmenu> maitre: permet de faire defiler
        ;;; les <stringmenu> associee en paralelle avec celui-ci
        ;;; l'action doit-elle etre passee en parametre et ou etre fixe?
        ;;; Rajouter un test sur chmp-maj pour tester ()
         (set-action maitre
           (lambda (a)
             (and
              (mapcar (lambda (l)
                   (send 'set-selected-rank
                     l
                     (send 'get-selected-rank a)))
                 (send 'component 'esclaves a))
               (send 'set-line 
                   (component (component 'chmp-maj a) a)
                   (car (send 'i-get-value (component 'user a)))))))
        ;;; inhibition des <stringmenu> associes
       ;; (mapc 'inhibit-application
       ;;         esclaves)
        ;; FD - 23/05/95
        ;; inhibition des stringmenus associes sans modification
        ;; de leur aspect contrairement a <inhibit-application>
        (mapc (lambda (a)
                (send 'set-inhibit-flag 
                    a
                    t))
            esclaves)
        appli
))

(eval-when (load eval local-compile)
  (defabbrev hmedit {application}:hmedit))

(eval-when (load eval local-compile)
  (defstruct {hmedit}
    accesseur
    sort-func
    out-func
    l-value
    l-value2))

(in-package '{hmedit})

;;; Definition de l'editeur <hmedit>

;;; arguments:
;;;      accesseur -> un accesseur sur le champs de l'objet edite
;;;      out-func -> une fonction de filtre de sortie
;;;      sort-func -> une fonction de tri des elements de la liste
;;;      l-value   -> liste des elements a edites
;;;      l-value2  -> listes des elements associes a la liste precedente

(defun hmedit (titre accesseur
  chmp-maj
  out-func
  sort-func 
  l-value . l-value2)

  (:initialise-structure 
    (new '{hmedit})
    accesseur
    chmp-maj
    out-func
    sort-func
    l-value
    l-value2))

(defun :initialise-structure (appli accesseur chmp-maj out-func sort-func
  l-value l-value2)
 (with ((current-font FONT-EDITEUR)) 
 (let
    ((view ;;;boxedview
      (apply 'multi-select 
	(append (list 200 50 chmp-maj l-value) l-value2))))
         ;;; !fd largeur et hauteur fixe?    
    (:accesseur appli accesseur)
    (:l-value appli l-value)
    (:l-value2 appli l-value2)
    (:out-func appli out-func)
    (:sort-func appli sort-func)
    (add-component appli 'user appli)
    (add-component appli 'menu view)
    (add-component appli 'ed appli)
    (add-component appli 'chmp-maj chmp-maj)
    (add-component appli 'scroller (component 'scroller view))
    ({application}:initialise-structure appli
      0 0 300 50
      view)
    appli
    )))

(defun :i-set-values (ed liste-maitre . liste-esclaves)
  (let ((menu (component 'menu ed)))
    (send 'set-strings (component 'maitre menu) liste-maitre)
    (mapcar (lambda (e l)
                  (send 'set-strings e l))
        (component 'esclaves menu)
        liste-esclaves)
        (send 'reinitialise ed)))


(defun  :i-get-value (ed)
  (let* ((menu (component 'menu ed))
        (valeur (or (send 'get-selected-string 
                        (component 'maitre menu))
                    ""))
        (valeurs (mapcar (lambda (m)
                           (or (send 'get-selected-string m)
                               ""))
                         (component 'esclaves menu))))
;;; !FD rq ajouter la fonction de filtre de sortie (out-func)
      (funcall  (:out-func ed))
        (cons valeur valeurs)
))
       
(defun :i-get-valider (ed)
       t)

(defun :i-next (ed)
  (current-keyboard-focus-window
    (send 'window 
      (if (send 'i-valider ed) 
        (next-user-appli ed 'user)
        ed))))

;;; FD-02/05/95, on ne donne jamais le controle du clavier a ce type
;;; d'appli
(defun :readonly-flag (ed)
  ())

(defun :i-valider (ed)
  t)

(defun :i-sauver (ed)
  t
)

(defun :i-restore (ed)
  t
)

;;; cette fonction est specifique a l'editeur de resa
(defun :reinitialise (ed)
  (let* ((defileur (component 'scroller ed))
         (menu (component 'menu ed))
         (nl (length (send 'get-strings 
                         (component 'maitre menu)))))
    (send 'modify-scrollbar
        (send 'verticalscrollbar defileur)
        0
        (mul nl 14) 
        #7hch  ;;; 7 lignes de hauteur
        10 0)
    (send 'ensure-visible defileur 0 0)
    (send 'full-redisplay (component 'salles-disponibles ed))
))
