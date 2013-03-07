;;; --------------------------------------------------------------------------
;;; - I-Hyedit.ll                                             VARUNA 1.2     -
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; - Interface Graphique                                                    -
;;; -                                                                        -
;;; --------------------------------------------------------------------------
;;; - Copyright by Abstract & P. Riviere - 1993-1996                         -
;;; - Creation: Mars 1995 (FD)                                               -
;;; - Mise a jour pour version                                               -
;;; - Build 1.22.1 01.07.99 / MO FD                                          -
;;; --------------------------------------------------------------------------


(eval-when (load eval local-compile)
  (defabbrev hyedit {application}:hyedit))

(eval-when (load eval local-compile)
  (defstruct {hyedit}
    valeur
    accesseur
    add-func
    out-func
    sort-func
    print-func
    obli
    uniq))

(in-package '{hyedit})

;;; Definition de l'editeur <hyedit>

;; arguments : l-value   -> liste des elements a editer
;;             accesseur -> un accesseur sur le champ de l'objet edite
;;             add-func  -> une fonction de demande pour les valeurs a ajouter
;;                          fonction a 1 argument, doit renvoyer une string
;;             out-func  -> une fonction de filtre de sortie
;;             obli      -> boolean : champ obligatoire ou non
;;             sort-func -> fonction de tri des elements dans la liste
;;             print-func-> fonction d'affichage (doit renvoyer un string)
;;             uniq      -> verifie que tous les elements de la liste sont #


(defun hyedit (l-value
  accesseur
  add-func
  out-func
  sort-func
  print-func
  obli
  uniq)
    
  (:initialise-structure 
    (new '{hyedit}) l-value accesseur
      add-func
      out-func
      sort-func
      print-func
      obli
      uniq))

(defun :initialise-structure (appli l-value accesseur
  add-func
  out-func
  sort-func
  print-func
  obli
  uniq)
  (with ((current-font FONT-EDITEUR))    
    (let* (
      (zone (stringmenu (mapcar print-func 
        (if sort-func (sort sort-func l-value) l-value))))  
      (view-zone  (hypersel  200 50 l-value))
      (view (constrainedview view-zone )))
      (send 'update-constraint view view-zone (stretchconstraint 1 1 1 1))
      
      (:accesseur appli accesseur)
      (:valeur appli l-value)
      (:add-func appli add-func)
      (:out-func appli out-func)
      (:sort-func appli sort-func)
      (:print-func appli print-func)
      (:uniq appli uniq)
      (add-component appli 'user appli)
      (add-component appli 'menu zone)
      (add-component appli 'ed appli)
      ({application}:initialise-structure appli
        0 0 0 0
        view)
      appli)))



(defun :i-get-values (ed)
  (let ((values (send 'get-strings (component 'menu ed))))
    (mapcar (lambda (v) (funcall (:out-func ed) v)) values)))

(defun :i-valider (ed)
  t)

(defun :current-keyboard-focus-window (ed)
  (:i-next ed))

(defun :i-next (ed)
  (current-keyboard-focus-window
    (send 'window (if (send 'i-valider ed) (next-user-appli ed 'user) ed))))

(defun :i-sauver (ed)
  (let ((objet-edite ({application}:userdata (component 'appli ed))))
    (and objet-edite
      (:accesseur ed)
      (funcall (:accesseur ed) objet-edite (:i-get-values ed)))))


(defun :i-restore (ed)
  (let ((objet-edite ({application}:userdata (component 'appli ed))))
    (and objet-edite
      (:accesseur ed)
      (funcall (:accesseur ed) objet-edite (:valeur ed)))))

)
