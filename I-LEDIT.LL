;;; --------------------------------------------------------------------------
;;; - I-ledit.ll                                                VARUNA 1.2   -
;;; - Interface Graphique                                                    -
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; - Bibliotheque d'editeur: editeur de liste                               -
;;; --------------------------------------------------------------------------
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; - Creation:  Juillet 94                                                  -
;;; - FD-13/04/95 modifications pour gerer le focus                          -
;;; - Build 1.22.1 01.07.99 / JH MO                                          -
;;; --------------------------------------------------------------------------



;;; Chgmt FD - 23/03/95 - initialise-structure :
;;;            + => Ajouter
;;;            - =>Supprimer
;;; cet objet permet d'editer des liste de tout type (non objet)
;;; 


(eval-when (load eval local-compile)
  (defabbrev ledit {application}:ledit))

(eval-when (load eval local-compile)
  (defstruct {ledit}
    valeur
    accesseur
    add-func
    out-func
    sort-func
    print-func
    obli
    uniq))

(in-package '{ledit})

;;; II. Definition de l'editeur <ledit>
;;; 

;; arguments : l-value   -> liste des elements a editer
;;             accesseur -> un accesseur sur le champ de l'objet edite
;;             add-func  -> une fonction de demande pour les valeurs a ajouter
;;                          fonction a 1 argument, doit renvoyer une string
;;             out-func  -> une fonction de filtre de sortie
;;             obli      -> boolean : champ obligatoire ou non
;;             sort-func -> fonction de tri des elements dans la liste
;;             print-func-> fonction d'affichage (doit renvoyer un string)
;;             uniq      -> verifie que tous les elements de la liste sont #


(defun ledit (l-value
  accesseur
  add-func
  out-func
  sort-func
  print-func
  obli
  uniq)
    
  (:initialise-structure 
    (new '{ledit}) l-value accesseur
      add-func
      out-func
      sort-func
      print-func
      obli
      uniq))

(defun :initialise-structure (appli l-value accesseur add-func out-func
  sort-func print-func obli uniq)
  (with ((current-font FONT-EDITEUR))  
    (let*
      ((taille-bouton 
	(max (slength #Mv_ajouter) (slength #Mv_supprimer)))
       (b-plus
	 (standardbutton
	   (font FONT-EDITEUR
	     (cadrer-centrer #Mv_ajouter taille-bouton))))
       (b-moins
	 (standardbutton
	   (font FONT-EDITEUR
	     (cadrer-centrer #Mv_supprimer taille-bouton))))
       (view-buttons (row b-plus b-moins))
       (zone (stringmenu (mapcar print-func 
	 (if sort-func (sort sort-func l-value) l-value))))  
       (view-zone (standard-list 0 0 0 (send 'width view-buttons) zone))
       (view (constrainedview view-zone view-buttons)))
      (send 'update-constraint view view-zone (stretchconstraint 1 1 1 1))
      (send 'update-constraint view view-buttons (stretchconstraint 1 -1 -1 1))
      
      (set-action b-plus
        (lambda (b)
              (let* ((menu (component 'menu b))
                (scroller
                  (and
                    (componentp 'scroller menu)
                    (component 'scroller menu)))
                (ed (component 'ed b))
                (lv (mapcar (:out-func ed) (send 'get-strings menu)))
                (val1 (funcall (:add-func ed)))
                (val (and val1 (if (not (consp val1)) (list val1) val1)))
                (val-ok (if (:uniq ed)
                  (sous-liste (lambda (x) (not (member x lv))) val)
                  val)))
                    (when val-ok
                      (setq lv (append lv val-ok))
                      (send 'set-strings menu
                        (mapcar (:print-func ed)
                          (if (:sort-func ed)
                            (sort (:sort-func ed) lv)
                            lv))))
                    (when scroller
                      (send 'reinitialise scroller)))))
      
      
      (set-action b-moins
        (lambda (b)
          (let*
	    ((menu (component 'menu b))
            (v (send 'get-selected-string menu)))

	    (when v
	      (send 'set-strings menu
		(delete v (send 'get-strings menu)))))))
      
      
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

;;; FD-12/04/95
;;; pas de focus possible sur ce type d'application donc on passe le focus
;;; a la suivante 
(defun :current-keyboard-focus-window (ed)
  (:i-next ed))

(defun :readonly-flag (ed)
  t)

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
