;;; --------------------------------------------------------------------------
;;; - I-obmenu.ll                                            VARUNA 1.2      -
;;; - Interface Graphique                                                    -
;;; - Utilitaire pour la bibliotheque d'editeurs                             -
;;; - Un selecteur pour gerer simultanement un identifiant et un libelle     -
;;; --------------------------------------------------------------------------
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; - Creation:  Fevrier 94                                       JH         -
;;; - Build 1.22.1 01.07.99 / JH MO                                          -
;;; --------------------------------------------------------------------------

;;; cet editeur fonctionne avec des objets pour lesquels sont definis
;;; les deux accesseurs : libelle-<objet> et code-<objet>.
;;; ou libelle-<objet> renvoie une chaine et code-<objet> est
;;; un symbole qui pointe sur l'objet (donc unique).

;;; obmenu est une sous-classe de stringmenu


(eval-when (load eval local-compile)
  (defabbrev doublestringmenu {stringmenu}:doublestringmenu))

(eval-when (load eval local-compile)
  (defstruct {doublestringmenu}
    convertir-code
    convertir-libelle))

(in-package '{doublestringmenu})

;;; conversion-code et conversion-libelle
;;; sont des fonction de conversion du code et du libelle en chaine.
;;; si conversion-libelle vaut nil. Seul le code de l'objet est affiche

(defun doublestringmenu (liste-objets conversion-code conversion-libelle)
  (:initialise-structure (new '{doublestringmenu}) liste-objets
    conversion-code conversion-libelle))

(defun :initialise-structure
  (menu liste-objets conversion-code conversion-libelle)
  ({stringmenu}:initialise-structure menu ())
  (:convertir-code menu conversion-code)
  (:convertir-libelle menu conversion-libelle)
  (:o-set-objects menu liste-objets)
  menu)

;;; renvoie l'image de l'objet objet.

(defun :format-double-id (menu objet)
  (catenate
    (funcall (:convertir-code menu) objet)
    (if (:convertir-libelle menu)
      (catenate
        " - "
        (funcall (:convertir-libelle menu) objet))
      "")))

;;; renvoie le code contenu dans l'image chaine du menu

(defun :extraire-code (menu chaine)
  (let* ((position-espace (index " " chaine))
    (code-objet (if position-espace 
      (substring chaine 0 position-espace)
      chaine)))
        (and
          code-objet
          (symbol () code-objet))))

;;; ajout de plusieurs objets
;;; cette fonction ne verifie pas que l'objet figure deja dans la liste
;;; affichee

(defun :o-set-objects (menu liste-objets)
  (let ((scroller (componentp 'scroller menu)))
    (when (car liste-objets)
      (send 'set-strings menu
        (mapcar (lambda (code-objet)
          (:format-double-id menu code-objet))
            liste-objets)))
    (when scroller (send 'reinitialise (component 'scroller menu)))))

;;; renvoie la liste des codes d'objets du menu

(defun :o-get-codes (menu)
  (mapcar
    (lambda (chaine) (send 'extraire-code menu chaine))
    (send 'get-strings menu)))

;;; renvoie les objets du menu

(defun :o-get-objects (menu)
  (evlis (send 'o-get-codes menu)))

;;; ajout d'un objet dans le menu

(defun :o-add-object (menu object)
  (and
    object
    (not (member (funcall (:convertir-code menu) object) (:o-get-codes menu)))
    (send 'set-strings menu
      (cons (:format-double-id menu object)
	(send 'get-strings menu)))
    (componentp 'scroller menu)
    
    (send 'reinitialise (component 'scroller menu))))

;;; supprime l'objet selectionne du menu
;;; renvoie l'objet supprimer evalue

(defun :o-delete-object (menu)
  (let* ((str (send 'get-selected-string menu)))
    (when str
      (send 'set-strings menu
	(delete str (send 'get-strings menu)))
      (eval (:extraire-code menu str))
      (when (componentp 'scroller menu)
	(send 'reinitialise (component 'scroller menu))))))

