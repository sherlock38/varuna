;;; --------------------------------------------------------------------------
;;; - i-objets.ll                                               VARUNA 1.2   -
;;; - Des objets aida proprement definis par abstract                        -
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; --------------------------------------------------------------------------
;;; - Creation:  Juin  95 MO                                                 -
;;; - Derniere mise a jour pour version 1.13:                                -
;;; - Build 1.22.1 01.07.99 / MO JH                                          -
;;; --------------------------------------------------------------------------


;;; 1. LE THERMOMETRE
;;;

;;; un thermome^tre horizontal sans graduation avec un compteur a cote'

;;; l'instance mon-thermo
;;;

(defun mon-thermo (longueur hauteur bordure) 
  (let*
    ((met (meter longueur hauteur bordure))
     (pou (pourcent 18 "0"))
     (appli 
       (mixed-applicationq
         compteur met
         libelle pou
         name 'thermo
         image 
          (row 
               met
               (translation 2 2 pou)
               (translation 8 2 (font time8 "% "))))))
      (send 'show-value (component 'compteur appli) 0)
      (send 'set-line (component 'libelle appli) (string 0))        
      appli))


;;; la primitive de mise a jour du thermo
;;; cette primitive ne met a jour le thermo que si la valeur a change au moins 
;;; d'un point

(defun maj-thermo (thermo nombre)
  (when 
    (numberp nombre)
    (let 
      ((n (cond ((> nombre 100) 100)((< nombre 0) 0)(t (truncate nombre)))))
      (when 
        (not (eq n (send 'get-value (component 'compteur thermo))))
        (send 'show-value (component 'compteur thermo) n)
        (send 'set-line (component 'libelle thermo) (string n)))
      t)))


;;; les sous-instances de l'objet thermo
;;; meter et pourcent

(defun meter (longueur hauteur bordure) 
  (let ((vieille-hauteur {meter}:bar-size)
        (vieille-longueur {meter}:bar-size)
        (vieille-bordure {meter}:border-size))
    (setq {meter}:length longueur)
    (setq {meter}:bar-size hauteur)
    (setq {meter}:border-size bordure)
    (let 
      ((le-thermo (standardhorizontalmeter 0 100 () () () ())))
      (setq {meter}:bar-size vieille-hauteur)
      (setq {meter}:bar-size vieille-longueur)
      (setq {meter}:border-size vieille-bordure)
      le-thermo)))       

(defun pourcent (largeur texte)
  (with
    ((current-font time8)
     (current-background (make-named-color "white"))
     (current-foreground (make-named-color "black")))
    (let 
      ((editeur (stringedit 0 0 largeur texte)))
      (widget-set-args
        editeur
        'font (current-font)
        'foreground (current-foreground)
        'background (current-background))
      (send 'readonly-flag editeur t)
      editeur)))



;;; UN TITRE DIGNE DE CE NOM
;;;



(defun mon-titre (largeur texte)
  (with
    ((current-font arial8b)
     (current-background (trouver-couleur-varuna "jaune-pastel"))
     (current-foreground (make-named-color "black")))
    (let 
      ((editeur (stringedit 0 0 largeur texte)))
      (widget-set-args
        editeur
        'font (current-font)
        'foreground (current-foreground)
        'background (current-background))
      (send 'readonly-flag editeur t)
      editeur)))

(defun maj-mon-titre (titre texte)
  (send 'set-line titre texte))

(defun trouver-mon-titre (titre)
  (send 'get-line titre))
