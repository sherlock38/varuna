;;; --------------------------------------------------------------------------
;;; - i-string.ll                                               VARUNA 1.2   -
;;; - display d'une chaine avec gestion des sauts de ligne                   -
;;; -                                                                        -
;;; --------------------------------------------------------------------------
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; - Creation:  Septembre 94                                                -
;;; - Derniere mise a jour pour version 1.13:: Septembre                     -
;;; - Build 1.22.1 01.07.99 / JH MO                                          -
;;; --------------------------------------------------------------------------

(eval-when (load eval local-compile)
  (defabbrev chaine {rectangle}:chaine))

(eval-when (load eval local-compile) 
  (defstruct {chaine} 
    the-string
    nlig
    font))

(in-package '{chaine})

(defun chaine (x y w font n-lig chaine)
  (:initialise-structure (new '{chaine}) x y w font n-lig chaine))


(defun :initialise-structure (chaine x y w font n-lig the-string)
  (with ((current-font font))
    ({rectangle}:initialise-structure chaine x y w (mul n-lig #hchar)))
  (:the-string chaine (list the-string))
  (:nlig chaine n-lig)
  (:font chaine font)
  (:adjust-string chaine)
  chaine)


(defun :prin (chaine)
  (prin "<chaine;" (:the-string chaine) ">"))


(defun :adjust-string (chaine)
  (with ((current-font (:font chaine)))
    (:the-string
      chaine
      (split-string
	(apply 'catenate (:the-string chaine))
	(:nlig chaine)
	(send 'width chaine)))))
    
  
(defun :grow (chaine dx dy)
  (setq chaine ({rectangle}:grow chaine dx dy))
  (:adjust-string chaine))
 


;;; trouver la globale pour 10 (largeur du titre de fenetre windows)
    
(defun :display (chaine dx dy region)
  (with ((current-font (:font chaine)))
    (let* ((w (send 'width chaine))
      (h (send 'height chaine))
      (l-buffer (:the-string chaine))
      (ligne 0)
      (taille-ligne #hchar)
      (bordure-fenetre 10)
      (marge-x 3))
	(while l-buffer
	  (draw-string
	    (add dx marge-x)
	    (add dy (add bordure-fenetre (mul ligne taille-ligne)))
	    (nextl l-buffer))
	  (incr ligne)))))

	
      