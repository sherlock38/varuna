;; ---------------------------------------------------------------------------
;; - I-Vappli.ll                                                VARUNA 1.2   -
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;; - Interface Graphique                                                     -
;; - Classes d'applications de Varuna, gestion du panneau                    -
;; - principal                                                               -
;; ---------------------------------------------------------------------------
;; - Creation: Mars 93                                                       -
;; - Mise a jour pour version 1.07: décembre 93                              -
;; - Build 1.22.1 01.07.99 / JH MO                                           -
;; ---------------------------------------------------------------------------



(eval-when (load eval local-compile)
  (defabbrev v-application {application}:v-application))

(eval-when (load eval local-compile)
  (defstruct {v-application}
    v-image))

(in-package '{v-application})

(defun v-application (x y w h image)
  (:initialise-structure 
    (new '{v-application}) x y w h image))

(defun :initialise-structure (appli x y w h image)
  (:v-image appli image)
  (send-super '{v-application} 'initialise-structure appli x y w h
    (elasticview image)))

(defun :prin (app)
  (prin "APPLICATION (" (send 'x app) "," (send 'y app)")"))

; ajoute une application, caclule ses coordonnes a partir 
; des numeros de ligne et de colone passees en parametres

(defun :v-add-application (v-appli appli comp-name rect sl nsl tsl border)
  (let* 
    ((pos (send 'rectangle2position (:v-image v-appli) rect))
      (x (x-pos-rect pos))
      (y (y-pos-rect pos))
      (w (w-pos-rect pos))
      (h (h-pos-rect pos))
      (h1 (round (/ h tsl) 1))
      (ofset-x (send 'x (send 'window v-appli)))
      (ofset-y (send 'y (send 'window v-appli)))
      (new-y (add y (round (/ (* sl h) tsl) 1)))
      (new-h (round (/ (* nsl h) tsl) 1))
      (rect-aida (rectangle (add x ofset-x) (add new-y ofset-y)
	(add1 w)
	(add1 h)))
      (app
	(if border
	  (translation x new-y appli)
	  (translation (add1 x) (add1 new-y) appli))))
    (setq app
      (send 'grow app
	(if border (add1 w) (sub1 w))
	(if border (add1 new-h) (sub1 new-h)))) 
    
    (add-component v-appli comp-name app)
    (send 'insert-image v-appli app 32000)
    (send 'redisplay v-appli rect-aida)
    ))

; retire toutes les images de v-appli
;; on ne retire que les rectangle de cours

(defun :v-remove-applications (v-appli)
  (mapc (lambda (comp)
    (when (eq (type-of comp) '#:tclass:cours)
      (print comp)
      (send 'remove-image v-appli (component comp v-appli))
      (remove-component v-appli comp)))
  (all-components v-appli)))

; retire l'image de nom de composant comp-name de v-appli

(defun :v-remove-application (v-appli comp-name)
  (let* ((appli (component comp-name v-appli))
    (x (and appli (send 'x appli))) (y (and appli (send 'y appli)))
      (w (and appli (send 'width appli))) (h (and appli (send 'height appli)))
	(ofset-x (send 'x (send 'window v-appli)))
	(ofset-y (send 'y (send 'window v-appli)))
	(rect (rectangle (add x ofset-x) (add y ofset-y) w h))
	(blanc-rect (and x (filledbox x y w h 0))))
	  (when appli
	    (send 'remove-image v-appli appli)
	    ({application}:insert-image v-appli blanc-rect 1)
	    (send 'redisplay v-appli rect)
	    (send 'remove-image v-appli blanc-rect)
	    (send 'redisplay v-appli rect)
	    (remove-component v-appli comp-name))))

