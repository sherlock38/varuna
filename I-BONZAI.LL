;;; --------------------------------------------------------------------------
;;; - i-Bonzai.ll                                               VARUNA 1.2   -
;;; - La premiere appli de Varuna                                            -  
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; --------------------------------------------------------------------------
;;; - Creation:  Juin 95 MO                                                  -
;;; - Derniere mise a jour pour version 1.13:                                -
;;; - Build 1.22.1 01.07.99 / MO FD                                          -
;;; --------------------------------------------------------------------------



(defvar Bonzai)

(defun initialiser-bonzai ()
  (setq Bonzai
    (let
      ((titre1 (mon-titre 15 #M v_nbcours))
       (titre2 (mon-titre 15 #M v_nbcours))
       (titre3 (mon-titre 15 #M v_nbcours))
       (titre4 (mon-titre 15 #M v_nbcours))
       (titre5 (mon-titre 15 #M v_nbcours))
       (bouton1 (standardbutton "réso"))
       (bouton2 (standardbutton "réso")))
       
      (mixed-applicationq
          name bonzai
          cours titre1
          famille titre2
          salle titre3
          prof titre4
          resa titre5
          image
          (column
              (rectangle 0 0 10 10)
              titre1
              (rectangle 0 0 10 10)
              titre2
              (rectangle 0 0 10 10)
              titre3
              (rectangle 0 0 10 10)
              titre4
              (rectangle 0 0 10 10)
              titre5
              (row
                  bouton1
                  (translation 50 0 bouton2)))))))

(defun maj-bonzai ()




(defun ajoute-mon-titre (titre texte)
  (send 'set-line titre (catenate (send 'get-line titre) texte)))
