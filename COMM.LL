;;; --------------------------------------------------------------------------
;;; - Comm.ll                                                   VARUNA 1.2   -
;;; - Communication (succincte) entre interface AIDA et Moteur VARUNA        -
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; --------------------------------------------------------------------------
;;; - Creation:  Aout 94 JH                                                  -
;;; - Build 1.22.1 01.07.99 / MO JH                                          -
;;; --------------------------------------------------------------------------

;;; le but du jeu est pour le moment de traduire les concepts
;;; interface en concepts 
;;; moteur
;;;

;;; libelles de differents etats de la resolution

(defmessage resolution
  (french "Résolution")
  (english "Solving"))

(defmessage preparation
  (french "Préparation")
  (english "Preparing"))

(defmessage affectation-salles
  (french "Affectation des salles")
  (english "???"))

;;; unique fonction de l'interface appelee par le moteur

(defun action-resolution (nouvel-etat ofset code-cours instant-cours)
  (when nouvel-etat                     ; mise a jour du titre
    (donner-titre (get-message nouvel-etat)))              
  (when ofset                           ; mise a jour du thermometre 
    (mise-a-jour-thermometre ofset 100))
  (when code-cours                      ; affichage d'un cours
    (afficher-cours-en-resolution code-cours instant-cours)))
    
;;; appel au moteur cours

(defun resolution-moteur (liste-codes)
  (setq Affichage-Interface 'action-resolution)
  (funcall 'lancer-moteur liste-codes)
  (setq Affichage-Interface ()))

;;; appel au moteur salle

(defun resolution-salles ()
  (setq Affichage-Interface 'action-resolution)
  (funcall 'moteur-salle)
  (setq Affichage-Interface ()))
