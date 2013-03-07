;;; --------------------------------------------------------------------------
;;; - gestsite.ll                                              VARUNA 1.2    -
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; - Varuna reseau                                                          -
;;; - Routines associees a la gestion des sites                              -
;;; --------------------------------------------------------------------------
;;; - Creation:  Mars 96
;;; - Build 1.22.1 01.07.99
;;; --------------------------------------------------------------------------

;;; !!! FONCTIONS DE TEST !!!
;;; !!! il faut faire des defmessage 
;;; <tester-demande-type-cours>
;;; fonction permettant d'ajouter un type de cours

(defun tester-demande-type-cours ()
  (demandes-i-type-cours "Quels types de cours : " () t))

;;; <tester-print-type-cours>
;;; retourne une chaine composee du code + " - " + libelle du type de cours

(defun tester-print-type-cours (type-cours)
  (catenate
    (code-i-type-cours type-cours) " - " (libelle-i-type-cours type-cours)))

;;; <tester-sortie-type-cours>
;;; retourne le code du type de cours, si ici que nous devons verifier la
;;; validiter d'un type de cours

(defun tester-sortie-type-cours (type-cours)
  (symbol () (libelle-type-cours2type-cours type-cours)))

;;; <libelle-type-cours2type-cours>
;;; retourne le code d'un type de cours a partir de la chaine contenant le code
;;; et le libelle separes par " - "

(defun libelle-type-cours2type-cours (lib-type-cours)
  (let*
    ((tiret (index " - " lib-type-cours))
     (code (and tiret (substring lib-type-cours 0 tiret))))
    
    code))

;;; <tester-demande-type-salle>
;;; fonction permettant d'ajouter un type de salle

(defun tester-demande-type-salle ()
  (demandes-type "Quels types de salle : " (liste-type-salle) 'vrai () t))

;;; <tester-print-type-salle>
;;; fonction d'affichage d'un type de salle dans un editeur de type liste

(defun tester-print-type-salle (type-salle)
  (catenate type-salle))

;;; <tester-sortie-type-salle>
;;; fonction retournant un type de salle, si ici que nous devons verifier la
;;; validiter d'un type de salle

(defun tester-sortie-type-salle (type-salle)
  (symbol () type-salle))

