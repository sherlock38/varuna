;;; --------------------------------------------------------------------------
;;; - Erreurs.ll                                               VARUNA 1.2    -
;;; - Utilitaires                                                            -
;;; - Utilitaires spécifiques a la gestion des erreurs                       -
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; --------------------------------------------------------------------------
;;; - Creation: 93 - 94                                        PR JH MO OJ   -
;;; - Derniere mise a jour pour version 1.13:  Novembre 1994 JH              -
;;; - Build 1.22.1 01.07.99 / JH                                             -
;;; --------------------------------------------------------------------------



;;; GESTION DES ERREURS          

(defvar *erreurs* ())
;;; CRE RM 19/12/96 : sauvegarde de la derniere erreur
(defvar *last-error* ())

(defun cons-erreur (code-erreur objets)
  (newl *erreurs* (cons code-erreur objets))
  (setq *last-error* (cons code-erreur objets)))

(defun erreur-suivante ()
  (nextl *erreurs*))

(defun delete-erreur ()
  (setq *erreurs* ()))

(defun message-erreur (cours code)
  (cons-erreur code (list cours)))

(defun constater-erreur (bool code)
  (let ((x (premier bool Liste-Cours)))
    (when x (message-erreur x code))))


;;; chg (09/05/94) MO: on autorise les cours sans prof

(defun controle-donnees ()
  (not
    (or
      (constater-erreur 'sans-type-p 'errNoType))))




;;; TRAITEMENT DE L'ERREUR

;;; redefinition de l'interrruption programmable syserror en lecture

(defun #:lecture:syserror (f m a)
  (if 
    (eq m 'errudv)
    (progn (newl tag-erreurs-lecture a) ())
    (syserror f m a)))


;;; prise en compte ou non du package lecture pour syserror
;;; arg      
;;;    t ->   prise en compte
;;;   () ->   arret de prise en compte

(defun switch-lecture (arg)
  (if arg
    (when (not (member 'lecture #:sys-package:itsoft))
      (newl #:sys-package:itsoft 'lecture))
    (when (member 'lecture #:sys-package:itsoft)
      (setq #:sys-package:itsoft (delq 'lecture #:sys-package:itsoft))))
  #:sys-package:itsoft)


;;; creation de l'erreur Varuna champ manquant dans un enregistrement

(defun construire-erreur-valeurs-manquantes (enr)
  (when Tag-Erreurs-Lecture
    (cons-erreur 'ERRNOVAL (list enr Tag-Erreurs-Lecture)) 
    (setq Tag-Erreurs-Lecture ())))



;;; Fonction de construction d'information a usage de l'interface quelle
;;; qu'elle soit


(defun cons-info (etat-nouveau pourcent cours instant)
  (when affichage-interface
    (funcall  
      affichage-interface
      etat-nouveau
      pourcent
      cours
      instant))
  ())





