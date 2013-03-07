;;; --------------------------------------------------------------------------
;;; - LLInit.ll                                                 VARUNA 1.2   -
;;; - Moteur Varuna Seul sous Lisp sans Aida                                 -
;;; - Module d'initialisation du moteur                                      -
;;; --------------------------------------------------------------------------
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; - Creation:  Fevrier 94                                         JH       -
;;; - Derniere mise a jour pour version 1.13:: Aout 94                       -
;;; - Build 1.22.1 01.07.99 / JH MO                                          -
;;; --------------------------------------------------------------------------



;;; Le init global, a charger a partir du prompt lisp.
;;; Copyright Abstract, France 1993-1996

;;; CHARGEMENT
;;;--------------------------------

(loadmodule 'sets)
(loadmodule 'hash)
(loadmodule 'edlin)
(loadmodule 'stringio)

(load "/usr/ilog/pecos/loadpeco.ll")


(load "/usr/varuna/varutils/precomp.lu")
(load "/usr/varuna/varutils/comp.lu")
(printlevel 4)

;;; CHARGEMENT DE VARUNA
;;;----------------------------------------------

(defun varuna (compile)
  (ifn compile
    (print "Varuna ne peut tourner en mode interprete")
    (loadmodule 'utilit)
    (loadmodule 'salles)
    (loadmodule 'moteur)))
    
    
(debug t)
(edlin)

;; VARUNA MODE INTERPRETE
					;;;(varuna ())

;; VARUNA MODE COMPILE
					(varuna t)

;;;; Redefinition d'une interface rudimentaire
;;; ces routines doivent etre definies apres le chargement
;;; des modules de varuna
;;;; ------------------------------------------

;;; redefinition du maybe-show-rect
(defun maybe-show-rect (rect)
  (afficher-rect rect))

(defun affichage-interface (a b c d) (print b " % - "  c))


;;; redefinition des routines d'erreur

(defun echec-contraintes (var constraint)
  (print "Erreur pendant la pose des contraintes")
  (exit contraintes
    (cons-erreur 'errContraintes (cons (code-rect (ct-object var))
      (list constraint)))))

 (defun interok ()
   (let ((ok (read)))
     (equal ok 't)))

 ;; Gestion des erreurs pour le d‚veloppeur
 ;; avec interruption possible 
 
 (defun echec-resolution (var constraint)
   (print "Echec en voulant placer le rect " (code-rect (ct-object var))
      " : " )
   (setq nombre-echecs (add1 nombre-echecs))
   (when (gt nombre-echecs seuil-echec)
     (print "Nombre-Echecs > " seuil-echec " - Interrompre ?")
     (if (interok)
       (exit running)
       (setq nombre-echecs 0))))

 ;;;(load "travail.ll")
 ;;;(lire-fichier "ecole.edt" 'moteur)
 ;;;(lire-donnees-moteur)
	 
	 
	 