;;; --------------------------------------------------------------------------
;;; - ms-ecrir.ll                                               VARUNA 1.2   -
;;; - Moteur des salles                                                      -
;;; - Module d'ecriture des donnees                                          -
;;; --------------------------------------------------------------------------
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; - Creation:  septembre 94                                                -
;;; - Mise a jour pour version 1.09: Septembre                               -
;;; - Build 1.22.1 01.07.99 / MO                                             -
;;; --------------------------------------------------------------------------

;; renvoie l'objet cours de l'enregistrement

(defun trouver-objet-ms (enr)
  (let ((objet ()))
    (mapc (lambda (ch-val)
      (when (eq (car ch-val) 'code)
	(let ((code (prefixer-code-ms (eval (cadr ch-val)))))
	  (when (boundp code)
	    (setq objet (eval code))))))
	      (cdr enr))
    objet))

;;; renvoie le nouvel enregistrement avec le champ
;;; instant-trouve a jour

(defun ecrire-nouveaux-cours-ms (dernier-index)
  ())

(defun ecrire-cours-ms (enr ind)
  (let ((co (trouver-objet-ms enr)))
    (when co
      (imprimer-nouvel-enregistrement enr co 'cours '(salle-trouvee) ()))))

(defun ecrire-resultat-ms ()
  (cons-info 'ecriture () () ())
  (ecrire-fichier FICHIER-COURS 'ms 'cours
    (lambda (x)
      (cons-info () (floor (* (/ x (length Liste-Cours)) 100)) () ()))))
