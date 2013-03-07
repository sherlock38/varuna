;;; --------------------------------------------------------------------------
;;; - m-ecrire.ll                                               VARUNA 1.2   -
;;; - Moteur                                                                 -
;;; - Module d'ecriture des donnees et calcul des principaux champs          -
;;; --------------------------------------------------------------------------
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; - Creation:  septembre 94                                                -
;;; - Mise a jour pour version 1.09: Septembre                               -
;;; - Build 1.22.1 01.07.99 / MO                                             -
;;; --------------------------------------------------------------------------


;; renvoie l'objet rect de l'enregistrement

(defun trouver-objet-moteur (enr)
  (let ((objet ()))
    (mapc (lambda (ch-val)
      (when (eq (car ch-val) 'code)
        (let ((code (prefixer-code-moteur (eval (cadr ch-val)))))
          (when (boundp code)
            (setq objet (eval code))))))
              (cdr enr))
    objet))


(defun ecrire-nouveaux-cours-moteur (dernier-index)
  ())

;;; renvoie le nouvel enregistrement avec le champ
;;; instant-trouve a jour

(defun ecrire-cours-moteur (enr ind)
  (let ((rect (trouver-objet-moteur enr)))
    (when rect
      (imprimer-nouvel-enregistrement enr rect 'rect '(instant-trouve) ()))))

;;; <ecriture-resultat-moteur>
;;; FD - MOD - 20/07/95
;;; dans le cas d'une resolution partielle, Liste-Rect est egale au
;;; nombre de cours sur lequel se fait la resolution et non pas au
;;; nombre total de cours (qui correspond a 100%), ce qui fait que
;;; le themo arrivait a 100 avant que l'ecriture ne soit finie.
(defun ecrire-resultat-moteur ()
  (cons-info 'ecriture () () ())
  (ecrire-fichier FICHIER-COURS 'moteur 'cours 
    (lambda (x)
;; 20/07/95     (cons-info () (floor (* 100 (/ x (length Liste-Rect)))) () ()))))
      (cons-info () (floor (* 100 (/ x (length Liste-I-Cours)))) () ()))))

