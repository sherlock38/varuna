;;; --------------------------------------------------------------------------
;;; - Ecole.ll                                                  VARUNA 1.2   -
;;; - Utilitaires Generaux                                                   -
;;; - Calcul des globales sur l'ecole et qqs primitives bien utiles          -
;;; --------------------------------------------------------------------------
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; - Creation:  Septembre 94                                                -
;;; - Derniere mise a jour pour version 1.13:: Septembre                     -
;;; - Build 1.22.1 01.07.99 / JH MO                                          -
;;; --------------------------------------------------------------------------



;;; CONFIGURATION

(defun preinit ()
  (lire-fichier FICHIER-ECOLE 'moteur))

;;; CALCUL DES GLOBALES DE L'ECOLE

;;; il faut generer des erreurs en cas de () etc...

(defun calculer-globales-ecole ()
  (setq semaine-debut (premier-element semaines-debut-ecole))
  (setq semaine-fin (dernier-element semaines-debut-ecole))
  (setq jours-ouvres
    (sortn
      (supprimer-occurences-multiples 
        (mapcar 'jour-moment-cumule 
          (apply 'append liste-moments-debut-ecole)))))
  (setq horaire-cumule-ouverture
    (apply 'min (liste-horaires-cumules-ouverture)))
  (setq horaire-cumule-fermeture
    (apply 'max (liste-horaires-cumules-fermeture)))
  (setq Nb-Jours (length jours-ouvres))
  (setq Nb-Unites
    (div (sub horaire-cumule-fermeture horaire-cumule-ouverture) Unite-Temps))
  (setq Nb-Moments
    (mul Nb-Unites 7)))


;;; QUELQUES PRIMITIVES DE CALCUL SUR LES DONNEES DE L'ECOLE

;;; calcule la liste des horaires cumules d'ouverture (resp. fermeture) de l'ecole
;;; le rang dans la liste egale le rang du jour dans Jours
  
(defun liste-horaires-cumules-ouverture ()
  (mapcar 
    (lambda (int) (horaire-moment-cumule (borne-inf-intervalle int)))
    (trouver-dispo-moment Dispo-Ecole)))

(defun liste-horaires-cumules-fermeture ()
  (mapcar 
    (lambda (int) (horaire-moment-cumule (borne-sup-intervalle int)))
    (trouver-dispo-moment Dispo-Ecole)))


;;; calcule les moments de debut de cours (resp. semaines) par type de cours
;;; rem (13/09/94) a implementer une structure satifaisante de table dans
;;; ecole.edt
;;; type rect est un entier indiquant l'ordre des moments de debut
;;; dans liste-moments-debut-ecole (JH 2/10/94)

(defun moments-debut (type-rect)
  (when type-rect
    (nth type-rect
      Liste-Moments-Debut-Ecole)))

;;; pour l'instant, pas de semaines par type de cours

(defun semaines-debut (type-rect)
  Semaines-Debut-Ecole)    
