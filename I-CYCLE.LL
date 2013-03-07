;;; ------------------------------------------------------------------------
;;; - I-Cycle.ll                                               VARUNA 1.21 -
;;; - Copyright Abstract, France 1993-2006                                 -
;;; - under licence MIT Expat since 2006                                   -
;;; - Interface                                                            -
;;; - Module d'utilitaires pour la detection de cycles dans le grpahe      -
;;; - Aida                                                                 -
;;; ------------------------------------------------------------------------
;;; - Creation:  Oct 1997                                                  -
;;; - Build 1.22.1 01.07.99 / RM                                           -
;;; ------------------------------------------------------------------------

;;; Recherche de la presence de cycle dans le graphe.

;;; Procedure recursive qui consiste a eliminer successivement
;;; les racines d'un graphe.
;;; A la fin :
;;;   - la liste est vide : pas de cycle.
;;;   - il reste des familles : cycle

;;; La fonction graphe-cyclique retour () ou la liste des familles qui
;;; composent le cycle

(defun graphe-cyclique ()
  (let
    ((liste-travail liste-i-groupe)
     (liste-sauvee) 
     (continuerp t)) 

    (while continuerp
      (setq liste-sauvee liste-travail)
      (if (graphe-sans-racines-p liste-travail)
	(setq continuerp ())
	(setq liste-travail (liste-non-racines liste-travail))
	(when (equal liste-travail liste-sauvee)
	  (setq continuerp ()))))
    liste-travail))


;; Fonction qui retourne la liste des racines contenues dans un graphe
;; passe en parametre

(defun liste-racines (liste)
  (let
    ((ltmp liste))
    (sous-liste (lambda (f) (racine-p f ltmp)) liste)))

;; Fonction qui retourne la liste des familles non-racines d'un graphe

(defun liste-non-racines (liste)
  (let
    ((ltmp liste))
    (sous-liste (lambda (f) (not (racine-p f ltmp))) liste)))

;; Retourne t si le graphe contenu dans la liste de familles passee en 
;; parametre ne contient aucune racine

(defun graphe-sans-racines-p (liste)
  (not (any (lambda (f) (racine-p f liste)) liste)))

;; Retourne t si une famille est racine

(defun racine-p (famille liste)
  (not
    (any
      (lambda (f)
	(appartenir-liste famille (groupes-i-groupe f)))
      liste)))
