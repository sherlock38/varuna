;;; --------------------------------------------------------------------------
;;; - ms-noyau.ll                                              VARUNA 1.2    -
;;; - Moteur des salles                                                      -
;;; - Utilitaires spécifiques a la gestion des salles et noyau               -
;;; --------------------------------------------------------------------------
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; - Creation: 93 - 94                                        PR JH MO OJ   -
;;; - Derniere mise a jour pour version 1.13::  07.11.94                     -
;;; - Build 1.22.1 01.07.99 / JH                                             -
;;; --------------------------------------------------------------------------


;;; AJT (11/04/95) MO
;;; rajout de la primitive <maj-salles-reservees> dans <calculer-salles>
;;; on prend en compte la notion de salle reservee
;;; a priori, on ne verifie pas la coherence des resa entre elles
;;; la nouvelle primitive se trouve dans ms-util.ll

;;; REM (25/10/94) MO
;;; j'ai modifie <calculer-salles> <lire-cours-ms> et le concept cours
;;; pour pouvoir supporter des salles deja figees
;;; je n'ai en rien modifie l'ancien moteur pour le moment
;;; il merite pourtant une verif sur ce qui est evalue, ce qui ne l'est pas
;;; etc...

;;; chg (09/11/94) MO
;;; j'ai rajoute un argument a calculer-salles, a savoir une liste
;;; en entree dans moteur-salles, je n'envoie que les cours ayant ete 
;;; instancies

;;; MOD RM 06/01/99 : un peu de reecriture propre !
;;; La liste passee en argument est maintenant triee.

;;; L'AFFECTATION DES SALLES

;;; Calcul des salles pour chaque cours

(defun calculer-salles (liste)
  (gc)
  (setq Liste-Salle (sort 'plus-grande-capacite-p Liste-Salle))
  
  (let*
    ((long (length liste))
     (compteur 0)
     (seuil (max 1 (div long 20)))
     (liste-travail ())
     (cours)
     (salle))

    ;; Traitement des reservations et des ajouts de seances
    
    (maj-salles-reservees liste)

    ;; on retrie pour mettre en fin de liste les cours n'ayant
    ;; ni salle figee, ni salle souhaitee
    
    (setq liste (cours-modifies-fin-liste liste))
    
    ;; Traitement des cours ayant une salle figee puis une salle souhaitee

    (setq liste-travail liste)
    
    (while liste-travail
      (setq cours (nextl liste-travail))
      (when (salle-trouvee-cours cours)
	(incr compteur)
	(envoi-info compteur seuil long)
	(setq liste-travail (maj-salles-interdites cours liste-travail))))

    ;; on retrie pour mettre en fin de liste les cours n'ayant
    ;; ni salle figee, ni salle souhaitee
    
    (setq liste (cours-modifies-fin-liste liste))

    ;;  Traitement des autres cours

    (while liste
      (setq cours (nextl liste))
      (when (not (salle-trouvee-cours cours))
	(incr compteur)
	(envoi-info compteur seuil long)
	(setq salle (choix-salle cours))
	(when salle
	  (salle-trouvee-cours cours (code-salle salle))
	  (maj-salles-interdites cours liste))))))

;;; PROGRAMME PRINCIPAL

;;; MOD RM - 04/10/95 : ajout d'un pr‚dicat dans la lambda et cr‚ation d'une
;;; fonction pour l'affectation des salles pour les cours.
  

(defun moteur-salle ()  
  (cons-info 'affectation-salles 0 () ())
  (lire-fichier FICHIER-ECOLE 'ms)
  (lire-donnees-ms)
  (cons-info () 20 () ())
  (calculer-salles 
    (liste-salles-figees-dabord
      (sous-liste (lambda (cours) (ms-instant-trouve-p cours)) liste-cours)))
  (cons-info () 80 () ())
  (affecter-salles-aux-cours)
  (cons-info () 90 () ())
  (ecrire-resultat-ms)
  (liberer-memoire-ms-post))
