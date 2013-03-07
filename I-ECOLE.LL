;;; --------------------------------------------------------------------------
;;; - I-Ecole.ll :                                              VARUNA 1.2   -
;;; - Utilitaires de gestion des parametres de l'ecole                       -
;;; -                                                                        -
;;; --------------------------------------------------------------------------
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; - Creation:  Septembre 94                                                -
;;; - Mise a jour pour version 1.10: Octobre 94                              -
;;; - Build 1.22.1 01.07.99 / JH MO                                          -
;;; --------------------------------------------------------------------------


;;; creation d'une instance de la classe i-ecole
;;; avec une serie de valeurs par defaut
;;; Met a jour les variables globales I-ECOLE et Liste-I-Ecole
;;; cette fonction fait office de fonction de creation
;;; de toute instance de la classe. Elle contient toutes
;;; les valeurs par defaut. Ce qui peut eviter les ennuis
;;; lies a () dans l'un des champs par rapport a l'interface

;;; attention, si les horaires de debut par types sont modifies
;;; (manuellement dans ce fichier)
;;; il faut aussi modifier les jours-ouvres

(defun ecole-defaut ()
  (setq compteur-i-ecole 0)		; on reinitialise les compteurs
  (setq compteur-i-type-cours 0)
  (let*
    ((ecole (nouveau-i-ecole))
     (code1 (gencode-i-type-cours))
     (type-cours1 (creer-i-type-cours code1 "NORMAL")))

    (libelle-ecole-i-ecole ecole "ECOLE PAR DEFAUT")
    (libelle-i-ecole ecole "ECOLE PAR DEFAUT")
    (grain-dispo-i-ecole ecole 60)
    (liste-moments-cumules-i-type-cours type-cours1
      (list 480 540 600 660 720 780 840 900 960 1020 1080
	1140 1200 1260 1920 1980 2040 2100 2160 2220 2280 2340 2400 2460
	2520 2580 2640 2700 3360 3420 3480 3540 3600 3660 3720 3780 3840
	3900 3960 4020 4080 4140 4800 4860 4920 4980 5040 5100 5160 5220
	5280 5340 5400 5460 5520 5580 6240 6300 6360 6420 6480 6540 6600
	6660 6720 6780 6840 6900 6960 7020 7680 7740 7800 7860 7920))
    (dispo-ecole-i-ecole ecole
      '(((480 . 1320) (1920 . 2760) (3360 . 4200)
	(4800 . 5640) (6240 . 7080) (7680 . 7980)) (1 . 31)))
    (types-cours-i-ecole ecole (list type-cours1))
    (liste-moments-debut-ecole-i-ecole ecole
      (list (liste-moments-cumules-i-type-cours type-cours1)))
    (semaines-debut-ecole-i-ecole ecole
      '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18
	19 20 21 22 23 24 25 26 27 28 29 30))
    (unite-temps-i-ecole ecole 10)
    (param-chevauchement-i-ecole ecole 1)
    (param-chevauchement-max-i-ecole ecole 3)
    (horaire-cumule-pause-i-ecole ecole ())
    (semaine-debut-i-ecole ecole (car (semaines-debut-ecole-i-ecole ecole)))
    (semaine-fin-i-ecole ecole (car
      (last (semaines-debut-ecole-i-ecole ecole))))
    (dispo-i-ecole ecole
      (trouver-dispo-moment (dispo-ecole-i-ecole ecole)))
    (liste-codes-types-cours-i-ecole ecole (list code1 ))
    (liste-libelles-types-cours-i-ecole ecole (list "NORMAL"))
    (jours-ouvres-i-ecole ecole '(0 1 2 3 4 5))
    (index-i-ecole ecole ())
    (pause-i-ecole ecole 0)
    (setq I-ECOLE ecole)
    (setq Liste-I-Ecole (list I-ECOLE))
    (setq Liste-I-Type-Cours (list type-cours1))
    ecole))


;;; modification des parametres de l'ecole
;;; cette fonction evalue les champs calcules
;;; d'un objet i-ecole
;;; ecrit les fichier ecole.edt
;;; appelle preinit pour reinitialiser certaines variables du temps

;;; champs : <dispo-ecole>
;;;          <liste-moments-debut-ecole>
;;;          <liste-codes-types-cours>
;;;          <liste-libelles-types-cours>
;;;          <libelle>
;;;          <jours-ouvres>

(defun calculer-champs-i-ecole ()
;;; le libelle
  (libelle-ecole-i-ecole I-ECOLE (libelle-i-ecole I-ECOLE))
;;; les semaines
  (semaines-debut-ecole-i-ecole i-ecole 
    (liste-nombres-inclus
      1 (semaine-fin-i-ecole I-ECOLE) 1 1))
  
;;; la dispo
  (dispo-ecole-i-ecole I-ECOLE
    (cons-dispo
      (dispo-i-ecole I-ECOLE)
      (creer-zone
	(creer-intervalle
	  1
	  (add1 (semaine-fin-i-ecole I-ECOLE))))))
;;; FD 21/06/95	  (semaine-fin-i-ecole I-ECOLE)))))

;;; les moments-debut par type de cours
  (liste-moments-debut-ecole-i-ecole I-ECOLE 
    (mapcar 'liste-moments-cumules-i-type-cours 
      (types-cours-i-ecole I-ECOLE)))
  (liste-codes-types-cours-i-ecole I-ECOLE
    (mapcar 'code-i-type-cours
      (types-cours-i-ecole I-ECOLE)))
  (liste-libelles-types-cours-i-ecole I-ECOLE
    (mapcar 'libelle-i-type-cours
      (types-cours-i-ecole I-ECOLE)))

;;; les jours-ouvres
  
  (ecrire-fichier FICHIER-ECOLE 'interface 'ecole ())
  (lire-fichier FICHIER-ECOLE 'inter)
  (initialiser-affichage))


;;; construit le i-ecole
;;; a partir d'un fichier ecole.edt


(dmd lire-ecole-interface (enr index)
  `(let ,(cdr enr)
    (when libelle-ecole
      (let
	((ecole (nouveau-i-ecole)))

	(libelle-ecole-i-ecole ecole libelle-ecole)
	(libelle-i-ecole ecole libelle-ecole)
	(unite-temps-i-ecole ecole unite-temps)
	(param-chevauchement-i-ecole ecole param-chevauchement)
	(param-chevauchement-max-i-ecole ecole param-chevauchement-max)
	(horaire-cumule-pause-i-ecole ecole horaire-cumule-pause)
	(semaine-debut-i-ecole ecole (premier-element semaines-debut-ecole))
	(semaine-fin-i-ecole ecole (dernier-element semaines-debut-ecole))
	(semaines-debut-ecole-i-ecole ecole semaines-debut-ecole)
	(liste-moments-debut-ecole-i-ecole ecole liste-moments-debut-ecole)
	(dispo-ecole-i-ecole ecole dispo-ecole)
	(dispo-i-ecole ecole (trouver-dispo-moment dispo-ecole))
	(grain-dispo-i-ecole ecole grain-dispo)
	(index-i-ecole ecole index)
	(types-cours-i-ecole ecole
	  (mapcar
	    (lambda (code libelle liste-mom)
	      (let ((tc (creer-i-type-cours code libelle)))
		(liste-moments-cumules-i-type-cours tc 
		  liste-mom)
		tc))
	    liste-codes-types-cours
	    liste-libelles-types-cours
	    liste-moments-debut-ecole))
	;; AJT - FD - 26/07/96 - champs suplementaires pour semaines
	;;  calendaires
	(calendaire-i-ecole ecole calendaire)
	(periodes-calendaires-i-ecole
	  ecole
	  (or periodes-calendaires
	    (list
	      (cons
		(semaine-debut-i-ecole ecole)
		(add1 (dernier-element semaines-debut-ecole))))))
	(if pause
	  (pause-i-ecole ecole pause)
	  (pause-i-ecole ecole 0))
	(mapc
	  (lambda (tc) (newl Liste-I-Type-Cours tc)) 
	  (types-cours-i-ecole ecole))
	(liste-codes-types-cours-i-ecole ecole liste-codes-types-cours)
	(liste-libelles-types-cours-i-ecole ecole liste-libelles-types-cours)
	(setq Liste-I-Ecole (list ecole))
	(setq I-ECOLE ecole)))))


    
    


