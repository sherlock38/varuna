;;; --------------------------------------------------------------------------
;;; - Temps.ll                                                  VARUNA 1.2   -
;;; - Utilitaires Generaux                                                   -
;;; - Representation interne du temps                                        -
;;; --------------------------------------------------------------------------
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; - Creation:  Mars 93                                                     -
;;; - Derniere mise a jour pour version 1.13::  Aout 94                      -
;;; - Build 1.22.1 01.07.99 / MO                                             -
;;; --------------------------------------------------------------------------



;;; rec (06/12/94) PR
;;; un bug dans.... moment-instant-cumule
;;; dbg (20/12/94) MO
;;; Kolossale Error.... bug med du 17/12/94 lie a cela

;;; chg (29/11/94) MO
;;; j'ai changé les operateurs sur les instant-interne: il n'y a plus que des operateurs
;;; sur entiers (add, mul, sub....)
;;; d'une facon generale, je n'utilise plus que des operateurs "types" (pas d'arithme
;;; tique generique) parce que cela posait des pb (ex: modulo !) et que ca trainait.
;;;  
;;; type des differentes conceptions du temps
;;;            
;;;                interne        cumule      naturel
;;;  horaire         fix           fix         string
;;;  moment          fix           fix         xxxxxx
;;;  instant         fix          float        xxxxxx
;;;
;;;  jour            fix
;;;  semaine         fix
;;;  



;;; REPRESENTATION INTERNE DU TEMPS: BARRIERES D'ABSTRACTION
;;; Semaine : numero
;;; Jour : numero (0 pour lundi, 1 pour mardi, ...7 pour dimanche)
;;; quelle que soit la semaine
;;; Heure : au sens 14h30, donc quels que soient jour et semaine
;;; Moment = jour X heure, quelle que soit la semaine
;;; Instant =  semaine X moment (i.e. instant t, comme en physique)
;;; Une date serait semaine X jour, mais ca ne sert pas


;;; I. Les Horaires


;;; La notion de temps au niveau du moteur prend plusieurs formes:
;;; concernant la facon de comptabiliser l'heure (qu'on appellera horaire,
;;; pour eviter des confusions), on retiendra:
;;;
;;;  - l'Horaire Naturel :      8h30, 12h40, 21h00
;;;    soit une chaine de caracteres dont le nombre avant le "h" represente la
;;;    "petite" aiguille d'une horloge (0 a 23) et la partie apres le "h"
;;;    la "grande aiguille" (0 a 59)
;;;
;;;  - l'Horaire Interne :     0, 28, 75 
;;;    si l'heure de debut des cours est 8h30, l'unite de temps 0h10 (Horaires
;;;    naturels) soit un entier qui represente la quantite de temps entre 
;;;    l'horaire present et l'horaire de debut des cours en unite de temps 
;;;    (variables globales)
;;;
;;;  - l'Horaire en Minutes Cumulees  : 510, 760, 1260
;;;    soit un entier representant l'horaire naturel totalise en minutes

;;; Variables globales servant dans ce module 

(defvar Unite-Temps)
(defvar Horaire-Cumule-Ouverture)

;;; Constantes Globales servant dans ce paragraphe

(defvar NB-MINUTES 60)
(defvar NB-HEURES 24)
(defvar NB-JOURS-SEMAINE 7)
(defvar CHAINE-ZERO "0")
(defvar CHAINE-VERS "2")
(defvar CHAINE-HEURE "h")
(defvar SYSTEME-NUMERIQUE 10)


;;; predicats de reconnaissance de type

(defun type-horaire-valide-p (type)
  (and 
    (member type '(cumule interne naturel))
    t))

(defun horaire-valide-p (horaire type)
  (and
    (selectq type
      ('cumule 
        (and 
          (ge horaire 0)
          (lt horaire (* NB-MINUTES NB-HEURES))))
      ('interne 
        (and 
          (ge horaire 0)
          (lt horaire 
            (div (sub (mul NB-MINUTES NB-HEURES) Horaire-Cumule-Ouverture)
              Unite-Temps))))

      ('naturel
        (and (index CHAINE-HEURE horaire)
          (let* ((hpos (index CHAINE-HEURE horaire))
            (heure (read-from-string (substring horaire 0 hpos)))
            (minute (read-from-string (substring horaire (1+ hpos)))))
              (and 
                (fixp heure)
                (fixp minute)
                (ge heure 0)
                (lt heure NB-HEURES)
                (ge minute 0)
                (lt minute NB-MINUTES))))))
    t))


;;; Accesseurs a l'Heure et Minutes a partir des formats d'Horaires

(defun acceder-horaire (accesseur horaire type)
  (selectq type
    ('cumule (selectq accesseur
      ('heure (div horaire NB-MINUTES))
      ('minute (rem horaire NB-MINUTES))
      (t ())))
    ('interne (acceder-horaire 
      accesseur
      (interne2cumule horaire) 
      'cumule))
    ('naturel (acceder-horaire
      accesseur
      (naturel2cumule horaire)
      'cumule))
    (t ())))


;;; Constructeurs d'horaires a partir de l'heure et minute

(defun construire-horaire (heure minute type)
  (if
    (eq type 'cumule)
    (add (mul NB-MINUTES heure) minute)
    (convertir-horaire
      (construire-horaire heure minute 'cumule)
      'cumule
      type)))


;;; Primitives de conversion entre format d'horaires et Primitive Globale:
;;;               <convertir-horaire>

(defun cumule2interne (horaire-cumule)
  (div (sub horaire-cumule Horaire-Cumule-Ouverture) Unite-Temps))

(defun interne2cumule (horaire-interne)
  (add (mul Unite-Temps horaire-interne) Horaire-Cumule-Ouverture)) 

(defun naturel2cumule (chaine)
  (let* 
    ((hpos (index CHAINE-HEURE chaine))
      (result chaine)
      (heure (read-from-string (substring result 0  hpos)))
      (minute (read-from-string (substring result (1+ hpos)))))
    (construire-horaire heure minute 'cumule)))

(defun cumule2naturel  (horaire-cumule)  
  (let
    ((horaire (acceder-horaire 'heure horaire-cumule 'cumule)) 
      (minute (acceder-horaire 'minute horaire-cumule 'cumule)))
    (if 
      (lt minute SYSTEME-NUMERIQUE)
      (catenate (string horaire) CHAINE-HEURE CHAINE-ZERO (string minute))
      (catenate (string horaire) CHAINE-HEURE (string minute)))))

(defun interne2naturel (horaire-interne)
  (cumule2naturel (convertir-horaire horaire-interne 'interne 'cumule)))

(defun naturel2interne (chaine)
  (convertir-horaire
    (naturel2cumule chaine)
    'cumule
    'interne))

(defun convertir-horaire (horaire type-source type-cible)
  (if
    (eq type-source type-cible)
    horaire
    (funcall (concat type-source CHAINE-VERS type-cible) horaire)))


;;; Operations sur les horaires 

;;; <horaire-plus-petit-p> & <horaire-plus-grand-p> permettent de comparer des
;;; horaires selon leur type

(defun horaire-compare-p (signe horaire1 horaire2 type1 . type2)
  (if
    (if (not type2)
      (if (eq type1 'naturel)
        (funcall 
          signe
          (naturel2cumule horaire1)
          (naturel2cumule horaire2))
        (funcall signe horaire1 horaire2))
      (horaire-compare-p
        signe
        horaire1 
        (convertir-horaire horaire2 (car type2) type1)
        type1 ))
    t
    ()))

(defun horaire-naturel-plus-petit-p (horaire1 horaire2)
  (horaire-compare-p 'lt horaire1 horaire2 'naturel))

(defun horaire-naturel-plus-grand-p (horaire1 horaire2)
  (horaire-compare-p 'gt horaire1 horaire2 'naturel))

(defun horaire-naturel-plus-petit-egal-p (horaire1 horaire2)
  (horaire-compare-p 'le horaire1 horaire2 'naturel))

(defun horaire-naturel-plus-grand-egal-p (horaire1 horaire2)
  (horaire-compare-p 'ge horaire1 horaire2 'naturel))


;;; chg (29/11/94) MO
;;; je retire cette primitive qui ne sert plus a rien, me semble-t-il
;;; !! a oter un jour

;;; <longueur> nous donne la longueur d'un cours donne en format cumule 
;;; converti en format interne (rem: on pourrait redefinir une conversion
;;; pour les durees) 
;(defun longueur (duree) (div duree Unite-Temps))



;;; II. Les Moments
;;;


;;; On retiendra deux moments: 
;;;
;;;           - moment interne: horaire-interne x jour (fix)
;;;           - moment cumule: horaire-cumule x jour   (fix)
;;;
;;; rem: on ne manipule que des moments cumules dans l'interface VARUNA
;;; et pour les dispo en general. La notion de temps interne ne sert que dans
;;; le moteur. En effet, on ne peut se passer de la representation en interne
;;; a cause de la limitation des integer a 32000. (cruciale pour les instants)



;;; variables globales a definir pour utiliser les notions de moments et
;;; d'instants

(defvar Nb-Unites 0)  



;;; Construction d'un moment a partir d'un jour et d'un horaire

;;; Rem (02/05/94) MO: 
;;; ATTENTION, le numero du jour envoye en parametre est le rang de ce jour 
;;; dans la liste des jours ouvrables et non le LIBELLE-JOUR
;;; chg (29/11/94) MO:
;;; la remarque ci-dessus ne vaut plus, nb-unite etant calcule correctement
;;; maintenant (cf calculer-donnees-ecole dans ecole.ll)

(defun cons-moment (nj nh) (add nh (mul Nb-Unites nj)))

(defun cons-moment-cumule (nj nhc) 
  (add nhc (mul nj (mul NB-HEURES NB-MINUTES))))



;;; Accesseurs au jour et a l'heure a partir d'un moment

(defun jour-moment (moment) (div moment Nb-Unites))
(defun horaire-moment (moment) (rem moment Nb-Unites))


(defun jour-moment-cumule (moment-cumule) 
  (div moment-cumule (* NB-HEURES NB-MINUTES)))
(defun horaire-moment-cumule (moment-cumule)
  (rem moment-cumule (* NB-HEURES NB-MINUTES)))



;;; III. Les instants 
;;;

;;; Variables globales utilisees dans ce paragraphe

(defvar Nb-Moments)
(defvar Nb-Semaines)
(defvar Nb-Jours)


;;; Construction d'un instant a partir d'un moment et d'une semaine

;;; instant interne (attention, c'est un fix)
;;; intant cumule   (attention, c'est un mixte)


;;; on travaille sur des entiers
(defun cons-instant (sem moment) (add moment (mul sem Nb-Moments)))

;;; on travaille sur des mixtes
(defun cons-instant-cumule (sem mc) (plus mc (times sem 7 60 24))) 


(defun semaine-instant (instant) (div instant Nb-Moments))
(defun moment-instant (instant) (rem instant Nb-Moments))
(defun jour-instant (x) (jour-moment (moment-instant x)))
(defun horaire-instant (x) (horaire-moment (moment-instant x)))

(defun semaine-instant-cumule (ic) 
  (truncate (divide ic (times NB-MINUTES NB-JOURS-SEMAINE NB-HEURES))))

(defun moment-instant-cumule (ic) 
  (truncate 
    (differ 
       ic 
       (times (semaine-instant-cumule ic) NB-MINUTES NB-JOURS-SEMAINE NB-HEURES))))


(defun jour-instant-cumule (ic) (jour-moment-cumule (moment-instant-cumule ic)))

(defun horaire-instant-cumule (ic) (horaire-moment-cumule (moment-instant-cumule ic)))


;;; dbg & rem (07/11/94) MO !!!!!
;;; attention, la presence d'un cumule peut etre cruciale
;;; tout est a verifier a la vrigule
;;; il faudrait remetter des interne partout
;;; pour le moment - question de compatibilite - ca n'a pas ete fait

(defun instant-interne2instant-cumule (i) 
  (cons-instant-cumule 
    (semaine-instant i) 
    (cons-moment-cumule (jour-instant i) (interne2cumule (horaire-instant i)))))

(defun instant-cumule2instant-interne (ic)
  (cons-instant
    (semaine-instant-cumule ic)
    (cons-moment (jour-instant-cumule ic) 
      (cumule2interne (horaire-instant-cumule ic)))))


;;; conversion d'un moment cumule en un moment interne

(defun moment-cumule2moment-interne (mc)
  (let ((jour (jour-moment-cumule mc))
    (horaire (horaire-moment-cumule mc)))
  (cons-moment jour (cumule2interne horaire))))


;;; FD


;;; transforme un intervalle en horaires cumules en intervalle
;;; en horaires naturels
;;;
(defun intervalle-cumule2intervalle-naturel (intervalle)
  (catenate
      (cumule2naturel (horaire-moment-cumule
                          (borne-inf-intervalle intervalle)))
      "-"
      (cumule2naturel (horaire-moment-cumule
                          (borne-sup-intervalle intervalle)))))


;;; transforme une liste d'intervalles en horaires cumules en 
;;; une chaine d'intervalles en horaires natuels
(defun intervalles-cumules2intervalles-naturels (intervalles)
  (let ((resultat ""))
     (mapc (lambda (interv)
             (let (
                   (interv-naturel 
                       (intervalle-cumule2intervalle-naturel interv)))
               (setq resultat
                 (catenate 
                   resultat 
                   ESP
                   interv-naturel))))
        intervalles)
    resultat))

;;; transforme une liste d'horaires cumules en une chaine d'horaires
;;; naturels
(defun horaires-cumules2horaires-naturels (liste)
  (let ((resultat ""))
    (mapc (lambda (h)
            (setq resultat
                  (catenate 
                      resultat 
                      ESP
                      (cumule2naturel (horaire-moment-cumule h)))))
        liste)
    resultat))

;;; PREDICATS lies aux jours et aux semaines

;;; <meme-jour-p>
;;; indique si on le meme jour
(defun meme-jour-p (jour1 jour2)
  (= jour1 jour2))

;;; <precede-semaine-p>
;;; indique si une semaine en precede une autre ou est egale 

(defun precede-semaine-p (semaine1 semaine2)
  (or
    (= semaine1 semaine2)
    (precede-semaine-strictement-p  semaine1 semaine2)))

;;; <precede-semaine-strictement-p>
;;; indique si une semaine en precede une autre (et n'est pas egale a celle-ci)

(defun precede-semaine-strictement-p (semaine1 semaine2)
  (< semaine1 semaine2))

;;; <semaine-interne2semaine-calendaire>
;;; CRE - FD - 26/07/96
;;; ATTENTION: si nous ne trouvons pas de correspondance entre semaine 
;;; calendaire et semaine interne nous 'prolongeons' le dernier intervalle
;;; ex. on a ((30 . 35) (40 . 42)) (intervalles ouverts a droite)
;;; la semaine 1 correspond a la semaine 30 en interne
;;;            7                         41
;;; et (semaine-calendaire2semaine-interne 8 retournera 42


(defun semaine-interne2semaine-calendaire (semaine-interne)
  (let
     ((lst-tmp (periodes-calendaires-i-ecole I-ECOLE))
      (periode)
      (semaine-debut 1)
      (semaine-fin))
     (nextl lst-tmp periode)
     (setq semaine-fin
       (+ semaine-debut
	 (- (borne-sup-intervalle periode) (borne-inf-intervalle periode))))
     (until
       (or
	 (null lst-tmp)
	 (appartenir-intervalle-p
	   semaine-interne
	   (creer-intervalle semaine-debut semaine-fin)))
       (nextl lst-tmp periode)
       (when (not (null periode))
	 (setq semaine-debut semaine-fin)
	 (setq semaine-fin
	   (+ semaine-debut
	     (- (borne-sup-intervalle periode)
	       (borne-inf-intervalle periode))))))
     (+
       (borne-inf-intervalle periode)
       (- semaine-interne semaine-debut))))



;;; <semaine-calendaire2semaine-interne>
;;; CRE - FD - 26/07/96
;;; ATTENTION: si nous ne trouvons pas de correspondance entre semaine 
;;; calendaire et semaine interne nous 'prolongeons' le dernier intervalle
;;; ex. on a ((30 . 35) (40 . 42)) (intervalles ouverts a droite)
;;; la semaine 30 correspond a la semaine 1 en interne
;;;            41                         7
;;; et (semaine-calendaire2semaine-interne 42) retournera 8

(defun semaine-calendaire2semaine-interne (semaine-calendaire)
  (let*
    ((lst-tmp (periodes-calendaires-i-ecole I-ECOLE))
     (periode (nextl lst-tmp))
     (semaine-interne-debut 1))
  (until
    (or
      (null lst-tmp)
      (appartenir-intervalle-p semaine-calendaire periode))
      (setq semaine-interne-debut
	(+ semaine-interne-debut
	  (- (borne-sup-intervalle periode)(borne-inf-intervalle periode))))
      (nextl lst-tmp periode))
  (+ semaine-interne-debut
    (- semaine-calendaire (borne-inf-intervalle periode)))))

;;; <semaine-calendaire-p>
;;; CRE - FD 26/07/96
;;; predicat verifiant que nous avons bien une semaine calendaire valide

(defun semaine-calendaire-p (semaine-calendaire)
  (let*
    ((lst-tmp (periodes-calendaires-i-ecole I-ECOLE))
     (periode (nextl lst-tmp)))
  (until
    (or
      (null periode)
      (appartenir-intervalle-p semaine-calendaire periode))
    (nextl lst-tmp periode))
  (not (null periode))))

;;; <ecrire-numero-semaine> - CRE - FD - 26/07/96
;;; retourne le numero de semaine en calendaire si nous sommes en calendaire

(defun ecrire-numero-semaine (semaine-interne)
  (if
    (calendaire-i-ecole I-ECOLE)
    (semaine-interne2semaine-calendaire semaine-interne)
    semaine-interne))

;;; <ecire-liste-numeros-semaine> - CRE - FD - 29/07/96
;;; retourne une liste de semaines en calendaire si necessaire

(defun ecrire-liste-numeros-semaine (liste-semaines-internes)
  (if (calendaire-i-ecole I-ECOLE)
    (mapcar
      'ecrire-numero-semaine
      liste-semaines-internes)
    liste-semaines-internes))

;;; <lire-numero-semaine> - CRE - FD - 26/07/96
;;; lit le numero de semaine pour le convertir en format interne si besoin

(defun lire-numero-semaine (semaine)
  (if
    (calendaire-i-ecole I-ECOLE)
    (semaine-calendaire2semaine-interne semaine)
    semaine))

;;; <libelle-intervalle-semaines2intervalle-semaines> - CRE - FD - 30/07/96
;;;

(defun libelle-intervalle-semaines2intervalle-semaines (libelle)
  (let*
    ((tiret (index "-" libelle))
     (libelle1 (substring libelle 0 tiret))
     (libelle2
       (substring libelle (add1 tiret) (sub (slength libelle) (add1 tiret)))))
    (when
      (and
	(not (nullstrp libelle1))
	(not (nullstrp libelle2)))
      (creer-intervalle
	(string2number libelle1)
	(add1 (string2number libelle2))))))


;;; <intervalle-semaine2libelle> - CRE - FD - 30/07/96
;;; renvoie le libelle d'un intervalle de semaines

(defun intervalle-semaines2libelle (intervalle)
  (when intervalle
    (catenate
      (borne-inf-intervalle intervalle)
      " - "
      (sub1 (borne-sup-intervalle intervalle)))))


;;; CRE RM - 15/01/97

(defun intersection-periodes-calendaires (intervalle)
  (let
    ((interv-ecole (periodes-calendaires-i-ecole I-ECOLE))
     (borne-inf (borne-inf-intervalle intervalle))
     (borne-sup (borne-sup-intervalle intervalle))
     (interv)
     (result))
    (if (> borne-sup borne-inf)
      (setq interv intervalle)
      (setq interv (list (cons borne-inf 53) (cons 1 borne-sup ))))
    (if (eq (length interv) 1)
      (setq result
	(mapcar
	  (lambda (int)
	    (intersection-intervalle interv int))
	  interv-ecole))
      (setq result
	(append
	  (mapcar
	    (lambda (int)
	      (intersection-intervalle (car interv) int))
	    interv-ecole)
	  (mapcar
	    (lambda (int)
	      (intersection-intervalle (cadr interv) int))
	    interv-ecole))))
    (supprimer-occurences-vides result)))
	  
      
