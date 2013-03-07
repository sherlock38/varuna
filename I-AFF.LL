;;; --------------------------------------------------------------------------
;;; - i-aff.ll                                                  VARUNA 1.2   -
;;; - Interface graphique VARUNA/AIDA                                        -
;;; - Module des actions associees aux Menus: ici, menu <afficher>           -
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; --------------------------------------------------------------------------
;;; - Creation: JH                                                           -
;;; - Derniere mise a jour pour version 1.13: 23 Mai 1995 - MO               -
;;; - Build 1.22.1 01.07.99 / JH MO                                          -
;;; --------------------------------------------------------------------------

;;; DBG (23/05/95) MO
;;; j'ai change aff-groups en afficher-familles pour permettre un affichage
;;; automatique lorsqu'il n'y a qu'une seule famille, un affichage propre
;;; de la demande des sous-familles (cf <afficher-familles>)

;;; FD
;;; -    globale Liste-Resas-Aff, fonction aff-res                           -

;;; RM
;;; -    globale Liste-Ajouts-Aff, fonction aff-ajt                           -


;;; le menu afficher de la fenetre de resolution comporte 8 options, qui sont
;;; associees aux 
;;; primitives suivantes
;;;              -Groupe         2      <afficher-familles>
;;;              -Salle          2      <afficher-salles>
;;;              -Hebdo          2      <aff-week>
;;;              -Planning       2      <aff-pla>
;;;              -Partiel        2      <aff-partiel>
;;;              -Complet        2      <aff-complet>
;;;              -Reafficher     2      <aff-refresh>
;;;              -Effacer        2      <aff-clear>
;;;
;;; On trouvera aussi dans ce sous-module les primitives suivantes:
;;;              <show-edt>
;;;              <aff-cours-but>
;;;              <aff-change>
;;; Le traitement de l'affichage se fait au moyen d'un certain nombre de varia
;;; bles globales. Selon qu'elles sont () ou qu'elles soient des listes, l'af
;;; fichage est aiguille vers annuel/hebdo partiel/complet familles/salles.
;;; ces variables sont: Liste-Salles-Aff Liste-Groupes-Aff Week-Aff 
;;;
;;;  globs: grid-type, week-aff, liste-salles-aff, liste-groupes-aff



;;; I. Actions associees au menu Afficher:
;;; -------------------------------------- 



;;;  1.1 primitives de premier niveau
;;; 

;;; redessine tous les cours affichables (action de l'option Reafficher)

(defun aff-refresh (a)
  (show-edt a))


;;; changement de type d'affichage planing2semaine
;;; la semaine est demandee explicitement dans cette fonction
;;; MOD RM - 20/12/95 : Utilisation de la boite d'options
;;; MOD RM - 07/02/96 : On enleve la boite

(defun aff-week (a)
  (let 
    ((week
      (demande-semaine #Mv_pro-sem Semaine-Debut Semaine-Fin)))
    (when week
      (setq Grid-Type 'week)
      (setq Week-Aff week)
      (update-grid-titles)
      (show-edt a))))

;;; changement du type d'affichage hebdo -> annuel
;;; en fait se contente d'appeler aff-change avec Week-Aff=()
;;; MOD RM - 20/12/95 : Utilisation de la boite d'options
;;; MOD RM - 07/02/96 : On enleve la boite

(defun aff-pla (a)
  (when (not (eq Grid-Type 'year))
    (setq Week-Aff ())
    (aff-change ())
    (update-grid-titles)
    (aff-refresh a)))

;;; CRE RM - 07/02/96 : Affichage avec chevauchement ou non chevauchement

(defun aff-sans-chevauchement (a)
  (setq Aff-Avec-Chevauchement ())
  (show-edt a))

(defun aff-avec-chevauchement (a)
  (setq Aff-Avec-Chevauchement t)
  (show-edt a))


;;; <aff-groups> -> <afficher-famille> (s) ???
;;; 
;;; selon les familles de liste-i-groupes, aff-groups:
;;;     1. pas de familles -> qu'est-ce-qu'on affiche ???
;;;     2. on teste si il n'y a qu'une seule famille
;;;     3. sinon il y a au moins deux familles et donc du choix
;;;        on demande donc le choix de l'utilisateur
;;;          3.a.  familles avec sous-familles affichage ??? oui/non
;;;          3.b.  familles sans sous-familles -> affichage multi ou non
;;;  ->-> <show-edt> avec globales a jour

;;; Min (12/05/95) MO
;;; quand il n'ya qu'une seule famille, on devrait automatiquement passer a
;;; l'affichage de celle-ci 

;;;  Min (12/05/95) MO
;;;  quand il n'y a qu'une seule famille et que l'on demande de nouveau
;;;  l'affichage famille, on devrait mettre un message disant qu'il n'y a
;;;  qu'un seule famille

;;;  Med (22/05/95) MO
;;;  quand on demande deux familles dont une a des sous-familles et pas l'autre
;;;  il ne demande pas si on veut les sous-familles ???
;;;  val (22/05/95) MO, FD
;;;  cela vient du <every>

;;;  <afficher-familles> renvoie t si au moins une famille est selectionnee

(defun afficher-familles (a)  
  (when
    ;;  le cond renvoie t si au moins une famille est selectionnee
    (cond
      ;;  1. pas de familles dans le jeu de donnees
      ((null Liste-I-Groupe) (afficher-attention #Mv_no-groups)())
      ;;  2. une seule famille, on l'affiche avec un message
      ((mono-element-p Liste-i-Groupe)
        (afficher-information #Mv_famille-unique)
        (maj-liste-groupes-aff Liste-I-Groupe ())
        t) 
      ;;  3. plusieurs familles donc un choix -> familles-demandees 
      (t                                                           
        (let 
          ((familles-demandees (demandes-i-groupe #Mv_pro-groups () ())))
          (cond
            ;; 3.a.  pas de familles selectionnees
            ((null familles-demandees) ())      ; -> que fait-on ?
            ;; 3.b.  des familles avec des sous-familles  
            ((any 'groupes-i-groupe familles-demandees)
              (maj-liste-groupes-aff
                familles-demandees
                (prompt-oui-non
                  (column
                    #Mv_affichage-sous-familles1
                    #Mv_affichage-sous-familles2)))
              t)
            ;;  3.c  aucune familles avec sous-familles
            (t
              ;; rem (22/05/95) mo: on purrait inserer un message disant cela
              (maj-liste-groupes-aff
                familles-demandees
                ())
              t)))))
    (update-grid-titles)
    (show-edt a)
    t))


;;; CRE (26/05/95) MO !!!
;;; j'ai renomme un bout de aff-groups en cette primitive et cela
;;; sans grande conviction. Il faut ABSOLUMENBT revoir ce bout de code
;;;  c'est du N'IMPORTE QUOI, a mon avis (mapc 'mapc 'newl ????)
;;; MOD FD - RM 19/09/96 : reecriture propre !!! 

(defun maj-liste-groupes-aff (liste-familles-a-afficher flag-sous-famille)
  (let
    ((first (car (last liste-familles-a-afficher))))
    (setq Liste-Salles-Aff ())
    (setq Liste-Profs-Aff ())
    (setq Liste-Groupes-Aff liste-familles-a-afficher)
    (when flag-sous-famille
      (mapc
	(lambda (groupe)
	  (setq Liste-Groupes-Aff
	    (append (tous-les-sous-groupes groupe) Liste-Groupes-Aff)))
	liste-familles-a-afficher))
    (setq Liste-Groupes-Aff
      (cons
	(car Liste-Groupes-Aff)
	(sous-liste cours-i-groupe (cdr Liste-Groupes-Aff))))
    (setq Liste-Groupes-Aff
      (supprimer-occurences-multiples Liste-Groupes-Aff))
    (setq Liste-Groupes-Aff (delq first Liste-Groupes-Aff))
    (newl Liste-Groupes-Aff first)
    (trier-groupes-aff)))

(defun aff-groups (a)
  (ifn Liste-I-Groupe
    (afficher-attention #Mv_no-groups)
    (let 
      ((groupes (demandes-i-groupe #Mv_pro-groups () ())))
      (when groupes
        (setq Liste-Salles-Aff ())
        (setq Liste-Profs-Aff ())
        (setq Liste-Groupes-Aff ())
        (if (not (every 'groupes-i-groupe groupes))
          (setq Liste-Groupes-Aff groupes)
          (ifn 
            (prompt-oui-non "Voulez-vous afficher les sous-familles ?")
            (setq Liste-Groupes-Aff groupes)
            (mapc (lambda (g)
              (newl Liste-Groupes-Aff g)
              (mapc 
                (lambda (subg)
                  (newl Liste-Groupes-Aff subg))
                (tous-les-sous-groupes g)))
            groupes)
            (setq Liste-Groupes-Aff
              (supprimer-occurences-multiples Liste-Groupes-Aff))))
        (trier-groupes-aff)
        (update-grid-titles)
        (show-edt a)))))


;;; demande les salles a afficher, met a jour les globales d'affichage,
;;; et appelle show-edt
;;; FD-02/05/95
;;; ajout d'un afficher-warning lorsqu'il n'y a pas de salles
;;; DBG (24/05/95) MO
;;; prise en compte de l'absence de salle, d'une unique salle et ajout d'une
;;; primitive de mise a jour dans laquelle on traite les variables globales


(defun afficher-salles (a)
  (when

    ;; le cond renvoie t si au moins une salle est selectionnee
    (cond
      ((null Liste-I-Salle) (afficher-attention #Mv_aucune-salle)())
      ((mono-element-p Liste-I-Salle)
        (afficher-information #Mv_salle-unique)
        (maj-liste-salles-aff Liste-I-Salle)
        t)
      (t
        (let
          ;; pour le moment , on demande une seule salle
          ;; mais maj-liste-salles-aff supporte une liste de salles
          ((salle-demandee (demande-i-salle #Mv_pro-salle ()))) 
          (ifn
            salle-demandee
            ()                                  ;  que fait-on dans ce cas ???
            (maj-liste-salles-aff (list salle-demandee))
            t))))
    (update-grid-titles)
    (show-edt a)
    t))
  
;;; CRE (24/05/95) MO
;;; primitive de mise a jour des globales d'affichage
;;; but du jeu: toutes ces mises a jour se font au meme endroit

(defun maj-liste-salles-aff (salle-a-afficher)
  (setq Liste-Groupes-Aff ())
  (setq Liste-Cours-Aff ())
  (setq Liste-Profs-Aff ())
  (setq Liste-Salles-Aff salle-a-afficher))

;;; CRE RM - 04/12/95 : Affichage des plannings des profs
;;; demande le prof a afficher, met a jour les globales d'affichage,
;;; et appelle show-edt

(defun afficher-profs (a)
  (when
    ;; le cond renvoie t si au moins une salle est selectionnee
    (cond
      ((null Liste-I-Prof) (afficher-attention #Mv_aucun-prof)())
      ((mono-element-p Liste-I-Prof)
        (afficher-information #Mv_prof-unique)
        (maj-liste-profs-aff Liste-I-Prof)
        t)
      (t
        (let
          ;; pour le moment , on demande un seul prof
          ;; mais maj-liste-profs-aff supporte une liste de profs
          ((prof-demande (demande-i-prof #Mv_pro-prof ()))) 
          (ifn
            prof-demande
            ()                                  ;  que fait-on dans ce cas ???
            (maj-liste-profs-aff (list prof-demande))
            t))))
    (update-grid-titles)
    (show-edt a)
    t))
  
;;; CRE RM - 04/12/95
;;; primitive de mise a jour des globales d'affichage
;;; but du jeu: toutes ces mises a jour se font au meme endroit

(defun maj-liste-profs-aff (prof-a-afficher)
  (setq Liste-Groupes-Aff ())
  (setq Liste-Cours-Aff ())  
  (setq Liste-Salles-Aff ())
  (setq Liste-Profs-Aff prof-a-afficher))


;;; FD-24/04/95
;;; demande les reservations a afficher
;;; CHG (02/05/95) MO
;;; je change la specif: si on demande une resa, on affiche la salle de la
;;; resa comme si c'etait un affichage salle

;;;  CHG (23/05/95) MO
;;;  ajout de conditions pour supporter une unique resa et ajout de maj-liste..
;;;  pour plus de proprete. Cette primtive recoit comme argument une liste,
;;;  en effet, du moment que l'on aura l'affichage multi en salle, on pourra
;;;  avoir l'affichage multi en resa...

;;; MOD RM - 15/01/97 : on se place dans la bonne semaine

(defun aff-res (a)
  (let
    ((resa ()))
    (cond
      ((null Liste-I-Resa) (afficher-attention #Mv_aucune-resa))
      ((mono-element-p Liste-I-Resa)
        (afficher-information #Mv_resa-unique)
        (setq resa (car Liste-I-Resa))
        (maj-liste-resas-aff Liste-I-Resa))
      (t
        (setq resa (demande-i-resa #Mv_pro-resa1 ()))
        (when resa
          (maj-liste-resas-aff (list resa)))))
    (when resa
      (setq Grid-Type 'week)
      (setq Week-Aff
        (semaine-instant (instant-i-resa resa)))
      (update-grid-titles)
      (show-edt a))))

;;; <maj-liste-resas-aff>
;;; met a jour les variables globales d'affichage

(defun maj-liste-resas-aff (liste-resa-a-afficher)
  (setq Liste-Groupes-Aff ())
  (setq Liste-Profs-Aff ())
  (setq Liste-Salles-Aff 
        (mapcar 'salle-i-resa liste-resa-a-afficher)))


;;; on efface le contenu de la grille (en fait on 
;;; reconstruit une nouvelle grille vierge)

(defun aff-clear (a)
  (add-grid (component 'grid a)))

;;; CRE - FD - 14/09/95
;;; <maj-affichage>
;;; arg:
;;;     - <type> d'affichage (groupe, salle, resa)
;;;     - <liste> de ce qu'on desire afficher (liste de familles, liste de 
;;;       salles liste de resas)
;;;     - <semaine> que l'on desire afficher si on est en affichage semaine
;;;       (rem: si semaine vaut () on ne modifie pas la semaine

(defun maj-affichage (type liste-objets semaine)
  (when
    (and Week-Aff semaine)
    (setq Week-Aff semaine))
  (selectq type
    ('groupe
      (maj-liste-groupes-aff liste-objets ()))
    ('salle
      (maj-liste-salles-aff liste-objets))
    ('resa
      (maj-liste-resas-aff liste-objets)))
  (update-grid-titles)
  (show-edt Gen-App))
    
    

;;;  1.2 Primitives permettant l'affichage multi-familles
;;;

;;; 
;;; verifie que gr1 est avant gr2 dans Liste-Groupes-Aff

(defun groupe-avant (gr1 gr2)
  (let ((n1 (numero-dans-liste gr1 Liste-Groupes-Aff))
    (n2 (numero-dans-liste gr2 Liste-Groupes-Aff)))
  (when (and n1 n2)
    (lt n1 n2))))


;;; tri les groupes des cours dans le meme ordre que Liste-Groupes-Aff
;;; permet de rendre l'affichage multi-groupe independant
;;; de l'ordre des groupes dans Liste-Groupe-Aff

(defun trier-groupes-aff ()
  (mapc
    (lambda (i-cours)
      (groupes-i-cours i-cours
        (sort 'groupe-avant 
          (groupes-i-cours i-cours))))
    Liste-I-Cours))


;;;  1.3 Impression graphique
;;;


;;; impression de l'appli impression-edt
;;; avec quelques fioritures

;;; <liste-libelles-groupes-aff>
;;; CHG (24/05/95) MO
;;; j'ai rajoute la virgule-espace apres le premier groupe

(defun liste-libelles-groupes-aff ()
  (let
    ((str ""))
    (mapc
      (lambda (g)
        (setq
          str
          (catenate str (when (nequal str "") ", ") (libelle-i-groupe g))))
      Liste-Groupes-Aff)
    str))


;;; <liste-libelles-salles-aff>
;;; CHG (24/05/95) MO
;;; j'ai rajoute la virgule-espace apres le premier groupe

(defun liste-libelles-salles-aff ()
  (let
    ((str ""))
    (mapc
      (lambda (s)
      (setq
        str
        (catenate str (when (nequal str "") ", ") (libelle-i-salle s))))
      Liste-Salles-Aff)
    str))


;;; chg (06/04/95) MO
;;; ai change la ligne de commande pour ne prendre en compte que le premier
;;; groupe

;;; !! il faudrait une bonne fois pour toute rendre propre le code sur les 
;;; titres
;;; MOD RM - 05/12/95 : Affichage des professeurs
;;; MOD RM - 20/08/96 : affichage de la date calendaire

(defun titre-impression ()
  (let
    ((week-string (creer-chaine-dates Week-Aff)))
    (catenate LIBELLE-ECOLE 
      " - "
      (cond
	((and Liste-Groupes-Aff t)
	  (catenate #Mv_i-groupes " "
	    (libelle-i-groupe (car liste-groupes-aff))))
	((and Liste-Salles-Aff t)
	  (catenate #Mv_i-salle " " 
	    (libelle-i-salle (car liste-salles-Aff))))
	(t
	  (catenate #Mv_i-prof " "
	    (civilite-i-prof (car liste-profs-aff))
	    " " (libelle-i-prof (car liste-profs-aff)))))
      " - "
      (if Week-Aff
	(catenate #Mv_semaine
	  (string (ecrire-numero-semaine Week-Aff))
	  (when (calendaire-i-ecole i-ecole)
	    (catenate " (" week-string ")")))
	#Mv_aff-pla))))

(defun creer-chaine-dates (week)
  (let
    ((chaine-dates ""))
    (when
      (and
	(calendaire-i-ecole i-ecole)
	week
	(= (funcall 'semaine_reelle week (current-directory)) 0))
      (with ((current-directory REP-VARUNA))
	(when (probefile DATESFILE)
	  (with ((inchan (openi DATESFILE)))
	    (setq chaine-dates (string (read)))
	    (close))
	  (deletefile DATESFILE))))
    chaine-dates))
 

;;; affiche les cours dans appli-impression

;;; DBG (08/03/95) MO
;;; to-print est une variable locale de l'appli aff-pri il faut donc l'envoyer
;;; en parametre

(defun afficher-cours-impression (appli)
  (setq Impressionp t)
  (mapc 
    (lambda (co)
      (when (cours-affichable-p co) (show-cours (component 'grid appli) co))) 
    Liste-I-Cours)
  (setq Impressionp ()))

;;; <afficher-resas-impression>
;;; CRE - FD - 18/09/95
;;; affiche les resa dans appli-impression
(defun afficher-resas-impression (appli)
  (setq Impressionp t)
  (mapc
    (lambda (resa)
      (when
        (resa-affichable-p resa)
        (afficher-resa (component 'grid appli) resa)))
    Liste-I-Resa)
  (setq Impressionp ()))
    
;;; <afficher-ajouts-impression>
;;; CRE - FD - 18/09/95
;;; affiche les ajouts dans appli-impression
(defun afficher-ajouts-impression (appli)
  (setq Impressionp t)
  (mapc
    (lambda (ajout)
      (when
        (ajout-affichable-p ajout)
        (afficher-ajout (component 'grid appli) ajout)))
    Liste-I-Ajout)
  (setq Impressionp ()))
    
;;; demande le format d'impression, ajoute appli-impression
;;; et les cours, dumpe l'appli et envoie le fichier genere
;;; a l'imprimante

;;; DBG (07/03/95) MO et FD
;;; le bug aff-pri: divide by zero provenait d'un add-and-select-application
;;; bizarre remplace par un simple add-application
;;; !! la primitve add-and-select-application (i-util.ll) merite d'etre
;;; regarde de plus pres
;;; !! il ne faut surtout pas oublie de loader le patch.ll 
;;; !! et si on lui trouvait un autre nom a ce patch ?????


;;; AJT (27/03/95) MO
;;; ajout d'un deletefile pour pallier l'imcompatibilite de la 15.26 sur le 
;;; copyfile
;;; CHG (29/03/95) MO
;;; release ILOG debuggant -a priori- cette icompatibilite

;;; MOD RM - 20/08/96 : aiguillage pour une impression postscript ou
;;; l'utilisation d'un emulateur
 
(defun aff-pri (num-fichier titre long-liste)
  (let* 
    ((le-titre titre)
     (to-print (application-impression))
     (device Format-Impression)
     (old-rmargin (rmargin)))

  (when (and device le-titre)
    (send 'set-line (component 'grid1-title to-print) le-titre)
    (send 'set-line (component 'grid2-title to-print) (titre-impression))

    ;; AJT RM : date de derniere modif
    (send 'set-line (component 'date-title to-print)
      (format () "Imprimé le ~A" (afficher-date)))

    ;; change' le add-and-select-application en add-application
    (add-application to-print)
    (add-grid (component 'grid to-print))
    (afficher-objets-impression to-print)
    (rmargin 1024)
    (if (and num-fichier (> long-liste 1))
      (send 'filename device (catenate "ps" num-fichier ".ps"))
      (send 'filename device "psdump.ps"))
    (ps-dump device to-print)
    (remove-application to-print)
    (rmargin old-rmargin))))

(defun imprimer-fichier (multi)
  (ifn multi
    (if POSTSCRIPT-DEVICE
      (imprimer-fichier-ps FICHIER-POSTSCRIPT)
      (imprimer-fichier-non-ps))
    (if POSTSCRIPT-DEVICE
      (imprimer-fichier-ps-multi FICHIER-PS-MULTI multi)
      (imprimer-fichier-non-ps-multi multi))))
  
  
;;; CRE RM - 20/08/96 : impression du fichier en mode postscript
;;; MOD RM - 24/02/97 : utilisation de Vprinter

(defun imprimer-fichier-ps (fichier)
  (let
    ((commande-impression
      (catenate (getenv "VAREDIT")
        "\vprinter.exe " fichier " -ps")))
    (when (< (funcall 'vexecute commande-impression (current-directory)) 33)
      (afficher-erreur "Erreur d'impression"))))

;;; CRE RM - 26/08/97 : impression multiple PS

(defun imprimer-fichier-ps-multi (fichier num)
  (let
    ((commande-impression
      (catenate (getenv "VAREDIT")
        "\vprinter.exe " fichier " -f" num " -ps")))
    (when (< (funcall 'vexecute commande-impression (current-directory)) 33)
      (afficher-erreur "Erreur d'impression"))))

(defun imprimer-fichier-texte (fichier)
  (let
    ((commande-impression
      (catenate (getenv "VAREDIT")
        "\vprinter.exe " fichier " -oem -end")))
    (when (< (funcall 'vexecute commande-impression (current-directory)) 33)
      (afficher-erreur "Erreur d'impression"))))

(defun imprimer-fichier-non-ps ()
  (when CMDLINE-NON-PS
    (if (< (funcall 'vexecute CMDLINE-NON-PS (current-directory)) 33)
      (afficher-erreur "Erreur d'impression"))))

;;; CRE RM - 26/08/97 : impression multiple non PS

(defun imprimer-fichier-non-ps-multi (num)
  (when CMDLINE-NON-PS
    (let
      ((new-cmdline
	(substring CMDLINE-NON-PS 0 (index "psdump.ps" CMDLINE-NON-PS 0)))
       (i 0))
      (when num
	(setq num (- num 1)))
      (for (i 0 1 num t)
	(setq new-cmdline
	  (catenate new-cmdline " ps" i ".ps")))
      (setq new-cmdline (catenate new-cmdline " quit.ps"))
      (if (< (funcall 'vexecute new-cmdline (current-directory)) 33)
	(afficher-erreur "Erreur d'impression")))))

;;; <afficher-objets-impression>
;;; CRE - FD - 18/09/95
;;; afficher tous les objets affichables dans appli

(defun afficher-objets-impression (appli)
  (afficher-cours-impression appli)
  (afficher-resas-impression appli)
  (afficher-ajouts-impression appli))


;;; II. Autres primitives globales:


;;; la primitive <show-edt> gere l'affichage des cours qui doivent etre 
;;; affiches selon la semaine, la filiere.... choisies. Son nom n'est donc 
;;; pas astucieux (devrait etre quelque-chose comme <show-cours-affichables>.
;;; rem (10/04/95) MO: pourquoi pas du franglais, tant qu'on y est !!!!

;;; DBG (24/05/95) MO
;;; j'ai rajoute le <or> pour traiter le cas ou les deux globales Liste-Salles
;;; -aff et liste-groupes-aff sont ()

;;; MIN (10/07/2000) MO
;;; quand une salle n'existe pas, show-edt plante car la variable n'est pas définie et l'erreur n'est pas trappée
;;; le curseur reste sur le sablier et les cours ne sont pas affichés



(defun show-edt (appli)
  (change-curseur appli Varuna-Curseur-Sablier)
  (add-grid (component 'grid Gen-App))
  (when Liste-Groupes-Aff
    (setq Liste-Cours-Aff (sous-liste cours-affichable-p Liste-I-Cours)))
  (mapc 'maybe-show-cours Liste-I-Cours)
  (mapc
    (lambda (c) (afficher-resa (component 'grid Gen-App) c))
    Liste-I-Resa)
  (mapc
    (lambda (a) (afficher-ajout (component 'grid Gen-App) a))
    Liste-I-Ajout)
  (change-curseur appli Varuna-Curseur-Fleche))  

;;; rem MO FD 20/09/95 cette primitive ne semble pas servir dans le code
;;; action associee au bouton du display

(defun aff-cours-but (b)
  (info-cours (component 'cours b)))


;;; switch affichage semaine/annuel

(defun aff-change (a)
  (if (eq Grid-Type 'year)
    (setq Grid-Type 'week)
    (setq Grid-Type 'year))
  (add-grid (component 'grid Gen-App)))


;;; actions des boutons suivant et precedent du display
;;; selon le type d'affichage
;;; passe a la filiere suivante ou a la semaine suivante
;;; rem (23/05/95) MO: n'est-ce pas obsolete dorenavant ???


 (defun aff-precedent (appli)
  (when Week-Aff
    (setq
      Week-Aff
      (or 
        (ge (sub1 Week-Aff) Semaine-Debut)
        Semaine-Fin))
    (update-grid-titles)
    (aff-refresh Gen-App)))

 ;;; rem (23/05/95) MO: n'est-ce pas obsolete dorenavant ???
 
(defun aff-suivant (appli)
  (when Week-Aff
    (setq
      Week-Aff
      (or 
        (le (add1 Week-Aff) Semaine-Fin)
          Semaine-Debut))
    (update-grid-titles)
    (aff-refresh Gen-App)))


;;; gestion de la taille de la grille
;;; des quatre boutons agrandir, rapetir, etc..

;;; mutiplie l'echelle de appli par facteur
;;; met a jour les barres de defilement de l'appli
;;; lorsque facteur vaut 0, est equivalent a un fit-to-contents

(defun modifier-taille-grille (appli facteur)
  (let* ((grille (component 'grid appli))
    (defileur (component 'defileur appli))
    (w (send 'width grille))
    (h (send 'height grille))
    (new-width (floor (max (or (< (* w facteur) 32000) w)
      (- (send 'width defileur) 200 {scroller}:scrollbarwidth))))
    (new-height (floor (max (or (< (* h facteur) 32000) h)
      (- (send 'height defileur) 200 {scroller}:scrollbarheight))))) 
  (send 'resize grille new-width new-height)
  (send 'reinitialise defileur)
  (aff-refresh appli)))


(defun agrandir-grille (appli)
  (modifier-taille-grille appli PLUS-GROS))

(defun rapetir-grille (appli)
  (modifier-taille-grille appli PLUS-PETIT))

(defun echelle-grille (appli)
  (let ((scale (demande-entier #Mv_echelle-pourcent 100 500)))
    (when scale
      (modifier-taille-grille appli (/ scale 100)))))

(defun retailler-grille (appli)
  (modifier-taille-grille appli 0))


;;; affichage partiel - demande de jours, et maj globales affichage

(defun aff-jours (a)
  (let ((les-jours (demande-jours-ouvres)))
    (when les-jours 
      (setq Jours-Aff (sortn (mapcar 'jour-libelle les-jours))))))

;;; affichage partiel - demande un intervalle de semaine
;;; maj globales affichage
;;; MOD - FD - 29/07/96 semaines calendaires
;;; demande-entier remplace par demande-semaine

(defun aff-semaines (a)
  (let*
    ((semaine1 (demande-semaine #Mv_premiere-semaine 1 Semaine-Fin))
     (semaine2 (when semaine1
       (demande-semaine #Mv_deuxieme-semaine semaine1 Semaine-Fin))))
    (when (and semaine1 semaine2)
      (setq Semaine-Debut-Aff semaine1)
      (setq Semaine-Fin-Aff semaine2))))

;;; affichage partiel - demande d'horaire de debuts, en fait des types
;;; de cours. maj globales

(defun aff-heures (a)
  (let
    ((types-cours (demandes-i-type-cours "Types de cours :" () t)))
    (when types-cours
      (setq Heures-Debut-Aff (horaires-types-cours types-cours)))))
        

;;; affichage partiel - interface util
;;; MOD RM - 20/12/95 : Utilisation de la boite d'options
;;; MOD RM - 07/02/96 : On enleve la boite

(defun aff-partiel (a)
  (and
    (aff-jours a)
    (or (eq Grid-Type 'week) (aff-semaines a))
    (or (le (length Liste-I-Type-Cours) 1) (aff-heures a))
    (aff-clear a)
    (aff-refresh a)))

;;; affichage complet - mise a jour des globales, puis refresh
;;; MOD RM - 20/12/95 : Utilisation de la boite d'options
;;; MOD RM - 07/02/96 : On enleve la boite

(defun aff-complet (a)
  (setq Jours-Aff Jours-Ouvres)
  (setq Semaine-Debut-Aff 1)
  (setq Semaine-Fin-Aff Semaine-Fin)
  (setq Heures-Debut-Aff (horaires-types-cours (types-cours-i-ecole I-ECOLE)))
  (aff-clear a)
  (aff-refresh a))


;;; modifie les parametres d'affichage de telle sorte que
;;; le cours cours soit visible a l'ecran
          
(defun rendre-visibilite (a cours)
  (setq Liste-Groupes-Aff (groupes-i-cours cours))
  (setq Liste-Salles-Aff ())
  (setq Liste-Profs-Aff ())
  (when Week-Aff 
    (setq Week-Aff (semaine-instant (instant-trouve-i-cours cours))))
  (aff-complet a))))


;;; AJT RM - 270795 : AJOUTS DE SEANCES
;;; MOD RM - 15/01/97 : on se met dans la bonne semaine

(defun aff-ajt (a)
  (let
    ((ajout ()))
    (cond
      ((null Liste-I-Ajout) (afficher-attention #Mv_aucun-ajout))
      ((mono-element-p Liste-I-Ajout)
        (afficher-information #Mv_ajout-unique)
        (setq ajout (car Liste-I-Ajout))
        (maj-liste-ajouts-aff Liste-I-Ajout))
      (t
        (setq ajout (demande-i-ajout #Mv_pro-ajout1 ()))
        (when ajout
          (maj-liste-ajouts-aff (list ajout)))))
    (when ajout
      (setq Grid-Type 'week)
      (setq Week-Aff (semaine-i-ajout ajout))
      (update-grid-titles)
      (show-edt a))))


;;; <maj-liste-ajouts-aff>
;;; met a jour les variables globales d'affichage

(defun maj-liste-ajouts-aff (liste-ajouts-a-afficher)
  (setq Liste-Groupes-Aff ())
  (setq Liste-Profs-Aff ())
  (setq Liste-Salles-Aff
    (mapcar 'salle-i-ajout liste-ajouts-a-afficher)))

;;; CRE RM 25/11/96 : affichage des chevauchements d'un creneau

(defun bouton-info-cours (bouton)
  (let
    ((cours ({asker}:get-answer (component 'appli bouton))))
    (if (not cours)
      (afficher-attention #Mv_selectionner-cours)
      (info-cours
        (eval (string2symbol
          (substring cours 0 (+ (chrpos #/ cours) 1))))))))

(defun afficher-cours-chevauchement (cours liste)
  (let*
    ((b-info
      (bouton-standard
        (font FONT-EDITEUR (cadrer-centrer "Infos" 10))
        'bouton-info-cours))
     (dialogue
       (object-asker
         (column
           #Mv_liste-cours-chevauchement
           (catenate
             (libelle-i-cours cours) " (" (code-i-cours cours) ")")
           (hypersel 400 200 liste)
	   ESP)
         ()
         (list b-info))))
    (with ((current-background (make-named-color "white")))
      (send 'set-title dialogue "Cours en chevauchement")
      (add-component b-info 'appli dialogue)
      (grab-and-select-application dialogue)
      ({asker}:get-answer dialogue))))

(defun liste-cours-chevauchement (cours libellep)
  (let
    ((instant (instant-trouve-i-cours cours))
     (duree (div (duree-i-cours cours) Unite-Temps))
     (seances (nb-seances-i-cours cours))
     (result ()))
    (mapc
      (lambda (c)
        (when
          (cours-chevauchent-p instant duree seances
            (instant-trouve-i-cours c)
            (div (duree-i-cours c) Unite-Temps)
            (nb-seances-i-cours c))
          (if libellep
            (newl result
              (catenate (code-i-cours c) " - " (libelle-i-cours c)))
            (newl result (code-i-cours c)))))
      (sous-liste cours-affichable-p Liste-I-Cours))
    result))

(defun valeur-max-cours-chevauchement (cours liste-cours-aff)
  (let
    ((instant (instant-trouve-i-cours cours))
     (duree (div (duree-i-cours cours) Unite-Temps))
     (seances (nb-seances-i-cours cours))
     (valeur 0)
     (valeur-max 1))
    (for (i 0 3 (- (div (duree-i-cours cours) UNITE-TEMPS) 1) valeur-max)
      (setq valeur 0)
      (mapc
        (lambda (c)
          (when
            (cours-chevauchent-p (+ instant i) 1 seances
            (instant-trouve-i-cours c)
            (div (duree-i-cours c) Unite-Temps)
            (nb-seances-i-cours c))
            (incr valeur)))
        liste-cours-aff)
      (setq valeur-max (max valeur-max valeur)))))

(defun traitement-chevauchement (cours)
  (let
    ((l-chevauchement (liste-cours-chevauchement cours t))
     (nouveau-cours))
    (if (eq (length l-chevauchement) 1)
      (info-cours cours)
      (setq nouveau-cours
        (afficher-cours-chevauchement cours l-chevauchement))
     (when nouveau-cours
       (setq nouveau-cours
         (eval (string2symbol
           (substring nouveau-cours 0 (+ (chrpos #/ nouveau-cours) 1)))))
       (send 'v-remove-application
         (component 'grid Gen-App)
         (code-i-cours nouveau-cours))
       (show-cours
         (component 'grid Gen-App) nouveau-cours)))))

(defun cours-chevauchent-p (instant1 duree1 nbsem1 instant2 duree2 nbsem2)
  (and
    (let
      ((semaine1 (semaine-instant instant1))
        (semaine2 (semaine-instant instant2))) 
      (and
        (lt semaine1 (add semaine2 nbsem2))
        (lt semaine2 (add semaine1 nbsem1))))
    (let
      ((moment1 (moment-instant instant1))
        (moment2 (moment-instant instant2)))
      (and
        (lt moment1 (add moment2 duree2))
        (lt moment2 (add moment1 duree1))))))

;;; CRE RM 19/12/96 : affichage de la derniere erreur

(defun aff-derniere-erreur (a)
  (ifn *last-error*
    (afficher-information #Mv_no-error)
    (newl *erreurs* *last-error*)
    (traiter-erreur)))

;;; CRE RM 11/05/99 : compatibilite AN 2000 ! (merci ILOG ...)

(defun afficher-date ()
  (let
    ((ma-date ""))
    (when (= (funcall 'afficher_date) 0)
      (with ((current-directory REP-VARUNA))
	(when (probefile DATESFILE)
	  (with ((inchan (openi DATESFILE)))
	    (setq ma-date (string (read)))
	    (close))
	  (deletefile DATESFILE))))
    ma-date))

