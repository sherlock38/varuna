;;; --------------------------------------------------------------------------
;;; - i-msg.ll                                                  VARUNA 1.2   -
;;; - les messages de l'interface de VARUNA                                  -
;;; -                                                                        -
;;; --------------------------------------------------------------------------
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; - Creation:  Septembre 94 JH                                             -
;;; - Derniere mise a jour pour version 1.13:                                -
;;; - Build 1.22.1 01.07.99 / MO FD                                          -
;;; --------------------------------------------------------------------------


;;; REM (09/01/94) MO
;;; ce fichier merite d'e^tre remis a jour
;;; un certain nombre de message ne sont plus utilise, d'autres sont appeles
;;; par des macros et on devrait savoir d'ou ils sont appelles exactement
;;; bref un petit nettoyage s'impose...



(defun initialiser-messages ()



  
;;; I. Messages Demandes d'objet
;;; ----------------------------

  (defmessage v_pro-profs
    (french "Quels professeurs ?")
    (english "Which professors ?"))

  (defmessage v_pro-groups
    (french "Quelles familles ?")
    (english "Which groups ?"))
  
  (defmessage v_pro-group
    (french "Quelle famille ?")
    (english "Which group"))

  (defmessage v_pro-salle
    (french "Quelle salle ?")
    (english "???"))

;;; AJT RM - 04/07/95 : Reservation de salles
  (defmessage v_pro-resa
    (french "Quelles réservations ?")
    (english "???"))

  ;;; AJT RM - 26/07/95 : Annulations de seances
  (defmessage v_pro-annulation
    (french "Quelles annulations de séances ?")
    (english "???"))

  ;;; AJT RM - 26/07/95 : ajouts de seances
  (defmessage v_pro-ajout
    (french "Quels ajouts de séances ?")
    (english "???"))

  
  
;;; II. Messages Mode Saisie
;;; -----------------------
  
;;; II.1. Titres de panneaux
;;; 

  
  (defmessage v_titre-saisie-i-groupe
    (french "Saisie d'une famille")
    (english "???"))
  
  (defmessage v_titre-saisie-i-groupe-enseignement
    (french "Saisie d'un enseignement")
    (english "???"))
  
  (defmessage v_titre-saisie-i-groupe-filiere
    (french "Saisie d'une filière")
    (english "???"))
  
  (defmessage v_titre-saisie-i-groupe-td-a
    (french "Saisie d'une famille de TD/TP simultanés")
    (english "???"))
  
  (defmessage v_titre-saisie-i-groupe-td-b
    (french "Saisie d'une famille de TD/TP même semaine")
    (english "???"))
  

;;;

  (defmessage v_bande-varuna
    (french "Varuna 1.22 - Copyright Abstract 1993 - 1999" )
    (english "Varuna 1.22 - Copyright Abstract 1993 - 1999" ))

  
  
;;; III. Messages Mode Resolution
;;; ----------------------------
  
  
;;; III.1 Imprimante
;;; 
  
  (defmessage v_pro-device
    (french "Mise en page ?")
    (english "Page layout ?"))
  
  
;;; III.2. Messages concernant l'affichage famille, salle, resa
;;; 
  
  (defmessage v_no-groups
    (french "Aucune famille n'est définie")
    (english "???"))
  
  (defmessage v_aucune-famille
    (french "Il n'existe pas de familles")
    (english "???"))
  
  (defmessage v_famille-unique
    (french "Il n'existe qu'une seule famille.")
    (english "???"))
  
  (defmessage v_affichage-sous-familles1
    (french "Certaines des familles selectionnées ont des sous-familles.")
    (english "???"))
  
(defmessage v_affichage-sous-familles2
    (french "Les voulez-vous ?")
    (english "???"))


  (defmessage v_salle-unique
    (french "Il n'existe qu'une seule salle.")
    (english "???"))
  
  (defmessage v_aucune-salle
    (french "Aucune salle n'est définie")
    (english "???"))

  (defmessage v_prof-unique
    (french "Il n'existe qu'un seul professeur.")
    (english "???"))
  
  (defmessage v_aucun-prof
    (french "Aucun professeur n'est défini.")
    (english "???"))

  (defmessage v_aucun-prof-sans-cours
    (french "Tous les professeurs dispensent des cours.")
    (english "???"))

  (defmessage v_resa-unique
    (french "Il n'existe qu'une seule réservation.")
    (english "???"))

  (defmessage v_quel-affichage
    (french "Vous n'avez pas défini de familles ou salles d'affichage")
    (english "???"))
 
  (defmessage v_copyright-resolution
     (french  "Varuna 1.22 - Copyright Abstract 1993 - 1999" )
     (english  "Varuna 1.22 - Copyright Abstract 1993 - 1999" ))


;;; messages lies a la suppression d'un type de cours
  (defmessage v_i-remplacer-type
    (french "Remplacer un type de cours")
    (english "???"))
  (defmessage v_i-supprimer-cours
    (french "Supprimer les cours")
    (english "???"))
  
;;;  A CLASSER
;;;  ---------
  
  (defmessage v_champ
    (french "Le champ ")
    (english "The field "))
  (defmessage v_i-groupe-libelle
    (french "Libelle : ")
    (english "Name : "))

  (defmessage v_i-groupes
    (french "Familles : ")
    (english "Groups : "))
;;; AJT RM - 29/06/95 Cours sans salle
  (defmessage v_don-csal
    (french "Cours sans salle")
    (english "???"))
;;; AJT RM - 26/07/95 Annulation de seances
  (defmessage v_don-ann
    (french "Annulations de séances")
    (english "???"))
;;; AJT RM - 26/07/95 Ajout de seances
  (defmessage v_don-ajt
    (french "Ajouts de séances")
    (english "???"))
;;; AJT - FD - 04/07/96
  (defmessage v_don-cours-sans-prof
    (french "Cours sans professeur")
    (english "???"))
  (defmessage v_don-type-salle
    (french "Salle d'un type donné")
    (english "???"))
  (defmessage v_don-cours-type
    (french "Cours d'un type donné")
    (english "???"))

  
  (defmessage v_choix-type-salle
    (french "Quel type de salle ?")
    (english "???"))
  (defmessage v_choix-type-cours
    (french "Quel type de cours ?")
    (english "???"))
  
  (defmessage v_type-cours
    (french "Type du cours : ")
    (english "Class type :  "))
  (defmessage v_i-groupe-contraintes
    (french "Contraintes : ")
    (english "Constraints : "))
  (defmessage v_i-groupe-groupes
    (french "Sous-Famille(s) : ")
    (english "Groups : "))
  (defmessage v_i-groupe-cours
    (french "Cours : ")
    (english "Classes : "))
  (defmessage v_i-groupe-libelle
    (french "Libellé : ")
    (english "Name : "))
  (defmessage v_i-groupe-code
    (french "Code : ")
    (english "ID : "))
  (defmessage v_i-groupe-type-cours
    (french "Type de cours : ")
    (english "???"))
  (defmessage v_paperh
    (french "Hauteur du papier (mm) : ")
    (english "Paper height (mm) : "))
  (defmessage v_paperw
    (french "Largeur du papier (mm) : ")
    (english "Paper width (mm) : "))
  (defmessage v_orient
    (french "Orientation  : ")
    (english "Orientation : "))
  (defmessage v_copies
    (french "Copies : ")
    (english "Copies : "))
  (defmessage v_paysage
    (french "Paysage")
    (english "Landscape"))
  (defmessage v_portrait
    (french "Portrait")
    (english "Portrait"))
  (defmessage v_mise-en-page
    (french "Mise en page")
    (english "Page layout"))
  (defmessage v_duree-non-valide
    (french "la durée doit être multiple de l'unité de temps : ")
    (english " ??? "))
  (defmessage v_titre-terminal
    (french " Edition de Données ")
    (english " ??? "))
  (defmessage v_titre-demande
    (french " Varuna - Demande")
    (english " Varuna - Asker "))
  (defmessage v_titre-warning
    (french " Varuna - Avertissement ")
    (english " Varuna - Warning "))
  (defmessage v_titre-confirmation
    (french " Varuna - Confirmation ")
    (english " ??? "))
  (defmessage v_titre-oui-non-annuler
    (french " Confirmation ")
    (english " ??? "))
  (defmessage v_titre-saisie-i-salle
    (french " Saisie d'une salle")
    (english " ??? "))
  (defmessage v_titre-saisie-i-prof
    (french " Saisie d'un professeur")
    (english " ??? "))
  (defmessage v_titre-saisie-i-ecole
    (french " Saisie des données de l'établissement")
    (english " ??? "))
  (defmessage v_titre-saisie-i-cours
    (french " Saisie d'un cours")
    (english " ??? "))
  (defmessage v_titre-saisie-i-type-cours
    (french "Saisie d'un type de cours")
    (english "???"))
  (defmessage v_titremoderesolution
    (french " Varuna - Mode Résolution ")
    (english " Varuna - Solver "))
  (defmessage v_titremodesaisie
    (french " Varuna - Mode Saisie ")
    (english " Varuna - Enter "))
  (defmessage v_mode-consultation
    (french " - Mode Consultation ")
    (english " - Consult "))

  (defmessage v_version
    (french "Varuna version 1.22 - 01/07/99")
    (english "Varuna version 1.22 - 01/07/99"))
  (defmessage v_build
    (french "Build 1 - 01/07/99")
    (english "Build 1 - 01/07/99"))
  (defmessage v_copyright
    (french "Copyright Abstract & P. Rivière 1993-1999")
    (english "Copyright Abstract & P. Rivière 1993-1999"))
  (defmessage varuna
    (french "VARUNA")
    (english "VARUNA"))
  (defmessage v_support-technique
    (french "Support Technique :")
    (english "Hotline :"))
  (defmessage v_telephone
    (french "   Téléphone : 01 42 53 99 66")
    (english "   Phone number : 01 42 53 99 66"))
  (defmessage v_fax
    (french "   Télécopie  : 01 42 53 10 41")
    (english "   Fax : 01 42 53 10 41"))
  (defmessage v_email
    (french "   Email : support@varuna.com")
    (english "   Email : support@varuna.com"))


;;;  Message d'erreur de deplacement des cours

  (defmessage errdepl
    ;; (french "Impossible de déplacer le cours")
    (french "Impossible de placer le cours à cet instant")
    (english "???"))
  (defmessage errDureeCours
    (french "La durée du cours est incorrecte")
    (english "???"))
  (defmessage Info-Dep-Dispo-Cours
    (french "Les choix de disponibilité pour ce cours ne sont pas respectés")
    (english "???"))
  (defmessage Info-Dep-Dispo-Profs
    (french "L'un des professeurs n'est pas disponible.")
    (english "???"))
  (defmessage Info-Dep-Dispo-Groupes
    (french "La disponibilité de la famille n'est pas respectée")
    (english "???"))
  (defmessage Info-Dep-Profs-Pris
    (french "L'un des professeurs donne un autre cours à cet instant")
    (english "???"))
  (defmessage Info-Dep-Pas-de-salles
    (french "Aucune salle de ce type et de cette audience n'est disponible")
    (english "???"))
  (defmessage Info-Dep-Contraintes-Non-Respectees
    (french "Les contraintes suivantes ne sont pas respectées")
    (english "???"))
  (defmessage Info-Dep-Mauvais-Horaire-Debut
    (french "Cet horaire de début ne correspond pas au type du cours.")
    (english "???"))
  (defmessage Info-Dep-Mauvais-Moment-Debut
    (french "Le cours n'est pas compatible avec les heures d'ouverture de l'école")
    (english "???"))
  (defmessage errdepsal
    (french "Il n'y a pas de salle disponible pour ce cours")
    (english "There is no room for this class"))
  (defmessage errdepcou3rem
    (french "Car il est multi-fréquence")
    (english "???"))
  (defmessage errdepcou3
    (french "Ce cours ne peut être placé à cette semaine")
    (english "???"))
  (defmessage errdepcou2
    (french "Les contraintes du cours sont violées")
    (english "The class constraints are violated"))
  (defmessage errdepcou1
    (french "La contrainte du cours est violée")
    (english "The class constraint is violated"))
  (defmessage errdeppro2
    (french "Le professeur donne un autre cours à cet instant")
    (english "The professor gives another lecture at this time"))
  (defmessage errdeppro1
    (french "Le professeur n'est pas disponible à cet instant")
    (english "The professor is not disponible"))
  (defmessage errdepeco
    (french "Déplacement impossible")
    (english "You can't move this class there"))
  (defmessage errnodomrem
    (french "Vérifiez les disponibilités du professeur et du cours")
    (english " "))
  (defmessage errnodom
    (french "Le cours est trop contraint")
    (english "Too much constraint on this class"))
  (defmessage errseuilechec
    (french " échecs successifs")
    (english " echecs successifs"))
  (defmessage errgene
    (french " Erreur générale.")
    (english " severe error."))
  (defmessage errresol
    (french " Erreur en cours de résolution.")
    (english " solver error."))
  (defmessage erralias
    (french " Jour inconnu.")
    (english " Alias inconnu."))
  (defmessage errsyntaxpro
    (french " Erreur de syntaxe (expression des contraintes)")
    (english " Syntax error."))
  (defmessage errnopro
    (french " n'a pas de professeur.")
    (english " have no professor."))
  (defmessage errnofili
    (french " n'a pas de famille.")
    (english " has no groups."))
  (defmessage errnoroom
    (french "Le ou les cours suivants n'ont pas de salles")
    (english "the following classes have no room"))
  (defmessage errnotype
    (french "Aucune salle de type ")
    (english "No possible room for class due to type "))
  (defmessage errcontraintes
    (french "Echec en posant les contraintes.")
    (english "Fail to settings constraints"))
  (defmessage errnodate
    (french "Les cours suivant n'ont pas été places")
    (english "No solution - try again... "))
  (defmessage errMCNC
    (french "Contrainte de non chevauchement : ")
    (english "???"))
  (defmessage errMCPS
    (french "Contrainte de proximité semaines")
    (english "???"))
  (defmessage errMCP
    (french "Contrainte de précédence")
    (english "???"))
  (defmessage errMCJD
    (french "Contrainte de jours différents")
    (english "???"))

  (defmessage v_nom-fichier
    (french "Nom du fichier :")
    (english "Output file :"))
  (defmessage v_enregistrer
    (french "Voulez-vous enregistrer les modifications ?")
    (english "Save data"))
  (defmessage v_confirm
    (french "Etes-vous certain ?")
    (english "Are you sure ?"))
  (defmessage v_createf
    (french "Voulez-vous la créer ?")
    (english "Do you want to create it ?"))
  (defmessage v_createm
    (french "Voulez-vous le créer ?")
    (english "Do you want to create it ?"))
  (defmessage v_existe-deja
    (french " existe déjà. ")
    (english " already  exists. "))
  (defmessage v_noexist
    (french "n'existe pas. ")
    (english "doesn't exist. "))
  (defmessage v_continuer
    (french " Continuer ")
    (english " Continue "))
  (defmessage v_voulez-vous-continuer
    (french "Voulez vous continuer ?")
    (english "Do you still want to continue ?"))
  (defmessage v_interrompre
    (french " Interrompre ")
    (english "Stop"))
  (defmessage v_dupliquer
    (french "Copier")
    (english "Copy"))
  (defmessage v_supprimer
    (french "Supprimer")
    (english "Delete"))
  (defmessage v_editer
    (french "Modifier")
    (english "Edit"))
  (defmessage v_creer
    (french "Créer")
    (english "Create"))
  (defmessage v_annuler
    (french " Annuler ")
    (english " Cancel "))
  (defmessage v_non
    (french "  Non  ")
    (english " No  "))
  (defmessage v_oui
    (french "  Oui  ")
    (english " Yes "))
  (defmessage v_avant-quitter
    (french "avant de quitter ?")
    (english "before quitting ?"))
  (defmessage v_ecole-autres-heures-fin
    (french "Autres horaires de fin : ")
    (english "Other end of classes : "))
  (defmessage v_ecole-autres-heures-debut
    (french "Autres horaires de début : ")
    (english "Other time of classes : "))
  (defmessage v_ecole-heures-debut
    (french "Horaires de début des cours : ")
    (english "Begining of classes : "))
  (defmessage v_ecole-jours-ouvrables
    (french "Jours ouvrables : ")
    (english "Workdays : "))
  (defmessage v_heures-debut-vs-dispo
    (french "Les heures de début de cours sont incompatibles")
    (english "???"))
  (defmessage v_cours-suivants-incompatibles
    (french "Les disponibilités de cours suivants sont incompatibles")
    (english "???")) 
  (defmessage v_heures-debut-vs-dispo2
    (french "avec les horaires d'ouverture de l'établissement.")
    (english "???"))
  (defmessage v_ecole-heure-pause
    (french "Heure repas : ")
    (english "Lunch hour: "))
  (defmessage v_i-ecole-unite-temps
    (french "Unité de temps  : ")
    (english "Time unit : "))
  (defmessage v_i-ecole-grain-dispo
    (french "Grain disponibilites : ")
    (english "???"))
  (defmessage v_i-ecole-param-chevauchement
    (french "Paramètre de chevauchement : ")
    (english "Overlapping parms : "))
  (defmessage v_i-ecole-param-chevauchement-max
    (french "Par max : ")
    (english "???"))
  (defmessage v_heure-fin
    (french "Heure de fermeture : ")
    (english "Closing hour : "))
  (defmessage v_heure-debut
    (french "Heure d'ouverture : ")
    (english "Opening hour : "))
  (defmessage v_i-ecole-semaine-fin
    (french "Nombre de semaines : ")
    (english "Last week of class : "))
  (defmessage v_i-ecole-dispo
    (french "Horaires d'ouverture de l'établissement")
    (english "???"))
  (defmessage v_i-ecole-semaine-debut
    (french "Semaine de début des cours : ")
    (english "First week of class : "))
  (defmessage v_i-ecole-libelle
    (french "Nom de l'établissement : ")
    (english "Name : "))
  (defmessage v_i-ecole-types-cours
    (french "Types de cours : ")
    (english "Class types : ")) 
  (defmessage v_jours-ouvrables
    (french "Jours ouvrables : ")
    (english "Workdays : "))
  (defmessage v_i-type-cours 
    (french "Type de cours : ")
    (english "???"))
  (defmessage v_i-type-cours-code
    (french "Code : ")
    (english "ID : "))
  (defmessage v_i-type-cours-libelle
    (french "Type de cours : ")
    (english "Class type : "))
  (defmessage v_i-type-cours-liste-moments-cumules
    (french "Horaires de début de cours")
    (english "???"))
  (defmessage v_ecole
    (french "Etablissement : ")
    (english "School  "))
  (defmessage v_fili-cours
    (french "Cours : ")
    (english "Classes : "))
  (defmessage v_fili-libelle
    (french "Nom : ")
    (english "Name : "))
  (defmessage v_fili-code
    (french "Code : ")
    (english "ID : "))
  (defmessage v_la-filiere
    (french "la famille ")
    (english "this group "))
  (defmessage v_filiere
    (french "Famille : ")
    (english "Group : "))
  (defmessage v_de-la-filiere
    (french "de la filière")
    (english "???"))
  (defmessage v_i-prof-cours
    (french "Cours : ")
    (english "Class : "))
  (defmessage v_i-prof-libelle
    (french "Nom : ")
    (english "Name : "))
  (defmessage v_i-prof-code
    (french "Code : ")
    (english "ID : "))
  (defmessage v_le-prof
    (french "le professeur ")
    (english "this professor "))
  (defmessage v_i-prof
    (french "Professeur : ")
    (english "Professor : "))
  (defmessage v_i-salle-capacite
    (french "Capacité : ")
    (english "Capacity : "))
  (defmessage v_i-salle-type
    (french "Type : ")
    (english "Type : "))
  (defmessage v_i-salle-libelle
    (french "Nom : ")
    (english "Name : "))
  (defmessage v_i-salle-code
    (french "Code : ")
    (english "ID : "))
  (defmessage v_i-salle-commentaires
    (french "Commentaires : ")
    (english "Comments : "))
  (defmessage v_la-salle
    (french "la salle ")
    (english "this room "))
  (defmessage v_i-salle
    (french "Salle : ")
    (english "Room : "))
  (defmessage v_i-cours-audience
    (french "Audience : ")
    (english "Audience : "))
  (defmessage v_i-cours-salle-figee
    (french "Salle : ")
    (english "Room : "))
  (defmessage v_i-cours-commentaires
    (french "Commentaires : ")
    (english "Comments : "))
  (defmessage v_i-cours-type-salle
    (french "Type de salle : ")
    (english "Room's type : "))
  (defmessage v_i-cours-duree
    (french "Durée : ")
    (english "Length : "))
  (defmessage v_i-cours-nb-seances
    (french "Nombre de semaines : ")
    (english "Period : "))
  (defmessage v_i-cours-libelle
    (french "Libellé : ")
    (english "Title : "))
  (defmessage v_i-cours-code
    (french "Code : ")
    (english "ID : "))
  (defmessage v_le-cours
    (french "Le cours ")
    (english "Class "))
  (defmessage v_i-cours
    (french "Cours : ")
    (english "Class : "))
  (defmessage v_don
    (french "Données")
    (english "Data"))
  (defmessage v_outils
    (french "Outils")
    (english "Tools"))
  (defmessage v_edi
    (french "Editions")
    (english "???"))
  (defmessage v_mod
    (french "Mode")
    (english "Mode"))
  (defmessage v_car
    (french "Caractéristiques")
    (english "Parameters"))
  (defmessage v_con
    (french "Contraintes")
    (english "Constraints"))
  (defmessage v_aff
    (french "Afficher")
    (english "Display"))
  (defmessage v_edt
    (french "Emploi du Temps")
    (english "Time Table"))
  (defmessage v_dir
    (french "Fichier")
    (english "File"))
  (defmessage v_coc-effacer
    (french "Retirer les cours non figés")
    (english "Display fixed classes"))
  (defmessage v_coc-deplacer
    (french "Retirer un cours")
    (english "Retirer un cours"))
  (defmessage v_coc-placer
    (french "Placer un cours")
    (english "Placer un cours"))
  (defmessage v_coc-deplacer
    (french "Déplacer un cours")
    (english "Deplacer un cours"))
  (defmessage v_coc-defig
    (french "Défiger ")
    (english "Defiger "))
  (defmessage v_coc-fig
    (french "Figer ")
    (english "Fix "))
  (defmessage v_coc-meme
    (french "Même semaine")
    (english "same week"))
  (defmessage v_coc-prox
    (french "Proximité semaine")
    (english "Close order"))
  (defmessage v_coc-pre
    (french "Précédence")
    (english "Order"))
  (defmessage v_coc-non
    (french "Non-chevauchement")
    (english "Non-simultaneousness"))
  (defmessage v_coc-jour
    (french "Jours différents")
    (english "No same day"))
  (defmessage v_res-partiel
    (french "Résolution partielle")
    (english "???"))
  (defmessage v_aff-lig2
    (french "———————————")
    (english "———————————"))
  (defmessage v_aff-lig
    (french "—————————")
    (english "—————————"))
  (defmessage v_aff-lig3
    (french "——————————————")
    (english "——————————————"))
  (defmessage v_aff-lig4
    (french "——————————————————")
    (english "——————————————————"))
  (defmessage v_aff-lig5
    (french "———————————————————————")
    (english "———————————————————————"))
  (defmessage v_aff-pri
    (french "Imprimer")
    (english "Print"))
  (defmessage v_aff-pla
    (french "Annuel")
    (english "Planning"))
  (defmessage v_aff-sal
    (french "Salle")
    (english "Room"))
  (defmessage v_aff-pro
    (french "Professeur")
    (english "???"))
  (defmessage v_aff-eff
    (french "Effacer")
    (english "Clear"))
  (defmessage v_aff-rea
    (french "Réafficher")
    (english "Refresh"))
  (defmessage v_aff-jours
    (french "Jours")
    (english "Days"))
  (defmessage v_aff-semaines
    (french "Semaines")
    (english "Weeks"))
  (defmessage v_aff-partiel
    (french "Partiel")
    (english "???"))
  (defmessage v_aff-complet
    (french "Complet")
    (english "???"))
  (defmessage v_aff-pro
    (french "Professeur")
    (english "Professor"))

  (defmessage v_aff-cou
    (french "Cours")
    (english "Class"))

  (defmessage v_aff-resa
    (french "Réservation")
    (english "Reservation"))

  (defmessage v_outils-resa-consulter
    (french "Consulter une réservation")
    (english "Consultation"))

  (defmessage v_outils-resa-demander
    (french "Demander une réservation")
    (english ""))

  (defmessage v_outils-resa-config
    (french "Configurer les réservations")
    (english ""))
  
  (defmessage v_outils-resa-afficher
    (french "Afficher une réservation")
    (english ""))

  (defmessage v_aff-sem
    (french "Hebdomadaire")
    (english "Week"))
  (defmessage v_aff-semnum
    (french "de la semaine")
    (english "week "))
  (defmessage v_affichage
    (french "Affichage")
    (english "Edit of "))

  (defmessage v_aff-gro
    (french "Familles")
    (english "Groups"))

  (defmessage v_aff-sal1
    (french "pour la salle")
    (english "of room"))

  (defmessage v_aff-gro1
    (french "pour la famille")
    (english "of group"))

  (defmessage v_aff-pro1
    (french "pour le professeur")
    (english "???"))
  
  (defmessage v_don-gro
    (french "Familles")
    (english "Goups"))
  (defmessage v_don-sal
    (french "Salles")
    (english "Rooms"))
  (defmessage v_don-pro
    (french "Professeurs")
    (english "Professors"))
  (defmessage v_don-prof-sans-cours
    (french "Professeurs sans cours")
    (english "???"))
  (defmessage v_don-cou
    (french "Cours")
    (english "Classes"))
  (defmessage v_don-eco
    (french "Etablissement")
    (english "School"))
  (defmessage v_don-res
    (french "Réservations")
    (english "???"))
  (defmessage v_mod-analyse
    (french "Analyse")
    (english "Analyze"))
  (defmessage v_mod-resolution
    (french "Résolution")
    (english "Solver"))
  (defmessage v_mod-saisie
    (french "Saisie")
    (english "Enter"))

  (defmessage v_outils-resa
    (french "Réservations")
    (english "???"))
  (defmessage v_outils-edition
    (french "Editions")
    (english "???"))


  (defmessage v_edtsto
    (french "Interrompre")
    (english "Stop"))
  (defmessage v_edtana
    (french "Analyser")
    (english "Analyze"))
  (defmessage v_edtini
    (french "Initialiser")
    (english "Initialize"))
  (defmessage v_text
    (french "Texte")
    (english "Text"))
  (defmessage v_quit
    (french "Quitter Varuna ?")
    (english "Quit Varuna ?"))
  (defmessage v_dir-qui
    (french "Quitter")
    (english "Quit"))
  (defmessage v_apropos
    (french "A propos de Varuna")
    (english "About Varuna"))
  (defmessage v_dir-a-propos
    (french "A propos")
    (english "About"))
  (defmessage v_dir-sav-as
    (french "Enregistrer sous")
    (english "Save as"))
  (defmessage v_dir-sav
    (french "Enregistrer")
    (english "Save"))
  (defmessage v_dir-pri
    (french "Imprimer")
    (english "Print"))
  (defmessage v_dir-dir
    (french "Ouvrir")
    (english "Open"))
  (defmessage v_dir-nou
    (french "Nouveau")
    (english "New"))
  (defmessage v_calcul-salle
    (french "Affectation des salles")
    (english "Rooms"))
  (defmessage v_solv
    (french "résolution")
    (english "solver"))
  (defmessage v_cont
    (french "contraintes")
    (english "constraining"))
  (defmessage v_read
    (french "Lecture")
    (english "reading"))
  (defmessage v_prep
    (french "Préparation")
    (english "preparing"))
  (defmessage v_load
    (french "chargement")
    (english "loading"))
  (defmessage v_init
    (french "Initialisation")
    (english "initializing"))
  (defmessage v_saisie
    (french "saisie")
    (english "input"))
  (defmessage v_info-profs
    (french "professeur(s) : ")
    (english "professor(s) : "))
  (defmessage v_echelle
    (french "Echelle : ")
    (english "Scale : "))
  (defmessage v_echelle-pourcent
    (french "Echelle (%) ")
    (english "Scale : "))
  (defmessage v_info-en
    (french " en ")
    (english " in "))
  (defmessage v_info-le
    (french "le ")
    (english "???"))
  (defmessage v_info-de
    (french " de ")
    (english " from "))
  (defmessage v_info-a
    (french " à ")
    (english " to "))
  (defmessage v_info-semaines
    (french "semaines ")
    (english "weeks "))
  (defmessage v_info-semaine
    (french "semaine ")
    (english "weeks "))
  (defmessage v_invalid-dir
    (french "Répertoire invalide : ")
    (english "Invalid directory"))
  (defmessage v_donnees-utilisees-reseau
    (french "les données de ce répertoire sont en cours d'utilisation.")
    (english "???"))
  (defmessage v_effacer-donnees
    (french "ce répertoire contient déjà des fichiers de données.")
    (english "???"))
  (defmessage v_pas-de-donnees
    (french "ce répertoire ne contient pas de fichiers de données.")
    (english "???"))
  (defmessage v_pas-de-contrainte
    (french "Pas de contraintes pour les cours de fréquence multiple")
    (english "Pas de contraintes pour les cours de frequence multiple"))
  (defmessage v_confirm-contraintes-associees
    (french "(remarque : ce cours sera supprimé du jeu de contraintes)")
    (english "(remark : this class will be deleted from contraints "))
    (defmessage v_confirm-supression-cours
      (french "Etes-vous certain de vouloir supprimer le cours ")
      (english "Are you sure to delete the class "))
    (defmessage v_seuil-echec-atteint
      (french " échecs successifs, voulez-vous arrêter la résolution ?")
      (english " failures, do you want to stop the solver ?"))
    (defmessage v_unite-temps-fausse
      (french "Mauvaise unité de temps")
      (english "Wrong time unit"))
    (defmessage v_heure-pause-discordante-fin
      (french "Mauvaise heure de pause")
      (english "Wrong lunch hour"))
    (defmessage v_heure-pause-discordante-debut
      (french "Mauvaise heure de pause")
      (english "Wrong lunch hour"))
    (defmessage v_heure-fin-discordante
      (french "Mauvaise heure de fermeture")
      (english "Wrong close hour"))
    (defmessage v_heure-debut-egales
      (french "L'heure de début doit être la première heure de début de cours")
      (english "???"))
    (defmessage v_heure-debut-discordante
      (french "Mauvaise heure d'ouverture")
      (english "Wrong open hour"))
    (defmessage v_semaines-discordantes
      (french "La semaine de fin doit être supérieure à la semaine de début")
      (english "The last week must be greater than the first week"))

    (defmessage v_deplacer-au
      (french "Déplacer le cours au ")
      (english "Move the class to "))

    (defmessage v_placer-le-cours
      (french "Placer le cours")
      (english "???"))
    
    (defmessage v_deplacer-avec-la-salle
      (french "et lui affecter la salle:")
      (english "???"))

    (defmessage v_figer-semaine
      (french "figer jusqu'à la semaine ")
      (english "figer jusqu'à la semaine "))
    (defmessage v_figer-edt
      (french "figer l'emploi du temps ")
      (english "figer l'emploi du temps "))
    (defmessage v_figer-cours
      (french "figer les cours ")
      (english "figer les cours "))
    (defmessage v_plusieurs-reponses
      (french "Il y a plusieurs réponses pour ")
      (english "There is more than one answer for "))
    (defmessage v_le-chargement
      (french "le chargement de ")
      (english "loading "))
    (defmessage v_stop
      (french "STOP")
      (english "STOP"))
    (defmessage v_run
      (french "RUN")
      (english "RUN"))
    (defmessage v_titre-nyi
      (french "Message")
      (english "Message"))
    (defmessage v_ok
      (french "OK")
      (english "OK"))
    (defmessage v_add-comment
      (french "Titre ")
      (english "Title "))
    (defmessage v_semaines
      (french "Semaines :")
      (english "Weeks :"))
    (defmessage v_semaine
      (french "Semaine : ")
      (english "Weeek : "))
    (defmessage v_nyi
      (french "n'est pas présent dans cette version")
      (english "is not implemented yet"))
    (defmessage v_i-prof-existe-pas
      (french "Ce professeur n'est pas dans la base de données")
      (english "This professor is not in the data base."))
    (defmessage v_nas-pas-de-groupe
      (french " n'a pas de famille.")
      (english " has no group."))
    (defmessage v_na-pas-de-prof
      (french " n'a pas de professeur")
      (english " has no professor"))
    (defmessage v_le-cours
      (french "Le cours ")
      (english "Class "))
    (defmessage v_obligatoire
      (french " est obligatoire")
      (english " is needed"))
    (defmessage v_le-champ
      (french "Le champ  ")
      (english "The field "))
    (defmessage v_device-orient
      (french "Orientation : ")
      (english "Orientation : "))
    (defmessage v_device-nbcol
      (french "Nombre de feuilles verticales : ")
      (english "Number of strips verticals : "))
    (defmessage v_device-nbrow
      (french "Nombre de feuilles horizontales : ")
      (english "Number of horizontal strips : "))
    (defmessage v_war-inter
      (french " doit être dans l'intervalle ")
      (english " must be in the range "))
    (defmessage v_war-symbol
      (french " doit être un symbole ")
      (english " must be a symbol"))

    (defmessage v_war-num
      (french " doit être un nombres")
      (english " must be a number"))
    (defmessage v_warn-2cours
      (french "Vous devez choisir au moins deux cours")
      (english "You must select at least two classes"))
    (defmessage v_war-ide
      (french "Cours identiques !")
      (english "Same class !"))
    (defmessage v_pro-duree
      (french "Durée ")
      (english "Length"))
    (defmessage v_pro-heuref
      (french "heure de fin")
      (english "end hour"))
    (defmessage v_pro-heured
      (french "heure de début")
      (english "begin hour"))
    (defmessage v_pro-nbsem
      (french "nombre de semaines")
      (english "number of weeks"))
    (defmessage v_pro-prec
      (french " doit précéder ?")
      (english " must be before ?"))
    (defmessage v_pro-ms
      (french " même semaine que ?")
      (english " same week than ?"))
    (defmessage v_pro-sis
      (french " proche de ?")
      (english " close to ?"))
    (defmessage v_pro-nsc2
      (french " non-chevauchement entre ?")
      (english " not simultaneous with ?"))
    (defmessage v_pro-1co
      (french "Premier cours ?")
      (english "First class ?"))
    (defmessage v_pro-nchev
      (french "Non-chevauchement : quel cours ?")
      (english "Non-simultaneousness : which class ?"))
    (defmessage v_pro-sem
      (french "Quelle semaine ")
      (english "Which week ?"))
    (defmessage v_pro-prof
      (french "Quel professeur ?")
      (english "Which professor ?"))
    (defmessage v_pro-cou
      (french "Quel cours ?")
      (english "Which class ?"))
    (defmessage v_pro-cours
      (french "Quels cours ?")
      (english "Which classes ?"))
    (defmessage v_pro-jour
      (french "Quel jour ?")
      (english "Which day?"))
    (defmessage v_pro-jours
      (french "Quels jours ?")
      (english "Which days ?"))

    (defmessage v_i-groupe
      (french "Famille : ")
      (english "Group : "))
    (defmessage v_i-courss
      (french "Cours : ")
      (english "Classes : "))
    (defmessage v_pro-sals
      (french "Quelles salles ?")
      (english "Which rooms ?"))
    (defmessage v_pro-salle
      (french "Quelle salle ?")
      (english "???"))
    (defmessage v_i-cours-groupes
      (french "Famille(s) : ")
      (english "Groups : "))
    (defmessage v_i-cours-profs
      (french "Professeurs : ")
      (english "Professors : "))
    (defmessage v_i-cours-type
      (french "Type du cours : ")
      (english "Type of class : "))
    (defmessage v_i-prof-civilite
      (french "Civilité : ")
      (english "Titre : "))
    (defmessage v_i-prof-commentaires
      (french "Commentaires : ")
      (english "Comments : "))
    (defmessage v_i-salles
      (french "Salles : ")
      (english "Rooms : "))
    (defmessage v_ajouter
      (french "Ajouter")
      (english "Add"))
    (defmessage v_modifier
      (french "Modifier")
      (english "Edit"))
    (defmessage v_dir-device
      (french "Format d'impression")
      (english "Page layout"))
    (defmessage v_dir-print
      (french "Imprimer")
      (english "Print"))
    (defmessage v_quelle-contrainte
      (french "Quelle contrainte ?")
      (english "???"))
    (defmessage v_i-groupe-nb-seances
      (french "Nombre de semaines : ")
      (english "???"))
    (defmessage v_i-groupe-type-salle
      (french "Type de salle : ")
      (english "???"))
    (defmessage v_i-groupe-commentaires
      (french "Commentaires : ")
      (english "Comments : "))
    (defmessage v_i-groupe-profs
      (french "Professeurs : ")
      (english "???"))
    (defmessage v_i-groupe-audience
      (french "Audience : ")
      (english "???"))
    (defmessage v_i-groupe-durees
      (french "Durées : ")
      (english "???"))
    (defmessage v_i-groupe-nb-td
      (french "Nombre de groupes de TD/TP : ")
      (english "???"))
    (defmessage v_i-groupe-duree
      (french "Durée : ")
      (english "???"))
    (defmessage v_du-prof
      (french " du professeur ")
      (english " ??? "))
    (defmessage v_du-cours
      (french " du cours ")
      (english " ??? "))
    (defmessage v_du-groupe
      (french " de la famille ")
      (english " ??? "))
    (defmessage v_type-i-groupe
      (french "Type de la famille")
      (english "???"))
    (defmessage v_valeur
      (french "La valeur ")
      (english "The value "))
    (defmessage v_fermer
      (french "Fermer")
      (english "Close"))
    (defmessage v_ecrire
      (french "Ecriture des données")
      (english "Writing data"))
    (defmessage ecriture
      (french "Ecriture des données")
      (english "???"))
    (defmessage v_horaires 
      (french "Quels horaires ")
      (english "Times : "))
    (defmessage v_tout
      (french "Tous les cours : ")
      (english "All classes : "))
;;; AJT RM - 290695
    (defmessage v_i-cour-ss-salle
      (french "Tous les cours sans salle : ")
      (english "???"))
    (defmessage v_avec-commentaires
      (french "Ajouter les commentaires ?")
      (english "Add comments ?"))
    (defmessage v_a-partir-semaine
      (french "A partir d'une semaine : ")
      (english "From a specific weeek : "))
    (defmessage v_figer-salle
      (french "Voulez-vours figer aussi la salle ?")
      (english "???"))
    (defmessage v_figer-salles
      (french "Voulez-vous figer aussi les salles ?")
      (english "???"))
    (defmessage v_defiger-salle
      (french "Voulez-vous défiger aussi la salle ?")
      (english "???"))
    (defmessage v_defiger-tous-les-cours
      (french "Voulez-vous défiger tous les cours ?")
      (english "???"))
    (defmessage v_aucun-cours-figes
      (french "Aucun cours n'est figé.")
      (english "???"))  
    (defmessage v_dispo-incompatibles
      (french "Les disponiblités du cours sont incompatibles.")
      (english "???"))
    (defmessage v_dispo-prof-incompatibles
      (french "Les disponiblités du professeur sont incompatibles.")
      (english "???"))
    (defmessage v_dispo-groupe-incompatibles
      (french "Les disponiblités de la famille sont incompatibles.")
      (english "???"))
    (defmessage v_verifier-dispo
      (french "Verifiez les disponibilités des professeurs, des familles et de l'établissement.")
      (english "???"))
    (defmessage v_groupe-incomplet
      (french "Une famille doit contenir au moins un cours ou au moins une sous-famille.")
      (english "???"))
    (defmessage v_pas-cours-non-instancies
      (french "Tous les cours sont déjà placés.")
      (english "???"))
    (defmessage v_aucun-cours-modifiable
      (french "Tous les cours sont en lecture seule.")
      (english "???"))
    (defmessage v_premiere-semaine
      (french "De la semaine")
      (english "First week : "))
    (defmessage v_deuxieme-semaine
      (french "A la semaine")
      (english "Second week : "))
    (defmessage v_de-l-enseignement
      (french " de l'enseignement ")
      (english " ???"))
    (defmessage v_du-groupe-td
      (french " de la famille de TD/TP ")
      (english " ??? "))
    (defmessage v_horaire
      (french "Quel horaire ")
      (english "???"))
    (defmessage v_tous-les-cours
      (french "Voulez vous afficher tous les cours du jeu de données ?")
      (english "???"))
    (defmessage v_groupe-reste
      (french "Cours sans famille")
      (english "???"))
    (defmessage v_contrainte-existe
      (french "Cette contrainte est déjà définie")
      (english "???"))
    (defmessage v_defiger-toutes-les-salles
      (french "Voulez-vous défiger aussi les salles ?")
      (english "???"))
    (defmessage v_erreur-ecriture
      (french "Une erreur est survenue pendant l'ecriture du fichier ")
      (english "IO error while writting the file : "))
    (defmessage v_utiliser-sauvegarde
      (french "Utiliser la sauvegarde ?")
      (english "Use the backup file ?"))
    (defmessage v_cours
      (french "sur le cours:")
      (english "???"))
    (defmessage v_limitation-temporaire
      (french "Limitation temporaire")
      (english "???"))
    (defmessage v_suppression-cours-genere
      (french "Un cours généré par une famille ne peut pas être supprimé.")
      (english "???"))
    (defmessage v_supprimer-famille
      (french "Il faut supprimer la famille.")
      (english "???"))
    (defmessage errdispo-insuffis
      (french "La disponibilité du cours n'est pas suffisante.")
      (english "???"))
    (defmessage errdispo-groupe
      (french "Verifiez la disponibilité de ses familles.")
      (english "???"))
    (defmessage errdispo-prof
      (french "Verifiez la disponibilité de ses profs.")
      (english "???"))
    (defmessage errdispo-ecole
      (french "Verifiez la disponibilité de l'école.")
      (english "???"))
  ;; erreur dans la pose des contraintes
    (defmessage errinccontr
      (french "Pose de contrainte impossible: ")
      (english "???"))

;;; AJT RM - 250795 : Concept d'annulation de s‚ance
    (defmessage v_titre-saisie-i-annulation
      (french "Saisie d'une annulation de séance ")
      (english " ??? "))
    (defmessage v_i-annulation-code
      (french "Code : ")
      (english "ID : "))
    (defmessage v_i-annulation-libelle
      (french "Libellé : ")
      (english "Name : "))
    (defmessage v_i-annulation-code-cours
      (french "Code du cours : ")
      (english "???"))
    (defmessage v_i-annulation-libelle-cours
      (french "Cours : ")
      (english "???"))
    (defmessage v_i-annulation-semaine
      (french "Séance de la semaine : ")
      (english "???"))
    (defmessage v_i-annulation-duree
      (french "Durée : ")
      (english "Length : "))
    (defmessage v_i-annulation-rattrapage
      (french "Rattrapage de la séance : ")
      (english "???"))
    (defmessage v_i-annulation-commentaires
      (french "Commentaires : ")
      (english "Comments : "))
    (defmessage v_i-annulation
      (french "Annulation de séance : ")
      (english "???"))
    
  ;; FD - 24/07/95 - AJOUT de seance
  ;; messages associes a la fenetre d'ajout d'une seance a un cours
    (defmessage v_i-ajout-duree 
      (french "Durée: ")
      (english "???"))
    (defmessage v_i-ajout-semaine
      (french "Semaine: ")
      (english "???"))
    (defmessage v_i-ajout-jour
      (french "Jour: ")
      (english "???"))
    (defmessage v_i-ajout-horaire1
      (french "Créneau horaire entre: ")
      (english "???"))
    (defmessage v_i-ajout-horaire2
      (french "et: ")
      (english "???"))
    (defmessage v_i-ajout-audience
      (french "Audience: ")
      (english "???"))
    (defmessage v_i-ajout-type-salle
      (french "Type de salle: ")
      (english "???"))
    (defmessage v_i-ajout-contraintes-familles
      (french "Contraintes des familles: ")
      (english "???"))
    (defmessage v_i-ajout-dispo-cours-familles
      (french "Disponibilités du cours: ")
      (english "???"))
    (defmessage v_i-ajout-dispo-profs
      (french "Disponibilités des profs: ")
      (english "???"))
    (defmessage v_i-ajout-salles-disponibles
      (french "Liste des salles libres / Capacité / Créneaux")
      (english "???"))
    (defmessage v_i-ajout-libelle
      (french "Libellé: ")
      (english "???"))
    (defmessage v_i-ajout-code
      (french "Code: ")
      (english "???"))
    (defmessage v_i-ajout-salle
      (french "Salle: ")
      (english "???"))
    (defmessage v_i-ajout-heure-ajout
      (french "Heure de début: ")
      (english "???"))
    (defmessage v_titre-saisie-i-ajout
      (french "Saisie d'un ajout de séance - CREATION")
      (english "???"))
;;; AJT RM 270795
    (defmessage v_aucun-ajout
      (french "Aucun ajout de séance n'est défini.")
      (english "???"))
    (defmessage v_ajout-unique
      (french "Il n'existe qu'un seul ajout de séance défini.")
      (english "???"))
    (defmessage v_pro-ajout1
      (french "Quel ajout de séance ?")
      (english "???"))

;;; AJT RM 310795
    (defmessage v_aucune-annulation
      (french "Aucune annulation de séance n'est définie.")
      (english "???"))
    (defmessage v_outils-raco
      (french "Rattrapages de cours ")
      (english "???"))
    (defmessage v_outils-raco-demander-annulation
      (french "Demander une annulation de séance")
      (english "???"))
    (defmessage v_outils-raco-supprimer-annulation
      (french "Supprimer une annulation de séance")
      (english "???"))
    (defmessage v_outils-raco-demander-ajout
      (french "Demander un ajout de séance")
      (english "???"))
    (defmessage v_outils-raco-supprimer-ajout
      (french "Supprimer un ajout de séance")
      (english "???"))
    (defmessage v_outils-raco-afficher-ajout
      (french "Afficher un ajout de séance")
      (english "???"))
    (defmessage v_pro-ann
      (french "Quelle annulation de séance ?")
      (english "???"))
    (defmessage v_i-sup-annul
      (french "Supprimer l'annulation de séance")
      (english "???"))
    (defmessage v_pro-ajout1
      (french "Quel ajout de séance ?")
      (english "???"))
    (defmessage v_info-annuls
      (french "Séance(s) annulée(s) : ")
      (english "???"))
    (defmessage v_info-ajouts
      (french "Séance(s) ajoutée(s) : ")
      (english "???"))
;;; AJT RM 070895
    (defmessage v_i-ajout-profs
      (french "Professeurs :")
      (english "???"))
    (defmessage v_i-ajout-confirm
      (french "Voulez-vous effectuer l'ajout de séance :")
      (english "???"))
    (defmessage v_i-annul-confirm
      (french "Voulez-vous effectuer l'annulation de séance :")
      (english "???"))
    (defmessage v_i-sup-ajout
      (french "Supprimer l'ajout de séance")
      (english "???"))
    (defmessage v_aff-ajout
      (french "Ajout de séance")
      (english "???"))
    (defmessage v_i-ajt
      (french "Ajout de séance : ")
      (english "???"))
    (defmessage v_i-sup-annul-ajout-run1
      (french "La résolution va supprimer les annulations et les")
      (english "???"))
    (defmessage v_i-sup-annul-ajout-run2
      (french "ajouts de séances concernant les cours non figés.")
      (english "???"))
    (defmessage v_i-run
      (french "Voulez-vous relancer une résolution ?")
      (english "???"))

    (defmessage v_resolution-en-cours
      (french "Une résolution est en cours.")
      (english "????"))

    (defmessage v_acces-interdit
      (french "Il est impossible de charger VARUNA.")
      (english "????"))
    
    (defmessage v_acces-edt-interdit
      (french "Données en cours d'utilisation. Accès momentanement impossible.")
      (english "???"))

    (defmessage v_resolution-impossible
      (french "Résolution impossible car d'autres utilisateurs sont connectés.")
      (english "???"))
    (defmessage v_conf-imp
      (french "Oui et imprimer")
      (english "???"))
    (defmessage v_confirmer
      (french " Confirmer ")
      (english "???"))
    (defmessage v_msg-sup-annul-ajouts1
      (french "Ce deplacement entrainera la suppression")
      (english "????"))
    (defmessage v_msg-sup-annul-ajouts2
      (french "des annulations et ajouts de seances de")
      (english "???"))
    (defmessage v_msg-sup-annul-ajouts3
      (french "ce cours.")
      (english "????"))
    (defmessage v_dialogue_creer
      (french "Créer")
      (english "????"))
    (defmessage v_dialogue_tous
      (french "Tous")
      (english "????"))
    (defmessage v_defiger-cours-ss-salle
      (french "Voulez-vous défiger les cours sans salle ?")
      (english "????"))

    (defmessage v_aff-periode
      (french "Période")
      (english "???"))
    (defmessage v_aff-detail
      (french "Détail")
      (english "???"))
    (defmessage v_aff-chevauchement
      (french "Chevauchement")
      (english "???"))
    (defmessage v_aff-sans-chevauchement
      (french "Sans chevauchement")
      (english "???"))
    (defmessage v_aff-avec-chevauchement
      (french "Avec chevauchement")
      (english "???"))
    (defmessage v_aff-titre
      (french "Options d'affichage")
      (english "???"))
    (defmessage v_aff-option
      (french "Options")
      (english "???"))

    (defmessage v_aucun-cours
      (french "Aucun cours n'est défini.")
      (english "???"))
    (defmessage v_aucune-resa
      (french "Aucune réservation de salle n'est définie.")
      (english "???"))

    ;; 21/02/96
    ;; messages associes au choix d'une salle quand on place un cours
    ;; (fichier i-depl.ll)
    (defmessage v_choisir-salle-disponible
      (french "Choisissez une salle parmi celles disponibles :")
      (english "???"))
    
    ;; messages associes a la gestion du multiposte  - 23/02/96 - MultiP
    (defmessage v_titre-lecture-seule
      (french " - CONSULTATION")
      (english "???"))

    (defmessage v_lecture-seule
      (french "Donnée en lecture seule.")
      (english "???"))

    (defmessage v_comment-quitter
      (french "Quittez avec le bouton annuler.")
      (english "???"))

    (defmessage v_enregistrement-impossible
      (french "l'enregistrement n'est pas possible sur ce poste")
      (english "???"))
    
    (defmessage v_de-l-alternance
      (french " de la famille de cours alternés ")
      (english " ???"))

    (defmessage v_titre-saisie-i-groupe-alternance
      (french "Saisie d'une famille de cours alternés")
      (english "???"))

    (defmessage v_i-frequence
      (french "Périodicité : ")
      (english "???"))

    (defmessage v_titre-creation
      (french " - CREATION")
      (english "???"))

    (defmessage v_titre-modification
      (french " - MODIFICATION")
      (english "???"))

  (defmessage v_aucun-cours-sans-salle
    (french "Tous les cours ont une salle.")
    (english "???"))

  ;; AJT - FD - 04/07/96
  (defmessage v_aucun-cours-sans-prof
    (french "Tous les cours ont au moins un professeur.")
    (english "???"))

  (defmessage v_aucun-cours-type
    (french "Aucun cours de ce type n'est défini.")
    (english "???"))

  (defmessage v_resolution-imp-groupe
    (french "Résolution impossible car aucune famille n'est définie.")
    (english "???"))
  (defmessage v_resolution-imp-cours
    (french "Résolution impossible car aucun cours n'est défini.")
    (english "???"))

  (defmessage v_les-cours
    (french "Les cours ")
    (english "???"))
  (defmessage v_le-cours
    (french "Le cours ")
    (english "???"))
  (defmessage v_la-resa
    (french "la réservation ")
    (english "???"))
  (defmessage v_l-ajout
    (french "l'ajout de séance ")
    (english "???"))

  (defmessage v_chevauche
    (french "se chevauchent dans la même salle.")
    (english "???"))
  (defmessage v_varuna-cherche
    (french "Varuna va chercher une autre salle ")
    (english "???"))
  (defmessage v_pour-le-cours
    (french "pour le cours")
    (english "???"))

  (defmessage v_groupe-alternance-dispo-insuffisante
    (french "La disponibilité de la famille n'est pas suffisante")
    (english "???"))
  (defmessage v_cours-non-place
    (french "Le cours n'est pas placé")
    (english "???"))

  (defmessage v_err-non-chevauchement
    (french "NON CHEVAUCHEMENT")
    (english "???"))
  (defmessage v_err-jours-differents
    (french "JOURS DIFFERENTS")
    (english "???"))
  (defmessage v_err-proximite-semaine
    (french "PROXIMITE DE SEMAINES")
    (english "???"))
  (defmessage v_err-precedence
    (french "PRECEDENCE")
    (english "???"))
  (defmessage v_err-meme-semaine
    (french "MEME SEMAINE")
    (english "???"))
  (defmessage v_err-meme-jour
    (french "MEME JOUR")
    (english "???"))
  (defmessage v_err-meme-horaire
    (french "MEME HORAIRE")
    (english "???"))
  (defmessage v_err-simultaneite
    (french "SIMULTANEITE (DEBUT DE SEANCE)")
    (english "???"))
  (defmessage v_err-simultaneite-fin-seance
    (french "SIMULTANEITE (FIN DE SEANCE)")
    (english "???"))
  (defmessage v_err-k-ecart
    (french "K ALTERNANCE")
    (english "???"))
  (defmessage v_err-contrainte
    (french "AUTRE CONTRAINTE")
    (english "???"))

  (defmessage v_continuer-deplacement
    (french "Vous pouvez continuer le déplacement")
    (english "???"))
  (defmessage v_lever-contrainte-dispo
    (french "en ne tenant pas compte des disponibilités.")
    (english "???"))
  (defmessage v_lever-contraintes
    (french "en ne tenant pas compte des contraintes.")
    (english "???"))
  (defmessage v_lever-contrainte-salle
    (french "en n'affectant aucune salle au cours.")
    (english "???"))

  ;; AJT - FD - 25/07/96
  ;;  messages lies a la gestion de sites
  (defmessage v_type-interdit
    (french "Donnée non accessible depuis votre site")
    (english "Acces denied for this site"))
  (defmessage v_i-site-code
    (french "Code : ")
    (english "Code : "))
  (defmessage v_i-site-types-cours
    (french "Types de cours : ")
    (english "???"))
  (defmessage v_i-site-types-salles
    (french "Types de salle : ")
    (english "???"))
  (defmessage v_i-site-type
    (french "Type de site : ")
    (english "???"))
  (defmessage v_i-site-libelle
    (french "Libellé : ")
    (english "???"))
  (defmessage v_i-site-numero-dd
    (french "Numero DD : ")
    (english "???"))
  (defmessage v_sites
    (french "Sites ")
    (english "???"))
  (defmessage v_titre-saisie-i-site
    (french "Saisie d'un site")
    (english "???"))
  ;; messages lies a la gestion des semaines calendaires
  (defmessage v_i-calendaire-periodes
    (french "semaines : ")
    (english "???"))
  (defmessage v_i-calendaire-premiere-sem
    (french "1ère semaine : ")
    (english "???"))
    (defmessage v_sem-cal-debut
    (french "Semaine de début de période ")
    (english "???"))
  (defmessage v_sem-cal-fin
    (french "Semaine de fin de période ")
    (english "???"))
  (defmessage v_titre-saisie-i-calendaire
    (french "Semaines Calendaires")
    (english "???"))
  (defmessage v_semaines-calendaires
    (french "Semaines")
    (english "???"))
  (defmessage v_i-ecole-calendaire
    (french "Semaines calendaires")
    (english "???"))

  (defmessage v_device-couleur
    (french "Coloration   : ")
    (english "???"))
  (defmessage v_device-couleur-color
    (french "Couleurs")
    (english "???"))
  (defmessage v_device-couleur-grey
    (french "Niveaux de gris")
    (english "???"))
  (defmessage v_device-postscript
    (french "Impression : ")
    (english "???"))
  (defmessage v_device-ps-yes
    (french "Postscript")
    (english "???"))
  (defmessage v_device-ps-no
    (french "Non postscript")
    (english "???"))
  (defmessage v_ligne-commande
    (french "Ligne de commande :")
    (english "???"))
  (defmessage v_effacer-resa-ajouts
    (french "Les réservations et ajouts de séances ayant")
    (english "???"))
  (defmessage v_effacer-resa-ajouts2
    (french "lieu dans cette salle vont être supprimés.")
    (english "???"))
  (defmessage v_effacer-resa-ajouts3
    (french "Voulez-vous continuer la suppression ?")
    (english "???"))
  (defmessage v_cours-non-accessible-site
    (french "Aucun cours accessible depuis votre site.")
    (english "???"))
  (defmessage v_err-type-cours
    (french "Horaire incompatible avec le type de cours.")
    (english "???"))

  (defmessage v_liste-cours-chevauchement
    (french "Liste des cours en chevauchement avec le cours :")
    (english "???"))
  (defmessage v_selectionner-cours
    (french "Sélectionnez un cours dans la liste.")
    (english "???"))

  (defmessage v_cours-fige
    (french "Cours figé")
    (english "???"))
  (defmessage v_cours-defige
    (french "Cours non figé")
    (english "???"))
  (defmessage v_salle-fige
    (french "Salle figée")
    (english "???"))
  (defmessage v_salle-defige
    (french "Salle non figée")
    (english "???"))
  (defmessage v_selection-fige
    (french "Sélectionnez les critères : ")
    (english "???"))
  (defmessage v_supprimer-cours-type
    (french "supprimer tous les cours de type : ")
    (english "???"))
  (defmessage v_aucun-cours-place
    (french "Aucun cours n'est placé.")
    (english "???"))

  (defmessage v_res-totale
    (french "Résolution complète (RUN)")
    (english "???"))
  (defmessage v_res-cours-total
    (french "Résolution complète sans salles")
    (english "???"))
  (defmessage v_res-cours-partiel
    (french "Résolution partielle sans salles")
    (english "???"))
  (defmessage v_res-salle
    (french "Affectation des salles")
    (english "???"))
  (defmessage v_aff-echelle
    (french "Echelle")
    (english "???"))
  (defmessage v_aff-reduire
    (french "Zoom arrière")
    (english "???"))
  (defmessage v_aff-agrandir
    (french "Zoom avant")
    (english "???"))
  (defmessage v_aff-ecran
    (french "Plein écran")
    (english "???"))
  (defmessage v_aff-pourcent
    (french "Pourcentage du zoom")
    (english "???"))
  (defmessage v_aff-sem-prec
    (french "Semaine précédente")
    (english "???"))
  (defmessage v_aff-sem-suiv
    (french "Semaine suivante")
    (english "???"))
  (defmessage v_don-tous
    (french "Tous")
    (english "???"))
  (defmessage v_coc-desaffecter
    (french "Retirer les salles non figées")
    (english "???"))
  (defmessage v_resolution-ok
    (french "La résolution est terminée.")
    (english "???"))
  (defmessage v_affectation-salles-ok
    (french "L'affectation des salles est terminée.")
    (english "???"))
  (defmessage v_analyse-dispo
    (french "Analyse des disponibilités")
    (english "???"))
  (defmessage v_analyse-contraintes
    (french "Analyse des contraintes")
    (english "???"))

  (defmessage v_resolution
    (french "Résolution  ")
    (english "???"))
  (defmessage v_gestion
    (french "Gestion")
    (english "???"))
  (defmessage v_analyse-dispo-ok
    (french "Aucune anomalie détectée durant l'analyse des disponibilités.")
    (english "???"))
  (defmessage v_analyse-contr-ok
    (french "Aucune anomalie détectée durant l'analyse des contraintes.")
    (english "???"))

  (defmessage v_pause
    (french "Pause")
    (english "???"))
  (defmessage v_ancienne-pause
    (french "La pause a pour durée (en minutes) : ")
    (english "???"))
  (defmessage v_valeur-pause
    (french "Nouvelle durée de la pause")
    (english "???"))
  (defmessage v_multiple
    (french "doit être un multiple de ")
    (english "???"))
  (defmessage v_last-error
    (french "Dernier message d'erreur")
    (english "???"))
  (defmessage v_no-error
    (french "Aucune erreur n'est apparue.")
    (english "???"))

  (defmessage v_stats
    (french "Statistiques")
    (english "???"))
  (defmessage v_outils-permut
    (french "Permutation de séances")
    (english "???"))
  (defmessage v_permut-cours1
    (french "Premier cours intervenant dans la permutation :")
    (english "???"))
  (defmessage v_permut-cours2
    (french "Deuxième cours intervenant dans la permutation :")
    (english "???"))
  (defmessage v_permut-sem1
    (french "Semaine du premier cours")
    (english "???"))
  (defmessage v_permut-sem2
    (french "Semaine du deuxième cours")
    (english "???"))
  (defmessage v_permut-ok
    (french "La permutation de séances est terminée.")
    (english "???"))
  (defmessage v_creer-site
    (french "Créer un site")
    (english "Create a site"))
  (defmessage v_modif-site
    (french "Modifier un site")
    (english "Change a site"))
  (defmessage v_Suppr-site
    (french "Supprimer un site")
    (english "Delete a site"))

  (defmessage v_toutes-contraintes
    (french "Toutes les contraintes")
    (english "???"))
  (defmessage v_aucune-contrainte
    (french "Aucune contrainte")
    (english "???"))
  (defmessage v_niveau-contraintes
    (french "Prendre en compte les contraintes avec")
    (english "???"))
  (defmessage v_niveau-contraintes2
    (french "les ajouts de séances des cours figés :")
    (english "???"))

  (defmessage v_outils-manip
    (french "Traitement de données")
    (english "???"))
  (defmessage v_outils-manip-2standard
    (french "Transformer en famille standard")
    (english "???"))
  (defmessage v_outils-manip-raz-dispo-cours
    (french "Disponibilité complète des cours")
    (english "???"))
  (defmessage v_outils-manip-raz-dispo-prof
    (french "Disponibilité complète des professeurs")
    (english "???"))
  (defmessage v_outils-manip-raz-dispo-famille
    (french "Disponibilité complète des familles")
    (english "???"))
  (defmessage v_outils-manip-sup-resas
    (french "Supprimer toutes les réservations de salles")
    (english "???"))
  (defmessage v_outils-manip-sup-annuls
    (french "Supprimer toutes les annulations de séances")
    (english "???"))
  (defmessage v_outils-manip-sup-ajouts
    (french "Supprimer tous les ajouts de séances")
    (english "???"))
  (defmessage v_outils-manip-sup-liens-prof
    (french "Supprimer les liens enseignants / cours")
    (english "???"))

  (defmessage v_changer-type-fam
    (french "De quelle famille voulez-vous changer le type ?")
    (english "???"))
  (defmessage v_toutes-fam-standard
    (french "Toutes les familles sont de type standard ou alternance.")
    (english "???"))
  (defmessage v_manip-sous-fam
    (french "Effectuer le traitement sur les sous-familles ?")
    (english "???"))
  
  (defmessage v_traitement-termine
    (french "Traitement des données terminé.")
    (english "???"))
  (defmessage v_suppressions-effectuees
    (french "Suppressions effectuées.")
    (english "???"))

  (defmessage v_imp-une-semaine
    (french "Imprimer la semaine courante")
    (english "???"))
  (defmessage v_imp-plusieurs-semaines
    (french "Imprimer plusieurs semaines")
    (english "???"))
    
  (defmessage v_outils-ferie
    (french "Gestion des jours fériés")
    (english "???"))

  (defmessage v_annuler-journee
    (french "Annuler une journée")
    (english "???"))

  (defmessage v_dep_journee
    (french "Déplacer tous les cours d'une journée")
    (english "???"))

  (defmessage v_dep-cours-journee
    (french "Déplacer certains cours d'une journée")
    (english "???"))

  (defmessage v_sup-annul-journee
    (french "Supprimer les annulations d'une journée")
    (english "???"))

  (defmessage v_sup-ajt-journee
    (french "Supprimer les ajouts d'une journée")
    (english "???"))
  
  (defmessage v_outils-imp-ecran
    (french "Grille affichée")
    (english "???"))
  (defmessage v_outils-imp-salles
    (french "Liste de salles")
    (english "???"))
  (defmessage v_outils-imp-profs
    (french "Liste de professeurs")
    (english "???"))

  (defmessage Info-Cycle
    (french "La modélisation contient un cycle formé par les familles suivantes :")
    (english "???"))

  (defmessage v_don-cours-souhait
    (french "Souhaits de salle non respectés")
    (english "???"))
    
  (defmessage v_souhaits-salle-ok
    (french "Tous les souhaits de salle sont respectés.")
    (english "???"))

  (defmessage v_conf-resa
    (french "Nombre de séance(s) :")
    (english ""))

  (defmessage v_resa-simple
    (french "Une séance")
    (english ""))

  (defmessage v_resa-multiple
    (french "Plusieurs séances")
    (english ""))
    
  )
