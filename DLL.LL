;;; --------------------------------------------------------------------------
;;; - dll.ll                                                    VARUNA 1.2   -
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; --------------------------------------------------------------------------
;;; - Build 1.22.1 01.07.99 / MO                                             -
;;; --------------------------------------------------------------------------

;;; Definition des fonctions externes presentes dans les DLLs

(with   
  ((current-directory (string (getenv "VARDLL"))))
  (if
    (and
      (probefile "varuna.dll")
      (probefile "transac.dll")
      (probefile "chkfile.dll"))
    (progn

      (defextern mygale "varuna.dll" (string) fix)
      (defextern vexecute "varuna.dll" (string string) fix)
      (defextern creer_repertoire "varuna.dll" (string) fix)
      (defextern semaine_reelle "varuna.dll" (fix string) fix)
      (defextern afficher_date "varuna.dll" () fix)
      
      (defextern checkvaruna "chkfile.dll" () fix)
      (defextern checkfiles "chkfile.dll" (string string) fix)
      
      (defextern repertoire_donnees "transac.dll" (string) fix)
      (defextern debut_transaction "transac.dll" (string) fix)
      (defextern fin_transaction "transac.dll" (string) fix)
      (defextern nettoyer_transfile "transac.dll" (string) fix)
      (defextern debut_modif "transac.dll" (string string fix) fix)
      (defextern fin_modif "transac.dll" (string string) fix)
      (defextern recupere_site "transac.dll" () fix)
      (defextern etat_objet "transac.dll" (string string) fix)
      (defextern etat_courant_objet "transac.dll" (string string) fix)
      (defextern interdire_acces "transac.dll" (string fix) fix)
      (defextern interdire_acces_attente "transac.dll" (string fix) fix)      
      (defextern autoriser_acces "transac.dll" (string fix) fix)
      (defextern nb_utilisateurs "transac.dll" () fix)
      (defextern repertoire_tmp "transac.dll" (string) fix)
      (defextern etat_trace "transac.dll" (fix) fix)
      (defextern maj_fichier_trace "transac.dll" () fix)
      (defextern lecture_seule "transac.dll" (string) fix))
    
    (progn
      (grab-warn
	(column
	  "Les DLLs ne sont pas toutes presentes"
	  (row "dans le repertoire " (current-directory))))
      (end))))
