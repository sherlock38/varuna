;;; --------------------------------------------------------------------------
;;; - I-Gene.ll                                                 VARUNA 1.2   -
;;; - Interface Graphique                                                    -
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; - Routines Générales dont "Main"                                         -
;;; --------------------------------------------------------------------------
;;; - Creation:    Mars 93                                                   -
;;; - Mise a jour pour version 1.13:   Juin 95                               -
;;; - Build 1.22.1 01.07.99 / JH MO FD                                       -
;;; --------------------------------------------------------------------------

;;; Initialisation de l'inferface de Varuna

(defun interface-varuna (rep nouveaup)
  (let
    ((status t))
    (if nouveaup
      (generer-interface)
      (if rep
	(setq status (check-files-p rep SITE))
	(setq status (check-files-p (catenate REP-DONNEES "\\") SITE)))
      (when status
	(generer-interface)))
    status))

(defun generer-interface ()
  (let
    ((thermo (mon-thermo 200 20 2))
     (app-init))
    (setq app-init (add-and-select-application (translation 200 200 thermo)))
    (add-component app-init 'th thermo)
    (maj-thermo (component 'th app-init) 0)
    (send 'set-title app-init App-Name)
    (make-ttyapp)			; fabrique le terminal de sortie
    (init-dialog)			; initialise deux constantes 
    (rmargin 1024)			; definition d'une marge
    (initialiser-lecture-seule)		; initialisation du mode d'acces
    (initialiser-trace)			; initialisation du mode TRACE
    (traiter-donnees app-init)		; ouvre ou cree un jeu de donnees
    (init-globales-etablissement)	; Analyse de ECOLE.EDT 
    (initialiser-semaines-calendaires)	; initialise les semianes calendaires
    (initialiser-affichage)
    (init-application-resolution)
    (init-application-saisie)
    (maj-thermo (component 'th app-init) 100)    
    (environnement-selon-acces-autorises)
    (maj-items-menus-selon-environnement)  
    (remove-application app-init)
    (aiguillage-application)
    (when MODULE-RESEAU
      (debut-transaction SITE))
    (setq *last-error* ())
    (setq compter-contraintes-p ())
    (setq compteur-contraintes-posees 0.)
    (maj-site-connecte)))		; initialisation du site connecte

;;; Verification de l'etat d'un jeu de donnees

(defun check-files-p (rep site)
  (let
    ((status))
    (interdire-acces VARUNA-SEM)	; on lock la semaphore VARUNA.SEM
    (setq status (funcall 'checkfiles rep site))
    (autoriser-acces VARUNA-SEM)
    (and (eq status 0) t)))

;;; Initialisation des variables de l'etablissement du jeu de donnees

(defun init-globales-etablissement ()
  (with ((current-directory REP-DONNEES))
    (when (probefile FICHIER-ECOLE)
      (preinit)
      (calculer-globales-ecole))))

;;; l'aiguillage au debut vers gen-app ou input-app
;;; AJT RM - 12/02/98 : cycle -> on reste en saisie au lancement !

(defun aiguillage-application ()
  (let
    ((cycle (graphe-cyclique)))
    (cond
      (*erreurs*
	(mod-sai Gen-App)
	(interface-erreur))
      (cycle
	(mod-sai Gen-App)	
	(erreur-graphe-cyclique cycle))
      ((and Liste-I-Cours (probleme-resolu-p))
	(mod-res Gen-App))
      (t (mod-sai Gen-App)))))

;;; initialisation de l'application Gen-App

(defun init-application-resolution ()
  (setq Gen-App (application-resolution))
  (update-grid-titles)
  (make-resolution-submenus)
  (donner-titre ()))

;;; initialisation de l'application Input-App

(defun init-application-saisie ()
  (setq Input-App (application-saisie))
  (make-input-submenus))

;;; Initialisation du jeu de donnees (lecture)

(defun traiter-donnees (app-init)
  (ifn (probefile (catenate (current-directory) FICHIER-ECOLE))
    (creer-nouvelles-donnees app-init)
    (traiter-donnees-existantes app-init)))

;;; Fonction appelee lors de la creation d'un nouveau jeu de donnees
;;; On garde le libelle de l'ecole lors de la creation

(defun creer-nouvelles-donnees (app-init)
  (let
    ((ancien-libelle (libelle-i-ecole I-Ecole)))
    (maj-thermo (component 'th app-init) 10)
    (free-interface)
    (maj-thermo (component 'th app-init) 20)
    (ecole-defaut)
    (maj-thermo (component 'th app-init) 50)    
    (libelle-ecole-i-ecole I-Ecole ancien-libelle)
    (libelle-i-ecole I-Ecole ancien-libelle)
    (maj-thermo (component 'th app-init) 70)        
    (ecrire-fichier FICHIER-ECOLE 'interface 'ecole ())
    (maj-thermo (component 'th app-init) 80)        
    (lire-fichier FICHIER-ECOLE 'interface)
    (maj-thermo (component 'th app-init) 90)    
    (ecrire-donnees-interface)
    (cons-erreur 'errEcole ())))

;;; Fonction appelee a l'ouverture d'un jeu de donnees existant

(defun traiter-donnees-existantes (app-init)
  (sauver-donnees)			; creation des fichiers .SAV
  (lire-donnees-interface		; lecture des donnees
    (lambda (message pourcent) 
      (when message
	(send 'set-title app-init (get-message message)))
      (maj-thermo (component 'th app-init) pourcent)))
  (delete-erreur)
  (when (not I-Ecole)
    (ecole-defaut)
    (cons-erreur 'errEcole ()))
  (when (not LECTURE-SEULE?)
    (ecrire-fichier FICHIER-ECOLE 'interface 'ecole ()))
  (lire-fichier FICHIER-ECOLE 'inter)
  (initialiser-prefixe-code-interdit))

;;; Creation des fichiers .SAV

(defun sauver-donnees ()
  (when (not LECTURE-SEULE?)		; pas de sauvegarde si LECTURE SEULE
    (let
      ((liste-fichiers
	(list FICHIER-ECOLE FICHIER-COURS FICHIER-PROFS FICHIER-SALLES
	  FICHIER-GROUPES FICHIER-CONTRAINTES)))
      (mapc
	(lambda (fichier)
	  (sauver-fichier-si-besoin fichier (nom-sauvegarde fichier EXT-SAV)))
	liste-fichiers)

      (when MODULE-RESA
	(sauver-fichier-si-besoin FICHIER-RESA
	  (nom-sauvegarde FICHIER-RESA EXT-SAV)))
      
      (when MODULE-RACO
	(sauver-fichier-si-besoin FICHIER-ANNUL
	  (nom-sauvegarde FICHIER-ANNUL EXT-SAV))
	(sauver-fichier-si-besoin FICHIER-AJOUT
	  (nom-sauvegarde FICHIER-AJOUT EXT-SAV))))))

;;; On effectue la sauvegarde si le fichier existe et qu'il n'est pas vide

(defun sauver-fichier-si-besoin (fichier-source fichier-cible)
  (when 
    (and
      (probefile fichier-source)
      (not (fichier-vide-p fichier-source)))
    (copyfile fichier-source fichier-cible)))

;;; <initialiser-prefixe-interdit>
;;; CRE - FD - 01/03/96 - MultiP
;;; intialisation du PREFIXE-INTERDIT, tout les objets dont le code commence
;;; par ce prefixe ne pourront pas etre modifi‚ mais uniquement consultes
;;; doit etre appele a chaque ouverture d'un jeu de donnee
;;; rq: cette initialisation se fait a partir du REPERTOIRE COURANT

(defun initialiser-prefixe-code-interdit ()
  (if (probefile FICHIER-PREFIXE-INTERDIT)
    (with
      ((inchan (openi FICHIER-PREFIXE-INTERDIT)))
      (setq PREFIXE-INTERDIT (string (read)))
      (close))
    (setq PREFIXE-INTERDIT ())))
      
;;; <initialiser-semaines-calendaires> CRE - FD - 01/08/96
;;; initalise l'objet SEM-CALENDAIRES contenant la liste des semaines
;;; calendaires et la semaine de debut des cours

(defun initialiser-semaines-calendaires ()
  (let
    ((objet-calendaire (nouveau-i-calendaire))
     (code (gencode-i-calendaire)))
    (code-i-calendaire objet-calendaire code)
    (libelle-i-calendaire objet-calendaire "")
    (set code objet-calendaire)
    (setq SEM-CALENDAIRES objet-calendaire)
    (periodes-i-calendaire SEM-CALENDAIRES
      (periodes-calendaires-i-ecole I-ECOLE))
    (premiere-sem-i-calendaire SEM-CALENDAIRES
      (borne-inf-intervalle (car (periodes-calendaires-i-ecole I-ECOLE))))))
