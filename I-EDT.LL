;;; --------------------------------------------------------------------------
;;; - I-Edt.ll                                                  VARUNA 1.2   -
;;; - Interface Graphique                                                    -
;;; - Appel aux routines de resolution                                       -
;;; --------------------------------------------------------------------------
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; - Creation:  Mars 93                                                     -
;;; - Mise a jour pour version 1.07: Decembre 93                             -
;;; - Build 1.22.1 01.07.99 / JH MO                                          -
;;; --------------------------------------------------------------------------
;;; TRV - FD - !!!! permet de savoir qu'on est dans un tag

(defvar dans-tag ())

;;; rem MO (23/09/95) toujours d'actualite
;;; XXX BUG 5 (mai 95)
;;; combien de variables designant les etats dans lesquels on peut se trouver
;;; dirty, i-running, stop-running
;;; dirty -> les donnees sont sales ou propres (ie: il faut les enregistrer
;;; avant de quitter varuna)
;;; i-running: ???
;;; stop-running: ??? 


;;; appelle (resoudre) si on est pas deja en train de resoudre, sinon
;;; on stope

;;; DBG (13/03/95) MO
;;; je mets le ...if l-codes... au debut pour eviter d'enregistrer les donnees
;;; pour rien en cas de resolution partielle (non presente dans cette version
;;; je mets un (when dirty...) pour les memes raisons: on n'a pas besoin de 
;;; copier les donnees si elles sont propres ....
;;; ATTENTION: la bonne specif est de ne pas resoudre si cela a deja ete fait
;;; et que les donnees sont propres .... a voir .... une glob differente

;;; !!! (15/03/95) MO
;;; mise-a-jour-thermometre -> ne prend pas () comme argument -> explose

;;; MOD 29/06/95 FD traitement de la resolution partielle
;;; MOD - FD - 05/09/95 - traitement reseau

;;; MOD RM - 05/12/96 : on separe les deux phases de resolution pour
;;; les appeler de differentrs menus (placement des cours et affectation
;;; des salles

;; Resolution de l'ensemble des cours (instanciation des cours)

(defun resolution-cours (a)
  (setq compter-contraintes-p t)
  (edt-run a () t ()))

;; Affectation des salles (sur l'ensemble des cours instancies)
;; L'affectation si au moins un cours est instancie

(defun affectation-salles (a)
  (if (any 'i-instanciep Liste-i-Cours)
    (edt-run a () () t)
    (afficher-erreur #Mv_aucun-cours-place)))

;; Resolution complete

(defun resolution-complete (a)
  (setq compter-contraintes-p t)
  (edt-run a () t t))

;; Declenchement de la resolution
;; 4 parametres :
;;    - bouton
;;    - liste de cours (() = tous les cours)
;;    - booleen pour l'instanciation des cours
;;    - booleen pour l'affectation des salles

(defun edt-run (a l-cours coursp sallesp) 
  (if I-Running
    (edt-stop ())
    (if (and MODULE-RESEAU (not (resolution-p)))
      (afficher-attention #Mv_resolution-impossible)
      (when (declencher-resolutionp coursp)
	(when MODULE-RESEAU (interdire-acces RESOLUTION-SEM))
	(begin-run)
	(preparer-donnees-resolution coursp sallesp)
	(ecrire-donnees-interface)
	(aff-refresh Gen-App)
	(setq I-Running t)
	(delete-erreur)
	(change-curseur a Varuna-Curseur-Sablier)
	(when coursp
	  (tag fin-resolution
	    (setq dans-tag t)
	    (resolution-moteur l-cours)
	    (setq dans-tag ()))
	  (mise-a-jour-thermometre 0 100))
	(when (and sallesp (not *erreurs*))
	  (tag fin-resolution
	    (setq dans-tag t)
	    (when (and MODULE-SALLES (not Stop-Running))
	      (resolution-salles))
	    (setq dans-tag ())))
	(setq Stop-Running ())
	(end-run)
	(lire-fichier FICHIER-COURS 'interface)
	(when sallesp
	  (affecter-salles-cours))
	(when (not *erreurs*)
	  (when (and MODULE-SALLES sallesp) (controle-resultat-salles))
	  (when coursp (controle-resultat-instanciation l-cours)))
	(mise-a-jour-thermometre 0 100)
	(donner-titre ())
	(aff-refresh Gen-App)
	(if *erreurs*
	  (interface-erreur)
	  (if coursp
	    (afficher-information #Mv_resolution-ok)
	    (afficher-information #Mv_affectation-salles-ok)))))))
  
;;; la, c'est sur, on stope
;;; rem FD : que se passe t-il si on clique sur le bouton stop losqu'on ait pas
;;; dans le tag

(defun edt-stop (a)
  (end-run)
  (setq Stop-Running t)
  (donner-titre "interrompu")
  ;;TRV - FD 25/09/95 
  (if dans-tag
    (exit fin-resolution (cons-erreur 'arret-resolution ()))))

;;; gestion de l'image du bouton RUN/STOP,

(defun begin-run ()
  (let 
    ((b (component 'bouton Gen-App)))
    (bouton-run2stop b)))

;;; MOD - FD - 05/09/95 - traitement reseau RESOLUTION-SEM

(defun end-run ()
  (let 
    ((b (component 'bouton Gen-App)))
    (setq I-Running ())
    (bouton-stop2run b)
    (when MODULE-RESEAU
      (autoriser-acces RESOLUTION-SEM))))


;;; verifie que les cours ont ete instancies, construit une erreur sinon
;;; MOD - FD - 04/07/95
;;;  le controle du resultat de l'instanciation se fait sur la liste des
;;;  cours qui est passee en parametre et non plus sur la liste de tous les
;;;  cours (ou sur tous les cours si cette liste est vide pour garder une
;;;  uniformite avec la resolution)

(defun controle-resultat-instanciation (liste-cours-a-resoudre)
  (let*
    ((cours-a-resoudre
      (ifn liste-cours-a-resoudre
	Liste-I-Cours
	liste-cours-a-resoudre))
     (non-instancies (sous-liste i-pas-instanciep cours-a-resoudre)))
    (when non-instancies
      (cons-erreur 'errNoDate non-instancies))))

;;; verifie que les cours ont une salle, construit une erreur sinon

(defun controle-resultat-salles ()
  (let
    ((non-places (sous-liste i-pas-de-salle-trouvee Liste-I-Cours)))
    (when non-places
      (cons-erreur 'errNoRoom non-places))))

;;; preparation des donnees

(defun preparer-donnees-resolution (coursp sallesp)
  (mapc
    (lambda (c)
      (when (and coursp (not (fige-i-cours c)))
	(instant-trouve-i-cours c ()))
      (when (and (or coursp sallesp) (not (salle-figee-reelle-i-cours c)))
	(salle-trouvee-i-cours c ())))
    Liste-I-Cours))

;;; affectation des salles au cours (code -> objet)

(defun affecter-salles-cours ()
  (mapc
    (lambda (cours)
      (when
	(and
	  (salle-trouvee-i-cours cours)
	  (boundp (salle-trouvee-i-cours cours)))
	(salle-trouvee-i-cours cours
	  (eval (salle-trouvee-i-cours cours)))))
    liste-I-Cours))

