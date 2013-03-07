;;; --------------------------------------------------------------------------
;;; - I-dispo.ll                                                VARUNA 1.2   -
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; - Interface Graphique                                                    -
;;; - Bibliotheque d'editeur: editeur de chaine de caracteres                -
;;; --------------------------------------------------------------------------
;;; - Creation:  Mars 93                                                     -
;;; - Mise a jour pour version 1.07: Decembre 93                             -
;;; - Build 1.22.1 01.07.99 / JH MO                                          -
;;; --------------------------------------------------------------------------

;;; ! utilise la notion de disponibilites definie "ailleurs"
;;; utilise l'objet detect pour la fenetre associee

(defmessage v_def-dispo
  (french "Horaires disponibles ")
  (english "??? "))

(defmessage v_titre-saisie-dispo
  (french "Saisie des disponibilités")
  (english "???"))

(defmessage v_disponibilites
  (french "Disponibilités")
  (english " ??? "))

(defmessage v_disponibilites-invalides
  (french "Disponibilités non valides")
  (english "???"))

;;; dispo est une sous-classe de application

(eval-when (load eval local-compile)
  (defabbrev dispo {application}:dispo))

(eval-when (load eval local-compile)
  (defstruct {dispo} 
    valeur
    edi-dispo-semaine
    edi-dispo-moment
    accesseur
    message
    accesseur-libelle))

(in-package '{dispo})

(defun i-dispo (dispo accesseur message accesseur-lib)
  (:initialise-structure (new '{dispo}) dispo accesseur message accesseur-lib))

(defun :initialise-structure (editeur dispo accesseur message accesseur-libelle)
  (let*
    ((label (font arial8b #Mv_disponibilites))
     (the-box (box 0 0 (send 'width label) (send 'height label)))
     (b (button (elasticcenteredview the-box label)))
     (grille-semaine (funcall 'grille-semaines-dispo))
     (grille-planning (funcall 'grille-planning-dispo))
     (les-heures (send 'heures grille-semaine))
     (fin (send 'semaines grille-planning))
     (debut 1)
     (les-jours (mapcar 'jour-libelle JOURS-OUVRABLES)))
          
    (send-super '{dispo} 'initialise-structure editeur 0 0
      (send 'width b) (send 'height b) b)
    (:message editeur message)
    (:accesseur-libelle editeur accesseur-libelle)
    (:valeur editeur dispo)
    (:accesseur editeur accesseur)
    (:edi-dispo-semaine editeur
      (detector 0 0 (- 635 20) 50 grille-planning ()))
    (:edi-dispo-moment editeur
      (detector 0 0 (- 635 20) (- 470 150) grille-semaine ()))
    (send 'zones2rectangle
      (:edi-dispo-moment editeur)
      (funcall 'expanser-dispo-moment
	(funcall 'trouver-dispo-moment dispo) les-heures les-jours)
      les-heures)
    (send 'zones2rectangle
      (:edi-dispo-semaine editeur)
      (funcall 'expanser-dispo-semaine
	(funcall 'trouver-dispo-semaine dispo) debut fin)
      ())
    (set-action b
      #'(lambda (b) (send 'edit-dispo (component 'editeur b))))
    (add-component b 'editeur editeur)
    (add-component editeur 'user editeur)
    editeur))


(defun :editeur-dispo (ed)
  (let*
    ((ob ({application}:userdata (component 'appli ed)))
     (objet ({application}:userdata (component 'appli ed)))
     (libelle (funcall (:accesseur-libelle ed) objet))
     (titre (translation 15 15 (font time8b (ajouter-espace
       (catenate #Mv_def-dispo (:message ed) " " (or libelle "")) 3 0))))
     (height (mul 2 (send 'height titre)))
     (pat (boxedview (filledbox 10 10 50 height DISPO-PAT)))
     (b-ok (standardbutton (ajouter-espace #Mv_OK 3 3)))
     (appli          
       (application '{application} 0 0 640 470
	 (column
	   (row pat titre)
	   (:edi-dispo-moment ed)
	   (:edi-dispo-semaine ed)
	   (translation 10 10 (view b-ok))))))
    
    (set-action b-ok (lambda (b)
      (remove-application (component 'appli-dispo b))))
    (add-component appli 'appli-dispo appli)
    (add-component appli 'appli-dispo-moment (:edi-dispo-moment ed))
    (add-component appli 'appli-dispo-semaine (:edi-dispo-semaine ed))
    (send 'set-title appli #Mv_titre-saisie-dispo)
    (send 'fit-to-window appli)
    appli))

                

(defun :edit-dispo (ed)
  (grab-and-select-application (:editeur-dispo ed)))

(defun :get-dispo (ed)
  (let*
    ((grille-semaine (funcall 'grille-semaines-dispo))
     (grille-planning (funcall 'grille-planning-dispo))
     (heures (send 'heures grille-semaine))
     (fin (send 'semaines grille-planning))
     (debut 1)
     (liste-sem (liste-nombres-inclus debut fin 1 1))
     (zones-sem (send 'rectangles2zones (:edi-dispo-semaine ed) liste-sem))
     (zones-mom  (send 'rectangles2zones (:edi-dispo-moment ed) heures)))
    (concatener-i-dispo zones-sem zones-mom)))


(defun :i-valider (ed)
  (let
    ((la-dispo (:get-dispo ed)))
    (if (dispop la-dispo)
      t
      (not
	(afficher-erreur
          #Mv_disponibilites-invalides)))))

(defun :i-next (ed)
  t)

;;; FD-02/05/95, on ne donne jamais le controle du clavier a ce type
;;; d'appli
(defun :readonly-flag (ed)
  t)

(defun :i-sauver (ed)
  (let
    ((objet-edite ({application}:userdata (component 'appli ed))))
    (and objet-edite
      (:accesseur ed)
      (funcall (:accesseur ed) objet-edite (:get-dispo ed)))))

(defun :i-restore (ed)
  (let
    ((objet-edite ({application}:userdata (component 'appli ed))))
    (and objet-edite
      (:accesseur ed)
      (funcall (:accesseur ed) objet-edite (:valeur ed)))))

