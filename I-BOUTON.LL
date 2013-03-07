;;; --------------------------------------------------------------------------
;;; - I-Bouton.ll                                               VARUNA 1.2   -
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; - Interface Graphique                                                    -
;;; - Bibliotheque de boutons                                                -
;;; --------------------------------------------------------------------------
;;; - Creation:  Janvier 94                                                  -
;;; - Mise a jour pour version 1.1:                                          -
;;; - Build 1.22.1 01.07.99 / JH MO                                          -
;;; --------------------------------------------------------------------------
;;;
;;; Un certain nombre de boutons dits "standard" sont repertories dans cette
;;; bibliotheque. 
;;; rem (23/09/95) MO : cette bibliotheque est reservee aux boutons standards
;;; ou aux fonctions generiques de ceux-ci
;;; c'est donc une biblio pouvant servir a d'autres logiciels que varuna
;;; pour les boutons varuna, on devrait avoir un autre fichier...

;;;  I. Les boutons de reference:
;;;  ----------------------------------------
;;;		
;;;      <bouton-olivier>  -> ne sert plus dans la v 1.12
;;;      <notre-bouton> a venir -> rem MO (23/09/95) remplace par
;;;                               bouton-standard ??? ne sert plus v 1.12
;;;      <bouton-standard> RM



;;;  <bouton-olivier>
; (defun bouton-olivier (image foreground)
;        (let ((but (standardbutton image)))
;             (widget-set-args but
;                     'foreground (make-named-color foreground)
;                     'background (make-named-color "grey")
;                     'font time8b)
;             but))

; (defun notre-bouton-standard (message)
; (standardbutton (font arial10b (get-message message))))



; (defun bouton-centre (chaine)
;   (let
;     ((w (send 'width chaine))
;      (h (send 'height chaine)))
;     (standardbutton (centeredimage 0 0 w h chaine))))
    



;;;  CRE RM - 25/07/95
;;;  Boutons pour l'annulation et l'ajout de seances
;;;  On cree une fonction generique de creation de bouton
;;;  dans laquelle on passe en parametres une string et une fonction

;;;  rem (23/09/95) MO
;;;  il faudrait avoir des accesseurs ressources sur ce bouton
;;;  d'autre part, je pense que l'on peut lui envoyer une image quelc. ????

(defun bouton-standard (chaine fonction)
  (let 
    ((b (standardbutton chaine)))
    (set-action b fonction)
    b))  


;;; rem (23/09/95) MO
;;;  -> fichier specifique varuna ????

;; un bouton d'ajout dans un dialogue, on passe la fonction en parametre
;; le bouton met a jour le champ answer du dialogue avec
;; le resultat de cette fonction

(defun bouton-ajout-dialogue (fonction-ajout)
  (let
    ((b
      (standardbutton
	(font FONT-EDITEUR
	  (cadrer-centrer #Mv_dialogue_creer 10)))))
    (set-action
      b
      (lambda (b)
	(let*
	  ((dialogue (component 'asker b))
	   (objet (funcall fonction-ajout))
	   (lo
	     (when objet (cons objet (send 'get-answers dialogue)))))
	  (when objet
	    ({asker}:set-answers dialogue lo)
	    ({asker}:ok-action dialogue)))))
    b))


;;; rem (23/09/95) MO
;;;  -> fichier specifique varuna ????

(defun bouton-tous-dialogue ()
  (let
    ((b
      (standardbutton
	(font FONT-EDITEUR
	  (cadrer-centrer #Mv_dialogue_tous 10)))))
    (set-action b (lambda (b)
      (let* ((dialogue (component 'asker b)))
	({asker}:set-answers dialogue (list 'tous))
	({asker}:ok-action dialogue))))
    b))


;;; ce bouton OK a pour action de retirer le composant de nom 'appli
;;; par exemple pour fermer un fenetre sans aucune autre action
;;; si le composant appli n'est pas present
;;; la fenetre bouton est supprimee (ca ne sert a rien)

(defun standard-ok-button (n)
  (let* ((lib (ajouter-espace #Mv_ok n n))
    (b (standardbutton lib)))
      (set-action b
        #'(lambda (b)
          (if (componentp 'appli b)
            (remove-application (component 'appli b))
            (remove-application b))))
      b))


;;; ce bouton change de nom et d'action a chaque fois qu'il est active
;;; Par exemple un bouton Commencer/Arreter
;;; image1p est un booleen qui indique l'image <et donc l'action>
;;; qui apparait en premier ordre d'affichage

(defun bouton-double-action (image1 action1 image2 action2 image1p)
  (let ((but (standardbutton (if image1p image1 image2))))
    ({application}:userdata but
      (cons (cons image2 action1)
        (list (cons image1 action2))))
    (when (not image1p)
      ({application}:userdata but (reverse ({application}:userdata but))))
    (set-action but
      #'(lambda (b)
        (let
	  ((image (caar ({application}:userdata b)))
	   (action (cdar ({application}:userdata b))))
	  ;; AJT - 27/02/96 - test du retour de l'action
	  (when (funcall action b) 
	    (send 'new-image b (buttonbox 0 0 (send 'w b) (send 'h b) image))
	    (send 'full-redisplay b)
	    ({application}:userdata b (reverse ({application}:userdata b)))))))
      (send 'resize but (+ 10 (max (send 'width image1)
	(send 'width image2))) ())
    but))



;;; cette routine permet de grouper des boutons 
;;; c'est a dire, les rendre elastiques et assure qu'ils sont
;;; toujours de la meme taille
;;; intervalle est un intervalle entre les boutons
;;; sens : vertical (t) ou horizontal (null)
;;; liste-libelles-actions est une liste de listes a deux elements
;;; le premier est le libelle du bouton 
;;; le deuxiement est un symbole ou une lambda exp 
;;; qui devient l'action du bouton

(defun boutons-groupes (intervalle sens . liste-libelles-actions)
  (let ((taille-bouton (max-string-len (mapcar 'car liste-libelles-actions))))
    (apply (if sens 'elasticrow 'elasticcolumn)
      (mapcar (lambda (libelle-action) 
        (let* ((libelle (car libelle-action))
               (action1 (caadr libelle-action))
               (action2 (cadadr libelle-action))
               (b (standardbutton (cadrer-centrer libelle taille-bouton))))
              ({application}:userdata b action2)
;;; l'action 2 doit etre une lambda, elle se trouve dans
;;; le champ userdata du bouton
              (set-action b action1)
              (if (equal libelle-action (car liste-libelles-actions))
                  b
                  (translation
                    (if sens intervalle 0)
                    (if sens 0 intervalle) b))))
        liste-libelles-actions))))


;;; rem (23/09/95) MO
;;;  -> fichier specifique varuna ????
;;; rem (02/06/95) mo !!!
;;; et pourquoi des chaine de texte dans le code ????

;;; un exemple du boutons groupes
;;; les deux boutons OK et Annuler avec les actions habituelles
;;; sauver-editeur et annuler-editeur


(defun ok-annuler-editeur (func)
  (let 
    ((liste-actions-ok
      (if LECTURE-SEULE?;; 22/02/96 FD - MultiP
	(list 'sauver-editeur 'acces-lecture-seule)
	(list 'sauver-editeur func)))
     (liste-actions-annuler (list 'annuler-editeur)))

    (boutons-groupes #2wchar t 
        (list "OK" liste-actions-ok)
        (list "Annuler" liste-actions-annuler))))

;;; CRE - FD - 22/02/96
(defun acces-lecture-seule (objet)
  (and
    (afficher-attention (column #Mv_lecture-seule #Mv_comment-quitter))
    ()))

;;; <bouton-rechercher>
;;; bouton de l'application reservation
;;; bouton lancant la recherche de toutes les salles disponibles 
;;; avec leur dispo apres verification que les champs necessaires
;;; a la recherche ont bien ete saisis et qu'ils contiennent des 
;;; valeurs correctes et que les champs facultatifs ont bien des
;;; valeurs correctes
;;;
;;; FD - 24/07/95 - MOD ajout de la fonction associee au bouton 
;;;                     comme parametre de la focntion
;;;
;;; rq FD, pour rendre ce bouton plus general, il faut passer en 
;;; argument la liste des champs necessaires a la recherche

(defun bouton-rechercher (fonction-recherche)
  (let 
    ((b (standardbutton #Mv_rechercher)))
    (set-action b fonction-recherche)
    ;; rem (02/06/95) mo: a terme, j'aurai aime avoir ce type de de code 
    ;;(set-action b #Gva_rechercher)
    b))



;;; LE BOUTON RUN/STOP
;;;
;;; le bouton run/stop est un bouton qui peut avoir deux images diffe'rentes
;;; avec des ressources elles-memes diffe'rentes
;;; on a donc de'fini deux images, le bouton en lui-me^me et deux primitives
;;; de changement d'image
;;;
;;; rem (20/03/95) MO
;;; a terme, on pourra inserer des ressources de boutons 
;;;
;;; rem (08/06/95) Mo
;;; je transfere ce code de i-appli a i-bouton
;;; et remarque qu'il y a de grandes similitudes entre bouton-double-action
;;; et bouton-run-stop ... a suivre


;;; l'image du bouton run est une chaine centree contenant le message #Mv_run

(defun image_bouton_run ()
  (with 
    ((current-font time8b))
    (buttonbox 0 0 72 32 (cadrer-centrer #Mv_run 12))))

;;; l'image du bouton stop est une chaine centree contenant le message #Mv_stop
  
(defun image_bouton_stop ()
  (with 
    ((current-font time8b))
    (buttonbox 0 0 72 32 (cadrer-centrer #Mv_stop 10))))

;;; un bouton de type run/stop

(defun bouton-run-stop (flag)
  (let
    ((bouton  (button (if flag (image_bouton_run) (image_bouton_stop)))))
    (widget-set-args bouton
      'font time8b
      'foreground (make-named-color (if flag "blue" "red"))
      'background (make-named-color "grey"))
    bouton))


;;; les deux methodes de changement d'image stop -> run et run -> stop
;;;  AJT - FD - 31/08/95 - traitement du curseur avec <change-curseur>

(defun bouton-run2stop (bouton)
  (change-curseur bouton Varuna-Curseur-Sablier)
  (send 'new-image bouton (image_bouton_stop))
  (widget-set-args bouton
    'foreground (make-named-color "red")
    'background (make-named-color "grey")))


(defun bouton-stop2run (bouton)
  (send 'new-image bouton (image_bouton_run))
  (widget-set-args bouton
    'foreground (make-named-color "blue")
    'background (make-named-color "grey"))
  (change-curseur bouton Varuna-Curseur-Fleche))

