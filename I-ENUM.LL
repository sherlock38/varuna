;;; --------------------------------------------------------------------------
;;; - I-enum.ll                                                 VARUNA 1.2   -
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; - Interface Graphique                                                    -
;;; - Bibliotheque d'editeur: editeur de types enumeres                      -
;;; --------------------------------------------------------------------------
;;; - Creation:  Mars 93                                                     -
;;; - Mise a jour pour version 1.07: Decembre 93                             -
;;; - Build 1.22.1 01.07.99 / JH MO                                          -
;;; --------------------------------------------------------------------------

;;; TRV - FD - 19/03/96
;;; ajout du champ :valide-p contenant un predicat verifiant la validiter
;;; du type saisie

;;; - FD-27/04/95 AJT de l'envoie du message <ascii-event> avec<post-event>  -
;;; - a l'pplication qui contiendra un editeur de type enumere.              -
;;; - Cette focntion a ete ajoute pour que lorsque l'on clique dans un       -
;;; - editeur de type enumere on puisse dans la fenetre de reservation effacer
;;; - la liste des salles disponibles (rq la fonction d'effacement des salles-
;;; - disponibles est declenchee lorsque l'application contenant les champs  -
;;; - de la demande de reservation recoit le message <ascii-event> qui ne    -
;;; - declenche habituellement aucune reaction de la parts d'une applicaton  -
;;; - quand elle le recoit                                                   -
;;; - FD - 12/04/95 ajout de la focntion <current-keyboard-focus-window>     -
;;; - et de <:i-next> pour pouvoir gerer le focus dans les editeurs de type  -
;;; - enumere                                                                -


;;; un enumere est un champ dont les valeurs appartiennent a une liste

(defmessage i-aucun 
  (french "Aucun")
  (english "None"))

(defmessage i-autre
  (french "Autres")
  (english "Others"))

(defmessage v_quelle-valeur
  (french "Quelle valeur ?")
  (english "Select a value :"))

(defvar MaxEnum 25)                     ; nombre d'item de menu max
                                        ; car un menu ne scrolle pas

(eval-when (load eval local-compile)
  (defabbrev enum {application}:enum))

(eval-when (load eval local-compile)
  (defstruct {enum} 
    valeur
    accesseur
    saisissable
    obligatoire
    stringp
    reste
    valide-p
    codep))

(in-package '{enum})

(defun enum (sym accesseur enum stringp saisissable obligatoire valide-p codep)
  (:initialise-structure
    (new '{enum})
    sym accesseur enum stringp
    saisissable obligatoire valide-p codep))

;;; 16 est la taille suposee de la fleche 
;;; du menu <!j'ai pas trouve la globale>
;;; le cons "" sert a masquer la valeur selectionnee dans l'enum
;;; le cons #Mi-aucun (= troit traits) sert a selectionner une valeur ()
;;; lorsque l'editeur n'est pas modifiable (au clavier)

(defun :initialise-structure
  (editeur sym accesseur enum strp saisissable obligatoire valide-p codep)
  (when (and codep sym)
    (setq sym (creer-code-chaine sym)))
  (with ((current-font FONT-EDITEUR))
    (let*
      ((zone-texte (stringedit 0 0 0 (string sym)))
	(taille-ecran (bitymax))
	(enum1
	  (if (and (not obligatoire) (not saisissable))
	    (cons
	      (font FONT-EDITEUR #Mi-aucun)
	      (encapsuler-font-liste enum FONT-EDITEUR))
	    (encapsuler-font-liste enum FONT-EDITEUR)))
	(enum2
	  (if (ge (length enum1) MaxEnum)
	    (cons (font FONT-EDITEUR #Mi-autre) enum1)
	    enum1))
       (liste-choix
	 (optionmenubutton 0 0 16 (send 'height zone-texte)
	   (cons "" (firstn MaxEnum enum2))))
       (view (constrainedview zone-texte liste-choix)))

      (send 'update-constraint view zone-texte (stretchconstraint 1 1 1 1))
      (send 'update-constraint view liste-choix (stretchconstraint -1 1 1 1))
      (send 'set-selected-image liste-choix "")
      (:stringp editeur strp)
      (when (eq (length liste-choix) 1)
        (send 'set-line zone (car liste-choix)))
      (add-component editeur 'zone zone-texte)
      (add-component editeur 'choix liste-choix)
      ({application}:initialise-structure
        editeur
        0
        0
        (send 'width liste-choix) (send 'height zone-texte) view)
      (when (ge (length enum2) MaxEnum)
        (:reste editeur                 ; en pratique il faudrait n'afficher
          (lastn                        ; dans le menu que les MaxEnum
            ; (add 4 (sub (length enum2) MaxEnum)) FD - 16/08/95
            (sub (length enum2) MaxEnum)
            enum2)))                    ; premieres valeurs
                                        ; au risque de paraitre ridicule
                                        ; lorsque la taille de l'ecran
                                        ; est plus grande que la taille
                                        ; normale
      (add-component editeur 'user editeur)
      (add-component editeur 'editeur zone-texte) ;;; FD-11/04/95
      (set-action zone-texte
	#'(lambda (str-ed) (send 'i-next (component 'user str-ed))))
      (set-action editeur #'(lambda (ed) (send 'i-next ed)))
      (:accesseur editeur accesseur)
      (:valeur editeur
	(if (and sym codep)
	  (string2symbol (extraire-code sym))
	  sym))
      (:obligatoire editeur obligatoire)
      (:saisissable editeur saisissable) ;; FD-02/05/95
      (:valide-p editeur valide-p)
      (:codep editeur codep)
      (when (not saisissable)
        (send 'inhibit-application zone-texte))
      (when (and (eq (length enum) 1) obligatoire)
        (send 'set-line zone-texte (car enum))) 
      (set-action liste-choix
        (lambda (appli)
          (let* ((zone (component 'zone appli))
            (image0 (send 'get-selected-image appli))
            (image1
              (if (equal image0 (font FONT-EDITEUR #Mi-autre))
                (send 'demande-reste (component 'user appli))
                image0))
            (image
	      (if (equal image1 (font FONT-EDITEUR #Mi-aucun))
		(font FONT-EDITEUR "")
		image1)))
            
            ;; FD-27/04/95
            (post-event 'ascii-event
                ({application}:father ({application}:father appli))
                0)
          (when image
	    (if (stringp image)
	      (send 'set-line zone (string image))
	      (send 'set-line zone (string (send 'image image))))) 
          (send 'set-selected-image appli ""))))
      editeur)))

;;; <demande-reste>
;;; MOD - FD - 16/08/95 <demandes-simples-type> -> <demande-simple-type>
 
(defun :demande-reste (enum)
  ;; FD pourquoi ne pas avoir utiliser <demande-simple-type> ?
  ;; (car
  ;;    (demandes-simples-type #Mv_quelle-valeur (:reste enum) ())))
  (demande-simple-type #Mv_quelle-valeur
    (desencapsuler-font-liste (:reste enum)) ()))

;;; FD - 12/05/95
;;; le focus est donne au <stringedit> 
(defun :current-keyboard-focus-window (ed)
  (current-keyboard-focus-window
      (send 'window (component 'editeur ed)))
)

;;; FD-02/05/95, permet de savoir si on peut donner le controle du clavier
;;; a cette editeur
;;; est utilise pour la gestion du focus
(defun :readonly-flag (ed)
  (not (:saisissable ed)))

(defun :i-valider (ed)
  (let
    ((val (send 'get-line (component 'zone ed)))
     (erreur))
    (when (:codep ed)
      (setq val (extraire-code val)))
    (setq erreur 
      (or
	(and
	  (:valide-p ed)
	  (not (funcall (:valide-p ed) val))
	  (traiter-erreur))
        (and
          (:obligatoire ed)
          (nullstrp val)
          (alerte (funcall (:accesseur ed) ())))
        (and
          (not (nullstrp val))
	  (not (:stringp ed))
          (not (est-un-symbole val))
          (afficher-erreur
            (column
              #Mv_smust-be-symb
              (funcall (:accesseur ed) ()))))))
    (not erreur)))

(defun :i-next (ed)
  (current-keyboard-focus-window
    (send 'window 
      (if (send 'i-valider ed) 
        (next-user-appli ed 'user) 
        ed))))

(defun :i-get-value (editeur)
  (let* ((selection (send 'get-line (component 'zone editeur)))
    (filtre-selection (if (equal selection #Mi-aucun) "" selection)))
  (if (:stringp editeur)
    (string filtre-selection)
    (symbol () filtre-selection))))
  

(defun :i-sauver (ed)
  (let
    ((sym (:i-get-value ed))
     (objet-edite ({application}:userdata (component 'appli ed))))
    (when (:codep ed)
      (setq sym (string2symbol (extraire-code sym))))
    (and objet-edite
      (:accesseur ed)
      (funcall (:accesseur ed) objet-edite sym))))


(defun :i-restore (ed)
  (let ((objet-edite ({application}:userdata (component 'appli ed))))
    (and objet-edite
      (:accesseur ed)
      (funcall (:accesseur ed) objet-edite (:valeur ed)))))

;;; puisque'un stringedit ne gere par un champ saisissable
;;; on overwrite le ascii event de l'appli 
;;; cette petite bidouille permet de s'en sortir

(defun :ascii-event (appli event)
  (when
    (:saisissable appli)
    ({application}:ascii-event appli event)))

;;;  CRE RM - 05/09/96 : extraction du code dans une chaine de type
;;;  code - libelle

(defun extraire-code (chaine)
  (if (and chaine (not (equal chaine "")))
    (substring chaine 0 (chrpos #/  chaine))
    chaine))

;;;  CRE RM - 05/09/96 : construction d'une chaine code - libelle

(defun creer-code-chaine (sym)
  (ifn sym
    sym
    (let
      ((objet (eval sym))
	(code)
	(libelle))
      (setq code (send 'code objet))
      (setq libelle (send 'libelle objet))
      (catenate (string code) " - " (string libelle)))))
