;;; --------------------------------------------------------------------------
;;; - I-Msgres.ll                                               VARUNA 1.2   -
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; - Interface Graphique                                                    -
;;; -  Messages necessaires pour le module de reservation                                                                       -
;;; --------------------------------------------------------------------------
;;; - Copyright by Abstract & P. Riviere - 1993-1996                         -
;;; - Creation: Mars 1995 (FD)                                               -
;;; - Mise a jour pour version                                               -
;;; - Build 1.22.1 01.07.99 / MO FD                                          -
;;; --------------------------------------------------------------------------

;;; a mettre dans I-Don.ll
(defun liste-libelle-salle ()
  (mapcar
    (lambda (sa)
      (string (libelle-i-salle sa)))
    Liste-I-Salle))

;;; a mettre dans I-Msg.ll
(defmessage v_titre-saisie-i-resa
  (french "Saisie d'une r�servation de salle - CREATION")
  (english "????"))

(defmessage v_rechercher
  (french "Rechercher")
  (english "Find"))

(defmessage v_choisir
  (french "Choisir")
  (english "?????"))


(defmessage v_i-resa-libelle
  (french "Libell�: ")
  (english "???"))

(defmessage v_i-resa-code
  (french "Code: ")
  (english "???"))

(defmessage v_i-resa-duree
  (french "Dur�e: ")
  (english "???"))


(defmessage v_i-resa-audience
  (french "Audience: ")
  (english "???"))


(defmessage v_i-resa-semaine
  (french "Semaine: ")
  (english "???"))


(defmessage v_i-resa-jour
  (french "Jour: ")
  (english "???"))


(defmessage v_i-resa-salle
  (french "Salle: ")
  (english "???"))


(defmessage v_i-resa-type-salle
  (french "Type de salle: ")
  (english "???"))

(defmessage v_i-resa-salles-disponibles
  (french "Liste des salles libres / Capacit� / Cr�neaux")
  (english "???"))

(defmessage v_i-resa-type-cours
  (french "Type de cours: ")
  (english "???"))

(defmessage v_i-resa-horaire1
  (french "Cr�neau horaire entre: ")
  (english "???"))

(defmessage v_i-resa-horaire2
  (french "et: ")
  (english "???"))

(defmessage v_i-resa-salle-resa
  (french "Salle choisie: ")
  (english "???"))

(defmessage v_i-resa-heure-resa
  (french "Heure de d�but: ")
  (english "???"))

(defmessage v_i-titre1
  (french " DEMANDE")
  (english "????"))

(defmessage v_i-oblig
  (french "Obligatoire")
  (english "????"))

(defmessage v_i-facult
  (french "Facultatif")
  (english "????"))

(defmessage v_i-titre2
  (french " RECHERCHE")
  (english "????"))

(defmessage v_i-titre3
  (french " RESERVATION")
  (english "????"))

(defmessage err-capa-aud
  (french "incompatibilit� entre la capacit� de la salle et l'audience")
  (english "err-capa-aud"))

(defmessage err-resa-salle
  (french "pas de salle correspondant � la demande")
  (english "no room"))

(defmessage err-type-salle
  (french "incompatibilit� entre le type de salle et l'audience")
  (english "????"))

(defmessage v_aucune-resa
  (french "Il n'y a pas de r�servations")
  (english "??????"))

(defmessage v_pro-resa
  (french "Quelles r�servations ?")
  (english "??????"))

(defmessage v_pro-resa1
  (french "Quelle r�servation ?")
  (english "??????"))

(defmessage v_i-resa
  (french "R�servation : ")
  (english "?????"))

(defmessage v_i-sup-resa
  (french "Supprimer la r�servation")
  (english "?????"))

(defmessage v_aff-resas
  (french "R�servations")
  (english "??????"))

(defmessage v_resa-confirm
  (french "Voulez-vous effectuer la r�servation: ")
  (english "?????"))

(defmessage v_pourle
  (french "pour le ")
  (english "?????"))

(defmessage v_de
  (french " de ")
  (english "???"))

(defmessage v_a
  (french " � ")
  (english "???"))

(defmessage v_delasem
  (french "de la semaine ")
  (english "???????"))

(defmessage v_alasem
  (french " � la semaine ")
  (english "???????"))

(defmessage v_ensalle
  (french "en salle: ")
  (english "????"))
