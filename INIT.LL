;;; --------------------------------------------------------------------------
;;; - Init.ll                                                   VARUNA 1.2   -
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; - Moteur Varuna                                                          -
;;; - Module d'initialisation du moteur                                      -
;;; --------------------------------------------------------------------------
;;; - Creation:  Mars 93                                                     -
;;; - Mise a jour pour version 1.07: Decembre 93                             -
;;; - Build 1.22.1 01.07.99 / JH MO                                          -
;;; --------------------------------------------------------------------------

;;; Le init global, a charger a partir du prompt lisp.
;;; Copyright Abstract, France 1993-1996

(load "\usr\ilog\pecos\loadpeco.ll")
(load "\usr\ilog\lescript\loadps.ll")
(loadmodule 'sets)
(loadmodule 'stringio)
(loadmodule 'hash)
(printlevel 4)

(defvar cp t)

(if cp
  (progn
    (print "Le Frontal/Lanceur")
    (loadmodule 'frontal)
    )
  (load "utilit.lu")
  (load "moteur.lu")
  (load "salles.lu")
  (load "inter.lu")
  (load "comm.ll")
  (load "frontal.ll")
  )

(defvar REP-DONNEES 
  (or (string (getenv "VARDATA")) (current-directory)))

(defvar REP-DLL 
  (catenate
    (or (string (getenv "VARUNA")) (current-directory))
    "\"
    "dll\"))

(defvar REP-VARUNA
  (or (string (getenv "VARUNA")) (current-directory)))

(current-directory REP-DONNEES)


(with
  ((current-directory REP-VARUNA))
  (if
    (probefile "dll.ll") 
    (load "dll.ll")				; dlls
    (print "ATTENTION: le fichier dll.ll n'est pas present"))
  (if
    (probefile "patch.ll")
    (load "patch.ll")				; patch postcript
    (print "ATTENTION: le fichier patch.ll n'est pas present")))
  
(inibitmap 'ps)

