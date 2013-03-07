;;; --------------------------------------------------------------------------
;;; - Patch.ll                                                  VARUNA 1.2   -
;;; - patch corrigeant le bug des menubars - amen‚ a disparaitre            -
;;; - Copyright Abstract, France 1993-2006                                   -
;;; - under licence MIT Expat since 2006                                     -
;;; - Build 1.22.1 01.07.99                                                  -
;;; --------------------------------------------------------------------------

(setq #:sys-package:colon '#:display:ps)

(defun :pssegments (display n vx1 vy1 vx2 vy2)
       ;; draws a number of unconnected segments
       ;; --------------------------------------
       (:psprin display :newpath :s)
       (let ((i 0))
            (repeat
                    n
                    (:psprint display
                            (+ .5 (vref vx1 i))
                            (:pstransy (+ .5 (vref vy1 i)) display) :moveto :s)
                    (:psprint display
                            (+ .5 (vref vx2 i))
                            (:pstransy (+ .5 (vref vy2 i)) display) :lineto :s)
                    (setq i (add1 i))))
       (:stroke display))


(defun aidamenubar-lowlight-item (appli item)
  (with 
    ((current-window ({application}:window appli)))
    (with 
      ((current-foreground (current-background ))
	(current-mode #:MODE:set ))
      (let* 
	((x (send 'ix1 item))
	 (y (send 'iy1 item))
	 (w (sub1 (sub (send 'ix2 item) x)))
	 (h (sub1 (sub (send 'iy2 item) y))))
	(draw-rectangle
	  (add x 3)
	  (add y 3)
	  (sub w 6)
	  (sub h 6))))))

(defun aidamenubar-highlight-item (appli item)
  (with 
    ((current-window ({application}:window appli)))
    (with 
      ((current-mode #:MODE:set ))
      (let* 
	((x (send 'ix1 item))
	 (y (send 'iy1 item))
	 (w (sub1 (sub (send 'ix2 item) x)))
	 (h (sub1 (sub (send 'iy2 item) y))))
	(draw-rectangle
	  (add x 3)
	  (add y 3)
	  (sub w 6)
	  (sub h 6))
	(when 
	  (color-displayp)
	  (with 
	    ((current-foreground 
	      (llresource 'background menu.clear)))
	    (draw-line 
	      (add x 3) 
	      (add y 3)
	      (add x 3)
	      (sub (send 'iy2 item) 5))
	    (draw-line
	      (add x 3)
	      (add y 3)
	      (sub (send 'ix2 item) 5)
	      (add y 3))))))))

					  

