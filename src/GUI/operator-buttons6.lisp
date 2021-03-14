;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: GUI; Base: 10 -*-

(in-package gui)

(defconstant +ink-arg-object+ +black+)

(defconstant +ink-res-object+ +red+)

(defconstant +operator-button-text-style+
    (make-text-style
     :sans-serif nil :very-small))

(defclass operator-button (button)
  ((draw-function :accessor draw-function
		  :initform #'(lambda (button stream cell-size) 
				(declare (ignore button stream cell-size))
				nil)
		  :initarg :draw-function)
   (redraw-when :accessor redraw-when :initform #'no :initarg :redraw-when)
   (command :accessor command :initform nil :initarg :command)
   (scale-fac :accessor scale-fac :initarg :scale-fac :initform 0.8)  
   (when-selected :initform #'choose-operator)))

;;;
;;;
;;;

(defmethod initialize-instance :after ((obj operator-button) &rest initargs)
  (declare (ignore initargs))
  (pushend obj *operator-buttons*))

(defun make-operator-button (draw-function &rest args)
  (apply #'make-instance 'operator-button :draw-function draw-function args))

(defun initialize-operator-buttons ()
  (switch-off *operator-buttons*)
  (setf *operator-mode* *button-no-operator*))

(defmethod output-draw-button ((button operator-button) stream cell-size)
  (let ((tester (funcall (tester button) button)))
    (when (and (on button) (not tester))
      (switch-off button)
      (setf *operator-mode* nil))
    (updating-output (stream 
		      :cache-value (list button
					 (on button)
					 (funcall (redraw-when button) button))
		      :cache-test #'equal)
      (with-output-as-presentation
	  (stream button 'operator-button
		  :single-box t
		  :allow-sensitive-inferiors nil)
	(draw-button button stream cell-size)))))


(defmethod underlabel-button ((button operator-button) stream cell-size)
  (let ((half-cell-size (/ cell-size 2)))
    (with-output-as-presentation 
	(stream button 'operator-button
		:single-box t
		:allow-sensitive-inferiors nil)      
      (with-drawing-options (stream
			     :text-style
			     +operator-button-text-style+)
	(let ((doc (if (stringp (doc button))
		       (list (doc button))
		     (doc button)))
	      (y-off 10))
	  (dolist (line doc)
	    (draw-text* stream line
			(- half-cell-size)
			(- (- half-cell-size) y-off))
	    (incf y-off 7)))))))

(defmethod draw-button ((button operator-button) stream cell-size)
  (with-scaling (stream (scale-fac button))
    (funcall (draw-function button) button stream cell-size)))

(defun choose-operator (current-button)
  (declare (ignore current-button))
  (with-visco-buttons-frame (buttons)
    (with-visco-frame (visco)
      (let ((res-op (menu-choose (mapcar #'(lambda (button)
					     (list button
						   :documentation (first (doc button))
						   :active (command-enabled (command button) visco)))
					 (remove *button-no-operator*
						 *operator-buttons*))
				 :n-columns 3
				 :pointer-documentation (get-frame-pane buttons 'pointer-documentation-pane)
				 :printer
				 #'(lambda (item stream)
				     (let ((button (first item)))
				       (formatting-table (stream)
					 (formatting-column (stream)
					   (formatting-cell (stream)
					     (with-scaling (stream 1 -1)
                                               (with-output-as-presentation (stream button
                                                                                    (type-of button)
                                                                                    :single-box t
                                                                                    :allow-sensitive-inferiors nil)
					         (draw-button button stream 50))))
					   (formatting-cell (stream :align-x :center)
					     (with-drawing-options (stream
								    :text-style
								    +operator-button-text-style+)
					       (draw-text* stream (second (doc button)) 0 0))))))))))
	(when res-op
	  (switch-off *operator-buttons*)
	  (switch-on res-op)
	  (setf *operator-mode* res-op))))))

(defun accept-choose-operator (frame stream)
  (multiple-value-bind (w h)
      (window-inside-size stream)
    (terpri stream)
    (with-centering (stream)
      (accept-buttons (list *operator-mode*)
		      1 1 frame stream
		      :width (- w (/ w 3))
		      :height (- h (/ h 3))))))

(defun update-operator-button ()
  (with-visco-frame (visco)
    (with-visco-buttons-frame (visco-buttons)
      (when (not (eq *operator-mode* *button-no-operator*))
	(setf (on *operator-mode*)
	  (command-enabled (command *operator-mode*) visco))
	(redisplay-frame-pane visco-buttons 'operator-buttons)))))

;;;
;;;
;;;

(let ((text-style 
       (parse-text-style '(:sans-serif nil :huge))))
  
  (defparameter *button-no-operator*
      (make-operator-button #'(lambda (button stream cell-size)
				(declare (ignore button cell-size))
				(with-drawing-options (stream
						       :text-style
						       text-style)
				  (draw-circle* stream 0 0 20 :filled nil)
				  (draw-circle* stream 0 0 15 :filled nil)
				  (draw-text* stream "?"
					      -5 -9)))
			    :tester #'yes
			    :doc '("No Operator Selected"))))

(defparameter *button-create-centroid*
    (make-operator-button #'(lambda (button stream cell-size)			     
			      (declare (ignore button))
			      (let ((hcs (/ cell-size 2)))
				(draw-rectangle* stream 
						 (- hcs) (- hcs)
						 hcs hcs
						 :ink +ink-arg-object+
						 :filled nil))
			      (when *point-mode*
				(draw *point-mode* stream
				      :ink +ink-res-object+
				      :label-x 0
				      :label-y -20)))
			  :redraw-when #'(lambda (button)
					   (declare (ignore button))
					   (list *point-mode*
						 *point-status*))
			  :command 'com-create-centroid
			  :doc `("Create Centroid" "S, Chain, Poly")))

(defparameter *button-create-intersection-point*
    (make-operator-button #'(lambda (button stream cell-size)
			      (declare (ignore button))
			      (let ((hcs (/ cell-size 2)))
				(draw-line* stream 
					    (- hcs) (- hcs)
					    hcs hcs
					    :ink +ink-arg-object+)
				(draw-line* stream 
					    (- hcs) hcs
					    hcs (- hcs)
					    :ink +ink-arg-object+))
			      (when *point-mode*
				(draw *point-mode* stream
				      :ink +ink-res-object+
				      :label-x 0
				      :label-y -20)))
			  :redraw-when #'(lambda (button)
					   (declare (ignore button))
					   (list *point-mode*
						 *point-status*))
			  :command 'com-create-intersection-point
			  :doc '("Create Intersection Point" "S x S")))

(let* ((outer-enclosure
	(make-instance 'gui-outer-enclosure 
	  :drawable-pointlist 
	  (get-negated-drawable-pointlist
	   (get-drawable-pointlist-for (get-expanded-polygon (icon *button-polygon*) 4))
	   -32 -32 32 32)
	  :dont-initialize t)))
  
  (defparameter *button-create-inverse-drawn-enclosure*
      (make-operator-button #'(lambda (button stream cell-size)
				(declare (ignore button))
				(let ((a (/ cell-size 2)))
				  (draw (icon *button-drawn-enclosure*) stream 
					:ink
					(if *opaque-enclosures-p*
					    +ink-res-object+
					  (make-grid-in +ink-res-object+)))
				  (draw outer-enclosure stream :ink +ink-arg-object+)
				  (draw-arrow* stream (- a) (- a) -4 -4  :ink +white+)
				  (draw-arrow* stream a a 4 4 :ink +white+)))
			    :scale-fac 0.7
			    :redraw-when #'(lambda (button)
					     (declare (ignore button))
					     (list *opaque-enclosures-p*))
			    :command 'com-create-inverse-drawn-enclosure
			    :doc '("Create Inverse Constant Enclosure" "E"))))

(let* ((polygon (icon *button-polygon*))
       (inner-enclosure
	(make-instance 'gui-inner-enclosure :arg-object polygon
		       :drawable-pointlist 
		       (get-drawable-pointlist-for (get-shrinked-polygon (icon *button-polygon*) 4))
		       :dont-initialize t))
       (outer-enclosure
	(make-instance 'gui-outer-enclosure :arg-object polygon
		       :drawable-pointlist 
		       (get-negated-drawable-pointlist
			(get-drawable-pointlist-for (get-expanded-polygon (icon *button-polygon*) 4))
			-32 -32 32 32)
		       :dont-initialize t)))
  
  (defparameter *button-create-inner-enclosure*
      (make-operator-button #'(lambda (button stream cell-size)
				(declare (ignore button cell-size))
				(draw polygon stream
				      :ink +ink-arg-object+
				      :gravity-field nil 
				      :draw-component-objects nil
				      :draw-label nil)
				(draw inner-enclosure stream
				      :ink
				      (if *opaque-enclosures-p*
					  +ink-res-object+
					(make-grid-in +ink-res-object+))))
			    :redraw-when #'(lambda (button)
					     (declare (ignore button))
					     (list *opaque-enclosures-p*))
			    :command 'com-create-inner-enclosure
			    :doc '("Create Inner Enclosure" "Poly")))
  
  (defparameter *button-create-outer-enclosure*
      (make-operator-button  #'(lambda (button stream cell-size)
				 (declare (ignore button cell-size))
				 (draw polygon stream
				       :ink +ink-arg-object+
				       :gravity-field nil 
				       :draw-component-objects nil
				       :draw-label nil)			     
				 (draw outer-enclosure stream
				       :ink 
				       (if *opaque-enclosures-p*
					   +ink-res-object+
					 (make-grid-in +ink-res-object+))))
			     :redraw-when #'(lambda (button)
					      (declare (ignore button))
					      (list *opaque-enclosures-p*))
			     :scale-fac 0.7
			     :command 'com-create-outer-enclosure
			     :doc '("Create Outer Enclosure" "Poly"))))


(let* ((line 
	(make-instance 'gui-beam 
	  :p1 (p -25 -25)
	  :p2 (p 25 25)
	  :dont-initialize t))
       (epsilon-enclosure (make-instance 'gui-epsilon-enclosure
			    :arg-object line
			    :radius 10
			    :dont-initialize t)))
  
  (defparameter *button-create-epsilon-enclosure*
      (make-operator-button #'(lambda (button stream cell-size)
				(declare (ignore button cell-size))
				(draw epsilon-enclosure stream
				      :ink
				      (if *opaque-enclosures-p*
					  +ink-res-object+
					(make-grid-in +ink-res-object+)))
				(draw line stream 
				      :ink +ink-arg-object+
				      :gravity-field nil 
				      :draw-component-objects nil
				      :draw-label nil))
			    :redraw-when #'(lambda (button)
					     (declare (ignore button))
					     (list *opaque-enclosures-p*))
			    :scale-fac 0.6
			    :command 'com-create-epsilon-enclosure
			    :doc '("Create Epsilon Enclosure" "P, S, Chain, Poly"))))

#|

(let ((text-style 
       (parse-text-style '(:sans-serif nil :huge))))
  (defparameter *button-recreate*
      (make-operator-button #'(lambda (button stream cell-size)
				(draw-circle* stream 0 0 20 :filled nil)
				(draw-circle* stream 0 0 15 :filled nil)				
				(draw-label* '("!") -3 -7 stream 
					     :label-text-style text-style))			   
			    :scale-fac 1.0
			    :command 'com-recreate
			    :doc "Recreate")))
|#

;;;
;;;
;;;

(let ((text-style 
       (parse-text-style '(:sans-serif nil :large))))
  
  (defparameter *button-set-semantics*
      (make-operator-button #'(lambda (button stream cell-size)
				(declare (ignore button))
				(let ((hcs (/ cell-size 2)))
				  (draw-polygon* stream
						 (list -20 -12 -15 -5 10 4 15 18)
						 :filled nil
						 :closed nil
						 :ink +ink-arg-object+)
				  (draw-text* stream "River" 
					      (- hcs) (- hcs) 
					      :ink +ink-res-object+
					      :text-style text-style)))
			    :command 'com-set-semantics
			    :doc '("Set Semantics" "P, S, Chain, Poly"))))

(let ((text-style 
       (parse-text-style '(:sans-serif nil :huge))))      
  
  (defparameter *button-set-at-most-constraint*
      (make-operator-button #'(lambda (button stream cell-size)
				(declare (ignore button))
				(let ((hcs (/ cell-size 2)))
				  (draw-text* stream "3" 
					      -5 -10
					      :ink +ink-res-object+
					      :text-style text-style)
				  (draw-line* stream (- hcs) (- hcs) hcs hcs 
					      :ink +ink-arg-object+)))
			    :command 'com-set-at-most-constraint
			    :doc '("Set At Most Constraint" "R, Chain, Poly"))))

(let ((arrow (make-instance 'gui-orientation-arrow
	       :alpha-off (- +pi/2+ (/ pi 4))
	       :dont-initialize t
	       :object (make-instance 'gui-beam :dont-initialize t
				      :p1 (p -25 -25)
				      :p2 (p 25 25)
				      :orientation-constraint (list 0)))))
  
  (defparameter *button-set-orientation-constraint*
      (make-operator-button #'(lambda (button stream cell-size)
				(declare (ignore button))
				(let ((hcs (/ cell-size 2)))
				  (setf (r arrow) hcs)
				  (draw arrow stream :ink +ink-res-object+)
				  (draw-line* stream (- hcs) (- hcs) hcs hcs
					      :ink +ink-arg-object+)))
			    :command 'com-set-orientation-constraint
			    :doc '("Set Orientation Constraint" "O, B, AR"))))


(let ((arrow (make-instance 'gui-orientation-arrow
	       :r 20
	       :dont-initialize t
	       :alpha-off (- +pi/2+ (/ pi 4))
	       :object (make-instance 'gui-beam :dont-initialize t
				      :p1 (p -25 -25)
				      :p2 (p 25 25)
				      :orientation-constraint (list 0 0.0001))))) ; 0.0001 = Trick
  
  (defparameter *button-create-orientation-constraint-mark*
      (make-operator-button #'(lambda (button stream cell-size)
				(declare (ignore button cell-size))
				(draw arrow stream :ink +ink-arg-object+)
				(draw-circle* stream 20 0 +bullet-size+ :ink +ink-res-object+)
				(draw-circle* stream -20 0 +bullet-size+ :ink +ink-res-object+))
			    :command 'com-set-orientation-constraint-mark
			    :doc '("Set Orientation Constraint Mark" "OC")))

  (defparameter *button-create-orientation-constraint-intervall*
      (make-operator-button #'(lambda (button stream cell-size)
				(declare (ignore button cell-size))
				(draw arrow stream :ink +ink-arg-object+)
				(draw-circle* stream 0 0 23
					      :start-angle (- pi (* 1/8 pi))
					      :end-angle (+ pi (* 1/8 pi))
					      :ink +ink-res-object+
					      :filled nil)
				(draw-circle* stream 0 0 23
					      :start-angle (- +2pi+ (* 1/8 pi))
					      :end-angle (+ +2pi+ (* 1/8 pi))
					      :ink +ink-res-object+
					      :filled nil))
			    :command 'com-set-orientation-constraint-intervall
			    :doc '("Set Orientation Constraint Intervall" "OC"))))

(let ((circle (make-instance 'gui-relative-orientation-circle
		:r 20
		:dont-initialize t
		:allowed-derivation 0		
		:object1 (l (p (- 10) (- 10)) (p 10 10))
		:object2 (l (p (- 10) 10) (p 10 (- 10))))))
  
  (defparameter *button-create-relative-orientation-constraint*
      (make-operator-button #'(lambda (button stream cell-size)
				(declare (ignore button))
				(let ((hcs (/ cell-size 2)))
				  (draw-line* stream (- hcs) (- hcs) hcs hcs :ink +ink-arg-object+)
				  (draw-line* stream (- hcs) hcs hcs (- hcs) :ink +ink-arg-object+)
				  (draw circle stream :ink +ink-res-object+)))
			    :command 'com-set-relative-orientation-constraint
			    :doc '("Set Rel. Orient. Constraint" "(B / AR) x (B / AR)"))))


(defparameter *button-set-transparency-properties*
    (make-operator-button #'(lambda (button stream cell-size)
			      (declare (ignore button))
			      (let ((hcs (/ cell-size 2)))
				(draw-rectangle* stream 
						 (- hcs) (- hcs) hcs hcs
						 :filled nil
						 :ink +ink-arg-object+)
				(draw-text* stream 
					    "100 m." (+ 5 (- hcs)) 0
					    :ink +ink-res-object+)))
			  :command 'com-set-transparency-properties
			  :doc '("Set Transparency Properties" "T")))

