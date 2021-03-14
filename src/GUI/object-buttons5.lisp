;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: GUI; Base: 10 -*-

(in-package gui)

(defclass object-button (icon-button)
  ((tester :initform #'standard-tester)
   (when-action-begins :initform #'creating-begins)   
   (when-action-ends :initform #'creating-ends)))

(defmethod when-selected ((button object-button))
  #'(lambda (button)
      (funcall (slot-value button 'when-selected) button)
      (set-object-modes)))

(defmethod standard-tester ((button button))
  (if (on button)
      (=> (eq *primary-mode*
	      (icon button))
	  (not *creating-active*))
    (valid-p button)))

;;;
;;;
;;;

(defmethod initialize-instance :after ((obj object-button) &rest initargs)
  (declare (ignore initargs))
  (push obj *object-buttons*)
  (when (slot-exists-p (icon obj) 'status)
    (setf (object (status-label (icon obj))) (icon obj))))

(defun initialize-object-buttons ()
  (setf *creating-active* nil)
  (switch-off *object-buttons*)
  (setf (on *button-transparency*) t)
  (activate *object-buttons*)
  (setf *primary-mode* (icon *button-transparency*)
	*point-mode* nil
	*segment-mode* nil)
  (dolist (button *object-buttons*)
    (when (slot-exists-p (icon button) 'status)
      (setf (status (icon button)) nil))))


(defmethod inform-button-action-begins :after ((button object-button))
  (redraw-buttons))

(defun make-object-button (icon &rest args)
  (apply #'make-instance 'object-button :icon icon args))

;;;
;;;
;;;

(defun set-object-modes ()
  (labels ((get-activated-buttons (buttons)
	     (find-if #'(lambda (button)
			  (and (on button)
			       (not (deactivated button))
			       (valid-p button)))
		      buttons)))    
    (setf *point-mode*
      (let ((button 
	     (get-activated-buttons
	      (list *button-nail* *button-marble* *button-origin*))))
	(and button (icon button))))
    (setf *segment-mode*
      (let ((button 
	     (get-activated-buttons
	      (list *button-beam* *button-atomic-rubberband*
		    *button-atomic-<=-rubberband* *button-atomic->=-rubberband*
		    *button-rubberband*))))
	(and button (icon button))))
    (setf *primary-mode*
      (let ((button 
	     (get-activated-buttons
	      *object-buttons*)))
	(and button (icon button))))

    (dotimes (i 4)
      (dolist (button *object-buttons*)
	(when (and (on button)
		   (not (valid-p button)))
	  (switch-off button))))

    (redraw-buttons)))

;;;
;;;
;;;

(defmethod line-validator ((button button))
  (and (get-current-transparency)
       *segment-status*))

(defmethod chain-or-polygon-validator ((button button))
  (and (get-current-transparency)
       *chain-or-polygon-status*))

;;;
;;;
;;;

(defun creating-begins (button)
  (declare (ignore button))
  (dolist (button *object-buttons*)
    (when (or (and (typep *primary-mode* 'gui-point)
		   (not (typep (icon button) 'gui-point)))
	      (and (typep *primary-mode* 'gui-line)
		   (typep (icon button) 'gui-chain-or-polygon))
	      (and (typep *primary-mode* 'gui-transparency)
		   (not (typep (icon button) 'gui-transparency)))
	      (and (typep *primary-mode* 'gui-drawn-enclosure)
		   (not (typep (icon button) 'gui-drawn-enclosure))))
      (deactivate button)))
  (set-object-modes)
  (setf *creating-active* t))

(defun creating-ends (button)
  (when (and (=> (typep (icon button) 'gui-point)
		 (eq *point-mode* *primary-mode*))
	     (=> (typep (icon button) 'gui-line)
		 (eq *segment-mode* *primary-mode*)))
    (activate *object-buttons*)
    (setf *creating-active* nil)))

;;;
;;;
;;;


(defparameter *button-transparency* 
    (make-object-button 
     (make-instance 'gui-transparency 
       :segments nil
       :pmin (p -15 -15)
       :pmax (p 15 15)
       :bounding-box-p nil
       :dont-initialize t)
     :valid #'(lambda (button)
		(declare (ignore button))
		(get-current-query))
     :when-selected #'(lambda (button)  						
			(switch-off (remove button *object-buttons*))
			(toggle button))
     :doc "Transparency (T)"))

;;;
;;;
;;;

(defparameter *button-origin*
    (make-object-button (make-instance 'gui-origin :x 0 :y 0 :dont-initialize t)			
			:when-action-ends #'(lambda (button)
					      (creating-ends button)
					      (toggle button)
					      (toggle *button-nail*)
					      (set-object-modes))
			:valid #'(lambda (button)
				   (declare (ignore button))
				   (let ((transparency (get-current-transparency)))
				     (and transparency
					  *point-status*
					  (not 
					   (some #'(lambda (dummy)
						     (and (typep dummy 'gui-origin)
							  (eq (dummy-p dummy) 'locked)))
						 *dummies*))
					  (not (origin transparency)))))
			:when-selected #'(lambda (button)
					   (toggle button)
					   (switch-off (list *button-marble*
							     *button-nail*
							     *button-transparency*
							     *button-drawn-enclosure*)))
			:doc "Origin (O)"))


(defparameter *button-nail* 
    (make-object-button (make-instance 'gui-nail :x 0 :y 0 :dont-initialize t)			
			:valid #'(lambda (button)
				   (declare (ignore button))
				   (let ((transparency (get-current-transparency)))
				     (and transparency
					  *point-status*)))
			:when-selected #'(lambda (button)
					   (toggle button)
					   (switch-off (list *button-origin*
							     *button-marble*
							     *button-transparency*
							     *button-drawn-enclosure*)))
			:doc "Nail (N, P)"))


(defparameter *button-marble* 
    (make-object-button (make-instance 'gui-marble :x 0 :y 0 :dont-initialize t)			
			:valid #'(lambda (button)
				   (declare (ignore button))
				   (let ((transparency (get-current-transparency)))
				     (and transparency
					  *point-status*
					  (some #'(lambda (obj)
						    (typep obj 'enclosure))
						(transparency-query-objects-and-enclosures transparency)))))
			:when-selected #'(lambda (button)
					   (toggle button)
					   (switch-off (list *button-nail*
							     *button-origin*
							     *button-transparency*
							     *button-drawn-enclosure*)))
			:doc "Marble (M, P)"))

;;;
;;;
;;;


(let ((p1 (p -25 -25))
      (p2 (p 25 25)))
  
  (defparameter *button-atomic-rubberband*
      (make-object-button 
       (make-instance 'gui-atomic-rubberband
	 :p1 p1
	 :p2 p2
	 :dont-initialize t)
       :valid #'line-validator
       :when-selected #'(lambda (button)
			  (toggle button)
			  (switch-off (list *button-transparency*
					    *button-rubberband*
					    *button-beam*
					    *button-atomic->=-rubberband*
					    *button-atomic-<=-rubberband*
					    *button-drawn-enclosure*)))
       :doc "Atomic Rubberband (AR, S)"))


  (defparameter *button-atomic-<=-rubberband* 
      (make-object-button 
       (make-instance 'gui-atomic-<=-rubberband
	 :p1 p1
	 :p2 p2
	 :dont-initialize t)
       :valid #'line-validator
       :when-selected #'(lambda (button)
			  (toggle button)
			  (switch-off (list *button-transparency*
					    *button-rubberband*
					    *button-beam*
					    *button-atomic-rubberband*
					    *button-atomic->=-rubberband*
					    *button-drawn-enclosure*)))
       :doc "Atomic <= Rubberband (AR, S)"))


  (defparameter *button-atomic->=-rubberband*
      (make-object-button 
       (make-instance 'gui-atomic->=-rubberband
	 :p1 p1
	 :p2 p2
	 :dont-initialize t)
       :valid #'line-validator
       :when-selected #'(lambda (button)
			  (toggle button)
			  (switch-off (list *button-transparency*
					    *button-rubberband*
					    *button-beam*
					    *button-atomic-rubberband*
					    *button-atomic-<=-rubberband*
					    *button-drawn-enclosure*)))
       :doc "Atomic >= Rubberband (AR, S)"))


  (defparameter *button-rubberband*
      (make-object-button
       (make-instance 'gui-rubberband
	 :p1 p1
	 :p2 p2
	 :dont-initialize t)
       :valid #'(lambda (button) 
		  (and (line-validator button)
		       (eq *point-status* 'db-component)))
       :when-selected #'(lambda (button)
			  (toggle button)
			  (switch-off (list *button-transparency*
					    *button-beam*
					    *button-atomic-rubberband*
					    *button-atomic->=-rubberband*
					    *button-atomic-<=-rubberband*
					    *button-drawn-enclosure*)))
       :doc "Rubberband (R, S)"))


  (defparameter *button-beam*
      (make-object-button (make-instance 'gui-beam 
			    :p1 p1
			    :p2 p2
			    :dont-initialize t)
			  :valid #'line-validator
			  :when-selected #'(lambda (button)
					     (toggle button)
					     (switch-off (list *button-transparency*
							       *button-rubberband*
							       *button-atomic-rubberband*
							       *button-atomic->=-rubberband*
							       *button-atomic-<=-rubberband*
							       *button-drawn-enclosure*)))
			  :doc "Beam (B, S)")))


(let* ((p1 (p -25 0))
       (p2 (p 0 -25))
       (p3 (p 25 0))
       (p4 (p 0 25))
       (p5 (p 0 0))
       
       (s1 (l p1 p2))
       (s2 (l p2 p3))
       (s3 (l p3 p4))
       (s4 (l p4 p5))
       (s5 (l p5 p1)))

  (defparameter *button-chain* 
      (make-object-button 
       (make-instance 'gui-chain
	 :segments (list s1 s2 s3 s4)
	 :point-list (list p1 p2 p3 p4 p5)
	 :dont-initialize t)
       :valid #'chain-or-polygon-validator
       :when-selected #'(lambda (button)
			  (toggle button)
			  (switch-off (list *button-transparency*
					    *button-drawn-enclosure*
					    *button-polygon*)))
       :doc "Chain"))
  

  (defparameter *button-polygon* 
      (make-object-button 
       (make-instance 'gui-polygon
	 :segments (list s1 s2 s3 s4 s5)
	 :point-list (list p1 p2 p3 p4 p5)
	 :dont-initialize t)
       :valid #'chain-or-polygon-validator
       :when-selected #'(lambda (button)			  
			  (toggle button)
			  (switch-off (list *button-transparency*
					    *button-drawn-enclosure*
					    *button-chain*)))
       :doc "Polygon (Poly)"))
  
  (defparameter *button-drawn-enclosure*
      (make-object-button 
       (let ((obj
	      (make-instance 'gui-drawn-enclosure
		:on-transparency nil
		:segments (list s1 s2 s3 s4 s5)
		:point-list (list p1 p2 p3 p4 p5)
		:dont-initialize t)))
	 (setf (drawable-pointlist obj)
	   (get-drawable-pointlist-for obj))
	 obj)
       :valid #'(lambda (button)
		  (declare (ignore button))
		  (get-current-transparency))
       :when-selected #'(lambda (button)			  
			  (switch-off (remove button *object-buttons*))
			  (toggle button))
       :doc "Enclosure (E)"
       :initialize #'(lambda (button)
		       (setf (ink (icon button))
			 (if *opaque-enclosures-p*
			     (make-gray-color 0.4)
			   (make-grid-in +black+)))
		       (tick-object button)))))

;;;
;;;
;;;

(defun accept-object-buttons (frame stream)
  (multiple-value-bind (w h)
      (window-inside-size stream)
    (let ((w (/ w 3))
	  (h (/ (- h 15) 9)))
      
      (stream-set-cursor-position stream 4 0)
      
      (let ((x nil)
	    (y nil))
	(with-centering (stream)
	  (multiple-value-bind (xlu ylu xro yro)
	      (accept-buttons (list 
			       *button-transparency* *button-drawn-enclosure*)
			      1 2 frame stream 
			      :width (* w 2)
			      :height h)
	    (declare (ignore xro yro))
	    (setf x xlu 
		  y (+ 4 ylu))))
	(stream-set-cursor-position stream x (+ y 2 (* h 2)))
	
	(multiple-value-bind (xlu1 ylu1 xro1 yro1)
	    (accept-buttons (list *button-origin*
				  *button-nail*
				  *button-marble*)
			    3 1 frame stream
			    :width w
			    :height (* h 3))
	  (declare (ignore ylu1 yro1))
	  (stream-set-cursor-position stream (+ 4 xro1) y)

	  (multiple-value-bind (xlu2 ylu2 xro2 yro2)
	      (accept-buttons (list *button-beam*
				    *button-atomic-<=-rubberband*
				    *button-atomic->=-rubberband*
				    *button-atomic-rubberband*				    
				    *button-rubberband*)
			      5 1 frame stream
			      :width w
			      :height (* 5 h))
	    (declare (ignore yro2 xlu2))
	    (stream-set-cursor-position stream (+ 4 xro2) (+ y 2 (* h 3)))
	    

	    (accept-buttons (list *button-chain*
				  *button-polygon*)
			    2 1 frame stream
			    :width w
			    :height (* 2 h))
	    
	    (stream-set-cursor-position stream xlu1 (+ 8 ylu2))
	    
	    (accept-buttons (reverse *status-buttons*)
			    3 3 frame stream
			    :width (* 3 w)
			    :height (* 3 h))))))))



;;;
;;;
;;;

(defmethod output-draw-button ((button object-button) stream cell-size)
  (updating-output (stream 
		    :cache-value (list (on button)
				       (deactivated button)
				       (funcall (tester button) button)
				       (tick button)
				       (when (slot-exists-p (icon button) 'status)
					 (status (icon button))))
		    :cache-test #'equal)
    (with-output-as-presentation
	(stream button 'object-button
		:single-box t
		:allow-sensitive-inferiors nil)
      (draw-button button stream cell-size))))


(defmethod draw-button ((button object-button) stream cell-size)
  (let ((half-cell-size (/ cell-size 2)))
    (with-scaling (stream (scale-fac button)
			  (scale-fac button))      
      (draw (icon button) stream 
	    :label-x (- half-cell-size)
	    :label-y (+ (- half-cell-size) 35)
	    :draw-chain-or-polygon-icon-p nil
	    :gravity-field nil 
	    :draw-component-objects nil))))

