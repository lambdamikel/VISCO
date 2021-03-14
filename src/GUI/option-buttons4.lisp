;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: GUI; Base: 10 -*-

(in-package gui)

(defun accept-option-buttons (frame stream)
  (declare (ignore frame))
  (with-centering (stream)
    (format stream "The Grid Is"))
  
  (with-centering (stream)
    (multiple-value-bind (int ptype changed)
	(accept 'completion
		:stream stream 
		:query-identifier 'grid-resolution
		:default *grid-resolution*
		:prompt nil
		:view `(option-pane-view :items (off 5 10 20 30 40 50)
					 :value ,*grid-resolution*
					 :name-key ,#'(lambda (item)
							(case item
							  (off "Off")
							  (otherwise (format nil "On, Stepsize ~A" item))))
					 :value-key ,#'(lambda (item)
							 (and (numberp item) item))))
      (declare (ignore ptype))
      (when changed (setf *grid-resolution* int))))
  
  (with-centering (stream)
    (format stream "New Enclosures Are"))
  
  (with-centering (stream)
    (multiple-value-bind (bool ptype changed)
	(accept 'completion
		:query-identifier 'enclosures
		:prompt nil
		:stream stream 
		:default *opaque-enclosures-p*
		:view `(option-pane-view :items (opaque translucent)
					 :value-key ,#'(lambda (sym)
							 (eq sym 'opaque))
					 :value ,*opaque-enclosures-p*))
      (declare (ignore ptype))
      (when changed 
	(setf *opaque-enclosures-p* bool)
	(reinitialize *button-drawn-enclosure*))))
  
  
  (with-centering (stream)
    (format stream "Component Relations (I & D)"))
  
  (with-centering (stream)
    (multiple-value-bind (bool ptype changed)
	(accept 'boolean
		:query-identifier 'ignore-relations
		:prompt nil
		:stream stream 
		:default (not *ignore-disjoint-and-intersects-component-relations-p*))
      (declare (ignore ptype))
      (when changed 
	(setf *ignore-disjoint-and-intersects-component-relations-p* (not bool)))))
  
  (with-centering (stream)
    (format stream "Display Options"))
  
  (with-visco-frame (visco)
    (with-centering (stream)
      (formatting-table (stream)
	(formatting-row (stream)
	  (formatting-cell (stream :align-x :center)
	    (multiple-value-bind (set ptype changed)
		(accept `((subset-completion (Intersects Inside/Contains Disjoint)))
			:stream stream 
			:default *visualized-relations*
			:prompt nil
			:view `(list-pane-view :value ,*visualized-relations*))
	      (declare (ignore ptype))	      
	      (when changed
		(setf *visualized-relations* set)
		(redisplay-frame-pane visco 'display :force-p t))))
	  
	  (formatting-cell (stream :align-x :center)
	    (multiple-value-bind (bool ptype changed)
		(accept 'boolean
			:prompt "Focus"
			:stream stream 
			:default *focus-box*)
	      (declare (ignore ptype))	      
	      (when changed 
		(setf *focus-box* bool)
		(redisplay-frame-pane visco 'display :force-p t)))))))))
