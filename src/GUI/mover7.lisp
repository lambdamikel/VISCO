;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: GUI; Base: 10 -*-

(in-package gui)

;;;
;;;
;;;

(defun moveable-object-p (object)
  (and (typep object 
	      '(or #| gui-label |#
		gui-semantics-label
		gui-at-most-label
		gui-orientation-arrow
		gui-relative-orientation-circle
		gui-point
		gui-line
		gui-chain-or-polygon
		gui-drawn-enclosure))
       (typecase object
	 (gui-point (not (and (typep object 'possible-operator-result-mixin)
			      (res-of-operators object))))
	 ((or gui-line gui-chain-or-polygon)
	  (not (some #'(lambda (part)
			 (and (typep part 'possible-operator-result-mixin)
			      (res-of-operators part)))
		     (point-list object))))
	 (otherwise t))))

(define-visco-command (com-move :name nil)
    ((object 'gui-object) (x 'number) (y 'number))
  (with-visco-frame (visco)
    (let ((focus (current-focus visco)))      
      (prepare-to-move)
      (multiple-value-bind (xc yc)
	  (xy-to-grid x y)
	(interactive-move object xc yc))
      (set-current-focus-to (find-named-history-entry (name focus)))
      (refresh))))

(define-visco-command (com-move* :name "Move")
    ()
  (let ((obj (accept '(and gui-object
		       (satisfies moveable-object-p))
		     :prompt "Select Object To Move")))
    (multiple-value-bind (x y)
	(stream-pointer-position (with-visco-frame (visco)
				   (get-frame-pane visco 'display)))
      (com-move obj x y))))

(defmethod command-enabled ((command (eql 'com-move*)) (frame visco)
                            #+:mcl &optional #+:mcl unused)
  #+:mcl (declare (ignore unused))
  (and (get-current-query)
       (some #'moveable-object-p (visco-objects (get-current-query)))))


(defun prepare-to-move ()
  (let ((vr *visualized-relations*))  
    (setf *sceleton-view* t
	  *visualized-relations* nil)
    (refresh)
    (setf *sceleton-view* nil
	  *visualized-relations* vr)))

;;;
;;;
;;;

(define-presentation-to-command-translator move 
    (gui-object
     com-move visco
     :documentation ((object stream) (format stream "Move ~A" object))
     :tester ((object)
	      (moveable-object-p object))
     :echo nil
     :maintain-history nil
     :gesture :move)
  (object x y)
  (list object x y))

(define-presentation-to-command-translator move-label
    (gui-query-object
     com-move visco
     :documentation ((object stream) (format stream "Move ~A" (status-label object)))
     :echo nil
     :maintain-history nil
     :gesture nil)
  (object x y)
  (list (status-label object) x y))

;;;
;;;
;;;

(defmethod interactive-move ((object gui-label) xc yc)
  (with-visco-frame (visco)
    (let ((stream (get-frame-pane visco 'display))	  
	  (x-off (x-off object))
	  (y-off (y-off object)))
      (flet ((exit-code ()
               (setf (x-off object) x-off
                     (y-off object) y-off)
               (return-from interactive-move)))
        (with-output-recording-options (stream :draw t :record nil)
	  (tracking-pointer (stream)
	    (:pointer-motion (x y)
			     (multiple-value-bind (x y)
			                          (xy-to-grid x y)
			       
			     (draw object stream :handling-active t)
			     
			     (setf (x-off object) (+ (- x xc ) x-off)
				   (y-off object) (+ (- y yc) y-off))
			     (draw object stream :handling-active t)))
	    (:keyboard (character)
                       (when (and (characterp character)
                                  (char-equal character #\q))
                         (exit-code)))
	    (:pointer-button-press (event)
				   (if (= (pointer-event-button event)
					  +pointer-right-button+)
                                     (exit-code)
				     (progn
				       (register-undo-information (format nil "Move ~A" object))
				       (execute 
				        (register-history-entry 
				         ""
				         `(let* ((obj (find-named-visco-object ,(name (object object))))
					         (label ,(typecase object
							   (gui-status-label '(status-label obj))
							   (gui-semantics-label '(semantics-label obj))
							   (gui-at-most-label '(at-most-label obj)))))
					    (setf (x-off label) ,(x-off object)
						(y-off label) ,(y-off object))
					    obj)
				         (list (object object))))
				       (return))))))))))

;;;
;;;
;;;

(defmethod interactive-move ((object gui-orientation-arrow) xc yc)
  (declare (ignore xc yc))
  (with-visco-frame (visco)
    (let* ((stream (get-frame-pane visco 'display))
	   (master (object object))
	   (old-alpha (alpha-off object))
	   (old-r (r object))
	   (old-rho-off (rho-off object))
	   (old-r-off (r-off object))
	   (alpha-master (if (typep master 'geom-line)
			     (global-orientation master)
			   0)))
      (labels ((exit-code ()
                 (setf (r object) old-r
                       (alpha-off object) old-alpha
                       (rho-off object) old-rho-off
                       (r-off object) old-r-off)
                 (return-from interactive-move)))
        (with-output-recording-options (stream :draw t :record nil)
	  (tracking-pointer (stream)
	    (:pointer-motion (x y)
			   (multiple-value-bind (x y)
			       (xy-to-grid x y)
			     
			     (draw object stream :handling-active t)
			     (multiple-value-bind (ox oy)
				 (get-origin-for object)
			       (multiple-value-bind (r alpha)
				   (distance-and-orientation* ox oy
							      x y)
				 (setf (r object) r
				       (alpha-off object) (- alpha alpha-master))))
			     
			     (draw object stream :handling-active t)))
	    (:keyboard (character)
                       (when (and (characterp character)
                                  (char-equal character #\q))
                         (exit-code)))            
	    (:pointer-button-press (event x y)
                                 (cond ((= (pointer-event-button event)					
                                           +pointer-right-button+)                                        
                                        (exit-code))
                                       ((and (= (pointer-event-button event)
                                                +pointer-left-button+)
                                             (= (event-modifier-state event)
                                                +shift-key+))
                                        (draw object stream :handling-active t)					    
                                        (set-origin-to object x y)
                                        (draw object stream :handling-active t))
                                       ((and (= (pointer-event-button event)
                                                +pointer-left-button+))
                                        (register-undo-information (format nil "Move ~A" object))
                                        (execute 
                                         (register-history-entry 
                                          ""
                                          `(let* ((obj (find-named-visco-object ,(name (object object))))
                                                  (arrow (orientation-arrow obj)))
                                             (setf (r arrow) ,(r object))
                                             (setf (r-off arrow) ,(r-off object)
                                                   (rho-off arrow) ,(rho-off object))
                                             (setf (alpha-off arrow) ,(alpha-off object))
                                             obj)
                                          (list (object object))))
                                        (return))))))))))

;;;
;;;
;;;

(defmethod interactive-move ((object gui-relative-orientation-circle) xc yc)
  (declare (ignore xc yc))
  (let ((obj1 (object1 object))
	(obj2 (object2 object)))    
    (gui-delete-object object nil)    
    (gui-set-relative-orientation-constraint obj1 obj2)
    (pop-undo-stack)
    (set-undo-operation-description-to (format nil "Move ~A" object))))

;;;
;;;
;;;

(defmethod interactive-move ((object gui-point) xc yc)
  (interactive-mover object 
		     (list object) xc yc))

(defmethod interactive-move ((object gui-line) xc yc)
  (interactive-mover object
		     (list (p1 object)
			   (p2 object)) 
		     xc yc))

(defmethod interactive-move ((object gui-chain-or-polygon) xc yc) 
  (interactive-mover object 
		     (point-list object)
		     xc yc))

;;;
;;;
;;;

(defun interactive-mover (object point-list xc yc)
  (with-visco-frame (visco)
    (let ((stream (get-frame-pane visco 'display)))
      
      (register-undo-information (format nil "Move ~A" object))      
      
      (let* ((direct-masters (remove-duplicates
			      (mapcan #'(lambda (point)
					  (copy-list (part-of point)))
				      point-list)))
	     
	     (circles (remove-duplicates 
		       (mapcan #'relative-orientation-circles 
			       (remove-if-not #'(lambda (obj)
						  (typep obj 'atomic-rubberband))
					      direct-masters))))
	     
	     (roots (remove-duplicates 
		     (mapcan #'(lambda (segment)
				 (copy-list (part-of segment)))
			     direct-masters)))
	     
	     (x-offsets (mapcar #'(lambda (point)
				    (- xc (x point)))
				point-list))
	     (y-offsets (mapcar #'(lambda (point)
				    (- yc (y point)))
				point-list))
	     
	     (org-coords (mapcar #'(lambda (point) (list (x point) (y point)))
				 point-list)))
	
	(with-output-recording-options (stream :draw t :record nil)
	  (labels ((draw-it ()
		     (dolist (master direct-masters)
		       (draw master stream
			     :draw-relative-orientation-circles nil
			     :draw-component-objects nil
			     :handling-active t))
		     
		     (dolist (root roots)
		       (draw (status-label root) stream
			     :handling-active t)
		       (when (semantics root)
			 (draw (semantics-label root) stream
			       :handling-active t))
		       (when (at-most-constraint root) 
			 (draw (at-most-label root) stream
			       :handling-active t)))
		     
		     (dolist (circle circles)
		       (draw circle stream 
			     :handling-active t))
		     (dolist (point point-list)
		       (setf (already-drawn point) nil)
		       (draw point stream :handling-active t))))
	    
	    (if (not (tracking-pointer (stream)
		       (:pointer-motion (x y)
					(multiple-value-bind (x y)
					    (xy-to-grid x y)
					  (when (every #'(lambda (point xoff yoff)
							   (inside-p* (- x xoff)
								      (- y yoff)
								      (on-transparency point)))
						       point-list x-offsets y-offsets)
					    (draw-it)
					    (mapc #'(lambda (point xoff yoff)
						      (setf (x point) (- x xoff)
							    (y point) (- y yoff)))
						  point-list x-offsets y-offsets)
					    (dolist (master direct-masters)
					      (calculate-bounding-box master))
					    (dolist (root roots)
					      (calculate-centroid root))
					    (draw-it))))
	               (:keyboard (character)
                                  (when (and (characterp character)
                                             (char-equal character #\q))
                                    (return nil)))
		       (:pointer-button-press (event)
					      (if (= (pointer-event-button event)
						     +pointer-right-button+)
						  (return nil)
						(return t)))))
		(progn 
		  (pop-undo-stack)
		  (mapc #'(lambda (org-coord point)
			    (setf (x point) (first org-coord)
				  (y point) (second org-coord)))
			org-coords point-list)
		  (dolist (master direct-masters)
		    (calculate-bounding-box master)))
	      
	      (unless (change-query-from-to
		       (loop as point in point-list collect (history-entry point))
		       (loop as point in point-list collect
			     (get-history-entry-for point
						    :transparency (on-transparency point)
						    :x (x point)
						    :y (y point)
						    :description (operation-descr (history-entry point))
						    :transparency (on-transparency point)
						    :ink-rgb-list (ink-rgb-list point)
						    :name (name point)
						    :status (status point))))
		(pop-undo-stack)))))))))


(defun find-closest-point (xc yc point-list)
  (let ((min-dist nil)
	(min-point nil))
    (dolist (point point-list)
      (let ((d (distance-between* xc yc (x point) (y point))))
	(when (or (not min-dist)
		  (< d min-dist))
	  (setf min-dist d
		min-point point))))
    min-point))


(defmethod interactive-move ((object gui-drawn-enclosure) xc yc) 
  (with-visco-frame (visco)
    (let* ((stream (get-frame-pane visco 'display))
	   (point-list (point-list object))
	   (closest-point (find-closest-point xc yc (cons (centroid object) point-list)))
	   (modify-single-point (unless (eq closest-point (centroid object))
				  closest-point)))
      
      (register-undo-information (format nil "Move ~A" object))
      
      (let* ((xylist nil)
	     (x-offsets (mapcar #'(lambda (point)
				    (- xc (x point)))
				point-list))
	     (y-offsets (mapcar #'(lambda (point)
				    (- yc (y point)))
				point-list))
	     (org-coords (mapcar #'(lambda (point) (list (x point) (y point)))
				 point-list)))
	
	(with-output-recording-options (stream :draw t :record nil)
	  (if (not (tracking-pointer (stream)
		     (:pointer-motion (x y)
				      (multiple-value-bind (x y)
					  (xy-to-grid x y)
					
					(if modify-single-point
					    (when (inside-p* x y (on-transparency object))
					      (draw object stream :handling-active t)

					      (setf (x modify-single-point) x
						    (y modify-single-point) y)
					      
					      (setf xylist
						(mapcan #'(lambda (point)
							    (list (x point)
								  (y point)))
							point-list))
					      
					      (setf (drawable-pointlist object)
						(get-drawable-pointlist-for object))
					      
					      (draw object stream :handling-active t))

					  (when (every #'(lambda (point xoff yoff)
							   (declare (ignore point))
							   (inside-p* (- x xoff)
								      (- y yoff)
								      (on-transparency object)))
						       point-list x-offsets y-offsets)
					    (draw object stream :handling-active t)
					    
					    (setf xylist
					      (mapcan #'(lambda (point xoff yoff)
							  (progn (setf (x point) (- x xoff)
								       (y point) (- y yoff)))
							  (list (x point)
								(y point)))
						      point-list x-offsets y-offsets))
					    
					    (setf (drawable-pointlist object)
					      (get-drawable-pointlist-for object))
					    
					    (draw object stream :handling-active t)))))
                     (:keyboard (character)
                                (when (and (characterp character)
                                  (char-equal character #\q))
                                  (return nil)))
		     (:pointer-button-press (event)
					    (if (= (pointer-event-button event)
						   +pointer-right-button+)
						(return nil))
					    (return t))))
	      (progn
		(pop-undo-stack)
		(mapc #'(lambda (org-coord point)
			  (setf (x point) (first org-coord)
				(y point) (second org-coord)))
		      org-coords point-list)
		(setf (drawable-pointlist object)
		  (get-drawable-pointlist-for object)))
	    
	    (unless (change-query-from-to (history-entry object)
					  (get-history-entry-for object
								 :description (operation-descr (history-entry object))
								 :transparency (on-transparency object)
								 :pattern-id (pattern-id object)
								 :opaque-p (opaque-p object)
								 :ink-rgb-list (ink-rgb-list object)
								 :name (name object)
								 :negated-p (negated-p object)
								 :pointlist xylist))
	      (pop-undo-stack))))))))


