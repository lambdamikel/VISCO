;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: GUI; Base: 10 -*-

(in-package gui)

(defun xy-to-grid (x y)
  (if *grid-resolution*
      (values 
       (* *grid-resolution* (floor x *grid-resolution*))
       (* *grid-resolution* (floor y *grid-resolution*)))
    (values x y)))

(defmethod get-create-description ((object gui-query-object-or-enclosure))
  (format nil "Create ~A"
	  object))

(defmethod get-create-description ((object gui-transparency))
  (format nil "Create ~A" object))

;;;
;;;
;;;

(define-visco-command (com-create-object-from-position :name nil)
    ((x 'number) (y 'number))
  (register-undo-information nil)
  (inform-button-action-begins
   (get-button-for-icon *primary-mode*))
  (let ((object (interactive-create *primary-mode* (list x y))))
    (if object
	(set-undo-operation-description-to (get-create-description object))
      (pop-undo-stack)))
  (inform-button-action-ends
   (get-button-for-icon *primary-mode*))
  (set-current-focus-to-newest-history-entry)
  (set-object-modes)    
  (refresh))

(define-visco-command (com-create-object-from-point :name nil)
    ((object 'gui-point))
  (register-undo-information nil)
  (inform-button-action-begins
   (get-button-for-icon *primary-mode*))
  (let ((object (interactive-create *primary-mode* object)))
    (if object
	(set-undo-operation-description-to (get-create-description object))
      (pop-undo-stack)))
  (inform-button-action-ends
   (get-button-for-icon *primary-mode*))
  (set-current-focus-to-newest-history-entry)
  (set-object-modes)    
  (refresh))

;;;
;;;
;;;

(defun point-acceptable-p (point)
  (let ((cf (get-current-transparency)))
    (and (eq cf (on-transparency point))
	 (current-focus-on-newest-history-entry-p)
	 (=> *point-mode* 
	     (or (typep point (type-of *point-mode*))
		 (change-status-applicable-p point *point-status*)))
	 (fully-visible-p point))))

;;;
;;;
;;;

(define-presentation-to-command-translator create-transparency
    (blank-area com-create-object-from-position visco
		:gesture :select
		:documentation ((stream) (format stream "Create ~A" *primary-mode*))
		:tester (()
			 (and *primary-mode*
			      (typep *primary-mode* 'gui-transparency)))
		:echo nil :maintain-history nil)
  (x y)
  (with-visco-frame (visco)
    (multiple-value-bind (x y)
	(xy-to-grid x y)
      (stream-set-pointer-position (get-frame-pane visco 'display) x y)
      (list x y))))

(define-presentation-to-command-translator create-object-from-point
    (gui-point com-create-object-from-point visco
	       :documentation ((object stream)
			       (format stream "Create ~A From ~A On ~A"
				       *primary-mode*
				       object
				       (with-visco-frame (visco)
					 (current-transparency visco))))
	       :gesture :create
	       :tester ((object) 
			(and *primary-mode*
			     (not (typep *primary-mode* 'gui-point))
			     (point-acceptable-p object)))
	       :echo nil :maintain-history nil)
  (object)
  (list object))


(define-presentation-to-command-translator create-object-on-transparency-or-enclosure
    ((or gui-transparency gui-enclosure) com-create-object-from-position visco
					 :gesture :select
					 :documentation ((object stream)
							 (if (typep *primary-mode* 'gui-transparency)
							     (format stream "Create ~A" *primary-mode*)
							   (format stream "Create ~A On ~A"
								   *primary-mode*
								   (on-transparency object))))
					 :tester ((x y object)
						  (let ((cf (get-current-transparency)))
						    (and *primary-mode*
							 (current-focus-on-newest-history-entry-p)
							 cf
							 #| (=> (typep *primary-mode* '(or gui-line gui-chain-or-polygon))
							 *point-mode*) |#
							 (=> (typep object 'gui-transparency)					
							     (=> *point-mode*
								 (and (point-truly-inside-box-p* x y cf)
								      (eq object cf))))
							 (=> *point-mode*
							     (status-of-point-object-ok-p *point-status* 
											  (typecase *point-mode*
											    (gui-marble 'marble)
											    (gui-nail 'nail))
											  nil))
							 (=> (typep object 'gui-enclosure)
							     (eq (on-transparency object) cf))
							 (=> (typep *point-mode* 'gui-marble)
							     (find-if #'(lambda (obj)
									  (and (typep obj 'gui-enclosure)
									       (asg-inside-p* x y obj)))
								      (transparency-query-objects-and-enclosures cf))))))
					 :echo nil :maintain-history nil)
  (x y)
  (with-visco-frame (visco)
    (multiple-value-bind (x y)
	(xy-to-grid x y)
      (stream-set-pointer-position (get-frame-pane visco 'display) x y)
      (list x y))))

;;;
;;;
;;;

(defgeneric get-history-entry-for (dummy &key &allow-other-keys) ;v.h. no &rest
  (:method-combination standard))

(defmethod get-history-entry-for :around ((dummy gui-visco-object) &key (register-p t) description )
  (let ((entry (call-next-method)))
    (when register-p
      (register entry))
    (when description
      (setf (operation-descr entry)
	description))
    entry))

(defmethod get-history-entry-for ((dummy gui-transparency) &key name x1 y1 x2 y2)
  (make-constructor-history-entry 
   ""
   `(create ,dummy
	    :x1 ,x1 :y1 ,y1 
	    :x2 ,x2 :y2 ,y2
	    :name ,(or name (get-next-name)))
   nil))

(defmethod get-history-entry-for ((dummy gui-point) &key status name transparency x y)
  (make-constructor-history-entry 
   ""
   `(create ,dummy
	    :transparency (find-named-visco-object ,(name transparency))
	    :x ,x
	    :y ,y
	    :status ',status
	    :name ,(or name (get-next-name)))
   (list transparency)))

(defmethod get-history-entry-for ((dummy gui-line) &key status name transparency p1 p2 ignore-disjoint-and-intersects-component-relations-p)
  (make-constructor-history-entry 
   ""
   `(create ,dummy
	    :transparency (find-named-visco-object ,(name transparency))
	    :p1 (find-named-visco-object ,(name p1))
	    :p2 (find-named-visco-object ,(name p2))
	    :ignore-disjoint-and-intersects-component-relations-p ,ignore-disjoint-and-intersects-component-relations-p
	    :status ',status
	    :name ,(or name (get-next-name)))
   (list p1 p2 transparency)))

(defmethod get-history-entry-for ((dummy gui-drawn-enclosure) &key opaque-p pattern-id 
								   ink-rgb-list
								   negated-p 
								   name transparency pointlist)
  (make-constructor-history-entry
   ""
   `(create ,dummy
	    :name ,(or name (get-next-name))
	    :transparency (find-named-visco-object ,(name transparency))
	    :pattern-id ,(or pattern-id (get-next-pattern-id))
	    :ink-rgb-list (quote ,(or ink-rgb-list
				      (calculate-color-for-drawn-enclosure dummy)))
	    :negated-p ,negated-p
	    :opaque-p ,opaque-p
	    :pointlist ',pointlist)
   (list transparency)))

(defmethod get-history-entry-for ((dummy gui-chain-or-polygon)
				  &key status 
				       name transparency segments
				       ignore-disjoint-and-intersects-component-relations-p)
  (make-constructor-history-entry 
   ""
   `(create ,dummy
	    :transparency (find-named-visco-object ,(name transparency))
	    :ignore-disjoint-and-intersects-component-relations-p ,ignore-disjoint-and-intersects-component-relations-p
	    :segments (mapcar #'find-named-visco-object
			      ',(mapcar #'name 
					(reverse segments)))
	    :status ',status
	    :name ,(or name (get-next-name)))
   (cons transparency segments)))

;;;
;;;
;;;

(defmethod create-and-execute-history-entry ((obj gui-visco-object) &rest args)
  (multiple-value-bind (obj history-entry)
      (execute (apply #'get-history-entry-for obj args))
    (if obj
	(progn
	  (setf (operation-descr history-entry)
	    (get-create-description obj))
	  obj)
      (delete-history-entry history-entry))
    (set-current-focus-to-newest-history-entry)
    (redisplay-frame-pane (with-visco-frame (visco) visco) 'control)
    obj))

;;;
;;;
;;;


(defgeneric create (obj &key &allow-other-keys) ;v.h. no &rest
  (:method-combination standard))

(defmethod create :around ((obj gui-visco-object) &rest args &key name)
  (declare (ignore args))
  (let ((new (call-next-method)))
    (when (and new (not (eq new 'not-applicable)))
      (when name
	(setf (name new) name))
      new)))

(defmethod create ((obj gui-transparency) &key x1 y1 x2 y2 &allow-other-keys)
  (apply-create-transparency (get-current-query) x1 y1 x2 y2))

(defmethod create ((obj gui-nail) &key status transparency x y)
  (apply-create-nail transparency status x y nil))

(defmethod create ((obj gui-marble) &key status transparency x y)
  (apply-create-marble transparency status x y nil))

(defmethod create ((obj gui-origin) &key status transparency x y)
  (apply-create-origin transparency status x y nil))

(defmethod create ((obj gui-rubberband) &key status transparency p1 p2 ignore-disjoint-and-intersects-component-relations-p)
  (apply-create-rubberband transparency status p1 p2 :ignore-disjoint-and-intersects-component-relations-p ignore-disjoint-and-intersects-component-relations-p))

(defmethod create ((obj gui-beam) &key status transparency p1 p2 ignore-disjoint-and-intersects-component-relations-p)
  (apply-create-beam transparency status p1 p2 :ignore-disjoint-and-intersects-component-relations-p ignore-disjoint-and-intersects-component-relations-p))

(defmethod create ((obj gui-atomic-rubberband) &key status transparency p1 p2 ignore-disjoint-and-intersects-component-relations-p)
  (apply-create-atomic-rubberband transparency status p1 p2 :ignore-disjoint-and-intersects-component-relations-p ignore-disjoint-and-intersects-component-relations-p))

(defmethod create ((obj gui-atomic-<=-rubberband) &key status transparency p1 p2 ignore-disjoint-and-intersects-component-relations-p)
  (apply-create-atomic-<=-rubberband transparency status p1 p2 :ignore-disjoint-and-intersects-component-relations-p ignore-disjoint-and-intersects-component-relations-p))

(defmethod create ((obj gui-atomic->=-rubberband) &key status transparency p1 p2 ignore-disjoint-and-intersects-component-relations-p)
  (apply-create-atomic->=-rubberband transparency status p1 p2 :ignore-disjoint-and-intersects-component-relations-p ignore-disjoint-and-intersects-component-relations-p))

(defmethod create ((obj gui-polygon) &key status transparency segments ignore-disjoint-and-intersects-component-relations-p)
  (apply-create-polygon transparency status segments :ignore-disjoint-and-intersects-component-relations-p ignore-disjoint-and-intersects-component-relations-p))

(defmethod create ((obj gui-chain) &key status transparency segments ignore-disjoint-and-intersects-component-relations-p)
  (apply-create-chain transparency status segments :ignore-disjoint-and-intersects-component-relations-p ignore-disjoint-and-intersects-component-relations-p))

(defmethod create ((obj gui-drawn-enclosure) &key negated-p ink-rgb-list pattern-id opaque-p transparency pointlist)
  (let* ((pointlist2
	  (mapcar #'(lambda (point)
		      (p (first point)
			 (second point)))
		  (transform-xy-list pointlist)))
	 (segments
	  (mapcar #'(lambda (p1 p2)
		      (l p1 p2))
		  
		  (cons (first (last pointlist2))
			pointlist2)
		  pointlist2)))
    (apply-create-drawn-enclosure transparency segments opaque-p :pattern-id pattern-id
				  :ink-rgb-list ink-rgb-list
				  :negated-p negated-p)))

;;;
;;;
;;;

(defmethod set-point-attributes ((dummy gui-point) (transparency gui-transparency) x y)
  (setf (x dummy) x
	(y dummy) y
	(on-transparency dummy) transparency
	(part-of dummy) nil
	(status dummy) *point-status*)
  dummy)

(defmethod set-line-attributes ((dummy gui-line) (transparency gui-transparency) (p1 gui-point) (p2 gui-point))
  (setf (p1 dummy) p1
	(p2 dummy) p2
	(on-transparency dummy) transparency
	(part-of dummy) nil
	(status dummy) *segment-status*)
  (calculate-bounding-box dummy)
  dummy)

(defmethod set-chain-or-polygon-attributes ((dummy gui-chain-or-polygon) (transparency gui-transparency) (segments list))
  (setf (segments dummy) segments
	(on-transparency dummy) transparency
	(part-of dummy) nil
	(status dummy) *chain-or-polygon-status*
	(point-list dummy) 
	(append (mapcar #'p1 segments)
		(list (p2 (first (last segments))))))
  (calculate-centroid dummy)
  (calculate-bounding-box dummy)
  dummy)

;;;
;;;
;;;

(defclass no-object ()
  nil)

(defclass no-point (no-object geom-point gui-visco-object)
  nil)

(defclass no-segment (no-object geom-line gui-visco-object)
  nil)


(defmethod draw ((obj no-point) stream &rest args)
  (declare (ignore args))
  (with-output-recording-options (stream :draw t :record nil)
    (draw-text* stream "?" (x obj) (y obj) :ink +flipping-ink+)))


(defmethod draw ((obj no-segment) stream &rest args)
  (declare (ignore args))
  (with-output-recording-options (stream :draw t :record nil)
    (draw-line* stream
		(x (p1 obj)) (y (p1 obj))
		(x (p2 obj)) (y (p2 obj))
		:ink +flipping-ink+
		:line-dashes '(1 1))))

(defparameter *no-point-mode* (make-instance 'no-point :dont-initialize t))

(defparameter *no-segment-mode* (make-instance 'no-segment :dont-initialize t))

;;;
;;;
;;;

(defmethod dummy-create ((object no-point) &key x y)
  (let* ((dummy (get-and-lock-dummy (type-of object))))
    (setf (x dummy) x
	  (y dummy) y)
    dummy))

(defmethod dummy-create ((object no-segment) &key p1 p2)
  (let* ((dummy (get-and-lock-dummy (type-of object))))
    (setf (p1 dummy) p1
	  (p2 dummy) p2)
    dummy))

;;;
;;;
;;;

(defmethod dummy-create ((object gui-origin) &key x y)
  (let* ((transparency (get-current-transparency))
	 (dummy (get-and-lock-dummy (type-of object))))
    (if (create-origin-applicable-p transparency *point-status* x y nil)
	(set-point-attributes dummy transparency x y)
      (progn (release-dummy dummy) nil))))

(defmethod dummy-create ((object gui-nail) &key x y)
  (let* ((transparency (get-current-transparency))
	 (dummy (get-and-lock-dummy (type-of object))))
    (if (create-nail-applicable-p transparency *point-status* x y nil)
	(set-point-attributes dummy transparency x y)
      (progn (release-dummy dummy) nil))))

(defmethod dummy-create ((object gui-marble) &key x y)
  (let* ((transparency (get-current-transparency))
	 (dummy (get-and-lock-dummy (type-of object))))
    (if (create-marble-applicable-p transparency *point-status* x y nil)
	(set-point-attributes dummy transparency x y)
      (progn (release-dummy dummy) nil))))

(defmethod dummy-create ((object gui-rubberband) &key p1 p2)
  (let* ((transparency (get-current-transparency))
	 (dummy (get-and-lock-dummy (type-of object))))
    (if (create-rubberband-applicable-p transparency *segment-status* p1 p2)
	(set-line-attributes dummy transparency p1 p2)
      (progn (release-dummy dummy) nil))))

(defmethod dummy-create ((object gui-atomic-rubberband) &key p1 p2)
  (let* ((transparency (get-current-transparency))
	 (dummy (get-and-lock-dummy (type-of object))))
    (if (create-atomic-rubberband-applicable-p transparency *segment-status* p1 p2)
	(set-line-attributes dummy transparency p1 p2)
      (progn (release-dummy dummy) nil))))

(defmethod dummy-create ((object gui-atomic-<=-rubberband) &key p1 p2)
  (let* ((transparency (get-current-transparency))
	 (dummy (get-and-lock-dummy (type-of object))))
    (if (create-atomic-<=-rubberband-applicable-p transparency *segment-status* p1 p2)
	(set-line-attributes dummy transparency p1 p2)
      (progn (release-dummy dummy) nil))))

(defmethod dummy-create ((object gui-atomic->=-rubberband) &key p1 p2)
  (let* ((transparency (get-current-transparency))
	 (dummy (get-and-lock-dummy (type-of object))))
    (if (create-atomic->=-rubberband-applicable-p transparency *segment-status* p1 p2)
	(set-line-attributes dummy transparency p1 p2)
      (progn (release-dummy dummy) nil))))

(defmethod dummy-create ((object gui-beam) &key p1 p2)
  (let* ((transparency (get-current-transparency))
	 (dummy (get-and-lock-dummy (type-of object))))
    (if (create-beam-applicable-p transparency *segment-status* p1 p2)
	(set-line-attributes dummy transparency p1 p2)
      (progn (release-dummy dummy) nil))))

(defmethod dummy-create ((object gui-chain) &key segments)
  (let* ((transparency (get-current-transparency))
	 (dummy (get-and-lock-dummy (type-of object))))
    (if (and (segment-list-ok-p segments 'chain nil)
	     (status-of-chain-or-polygon-object-ok-p *chain-or-polygon-status* 'chain segments))
	#| (create-chain-applicable-p transparency *chain-or-polygon-status* segments) |#
      (set-chain-or-polygon-attributes dummy transparency segments)
      (progn (release-dummy dummy) nil))))

;;;
;;;
;;;

(defun get-and-lock-dummy (obj-symbol)
  (let ((obj2
	 (find-if #'(lambda (obj)
		      (and (eq (type-of obj) obj-symbol)
			   (not (eq (dummy-p obj) 'locked))))
		  *dummies*)))
    (if obj2
	(progn
	  (lock-dummy obj2)
	  obj2)
      (error "No more dummies!"))))

(defun lock-dummy (dummy)
  (setf (dummy-p dummy) 'locked))	

(defun release-dummy (dummy)
  (setf (dummy-p dummy) t))

(setf *dummies*    
  (apply #'append 
	 (append (loop as i from 1 to 5 collect
		       (list (make-instance 'gui-origin :dummy-p t  :dont-initialize t)
			     (make-instance 'gui-marble :dummy-p t  :dont-initialize t)
			     (make-instance 'gui-nail :dummy-p t  :dont-initialize t)))
		 
		 (loop as i from 1 to 5 collect
		       (list
			(make-instance 'gui-beam :dummy-p t :bounding-box-p t :dont-initialize t)
			(make-instance 'gui-rubberband :dummy-p t :bounding-box-p t :dont-initialize t)
			(make-instance 'gui-atomic-rubberband :dummy-p t :bounding-box-p t :dont-initialize t)
			(make-instance 'gui-atomic-<=-rubberband :dummy-p t :bounding-box-p t :dont-initialize t)
			(make-instance 'gui-atomic->=-rubberband :dummy-p t :bounding-box-p t :dont-initialize t)))
		 
		 (loop as i from 1 to 3 collect
		       (list 
			(make-instance 'gui-chain :dummy-p t :bounding-box-p t :dont-initialize t)
			(make-instance 'gui-polygon :dummy-p t :bounding-box-p t :dont-initialize t)))
		 
		 (loop as i from 1 to 3 collect
		       (list 
			(make-instance 'no-point :dummy-p t :dont-initialize t)
			(make-instance 'no-segment :dummy-p t :bounding-box-p t :dont-initialize t))))))


(dolist (dummy *dummies*)
  (when (typep dummy 'gui-query-object)
    (setf (object (status-label dummy)) dummy
	  (query dummy) nil)))

;;;
;;;
;;;

(defmethod interactive-create ((obj gui-transparency) (point list) &key &allow-other-keys)
  (let* ((x1 (first point))
	 (y1 (second point))
	 (x2 x1)
	 (y2 y1))
    (with-visco-frame (visco)
      (let ((stream (get-frame-pane visco 'display)))
	(labels ((draw-it ()
		   (draw-rectangle* stream
				    x1 y1
				    x2 y2
				    :ink +flipping-ink+
				    :filled nil)))
	  (with-output-recording-options (stream :draw t :record nil)
	    (tracking-pointer (stream)
	      (:pointer-motion (x y)
			       (multiple-value-bind (x y)
				   (xy-to-grid x y)
				 (draw-it)
				 (setf x2 x
				       y2 y)
				 (draw-it)))
              (:keyboard (character)
                         (when (and (characterp character)
                                    (char-equal character #\q))
                           (return)))
	      (:pointer-button-press (event)
				     (if (= (pointer-event-button event)
					    +pointer-right-button+)
					 (return)
				       (let ((obj (create-and-execute-history-entry *primary-mode* 
										    :x1 x1 :y1 y1 :x2 x2 :y2 y2
										    )))
					 (if (not obj)
					     (beep)
					   (return obj))))))))))))

(defmethod interactive-create ((obj gui-point) (point list) &key create-component-points-p &allow-other-keys)
  (interactive-point-creator obj point :create-component-points-p create-component-points-p))

(defmethod interactive-create ((obj no-point) (point list) &key create-component-points-p &allow-other-keys)
  (interactive-point-creator obj point :create-component-points-p create-component-points-p))

(defmethod interactive-create ((obj gui-line) point &key create-component-lines-p &allow-other-keys)
  (interactive-line-creator point :create-component-lines-p create-component-lines-p))

(defmethod interactive-create ((obj no-segment) point &key create-component-lines-p &allow-other-keys)
  (interactive-line-creator point :create-component-lines-p create-component-lines-p))

(defmethod interactive-create ((obj gui-chain-or-polygon) point &key &allow-other-keys)
  (interactive-chain-or-poly-creator point))

(defmethod interactive-create ((obj drawn-enclosure) (point list) &key &allow-other-keys)
  (interactive-drawn-enclosure-creator point))

;;;
;;;
;;;


(defmethod interactive-point-creator ((obj geom-point) (point list) &key create-component-points-p 
				      &allow-other-keys) ; geom-point wg. no-point
  (with-visco-frame (visco)
    (let ((stream (get-frame-pane visco 'display)))
      (let ((point nil)
	    (object nil))
	(with-output-recording-options (stream :draw t :record nil)
	  (tracking-pointer (stream)
	    (:pointer-motion (x y)
			     (multiple-value-bind (x y)
				 (xy-to-grid x y)
			       (with-input-context ('gui-point :override t)
				 nil
				 (let ((presentation
					(find-innermost-applicable-presentation *input-context* 
										stream x y
										:frame visco)))
				   (unhighlight-highlighted-presentation stream)
				   (setf object nil)
				   (when presentation
				     (let ((po (presentation-object presentation)))
				       (when (point-acceptable-p po)
					 (set-highlighted-presentation stream presentation)
					 (setf object po))))))
			       (when (and point (dummy-p point))
				 (draw point stream :handling-active t :output-recording nil))
			       (setf point
				 (or object (dummy-create (or *point-mode* *no-point-mode*) :x x :y y)))
			       (when (and point (dummy-p point))
				 (release-dummy point)
				 (draw point stream :handling-active t :output-recording nil))))
            (:keyboard (character)
                       (when (and (characterp character)
                                  (char-equal character #\q))
                         (return)))
	    (:pointer-button-press (event)
				   (if (= (pointer-event-button event)
					  +pointer-right-button+)
				       (return)
				     (if (and point (not (typep point 'no-point))
					      (=> (not (dummy-p point)) create-component-points-p))
					 (if (not (dummy-p point))
					     (return point)
					   (let ((point
						  (create-and-execute-history-entry point 
										    :transparency (get-current-transparency)
										    :status *point-status*
										    :x (x point) :y (y point))))
					     (if (not point)
						 (beep)
					       (progn
						 (return point)))))
				       (beep))))))))))

(defmethod interactive-line-creator ((p1 list) &key create-component-lines-p)
  (let ((obj (interactive-create (or *point-mode* *no-point-mode*) p1
				 :create-component-points-p t)))
    (when obj
      (inform-button-action-ends 
       (get-button-for-icon *point-mode*))
      (multiple-value-bind (res event key)	    
	  (interactive-line-creator obj
				    :create-component-lines-p create-component-lines-p)
	(unless res
	  (delete-object obj))
	(values res event key)))))

(defmethod interactive-line-creator ((p1 geom-point) &key create-component-lines-p)
  (with-visco-frame (visco)
    (let ((stream (get-frame-pane visco 'display)))
      (redisplay-frame-pane visco 'display)
      (when p1	
	(with-output-recording-options (stream :draw t :record nil)
	  (let ((line nil)
		(lastline nil)
		(line-valid nil)
		(p2 nil)
		(lastp2 nil)
		(object nil))
	    
	    (labels ((draw-it (line)
		       (when (dummy-p line)
			 (draw line stream :handling-active t :output-recording nil
			       :draw-component-objects nil))
		       (when (dummy-p (p2 line))
			 (draw (p2 line) stream :handling-active t :output-recording nil)))
		     (erase-it ()
		       (draw-it line))
		     (release-all ()
		       (when (and p2 (dummy-p p2))
			 (release-dummy p2))
		       (when (and line (dummy-p line))
			 (release-dummy line))
		       (erase-it)
		       (unhighlight-highlighted-presentation stream)))
	      
	      (tracking-pointer (stream :context-type 'gui-point)
		(:pointer-motion (x y)
				 (multiple-value-bind (x y)
				     (xy-to-grid x y)
				   (with-input-context ('gui-point :override t)
				     nil
				     (let ((presentation
					    (find-innermost-applicable-presentation *input-context* 
										    stream x y
										    :frame visco)))
				       (unhighlight-highlighted-presentation stream)
				       (setf object nil)
				       (when presentation
					 (let ((po (presentation-object presentation)))
					   (when (point-acceptable-p po)
					     (set-highlighted-presentation stream presentation)
					     (setf object po))))))
				   (let ((newp2 (or object
						    (dummy-create (or *point-mode* *no-point-mode*)
								  :x x :y y))))
				     (setf line-valid nil)
				     (when newp2
				       (let ((newline
					      (or (when create-component-lines-p 
						    (let ((master (get-direct-common-master p1 newp2)))
						      (when (and (null (rest master))
								 (first master)
								 (not (dummy-p (first master))))
							(first master))))
						  (dummy-create (or (when (not (typep newp2 'no-point))
								      *segment-mode*)
								    *no-segment-mode*)
								:p1 p1 :p2 newp2))))
					 (if newline
					     (progn
					       (setf line-valid 
						 (not (typep newline 'no-segment)))
					       (setf lastline line
						     line newline
						     lastp2 p2
						     p2 newp2)
					       (when (and lastline
							  (dummy-p lastline))
						 (release-dummy lastline))
					       (when (and lastp2 
							  (dummy-p lastp2))
						 (release-dummy lastp2))
					       (when line
						 (when lastline (draw-it lastline))
						 (unless (eq lastline line) 
						   (draw-it line))))
					   (when (dummy-p newp2)
					     (release-dummy newp2))))))))
		(:keyboard (character)
                           (flet ((exit (val)
                                    (release-all)
                                    (return (values nil nil val))))
                             (if (and (characterp character)
                                      (char-equal character #\q))
                               (exit :quit)
			       (when (typep character 'keyboard-event)
			         (let ((keysym (keyboard-event-key-name character)))
			           (when (eq keysym :rubout)
                                     (exit keysym)))))))
		(:pointer-button-press (event)
				       (if (= (pointer-event-button event)
					      +pointer-right-button+)
					   (progn
					     (release-all)
					     (return (values nil event nil)))
					 (when p2
					   (if (and line line-valid)
					       (progn		
						 (release-all)
						 (when (dummy-p line)
						   (setf p2 
						     (if (dummy-p p2)
							 (create-and-execute-history-entry p2 :x (x p2) :y (y p2)
											   :transparency (get-current-transparency)
											   :status (status p2))
						       p2)
						     line 
						     (create-and-execute-history-entry line
										       :transparency (get-current-transparency)
										       :p1 p1 :p2 p2
										       :status *segment-status*
										       :ignore-disjoint-and-intersects-component-relations-p
										       *ignore-disjoint-and-intersects-component-relations-p*)))
						 (return
						   (values line event nil)))
					     (beep)))))))))))))

(defmethod interactive-chain-or-poly-creator ((p1 list))
  (let ((obj (interactive-create (or *point-mode* *no-point-mode*) p1
				 :create-component-points-p t)))
    (when obj
      (inform-button-action-ends 
       (get-button-for-icon *point-mode*))
      (let ((res (interactive-chain-or-poly-creator obj)))
	(unless res 
	  (delete-object obj))
	res))))


(defmethod interactive-chain-or-poly-creator ((p1 gui-point))
  (with-visco-frame (visco)
    (let ((first-point p1)
	  (lines nil)
	  (objects (copy-list (visco-objects (get-current-query)))))
      
      (labels ((delete-it (object)				 
		 (unless (member object objects)
		   (delete-object object :delete-component-objects-p nil))
		 (dolist (p (get-direct-components object))
		   (when (and (not (member p objects))
			      (primary-p p))
		     (delete-object p)))
		 (set-current-focus-to-newest-history-entry)
		 (redisplay-frame-pane visco 'display)
		 (redisplay-frame-pane visco 'control))
               (exit-code ()
                 (mapc #'(lambda (line)
                           (unless (member line objects)
                             (delete-object line :delete-component-objects-p nil))
                           (dolist (p (get-direct-components line))
                             (unless (member p objects)
                               (delete-object p))))
                       lines)
                 (when (dummy-p first-point)
                   (release-dummy first-point))
                 (return-from interactive-chain-or-poly-creator)))
	
	(loop	
	  
	  (multiple-value-bind (line event key)
	      (interactive-create (or *segment-mode* 
				      *no-segment-mode*)
				  p1 
				  :create-component-lines-p t)
	    
	    (redisplay-frame-pane visco 'display)
	    
	    (when (dummy-p first-point)
	      (lock-dummy first-point))

	    (if key
		(case key
		  (:rubout
		   (let* ((delete (pop lines))
			  (first (first lines)))
		     (if delete
			 (progn
			   (delete-it delete)
			   (if first
			       (if (point-=-p p1 (p2 delete))
				   (setf p1 (p1 delete))
				 (setf p1 (p2 delete)))
			     (setf p1 first-point)))
		       (beep))))
                  (:quit (exit-code)))
                   
	        (let ((button (pointer-event-button event))
                      #+:mcl 
                      (modifier (event-modifier-state event)))
		  (cond (
                         #+:allegro (= button +pointer-middle-button+)
                         #+:mcl (and (= button +pointer-left-button+)
                                     (= modifier +shift-key+))
                         (if (and line
                                  (not
				    (member line lines)))
			    (progn
			      (push line lines)
			      (let ((obj
				     (create-and-execute-history-entry *primary-mode*
								       :transparency (get-current-transparency)
								       :segments (reverse lines)
								       :status *chain-or-polygon-status*
								       :ignore-disjoint-and-intersects-component-relations-p
								       *ignore-disjoint-and-intersects-component-relations-p*)))
			        (if obj
                                  (progn
                                    (when (dummy-p first-point)
                                      (release-dummy first-point))
                                    (return obj))
				  (progn 
				    (pop lines)
				    (unless (member line lines)
				      (delete-it line))
				    (beep)))))
			    (when line
			      (unless (member line lines)
			        (delete-it line))
			      (beep))))
                        ((= button +pointer-left-button+)
		         (if (and line
				  (not
				   (member line lines)))
			   (progn
			     (push line lines)
			     (let ((dummy 
				    (dummy-create (icon *button-chain*)
						  :segments (reverse lines))))
			       (if (=> (> (length lines) 1)
				       dummy)
                                 (progn
                                   (when dummy (release-dummy dummy))
                                   (if (eq p1 (p1 line))
                                     (setf p1 (p2 line))
                                     (setf p1 (p1 line))))
				 (progn 
				   (delete-it (pop lines))
				   (beep)))))
			   (when line
			     (beep))))
		        ((= button +pointer-right-button+)
                         (exit-code)))))))))))


(defmethod interactive-drawn-enclosure-creator ((p1 list))
  (with-visco-frame (visco)
    (let* ((stream (get-frame-pane visco 'display))
	   (pointx (first p1))
	   (pointy (second p1))
	   (points (list pointx pointy)))
      (flet ((draw-it ()
	       (if (rest (rest points))
		   (draw-polygon* stream 
				  (cons pointx (cons pointy points))
				  :filled t
				  :ink +flipping-ink+
				  :closed t)
		 (draw-line* stream 
			     (first p1) (second p1)
			     pointx pointy
			     :ink +flipping-ink+))))
	(with-output-recording-options (stream :draw t :record nil)
	  (tracking-pointer (stream)
	    (:pointer-motion (x y)
			     (multiple-value-bind (x y)
				 (xy-to-grid x y)
			       (when (inside-p* x y (get-current-transparency))
				 (draw-it)
				 (setf pointx x
				       pointy y)
				 (draw-it))))
	    (:keyboard (character)
                       (if (and (characterp character)
                                (char-equal character #\q))
                         (return)
		         (when (typep character 'keyboard-event)
			   (let ((keysym (keyboard-event-key-name character)))
			     (when (eq keysym 
				       :rubout)
			       (if (rest (rest points))
				 (progn
				   (draw-it)
				   (pop points)
				   (pop points)
				   (draw-it))			       
			         (beep)))))))
	    (:pointer-button-press (event)
				   (let ((button (pointer-event-button event))
                                         #+:mcl 
                                         (modifier (event-modifier-state event)))
				     (cond ((= button +pointer-right-button+)
					    (return))
					   (
                                            #+:allegro 
                                            (= button +pointer-middle-button+)
                                            #+:mcl
                                            (and (= button +pointer-left-button+)
                                                 (= modifier +shift-key+))
					    (push pointy points)
					    (push pointx points)
					    (let* ((enclosure
						    (create-and-execute-history-entry 
						     *primary-mode* 
						     :transparency (get-current-transparency)
						     :pointlist points
						     :opaque-p *opaque-enclosures-p*)))
					      (if enclosure
						  (return enclosure)
						(progn 
						  (pop points)
						  (pop points)
						  (beep)))))
                                           ((= button +pointer-left-button+)
					    (unless (cddr points)
					      (draw-it))
					    (push pointy points)
					    (push pointx points)))))))))))

