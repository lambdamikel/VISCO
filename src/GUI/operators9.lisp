;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: GUI; Base: 10 -*-

(in-package gui)

(define-presentation-translator execute-selected-command
    (gui-object command visco :tester ((object)
				       (and *operator-mode*
					    (first-argument-acceptable-p (command *operator-mode*) 
									 object)))
		:documentation ((stream)
				(and *operator-mode* 
				     (format stream "~A" (first (doc *operator-mode*)))))
		:gesture :apply-operator)
  (object)
  `(,(command *operator-mode*) ,object))


(defun command-enabled-p (command-name-symbol)
  (let ((query (get-current-query)))
    (and query
	 (some #'(lambda (object1)
		   (and (first-argument-acceptable-p command-name-symbol
						     object1)
			(=> (binary-command-p command-name-symbol)
			    (some #'(lambda (object2)
				      (second-argument-acceptable-p command-name-symbol
								    object1
								    object2))
				  (remove object1 (visco-objects query))))))
	       (visco-objects query)))))


(defun binary-command-p (name)
  (member name '(com-create-intersection-point com-create-intersection-point*
		 com-set-relative-orientation-constraint com-set-relative-orientation-constraint*)))

(defun accept-first-command-arg ()
  (with-visco-frame (visco)
    (redisplay-frame-pane visco 'display)
    (redraw-buttons)
    (accept `(and gui-object 
		  (satisfies first-arg-ok-p))
	    :prompt "Select First Argument")))

;;;
;;;
;;;

(defmethod first-argument-acceptable-p ((op symbol) (obj gui-object))
  nil)

(defmethod second-argument-acceptable-p ((op symbol)
					 (obj1 gui-object)
					 (obj2 gui-object))
  nil)

(defun first-arg-ok-p (obj)
  (and *operator-mode*
       (first-argument-acceptable-p (command *operator-mode*) obj)))

(defvar *first-arg* nil)

(defun second-arg-ok-p (obj)
  (and *operator-mode*
       (second-argument-acceptable-p (command *operator-mode*) *first-arg* obj)))

;;;
;;;
;;;

(define-visco-command (com-create-centroid :name "Create Centroid")
    ((obj `(and gui-object
		(satisfies first-arg-ok-p))))
  (register-undo-information nil)
  (multiple-value-bind (res he) 
      (execute 
       (register-constructor-history-entry 
	""
	`(let ((res (apply-create-centroid (find-named-visco-object ,(name obj))
					   ',*point-status* ,*point-mode*
					   :name ,(get-next-name))))
	   (when (and res (not (eq res 'not-applicable)))
	     res))
	(list obj)))
    (if res
	(let ((descr
	       (format nil "Create Centroid ~A Of ~A" res obj)))
	  (setf (operation-descr he) descr)
	  (set-undo-operation-description-to descr)
	  (set-current-focus-to-newest-history-entry))
      (progn (beep) (delete-history-entry he)
	     (pop-undo-stack))))
  (refresh))

(defmethod first-argument-acceptable-p ((op (eql 'com-create-centroid)) (obj gui-object))
  (and *point-status*
       *point-mode*
       (create-centroid-applicable-p obj *point-status* *point-mode*)))

(defmethod command-enabled ((command (eql 'com-create-centroid)) (frame visco)
                            #+:mcl &optional #+:mcl unused)
  #+:mcl (declare (ignore unused))
  (command-enabled-p command))

(defmethod command-enabled ((command (eql 'com-create-centroid*)) (frame visco)
                            #+:mcl &optional #+:mcl unused)
  #+:mcl (declare (ignore unused))
  (command-enabled-p 'com-create-centroid))

(define-visco-command (com-create-centroid* :name "Create Centroid")
    nil
  (let ((*operator-mode* *button-create-centroid*))
    (com-create-centroid (accept-first-command-arg)))
  (redraw-buttons))

;;;
;;;
;;;

(define-visco-command (com-create-intersection-point :name "Create Intersection Point")
    ((obj1 `(and gui-object 
		 (satisfies first-arg-ok))))
  (let ((*first-arg* obj1))
    (let ((obj2 
	   (accept `(and gui-object 
			 (satisfies second-arg-ok-p))
		   :prompt " Select Second Argument")))
      
      (register-undo-information nil)
      (multiple-value-bind (res he) 
	  (execute 
	   (register-constructor-history-entry 
	    ""
	    `(let ((res (apply-create-intersection-point
			 (find-named-visco-object ,(name obj1))
			 (find-named-visco-object ,(name obj2))
			 ',*point-status* ,*point-mode*
			 :name ,(get-next-name))))
	       (when (and res (not (eq res 'not-applicable)))
		 res))
	    (list obj1 obj2)))
	(if res
	    (let ((descr (format nil "Create Intersection Point ~A Of ~A And ~A"
				 res obj1 obj2)))
	      (setf (operation-descr he) descr)
	      (set-undo-operation-description-to descr)
	      (set-current-focus-to-newest-history-entry))
	  (progn (beep) (delete-history-entry he)
		 (pop-undo-stack))))))
  (refresh))

(defmethod first-argument-acceptable-p ((op (eql 'com-create-intersection-point))
					(obj1 gui-object))
  (and *point-status*
       *point-mode*
       (some #'(lambda (obj2)
		 (second-argument-acceptable-p op
					       obj1 obj2))
	     (visco-objects (get-current-query)))))

(defmethod second-argument-acceptable-p ((op (eql 'com-create-intersection-point))
					 (obj1 gui-object)
					 (obj2 gui-object))
  (create-intersection-point-applicable-p obj1 obj2 
					  *point-status*
					  *point-mode*))

(defmethod command-enabled ((command (eql 'com-create-intersection-point)) (frame visco)
                            #+:mcl &optional #+:mcl unused)
  #+:mcl (declare (ignore unused))
  (command-enabled-p command))

(defmethod command-enabled ((command (eql 'com-create-intersection-point*)) (frame visco)
                            #+:mcl &optional #+:mcl unused)
  #+:mcl (declare (ignore unused))
  (command-enabled-p 'com-create-intersection-point))

(define-visco-command (com-create-intersection-point* :name "Create Intersection Point")
    nil
  (let ((*operator-mode* *button-create-intersection-point*))
    (com-create-intersection-point (accept-first-command-arg)))
  (redraw-buttons))

;;;
;;;
;;;


(define-visco-command (com-create-inner-enclosure :name "Create Inner Enclosure")
    ((obj `(and gui-object
		(satisfies first-arg-ok-p))))
  
  (register-undo-information nil)
  (multiple-value-bind (res he) 
      (execute 
       (register-constructor-history-entry 
	""
	`(let ((res (apply-create-inner-enclosure (find-named-visco-object ,(name obj))
						  ,*opaque-enclosures-p*
						  :pattern-id ,(get-next-pattern-id)
						  :ink-rgb-list (quote ,(calculate-color-for-inner-enclosure obj)))))
	   (when (and res (not (eq res 'not-applicable)))
	     res))
	(list obj)))
    (if res
	(let ((descr (format nil "Create ~A" res)))
	  (setf (operation-descr he) descr)
	  (set-undo-operation-description-to descr)
	  (set-current-focus-to-newest-history-entry))
      (progn (beep) (delete-history-entry he)
	     (pop-undo-stack))))
  (refresh))

(defmethod first-argument-acceptable-p ((op (eql 'com-create-inner-enclosure)) (obj gui-object))
  (create-inner-enclosure-applicable-p obj *opaque-enclosures-p*))


(defmethod command-enabled ((command (eql 'com-create-inner-enclosure)) (frame visco)
                            #+:mcl &optional #+:mcl unused)
  #+:mcl (declare (ignore unused))
  (command-enabled-p command))

(defmethod command-enabled ((command (eql 'com-create-inner-enclosure*)) (frame visco)
                            #+:mcl &optional #+:mcl unused)
  #+:mcl (declare (ignore unused))
  (command-enabled-p 'com-create-inner-enclosure))

(define-visco-command (com-create-inner-enclosure* :name "Create Inner Enclosure")
    nil
  (let ((*operator-mode* *button-create-inner-enclosure*))
    (com-create-inner-enclosure (accept-first-command-arg)))
  (redraw-buttons))


;;;
;;;
;;;


(define-visco-command (com-create-outer-enclosure :name "Create Outer Enclosure")
    ((obj `(and gui-object
		(satisfies first-arg-ok-p))))
  (register-undo-information nil)
  (multiple-value-bind (res he) 
      (execute 
       (register-constructor-history-entry 
	""
	`(let ((res (apply-create-outer-enclosure (find-named-visco-object ,(name obj))
						  ,*opaque-enclosures-p*
						  :ink-rgb-list (quote ,(calculate-color-for-outer-enclosure obj))
						  :pattern-id ,(get-next-pattern-id))))
	   (when (and res (not (eq res 'not-applicable)))
	     res))
	(list obj)))
    (if res
	(let ((descr (format nil "Create ~A" res)))
	  (setf (operation-descr he) descr)
	  (set-undo-operation-description-to descr)
	  (set-current-focus-to-newest-history-entry))
      (progn (beep)
	     (delete-history-entry he)
	     (pop-undo-stack))))
  (refresh))

(defmethod first-argument-acceptable-p ((op (eql 'com-create-outer-enclosure)) (obj gui-object))
  (create-outer-enclosure-applicable-p obj *opaque-enclosures-p*))


(defmethod command-enabled ((command (eql 'com-create-outer-enclosure)) (frame visco)
                            #+:mcl &optional #+:mcl unused)
  #+:mcl (declare (ignore unused))
  (command-enabled-p command))

(defmethod command-enabled ((command (eql 'com-create-outer-enclosure*)) (frame visco)
                            #+:mcl &optional #+:mcl unused)
  #+:mcl (declare (ignore unused))
  (command-enabled-p 'com-create-outer-enclosure))

(define-visco-command (com-create-outer-enclosure* :name "Create Outer Enclosure")
    nil
  (let ((*operator-mode* *button-create-outer-enclosure*))
    (com-create-outer-enclosure (accept-first-command-arg)))
  (redraw-buttons))


;;;
;;;
;;;


(define-visco-command (com-create-epsilon-enclosure :name "Create Epsilon Enclosure")
    ((obj `(and gui-object
		(satisfies first-arg-ok-p))))
  (with-visco-frame (visco)
    (with-visco-buttons-frame (buttons)
      (let ((stream (get-frame-pane visco 'display)))
	(register-undo-information nil)
	(let ((res (apply-create-epsilon-enclosure obj 0 *opaque-enclosures-p*)))
	  (if (and res (not (eq res 'not-applicable)))
	      (labels ((draw-it ()
			 (draw res stream :handling-active t)))
		(draw-it)	
		(if (tracking-pointer (stream)		      
		      (:pointer-motion (x y)
				       (multiple-value-bind (x y)
					   (xy-to-grid x y)
					 (draw-it)
					 (let ((radius
						(distance-between-xy x y obj)))
					   (when (create-epsilon-enclosure-applicable-p obj radius *opaque-enclosures-p*)
					     (setf (radius res) radius)))
					 (draw-it)
					 (redisplay-frame-pane buttons 'option-buttons)))
	              (:keyboard (character)
                                 (when (and (characterp character)
                                            (char-equal character #\q))
                                   (return nil)))
		      (:pointer-button-press (event)
					     (if (= (pointer-event-button event)
						    +pointer-right-button+)
						 (return nil)
					       (return obj))))
		    (progn
		      (delete-object res)
		      (multiple-value-bind (res he)
			  (execute 
			   (register-constructor-history-entry 
			    ""
			    `(let ((res (apply-create-epsilon-enclosure
					 (find-named-visco-object ,(name obj))
					 ,(radius res)
					 ,*opaque-enclosures-p*
					 :ink-rgb-list (quote ,(calculate-color-for-epsilon-enclosure obj))
					 :pattern-id ,(pattern-id res))))
			       (when (and res (not (eq res 'not-applicable)))
				 res))
			    (list obj)))
			(let ((descr (format nil "Create ~A" res)))
			  (setf (operation-descr he) descr)
			  (set-undo-operation-description-to descr)
			  (set-current-focus-to-newest-history-entry))))
		  (progn 
		    (delete-object res)
		    (pop (undo-stack visco)))))
	    (pop-undo-stack))))))
  (refresh))

(defmethod first-argument-acceptable-p ((op (eql 'com-create-epsilon-enclosure)) (obj gui-object))
  (create-epsilon-enclosure-applicable-p obj 0 *opaque-enclosures-p*))

(defmethod command-enabled ((command (eql 'com-create-epsilon-enclosure)) (frame visco)
                            #+:mcl &optional #+:mcl unused)
  #+:mcl (declare (ignore unused))
  (command-enabled-p command))

(defmethod command-enabled ((command (eql 'com-create-epsilon-enclosure*)) (frame visco)
                            #+:mcl &optional #+:mcl unused)
  #+:mcl (declare (ignore unused))
  (command-enabled-p 'com-create-epsilon-enclosure))

(define-visco-command (com-create-epsilon-enclosure* :name "Create Epsilon Enclosure")
    nil
  (let ((*operator-mode* *button-create-epsilon-enclosure*))
    (com-create-epsilon-enclosure (accept-first-command-arg)))
  (redraw-buttons))


;;;
;;;
;;;


(define-visco-command (com-create-inverse-drawn-enclosure :name "Create Inverse Constant Enclosure")
    ((obj `(and gui-object
		(satisfies first-arg-ok-p))))
  (register-undo-information nil)
  (multiple-value-bind (res he) 
      (execute 
       (register-constructor-history-entry
	""
	`(let ((res (apply-create-drawn-enclosure 
		     (find-named-visco-object ,(name (on-transparency obj)))
		     (list ,@(segments obj))
		     ,(opaque-p obj)
		     :ink-rgb-list (quote ,(calculate-color-for-drawn-enclosure obj))
		     :negated-p ,(not (negated-p obj)))))
	   (when (and res (not (eq res 'not-applicable)))
	     res))
	nil))
    (if res
	(let ((descr (format nil "Create Inverse ~A Of ~A" res obj)))
	  (setf (operation-descr he) descr)
	  (set-undo-operation-description-to descr)
	  (set-current-focus-to he))
      (progn (beep) (delete-history-entry he)
	     (pop-undo-stack))))
  (refresh))

(defmethod first-argument-acceptable-p ((op (eql 'com-create-inverse-drawn-enclosure)) (obj gui-object))
  (typep obj 'gui-drawn-enclosure))

(defmethod command-enabled ((command (eql 'com-create-inverse-drawn-enclosure)) (frame visco)
                            #+:mcl &optional #+:mcl unused)
  #+:mcl (declare (ignore unused))
  (command-enabled-p command))

(defmethod command-enabled ((command (eql 'com-create-inverse-drawn-enclosure*)) (frame visco)
                            #+:mcl &optional #+:mcl unused)
  #+:mcl (declare (ignore unused))
  (command-enabled-p 'com-create-inverse-drawn-enclosure))

(define-visco-command (com-create-inverse-drawn-enclosure* :name "Create Inverse Constant Enclosure")
    nil
  (let ((*operator-mode* *button-create-inverse-drawn-enclosure*))
    (com-create-inverse-drawn-enclosure (accept-first-command-arg)))
  (redraw-buttons))


;;;
;;;
;;;

(define-visco-command (com-set-semantics :name "Set Semantics")
    ((obj `(and gui-object
		(satisfies first-arg-ok-p))))
  (with-visco-frame (visco)
    (let* ((stream (get-frame-pane visco 'display))
	   (semantics 
	    (accepting-values (stream :own-window t
				      :label (format nil "Semantics Of ~A" obj))
	      (accept `((subset-completion
			 ,*os*
			 :value-key ,#'second
			 :test ,#'equal)
			:name-key ,#'(lambda (item)
				       (format nil "~A ~A ~A" (first item) (second item) (third item))))
		      :prompt "Select A Subset Of"		      
		      :stream stream
		      :view `(list-pane-view :value ,(semantics obj)
                                             :visible-items 20))))
	   (descr     
	    (if semantics
		(format nil "Set Semantics Of ~A" obj)
	      (format nil "Delete Semantics Of ~A" obj))))
      (register-undo-information descr)
      (multiple-value-bind (res he) 
	  (execute
	   (register-history-entry 
	    descr
	    (if semantics
		`(let ((res (apply-set-semantics (find-named-visco-object ,(name obj))
						 ',semantics)))
		   (when (and res (not (eq res 'not-applicable)))
		     res))
	      `(let ((res (apply-delete-semantics (find-named-visco-object ,(name obj)))))
		 (when (and res (not (eq res 'not-applicable)))
		   (reinitialize (semantics-label res)))))
	    (list obj)))
	(unless res
	  (beep)
	  (delete-history-entry he)
	  (pop-undo-stack)))))
  (refresh))

(defmethod first-argument-acceptable-p ((op (eql 'com-set-semantics)) (obj gui-object))
  (and (typep obj 'gui-query-object)
       (matches-with-database-object-p obj)))

(defmethod command-enabled ((command (eql 'com-set-semantics)) (frame visco)
                            #+:mcl &optional #+:mcl unused)
  #+:mcl (declare (ignore unused))
  (command-enabled-p command))

(defmethod command-enabled ((command (eql 'com-set-semantics*)) (frame visco)
                            #+:mcl &optional #+:mcl unused)
  #+:mcl (declare (ignore unused))
  (command-enabled-p 'com-set-semantics))

(define-visco-command (com-set-semantics* :name "Set Semantics")
    nil
  (let ((*operator-mode* *button-set-semantics*))
    (com-set-semantics (accept-first-command-arg)))
  (redraw-buttons))


;;;
;;;
;;;

(define-visco-command (com-set-at-most-constraint :name "Set At Most Constraint")
    ((obj `(and gui-object
		(satisfies first-arg-ok-p))))
  (with-visco-frame (visco)
    (let* ((stream (get-frame-pane visco 'display))
	   (at-most 
	    (accepting-values (stream :own-window t
				      :label (format nil "Set At Most Constraint Of ~A" obj))
	      (accept 'integer		      
		      :prompt "Enter At Most Constraint"		      
		      :stream stream
		      :view 'text-field-view)))
	   (descr     
	    (if at-most
		(format nil "Set At Most Constraint Of ~A To ~A" obj at-most)
	      (format nil "Delete At Most Constraint Of ~A" obj))))	      
      (register-undo-information descr)
      (multiple-value-bind (res he)
	  (execute
	   (register-history-entry 
	    descr
	    (if at-most
		`(let ((res (apply-set-at-most-constraint (find-named-visco-object ,(name obj))
							  ,at-most)))
		   (when (and res (not (eq res 'not-applicable)))
		     res))
	      `(let ((res (apply-delete-at-most-constraint
			   (find-named-visco-object ,(name obj)))))
		 (when (and res (not (eq res 'not-applicable)))
		   (reinitialize (at-most-label res)))))
	    (list obj)))
	(unless res
	  (beep)
	  (delete-history-entry he)
	  (pop-undo-stack)))))
  (refresh))


(defmethod first-argument-acceptable-p ((op (eql 'com-set-at-most-constraint)) (obj gui-object))
  (and (or (typep obj 'gui-rubberband)
	   (and (typep obj 'gui-chain-or-polygon)
		(some #'(lambda (segment)
			  (and (typep segment 'rubberband)
			       (not (typep segment 'atomic-rubberband))))
		      (segments obj))))))

(defmethod command-enabled ((command (eql 'com-set-at-most-constraint)) (frame visco)
                            #+:mcl &optional #+:mcl unused)
  #+:mcl (declare (ignore unused))
  (command-enabled-p command))

(defmethod command-enabled ((command (eql 'com-set-at-most-constraint*)) (frame visco)
                            #+:mcl &optional #+:mcl unused)
  #+:mcl (declare (ignore unused))
  (command-enabled-p 'com-set-at-most-constraint))

(define-visco-command (com-set-at-most-constraint* :name "Set At Most Constraint")
    nil
  (let ((*operator-mode* *button-set-at-most-constraint*))
    (com-set-at-most-constraint (accept-first-command-arg)))
  (redraw-buttons))


;;;
;;;
;;;


(define-visco-command (com-set-orientation-constraint :name "Set Orientation Constraint")
    ((obj `(and gui-object
		(satisfies first-arg-ok-p))))
  (let ((descr     
	 (format nil "Set Orientation Constraint Of ~A" obj)))
    (register-undo-information descr)
    (multiple-value-bind (res he)
	(execute
	 (register-history-entry 
	  descr
	  `(let ((res (apply-set-orientation-constraint (find-named-visco-object ,(name obj))
							'(0))))
	     (when (and res (not (eq res 'not-applicable)))
	       res))
	  (list obj)))
      (unless res
	(beep) 
	(delete-history-entry he)
	(pop-undo-stack))))
  (refresh))

(defmethod first-argument-acceptable-p ((op (eql 'com-set-orientation-constraint)) (obj gui-object))
  (and (set-orientation-constraint-applicable-p obj (list 0))
       (not (orientation-constraint obj))))

(defmethod command-enabled ((command (eql 'com-set-orientation-constraint)) (frame visco)
                            #+:mcl &optional #+:mcl unused)
  #+:mcl (declare (ignore unused))
  (command-enabled-p command))

(defmethod command-enabled ((command (eql 'com-set-orientation-constraint*)) (frame visco)
                            #+:mcl &optional #+:mcl unused)
  #+:mcl (declare (ignore unused))
  (command-enabled-p 'com-set-orientation-constraint))

(define-visco-command (com-set-orientation-constraint* :name "Set Orientation Constraint")
    nil
  (let ((*operator-mode* *button-set-orientation-constraint*))
    (com-set-orientation-constraint (accept-first-command-arg)))
  (redraw-buttons))


;;;
;;;
;;;

(defmethod first-argument-acceptable-p ((op (eql 'com-set-orientation-constraint-mark)) (obj gui-visco-object))
  (and (typep obj 'orientation-constraint-mixin)
       (orientation-constraint obj)))

(defmethod first-argument-acceptable-p ((op (eql 'com-set-orientation-constraint-mark)) (obj gui-orientation-arrow))
  (first-argument-acceptable-p op (object obj)))

(defmethod command-enabled ((command (eql 'com-set-orientation-constraint-mark)) (frame visco)
                            #+:mcl &optional #+:mcl unused)
  #+:mcl (declare (ignore unused))
  (command-enabled-p command))

(defmethod command-enabled ((command (eql 'com-set-orientation-constraint-mark*)) (frame visco)
                            #+:mcl &optional #+:mcl unused)
  #+:mcl (declare (ignore unused))
  (command-enabled-p 'com-set-orientation-constraint-mark))

(define-visco-command (com-set-orientation-constraint-mark* :name "Set Orientation Constraint Mark")
    nil
  (let ((*operator-mode* *button-create-orientation-constraint-mark*))
    (com-set-orientation-constraint-mark (accept-first-command-arg)))
  (redraw-buttons))


(define-visco-command (com-set-orientation-constraint-mark :name "Set Orientation Constraint Mark")
    ((object `(and gui-object
		   (satisfies first-arg-ok-p))))
  (create-orientation-constraint-mark object))


(defmethod create-orientation-constraint-mark ((object gui-orientation-arrow))
  (create-orientation-constraint-mark (object object)))

(defmethod create-orientation-constraint-mark ((object gui-visco-object))
  (with-visco-frame (visco) 
    (let* ((stream (get-frame-pane visco 'display))
	   (constraint (orientation-constraint object))
	   (alpha (if (typep object 'geom-line)
		      (global-orientation object) 0))
	   (old-r (r (orientation-arrow object))))
      (flet ((exit-code ()
               (setf (r (orientation-arrow object)) old-r)
               (apply-set-orientation-constraint object constraint)
               (pop-undo-stack)))
        (register-undo-information 
         (format nil "Set Mark Of Orientation Constraint Of ~A" object))
      (with-output-recording-options (stream :draw t :record nil)
	(prepare-to-move)
	(tracking-pointer (stream)
	  (:pointer-motion (x y)
			   (multiple-value-bind (x y)
			       (xy-to-grid x y)
			     (draw (orientation-arrow object) stream :handling-active t)
			     (multiple-value-bind (xc yc)
				 (get-origin-for (orientation-arrow object))
			       (multiple-value-bind (r alpha2)
				   (distance-and-orientation* xc yc
							      x y)
				 (apply-set-orientation-constraint object
								   (cons (normalize 
									  (- alpha2 alpha 
									     (alpha-off 
									      (orientation-arrow object))))
									 constraint))
				 (setf (r (orientation-arrow object)) r))
			       (draw (orientation-arrow object) stream :handling-active t))))
          (:keyboard (character)
                     (when (and (characterp character)
                                (char-equal character #\q))
                       (exit-code)
                       (return)))
	  (:pointer-button-press (event)
				 (if (= (pointer-event-button event)
					+pointer-right-button+)
                                   (exit-code)				    
				   (multiple-value-bind (res he) 
				       (execute 
					(register-history-entry 
					 ""
					 `(let ((obj (find-named-visco-object ,(name object))))
					    (when obj
					      (let ((arrow (orientation-arrow obj))
						    (res 
						     (apply-set-orientation-constraint obj
										       ',(orientation-constraint object))))
						(when (and res (not (eq res 'not-applicable)))
						  (setf (r-off arrow) ,(r-off (orientation-arrow object)))
						  (setf (rho-off arrow) ,(rho-off (orientation-arrow object)))
						  (setf (r arrow) ,(r (orientation-arrow object)))
						  (setf (alpha-off arrow) 
						    ,(alpha-off (orientation-arrow object)))
						  res))))
					 (list object)))
				     (unless res
				       (beep)
				       (delete-history-entry he)
				       (pop-undo-stack))))
				 (return)))))))
  (refresh))

;;;
;;;
;;;

(defmethod first-argument-acceptable-p ((op (eql 'com-set-orientation-constraint-intervall))
					(obj gui-visco-object))
  (and (typep obj 'orientation-constraint-mixin)
       (orientation-constraint obj)))

(defmethod first-argument-acceptable-p ((op (eql 'com-set-orientation-constraint-intervall))
					(obj gui-orientation-arrow))
  (first-argument-acceptable-p op (object obj)))

(defmethod command-enabled ((command (eql 'com-set-orientation-constraint-intervall)) (frame visco)
                            #+:mcl &optional #+:mcl unused)
  #+:mcl (declare (ignore unused))
  (command-enabled-p command))

(defmethod command-enabled ((command (eql 'com-set-orientation-constraint-intervall*)) (frame visco)
                            #+:mcl &optional #+:mcl unused)
  #+:mcl (declare (ignore unused))
  (command-enabled-p 'com-set-orientation-constraint-intervall))

(define-visco-command (com-set-orientation-constraint-intervall* :name "Set Orientation Constraint Intervall")
    nil
  (let ((*operator-mode* *button-create-orientation-constraint-intervall*))
    (com-set-orientation-constraint-intervall (accept-first-command-arg)))
  (redraw-buttons))


(define-visco-command (com-set-orientation-constraint-intervall :name "Set Orientation Constraint Intervall")
    ((object `(and gui-object
		   (satisfies first-arg-ok-p))))
  (create-orientation-constraint-intervall object))


(defmethod create-orientation-constraint-intervall ((object gui-orientation-arrow))
  (create-orientation-constraint-intervall (object object)))

(defmethod create-orientation-constraint-intervall ((object gui-query-object))
  (with-visco-frame (visco) 
    (let* ((stream (get-frame-pane visco 'display))
	   (constraint (orientation-constraint object))
	   (alpha (if (typep object 'geom-line)
		      (global-orientation object) 0))
	   (old-r (r (orientation-arrow object)))
	   (first nil)
	   (angle nil)
	   (focus (current-focus visco)))
      (flet ((exit-code ()
               (setf (r (orientation-arrow object)) old-r)
               (apply-set-orientation-constraint object constraint)
               (pop-undo-stack)
               (set-current-focus-to (find-named-history-entry (name focus)))
               (return-from create-orientation-constraint-intervall)))				   
        (register-undo-information 
       (format nil "Set Intervall Of Orientation Constraint Of ~A" object))
      (with-output-recording-options (stream :draw t :record nil)
	(prepare-to-move)
	(tracking-pointer (stream)
	  (:pointer-motion (x y)
			   (multiple-value-bind (x y)
			       (xy-to-grid x y)			     
			     (draw (orientation-arrow object) stream :handling-active t)
			     (multiple-value-bind (xc yc)
				 (get-origin-for (orientation-arrow object))
			       (multiple-value-bind (r alpha2)
				   (distance-and-orientation* xc yc
							      x y)
				 (setf angle (normalize 
					      (- alpha2 alpha 
						 (alpha-off 
						  (orientation-arrow object)))))
				 (apply-set-orientation-constraint object
								   (if first 
								       (cons
									(list angle first)
									constraint)
								     (cons angle
									   constraint)))
				 (setf (r (orientation-arrow object)) r)))
			     (draw (orientation-arrow object) stream :handling-active t)))
          (:keyboard (character)
                     (when (and (characterp character)
                                (char-equal character #\q))
                       (exit-code)))
          (:pointer-button-press (event)
				 (if (= (pointer-event-button event)
					+pointer-right-button+)
                                   (exit-code)
                                   (if first
                                     (multiple-value-bind (res he)
					   (execute 
					    (register-history-entry 
					     ""
					     `(let ((obj (find-named-visco-object ,(name object))))
						(when obj
						  (let ((arrow (orientation-arrow obj))
							(res 
							 (apply-set-orientation-constraint obj
											   ',(orientation-constraint object))))
						    (when (and res (not (eq res 'not-applicable)))
						      (setf (r-off arrow) ,(r-off (orientation-arrow object)))
						      (setf (rho-off arrow) ,(rho-off (orientation-arrow object)))
						      (setf (r arrow) ,(r (orientation-arrow object)))
						      (setf (alpha-off arrow) 
							,(alpha-off (orientation-arrow object)))
						      res))))
					     (list object)))
					 (unless res
					   (beep)
					   (delete-history-entry he)
					   (pop-undo-stack))
					 (return))
				     (setf first angle)))))))))
  (refresh))

;;;
;;;
;;;

(defmethod gui-set-relative-orientation-constraint ((obj1 gui-atomic-rubberband) (obj2 gui-atomic-rubberband))
  (with-visco-frame (visco)
    (let ((stream (get-frame-pane visco 'display)))
      (multiple-value-bind (ix iy)
	  (calculate-intersection-point obj1 obj2)	   		  
	(let ((circle 
	       (make-instance 'gui-relative-orientation-circle 
		 :dont-initialize t
		 :object1 obj1
		 :object2 obj2
		 :allowed-derivation 0))
	      (alpha 0) 
	      (r 30))
	  (let ((descr
		 (format nil "Set Relative Orientation Constraint Between ~A And ~A" obj1 obj2)))	    
	    (with-output-recording-options (stream :draw t :record nil)
	      (prepare-to-move)
	      (draw circle stream :handling-active t)
	      (when
		  (tracking-pointer (stream)
		    (:pointer-motion (x y)
				     (multiple-value-bind (x y)
					 (xy-to-grid x y)
				       
				       (draw circle stream :handling-active t)
				       (multiple-value-bind (r2 alpha2)
					   (distance-and-orientation* ix iy
								      x y)
					 (setf alpha alpha2)
					 (setf r r2)
					 (setf (r circle) r)
					 (setf (allowed-derivation circle) alpha))
				       (draw circle stream :handling-active t)))
                    (:keyboard (character)
                               (when (and (characterp character)
                                          (char-equal character #\q))
                                 (return nil)))
		    (:pointer-button-press (event)
					   (if (= (pointer-event-button event)
						  +pointer-right-button+)
					       (return nil)
					     (return t))))
		(register-undo-information descr)
		(multiple-value-bind (res he)
		    (execute
		     (register-history-entry 
		      descr
		      `(let* ((obj1 (find-named-visco-object ,(max (name obj1)
								   (name obj2))))
			      (obj2 (find-named-visco-object ,(min (name obj1)
								   (name obj2))))
			      (res (apply-set-relative-orientation-constraint 
				    obj1 obj2 ,alpha)))
			 (when (and res (not (eq res 'not-applicable)))
			   (make-instance 'gui-relative-orientation-circle 			       
			     :object1 obj1
			     :object2 obj2
			     :r ,r
			     :allowed-derivation ,alpha)
			   res))
		      (list obj1 obj2)))
		  (unless res
		    (beep)
		    (delete-history-entry he)
		    (pop-undo-stack))))))))))
  (refresh))

(define-visco-command (com-set-relative-orientation-constraint :name "Set Relative Orientation Constraint")
    ((obj1 `(and gui-object 
		 (satisfies first-arg-ok))))
  (let ((*first-arg* obj1))
    (let ((obj2 
	   (accept `(and gui-object 
			 (satisfies second-arg-ok-p))
		   :prompt " Select Second Argument")))
      (gui-set-relative-orientation-constraint obj1 obj2))))

(defmethod first-argument-acceptable-p ((op (eql 'com-set-relative-orientation-constraint)) (obj1 gui-object))
  (and (typep obj1 'gui-atomic-rubberband)
       (some #'(lambda (obj2)
		 (second-argument-acceptable-p op
					       obj1 obj2))
	     (visco-objects (get-current-query)))))

(defmethod second-argument-acceptable-p ((op (eql 'com-set-relative-orientation-constraint))
					 (obj1 gui-object)
					 (obj2 gui-object))
  (set-relative-orientation-constraint-applicable-p obj1 obj2 0))

(defmethod command-enabled ((command (eql 'com-set-relative-orientation-constraint)) (frame visco)
                            #+:mcl &optional #+:mcl unused)
  #+:mcl (declare (ignore unused))
  (command-enabled-p command))

(defmethod command-enabled ((command (eql 'com-set-relative-orientation-constraint*)) (frame visco)
                            #+:mcl &optional #+:mcl unused)
  #+:mcl (declare (ignore unused))
  (command-enabled-p 'com-set-relative-orientation-constraint))

(define-visco-command (com-set-relative-orientation-constraint* :name "Set Relative Orientation Constraint")
    nil
  (let ((*operator-mode* *button-create-relative-orientation-constraint*))
    (com-set-relative-orientation-constraint (accept-first-command-arg)))
  (redraw-buttons))


;;;
;;;
;;;

(define-visco-command (com-set-transparency-properties :name "Set Transparency Properties")
    ((obj `(and gui-object
		(satisfies first-arg-ok-p))))
  (let ((descr     
	 (format nil "Set Properties Of ~A" obj)))
    (register-undo-information descr)
    (with-visco-frame (visco)
      (let ((stream (get-frame-pane visco 'display))
	    (minw (if (sxmin obj)
		      (write-to-string (* (sxmin obj) (width obj)))
		    "NIL"))
	    (maxw (if (sxmax obj)
		      (write-to-string (* (sxmax obj) (width obj)))
		    "NIL"))
	    (minh (if (symin obj)
		      (write-to-string (* (symin obj) (height obj)))
		    "NIL"))
	    (maxh (if (symax obj)
		      (write-to-string (* (symax obj) (height obj)))
		    "NIL"))
	    (proportional (sxsy-constraint obj)))
	(loop
	  (accepting-values (stream :own-window t :label "Set Transparency Properties")
	    (terpri stream)
	    (multiple-value-bind (string)
		(accept 'string
			:stream stream
			:query-identifier 'wmin
			:prompt "Min Width (or NIL)"
			:default minw)
	      (setf minw string))
	    (terpri stream)
	    (multiple-value-bind (string)
		(accept 'string
			:stream stream
			:query-identifier 'wmax
			:prompt "Max Width (or NIL)"
			:default maxw)
	      (setf maxw string))
	    
	    (terpri stream)
	    (terpri stream)  
	    (multiple-value-bind (string)
		(accept 'string
			:stream stream
			:prompt "Min Height (or NIL)"
			:query-identifier 'hmin
			:default minh)
	      (setf minh string))
	    (terpri stream)
	    (multiple-value-bind (string)
		(accept 'string
			:stream stream
			:prompt "Max Height (or NIL)"
			:query-identifier 'hmax
			:default maxh)
	      (setf maxh string))
	    
	    (terpri stream)
	    (terpri stream)
	    (multiple-value-bind (bool)
		(accept 'boolean
			:stream stream
			:prompt "Proportional"
			:default proportional)
	      (setf proportional bool)))
	  
	  (when (and (typep (read-from-string minw) '(or number null))
		     (typep (read-from-string maxw) '(or number null))
		     (typep (read-from-string minh) '(or number null))
		     (typep (read-from-string maxh) '(or number null)))
	    (return)))
	
	(setf minw (read-from-string minw)
	      maxw (read-from-string maxw)
	      minh (read-from-string minh)
	      maxh (read-from-string maxh))
	
	(let ((new-history-entry
	       (register-history-entry
		descr
		`(apply-set-transparency-properties 
		  (find-named-visco-object ,(name obj))
		  ,minw ,maxw
		  ,minh ,maxh
		  ,(when proportional 1.0))
		(list obj))))
	  (multiple-value-bind (res he)
	      (execute new-history-entry)
	    (unless res
	      (beep)
	      (delete-history-entry he)
	      (pop-undo-stack)))))))
  (refresh))

(defmethod first-argument-acceptable-p ((op (eql 'com-set-transparency-properties)) (obj gui-transparency))
  t)

(defmethod command-enabled ((command (eql 'com-set-transparency-properties)) (frame visco)
                            #+:mcl &optional #+:mcl unused)
  #+:mcl (declare (ignore unused))
  (command-enabled-p command))

(defmethod command-enabled ((command (eql 'com-set-transparency-properties*)) (frame visco)
                            #+:mcl &optional #+:mcl unused)
  #+:mcl (declare (ignore unused))
  (command-enabled-p 'com-set-transparency-properties))

(define-visco-command (com-set-transparency-properties* :name "Set Transparency Properties")
    nil
  (let ((*operator-mode* *button-set-transparency-properties*))
    (com-set-transparency-properties (accept-first-command-arg)))
  (redraw-buttons))


;;;
;;;
;;;

(define-command-table operator-table
    :menu (("Create Centroid (S, Chain, Poly)" :command (com-create-centroid*))
	   ("Create Intersection Point (S x S)" :command (com-create-intersection-point*))
	   ("divide1" :divider nil)
	   ("Create Inverse Constant Enclosure (E)" :command (com-create-inverse-drawn-enclosure*))
	   ("Create Inner Enclosure (Poly)" :command (com-create-inner-enclosure*))
	   ("Create Outer Enclosure (Poly)" :command (com-create-outer-enclosure*))
	   ("Create Epsilon Enclosure (P, S, Chain, Poly)" :command (com-create-epsilon-enclosure*))
	   ("divide3" :divider nil)
	   ("divide4" :divider nil)	   
	   ("Set Semantics (P, S, Chain, Poly)" :command (com-set-semantics*))
	   ("divide5" :divider nil)
	   ("Set At Most Constraint (R, Chain, Poly)" :command (com-set-at-most-constraint*))
	   ("divide6" :divider nil)
	   ("Set Orientation Constraint (O, B, AR)" :command (com-set-orientation-constraint*))
	   ("Set Orientation Constraint Mark (OC)" :command (com-set-orientation-constraint-mark*))
	   ("Set Orientation Constraint Intervall (OC)" :command (com-set-orientation-constraint-intervall*))
	   ("divide7" :divider nil)
	   ("Set Relative Orientation Constraint ((B/AR) x (B/AR))" 
	    :command (com-set-relative-orientation-constraint*))
	   ("divide8" :divider nil)
	   ("Set Transparency Properties (T)" :command (com-set-transparency-properties*))))
