;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: GUI; Base: 10 -*-

(in-package gui)

(defconstant +current-focus-entry-text-style+
    (parse-text-style '(:sans-serif (:bold :italic) :small)))

(defconstant +focus-entry-text-style+
    (parse-text-style '(:sans-serif nil :small)))

(defconstant +current-focus-entry-ink+
    +red+)

(defconstant +current-transparency-entry-ink+
    +blue+)

(defconstant +focus-entry-ink+
    +black+)


(define-presentation-method highlight-presentation ((type history-entry) record stream state)
  (inverse-highlighter type record stream state))

(defun accept-control-buttons (frame stream)
  (let ((line 1))
    (labels ((draw-it (op line level focus-p transparency-p)
	       (with-drawing-options (stream :text-style 
					     (cond (focus-p
						    +current-focus-entry-text-style+)
						   (t +focus-entry-text-style+))	
					     :ink
					     (cond (focus-p
						    +current-focus-entry-ink+)
						   (transparency-p
						    +current-transparency-entry-ink+)
						   (t +focus-entry-ink+)))
		 (draw-text* stream 
			     (format nil 
				     "Step ~A: ~A"
				     line
				     op)
			     (* 10 (1- level)) (* line 14)))))
      
      (dolist (entry (history frame))
	(when (typep entry 'visually-relevant-history-entry)
	  (let* ((obj
		  (and (typep entry 'visually-relevant-history-entry)
		       (object-to-draw entry)))
		 
		 (level (typecase (and (typep entry 'constructor-history-entry)
				       obj)
			  (gui-transparency 1)
			  (gui-point (if (primary-p obj) 2 
				       (if (every #'primary-p (part-of obj))
					   3
					 4)))
			  (gui-line (if (primary-p obj) 
					2 
				      3))
			  (otherwise 2)))
		 
		 (focus-p (eq (current-focus frame) entry))
		 (transparency-p (and (typep obj 'gui-transparency)
				      (current-transparency-is-transparency-p obj))))
	    
	    (updating-output (stream :unique-id entry
				     :cache-value (list level focus-p transparency-p line entry)
				     :cache-test #'equal)
	      (with-output-as-presentation 
		  (stream entry 'history-entry)
		(draw-it (operation-descr entry)
			 line
			 level
			 focus-p
			 transparency-p)))
	    (incf line)))))))

;;;
;;;
;;;

(defun delete-objects (objects)
  (dolist (obj objects)    
    (delete-object obj)))

(defun pop-undo-stack ()
  (with-visco-frame (visco)
    (prog1 
	(pop (undo-stack visco))
      (generate-name-for-undo-and-redo-command))))

(defun pop-redo-stack ()
  (with-visco-frame (visco)
    (prog1
	(pop (redo-stack visco))   
      (generate-name-for-undo-and-redo-command))))

(defun get-state (descr)
  (with-visco-frame (visco)
    (list (copy-list (history visco))
	  (get-button-state)
	  (and (current-focus visco)
	       (name (current-focus visco)))
	  (and (current-transparency visco)
	       (name (current-transparency visco)))
	  descr)))

(defun install-previous-state (state)
  (reconstruct-history
   (first state))
  
  (install-button-state (second state))
  
  (when (third state)
    (set-current-focus-to (find-named-history-entry				     
			   (third state))))
  
  (when (fourth state)
    (set-current-transparency-to (find-named-visco-object
				  (fourth state)))))

;;;
;;;
;;;

(defun register-undo-information (descr)
  (with-visco-frame (visco)
    (push (get-state descr)
	  (undo-stack visco))
    (when descr 
      (generate-name-for-undo-and-redo-command))))

(defun set-undo-operation-description-to (descr)
  (with-visco-frame (visco)
    (setf (fifth (first (undo-stack visco)))
      descr)
    (when descr (generate-name-for-undo-and-redo-command))))

(defun undo ()
  (with-visco-frame (visco)
    (let ((undo-info
	   (pop-undo-stack)))
      (if undo-info
	  (progn
	    (push (get-state (fifth undo-info))
		  (redo-stack visco))
	    (install-previous-state undo-info)
	    (format t "Undoing ~A~%" 
		    (fifth undo-info)))
	(progn
	  (beep)
	  (format t "No More Undo Information!~%"))))
    (generate-name-for-undo-and-redo-command)))

(defun redo ()
  (let ((redo-info
	 (pop-redo-stack)))
    (if redo-info
	(progn
	  (register-undo-information 
	   (fifth redo-info))
	  (install-previous-state redo-info)
	  (format t "Redoing ~A~%" 
		  (fifth redo-info)))
      (progn
	(beep)
	(format t "No More Redo Information!~%"))))
  (generate-name-for-undo-and-redo-command))

;;;
;;;
;;;

(defun reconstruct-history (history &key (give-up-after-first-try nil))
  (with-visco-frame (visco)
    (let ((history (copy-list history))
	  (old-history (copy-list (history visco)))
	  (new-objects nil)
	  (old-objects 
	   (copy-list (visco-objects (get-current-query))))
	  (focus (current-focus visco)))
      
      (delete-objects old-objects)
      
      (prog1
	  (dolist (history-entry history t)
	    #| (princ (source history-entry)) (terpri) |#
	    (let ((res (execute history-entry)))
	      (if res
		  (push res new-objects)
		(progn
		  (unless give-up-after-first-try
		    (setf (visco-objects (current-query visco))
		      old-objects)
		    (setf (history visco) old-history)
		    (setf history old-history)
		    (reconstruct-query :give-up-after-first-try t)
		    (when focus
		      (set-current-focus-to focus)))
		  (beep)
		  (format t "~%*** ERROR ***~%Not Reconstructable!~%")
		  (return-from reconstruct-history nil)))))
	(setf (history visco) history)))))

(defun reconstruct-query (&key (give-up-after-first-try nil))
  (with-visco-frame (visco)
    (reconstruct-history (history visco)
			 :give-up-after-first-try 
			 give-up-after-first-try)))

(defmethod substitute-history-entry-with ((old history-entry) (new history-entry))
  (with-visco-frame (visco)
    (setf (history visco)
      (nsubstitute new
		   old
		   (history visco))
      (name new)
      (name old))))

(defmethod change-query-from-to ((old history-entry)
				 (new history-entry))
  (with-visco-frame (visco) 
    (delete-history-entry new)
    (let ((history (copy-list (history visco))))
      (setf (name new) (name old))
      (setf history
	(nsubstitute new old history))      
      (reconstruct-history history))))

(defmethod change-query-from-to ((old list)
				 (new list))
  (with-visco-frame (visco)
    (delete-history-entries new)
    (let ((history (copy-list (history visco))))      
      (mapc #'(lambda (ohe nhe)
		(setf (name nhe) (name ohe)))
	    old new)      
      (mapc #'(lambda (ohe nhe)
		(setf history 
		  (nsubstitute nhe ohe history)))
	    old new)      
      (reconstruct-history history))))

;;;
;;;
;;;


(defun generate-name-for-undo-and-redo-command ()  
  (with-visco-frame (visco)
    (remove-command-from-command-table 
     'com-undo* 'control-table)
    (remove-command-from-command-table 
     'com-redo* 'control-table)
    
    (let* ((undo (first (undo-stack visco)))
	   (redo (first (redo-stack visco)))
	   (undo-name
	    (if undo (format nil "Undo ~A" (fifth undo))
	      "Undo"))
	   (redo-name
	    (if redo (format nil "Redo ~A" (fifth redo))
	      "Redo")))
      (add-command-to-command-table 'com-undo*
				    'control-table
				    :menu (list undo-name
						:after :start))
      (add-command-to-command-table 'com-redo*
				    'control-table
				    :menu (list redo-name
						:after undo-name)))))

(define-visco-command (com-undo* :name "Undo")
    nil
  (undo))


(defmethod command-enabled ((obj (eql 'com-undo*)) (frame visco)
                            #+:mcl &optional #+:mcl unused)
  #+:mcl (declare (ignore unused))
  (with-visco-frame (visco)
    (and visco
	 (undo-stack visco))))

;;;
;;;
;;;

(define-visco-command (com-redo* :name "Redo")
    nil
  (redo))


(defmethod command-enabled ((obj (eql 'com-redo*)) (frame visco)
                            #+:mcl &optional #+:mcl unused)
  #+:mcl (declare (ignore unused))
  (with-visco-frame (visco)
    (and visco
	 (redo-stack visco))))

;;;
;;;
;;;

(define-visco-command (com-set-current-focus-to :name nil)
    ((object 'history-entry))
  (set-current-focus-to object)
  (refresh))

(define-visco-command (com-set-current-focus-to* :name "Set Current Focus To")
    ()
  (let ((obj (accept '(and gui-visco-object
		       (satisfies history-entry))
		     :prompt "Select Current Focus Object")))
    (com-set-current-focus-to (history-entry obj))))

(defmethod command-enabled ((obj (eql 'com-set-current-focus-to*)) (frame visco)
                            #+:mcl &optional #+:mcl unused)
  #+:mcl (declare (ignore unused))
  (command-enabled 'com-set-current-focus-to frame))

(define-presentation-to-command-translator set-current-focus-to	
    (gui-visco-object com-set-current-focus-to visco
		      :gesture :set-focus
		      :documentation ((object stream)
				      (format stream "Set Current Focus To ~A" object))
		      :tester ((object) (history-entry object)))
  (object)
  (list (history-entry object)))

(define-presentation-to-command-translator set-current-focus-to2
    (history-entry com-set-current-focus-to visco
		   :documentation ((object stream)
				   (format stream "Set Current Focus To ~A" object))
		   :gesture :select)
  (object)
  (list object))


;;;
;;;
;;;

(define-visco-command (com-set-current-transparency-to :name nil)
    ((object 'gui-transparency))
  (set-current-transparency-to object))

(define-visco-command (com-set-current-transparency-to* :name "Set Current Transparency To")
    ()
  (let ((obj (accept 'gui-transparency
		     :prompt "Select Current Transparency")))
    (set-current-transparency-to obj)))

(defmethod command-enabled ((obj (eql 'com-set-current-transparency-to*)) (frame visco)
                            #+:mcl &optional #+:mcl unused)
  #+:mcl (declare (ignore unused))
  (with-visco-frame (visco)
    (and visco
	 (some #'(lambda (obj)
		   (typep obj 'gui-transparency))
	       (visco-objects (current-query visco))))))

(define-presentation-to-command-translator set-current-transparency-to	
    (gui-transparency com-set-current-transparency-to visco
		      :gesture :set-transparency
		      :documentation ((object stream)
				      (format stream "Set Current Transparency To ~A" object)))
  (object)
  (list object))

(define-presentation-to-command-translator set-current-transparency-to2
    (constructor-history-entry com-set-current-transparency-to visco
			       :gesture :set-transparency
			       :documentation ((object stream)
					       (format stream "Set Current Transparency To ~A"
						       (object-to-draw object)))
			       :tester ((object) (typep (object-to-draw object) 'gui-transparency)))
  (object)
  (list (object-to-draw object)))

;;;
;;;
;;;

(defmethod delete-object-applicable-p ((obj gui-object))
  nil)

(defmethod delete-object-applicable-p ((obj gui-visco-object))
  (=> (typep obj 'geom-thing) (primary-p obj)))

(defmethod delete-object-applicable-p ((obj gui-orientation-arrow))
  t)

(defmethod delete-object-applicable-p ((obj gui-semantics-label))
  t)

(defmethod delete-object-applicable-p ((obj gui-at-most-label))
  t)

(defmethod delete-object-applicable-p ((obj gui-relative-orientation-circle))
  t)

;;;
;;;
;;;

(define-visco-command (com-delete :name nil)
    ((object 'gui-object) (delete-component-objects-p 'boolean))
  (gui-delete-object object delete-component-objects-p))


(defmethod gui-delete-object ((object gui-visco-object) delete-component-objects-p)
  (with-visco-frame (visco)
    (register-undo-information
     (format nil "Delete ~A" object))
    (let ((old-history (copy-list (history visco)))
	  (focus (current-focus visco)))
      (delete-object object :delete-component-objects-p delete-component-objects-p)
      (if (reconstruct-history (history visco) :give-up-after-first-try t)
	  (progn
	    (set-object-modes)	   
	    (set-current-focus-to-newest-history-entry)
	    (when (typep object 'gui-transparency)
	      (set-current-transparency-to-newest-transparency)))
	(progn
	  (pop-undo-stack)
	  (reconstruct-history old-history)
	  (set-current-focus-to focus)))))
  (refresh))

(defmethod command-enabled ((obj (eql 'com-delete*)) (frame visco)
                            #+:mcl &optional #+:mcl unused)
  #+:mcl (declare (ignore unused))
  (with-visco-frame (visco)
    (and visco
	 (some #'(lambda (obj)
		   (delete-object-applicable-p obj))
	       (visco-objects (current-query visco))))))

(define-visco-command (com-delete* :name "Delete Object")
    ()
  (let ((obj (accept '(and gui-object
		       (satisfies delete-object-applicable-p))
		     :prompt "Select Object To Delete")))
    (com-delete obj t)))

(define-presentation-to-command-translator delete
    (gui-object com-delete visco
		:gesture :delete
		:priority 1
		:documentation ((object stream)
				(format stream "Delete ~A" object))
		:tester ((object)
			 (delete-object-applicable-p object))
		:maintain-history nil
		:echo nil)
  (object)
  (list object t))

;;;
;;;
;;;

(defmethod gui-delete-object ((object gui-orientation-arrow) delete-component-objects-p)
  (declare (ignore delete-component-objects-p))
  (register-undo-information
   (format nil "Delete ~A" object))
  (execute
   (register-history-entry 
    ""
    `(let ((res (apply-delete-orientation-constraint
		 (find-named-visco-object ,(name (object object))))))
       (when (and res (not (eq res 'not-applicable)))
	 (reinitialize (orientation-arrow res))
	 res))
    (list (object object))))
  (refresh))

(defmethod gui-delete-object ((object gui-semantics-label) delete-component-objects-p)
  (declare (ignore delete-component-objects-p))
  (register-undo-information
   (format nil "Delete ~A" object))
  (execute
   (register-history-entry 
    ""
    `(let ((res (apply-delete-semantics
		 (find-named-visco-object ,(name (object object))))))
       (when (and res (not (eq res 'not-applicable)))
	 (reinitialize (semantics-label res))
	 res))
    (list (object object))))
  (refresh))

(defmethod gui-delete-object ((object gui-at-most-label) delete-component-objects-p)
  (declare (ignore delete-component-objects-p))
  (register-undo-information
   (format nil "Delete ~A" object))
  (execute
   (register-history-entry 
    ""
    `(let ((res (apply-delete-at-most-constraint
		 (find-named-visco-object ,(name (object object))))))
       (when (and res (not (eq res 'not-applicable)))
	 (reinitialize (at-most-label res))
	 res))
    (list (object object))))
  (refresh))

(defmethod gui-delete-object ((object gui-relative-orientation-circle) delete-component-objects-p)
  (declare (ignore delete-component-objects-p))
  (register-undo-information
   (format nil "Delete ~A" object))
  (execute
   (register-history-entry 
    ""
    `(let* ((obj1 (find-named-visco-object ,(max (name (object1 object))
						 (name (object2 object)))))
	    (obj2 (find-named-visco-object ,(min (name (object1 object))
						 (name (object2 object)))))
	    (res (apply-delete-relative-orientation-constraint 
		  obj1 obj2)))
       (when (and res (not (eq res 'not-applicable)))
	 (dolist (circle (relative-orientation-circles obj1))
	   (when (or (and (eq (object1 circle) obj1)
			  (eq (object2 circle) obj2))
		     (and (eq (object2 circle) obj1)
			  (eq (object1 circle) obj2)))		   
	     (delete-object circle)))
	 res))
    (list (object1 object) (object2 object))))
  (refresh))

;;;
;;;
;;;

(defun delete-history-entry-applicable-p (he)
  (when (typep he '(or visually-relevant-history-entry constructor-history-entry))
    (let ((obj (object-to-draw he)))
      (and (typep obj 'visco-object)
	   (=> (typep obj 'geom-thing) (primary-p obj))
	   (=> (typep obj 'transparency) (not (transparency-query-objects-and-enclosures obj)))
	   (=> (typep obj 'possible-operator-argument-mixin)
	       (not (arg-of-operators obj)))))))

(define-visco-command (com-delete-history-entry :name nil)
    ((object 'history-entry))
  (com-delete (object-to-draw object) nil)
  (set-undo-operation-description-to
   (format nil "Delete History Entry ~A" object)))

(defmethod command-enabled ((obj (eql 'com-delete-history-entry*)) (frame visco)
                            #+:mcl &optional #+:mcl unused)
  #+:mcl (declare (ignore unused))
  (with-visco-frame (visco)
    (and visco
	 (some #'(lambda (obj)
		   (delete-history-entry-applicable-p obj))
	       (history visco)))))

(define-visco-command (com-delete-history-entry* :name "Delete History Entry")
    ()
  (let ((obj (accept '(and (or visually-relevant-history-entry
			    constructor-history-entry)
		       (satisfies delete-history-entry-applicable-p))
		     :prompt "Select History Entry To Delete")))
    (com-delete-history-entry obj)))

(define-presentation-to-command-translator delete-history-entry
    (history-entry com-delete-history-entry
		   visco
		   :gesture :delete
		   :maintain-history nil
		   :documentation ((object stream)
				   (format stream "Delete ~A" object))		
		   :tester ((object)
			    (delete-history-entry-applicable-p object))
		   :echo nil)
  (object)
  (list object))

;;;
;;;
;;;

(define-visco-command (com-inactivate-object :name nil)
    ((object 'gui-visco-object))
  (setf (inactive object) t)
  (refresh))

(defun not-inactivated-p (obj)
  (not (inactive obj)))

(define-visco-command (com-inactivate-object* :name "Inactivate Object")
    ()
  (let ((obj (accept '(and gui-visco-object
		       (satisfies not-inactivated-p))
		     :prompt "Select Object To Inactivate")))
    (com-inactivate-object obj)))

(defmethod command-enabled ((obj (eql 'com-inactivate-object*)) (frame visco)
                            #+:mcl &optional #+:mcl unused)
  #+:mcl (declare (ignore unused))
  (some #'(lambda (obj) 
	    (not (inactive obj)))
	(visco-objects (current-query frame))))

#|
(define-presentation-to-command-translator inactivate-object	
    (gui-visco-object com-inactivate-object visco
		      :gesture :inactivate)
  (object)
  (list object))
|#

;;;
;;;
;;;

(defun inactivated-p (obj)
  (inactive obj))

(define-visco-command (com-reactivate-object* :name "Reactivate Object")
    ()
  (with-visco-frame (visco)
    (setf *ignore-inactive-flag* t)
    (redisplay-frame-pane visco 'display :force-p t)
    (setf *ignore-inactive-flag* nil)
    (let ((obj (accept '(and gui-visco-object
			 (satisfies inactivated-p))
		       :prompt "Select Inactivated Object To Reactivate")))
      (setf (inactive obj) nil)
      (refresh))))

(defmethod command-enabled ((obj (eql 'com-reactivate-object*)) (frame visco)
                            #+:mcl &optional #+:mcl unused)
  #+:mcl (declare (ignore unused))
  (some #'(lambda (obj) 
	    (inactive obj))
	(visco-objects (current-query frame))))

;;;
;;;
;;;


(define-visco-command (com-recreate :name "Recreate")
    ((obj 'gui-query-object-or-enclosure))
  (with-visco-frame (visco)
    (register-undo-information
     (format nil "Recreate ~A" obj))
    (let ((focus (current-focus visco))
	  (new-history-entry
	   (etypecase obj
	     (gui-drawn-enclosure (get-history-entry-for
				   obj
				   :pointlist (mapcan #'(lambda (p)
							  (list (x p) (y p)))
						      (point-list obj))
				   :negated-p (negated-p obj)
				   :name (name obj)
				   :description (operation-descr (history-entry obj))
				   :transparency (on-transparency obj)
				   :opaque-p *opaque-enclosures-p*
				   :pattern-id (pattern-id obj)))
	     (gui-point (get-history-entry-for
			 (or *point-mode* obj)
			 :name (name obj)
			 :description (operation-descr (history-entry obj))			
			 :status (or *point-status* (status obj))
			 :transparency (on-transparency obj)
			 :x (x obj)
			 :y (y obj)))
	     (gui-line (get-history-entry-for 
			(or *segment-mode* obj)
			:name (name obj)
			:description (operation-descr (history-entry obj))
			:status (or *segment-status* (status obj))
			:p1 (p1 obj)
			:p2 (p2 obj)
			:transparency (on-transparency obj)))
	     (gui-chain-or-polygon (get-history-entry-for
				    obj
				    :name (name obj)
				    :description (operation-descr (history-entry obj))
				    :status *chain-or-polygon-status*
				    :segments (segments obj)
				    :transparency (on-transparency obj))))))
      (if (change-query-from-to (history-entry obj)
				new-history-entry)
	  (set-current-focus-to (find-named-history-entry (name focus)))
	(pop-undo-stack)))
    (refresh)))

(defun recreateable-object-p (obj)
  (and (typep obj 'gui-query-object-or-enclosure)
       (etypecase obj
	 (gui-enclosure t)
	 (gui-point (and (or *point-mode* *point-status*)
			 (not (and (typep obj 'possible-operator-result-mixin)
				   (res-of-operators obj)))))
	 (gui-line (or *segment-mode* *segment-status*))
	 (gui-chain-or-polygon *chain-or-polygon-status*))))

(defmethod command-enabled ((command (eql 'com-recreate*)) (frame visco)
                            #+:mcl &optional #+:mcl unused)
  #+:mcl (declare (ignore unused))
  (and (get-current-query)
       (some #'recreateable-object-p (visco-objects (get-current-query)))))

(define-visco-command (com-recreate* :name "Recreate")
    nil
  (let ((obj (accept '(and gui-visco-object
		       (satisfies recreateable-object-p))
		     :prompt "Select Object To Recreate")))
    (com-recreate obj)))

;;;
;;;
;;;

(define-command-table control-table
    :menu (("Undo" :command (com-undo*))
	   ("Redo" :command (com-redo*))
	   ("divide1" :divider nil)
	   ("Move Object (Shift Left)" :command (com-move*))
	   ("Recreate Object" :command (com-recreate*))
	   ("divide2" :divider nil)
	   ("Inactivate Object" :command (com-inactivate-object*))
	   ("Reactivate Object" :command (com-reactivate-object*))
	   ("divide3" :divider nil)
	   ("Set Current Focus To Object (Shift Middle)" :command (com-set-current-focus-to*))
	   ("Set Current Transparency To Transparency (Control Middle)" :command (com-set-current-transparency-to*))
	   ("divide4" :divider nil)
	   ("Delete Object (Middle)" :command (com-delete*))
	   ("Delete History Entry (Middle)" :command (com-delete-history-entry*))))


