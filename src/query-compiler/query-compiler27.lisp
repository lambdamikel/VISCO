;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: QUERY-COMPILER; Base: 10 -*-

(in-package query-compiler)

(define-condition compiler-error (simple-error) 
  ((descr :initarg :descr :accessor descr)))

(defconstant +max-no-of-plans+ 1)

(defvar *abort-search* nil)

;;;
;;;
;;;

(defclass compiler-condition ()
  ((candidate :reader candidate :initarg :candidate)
   
   (generator-rank :accessor generator-rank :initform nil)
   (tester-rank :accessor tester-rank :initform nil)
   
   (ignore-when :initarg :ignore-when :initform #'no)
   (deferable-when :initarg :deferable-when :initform #'yes)
   (additional-activated-when-condition :initarg :additional-activated-when-condition :initform #'yes)
   
   (d-flag :reader d-flag
	   :initform t)
   
   (group :accessor group
	  :initarg :group :initform nil)))

(defmethod initialize-instance :after ((obj compiler-condition) &rest initargs)
  (declare (ignore initargs))
  (push obj (compiler-conditions (candidate obj))))

;;;
;;;
;;;

(defclass property (compiler-condition) 
  ((deferable-when :initform #'no)))

(defmethod print-object ((obj property) stream) 
  (format stream "~A(~A)" (type-of obj) (candidate obj)))

(defclass dependency (compiler-condition)
  ((present :reader present :initarg :present)))

(defmethod print-object ((obj dependency) stream) 
  (format stream "~A(~A,~A)" (type-of obj) (candidate obj) (present obj)))

(defclass multidependency (compiler-condition)
  ((args :reader args :initarg :args)))

(defmethod print-object ((obj multidependency) stream) 
  (format stream "~A(~A,~A)" (type-of obj) (candidate obj) (args obj)))

;;;
;;;
;;;

(defmethod find-dependency ((type-symbol symbol) (a compiler-object) (b compiler-object))
  (find-if #'(lambda (cc)
	       (and (typep cc type-symbol)
		    (eq (present cc) b)))
	   (compiler-conditions a)))

;;;
;;;
;;;

(defmethod deferable-p ((cc compiler-condition))
  (and (d-flag cc)
       (funcall (slot-value cc 'deferable-when) cc)))

(defmethod ignored-p ((cc compiler-condition))
  (funcall (slot-value cc 'ignore-when) cc))

(defmacro with-activated-object ((obj) &body body)
  `(progn 
     (activate ,obj)
     ,@body	 
     (deactivate ,obj)))

(defmethod activate ((obj compiler-object))
  (setf (activated-p obj) t)
  (push (get-group-state obj)
	(saved-group-state obj))
  (dolist (cc (compiler-conditions obj))
    (unless (activated-p cc)
      (unless (ignored-p cc)
	(if (not (deferable-p cc))
	    (error "Not deferable!")
	  (defer cc))))))

(defmethod activate :after ((obj nail))
  (unless (typep obj 'origin)
    (let ((transparency (on-transparency obj)))
      (when (matches-with-database-object-p obj)
	(cond ((not (1st-nail transparency)) (setf (1st-nail transparency) obj))
	      ((not (2nd-nail transparency)) (setf (2nd-nail transparency) obj)))))))

;;;
;;;
;;;

(defmethod deactivate ((obj compiler-object))
  (setf (activated-p obj) nil)
  (restore-group-state
   obj
   (pop (saved-group-state obj))))

(defmethod deactivate :after ((obj nail))
  (let ((transparency (on-transparency obj)))
    (cond ((eq (1st-nail transparency) obj) (setf (1st-nail transparency) nil))
	  ((eq (2nd-nail transparency) obj) (setf (2nd-nail transparency) nil)))))

;;;
;;;
;;;

(defmethod activated-p ((dependency dependency))
  (and (activated-p (present dependency))
       (funcall (slot-value dependency 'additional-activated-when-condition) dependency)))

(defmethod activated-p ((property property))
  t)

(defmethod activated-p ((multidependency multidependency))
  (and (every #'activated-p (args multidependency))
       (funcall (slot-value multidependency 'additional-activated-when-condition) multidependency)))

;;;
;;;
;;;

(defmethod valid-p ((cc compiler-condition))
  (or (activated-p cc)
      (ignored-p cc)
      (deferable-p cc)))

(defmethod defer ((cc compiler-condition))
  (when (deferable-p cc)
    (setf (slot-value cc 'd-flag) nil)
    (dolist (other (group cc))
      (setf (slot-value other 'd-flag) nil))))

(defmethod get-group-state ((obj compiler-object))
  (mapcar #'(lambda (cc) 
	      (cons
	       (d-flag cc)
	       (mapcar #'d-flag (group cc))))
	  (compiler-conditions obj)))

(defmethod restore-group-state ((obj compiler-object) (state list))
  (mapcar #'(lambda (cc state)
	      (setf (slot-value cc 'd-flag) (first state))
	      (mapc #'(lambda (cc flag)
			(setf (slot-value cc 'd-flag) flag))
		    (group cc) (rest state)))
	  (compiler-conditions obj) state))

;;;
;;;
;;;

(defmethod get-activated-compiler-conditions ((obj compiler-object))
  (remove-if #'(lambda (cc)
		 (or (not (activated-p cc))
		     (ignored-p cc)))
	     (compiler-conditions obj)))

(defun deactivate-all (objects)
  (dolist (obj objects)
    (deactivate obj)))

(defun activate-all (objects)
  (dolist (obj objects)
    (activate obj)))

;;;
;;;
;;;

(defmethod get-logic-code ((cc compiler-condition) continuation &key &allow-other-keys)
  (funcall continuation))

;;;
;;;
;;;

(defmethod get-generator-code ((cc compiler-condition) continuation &key &allow-other-keys)
  (declare (ignore continuation))
  nil)

(defmethod get-generator-T-code ((cc compiler-condition) continuation  &key &allow-other-keys)
  (declare (ignore continuation))  
  nil)

(defmethod get-generator-NIL-code ((cc compiler-condition) continuation &key &allow-other-keys)
  (declare (ignore continuation))
  nil)

(defmethod get-generator-T->T-code ((cc compiler-condition) continuation &key &allow-other-keys)
  (declare (ignore continuation))
  nil)

(defmethod get-generator-NIL->T-code ((cc compiler-condition) continuation &key &allow-other-keys)
  (declare (ignore continuation))
  nil)

(defmethod get-generator-T->NIL-code ((cc compiler-condition) continuation &key &allow-other-keys)
  (declare (ignore continuation))
  nil)

(defmethod get-generator-NIL->NIL-code ((cc compiler-condition) continuation &key &allow-other-keys)
  (declare (ignore continuation))
  nil)

(defmethod get-tester-code ((cc compiler-condition) continuation &key &allow-other-keys)
  (declare (ignore continuation))
  nil)

(defmethod get-tester-T-code ((cc compiler-condition) continuation &key &allow-other-keys)
  (declare (ignore continuation))
  nil)

(defmethod get-tester-NIL-code ((cc compiler-condition) continuation &key &allow-other-keys)
  (declare (ignore continuation))
  nil)

(defmethod get-tester-T->T-code ((cc compiler-condition) continuation &key &allow-other-keys)
  (declare (ignore continuation))
  nil)

(defmethod get-tester-NIL->T-code ((cc compiler-condition) continuation &key &allow-other-keys)
  (declare (ignore continuation))
  nil)

(defmethod get-tester-T->NIL-code ((cc compiler-condition) continuation &key &allow-other-keys)
  (declare (ignore continuation))
  nil)

(defmethod get-tester-NIL->NIL-code ((cc compiler-condition) continuation &key &allow-other-keys)
  (declare (ignore continuation))
  nil)


(defmethod get-candidate-generator-code ((dependency dependency) continuation)  
  (let ((a (matches-with-database-object-p (candidate dependency)))
	(b (matches-with-database-object-p (present dependency))))
    (cond ((and a b) (or (get-generator-T->T-code dependency continuation)
			 (get-generator-T-code dependency continuation)
			 (get-generator-code dependency continuation)))
	  ((and a (not b)) (or (get-generator-T->NIL-code dependency continuation)
			       (get-generator-T-code dependency continuation)
			       (get-generator-code dependency continuation)	))
	  ((and (not a) b) (or (get-generator-NIL->T-code dependency continuation)
			       (get-generator-NIL-code dependency continuation)
			       (get-generator-code dependency continuation)))
	  ((and (not a) (not b)) (or (get-generator-NIL->NIL-code dependency continuation)
				     (get-generator-NIL-code dependency continuation)
				     (get-generator-code dependency continuation))))))

(defmethod get-candidate-tester-code ((dependency dependency) continuation)
  (let ((a (matches-with-database-object-p (candidate dependency)))
	(b (matches-with-database-object-p (present dependency))))
    (cond ((and a b) (or (get-tester-T->T-code dependency continuation)
			 (get-tester-T-code dependency continuation)
			 (get-tester-code dependency continuation)))
	  ((and a (not b)) (or (get-tester-T->NIL-code dependency continuation)
			       (get-tester-T-code dependency continuation)
			       (get-tester-code dependency continuation)))
	  ((and (not a) b) (or (get-tester-NIL->T-code dependency continuation)
			       (get-tester-NIL-code dependency continuation)
			       (get-tester-code dependency continuation)))
	  ((and (not a) (not b)) (or (get-tester-NIL->NIL-code dependency continuation)
				     (get-tester-NIL-code dependency continuation)
				     (get-tester-code dependency continuation))))))

;;;
;;;
;;;

(defmethod get-candidate-generator-code ((cc compiler-condition) continuation)
  (if (matches-with-database-object-p (candidate cc))
      (or (get-generator-T-code cc continuation)
	  (get-generator-code cc continuation))
    (or (get-generator-NIL-code cc continuation)
	(get-generator-code cc continuation))))


(defmethod get-candidate-tester-code ((cc compiler-condition) continuation)
  (if (matches-with-database-object-p (candidate cc))
      (or (get-tester-T-code cc continuation)
	  (get-tester-code cc continuation))
    (or (get-tester-NIL-code cc continuation)
	(get-tester-code cc continuation))))

;;;
;;;
;;;

(defgeneric legal-successor-p (obj)
  (:method-combination and))

(defmethod legal-successor-p and ((obj visco-object))
  (and (every #'valid-p
	      (compiler-conditions obj))
       (=> (and (not (typep obj 'enclosure))	   
		(not (typep obj 'transparency)))
	   (some #'activated-p (sorted-generators obj)))))

(defmethod legal-successor-p and ((transparency transparency))
  (and (activated-p (origin transparency))
       (let ((no (no-of-required-nails-without-origin transparency)))
	 (case no
	   (0 t)
	   (1 (1st-nail transparency))
	   (2 (and (1st-nail transparency)
		   (2nd-nail transparency)))))))

(defmethod legal-successor-p and ((obj nail))
  (let ((transparency (on-transparency obj)))
    (or (activated-p transparency)	
	(and (=> (not (matches-with-database-object-p obj)) ; Transparency noch nicht aktiv
		 (some #'(lambda (op)	; ein Operator ist aktiv und kann den Nagel konstruieren
			   (every #'activated-p (args op)))
		       (res-of-operators obj)))
	     (or (typep obj 'origin)
		 (and (=> (not (1st-nail transparency)) ; bin ich der erste Nagel ? Wenn JA => ....
			  (activated-p (origin transparency)))
		      (let ((sxt (sx-type transparency))
			    (syt (sy-type transparency))
			    (rt (rot-type transparency)))
			
			(cond ((and (not sxt) (not syt))
			       t)
			      
			      ((and sxt (not syt))
			       (=> (not (1st-nail transparency))
				   (not (= (x (origin transparency))
					   (x obj)))))
			      
			      ((and (not sxt) syt)
			       (=> (not (1st-nail transparency))
				   (not (= (y (origin transparency))
					   (y obj)))))
			      
			      ((and sxt (eq syt 'fac))
			       t)

			      ((and sxt syt (not rt))
			       (and (=> (not (1st-nail transparency))
					(and (not (= (x (origin transparency))
						     (x obj)))
					     (not (= (y (origin transparency))
						     (y obj)))))))
			      
			      ((and sxt syt rt)
			       (=> (and (1st-nail transparency)
					(not (2nd-nail transparency)))
				   (not (zerop (det (x (origin transparency))
						    (y (origin transparency))
						    (x (1st-nail transparency))
						    (y (1st-nail transparency))
						    
						    (x (origin transparency))
						    (y (origin transparency))
						    (x obj)
						    (y obj))))))))))))))

;;;
;;;
;;;

(defun generate-plans (all-nodes &optional (no-of-plans 1))
  (let ((plans nil)
	(plan-counter 0))    
    (labels ((do-it (sub-plan nodes-left)
	       (cond (nodes-left
		      (let ((activated-and-sorted-nodes
			     (sort (remove-if-not 
				    #'legal-successor-p
				    nodes-left)
				   #'compare-nodes)))			
			
			#| (mapc #'(lambda (a)
			(format t "(~A ~A)   " a (get-object-rank a)))
			activated-and-sorted-nodes)
			(terpri) (terpri) |#
			
			(dolist (succ activated-and-sorted-nodes)
			  (with-activated-object (succ)
			    (do-it (cons succ sub-plan)
			      (remove succ nodes-left))))))
		     (t
		      (push (reverse sub-plan) plans)
		      (incf plan-counter)
		      (when (= plan-counter no-of-plans)
			(deactivate-all sub-plan)
			(return-from generate-plans plans))))))
      (do-it nil all-nodes)
      plans)))

(defmethod generate-plan ((query query) &key (infos t))
  (dolist (obj (visco-objects query))
    (setf (compiler-conditions obj) nil)
    (create-compiler-dependencies obj)
    (create-compiler-properties obj))
  
  (setf (all-compiler-conditions query) nil)
  (dolist (obj (visco-objects query))
    (setf (all-compiler-conditions query)
      (append (all-compiler-conditions query) 
	      (compiler-conditions obj))))
  
  (let ((all-ccs (all-compiler-conditions query))) ; Bewerten der CCS
    (dolist (ccs1 (all-compiler-conditions query))
      (when (get-candidate-generator-code ccs1 nil)
	(setf (generator-rank ccs1)
	  (count-if #'(lambda (ccs2)
			(and (not (eq ccs1 ccs2))
			     (get-candidate-generator-code ccs2 nil)
			     (generator-is-better-than-p ccs1 ccs2)))
		    all-ccs)))
      
      (when (get-candidate-tester-code ccs1 nil)
	(setf (tester-rank ccs1)
	  (count-if #'(lambda (ccs2)
			(and (not (eq ccs1 ccs2))
			     (get-candidate-tester-code ccs2 nil)
			     (tester-is-better-than-p ccs1 ccs2)))
		    all-ccs)))))
  
  (dolist (obj (visco-objects query))	; fuer Objekt sortieren
    (setf (sorted-testers obj)
      (sort
       (remove-if-not #'tester-rank (compiler-conditions obj))
       #'> :key #'tester-rank))
    
    (setf (sorted-generators obj)
      (sort
       (remove-if-not #'generator-rank (compiler-conditions obj))
       #'> :key #'generator-rank)))
  
  (dolist (obj (visco-objects query))	; Gruppen installieren
    (install-groups obj)
    #| (when (typep obj 'chain-or-polygon)
    ( install-td-bo-groups obj)) |#
    (when (typep obj 'transparency)
      (setf (1st-nail obj) nil
	    (2nd-nail obj) nil)))
  
  (let ((plan (determine-best-plan   
	       (let ((plans
		      (generate-plans (visco-objects query) +max-no-of-plans+)))
		 (when infos
		   (format t "~%~A plans were generated - please wait..." (length plans))
		   (terpri)
		   #| (princ plans) |#
		   )
		 plans))))
    (when infos (princ plan))
    plan))


(defmethod install-td-bo-groups ((obj chain-or-polygon))
  (dolist (cc1 (compiler-conditions obj))
    (when (typep cc1 'cc-has-segment)
      (dolist (s (get-direct-components obj))
	(dolist (p (get-direct-components s))
	  (dolist (cc2 (compiler-conditions p))
	    (when (typep cc2 'cc-endpoint-of)
	      (push cc1 (group cc2))
	      (push cc2 (group cc1)))))))))



(defconstant +inverses+
    '((cc-joins cc-joins)
      (cc-segment-of cc-has-segment)
      (cc-segment-of cc-construct-chain-or-polygon-from-components) ; Effizienz-Kante
      
      (cc-rubberband-of cc-has-rubberband)
      (cc-rubberband-of cc-construct-chain-or-polygon-from-components) ; Effizienz-Kante
      
      (cc-endpoint-of cc-has-endpoint)	; fuer Ketten und Segmente
      (cc-endpoint-of cc-construct-atomic-rubberband-from-components)       
      (cc-endpoint-of cc-construct-rubberband-from-components)
      
      (cc-point-of cc-has-point)	; fuer Ketten und Polygone ; Effizienz-Kante
      
      (cc-inside cc-contains)
      
      (cc-outside cc-excludes)
      
      (cc-disjoint cc-disjoint)
      
      (cc-intersects cc-intersects)
      
      (cc-has-centroid cc-centroid-of)
      
      (cc-inside-epsilon cc-epsilon-contains)
      
      (cc-angle-between cc-angle-between)
      
      (cc-position-is cc-position-is-inverse)
      
      (cc-orientation-is cc-orientation-is-inverse)
      
      (cc-length-is cc-length-is-inverse)))


(defun cc-inverse (obj)
  (let ((type (type-of obj))
	(res nil))
    (dolist (entry +inverses+)
      (cond ((eq (first entry)
		 type)
	     (push (second entry) res))
	    ((eq (second entry) 
		 type)
	     (push (first entry) res))))
    res))


(defmethod install-groups ((obj compiler-object))
  (dolist (cc (compiler-conditions obj))
    (install-group cc)))

(defmethod install-group ((cc compiler-condition))
  nil)

(defmethod install-group ((cc dependency))  
  (dolist (inverse 
	      (cc-inverse cc))
    (dolist (cci (compiler-conditions 
		  (present cc)))
      (when (typep cci inverse)
	(typecase cci
	  (dependency
	   (when (and (eq (candidate cci)
			  (present cc))
		      (eq (present cci)
			  (candidate cc)))
	     (push cci (group cc))))
	  (multidependency
	   (when (and (eq (candidate cci)
			  (present cc))
		      (member (candidate cc)
			      (args cci)))
	     (push cci (group cc)))))))))

(defmethod install-group ((cc multidependency))  
  (dolist (inverse
	      (cc-inverse cc))
    (dolist (arg (args cc))
      (dolist (cci (compiler-conditions arg))
	(when (typep cci inverse)
	  (typecase cci
	    (dependency
	     (when (and (eq (present cci)
			    (candidate cc))
			(member (candidate cci)
				(args cc)))
	       (push cci (group cc))))))))))

;;;
;;;
;;;

(defgeneric create-compiler-properties (compiler-object)
  (:method-combination progn))

;;;
;;;
;;;

(defmethod create-compiler-properties progn ((obj compiler-object))
  nil)

(defmethod create-compiler-properties progn ((obj query-object))
  (with-slots (status semantics constraints) obj
    (make-type-is obj)
    (when (eq status 'db)		;  (primary-p obj) ist FALSCH !!!
      (make-is-primary obj))
    #| (when (matches-with-database-object-p obj)
    (make-legal-binding-is obj)) |#
    (when semantics
      (make-semantics-is obj semantics))
    #| (when constraints
    (make-relations-exists-is obj constraints)) |#
    (when (matches-with-database-object-p obj)
      (make-is-whole-DB-generator obj))))

(defmethod create-compiler-properties progn ((obj chain-or-polygon))
  (make-at-least-has-segments-is obj (length (segments obj)))
  (when (every #'(lambda (s)
		   (typep s 'atomic-rubberband))
	       (segments obj))
    (make-at-most-has-segments-is obj (length (segments obj)))))


(defmethod create-compiler-properties progn ((obj at-most-has-segments-constraint-mixin))
  (with-slots (at-most-constraint) obj
    (when at-most-constraint
      (make-at-most-has-segments-is obj at-most-constraint))))

#|

;;; noch nicht implementiert

(defmethod create-compiler-properties progn ((obj at-most-contains-constraint-mixin))
  (with-slots (at-most-constraint) obj
    (when at-most-constraint
      (make-at-most-contains-is obj at-most-constraint))))

|#

;;;
;;;
;;;

(defgeneric create-compiler-dependencies (compiler-object)
  (:method-combination progn))

;;;
;;;
;;;

(defmethod create-compiler-dependencies progn ((obj compiler-object))
  nil)

(defmethod create-compiler-dependencies progn ((obj epsilon-enclosure))  
  (make-depends-on obj (on-transparency obj) :deferable-when #'no)) ; wegen Radius

(defmethod create-compiler-dependencies progn ((obj transparency)) 
  (dolist (other (transparency-query-objects-and-enclosures obj))
    (let ((other other))
      (typecase other
	(nail
	 (make-position-is-inverse obj other
				   :ignore-when #'(lambda (self)
						    (declare (ignore self))
						    (let ((no (no-of-required-nails-without-origin obj)))
						      (or (eq other (origin obj))
							  (and (eq other (1st-nail obj))
							       (> no 0))
					; denn dann wurde Nail zur Bestimmung der Transformation benutzt => OK
							  (and (eq other (2nd-nail obj))
							       (> no 1)))))))
					; s.o.
	(atomic-rubberband
	 (when (orientation-constraint other)
	   (make-orientation-is-inverse obj other
					(orientation-constraint other)
					:ignore-when #'(lambda (self)
							 (declare (ignore self))
							 (and (activated-p (p1 other))
							      (activated-p (p2 other))
							      (typep (p1 other) 'nail)
							      (typep (p2 other) 'nail)))))
	 (when (typep other '(or atomic-<=-rubberband atomic->=-rubberband beam))    
	   (make-length-is-inverse obj other
				   :ignore-when #'(lambda (self)
						    (declare (ignore self))
						    (and (activated-p (p1 other))
							 (activated-p (p2 other))
							 (typep (p1 other) 'nail)
							 (typep (p2 other) 'nail))))))))))

(defmethod create-compiler-dependencies progn ((obj nail))
  (make-position-is obj (on-transparency obj)))

(defmethod create-compiler-dependencies progn ((obj point))
  (when (matches-with-database-object-p obj) ; DB, DB-COMPONENT
    (let ((lines
	   (append (p1-of obj)
		   (p2-of obj))))
      
      (dolist (line lines)
	(when (matches-with-database-object-p line) ; DB, DB-COMPONENT atomic-rubb., DB rubb.
	  (make-endpoint-of obj line)))

      (dolist (polygon-or-chain
		  (remove-duplicates (apply #'append (mapcar #'part-of lines))))
	(when (matches-with-database-object-p polygon-or-chain)
	  (make-point-of obj polygon-or-chain
			 :ignore-when #'(lambda (self)
					  (declare (ignore self))
					  (some #'activated-p
						(part-of obj)))))))))

(defmethod create-compiler-dependencies progn ((obj atomic-rubberband))
  (when (orientation-constraint obj)
    (make-orientation-is obj (on-transparency obj)
			 (orientation-constraint obj)
			 :ignore-when #'(lambda (self)
					  (declare (ignore self))
					  (and (activated-p (p1 obj))
					       (activated-p (p2 obj))
					       (typep (p1 obj) 'nail)
					       (typep (p2 obj) 'nail)))))
  
  (when (typep obj '(or atomic-<=-rubberband atomic->=-rubberband beam))		           
    (make-length-is obj (on-transparency obj)
		    :ignore-when #'(lambda (self)
				     (declare (ignore self))
				     (and (activated-p (p1 obj))
					  (activated-p (p2 obj))
					  (typep (p1 obj) 'nail)
					  (typep (p2 obj) 'nail)))))
  
  (case (status obj)
    (universe
     (make-construct-atomic-rubberband-from-components obj (list (p1 obj) (p2 obj))
						       :deferable-when #'no))
    ((db db-component)
     (make-has-endpoint obj (p1 obj)
			:ignore-when #'(lambda (self) ; denn dann ist "construct from" effizienter
					 (declare (ignore self))
					 (and (activated-p (p1 obj))
					      (activated-p (p2 obj)))))
     (make-has-endpoint obj (p2 obj)
			:ignore-when #'(lambda (self)
					 (declare (ignore self))
					 (and (activated-p (p1 obj))
					      (activated-p (p2 obj)))))
     
     (make-construct-atomic-rubberband-from-components obj (list (p1 obj) (p2 obj)))
     
     (dolist (chain-or-polygon (part-of obj))
       (when (matches-with-database-object-p chain-or-polygon)
	 (make-segment-of obj chain-or-polygon))))))


(defmethod create-compiler-dependencies progn ((obj rubberband))
  (case (status obj)
    (universe
     (make-construct-rubberband-from-components obj (list (p1 obj) (p2 obj))
						:deferable-when #'no))
    
    ((db db-component)
     (make-has-endpoint obj (p1 obj)
			:ignore-when #'(lambda (self) ; denn dann ist "construct from" effizienter
					 (declare (ignore self))
					 (and (activated-p (p1 obj))
					      (activated-p (p2 obj)))))
     (make-has-endpoint obj (p2 obj)
			:ignore-when #'(lambda (self)
					 (declare (ignore self))
					 (and (activated-p (p1 obj))
					      (activated-p (p2 obj)))))
     
     (make-construct-rubberband-from-components obj (list (p1 obj) (p2 obj)))
     
     (dolist (chain-or-polygon (part-of obj))
       (when (matches-with-database-object-p chain-or-polygon)
	 (make-rubberband-of obj chain-or-polygon))))))


(defmethod create-compiler-dependencies progn ((obj chain-or-polygon))
  (let ((segments (segments obj)))
    (case (status obj)
      (universe
       (make-construct-chain-or-polygon-from-components obj segments
							:deferable-when #'no))
      ((db db-component)
       (dolist (segment segments)
	 (if (typep segment 'rubberband)
	     (make-has-rubberband obj segment 
				  :ignore-when #'(lambda (self)
						   (declare (ignore self))
						   (every #'activated-p segments)))
	   (make-has-segment obj segment
			     :ignore-when #'(lambda (self)
					      (declare (ignore self))
					      (every #'activated-p segments)))))
       
       #| (multiple-value-call #'mapc #'(lambda (s1 s2)
       (make-joins s1 s2))
       (if (typep obj 'chain)
       (values segments (rest segments))
       (values (cons (first (last segments)) segments) segments))) |#
       
       (make-construct-chain-or-polygon-from-components obj segments)

       (dolist (point (point-list obj))
	 (let ((point point))					
	   (make-has-point obj point 
			   :ignore-when #'(lambda (self)
					    (declare (ignore self))
					    (some #'activated-p
						  (part-of point))))))))))

;;;
;;;
;;;

(defmethod create-compiler-dependencies progn ((obj on-transparency-mixin))  
  (when (and (matches-with-database-object-p obj)
	     (not (typep obj 'nail)))
    (make-inside obj (on-transparency obj) :ignore-when #'yes)))

(defmethod create-compiler-dependencies progn ((obj constraints-mixin))
  (dolist (constraint (constraints obj))
    (map-constraint-to-dependency constraint)))

(defmethod create-compiler-dependencies progn ((obj possible-operator-argument-mixin))
  (dolist (op (arg-of-operators obj))
    (with-slots (args res) op
      (typecase op
	(create-centroid		     
	 (when (and (matches-with-database-object-p res)
		    (matches-with-database-object-p obj))
	   (make-has-centroid obj res)))
	(create-intersection-point 
	 (when (every #'matches-with-database-object-p (cons res args))
	   nil))))))

(defmethod create-compiler-dependencies progn ((obj possible-operator-result-mixin))
  (unless (typep obj 'derived-enclosure) ; Sonderregelung: DERIVED ENCLOSURES werden nicht konstruiert
    (dolist (op (res-of-operators obj))
      (with-slots (args res) op
	(typecase op
	  (create-centroid		     
	   (make-centroid-of obj (first args)
			     :deferable-when (if (and (matches-with-database-object-p res)
						      (matches-with-database-object-p obj)) ; s. obige Inverse!
						 #'yes
					       #'no)))
	  (create-intersection-point 
	   (unless (every #'matches-with-database-object-p (cons res args))
	     (make-construct-intersection-point res args :deferable-when #'no))))))))

;;;
;;;
;;;
;;;

(defmethod map-constraint-to-dependency ((constraint binary-constraint))
  (with-slots (1st-arg 2nd-arg) constraint
    (labels ((transparency-must-be-activated (arg)
	       #'(lambda (self)
		   (declare (ignore self))
		   (activated-p (on-transparency arg))))
	     (all-components-or-some-master (a rel b) 
	       #'(lambda (self)		   
		   (declare (ignore self))
		   (or (let ((components 
			      (remove-if-not #'(lambda (o)
						 (and (typep o 'visco-object)
						      (activated-p o)))
					     (get-direct-components a))))
			 (and components 
			      (every #'(lambda (c)
					 (let ((cc (find-dependency rel c b)))
					   (and cc (not (d-flag cc)))))
				     components)))
		       
		       (some #'(lambda (m)
				 (let ((cc (find-dependency rel m b)))
				   (and cc (not (d-flag cc)))))
			     (remove-if-not #'(lambda (o)
						(and (typep o 'visco-object)
						     (activated-p o)))
					    (part-of a)))))))
      
      (etypecase constraint
	(angle-between (make-angle-between 1st-arg 2nd-arg 
					   (allowed-derivation constraint)
					   :additional-activated-when-condition
					   (transparency-must-be-activated 1st-arg)
					   :ignore-when #'(lambda (self)
							    (declare (ignore self))
							    (every #'(lambda (p)
								       (and (activated-p p)
									    (typep p 'nail)))
								   (list (p1 1st-arg)
									 (p2 1st-arg)
									 (p1 2nd-arg)
									 (p2 2nd-arg))))))
	
	(intersects (when (not (or (reduce #'intersection ; gemeinsamer Punkt? -> kein INTERSECTS-Constraint notw.
					   (mapcar #'(lambda (arg)
						       (typecase arg
							 (point (list arg))
							 (otherwise (point-list arg))))
						   (list 1st-arg 2nd-arg)))
				   (and (not (may-vary-p 1st-arg))
					(not (may-vary-p 2nd-arg)))
				   (and (typep 1st-arg '(or line point))
					(typep 2nd-arg '(or line point))
					(or (ignore-disjoint-and-intersects-relations-p 1st-arg)
					    (ignore-disjoint-and-intersects-relations-p 2nd-arg)))))
		      
		      (make-intersects 1st-arg 2nd-arg
				       :ignore-when #'(lambda (self)
							(declare (ignore self))
							(some #'(lambda (c)
								  (let ((cc
									 (find-dependency 'cc-intersects c 2nd-arg)))
								    (and cc (not (d-flag cc)))))
							      (remove-if-not #'activated-p
									     (get-all-components 1st-arg)))))))
	
	(disjoint (when (not (or (and (typep 1st-arg 'point)
				      (typep 2nd-arg 'point))
				 (and (not (may-vary-p 1st-arg))
				      (not (may-vary-p 2nd-arg)))
				 (and (typep 1st-arg '(or line point))
				      (typep 2nd-arg '(or line point))
				      (or (ignore-disjoint-and-intersects-relations-p 1st-arg)
					  (ignore-disjoint-and-intersects-relations-p 2nd-arg)))
				 
				 (some #'(lambda (m)
					   (and (typep m 'possible-operator-argument-mixin)
						(some #'(lambda (op)							
							  (let ((2nd-arg (res op)))
							    (find-constraint 'inside 1st-arg 2nd-arg)))
						      (arg-of-operators m))))
				       (get-all-masters 2nd-arg))
				 
				 (some #'(lambda (m)
					   (and (typep m 'possible-operator-argument-mixin)
						(some #'(lambda (op)							
							  (let ((1st-arg (res op)))
							    (find-constraint 'inside 2nd-arg 1st-arg)))
						      (arg-of-operators m))))
				       (get-all-masters 1st-arg))))
		    
		    (make-disjoint 1st-arg 2nd-arg
				   :ignore-when (all-components-or-some-master 1st-arg 'cc-disjoint 2nd-arg))))
	
	(inside (unless (typep 1st-arg 'nail)
		  (if (typep 2nd-arg 'drawn-enclosure)
		      (if (negated-p 2nd-arg)
			  (make-outside 1st-arg 2nd-arg
					:ignore-when (all-components-or-some-master 1st-arg 'cc-outside 2nd-arg)
					:additional-activated-when-condition
					(transparency-must-be-activated 1st-arg))
			(make-inside 1st-arg 2nd-arg
				     :ignore-when (all-components-or-some-master 1st-arg 'cc-inside 2nd-arg)
				     :additional-activated-when-condition
				     (transparency-must-be-activated 1st-arg)))
		    (when (or (may-vary-p 1st-arg) 
			      (may-vary-p 2nd-arg))
		      (typecase 2nd-arg
			(epsilon-enclosure
			 (make-inside-epsilon 1st-arg (arg-object 2nd-arg) (radius 2nd-arg)
					      :ignore-when 
					      (all-components-or-some-master 
					       1st-arg 'cc-inside-epsilon (arg-object 2nd-arg))
					      :additional-activated-when-condition 
					      (transparency-must-be-activated 1st-arg)))
			(inner-enclosure
			 (make-inside 1st-arg (arg-object 2nd-arg)
				      :ignore-when 
				      (all-components-or-some-master 1st-arg 'cc-inside (arg-object 2nd-arg))))
			(outer-enclosure
			 (make-outside 1st-arg (arg-object 2nd-arg)
				       :ignore-when
				       (all-components-or-some-master 1st-arg 'cc-outside (arg-object 2nd-arg)))))))))
	
	(contains (unless (typep 2nd-arg 'nail)
		    (if (typep 1st-arg 'drawn-enclosure)
			(if (negated-p 1st-arg)
			    (make-excludes 1st-arg 2nd-arg
					   :ignore-when (all-components-or-some-master 1st-arg 'cc-excludes 2nd-arg)
					   :additional-activated-when-condition
					   (transparency-must-be-activated 1st-arg))
			  (make-contains 1st-arg 2nd-arg
					 :ignore-when (all-components-or-some-master 1st-arg 'cc-contains 2nd-arg)
					 :additional-activated-when-condition
					 (transparency-must-be-activated 1st-arg)))
		      (when (or (may-vary-p 1st-arg) 
				(may-vary-p 2nd-arg))
			(typecase 1st-arg
			  (epsilon-enclosure
			   (make-epsilon-contains (arg-object 1st-arg) 2nd-arg (radius 1st-arg)
						  :ignore-when 
						  (all-components-or-some-master (arg-object 1st-arg)
										 'cc-epsilon-contains 2nd-arg)
						  :additional-activated-when-condition
						  (transparency-must-be-activated 1st-arg)))
			  (inner-enclosure
			   (make-contains (arg-object 1st-arg) 2nd-arg
					  :ignore-when (all-components-or-some-master (arg-object 1st-arg)
										      'cc-contains 2nd-arg)))
			  (outer-enclosure
			   (make-excludes (arg-object 1st-arg) 2nd-arg
					  :ignore-when
					  (all-components-or-some-master (arg-object 1st-arg)
									 'cc-excludes 2nd-arg))))))))))))

;;;
;;;
;;; 

(defmethod y-mirror-query ((query query))
  (dolist (obj (visco-objects query))
    (typecase obj
      (point (setf (y obj) (- (y obj))))
      ((or transparency drawn-enclosure)
       (dolist (point (point-list obj))
	 (setf (y point) (- (y point))))))
    (when (typep obj 'geom-chain-or-polygon)
      (invalidate-bounding-box obj))))

(defmethod reset-query ((query query))
  (dolist (obj (elements spatial-index::*cur-index*))
    (setf (bound-to obj) nil))
  (dolist (obj (visco-objects query))
    (when (typep obj 'query-object)
      (setf (bound-to obj) nil))))

(defmethod invalidate ((query query))
  (setf (exec-fn query) nil))

(defmethod executable-p ((query query))
  (exec-fn query))

(defmethod execute-query ((query query))
  (let ((db (get-current-db)))
    (setf *abort-search* nil)
    (when (and db (exec-fn query))
      (reset-query query)
      (y-mirror-query query)
      (catch 'abort
	(funcall (exec-fn query)))
      (reset-query query)
      (y-mirror-query query))))

(defmethod get-name-for-exec-fn ((obj compiler-object))
  (intern 
   (format nil "EXEC-~A" (name obj))))

;;;
;;;
;;;

(defmethod compile-query ((query query) &key (debug t) (check-for-abort t) (infos t))
  (let* ((plan (generate-plan query :infos infos))
	 (n (length plan)))
    (unless plan
      (error "Bad query! No plan(s)!"))
    (y-mirror-query query)
    (mapc #'(lambda (obj next-obj)
	      (activate obj)
	      (compile-it obj next-obj
			  :infos infos
			  :debug debug
			  :check-for-abort check-for-abort
			  :plan-length n
			  :plan-pos (1+ (position obj plan))))
	  plan (append (rest plan) '(nil)))
    (y-mirror-query query)
    (deactivate-all (reverse plan))
    (setf (exec-fn query)
      (get-name-for-exec-fn (first plan)))))

;;;
;;;
;;;

(defun get-query-semantics (query)
  (let ((plan (generate-plan query)))
    (unless plan
      (error "Bad query! No plan(s)!"))
    (prog1 
	(first (get-semantics plan))
      (deactivate-all (reverse plan)))))

(defun get-semantics (plan)
  (let ((obj (first plan))
	(rest-plan (rest plan)))
    (when obj
      (activate obj)
      (get-object-semantics obj #'(lambda () (get-semantics rest-plan))))))

(defmethod get-object-semantics ((obj visco-object) continuation)
  `( (exists (,obj)
	     (and ,@(get-condition-semantics  
		     (remove-if #'(lambda (cc)
				    (not (activated-p cc)))
				(compiler-conditions obj)))
		  ,@(funcall continuation)))))

(defun get-transparency-parameters (transparency)
  (mapcar #'(lambda (sym)
	      (get-transparency-parameter-name transparency sym))
	  '(tx ty sx sy r)))

(defun get-transparency-parameter-name (transparency name)
  (intern (string-upcase (format nil "~A-~A" name transparency))))

(defun get-const-name (obj)
  (intern (string-upcase (format nil "+CONST-~A+" obj))))

(defun get-transformed-object (obj)
  `(transform ,(get-const-name obj) ,@(get-transparency-parameters (on-transparency obj))))

(defun get-inverse-transformed-object (obj)
  `(inverse-transform ,obj ,@(get-transparency-parameters (on-transparency obj))))

(defmethod get-object-semantics ((obj transparency) continuation)
  (let ((tx (get-transparency-parameter-name obj 'tx))
	(ty (get-transparency-parameter-name obj 'ty))
	(sx (get-transparency-parameter-name obj 'sx))
	(sy (get-transparency-parameter-name obj 'sy))
	(r (get-transparency-parameter-name obj 'r)))
    `( (exists (,obj ,tx ,ty ,sx ,sy ,r)
	       (and (rectangle ,obj)
		    (real ,tx)
		    (real ,ty)
		    (real ,sx)
		    (real ,sy)
		    (real ,r)
		    (= ,obj ,(get-transformed-object obj))
		    ,@(when (sxmin obj)
			`( (>= sx ,(sxmin obj))))
		    ,@(when (symin obj)
			`( (>= sy ,(symin obj))))
		    ,@(when (sxmax obj)
			`( (<= sx ,(sxmax obj))))
		    ,@(when (symax obj)
			`( (<= sy ,(symax obj))))
		    ,@(when (sxsy-constraint obj)
			`( (= ,sx ,sy)))
		    ,@(when (orientation-constraint (origin obj))
			(list (get-check-circle-ticks-code 'r (orientation-constraint (origin obj)))))
		    
		    ,@(get-condition-semantics
		       (remove-if #'(lambda (cc)
				      (not (activated-p cc)))
				  (compiler-conditions obj)))
		    ,@(funcall continuation))))))

(defmethod get-object-semantics ((obj drawn-enclosure) continuation)
  `( ,@(get-condition-semantics
	(remove-if #'(lambda (cc)
		       (not (activated-p cc)))
		   (compiler-conditions obj)))
       ,@(funcall continuation)))

(defun get-condition-semantics (ccs)
  (when ccs
    `( ,@(get-logic-code (first ccs)
			 #'(lambda ()
			     (get-condition-semantics (rest ccs)))))))

;;;
;;;
;;;

#|
(defun show-search-progress (n)
  nil)
|#

(defmethod compile-it (obj next-obj &key &allow-other-keys)
  (declare (ignore obj next-obj))
  nil)

(defmethod compile-it :around ((obj compiler-object) next-obj &key (debug nil) (check-for-abort t)
								   (infos t)
								   plan-pos plan-length)
  (declare (ignore next-obj))
  
  (when infos
    (format t "~%-------------------------------------------------~%")
    (format t "~%Compiling ~A:~%" obj)
    (format t "~%All Compiler Conditions: ~A" (compiler-conditions obj))
    (format t "~%Activated & Sorted Compiler Conditions: ~A" 
	    (activated-and-sorted-compiler-conditions obj)))
  
  (let ((source 	 
	 `(lambda () 
	    ,@(when debug
		`((format t "Trying to match: ~A~%" ,obj)))
	    (show-search-progress ,(/ plan-pos plan-length))
	    ,@(when check-for-abort
		`((when *abort-search*	   
		    (throw 'abort nil))))
	    ,@(call-next-method))))
    (when infos (format t "~%~%~A :~%~A~%" (get-name-for-exec-fn obj) source))
    (setf (exec-fn obj)
      (compile (get-name-for-exec-fn obj) source))
    (setf (exec-source obj)
      source)))

;;;
;;;
;;;


(defmethod compile-it ((obj transparency) next-obj &key &allow-other-keys)
  `( (when (and ,@(compile-conditions 
		   (activated-and-sorted-compiler-conditions obj)))
       ,@(compile-next next-obj))))

(defmethod compile-it ((obj visco-object) next-obj &key &allow-other-keys)
  `( (let ((candidate ,obj))
       (when (and ,@(compile-conditions 
		     (activated-and-sorted-compiler-conditions obj)))
	 ,@(compile-next next-obj)))))

(defmethod compile-it ((obj query-object) next-obj &key (check-for-abort t) &allow-other-keys)
  (let ((generator    
	 (first (activated-and-sorted-compiler-conditions obj)))
	(cont  
	 #'(lambda ()
	     `( ,@(when check-for-abort
		    `((when *abort-search*	   
			(throw 'abort nil))))
		  (when (and ,@(compile-conditions
				(rest (activated-and-sorted-compiler-conditions obj))))
		    ,@(compile-next next-obj))))))
    (if (get-candidate-generator-code generator nil)
	(get-candidate-generator-code generator cont)
      (error "No candidate generator!"))))

(defmethod compile-it ((obj nail) next-obj &key (check-for-abort t) &allow-other-keys)
  (let* ((transparency (on-transparency obj))
	 (origin (origin transparency))
	 (sxt (sx-type transparency))	; nil, free 
	 (syt (sy-type transparency))	; nil, fac, free
	 (sxi (sx-s->w transparency))
	 (syi (sy-s->w transparency))	 
	 (sxsy-constraint (sxsy-constraint transparency))
	 (rt (rot-type transparency))
	 (no (no-of-required-nails-without-origin transparency)))
    
    (labels ((get-check-and-set-code (paste-in) ; Input: sx, sy, r
	       `( ,@(when check-for-abort
		      `((when *abort-search*	   
			  (throw 'abort nil))))
		    (when (and ,@(compile-conditions
				  (rest (activated-and-sorted-compiler-conditions obj))))
		      
		      (let* (sx sy r
			     (origin-bound-to (bound-to ,(origin transparency)))
			     (ox ,(slot-value origin 'x))
			     (oy ,(slot-value origin 'y))
			     (tx (slot-value origin-bound-to 'x))
			     (ty (slot-value origin-bound-to 'y)))
			
			,@paste-in
			
			(when (and sx (plusp sx)
				   (let ((a (/ sx ,sxi)))
				     ,(get-check-ticks-code 'a
							    (list (list (sxmin transparency)
									(sxmax transparency)))))
				   sy (plusp sy)
				   (let ((b (/ sy ,syi)))
				     ,(get-check-ticks-code 'b
							    (list (list (symin transparency)
									(symax transparency)))))
				   ,@(when (eq syt 'fac)
				       `((=-eps sy (* sx ,sxsy-constraint))))
				   
				   ,(get-check-circle-ticks-code
				     'r 
				     (orientation-constraint (origin transparency))))
			  
			  (setf (sx ,transparency) sx
				(sy ,transparency) sy
				(r ,transparency) r
				(epsilon-a ,transparency)
				,(if (eq rt 'free)
				     `(abs (- (* sx (cos r)) (* sy (sin r))))
				   'sx)
				(epsilon-b ,transparency) 
				,(if (eq rt 'free)
				     `(abs (+ (* sx (sin r)) (* sy (cos r))))
				   'sy))
			  
			  (let ((matrix ,(matrix transparency))
				(inverse-matrix ,(inverse-matrix transparency)))
			    
			    (reset matrix)
			    (translate matrix (- ox) (- oy))
			    (scale matrix sx sy)
			    ,@(when (eq rt 'free)
				`((rotate matrix r)))
			    (translate matrix tx ty)
			    
			    (reset inverse-matrix)
			    (translate inverse-matrix (- tx) (- ty))
			    ,@(when (eq rt 'free)
				`((rotate inverse-matrix (- r))))
			    (scale inverse-matrix (/ 1 sx) (/ 1 sy))
			    (translate inverse-matrix ox oy)
			    
			    ,@(compile-next next-obj))))))))
      
      (let* ((generator
	      (first (activated-and-sorted-compiler-conditions obj)))
	     (cont
	      (with-slots (origin sxsy-constraint) transparency
		(cond ((and (eq obj origin) ; (nil nil nil)
			    (= no 0))
		       
		       #'(lambda ()
			   (get-check-and-set-code 
			    `((setf sx ,sxi
				    sy ,syi
				    r 0)))))
		      
		      ((and (eq obj (1st-nail transparency))
			    (= no 1))
		       
		       (if (not rt)	; also (free free nil) (free fac nil) (free nil nil) (nil free nil)
			   #'(lambda ()
			       (get-check-and-set-code
				`((let ((sdx ,(- (slot-value obj 'x)
						 (slot-value origin 'x)))
					(sdy ,(- (slot-value obj 'y)
						 (slot-value origin 'y)))
					
					(wdx (- (slot-value candidate 'x)
						(slot-value origin-bound-to 'x)))
					(wdy (- (slot-value candidate 'y)
						(slot-value origin-bound-to 'y))))
				    
				    (setf r 0)
				    (setf sx (unless (or (zerop wdx) (zerop sdx))
					       (/ wdx sdx)))
				    (setf sy (unless (or (zerop wdy) (zerop sdy))
					       (/ wdy sdy)))
				    ,@(when (zerop (- (slot-value obj 'x)
						      (slot-value origin 'x)))
					`((unless (zerop wdx)
					    (setf sx nil
						  sy nil))))
				    ,@(when (zerop (- (slot-value obj 'y)
						      (slot-value origin 'y)))
					`((unless (zerop wdy)
					    (setf sx nil
						  sy nil)))))
				  
				  
				  ,@(when sxsy-constraint
				      `((when (and sx (not sy))
					  (setf sy (* sx ,sxsy-constraint)))
					(when (and sy (not sx))
					  (setf sx (/ sy ,sxsy-constraint))))))))
			 
			 #'(lambda ()	; (nil nil free) (free nil free) (nil free free) (free fac free) 
			     (get-check-and-set-code 
			      `((multiple-value-bind (s rot)
				    ( ,(if (or (and (not sxt) (not syt))
					       sxsy-constraint)
					   'proportional-2-point
					 (if (and sxt (not syt))
					     #| 'fixed-sy-2-point |#
					   (error "fixed-sy-2-point-transformation: not implemented!")
					   #| 'fixed-sx-2-point |# 
					   (error "fixed-sx-2-point-transformation: not implemented!")))
					
					ox oy
				      ,(slot-value obj 'x)
				      ,(slot-value obj 'y)
				      
				      tx ty
				      (slot-value candidate 'x)
				      (slot-value candidate 'y))
				  
				  (setf r rot)
				  ,@(when (eq sxt 'free)
				      `((setf sx s)))
				  ,@(when (eq syt 'free)
				      `((setf sy s)))
				  ,@(when sxsy-constraint
				      `((when (and sx (not sy))
					  (setf sy (* s ,sxsy-constraint)))
					(when (and sy (not sx))
					  (setf sx (/ s ,sxsy-constraint)))))
				  ,@(when (and (not sxt) (not syt))
				      `((setf sx s
					      sy s)))))))))
		      
		      ((> no 1)		; (free free free)
		       (error "3-point-transformation: not implemented!"))
		      
		      (t		; normaler Nagel, wird nicht zur Trafo.best. benutzt! 	            
		       #'(lambda ()	
			   `( ,@(when check-for-abort
				  `((when *abort-search*	   
				      (throw 'abort nil))))
				(when (and ,@(compile-conditions
					      (rest (activated-and-sorted-compiler-conditions obj))))
				  ,@(compile-next next-obj)))))))))
	
	(if (get-candidate-generator-code generator nil)	 
	    (get-candidate-generator-code generator cont)
	  (error "No candidate generator!"))))))


;;;
;;;
;;;


(defun compile-next (next-obj)
  (if next-obj
      `( (,(get-name-for-exec-fn next-obj) ) )
    `( (ready))))

(defun compile-conditions (all-ccs)
  (when all-ccs
    (get-candidate-tester-code (first all-ccs)
			       #'(lambda ()
				   (compile-conditions (rest all-ccs))))))

;;;
;;; Testhilfe: 
;;;


(defun legal-subplan (plan)
  (let ((trace 
	 (mapcar #'(lambda (succ)
		     (when
			 (legal-successor-p succ)
		       (activate succ)
		       succ))
		 plan)))
    (deactivate-all (remove nil (reverse trace)))
    trace))


(defun list-all-ccs (q)
  (dolist (o (visco-objects q))
    (princ o) (terpri)
    (princ (compiler-conditions o)) (terpri)))

