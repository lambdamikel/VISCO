;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: QUERY-COMPILER; Base: 10 -*-

(in-package query-compiler)

(eval-when (:compile-toplevel :load-toplevel)
  
  (defun get-args (lambda-list)      
    (dolist (item '(&rest &optional &key))
      (setf lambda-list (remove item lambda-list)))
    (mapcar #'(lambda (entry)
		(if (listp entry)
		    (first entry)
		    entry))
	    lambda-list)))



(eval-when (:compile-toplevel :load-toplevel)
  (defmacro defoperator (name lambda-list
			      (&key stored-operator additional-slots after-code)
			      (&key precondition)
			      (&key code))
    (let ((precond-name (intern 
		         (format nil "~A-APPLICABLE-P" name)))
	  (apply-name (intern 
		       (format nil "APPLY-~A" name))))
      
      `(progn
         ,(when precondition
            `(progn 
	       (defmethod ,precond-name ,(mapcar #'(lambda (lambda-entry)
						     (if (listp lambda-entry)
                                                       (first lambda-entry)
                                                       lambda-entry))
						 lambda-list)
		          (declare (ignore ,@(get-args lambda-list)))
		 nil)
	       (defmethod ,precond-name ,lambda-list ,@precondition)))
         ,(if stored-operator
	    `(progn
	       (defpersistentclass ,name (operator) 
	         ,(mapcar #'(lambda (slot)
			      `(,slot :accessor ,slot :initarg 
			              ,(intern (string-upcase (write-to-string slot))
				               (find-package 'keyword))))
			  additional-slots))
	       (defmethod ,apply-name ,lambda-list 
	                  ,(let ((args (get-args lambda-list)))
		             `(if (apply #',precond-name ,@args ,@(unless (member '&rest lambda-list)
								    '(nil)))
		                (multiple-value-bind (result args initargs)
			                             ,@code
			          (let ((operator
				         (apply #'make-instance ',name
				                :res result :args args initargs)))
			            (push operator (res-of-operators result))
                                    
			            (dolist (arg args)
			              (push operator (arg-of-operators arg)))
			            ,@(when after-code					 
				        (list after-code))
			            result))
		                'not-applicable))))
	    `(defmethod ,apply-name ,lambda-list 
	                ,(let ((args (get-args lambda-list)))
		           `(if 
		              (apply #',precond-name ,@args ,@(unless (member '&rest lambda-list)
							        '(nil)))
		              ,@code
		              'not-applicable))))))))


;;;
;;;
;;;

(defpersistentclass operator ()
  ((args :accessor args :initarg :args)
   (res :accessor res :initarg :res)))

(defmethod binary-operator-p ((op operator))
  (null (rest (args op))))

;;;
;;;
;;;

;;;
;;; Hier fehlen die at-most-Constraint-Checks fuer die Enclosures! Der Opererator ist noch nicht implementiert, da kompliziert.
;;; m.a.W.: ein Objekt darf nicht in eine Enclosure gesetzt werden, wenn dadurch der at-most Constraint der Enclosure verletzt wird.
;;;

#|					;
(=> primary-p 
    (let ((p (apply #'make-visco-marble transparency status x y :dont-initialize t initargs)))
      (with-temporary-inserted-object (p)
	(call-all-enclosure-at-most-constraints p))))
|#

(defoperator create-transparency ((query query) xmin ymin xmax ymax &rest initargs)
  (:stored-operator nil)
  (:precondition ((declare (ignore initargs))
		  (not (zerop (* (- xmax xmin) 
				 (- ymax ymin))))))
  (:code ((apply #'make-visco-transparency query xmin ymin xmax ymax initargs))))

;;;
;;;
;;;

(defun check-point (x y status type operator-result transparency)
  (and (not (get-present-point-at (query transparency) x y))
       (status-of-point-object-ok-p status type operator-result)
       (inside-p* x y transparency)
       (every #'(lambda (obj)
		  (=> (typep obj 'point)
		      (> (distance-between* x y (x obj) (y obj)) +intersects-threshold+)))
	      (visco-objects (query transparency)))))
	
(defoperator create-marble ((transparency transparency) (status symbol) (x number) (y number) operator-result &rest initargs)
  (:stored-operator nil)
  (:precondition ((declare (ignore initargs))
		  (and (check-point x y status 'marble operator-result transparency)
		       (inside-any-enclosure-p* x y (query transparency)))))
  (:code ((apply #'make-visco-marble transparency status x y initargs))))


(defoperator create-nail ((transparency transparency) (status symbol) (x number) (y number) operator-result &rest initargs)
  (:stored-operator nil)
  (:precondition ((declare (ignore initargs))
		  (check-point x y status 'nail operator-result transparency)))
  (:code ((apply #'make-visco-nail transparency status x y initargs))))


(defoperator create-origin ((transparency transparency) (status symbol) (x number) (y number) operator-result &rest initargs)
  (:stored-operator nil)
  (:precondition ((declare (ignore initargs))
		  (and (not (origin transparency))
		       (check-point x y status 'origin operator-result transparency))))
  (:code ((apply #'make-visco-origin transparency status x y initargs))))

;;;
;;;
;;;

(defun same-transparency-p (&rest args)  
  (let ((transparency (on-transparency (first args))))
    (every #'(lambda (arg)
	       (eq (on-transparency arg) transparency))
	   (rest args))))

(defun check-line (p1 p2 status type)
  (and (same-transparency-p p1 p2)
       (not (point-=-p p1 p2))
       (not (get-already-present-direct-master p1 p2))
       (status-of-line-object-ok-p status type (list p1 p2))
       (fully-visible-p p1)
       (fully-visible-p p2)))

;;;
;;;
;;;

(defoperator create-rubberband ((transparency transparency) (status symbol) (p1 point) (p2 point) &rest initargs)
  (:stored-operator nil)
  (:precondition ((declare (ignore initargs)) (check-line p1 p2 status 'rubberband)))
  (:code ((let ((obj (apply #'make-visco-rubberband transparency status p1 p2 initargs)))
	    (change-status-of-component-objects obj)
	    obj))))


(defoperator create-atomic-rubberband ((transparency transparency) (status symbol) (p1 point) (p2 point) &rest initargs)
  (:stored-operator nil)
  (:precondition ((declare (ignore initargs)) (check-line p1 p2 status 'atomic-rubberband)))
  (:code ((let ((obj (apply #'make-visco-atomic-rubberband transparency status p1 p2 initargs)))
	    (change-status-of-component-objects obj)
	    obj))))


(defoperator create-atomic-<=-rubberband ((transparency transparency) (status symbol) (p1 point) (p2 point) &rest initargs)
  (:stored-operator nil)
  (:precondition ((declare (ignore initargs)) (check-line p1 p2 status 'atomic-<=-rubberband)))
  (:code ((let ((obj (apply #'make-visco-atomic-<=-rubberband transparency status p1 p2 initargs)))
	    (change-status-of-component-objects obj)
	    obj))))


(defoperator create-atomic->=-rubberband ((transparency transparency) (status symbol) (p1 point) (p2 point) &rest initargs)
  (:stored-operator nil)
  (:precondition ((declare (ignore initargs)) (check-line p1 p2 status 'atomic->=-rubberband)))
  (:code ((let ((obj (apply #'make-visco-atomic->=-rubberband transparency status p1 p2 initargs)))
	    (change-status-of-component-objects obj)
	    obj))))

(defoperator create-beam ((transparency transparency) (status symbol) (p1 point) (p2 point) &rest initargs)
  (:stored-operator nil)
  (:precondition ((declare (ignore initargs)) (check-line p1 p2 status 'beam)))
  (:code ((let ((obj (apply #'make-visco-beam transparency status p1 p2 initargs)))
	    (change-status-of-component-objects obj)
	    obj))))

;;;
;;;
;;;

(defmethod check-chain-or-polygon ((transparency transparency) (status symbol) (segments list) (type symbol))
  (and (apply #'same-transparency-p segments)
       (every #'(lambda (segment)
		  (typep segment 'line))
	      segments)
       (segment-list-ok-p segments type nil)
       (let ((master (apply #'get-already-present-direct-master segments)))
	 (=> master
	     (not (and (= (length (segments master))
			  (length segments))
		       (every #'(lambda (i)
				  (= 1 
				     (count-if #'(lambda (j)
						   (equal-p i j))
					       (segments master))))
			      segments)))))
       (status-of-chain-or-polygon-object-ok-p status type segments)
       (every #'fully-visible-p segments)))

(defoperator create-chain ((transparency transparency) (status symbol) (segments list) &rest initargs)
  (:stored-operator nil)
  (:precondition ((declare (ignore initargs)) (check-chain-or-polygon transparency status segments 'chain)))
  (:code ((let ((obj (apply #'make-visco-chain transparency status segments initargs)))
	    (change-status-of-component-objects obj)
	    obj))))

(defoperator create-polygon ((transparency transparency) (status symbol) (segments list) &rest initargs)
  (:stored-operator nil)
  (:precondition ((declare (ignore initargs)) (check-chain-or-polygon transparency status segments 'polygon)))
  (:code ((let ((obj (apply #'make-visco-polygon transparency status segments initargs)))
	    (change-status-of-component-objects obj)
	    obj))))

;;; 
;;; durch Malen eines Polygones:
;;;

(defoperator create-drawn-enclosure ((transparency transparency) (segments list) opaque-p &rest initargs)
  (:stored-operator nil)
  (:precondition ((declare (ignore initargs opaque-p))
		  (and (every #'(lambda (segment)
				  (and (typep segment 'geom-line)
				       (inside-p segment transparency)
				       (not (typep segment 'line)))) ; ausschliesslich geom-lines o.ae.!
			      segments)
		       (segment-list-ok-p segments 'polygon nil))))
  (:code ((apply #'make-visco-drawn-enclosure transparency segments opaque-p initargs))))

	;;;
	;;; Abgeleitete Objekte => Konstruktor-Operatoren
	;;;


(defoperator create-inner-enclosure ((obj polygon) opaque-p &rest initargs)
  (:stored-operator t :after-code (initialize result))
  (:precondition ((declare (ignore initargs opaque-p)) t))
  (:code ((values 
	   (apply #'make-visco-inner-enclosure obj opaque-p :dont-initialize t initargs)
	   (list obj)))))


(defoperator create-outer-enclosure ((obj polygon) opaque-p &rest initargs)
  (:stored-operator t :after-code (initialize result))
  (:precondition ((declare (ignore initargs opaque-p)) t))
  (:code ((values 
	   (apply #'make-visco-outer-enclosure obj opaque-p :dont-initialize t initargs)
	   (list obj)))))

;;;
;;; 
;;;


(defoperator create-epsilon-enclosure ((obj query-object) (radius number) opaque-p &rest initargs)
  (:stored-operator t :additional-slots (radius) :after-code (initialize result))
  (:precondition ((declare (ignore initargs opaque-p))
		  (< radius (distance-between obj (on-transparency obj)))))
  (:code ((values
	   (apply #'make-visco-epsilon-enclosure obj radius opaque-p :dont-initialize t initargs)
	   (list obj)
	   (list :radius radius)))))

(defoperator create-epsilon-p-enclosure ((obj polygon) (radius number) opaque-p &rest initargs)
  (:stored-operator t :additional-slots (radius) :after-code (initialize result))
  (:precondition ((declare (ignore initargs opaque-p))
		  (< radius (distance-between obj (on-transparency obj)))))
  (:code ((values
	   (apply #'make-visco-epsilon-p-enclosure obj radius opaque-p :dont-initialize t initargs)
	   (list obj)
	   (list :radius radius)))))

(defoperator create-epsilon-m-enclosure ((obj polygon) (radius number) opaque-p &rest initargs)
  (:stored-operator t :additional-slots (radius) :after-code (initialize result))
  (:precondition ((declare (ignore initargs opaque-p))
		  (< radius (distance-between obj (on-transparency obj)))))
  (:code ((values
	   (apply #'make-visco-epsilon-m-enclosure obj radius opaque-p :dont-initialize t initargs)
	   (list obj)
	   (list :radius radius)))))
	
;;;
;;;
;;;

(defoperator create-centroid ((obj at-least-1d-query-object) (status symbol) (class-instance point) &rest initargs)
  (:stored-operator t)
  (:precondition ((declare (ignore initargs))
		  (let* ((centroid (centroid obj))
			 (pp 
			  (get-present-point-at (query obj) 
						(x centroid) (y centroid))))
		    (and (not (some #'(lambda (op)
					(typep op 'create-centroid))
				    (arg-of-operators obj)))
			 (if pp  
			     (typep pp (type-of class-instance))
			     (etypecase class-instance
			       (marble (create-marble-applicable-p
					(on-transparency obj) status (x centroid) (y centroid) t))
			       (origin (create-origin-applicable-p
					(on-transparency obj) status (x centroid) (y centroid) t))
                               (nail (create-nail-applicable-p
				      (on-transparency obj) status (x centroid) (y centroid) t))))))))
  (:code ((let* ((centroid (centroid obj))
		 (pp
		  (get-present-point-at (query obj) 
					(x centroid) (y centroid))))
	    (values
	     (if pp 
		 (progn
		   (initialize pp)
		   pp)
		 (apply #'change-class centroid
			(type-of class-instance)
			:on-transparency (on-transparency obj)
			:query (query obj)
			:status status
			initargs))
	     (list obj))))))

(defoperator create-intersection-point ((obj1 line) (obj2 line) (status symbol) (class-instance point) &rest initargs)
  (:stored-operator t)
  (:precondition ((declare (ignore initargs))
		  (and (same-transparency-p obj1 obj2)
		       (not (eq obj1 obj2))
		       #| (find-constraint 'intersects obj1 obj2) |#
		       (crosses-p obj1 obj2)
		       (not (ignore-disjoint-and-intersects-relations-p obj1))
		       (not (ignore-disjoint-and-intersects-relations-p obj2))
		       (crosses-p obj1 obj2)
	
		       (not (some #'(lambda (op) ; Schnittpkt. existiert noch nicht
				      (typep op 'create-intersection-point))
				  (intersection ; gem. Ops.
				   (arg-of-operators obj1)
				   (arg-of-operators obj2))))
	
		       (multiple-value-bind (ix iy)
			   (calculate-intersection-point obj1 obj2)
			 (let ((pp (get-present-point-at (query obj1)
							 ix iy)))
			   (if pp 
			       (typep pp (type-of class-instance))
			       (etypecase class-instance
				 (marble (create-marble-applicable-p (on-transparency obj1) status ix iy t))
				 (origin (create-origin-applicable-p (on-transparency obj1) status ix iy t))
				 (nail (create-nail-applicable-p (on-transparency obj1) status ix iy t)))))))))
  (:code ((multiple-value-bind (ix iy)
	                       (calculate-intersection-point obj1 obj2)	   
	    (let ((pp (get-present-point-at (query obj1)
					    ix iy)))
	      (values
	       (if pp
		   (progn
		     (initialize pp)
		     pp)
		   (apply (etypecase class-instance
			    (marble #'apply-create-marble)
			    (origin #'apply-create-origin)
			    (nail #'apply-create-nail))
			  (on-transparency obj1)
			  status
			  ix iy t initargs))
	       (list obj1 obj2)))))))

;;;
;;; Thematik / Semantik (DL-Konzepte o.”.):
;;;

(defoperator set-semantics ((obj query-object) semantics)
  (:stored-operator nil)
  (:precondition ((and (matches-with-database-object-p obj)
		       semantics)))
  (:code ((progn 
	    (setf (semantics obj) (if (consp semantics)
				      semantics
				      (list semantics)))
	    obj))))

(defoperator delete-semantics ((obj query-object))
  (:stored-operator nil)
  (:precondition ((semantics obj)))
  (:code ((progn (setf (semantics obj) nil)
		 obj))))

;;;
;;;
;;;

#|					;

(defmethod get-all-visible-containing-enclosures ((obj query-object))
  (loop as obj2 in (get-all-query-objects-and-enclosures obj)
	when (and (typep obj2 'enclosure)
		  (fully-visible-p obj))
	collect obj2))

(defmethod check-at-most-constraint ((obj enclosure) (at-most integer))
  (<= (count-if #'(lambda (cs)
		    (and (typep cs 'contains)					      
			 (matches-with-database-object-p (2nd-arg cs))
			 (primary-p (2nd-arg cs))))
		(constraints obj))
      at-most))

(defmethod check-all-enclosure-at-most-constraints ((obj query-object))
  (every #'(lambda (enclosure)
	     (=> (at-most-constraint enclosure)
		 (check-at-most-constraint enclosure (at-most-constraint enclosure))))
	 (get-all-visible-containing-enclosures obj)))

|#

;;;
;;;
;;;


(defoperator set-at-most-constraint ((obj rubberband) (at-most integer))
  (:stored-operator nil)
  (:precondition ((> at-most 0)))
  (:code ((progn (setf (at-most-constraint obj) at-most)
		 (dolist (master (part-of obj))
		   (when (every #'(lambda (s)
				    (or (typep s 'atomic-rubberband)
					(at-most-constraint s)))
				(segments master))
		     (setf (at-most-constraint master)
			   (loop as s in (segments master)
				 sum (if (typep s 'atomic-rubberband)
					 1
					 (at-most-constraint s))))))
		 obj))))

(defoperator set-at-most-constraint ((obj chain-or-polygon) (at-most integer))
  (:stored-operator nil)
  (:precondition ((and (<= (length (segments obj)) at-most)
		       (some #'(lambda (segment)
				 (and (typep segment 'rubberband)
				      (not (typep segment 'atomic-rubberband))))
			     (segments obj)))))
  (:code ((progn (setf (at-most-constraint obj) 
		       (if (at-most-constraint obj)
			   (min at-most (at-most-constraint obj))
			   at-most))
		 (let ((n (length (segments obj))))
		   (dolist (s (segments obj))
		     (when (typep s 'rubberband)
		       (setf (at-most-constraint s) 
			     (if (at-most-constraint s)
				 (min (at-most-constraint s)
				      (1+ (- at-most n)))
				 (1+ (- at-most n)))))))
		 obj))))

#|					;

;;;
;;; noch nicht implementiert => damit "at-most" Subsumption stimmt, muss inside(enclosure1,enclosure2)
;;; auch zwischen epsilon-enclosures berechnet werden!
;;;

(defoperator set-at-most-constraint ((obj enclosure) (at-most integer))
  (:stored-operator nil)
  (:precondition (check-at-most-constraint obj at-most))
  (:code (progn (setf (at-most-constraint obj) at-most)
		obj)))
|#

(defoperator delete-at-most-constraint ((obj at-most-constraint-mixin))
  (:stored-operator nil)
  (:precondition ((at-most-constraint obj)))
  (:code ((progn (setf (at-most-constraint obj) nil)
		 obj))))


;;;
;;;
;;;

(defun check-orientation-constraint-p (orientation-constraint) ; (a b) = von a nach b a.d. Kreis gegen die Uhr!
  (every #'(lambda (entry)
	     (or (and (numberp entry)
		      (<= 0 entry +2pi+))
		 (and (consp entry)
		      (numberp (first entry))			
		      (numberp (second entry))
		      (<= 0 (first entry) +2pi+)
		      (<= 0 (second entry) +2pi+))))
	 orientation-constraint))

(defun normalize-orientation-constraint (orientation-constraint)
  (labels ((do-it (orientation-constraint)
	     (let ((noc nil)
		   (found-one nil))
	       (dolist (i orientation-constraint)
		 (let ((intersecting-intervalls
			(loop as j in noc	  
			      when (circle-intervall-intersects-p 
				    (first i) (second i)
				    (first j) (second j))
			      collect j)))
		   (if (not intersecting-intervalls)
		       (push i noc)
		       (let ((int-min (first i))
			     (int-max (second i))
			     (intersecting-intervalls (cons i intersecting-intervalls)))
			 (dolist (j intersecting-intervalls)
			   (let ((j-min (first j))
				 (j-max (second j)))
			     (let* ((l
				     (circle-intervall-length int-min int-max))
				    (l1
				     (circle-intervall-length j-min j-max))
				    (l2
				     (circle-intervall-length int-min j-max))
				    (l3
				     (circle-intervall-length j-min int-max))
				    (max (max l l1 l2 l3)))
			       (cond ((= max l1)
				      (setf found-one t)
				      (setf int-min j-min
					    int-max j-max))
				     ((= max l2)
				      (setf found-one t)
				      (setf int-max j-max))
				     ((= max l3)
				      (setf found-one t)
				      (setf int-min j-min))))))
			 (dolist (j intersecting-intervalls)
			   (setf noc (delete j noc)))
			 (push (list int-min int-max) noc)))))	       
	       (values noc found-one))))
    (let* ((cs
	    (remove-if #'(lambda (entry)
			   (and (not (consp entry))
				(some #'(lambda (intervall)
					  (and (consp intervall)					      
					       (lies-in-circle-intervall-p entry 
									   (first intervall)
									   (second intervall))))
				      orientation-constraint)))
		       orientation-constraint))
	   (numbers (remove-if-not #'numberp cs))
	   (cs2 (remove-if #'numberp cs)))
      (loop
       (multiple-value-bind (res flag)
	   (do-it cs2)
	 (setf cs2 res)
	 (unless flag
	   (return (append numbers res))))))))


(defoperator set-orientation-constraint ((obj orientation-constraint-mixin) (orientation-constraint cons))
  (:stored-operator nil)
  (:precondition ((and (not (typep obj 'transparency))
		       (check-orientation-constraint-p orientation-constraint)
		       (=> (typep obj 'chain-or-polygon)
			   (some #'(lambda (segment)
				     (typep segment 'atomic-rubberband))
				 (segments obj))))))
  (:code ((progn (setf (orientation-constraint obj) orientation-constraint)
		 obj))))

#|					;
(normalize-orientation-constraint 
 (mapcan #'(lambda (o)
	     (if (consp o)
		 (let* ((a (normalize (first o)))
			(b (normalize (second o)))
			(c (normalize (+ pi a)))
			(d (normalize (+ pi b))))
		   (unless (circle-intervall-intersects-p a b c d)
		     (list o (list c d ))))
		 (list o (normalize (+ pi o)))))
	 orientation-constraint))
|#

(defoperator delete-orientation-constraint ((obj orientation-constraint-mixin))
  (:stored-operator nil)
  (:precondition ((orientation-constraint obj)))
  (:code ((progn (setf (orientation-constraint obj) nil)
		 obj))))


;;;
;;;
;;;

(defoperator set-relative-orientation-constraint ((obj1 atomic-rubberband) 
						  (obj2 atomic-rubberband)
						  (allowed-derivation number))
  (:stored-operator nil)
  (:precondition ((and (not (eq obj1 obj2))
		       (not (or (ignore-disjoint-and-intersects-relations-p obj1)
				(ignore-disjoint-and-intersects-relations-p obj2)))
		       (intersects-p obj1 obj2)
		       (not		; existiert noch nicht
			(some #'(lambda (constraint)
				  (and (typep constraint 'angle-between)
				       (eq (2nd-arg constraint) obj2)))
			      (constraints obj1))))))
  (:code ((make-and-memoize-binary-constraint 'angle-between obj1 obj2 :allowed-derivation allowed-derivation))))

#|					;

(defoperator create-relative-orientation-constraint ((obj point)
						     (orientation-constraint list))
  (:stored-operator nil)
  (:precondition (and (check-orientation-constraint-p orientation-constraint)
		      (>= (length orientation-constraint) 2)
		      (every #'(lambda (entry)
				 (some #'(lambda (segment)
					   (and (typep segment 'atomic-rubberband)
						(multiple-value-bind (r alpha)
						    (if (eq (p1 segment) obj)
							(distance-and-orientation* 
							 (x obj) (y obj)
							 (x (p2 segment)) (y (p2 segment)))
							(distance-and-orientation* 
							 (x obj) (y obj)
							 (x (p1 segment)) (y (p1 segment))))
						  (if (consp entry)
						      (inside-circle-intervall-p alpha entry)
						      (=-eps entry alpha)))))
				       (part-of obj)))
			     orientation-constraint)))
  (:code (progn 
	   (dolist (s1 (part-of obj))
	     (dolist (s2 (part-of obj))
	       (when (and (typep s1 'atomic-rubberband)
			  (typep s2 'atomic-rubberband)
			  (not (eq s1 s2)))
		 (labels ((find-all-containing-intervalls (constraint alpha)
			    (loop as cs in constraint when
				  (or (and (consp cs)
					   (inside-circle-intervall-p alpha cs))
				      (=-eps cs alpha))
				  collect cs)))
		   (let* ((i1
			   (sort #'<
				 (mapcar #'(lambda (cs)
					     (if (consp cs)
						 (angle-difference (first cs) (second cs))
						 0))
					 (find-all-containing-intervalls s1))))
			  (i2
			   (sort #'<
				 (mapcar #'(lambda (cs)
					     (if (consp cs)
						 (angle-difference (first cs) (second cs))
						 0))
					 (find-all-containing-intervalls s2))))
			  
			  (min (min (angle-difference (first i1) (first i2))
				    (angle-difference (first i1) (second i2))
				    (angle-difference (second i1) (first i2))
				    (angle-difference (second i1) (second i2))))
			  (max (max (angle-difference (first i1) (first i2))
				    (angle-difference (first i1) (second i2))
				    (angle-difference (second i1) (first i2))
				    (angle-difference (second i1) (second i2)))))
		     (make-and-memoize-binary-constraint 'angle-between
							 s1 s2
							 :ticks 
							 (list min max)))))))
	   obj)))


|#

(defoperator delete-relative-orientation-constraint ((obj1 atomic-rubberband)
						     (obj2 atomic-rubberband))
  (:stored-operator nil)
  (:precondition ((and
		   (some #'(lambda (constraint)
			     (eq (2nd-arg constraint) obj2))
			 (constraints obj1))
		   (some #'(lambda (constraint)
			     (eq (2nd-arg constraint) obj1))
			 (constraints obj2)))))
  (:code ((progn (find-and-delete-constraint 'angle-between obj1 obj2)
		 (list obj1 obj2)))))

;;;
;;;
;;;

(defoperator set-transparency-properties ((obj transparency) minw maxw minh maxh factor)
  (:stored-operator nil)
  (:precondition ((and (=> minw (plusp minw))
		       (=> minh (plusp minh))
		       (=> maxw (plusp maxw))
		       (=> maxh (plusp maxh))
		       (=> (and minw maxw)
			   (<= minw maxw))
		       (=> (and minh maxh)
			   (<= minh maxh))
		       (=> factor (plusp factor)))))
  (:code ((let* ((minw (if (and minh factor)
			   (if minw 
			       (min (/ minh factor) minw)
			       (/ minh factor))
			   minw))
		 (minh (if (and minw factor)
			   (if minh
			       (min (/ minw factor) minh)
			       (/ minw factor))
			   minh))
		 
		 (maxw (if (and maxh factor)
			   (if maxw 
			       (max (/ maxh factor) maxw)
			       (/ maxh factor))
			   maxw))
		 (maxh (if (and maxw factor)
			   (if maxh
			       (max (/ maxw factor) maxh)
			       (/ maxw factor))
			   maxh))
		 
		 (w (cond ((and maxw minw)
			   (/ (+ minw maxw) 2))
			  (t (or minw maxw))))
		 (h (cond ((and maxh minh)
			   (/ (+ minh maxh) 2))
			  (t (or minh maxh))))
		 
		 (sxmin (and w minw (/ minw w)))
		 (symin (and h minh (/ minh h)))
		 (sxmax (and w maxw (/ maxw w)))
		 (symax (and h maxh (/ maxh h))))
	    
	    (setf (width obj) w
		  (height obj) h
		  (sxmin obj) sxmin
		  (symin obj) symin
		  (sxmax obj) sxmax
		  (symax obj) symax
		  (sx-s->w obj) (if w 
				    (/ w (- (x (pmax obj))
					    (x (pmin obj))))
				    1)
		  (sy-s->w obj) (if h 
				    (/ h (- (y (pmax obj))
					    (y (pmin obj))))
				    1)
		  (sxsy-constraint obj) 
		  (when (or (sx-type obj)
			    (sy-type obj))
		    factor))
	    
	    obj))))
