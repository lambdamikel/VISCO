;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: QUERY-COMPILER; Base: 10 -*-

(in-package query-compiler)

;;;
;;;
;;;

(defmacro with-binding ((qo dbo) &body body)
  (let ((qo-gensym (gensym))		; evaluate a and b only once!
	(db-gensym (gensym)))
    `(let* ((,qo-gensym ,qo)
	    (,db-gensym ,dbo))
       (when ,db-gensym
	 (unless (bound-to ,db-gensym)
	   (setf (bound-to ,qo-gensym) ,db-gensym)
	   (setf (bound-to ,db-gensym) ,qo-gensym)
	   (unwind-protect
	       (progn
		 ,@body)
	     (setf (bound-to ,db-gensym) nil)))))))

;;;
;;;
;;;

(defun get-code-for-class-and-constructor (name superclasses
					   all-slots additional-slots)
  (let ((class-name (intern (format nil "CC-~A" name)))
	(constructor-name (intern (format nil "MAKE-~A" name))))
    
    `(
      (defpersistentclass ,class-name ,superclasses
	,(mapcar #'(lambda (name)
		     (list name :accessor name
			   :initarg
			   (intern (format nil "~A" name)
				   (find-package 'keyword))))
		 additional-slots))
	 
	 (defun ,constructor-name ( ,@all-slots &rest initargs)
	   (apply #'make-instance ',class-name 
		  ,@(mapcan #'(lambda (slot)
				(list (intern (format nil "~A" slot)
					      (find-package 'keyword))
				      slot))						    
			    all-slots)
		  initargs)))))

(defun get-code-for-method-definitions (class-name all-slots method-definitions)
  (let ((class-name (intern (format nil "CC-~A" class-name))))
    (remove nil
	    (mapcan #'(lambda (line)	
			(let* ((name (first line))
			       (cond (second line))
			       (pos (position :not-before (third line)))
			       (code (if pos (subseq (third line) 0 pos)
				       (third line)))
			       (precond (when pos (subseq (third line) (1+ pos)))))
			  (when code		
			    (cons
			     `(defmethod ,(intern (format nil "GET-~A-CODE" name))
				  ((self ,class-name) continuation &key (check t))
				,@(unless cond 
				    `((declare (ignore check))))
				(let ((continuation (or continuation
							#'(lambda () nil))))			       
				  (with-slots (,@all-slots) self
				    ,@(if cond
					  `((when check 
					      (unless 
						  (and 
						   ,(if (first cond) 
							`(matches-with-database-object-p candidate)
						      `(not (matches-with-database-object-p candidate)))
						   ,@(when (= (length cond) 2)
						       (if (second cond)
							   `((matches-with-database-object-p present))
							 `((not (matches-with-database-object-p present))))))
						(error "!!!")))
					    ,@code)
					`(,@code)))))
			     (mapcar #'(lambda (other-class)
					 `(defmethod may-appear-before-p
					      ((self ,class-name)
					       (other ,(intern 
							(format nil "CC-~A" other-class))))
					    nil))
				     precond)))))
		    method-definitions))))


(defmacro defdependency (name
			 &body body)
  (let ((body (remove nil body)))
    (unless      
	(every #'(lambda (entry)
		   (member (first entry)
			   '(:additional-slots
			     
			     :logic 
			     
			     :generator
			     :tester
			     
			     :generator-T
			     :generator-NIL
			     
			     :generator-T->T
			     :generator-T->NIL
			     :generator-NIL->T
			     :generator-NIL->NIL
			     
			     :tester-T
			     :tester-NIL
			     
			     :tester-T->T
			     :tester-T->NIL
			     :tester-NIL->T
			     :tester-NIL->NIL)))
	       body)
      (error "Syntax Error!"))
    
    (let ((additional-slots (rest (assoc :additional-slots body)))
	  (logic (rest (assoc :logic body)))
	  (generator (rest (assoc :generator body)))

	  (generator-T (rest (assoc :generator-T body)))
	  (generator-NIL (rest (assoc :generator-NIL body)))
	  
	  (generator-T->T (rest (assoc :generator-T->T body)))
	  (generator-T->NIL (rest (assoc :generator-T->NIL body)))
	  (generator-NIL->T (rest (assoc :generator-NIL->T body)))
	  (generator-NIL->NIL (rest (assoc :generator-NIL->NIL body)))

	  (tester (rest (assoc :tester body)))

	  (tester-T (rest (assoc :tester-T body)))
	  (tester-NIL (rest (assoc :tester-NIL body)))

	  (tester-T->T (rest (assoc :tester-T->T body)))
	  (tester-T->NIL (rest (assoc :tester-T->NIL body)))
	  (tester-NIL->T (rest (assoc :tester-NIL->T body)))
	  (tester-NIL->NIL (rest (assoc :tester-NIL->NIL body))))
      
      (let ((all-slots (append '(candidate present) additional-slots)))
	
	`(progn 
	   ,@(get-code-for-class-and-constructor name '(dependency)
						 all-slots additional-slots)	   
	   ,@(get-code-for-method-definitions name all-slots 
					      
					      (list (list 'logic nil logic)
						    (list 'generator nil generator)
						    
						    (list 'generator-T '(t) generator-t)
						    (list 'generator-NIL '(nil) generator-nil)
						    (list 'tester-T '(t) tester-t)
						    (list 'tester-NIL '(nil) tester-nil)
						    
						    (list 'generator-T->T '(t t) generator-t->t)
						    (list 'generator-T->NIL '(t nil) generator-t->nil)
						    (list 'generator-NIL->T '(nil t) generator-nil->t)
						    (list 'generator-NIL->NIL '(nil nil) generator-nil->nil)
						    
						    (list 'tester nil tester)
						    (list 'tester-T->T '(t t) tester-t->t)
						    (list 'tester-T->NIL '(t nil) tester-t->nil)
						    (list 'tester-NIL->T '(nil t) tester-nil->t)
						    (list 'tester-NIL->NIL '(nil nil) tester-nil->nil))))))))



(defmacro defmultidependency (name
			      &body body)
  (let ((body (remove nil body)))  
    (unless
	(every #'(lambda (entry)
		   (member (first entry)
			   '(:additional-slots
			     
			     :logic
			     
			     :generator
			     :tester

			     :generator-T
			     :generator-NIL
			     
			     :tester-T
			     :tester-NIL)))
	       
	       body)
      (error "Syntax Error!"))
    
    (let ((additional-slots (rest (assoc :additional-slots body)))
	  
	  (logic (rest (assoc :logic body)))
	  
	  (generator-T (rest (assoc :generator-T body)))
	  (generator-NIL (rest (assoc :generator-NIL body)))
	  
	  (tester (rest (assoc :tester body)))
	  (generator (rest (assoc :generator body)))
	  
	  (tester-T (rest (assoc :tester-T body)))
	  (tester-NIL (rest (assoc :tester-NIL body))))
      
      (let ((all-slots (append '(candidate args) additional-slots)))	
	`(progn 
	   ,@(get-code-for-class-and-constructor name '(multidependency)
						 all-slots additional-slots)	   
	   ,@(get-code-for-method-definitions name all-slots 
					      (list (list 'logic nil logic)
						    (list 'generator-T '(t) generator-t)
						    (list 'generator nil generator)
						    (list 'tester nil tester)
						    (list 'generator-NIL '(nil) generator-nil)
						    (list 'tester-T '(t) tester-t)
						    (list 'tester-NIL '(nil) tester-nil))))))))



(defmacro defproperty (name
		       &body body)
  (let ((body (remove nil body)))
    (unless
	(every #'(lambda (entry)
		   (member (first entry)
			   '(:additional-slots
			     
			     :logic
			     
			     :generator
			     :tester
			     
			     :generator-T
			     :generator-NIL
			     
			     :tester-T
			     :tester-NIL)))
	       
	       body)
      (error "Syntax Error!"))
    
    (let ((additional-slots (rest (assoc :additional-slots body)))
	  
	  (logic (rest (assoc :logic body)))
	  
	  (generator-T (rest (assoc :generator-T body)))
	  (generator-NIL (rest (assoc :generator-NIL body)))	  
	  
	  (tester (rest (assoc :tester body)))
	  (generator (rest (assoc :generator body)))
	  
	  (tester-T (rest (assoc :tester-T body)))
	  (tester-NIL (rest (assoc :tester-NIL body))))
      
      (let ((all-slots (append '(candidate) additional-slots)))	
	`(progn 
	   ,@(get-code-for-class-and-constructor name '(property)
						 all-slots additional-slots)	   
	   ,@(get-code-for-method-definitions name all-slots 
					      (list (list 'logic nil logic)
						    (list 'generator-T '(t) generator-t)
						    (list 'generator nil generator)
						    (list 'tester nil tester)
						    (list 'generator-NIL '(nil) generator-nil)
						    (list 'tester-T '(t) tester-t)
						    (list 'tester-NIL '(nil) tester-nil))))))))

