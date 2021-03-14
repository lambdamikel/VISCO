;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: SQD; Base: 10 -*-

(in-package sqd)

(defparameter *saetze* nil)
(defparameter *bloecke* nil)

(defmacro datensatz (fn-name &rest rest)
  (pushnew fn-name *saetze*)
  (let ((variables nil)
	(counter 0))
    
    (labels ((code-block (clauses variables)
	       (if (null clauses)   		   		  
		   t
		 (let ((clause (first clauses))
		       (dont-register-variable nil))                                 
		   (when (null clause)
		     (setf clause `( ( ,(intern (format nil "?ANONYMOUS~A" (incf counter))))))
		     (setf dont-register-variable t))
		   (unless (<= (length clause) 3)
		     (break "Compilation Error: 1!"))
		   (let ((optional nil))
		     
		     (when (eq (first clause) 'optional)                                   
		       (setf clause (rest clause))                   
		       (setf optional t))
		     
		     (unless (<= (length clause) 2)
		       (break "Compilation Error!"))
		     
		     (let* ((length (length clause))
			    (string-pattern 
			     (if (and (= length 1) (listp (first clause)))
				 ""
			       (first clause)))
			    (variable-pattern 
			     (if (and (= length 1) (listp (first clause)))
				 (first clause) (second clause))))
		       
		       (when (or (not (stringp string-pattern)) (null string-pattern))
			 (break "Compilation Error: 2!"))   
		       
		       (unless (listp variable-pattern)
			 (break "Compilation Error: 3!"))
		       
		       (let ((variable (first variable-pattern))
			     (l (length variable-pattern)))
			 
			 (when (not (or (and (= l 1)
					     (= length 1) (listp variable) (not optional))
					; einfache Funktion, keine Variable
					(and (= l 1) (= length 2) (listp variable))
					(and (= l 1) (symbolp variable)) ; einfache variable
					(and (= l 2) (symbolp variable) (listp (second variable-pattern)))
					(= l 0)))
			   (terpri)
			   (princ variable-pattern)
			   (princ variable)
			   (break "Compilation Error: 4!"))
			 
			 (cond ( (and (= l 1) (listp variable))
				 
				 (if (= length 1)          
				     (let ((dummy-var (gensym)))
				       `(progn
					  (let ((,dummy-var line))
					    (declare (ignore ,dummy-var))
					    ,variable
					    ,(code-block (rest clauses) variables))))
				   (if (= length 2)                                
				       (if optional                                             
					   `(let ((lastline line)
						  (line (consume line ,string-pattern)))
					      (if (eq line 'error) 
						  (let ((line lastline))
						    ,(code-block (rest clauses) variables))
						(progn
						  ,variable 
						  ,(code-block (rest clauses) variables))))
					 `(let ((line (consume line ,string-pattern)))
					    (when (eq line 'error) (return-from ,fn-name nil))
					    ,variable
					    ,(code-block (rest clauses) variables))))))
			       
			       (t
				(when (and variable (not dont-register-variable))
				  (when (member variable variables)
				    (break "Error! Double Variable!"))
				  (push variable variables))
				
				(let ((function                                                     
				       (or
					(second variable-pattern)
					t)))                
				  (if optional
				      `(let ((lastline line)
					     (line (consume line ,string-pattern)))
					 (if (eq line 'error) 
					     (let ((line lastline))
					       ,(code-block (rest clauses) (rest variables)))
					   ,@(if variable-pattern
						 `((multiple-value-bind (line ,variable)
						       (assign-variable-value line)
						     (when (eq ,variable 'error) (return-from ,fn-name nil))
						     (unless ,function
						       (return-from ,fn-name nil))
						     ,(code-block (rest clauses) variables)))
					       `(,(code-block (rest clauses) variables)))))
				    `(let ((line (consume line ,string-pattern)))
				       (when (eq line 'error) (return-from ,fn-name nil))
				       ,@(if variable-pattern
					     `((multiple-value-bind (line ,variable)
						   (assign-variable-value line)
						 (when (eq ,variable 'error) (return-from ,fn-name nil))
						 (unless ,function
						   (return-from ,fn-name nil))
						 ,(code-block (rest clauses) variables)))
					   `(,(code-block (rest clauses) variables)))))))))))))))
      `(defun ,fn-name (line)
	 ,(code-block rest variables)))))

(defun consume (line string-pattern)
  (let ((num (position-if-not #'(lambda (i) (char= #\space i)) line)))     
    (unless num
      (return-from consume 'error))
    (let* ((line (subseq line num)))            
      (if (string= string-pattern "")
	  line
	(let* ((end (position-if #'(lambda (i) (char= #\space i)) line))
	       (word (subseq line 0 end))            
	       (search (search string-pattern word)))
	  (if (or (not search) (not (zerop search)))
	      'error
	    (subseq line (length string-pattern))))))))


(defun assign-variable-value (line)
  (let ((num (position-if #'(lambda (i) (char= #\space i)) line)))
    (if (and num (zerop num))
	'error
      (if num          
	  (values (subseq line num) 
		  (let ((word (subseq line 0 num)))
		    (if (string= word ".")
			"" word)))                   
	(values ""
		(if (string= line ".")
		    "" line))))))

(defparameter *line-counter* 0)

(defun get-line (stream)
  (let ((line 
	 (read-line stream nil 'eof)))
    (incf *line-counter*)
    (if (eq line 'eof)
	'eof
      line)))


(defmacro datenblock (fn-name &rest rest)
  (pushnew fn-name *bloecke*)
  (let ((code nil))       

    (dolist (clause (reverse rest))
      (let ((l (length clause))
	    (1st (first clause))               
	    (optional nil)
	    (loop nil))
	
	(when (or (> l 3) (= l 0))
	  (break "Compilation Error: 1!"))
	(when (eq 1st 'optional)
	  (setf clause (rest clause))
	  (setf optional t))
	(when (eq 1st 'loop)
	  (setf clause (rest clause))
	  (setf loop t))
	
	(let ((1st (first clause))
	      (2nd (second clause)))
	  
	  (cond ( (and (= l 1) (listp 1st))
		  (push
		   1st                                            
		   code))
		(t
		 (if (or (not (symbolp 1st)) (not (symbol-function 1st)))
		     (break "Compilation Error: 2!"))                 
		 (unless (listp 2nd)
		   (break "Compilation Error: 3!"))
		 
		 (if  optional 
		     (if 2nd                
			 (push                          
			  `(if (,1st line)                         
			       ,2nd
			     (setf dont-read-next-line t))                               
			  code)
		       (push                          
			`(unless (,1st line)                                                  
			   (setf dont-read-next-line t))                               
			code))                                         
		   
		   (if  loop 
		       (if 2nd                
			   (push                          
			    `(loop
			       (if (,1st line)                         
				   ,2nd
				 (progn (setf dont-read-next-line t) (return)))
			       (setf line (get-line stream)))
			    code)
			 (push                          
			  `(loop
			     (unless (,1st line)                                                  
			       (setf dont-read-next-line t)
			       (return))
			     (setf line (get-line stream)))                             
			  code))
		     
		     (if 2nd
			 (push
			  `(if (,1st line)
			       ,2nd
			     (return-from ,fn-name 'error))                                             
			  code)
		       (push   
			`(unless (,1st line)
			   (return-from ,fn-name 'error))                              
			code))))
		 
		 (push '(setf dont-read-next-line nil) code)
		 (push 
		  '(unless dont-read-next-line (setf line (get-line stream)))
		  code))))))

    `(defun ,fn-name (stream line)
       (let (
	     (dont-read-next-line t))
	 (progn
	   ,@ code
	   t)))))


