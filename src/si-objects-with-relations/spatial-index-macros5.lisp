;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: SPATIAL-INDEX; Base: 10 -*-

(in-package spatial-index)

(defconstant +no-of-levels+ 20)

(defparameter *cur-level* -1)

(defparameter *level-modes* (make-array +no-of-levels+ :initial-element nil))

(defparameter *level-objects* (make-array +no-of-levels+ :initial-element nil))

;;;
;;;
;;;

(defmacro with-new-level ((level) &body body)
  `(unwind-protect
       (progn
	 (when (= *cur-level* (1- +no-of-levels+))
	   (error "No more levels!"))
	 (incf *cur-level*)
	 (prog1
	     (let ((,level *cur-level*))
	       (progn ,@body))))
     (decf *cur-level*)))

(defmacro with-selected-buckets ((bucket-bind-to obj mode &rest args) &body body)
  (let ((obj-var (gensym))
	(mode-var (gensym))
	(result-var (gensym)))
    `(let ((,obj-var ,obj)
	   (,mode-var ,mode)
	   (,result-var nil))
       (multiple-value-bind (ixmin iymin ixmax iymax)
	   (get-range-for-object ,obj-var ,mode-var ,@args)
	 ,(let ((x-var (gensym))
		(y-var (gensym)))
	    `(loop for ,x-var from ixmin to ixmax do
		   (loop for ,y-var from iymin to iymax do 
			 (let* ((,bucket-bind-to 
				 (get-current-bucket ,x-var ,y-var)))
			   (when (bucket-selected-p ,obj-var ,bucket-bind-to 
						    ,mode-var ,@args) ; relevanter Bucket ?
			     (setf ,result-var (progn ,@body))))
		       finally (return ,result-var))
		 finally (return ,result-var)))))))


(defmacro with-selected-objects ((obj-bind-to obj add-test
				  mode &rest args) &body body)
  (let ((obj-var (gensym))
	(mode-var (gensym))
	(result-var (gensym)))
    `(let ((,obj-var ,obj)
	   (,mode-var ,mode)
	   (,result-var nil))
       (with-new-level (level)
	 (unwind-protect
	     (with-selected-buckets (cur-bucket ,obj-var ,mode-var ,@args)
	       (dolist (,obj-bind-to (elements cur-bucket) ,result-var)
		 (let ((tested? 
			(get-value-for ,obj-bind-to level))) ; Yes, No, NIL
		   (when (and (not tested?)
			      ,add-test)
		     (if (candidate-selected-p ,obj-var ,obj-bind-to ,mode-var ,@args)
			 (setf ,result-var
			   (progn
			     (set-value-for ,obj-bind-to level 'yes)
			     ,@body))
		       (set-value-for ,obj-bind-to level 'no))))))
	   
	   (with-selected-buckets (cur-bucket ,obj-var ,mode-var ,@args)
	     (dolist (element (elements cur-bucket))
	       (set-value-for element level nil))))))))



