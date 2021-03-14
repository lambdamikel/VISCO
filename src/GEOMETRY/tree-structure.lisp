;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: GEOMETRY; Base: 10 -*-

(in-package geometry)

(defmethod primary-p ((obj geom-thing))
  (null (part-of obj)))

;;;
;;;
;;;

(defmethod component-p ((component geom-thing)
			(master geom-thing))
  nil)

(defmethod component-p ((component geom-point)
			(master geom-line))
  (or (eq (p1 master) component)
      (eq (p2 master) component)))

(defmethod component-p ((component geom-thing)
			(master geom-chain-or-polygon))
  (or (member component (segments master))
      (some #'(lambda (segment) 
		(component-p component segment))
	    (segments master))))

(defmethod component-p ((component geom-thing)
			(master geom-aggregate))
  (or (member component (has-parts master))
      (some #'(lambda (part) 
		(component-p component part))
	    (has-parts master))))

;;;
;;;
;;;


(defun for-each-master-holds-p (obj fn)
  (if obj 
      (every #'(lambda (master)
		 (and (funcall fn master)
		      (for-each-master-holds-p master fn)))
	     (part-of obj))
    t))

(defun for-each-component-holds-p (obj fn)
  (if obj
      (every #'(lambda (comp)
		 (and (funcall fn comp)
		      (for-each-component-holds-p comp fn)))
	     (get-direct-components obj))
    t))

(defun for-some-master-holds-p (obj fn)
  (if obj 
      (some #'(lambda (master)
		(or (funcall fn master)
		    (for-some-master-holds-p master fn)))
	    (part-of obj))
    nil))

(defun for-some-component-holds-p (obj fn)
  (if obj     
      (some #'(lambda (comp)
		(or (funcall fn comp)
		    (for-some-component-holds-p comp fn)))
	    (get-direct-components obj))
    nil))

;;;
;;;
;;;

(defun get-already-present-direct-master (&rest components)
  (let ((i
	 (reduce #'intersection
		 (mapcar #'part-of components))))
    (if (null (rest i))	
	(first i)
      (error 'geom-error :descr "More than one master object!"))))

(defun get-topmost-common-master (&rest components)
  (reduce #'intersection	
	  (mapcar #'get-topmost-master components)))

(defun get-topmost-master (obj)
  (let ((res nil))
    (labels ((do-it (obj)
	     (let ((masters (part-of obj)))
	       (if masters
		   (dolist (master masters)
		     (do-it master))
		 (pushnew obj res)))))
      (do-it obj) 
      res)))

(defun get-direct-common-master (&rest components)
  (reduce #'intersection
	  (mapcar #'part-of components)))

(defun get-direct-common-master-from-list (components)
  (reduce #'intersection
	  (mapcar #'part-of components)))

;;;
;;;
;;;

(defmethod get-direct-components ((obj geom-thing))
  nil)

(defmethod get-direct-components ((obj geom-line))
  (point-list obj))

(defmethod get-direct-components ((obj geom-chain-or-polygon))
  (segments obj))

(defmethod get-direct-components ((obj geom-aggregate))
  (has-parts obj))

;;;
;;;
;;;

(defmethod get-all-masters ((obj geom-thing))
  (let ((res nil))
    (dolist (master (part-of obj))
      (setf res 
	(nconc (cons master (get-all-masters master)) res)))
    res))

(defmethod get-all-components ((obj geom-thing))
  (let ((res nil))
    (dolist (part (get-direct-components obj))
      (setf res 
	(nconc (cons part (get-all-components part)) res)))
    res))

#|
(defmethod get-all-masters ((obj geom-point))
  (cons obj
	(mapcan #'get-all-masters
		(append (centroid-of obj)
			(pcenter-of obj)
			(part-of obj)))))
|#

;;;
;;;
;;;


(defmethod common-root-p ((obj1 geom-thing) (obj2 geom-thing))
  (or (eq obj1 obj2)
      (intersection 
       (get-all-masters obj1)
       (get-all-masters obj2))))

(defun for-each-parent-execute (obj fn)
  (when obj
    (funcall fn obj)
    (dolist (master (part-of obj))
      (for-each-parent-execute master fn))))

#|

(defmethod common-root-p ((obj1 geom-thing) (obj2 geom-thing))
  (labels ((for-each-parent-execute (obj fn)
	     (funcall fn obj)
	     (mapc #'(lambda (slot)
		       (dolist (ma (funcall slot obj))
			 (for-each-parent-execute ma fn)))
		   (typecase obj
		     (geom-point
		      (list #'part-of #'centroid-of #'pcenter-of))
		     (t (list #'part-of))))
	     nil)
	   
	   (set-it (obj value)
	     (for-each-parent-execute obj
				      #'(lambda (obj)
					  (with-slots (cp-flag) obj
					    (setf cp-flag value)))))

	   (test-it (obj value)
	     (for-each-parent-execute obj
				      #'(lambda (obj)
					  (with-slots (cp-flag) obj
					    (when (and (numberp cp-flag)
						       (= cp-flag value))
					      (return-from test-it t)))))))
    
    (let ((value (get-new-mark)))
      (set-it obj1 value)
      (test-it obj2 value))))
|#
