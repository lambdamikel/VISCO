;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: QUERY-COMPILER; Base: 10 -*-

(in-package query-compiler)

;;;
;;; Status
;;;

(defun status-of-chain-or-polygon-object-ok-p (status type components)
  (declare (ignore type))
  (case status 
    (universe (every #'instantiable-object-p components))
    ((db db-component)
     (every #'(lambda (component)
		(and (member (status component) '(db db-component))
		     (=> (eq (status component) 'db) 
			 (change-status-applicable-p component 'db-component))))
	    components))
    (otherwise nil)))

(defun status-of-line-object-ok-p (status type components)
  (case status 
    (universe (and (every #'instantiable-object-p components)
		   (=> (eq type 'rubberband)
		       (every #'(lambda (component)
				  (eq (status component) 'db-component))
			      components))))
    ((db db-component)
     (every #'(lambda (component)
		(and (member (status component) '(db db-component))
		     (=> (eq (status component) 'db) 
			 (change-status-applicable-p component 'db-component))))
	    components))     
    (otherwise nil)))

(defun status-of-point-object-ok-p (status type res-of-operator)
  (if (eq status 'universe)
      (case type
	((origin nail) t)
	(marble res-of-operator))
    (member status '(db db-component))))

;;;
;;;
;;;

(defmethod change-status-of-component-objects ((obj at-least-1d-query-object))
  (when (member (status obj) '(db db-component))
    (dolist (component (get-direct-components obj))
      (set-status component 'db-component))
    obj))

;;;
;;;
;;;

(defmethod instantiable-object-p ((obj marble))
  (=> (eq (status obj) 'universe)
      (res-of-operators obj)))

(defmethod instantiable-object-p ((obj nail))
  t)					; weil Koordinaten bekannt!

(defmethod instantiable-object-p ((obj at-least-1d-query-object)) ; fuer Linien, Ketten, Polygone
  (every #'(lambda (part)
	     (instantiable-object-p part))
	 (get-direct-components obj)))

;;;
;;;
;;;

(defmethod change-status-applicable-p ((obj query-object)
				       (status (eql 'db)))
  (and (primary-p obj)
       (every #'(lambda (component)
		  (change-status-applicable-p component 'db-component))
	      (get-direct-components obj))))

(defmethod change-status-applicable-p ((obj query-object)
				       (status (eql 'db-component)))
  (and (not (typep obj 'polygon))
       (every #'(lambda (component)
		  (change-status-applicable-p component 'db-component))
	      (get-direct-components obj))))


(defmethod change-status-applicable-p ((obj query-object)
				       (status (eql 'universe)))
  (instantiable-object-p obj))


(defmethod change-status-applicable-p ((obj point) 
				       (status (eql 'universe)))
  (every #'(lambda (master)
	     (not (typep master 'rubberband)))
	 (part-of obj)))


(defmethod change-status-applicable-p ((obj marble) 
				       (status (eql 'universe)))
  (res-of-operators obj))


(defmethod change-status-applicable-p ((obj origin)
				       (status (eql 'universe)))
  nil)

;;;
;;;
;;;

(defmethod set-status ((obj query-object) (status (eql 'db)))
  (when (and (every #'(lambda (master)
			(change-status-applicable-p master 'universe))
		    (part-of obj))
	     (every #'(lambda (part)
			(change-status-applicable-p part 'db-component))
		    (get-direct-components obj)))
    (setf (status obj) 'db)
    (dolist (part (get-direct-components obj))
      (set-status part 'db-component))
    (dolist (master (part-of obj))
      (set-status master 'universe))))

(defmethod set-status ((obj query-object) (status (eql 'db-component)))
  (when (every #'(lambda (part)
		   (change-status-applicable-p part 'db-component))
	       (get-direct-components obj))
    (setf (status obj) 'db-component)
    (dolist (part (get-direct-components obj))
      (set-status part 'db-component))))

(defmethod set-status ((obj query-object) (status (eql 'universe)))
  (when (every #'(lambda (master)
		   (change-status-applicable-p master 'universe))
	       (part-of obj))
    (setf (status obj) 'universe)
    (dolist (master (part-of obj))
      (set-status master 'universe))))

