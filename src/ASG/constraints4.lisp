;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: QUERY-COMPILER; Base: 10 -*-

(in-package query-compiler)

;;;
;;; Constraints sind stets unidirektional ("->"-Kante, es gibt jedoch IMMER eine entsp. Inverse!)
;;; Constraint sind stets binaer: unaere Constraints werden im Query-Objekt selbst modelliert
;;; (werden hineinkomponiert).
;;;

(defpersistentclass constraints-mixin ()
  ((constraints :accessor constraints :initform nil)))

;;;
;;;
;;;

(defpersistentclass constraint ()
  ())

(defpersistentclass binary-constraint (constraint)
  ((1st-arg :accessor 1st-arg :initarg :1st-arg)
   (2nd-arg :accessor 2nd-arg :initarg :2nd-arg)))

;;;
;;; Die folgenden Constraints werden auf der Oberflaeche (bzw. durch die Sprache) erzeugt:
;;;

(defpersistentclass disjoint (binary-constraint)
  ())

(defpersistentclass inside (binary-constraint)
  ())

(defpersistentclass contains (binary-constraint)
  ())

(defpersistentclass intersects (binary-constraint)
  ())

(defpersistentclass angle-between (binary-constraint)
  ((allowed-derivation :accessor allowed-derivation :initarg :allowed-derivation)))

;;;
;;;
;;;

(defun inverse (rel)
  (case rel
    (inside 'contains)
    (contains 'inside)       
    (outside 'excludes)
    (excludes 'outside)
    (inside-epsilon 'epsilon-contains)
    (epsilon-contains 'inside-epsilon)
    (otherwise rel)))

(defmethod find-constraint ((symbol symbol)
			    (obj1 constraints-mixin) 
			    (obj2 constraints-mixin))
  (find-if #'(lambda (constraint)
	       (and (typep constraint symbol)
		    (eq (2nd-arg constraint) obj2)))
	   (constraints obj1)))

(defmethod find-and-delete-constraint ((symbol symbol)
				       (obj1 constraints-mixin) 
				       (obj2 constraints-mixin))
  (setf (constraints obj1)
    (delete (find-constraint symbol obj1 obj2)
	    (constraints obj1)))
  (setf (constraints obj2)
    (delete (find-constraint (inverse symbol) obj2 obj1)
	    (constraints obj2))))

(defmethod make-and-memoize-binary-constraint ((symbol symbol)
					       (obj1 constraints-mixin) 
					       (obj2 constraints-mixin)
					       &rest initargs)
  (if (eq obj1 obj2)
      (error "No constraint!")
    (progn
      (unless (find-constraint symbol obj1 obj2)
	(push (apply #'make-instance symbol 
		     :1st-arg obj1
		     :2nd-arg obj2 
		     initargs)
	      (constraints obj1))
	(push (apply #'make-instance (inverse symbol)
		     :1st-arg obj2
		     :2nd-arg obj1 
		     initargs)
	      (constraints obj2))))))

;;;
;;;
;;;

(defun remove-eventually-present-constraints (&rest args)
  (when (every #'(lambda (obj)
		   (typep obj 'constraints-mixin))
	       args)
    (dolist (i args)
      (dolist (constraint (constraints i))
	(when (member (2nd-arg constraint)
		      args)
	  (setf (constraints i)
	    (delete constraint (constraints i))))))))
    
