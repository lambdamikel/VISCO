;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: OBJECTS-WITH-RELATIONS; Base: 10 -*-

(in-package objects-with-relations)

(defgeneric calculate-relations (obj)
  (:documentation "Calculate all interesting spatial relations to (and from) obj. Clients should specialize
this function and establish the relation by calling store-intersection-between or store-inside-between."))

(defgeneric remove-all-relations (obj)
  (:documentation "Remove all relations for obj."))

(defgeneric store-intersection-between (obj1 obj2)
  (:documentation "Store intersects relation between obj1 and obj2."))

(defgeneric store-inside-between (obj1 obj2)
  (:documentation "Store inside relation between obj1 and obj2."))


(defgeneric delete-intersection-between (obj1 obj2)
  (:documentation "Delete intersects relation between obj1 and obj2."))

(defgeneric delete-inside-between (obj1 obj2)
  (:documentation "Delete inside relation between obj1 and obj2."))

;;;
;;; 
;;;

(defparameter *objects-with-relations* nil)


(defpersistentclass geom-thing-with-relations (geom-thing)
  ((inside :accessor inside :initform nil)
   (intersects :accessor intersects :initarg :intersects :initform nil))) ; alle Schnitte

;;;
;;; Achtung: disjoint implizit vorhanden => keine Rel(a,b) eingetr. <=> disjoint(a,b)
;;; 

(defpersistentclass at-least-1d-geom-thing-with-relations (geom-thing-with-relations)
  ((intersects-points :accessor intersects-points :initarg :intersects-points :initform nil)
   (intersects-lines :accessor intersects-lines :initarg :intersects-lines :initform nil)
   (intersects-chains :accessor intersects-chains :initarg :intersects-chains :initform nil)
   (intersects-polygons :accessor intersects-polygons :initarg :intersects-polygons :initform nil)))

;;;
;;;
;;;


(defpersistentclass geom-point-with-relations (geom-thing-with-relations geom-point)
  ((intersects-lines :accessor intersects-lines :initarg :intersects-lines :initform nil)
   (intersects-chains :accessor intersects-chains :initarg :intersects-chains :initform nil)
   (intersects-polygons :accessor intersects-polygons :initarg :intersects-polygons :initform nil)))


(defpersistentclass geom-line-with-relations (at-least-1d-geom-thing-with-relations
					      geom-line)
  ())

(defpersistentclass geom-chain-or-polygon-with-relations (at-least-1d-geom-thing-with-relations
							  geom-chain-or-polygon)
  ())

(defpersistentclass geom-chain-with-relations (geom-chain-or-polygon-with-relations
					       geom-chain)
  ())

(defpersistentclass geom-polygon-with-relations (geom-chain-or-polygon-with-relations
						 geom-polygon)
  ((contains :accessor contains :initarg :contains :initform nil)
   (contains-points :accessor contains-points :initarg :contains-points :initform nil)
   (contains-lines :accessor contains-lines :initarg :contains-lines :initform nil)
   (contains-chains :accessor contains-chains :initarg :contains-chains :initform nil)
   (contains-polygons :accessor contains-polygons :initarg :contains-polygons :initform nil)))

#|
(defpersistentclass geom-aggregate-with-relations (at-least-1d-geom-thing-with-relations
						   geom-aggregate)
  ())
|#

;;;
;;;
;;;

(defmethod calculate-relations ((obj geom-thing-with-relations))
  "This function should be specialized by clients!"
  (dolist (obj2 *objects-with-relations*)
    (cond ((intersects-p obj obj2)
	   (store-intersection-between obj obj2))
	  ((inside-p obj obj2)
	   (store-inside-between obj obj2)))))

(defmethod initialize-instance :after ((obj geom-thing-with-relations) &rest initargs)
  (declare (ignore initargs))
  (calculate-relations obj)
  (push obj *objects-with-relations*))

(defmethod delete-object progn ((obj geom-thing-with-relations) &key)
  (setf *objects-with-relations*
    (delete obj *objects-with-relations*))
  (remove-all-relations obj))

;;;
;;;
;;;


(defmethod remove-all-relations ((obj geom-thing-with-relations))
  (dolist (i (intersects obj))
    (delete-intersection-between i obj)))

(defmethod remove-all-relations :after ((obj geom-polygon-with-relations))
  (dolist (c (contains obj))
    (delete-inside-between c obj)))


;;;
;;; INTERSECTS-Relation
;;;


(defmethod store-intersection-between ((obj1 geom-thing-with-relations) 
				       (obj2 geom-thing-with-relations))
  (pushnew obj2 (intersects obj1))
  (pushnew obj1 (intersects obj2)))

;;;
;;; fuer Punkte: 
;;;

(defmethod store-intersection-between :after ((obj1 geom-point-with-relations) 
					      (obj2 geom-line-with-relations))
  (pushnew obj2 (intersects-lines obj1))	
  (pushnew obj1 (intersects-points obj2)))

(defmethod store-intersection-between :after ((obj1 geom-point-with-relations) 
					      (obj2 geom-chain-with-relations))
  (pushnew obj2 (intersects-chains obj1))	
  (pushnew obj1 (intersects-points obj2)))

(defmethod store-intersection-between :after ((obj1 geom-point-with-relations) 
					      (obj2 geom-polygon-with-relations))
  (pushnew obj2 (intersects-polygons obj1))	
  (pushnew obj1 (intersects-points obj2)))


;;;
;;; fuer Linien:
;;;

(defmethod store-intersection-between :after ((obj1 geom-line-with-relations) 
					      (obj2 geom-point-with-relations))
  (pushnew obj2 (intersects-points obj1))	
  (pushnew obj1 (intersects-lines obj2)))

(defmethod store-intersection-between :after ((obj1 geom-line-with-relations) 
					      (obj2 geom-line-with-relations))
  (pushnew obj2 (intersects-lines obj1))	
  (pushnew obj1 (intersects-lines obj2)))

(defmethod store-intersection-between :after ((obj1 geom-line-with-relations) 
					      (obj2 geom-chain-with-relations))
  (pushnew obj2 (intersects-chains obj1))	
  (pushnew obj1 (intersects-lines obj2)))

(defmethod store-intersection-between :after ((obj1 geom-line-with-relations) 
					      (obj2 geom-polygon-with-relations))
  (pushnew obj2 (intersects-polygons obj1))	
  (pushnew obj1 (intersects-lines obj2)))

;;;
;;; fuer Ketten:
;;;

(defmethod store-intersection-between :after ((obj1 geom-chain-with-relations) 
					      (obj2 geom-point-with-relations))
  (pushnew obj2 (intersects-points obj1))	
  (pushnew obj1 (intersects-chains obj2)))

(defmethod store-intersection-between :after ((obj1 geom-chain-with-relations) 
					      (obj2 geom-line-with-relations))
  (pushnew obj2 (intersects-lines obj1))	
  (pushnew obj1 (intersects-chains obj2)))

(defmethod store-intersection-between :after ((obj1 geom-chain-with-relations) 
					      (obj2 geom-chain-with-relations))
  (pushnew obj2 (intersects-chains obj1))	
  (pushnew obj1 (intersects-chains obj2)))

(defmethod store-intersection-between :after ((obj1 geom-chain-with-relations) 
					      (obj2 geom-polygon-with-relations))
  (pushnew obj2 (intersects-polygons obj1))	
  (pushnew obj1 (intersects-chains obj2)))


;;;
;;; fuer Polygone:
;;;

(defmethod store-intersection-between :after ((obj1 geom-polygon-with-relations) 
					      (obj2 geom-point-with-relations))
  (pushnew obj2 (intersects-points obj1))	
  (pushnew obj1 (intersects-polygons obj2)))

(defmethod store-intersection-between :after ((obj1 geom-polygon-with-relations) 
					      (obj2 geom-line-with-relations))
  (pushnew obj2 (intersects-lines obj1))	
  (pushnew obj1 (intersects-polygons obj2)))

(defmethod store-intersection-between :after ((obj1 geom-polygon-with-relations) 
					      (obj2 geom-chain-with-relations))
  (pushnew obj2 (intersects-chains obj1))	
  (pushnew obj1 (intersects-polygons obj2)))

(defmethod store-intersection-between :after ((obj1 geom-polygon-with-relations) 
					      (obj2 geom-polygon-with-relations))
  (pushnew obj2 (intersects-polygons obj1))	
  (pushnew obj1 (intersects-polygons obj2)))

;;;
;;;
;;;


(defmethod delete-intersection-between ((obj1 geom-thing-with-relations) 
					(obj2 geom-thing-with-relations))
  (setf (intersects obj1) (delete obj2 (intersects obj1))
	(intersects obj2) (delete obj1 (intersects obj2))))

;;;
;;; fuer Punkte: 
;;;

(defmethod delete-intersection-between :after ((obj1 geom-point-with-relations) 
					       (obj2 geom-line-with-relations))
  (setf (intersects-lines obj1) (delete obj2 (intersects-lines obj1))
	(intersects-points obj2) (delete obj1 (intersects-points obj2))))

(defmethod delete-intersection-between :after ((obj1 geom-point-with-relations) 
					       (obj2 geom-chain-with-relations))
  (setf (intersects-chains obj1) (delete obj2 (intersects-chains obj1))
	(intersects-points obj2) (delete obj1 (intersects-points obj2))))

(defmethod delete-intersection-between :after ((obj1 geom-point-with-relations) 
					       (obj2 geom-polygon-with-relations))
  (setf (intersects-polygons obj1) (delete obj2 (intersects-polygons obj1))	
	(intersects-points obj2) (delete obj1 (intersects-points obj2))))


;;;
;;; fuer Linien:
;;;

(defmethod delete-intersection-between :after ((obj1 geom-line-with-relations) 
					       (obj2 geom-point-with-relations))
  (setf (intersects-points obj1) (delete obj2 (intersects-points obj1))	
	(intersects-lines obj2) (delete obj1 (intersects-lines obj2))))

(defmethod delete-intersection-between :after ((obj1 geom-line-with-relations) 
					       (obj2 geom-line-with-relations))
  (setf (intersects-lines obj1) (delete obj2 (intersects-lines obj1))	
	(intersects-lines obj2) (delete obj1 (intersects-lines obj2))))

(defmethod delete-intersection-between :after ((obj1 geom-line-with-relations) 
					       (obj2 geom-chain-with-relations))
  (setf (intersects-chains obj1) (delete obj2 (intersects-chains obj1))   
	(intersects-lines obj2) (delete obj1 (intersects-lines obj2))))

(defmethod delete-intersection-between :after ((obj1 geom-line-with-relations) 
					       (obj2 geom-polygon-with-relations))
  (setf (intersects-polygons obj1) (delete obj2 (intersects-polygons obj1))
	(intersects-lines obj2) (delete obj1 (intersects-lines obj2))))

;;;
;;; fuer Ketten:
;;;

(defmethod delete-intersection-between :after ((obj1 geom-chain-with-relations) 
					       (obj2 geom-point-with-relations))
  (setf (intersects-points obj1) (delete obj2 (intersects-points obj1))	
	(intersects-chains obj2) (delete obj1 (intersects-chains obj2))))

(defmethod delete-intersection-between :after ((obj1 geom-chain-with-relations) 
					       (obj2 geom-line-with-relations))
  (setf (intersects-lines obj1) (delete obj2 (intersects-lines obj1))	
	(intersects-chains obj2) (delete obj1 (intersects-chains obj2))))

(defmethod delete-intersection-between :after ((obj1 geom-chain-with-relations) 
					       (obj2 geom-chain-with-relations))
  (setf (intersects-chains obj1) (delete obj2 (intersects-chains obj1))	
	(intersects-chains obj2) (delete obj1 (intersects-chains obj2))))

(defmethod delete-intersection-between :after ((obj1 geom-chain-with-relations) 
					       (obj2 geom-polygon-with-relations))
  (setf (intersects-polygons obj1) (delete obj2 (intersects-polygons obj1))	
	(intersects-chains obj2) (delete obj1 (intersects-chains obj2))))


;;;
;;; fuer Polygone:
;;;

(defmethod delete-intersection-between :after ((obj1 geom-polygon-with-relations) 
					       (obj2 geom-point-with-relations))
  (setf (intersects-points obj1) (delete obj2 (intersects-points obj1))	
	(intersects-polygons obj2) (delete obj1 (intersects-polygons obj2))))

(defmethod delete-intersection-between :after ((obj1 geom-polygon-with-relations) 
					       (obj2 geom-line-with-relations))
  (setf (intersects-lines obj1) (delete obj2 (intersects-lines obj1))
	(intersects-polygons obj2) (delete obj1 (intersects-polygons obj2))))

(defmethod delete-intersection-between :after ((obj1 geom-polygon-with-relations) 
					       (obj2 geom-chain-with-relations))
  (setf (intersects-chains obj1) (delete obj2 (intersects-chains obj1))	
	(intersects-polygons obj2) (delete obj1 (intersects-polygons obj2))))

(defmethod delete-intersection-between :after ((obj1 geom-polygon-with-relations) 
					       (obj2 geom-polygon-with-relations))
  (setf (intersects-polygons obj1) (delete obj2 (intersects-polygons obj1))
	(intersects-polygons obj2) (delete obj1 (intersects-polygons obj2))))


;;;
;;; INSIDE-Relation
;;;


(defmethod store-inside-between ((obj1 geom-thing-with-relations) 
				 (obj2 geom-polygon-with-relations))
  (pushnew obj2 (inside obj1))
  (pushnew obj1 (contains obj2)))

;;;
;;;
;;;

(defmethod store-inside-between :after ((obj1 geom-point-with-relations) 
					(obj2 geom-polygon-with-relations))
  (pushnew obj1 (contains-points obj2)))

(defmethod store-inside-between :after ((obj1 geom-line-with-relations) 
					(obj2 geom-polygon-with-relations))
  (pushnew obj1 (contains-lines obj2)))

(defmethod store-inside-between :after ((obj1 geom-chain-with-relations) 
					(obj2 geom-polygon-with-relations))
  (pushnew obj1 (contains-chains obj2)))

(defmethod store-inside-between :after ((obj1 geom-polygon-with-relations) 
					(obj2 geom-polygon-with-relations))
  (pushnew obj1 (contains-polygons obj2)))


;;;
;;;
;;;


(defmethod delete-inside-between :after ((obj1 geom-thing-with-relations) 
					 (obj2 geom-polygon-with-relations))
  (setf (inside obj1) (delete obj2 (inside obj1))
	(contains obj2) (delete obj1 (contains obj2))))

;;;
;;; 
;;;


(defmethod delete-inside-between :after ((obj1 geom-point-with-relations) 
					 (obj2 geom-polygon-with-relations))
  (setf (contains-points obj2) (delete obj1 (contains-points obj2))))

(defmethod delete-inside-between :after ((obj1 geom-line-with-relations) 
					 (obj2 geom-polygon-with-relations))
  (setf (contains-lines obj2) (delete obj1 (contains-lines obj2))))

(defmethod delete-inside-between :after ((obj1 geom-chain-with-relations) 
					 (obj2 geom-polygon-with-relations))
  (setf (contains-chains obj2) (delete obj1 (contains-chains obj2))))


(defmethod delete-inside-between :after ((obj1 geom-polygon-with-relations) 
					 (obj2 geom-polygon-with-relations))
  (setf (contains-polygons obj2) (delete obj1 (contains-polygons obj2))))

