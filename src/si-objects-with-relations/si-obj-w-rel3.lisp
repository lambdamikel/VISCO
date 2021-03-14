;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: SI-OBJECTS-WITH-RELATIONS; Base: 10 -*-

(in-package si-objects-with-relations)

;;;
;;; Instantiierbare Klassen (Konstruktoren existieren):
;;;

(defpersistentclass si-geom-thing-with-relations (si-geom-thing geom-thing-with-relations)
  ())

(defpersistentclass si-geom-point-with-relations (si-geom-thing-with-relations
						  si-geom-point geom-point-with-relations)
  ())

(defpersistentclass si-geom-line-with-relations (si-geom-thing-with-relations
						 si-geom-line geom-line-with-relations)
  ())

(defpersistentclass si-geom-chain-with-relations (si-geom-thing-with-relations
						  si-geom-chain geom-chain-with-relations)
  ())

(defpersistentclass si-geom-polygon-with-relations (si-geom-thing-with-relations
						    si-geom-polygon geom-polygon-with-relations)
  ())

;;;
;;;
;;;


(defmethod make-point ((class (eql 'si-geom-point-with-relations))
		       x y &rest initargs)
  (let ((point
	 (get-point-from-spatial-index* x y)))
    (or point
	(apply #'make-instance 
	       class
	       :allow-other-keys t
	       :x x :y y	      
	       initargs))))


(defmethod make-line ((class (eql 'si-geom-line-with-relations))
		      p1 p2 &rest initargs)
  (let ((line 
	 (get-already-present-direct-master p1 p2)))
    (or line
	(apply #'make-instance class
	       :allow-other-keys t
	       :p1 p1 :p2 p2
	       :class-of-internal-points 'si-geom-point-with-relations
	       initargs))))



(defun polygon-or-chain-constructor (class segment-list &rest initargs)
  (let ((obj 
	 (apply #'get-already-present-direct-master segment-list)))
    (or obj
	(apply #'make-instance class	       
	       :allow-other-keys t
	       :segments segment-list
	       :class-of-internal-points 'si-geom-point-with-relations
	       initargs))))


(defmethod make-chain ((class (eql 'si-geom-chain-with-relations))
		       segment-list
		       &rest initargs)
  (apply #'polygon-or-chain-constructor class segment-list initargs))


(defmethod make-polygon ((class (eql 'si-geom-polygon-with-relations))
			 segment-list
			 &rest initargs)
  (apply #'polygon-or-chain-constructor class segment-list initargs))


;;;
;;;
;;;


(defmethod calculate-relations ((i si-geom-point-with-relations))
  "Wenn ein neuer Punkt eingefuehrt wird => Relationen zu bestehenden Linien berechnen, hochpropagieren!"  
  (with-selected-objects (j i (and (typep j 'si-geom-line-with-relations)
				   (not (component-p i j)))
			    :intersects)
    (store-intersection-between i j)))


(defmethod calculate-relations ((i si-geom-line-with-relations))
  "Wenn eine neue Linie eingefuehrt wird => Relationen zu bestehenden Linien berechnen, hochpropagieren!"  
  (let ((new-points nil))
    (with-selected-objects (j i (not (or (eq i j)
					 (component-p i j)
					 (component-p j i)))
			      :intersects)
      (store-intersection-between i j)
      (when (and (typep j 'si-geom-line-with-relations)
		 (crosses-p i j))
	(multiple-value-bind (ix iy)
	    (calculate-intersection-point i j)
	  (when (and ix iy)
	    (push (list ix iy) new-points)))))
    (dolist (new-point new-points)
      (make-point 'si-geom-point-with-relations 
		  (first new-point)
		  (second new-point)))))

;;;
;;;
;;;


(defun calculate-inside-relations (list-of-rel-objects)
  (dolist (obj list-of-rel-objects)
    (when (typep obj 'si-geom-polygon-with-relations)
      (with-selected-objects (cur-obj obj (not (component-p cur-obj obj))
				      :inside)
	(store-inside-between cur-obj obj)))))

;;;
;;;
;;;


(defun calculate-relations-for-chain-or-polygon (chain-or-polygon)
  (dolist (segment (segments chain-or-polygon)) ; hochpropagieren: von Segmenten => chain-or-polygon
    (dolist (intersecting-obj (intersects segment))
      (unless (or (eq intersecting-obj chain-or-polygon)
		  (component-p intersecting-obj chain-or-polygon))
	(store-intersection-between intersecting-obj chain-or-polygon)))))

;;;
;;;
;;;


(defmethod calculate-relations ((i si-geom-chain-with-relations))
  (calculate-relations-for-chain-or-polygon i))


(defmethod calculate-relations ((i si-geom-polygon-with-relations))
  (calculate-relations-for-chain-or-polygon i))

