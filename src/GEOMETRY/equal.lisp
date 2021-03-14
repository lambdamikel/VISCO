;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: GEOMETRY; Base: 10 -*-

(in-package geometry)

(defgeneric equal-p (obj1 obj2)
  (:documentation "Returns t, if both objects are semantically equivalent."))

;;;
;;; Equal-Relation
;;;


(defmethod equal-p ((obj1 geom-thing) (obj2 geom-thing))
  nil)

(defmethod equal-p ((obj1 geom-point) (obj2 geom-point))
  (point-=-p obj1 obj2))

(defmethod equal-p ((obj1 geom-line) (obj2 geom-line))
  (line-=-p obj1 obj2))

(defmethod equal-for-complex-objects-p ((obj1 geom-thing) (obj2 geom-thing) 
					(access-fn function))
  (and (= (length (funcall access-fn obj1))
	  (length (funcall access-fn obj2)))
       (every #'(lambda (i)
		 (= 1 
		    (count-if #'(lambda (j)
				  (equal-p i j))
			      (funcall access-fn obj2))))
	      (funcall access-fn obj1))))


(defmethod equal-p ((obj1 geom-chain-or-polygon) (obj2 geom-chain-or-polygon))
  (equal-for-complex-objects-p obj1 obj2 #'segments))
		   

(defmethod equal-p ((obj1 geom-aggregate) (obj2 geom-aggregate))
  (equal-for-complex-objects-p obj1 obj2 #'has-parts))



