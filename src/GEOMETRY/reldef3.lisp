;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: GEOMETRY; Base: 10 -*-

(in-package geometry)

(defgeneric calculate-relation (obj1 obj2 &rest rest)
  (:documentation "Recognizes various spatial relationships between obj1 and obj2."))

;;;
;;; Punkt-Relationen (Punkt ist erstes Argument)
;;;

(defun inverse (rel)
  (case rel
    (contains 'inside)
    (inside 'contains)
    (covered-by 'covers)
    (covers 'covered-by)
    (otherwise rel)))


(defmethod calculate-relation ((p geom-point) (l geom-line) &rest rest)
  (declare (ignore rest))
  (if (lies-on-p p l)
      'lies-on
    'disjoint))

(defmethod calculate-relation ((l geom-line) (p geom-point) &rest rest)
  (declare (ignore rest))
  (inverse
   (calculate-relation p l)))

;;;
;;; Strecken-Relationen (Strecke ist erstes Argument)
;;;


(defmethod calculate-relation ((l1 geom-line) (l2 geom-line) &key (detailed nil))
  (if (and (bounding-box-p l1)
	   (bounding-box-p l2)
	   (not (box-overlaps-box-p l1 l2)))
      'disjoint
    (let* ((l1p1 (p1 l1))
	   (l2p1 (p1 l2))
	   (l1p2 (p2 l1))
	   (l2p2 (p2 l2))
	   
	   (a  (ccw l1p1 l1p2 l2p1))
	   (b  (ccw l1p1 l1p2 l2p2))
	   (c  (ccw l2p1 l2p2 l1p1))
	   (d  (ccw l2p1 l2p2 l1p2)))
      
      (cond ((and (<= (* a b) 0.0) (<= (* c d) 0.0)) ; Schnitt liegt vor
	     (let* ((l1p1-l2 (lies-on-p (p1 l1) l2))
		    (l1p2-l2 (lies-on-p (p2 l1) l2))
		    (l2p1-l1 (lies-on-p (p1 l2) l1))
		    (l2p2-l1 (lies-on-p (p2 l2) l1))
		    (n (+ (if l1p1-l2 1 0)
			  (if l1p2-l2 1 0)
			  (if l2p1-l1 1 0)
			  (if l2p2-l1 1 0))))
	       (let ((res
		      (ecase n
			(0 'crosses)	; "X"
			(1 'touches)	; "T"
			(3 
			 (if detailed
			     (if (and l1p1-l2 l1p2-l2)
				 'covered-by		       
			       'covers)
			   '1d-intersects)) ; "covers", "covered-by"
			(4 'equal)
			(2 (if (joins-p l1 l2) 
			       'touches	; "--", "\/"
			     (if detailed
				 (if (and l1p1-l2 l1p2-l2)
				     'inside
				   (if (and l2p1-l1 l2p2-l1)
				       'contains
				     'overlaps))
			       '1d-intersects)))))) ; "contains", "inside", "overlaps"
		 (if detailed
		     (values res l1p1-l2 l1p2-l2 l2p1-l1 l2p2-l1)
		   res))))
	    (t 'disjoint)))))



