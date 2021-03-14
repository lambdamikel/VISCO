;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: GEOMETRY; Base: 10 -*-

(in-package geometry)

(defgeneric inside-p (obj1 obj2)
  (:documentation "Determine whether obj2 contains obj1. If obj1 touches obj2 from the inside, 
returns NIL."))

(defgeneric outside-p (obj1 obj2)
  (:documentation "Determine whether obj1 is outside of obj2. If obj1 touches obj2, returns NIL."))

(defgeneric inside-p* (x y obj)
  (:documentation "Determine whether obj contains point (x,y). If (x,y) lies on border of obj,
returns NIL."))

(defgeneric outside-p* (x y obj)
  (:documentation "Determine whether (x y) is outside of obj. If (x,y) lies on border of obj,
returns NIL."))

;;;
;;; Inside-Relation
;;;

(defparameter *cur-line*		; willkuerlich gewaehlt
    (l
     (p 0 0)
     (p 1 1)
     :bounding-box-p nil))

(defparameter *test-beam*		; willkuerlich gewaehlt
    (l
     (p 0 0)
     (p 1 1)
     :bounding-box-p nil))

(defun count-intersections* (x y polygon)
  (let* ((points (point-list polygon))
	 (count 0)
	 (from-point nil)
	 (cur-point nil)	 
	 (mark-point '?)
	 (flag nil))
    
    (setf 	  
	(x (p1 *test-beam*)) x
	(y (p1 *test-beam*)) y
	
	(x (p2 *test-beam*)) +big-int+
	(y (p2 *test-beam*)) y)
      
      (loop until (eq mark-point 'stop)
		
	do				; einmal ganz rum	  
	  (setf cur-point (first points)
		points (rest points))	  
	  (when (null points) 
	    (setf points (point-list polygon)))
	  
	  (when (eq cur-point mark-point)
	    (setf mark-point 'stop))
	  
	  (cond ((and (= y (y cur-point)) ; Zeit sparen...
		      (<= x (x cur-point))
		      (lies-on-p cur-point *test-beam*))
		 (setf flag t))
		 
		(t
		 (if (not from-point)	; noch kein erster Pkt. gefunden
		     (progn
		       (setf mark-point cur-point) ; merken => einmal rum!
		       (setf flag nil)
		       (setf from-point cur-point))
		   (progn
		     (cond (flag	; es lagen Punkte zwischen from-point und cur-point a.d. Teststrahl
			    (when (or (<= (y from-point) y (y cur-point))
				      (>= (y from-point) y (y cur-point)))
			      (incf count)))
			   (t		; es lagen keine Pkt. a.d. Teststrahl
			    (setf (p1 *cur-line*) from-point)
			    (setf (p2 *cur-line*) cur-point)
			    (let ((p1c (p1 *cur-line*))
				  (p2c (p2 *cur-line*))
				  (p1t (p1 *test-beam*))
				  (p2t (p2 *test-beam*)))				
			      (when (not (and (= (x p1c) (x p2c) x)))
				(when (intersects-p* (x p1c) (y p1c) (x p2c) (y p2c)
						     (slot-value p1t 'x) (slot-value p1t 'y)
						     (slot-value p2t 'x) (slot-value p2t 'y))
				  (incf count))))))
						     
		     
		     (setf flag nil)	     
		     (setf from-point cur-point))))))
      count))



(defun count-intersections (point polygon)
  (count-intersections* (x point) (y point) polygon))


;;;
;;; ACHTUNG: ALLE INSIDE/OUTSIDE-RELATIONEN SIND "TRULY"-INSIDE => RAND GEHOERT NICHT DAZU !!!
;;;

(defmethod inside-p* (x y (polygon geom-polygon))
  (and
   (=> (bounding-box-p polygon) 
       (point-truly-inside-box-p* x y polygon))
   (not (lies-on-p* x y polygon))
   (oddp (count-intersections* x y polygon))))

(defmethod inside-p ((point geom-point) (polygon geom-polygon))
  (inside-p* (x point) (y point) polygon))


(defmethod outside-p* (x y (polygon geom-polygon))
  (or 
   (and (bounding-box-p polygon)
	(point-truly-outside-box-p* x y polygon))
   (and 
    (not (lies-on-p* x y polygon))
    (evenp (count-intersections* x y polygon)))))
  
(defmethod outside-p ((point geom-point) (polygon geom-polygon))
  (outside-p* (x point) (y point) polygon))

;;;
;;;
;;;


(defmethod inside-p ((line geom-line) (poly geom-polygon))
  (and (=> (and (bounding-box-p line)
		(bounding-box-p poly))
	   (box-truly-inside-box-p line poly))
       (not (intersects-p line poly))
       (inside-p (p1 line) poly)
       (inside-p (p2 line) poly)))

(defmethod outside-p ((line geom-line) (poly geom-polygon)) 
  (or (and (bounding-box-p line)
	   (bounding-box-p poly)
	   (not (box-overlaps-box-p line poly)))
      
      (and (not (intersects-p line poly))
	   (outside-p (p1 line) poly)
	   (outside-p (p2 line) poly))))

;;;
;;;
;;;

(defmethod inside-p ((poly-or-chain geom-chain-or-polygon) (poly geom-polygon))
  (and (=> (and (bounding-box-p poly-or-chain)
		(bounding-box-p poly))
	   (box-truly-inside-box-p poly-or-chain poly))
       (every #'(lambda (segment)
		  (inside-p segment poly))
	      (segments poly-or-chain))))

(defmethod outside-p ((poly-or-chain geom-chain-or-polygon) (poly geom-polygon))
  (or (and (bounding-box-p poly-or-chain)
	   (bounding-box-p poly)
	   (not (box-overlaps-box-p poly-or-chain poly)))
      
      (every #'(lambda (segment) 
		 (outside-p segment poly))
	     (segments poly-or-chain))))





