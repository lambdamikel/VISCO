;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: GEOMETRY; Base: 10 -*-

(in-package geometry)

(defgeneric lies-on-p (point obj)
  (:documentation "Determine whether point lies on obj.
If obj is a polygon, this means the border of obj.
The point may conincide with one of the component points of obj => T."))

(defgeneric lies-on-p* (x y obj)
  (:documentation "Determine whether point (x,y) lies on obj.
If obj is a polygon, this means the border of obj.
The point may conincide with one of the component points of obj => T."))




(defgeneric intersects-p (obj1 obj2)
  (:documentation "Determine whether obj1 and obj2 intersect. If one of the objects is a polygon (or both), 
only the border is considered, not the interior. A point obj1 intersects-p an obj2 (line, polygon or chain),
if lies-on-p holds."))




(defgeneric touches-p (line1 line2)
  (:documentation "Determine whether line1 and line2 have only one common intersection point. Line1 and
line2 may not cross each other. A joins-p relation may be a touches-p relation."))

(defgeneric crosses-p (line obj)
  (:documentation "Determine whether line crosses obj. There must be exactly one common 
intersection point. This point may not coincide with one of the component points of line or obj.
No joins-p relation can ever be a crosses-p relation. touches-p and crosses-p are disjoint."))

(defgeneric 1d-intersects-p (line1 line2)
  (:documentation "Determine whether line1 and line2 are parallel and share a common piece of a line.
A joins-p relations may be a 1d-intersects-p relation. Crosses-p, touches-p and 1d-intersects-p are
disjoint."))


;;;
;;; intersects-p(line1,line2) => touches-p \/ crosses-p \/ 1d-intersects-p
;;;


(defgeneric 0d-touches-p (line poly-or-chain)
  (:documentation "Determine whether there is a poly-or-chain segment which touches-p the line.
Also, no crosses-p relation between line and poly-or-chain may hold."))

(defgeneric 1d-touches-p (line poly-or-chain)
  (:documentation "Determine whether there is a poly-or-chain segment which 1d-intersects-p the line.
Also, no crosses-p relation between line and poly-or-chain may hold."))

;;;
;;; LIES-ON
;;;

(defmethod lies-on-p* (x y (line geom-line))
  (or
   (zerop (ccw* (x (p1 line))
		(y (p1 line))
		(x (p2 line))
		(y (p2 line))
		x y))
   (zerop (ccw* (x (p2 line))
		(y (p2 line))
		(x (p1 line))
		(y (p1 line))
		x y))))

(defmethod lies-on-p ((point geom-point) (line geom-line))
  (or
   (zerop (ccw (p1 line) (p2 line) point))
   (zerop (ccw (p2 line) (p1 line) point))))

;;;
;;; INTERSECTS
;;;

(defmethod intersects-p ((point geom-point) (line geom-line))
  (lies-on-p point line))

(defmethod intersects-p ((line geom-line) (point geom-point))
  (lies-on-p point line))

;;;
;;; LIES-ON
;;;

(defmethod lies-on-p* (x y (poly-or-chain geom-chain-or-polygon))
  (some #'(lambda (segment)
	    (lies-on-p* x y segment))
	(segments poly-or-chain)))


(defmethod lies-on-p ((point geom-point) (poly-or-chain geom-chain-or-polygon))
  (some #'(lambda (segment)
	    (lies-on-p point segment))
	(segments poly-or-chain)))

;;;
;;; INTERSECTS
;;;

(defmethod intersects-p ((point geom-point) (poly-or-chain geom-chain-or-polygon))
  (lies-on-p point poly-or-chain))


(defmethod intersects-p ((poly-or-chain geom-chain-or-polygon) (point geom-point))
  (lies-on-p point poly-or-chain))


;;;
;;;
;;;

(defun intersects-p* (xs1 ys1 xe1 ye1
		      xs2 ys2 xe2 ye2)
  (let* ((a  (ccw* xs1 ys1 
		   xe1 ye1
		   xs2 ys2))
	 (b  (ccw* xs1 ys1
		   xe1 ye1
		   xe2 ye2))
	 (c  (ccw* xs2 ys2
		   xe2 ye2
		   xs1 ys1))
	 (d  (ccw* xs2 ys2
		   xe2 ye2
		   xe1 ye1)))
    (and
     (<= (* a b) 0.0) (<= (* c d) 0.0))))

(defun crosses-p* (xs1 ys1 xe1 ye1
		   xs2 ys2 xe2 ye2)
  (let* ((a  (ccw* xs1 ys1 
		   xe1 ye1
		   xs2 ys2))
	 (b  (ccw* xs1 ys1
		   xe1 ye1
		   xe2 ye2))
	 (c  (ccw* xs2 ys2
		   xe2 ye2
		   xs1 ys1))
	 (d  (ccw* xs2 ys2
		   xe2 ye2
		   xe1 ye1)))
    (when (and (<= (* a b) 0.0) (<= (* c d) 0.0)) ; Schnitt liegt vor
      (not (or (zerop a) 
	       (zerop b)
	       (zerop c)
	       (zerop d)
	       
	       (zerop (ccw* xe1 ye1
			    xs1 ys1 
			    xs2 ys2))
	       (zerop (ccw* xe1 ye1
			    xs1 ys1
			    xe2 ye2))
	       (zerop (ccw* xe2 ye2
			    xs2 ys2
			    xs1 ys1))
	       (zerop (ccw* xe2 ye2
			    xs2 ys2
			    xe1 ye1)))))))

;;;
;;; INTERSECTS
;;;


(defmethod intersects-p ((l1 geom-line) (l2 geom-line)) 
  (and (=> (and (bounding-box-p l1)
		(bounding-box-p l2))
	   (box-overlaps-box-p l1 l2))
       (let* ((l1p1 (p1 l1))
	      (l2p1 (p1 l2))
	      (l1p2 (p2 l1))
	      (l2p2 (p2 l2))
	      
	      (a  (ccw l1p1 l1p2 l2p1))
	      (b  (ccw l1p1 l1p2 l2p2))
	      (c  (ccw l2p1 l2p2 l1p1))
	      (d  (ccw l2p1 l2p2 l1p2)))
	 (and
	  (<= (* a b) 0.0) (<= (* c d) 0.0)))))


;;;
;;; CROSSES
;;;

(defmethod crosses-p ((l1 geom-line) (l2 geom-line)) ; "X" oder "+" 
  (and (=> (and (bounding-box-p l1)
		(bounding-box-p l2))
	   (box-truly-overlaps-box-p l1 l2))
       (let* ((l1p1 (p1 l1))
	      (l2p1 (p1 l2))
	      (l1p2 (p2 l1))
	      (l2p2 (p2 l2))
	      
	      (a  (ccw l1p1 l1p2 l2p1))
	      (a1 (ccw l1p2 l1p1 l2p1))
	      (b  (ccw l1p1 l1p2 l2p2))
	      (b1 (ccw l1p2 l1p1 l2p2))
	      (c  (ccw l2p1 l2p2 l1p1))
	      (c1 (ccw l2p2 l2p1 l1p1))
	      (d  (ccw l2p1 l2p2 l1p2))
	      (d1 (ccw l2p2 l2p1 l1p2)))
	 
	 (and (not (or (zerop a)
		       (zerop a1)
		       (zerop b)
		       (zerop b1)
		       (zerop c)
		       (zerop c1)
		       (zerop d)
		       (zerop d1)))
	      (<= (* a b) 0) (<= (* c d) 0)))))

;;;
;;; TOUCHES
;;;


(defmethod touches-p ((l1 geom-line) (l2 geom-line)) ; z.B. meets ("--"), aber auch "T" oder "\/"
  (and (=> (and (bounding-box-p l1)
		(bounding-box-p l2))
	   (box-overlaps-box-p l1 l2))
       (let ((l1p1-l2 (lies-on-p (p1 l1) l2))
	     (l1p2-l2 (lies-on-p (p2 l1) l2))
	     (l2p1-l1 (lies-on-p (p1 l2) l1))
	     (l2p2-l1 (lies-on-p (p2 l2) l1)))

	 (or (and l1p1-l2 (not l1p2-l2) (not l2p1-l1) (not l2p2-l1)) ; "T"-Faelle
	     (and (not l1p1-l2) l1p2-l2 (not l2p1-l1) (not l2p2-l1)) 
	     (and (not l1p1-l2) (not l1p2-l2) l2p1-l1 (not l2p2-l1))  
	     (and (not l1p1-l2) (not l1p2-l2) (not l2p1-l1) l2p2-l1)
	     
	     (and (joins-p l1 l2)	; "--" und "\/"-Faelle 
		  (not (or (and l1p1-l2 l1p2-l2) ; 1d-schnitte ausschliessen
			   (and l2p1-l1 l2p2-l1))))))))

;;;
;;; 1D-INTERSECTS
;;;


(defmethod 1d-intersects-p ((l1 geom-line) (l2 geom-line)) ; overlaps, contains, covers, equal + Inverse
  (and (=> (and (bounding-box-p l1)
		(bounding-box-p l2))
	   (box-truly-overlaps-box-p l1 l2))
       (let ((l1p1-l2 (lies-on-p (p1 l1) l2))
	     (l1p2-l2 (lies-on-p (p2 l1) l2))
	     (l2p1-l1 (lies-on-p (p1 l2) l1))
	     (l2p2-l1 (lies-on-p (p2 l2) l1)))
	 
	 
	 (or (and l1p1-l2       (not l1p2-l2) l2p1-l1       (not l2p2-l1) ; overlaps-faelle
		  (not (point-=-p (p1 l1) (p1 l2)))) ; touches ausschliessen
	     (and l1p1-l2       (not l1p2-l2) (not l2p1-l1) l2p2-l1
		  (not (point-=-p (p1 l1) (p2 l2))))
	     (and (not l1p1-l2) l1p2-l2       l2p1-l1       (not l2p2-l1)
		  (not (point-=-p (p2 l1) (p1 l2))))
	     (and (not l1p1-l2) l1p2-l2       (not l2p1-l1) l2p2-l1
		  (not (point-=-p (p2 l1) (p2 l2))))
	     
	     (and      l1p1-l2       l1p2-l2  (not l2p1-l1) (not l2p2-l1)) ; l1 contains l2
	     (and (not l1p1-l2) (not l1p2-l2)      l2p1-l1       l2p2-l1)
	     
	     (and l1p1-l2 l1p2-l2 l2p1-l1 l2p2-l1) ; equal
	     
	     (and l1p1-l2       (not l1p2-l2) l2p1-l1 l2p2-l1) ; l1 covers l2
	     (and (not l1p1-l2)      l1p2-l2  l2p1-l1 l2p2-l1)
	     
	     (and l1p1-l2 l1p2-l2      l2p1-l1  (not l2p2-l1)) ; l2 covers l1
	     (and l1p1-l2 l1p2-l2 (not l2p1-l1)      l2p2-l1)))))


;;;
;;; INTERSECTS
;;;

(defmethod intersects-p ((line geom-line) (poly-or-chain geom-chain-or-polygon))
  (and (=> (and (bounding-box-p line)
		(bounding-box-p poly-or-chain))
	   (box-overlaps-box-p line poly-or-chain))
       (some #'(lambda (segment) 
		 (intersects-p line segment))	     
	     (segments poly-or-chain))))


(defmethod intersects-p ((poly-or-chain geom-chain-or-polygon) (line geom-line))
  (intersects-p line poly-or-chain))

;;;
;;; CROSSES
;;;

(defmethod crosses-p ((line geom-line) (poly-or-chain geom-chain-or-polygon))                               
  (and (=> (and (bounding-box-p line)
		(bounding-box-p poly-or-chain))
	   (box-overlaps-box-p line poly-or-chain))
       (some #'(lambda (segment) 
		 (crosses-p line segment))	     
	     (segments poly-or-chain))))

;;;
;;; 0D-TOUCHES
;;;

(defmethod 0d-touches-p ((line geom-line) (poly-or-chain geom-chain-or-polygon))
  (and (=> (and (bounding-box-p line)
		(bounding-box-p poly-or-chain))
	   (box-overlaps-box-p line poly-or-chain))
       (some #'(lambda (segment) 
		 (touches-p line segment))
	     (segments poly-or-chain))
       (not (crosses-p line poly-or-chain))))

(defmethod 0d-touches-p ((poly-or-chain geom-chain-or-polygon) (line geom-line))
  (0d-touches-p line poly-or-chain))

;;;
;;; 1D-TOUCHES
;;;

(defmethod 1d-touches-p ((line geom-line) (poly-or-chain geom-chain-or-polygon))
  (and (=> (and (bounding-box-p line)
		(bounding-box-p poly-or-chain))
	   (box-overlaps-box-p line poly-or-chain))
       (some #'(lambda (segment) 
		 (1d-intersects-p line segment))
	     (segments poly-or-chain))
       (not (crosses-p line poly-or-chain))))


(defmethod 1d-touches-p ((poly-or-chain geom-chain-or-polygon) (line geom-line))
  (1d-touches-p line poly-or-chain))

;;;
;;; INTERSECTS
;;;

(defmethod intersects-p ((poly-or-chain1 geom-chain-or-polygon) (poly-or-chain2 geom-chain-or-polygon))
  (and (=> (and (bounding-box-p poly-or-chain1)
		(bounding-box-p poly-or-chain2))
	   (box-overlaps-box-p poly-or-chain1 poly-or-chain2))
       (some #'(lambda (segment) 
		 (intersects-p poly-or-chain1 segment))	     
	     (segments poly-or-chain2))))
