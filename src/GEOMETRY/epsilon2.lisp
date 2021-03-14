;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: GEOMETRY; Base: 10 -*-

(in-package geometry)

(defgeneric inside-epsilon-p (obj1 obj2 radius &key epsilon-a epsilon-b)
  (:documentation "Determine whether obj1 lies inside the epsilon-enclosure
of radius of object obj2."))

;;;
;;;
;;;

(defconstant +granularity+ 2)

(defmethod make-arc ((pfrom geom-point) 
		     (pto geom-point)
		     xc yc)
  (multiple-value-bind (radius from)
      (distance-and-orientation* xc yc (x pfrom) (y pfrom))
    (multiple-value-bind (radius1 to)
	(distance-and-orientation* xc yc (x pto) (y pto))
      (if (=-eps radius radius1 0.00001)
	  (let* ((lastp pfrom)
		 (collect nil)
		 (diff (normalize (- to from)))
		 (fac (/ diff +granularity+))
		 (res
		  (dotimes (i (1- +granularity+) collect)
		    (let* ((angle (+ from (* (1+ i)  fac)))
			   (x (+ xc (* radius (cos angle))))
			   (y (+ yc (* radius (sin angle)))))
		      (let ((p (p x y)))
			(push
			 (l lastp p)
			 collect)
			(setf lastp p))))))
	    (append (reverse res)
		    (list
		     (l lastp pto))))
	(error "Bad points!")))))


(defun make-circle (x y radius)
  (let* ((lastp nil)
	 (firstx nil)
	 (firsty nil)		
	 (collect nil)
	 (fac (/ (* 2 pi) +granularity+))
	 (res    
	  (dotimes (i +granularity+ collect)
	    (let* ((angle (* i fac))
		   (x (+ x (* radius (cos angle))))
		   (y (+ y (* radius (sin angle)))))
	      (let ((p (p x y)))
		(if lastp
		    (push
		     (l lastp p)
		     collect)
		  (setf firstx x
			firsty y))
		(setf lastp p))))))
    (cons (l 
	   lastp
	   (p firstx firsty))
	  res)))

(defmethod create-epsilon-enclosure ((obj geom-point) (radius number))
  (make-circle (x obj) (y obj) radius))

(defmethod get-epsilon-enclosure-relevant-points ((obj geom-line) (radius number))
  (multiple-value-bind (p1fx p1fy
			p1tx p1ty
			p2fx p2fy
			p2tx p2ty)
      (get-epsilon-enclosure-relevant-points* obj radius)
    (let ((p1f (p p1fx p1fy))
	  (p1t (p p1tx p1ty))
	  (p2f (p p2fx p2fy))
	  (p2t (p p2tx p2ty)))      
      (values p1f p1t p2f p2t))))

(defmethod get-epsilon-enclosure-relevant-points* ((obj geom-line) (radius number))
  (with-slots (p1 p2) obj
    (let ((alpha (global-orientation obj)))
      (labels ((calc-pf (x y)
		 (values (- x (* radius (sin alpha)))
			 (+ y (* radius (cos alpha)))))
	       (calc-pt (x y)
		 (values (+ x (* radius (sin alpha)))
			 (- y (* radius (cos alpha))))))	     			  
	(multiple-value-bind (p1fx p1fy)
	    (calc-pf (x p1) (y p1))
	  (multiple-value-bind (p1tx p1ty)
	      (calc-pt (x p1) (y p1))
	    
	    (multiple-value-bind (p2fx p2fy)
		(calc-pf (x p2) (y p2))
	      (multiple-value-bind (p2tx p2ty)
		  (calc-pt (x p2) (y p2))
		
		(values p1fx p1fy 
			p1tx p1ty
			p2fx p2fy
			p2tx p2ty)))))))))


(defmethod create-epsilon-enclosure ((obj geom-line) (radius number))
  (with-slots (p1 p2) obj
    (multiple-value-bind (p1f p1t p2f p2t)
	(get-epsilon-enclosure-relevant-points obj radius)
      (poly
       (append
	(make-arc p1f p1t (x p1) (y p1))
	(list (l p1t p2t))	
	(make-arc p2t p2f (x p2) (y p2))
	(list (l p2f p1f)))))))


#| nicht notwendig (und auch nicht korrekt)...

(defmethod create-epsilon-enclosure ((obj geom-chain) (radius number))
  (with-slots (segments) obj
    (let ((lines nil)
	  (first-segment (first segments))
          (last-segment (first (last segments))))

      (mapc #'(lambda (i j)
		(multiple-value-bind (p1fi p1ti p2fi p2ti)
		    (get-epsilon-enclosure-relevant-points i radius)
		  (multiple-value-bind (p1fj p1tj p2fj p2tj)
		      (get-epsilon-enclosure-relevant-points j radius)		
		    (let ((li (l p1ti p2ti))
			  (lj (l p1tj p2tj)))
		      (cond ((crosses-p li lj)
			     (multiple-value-bind (ix iy)
				 (calculate-intersection-point li lj)
			       (let ((ip (p ix iy)))
				 (push (l p1ti ip) lines)
				 (push (l ip p2tj) lines))))
			    ((point-=-p p2ti p1tj)
			     (push (l p1ti p2tj) lines))
			    (t (push li lines)
			       (setf lines 
				 (append (reverse (make-arc p2ti p1tj (x (p2 i)) (y (p2 i)))) lines))
			       (push lj lines)))))))
	    segments (rest segments))
      
      (multiple-value-bind (p1f p1t p2f p2t)
	  (get-epsilon-enclosure-relevant-points last-segment radius)
	(setf lines 
	  (append (reverse (make-arc p2t p2f (x (p2 last-segment)) (y (p2 last-segment)))) lines)))
      
      (mapc #'(lambda (i j)
		(multiple-value-bind (p1fi p1ti p2fi p2ti)
		    (get-epsilon-enclosure-relevant-points i radius)
		  (multiple-value-bind (p1fj p1tj p2fj p2tj)
		      (get-epsilon-enclosure-relevant-points j radius)		
		    (let ((li (l p2fi p1fi))
			  (lj (l p2fj p1fj)))
		      (cond ((crosses-p li lj)
			     (multiple-value-bind (ix iy)
				 (calculate-intersection-point li lj)
			       (let ((ip (p ix iy)))
				 (push (l p2fi ip) lines)
				 (push (l ip p1fj) lines))))
			    ((point-=-p p1fi p2fj)
			     (push (l p2fi p1fj) lines))
			    (t (push li lines)
			       (setf lines 
				 (append (reverse (make-arc p1fi p2fj (x (p1 i)) (y (p1 i)))) lines))
			       (push lj lines)))))))
	    (reverse segments) (rest (reverse segments)))
      
      (multiple-value-bind (p1f p1t p2f p2t)
	  (get-epsilon-enclosure-relevant-points first-segment radius)
	(setf lines 
	  (append (reverse (make-arc p1f p1t (x (p1 first-segment)) (y (p1 first-segment)))) lines)))
      
      (reverse lines))))

|#       			    

;;;
;;;
;;;

(defmethod inside-epsilon-p ((obj1 geom-point) (obj2 geom-thing) radius &key (epsilon-a 1) (epsilon-b 1))
  (let ((radius (* radius radius)))
    (<= (distance-between obj1 obj2 :sqrt nil :sx epsilon-a :sy epsilon-b) radius)))

(defmethod inside-epsilon-p* ((x number) (y number) (obj geom-thing) radius &key (epsilon-a 1) (epsilon-b 1))
  (let ((radius (* radius radius)))
    (<= (distance-between-xy x y obj :sqrt nil :sx epsilon-a :sy epsilon-b) radius)))

;;;
;;;
;;;

(defmethod inside-epsilon-p ((obj1 geom-line) (obj2 geom-point) radius &key (epsilon-a 1) (epsilon-b 1))
  (let ((radius (* radius radius)))
    (and (<= (distance-between (p1 obj1) obj2 :sqrt nil :sx epsilon-a :sy epsilon-b) radius)
	 (<= (distance-between (p2 obj1) obj2 :sqrt nil :sx epsilon-a :sy epsilon-b) radius))))

(defmethod inside-epsilon-p ((obj1 geom-line) (obj2 geom-line) radius &key (epsilon-a 1) (epsilon-b 1))
  (let ((radius (* radius radius)))
    (and (<= (distance-between (p1 obj1) obj2 :sqrt nil :sx epsilon-a :sy epsilon-b) radius)
	 (<= (distance-between (p2 obj1) obj2 :sqrt nil :sx epsilon-a :sy epsilon-b) radius))))


(defmethod inside-epsilon-p ((obj1 geom-line) (obj2 geom-chain-or-polygon) radius &key (epsilon-a 1) (epsilon-b 1))
  (let ((radius (* radius radius)))
    (labels ((do-it (x1 y1 x2 y2 mindist)
	       (if (< (distance-between* x1 y1 x2 y2 :sqrt nil :sx epsilon-a :sy epsilon-b) mindist)
		   t
		 (let ((mx (/ (+ x1 x2) 2))
		       (my (/ (+ y1 y2) 2)))
		   (and (some #'(lambda (segment)
				  (<= (distance-between-point-and-line mx my
								       (x (p1 segment))
								       (y (p1 segment))
								       (x (p2 segment))
								       (y (p2 segment))
								       :sqrt nil
								       :sx epsilon-a :sy epsilon-b)
				      radius))
			      (segments obj2))
			(do-it x1 y1 mx my mindist)
			(do-it mx my x2 y2 mindist))))))

      (and (<= (distance-between (p1 obj1) obj2 :sqrt nil :sx epsilon-a :sy epsilon-b) radius)
	   (<= (distance-between (p2 obj1) obj2 :sqrt nil :sx epsilon-a :sy epsilon-b) radius)
	   (do-it (x (p1 obj1)) (y (p1 obj1))
		  (x (p2 obj1)) (y (p2 obj1))
		  (/ (length-of-line obj1) 20)))))) ; 20 Teilstrecken untersuchen!

;;;
;;;
;;;

(defmethod inside-epsilon-p ((obj1 geom-chain-or-polygon) (obj2 geom-point) radius &key (epsilon-a 1) (epsilon-b 1))
  (let ((radius (* radius radius)))
    (every #'(lambda (p)
	       (<= (distance-between p obj2 :sqrt nil :sx epsilon-a :sy epsilon-b) radius))
	   (point-list obj1))))

(defmethod inside-epsilon-p ((obj1 geom-chain-or-polygon) (obj2 geom-line) radius &key (epsilon-a 1) (epsilon-b 1))
  (let ((radius (* radius radius)))
    (every #'(lambda (p)
	       (<= (distance-between p obj2 :sqrt nil :sx epsilon-a :sy epsilon-b) radius))
	   (point-list obj1))))


(defmethod inside-epsilon-p ((obj1 geom-chain-or-polygon) (obj2 geom-chain-or-polygon) radius &key (epsilon-a 1) (epsilon-b 1))
  (every #'(lambda (s)
	     (inside-epsilon-p obj1 s radius :epsilon-a epsilon-a :epsilon-b epsilon-b))
	 (segments obj2)))

