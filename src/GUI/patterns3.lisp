;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: GUI; Base: 10 -*-

(in-package gui)

;;;
;;;
;;;

(defvar *arrays*)

(defvar *array-ring*)

(defconstant +grid+
    (make-array '(8 8)
		:initial-contents '((1 0 1 0 1 0 1 0)
				    (0 1 0 1 0 1 0 1)
				    (1 0 1 0 1 0 1 0)
				    (0 1 0 1 0 1 0 1)
				    (1 0 1 0 1 0 1 0)
				    (0 1 0 1 0 1 0 1)
				    (1 0 1 0 1 0 1 0)
				    (0 1 0 1 0 1 0 1))))

(defmethod calculate-gray-color ((obj gui-object))
  (list 0 0 0))

(defun random-gray-color ()
  (let ((col
	 (+ 0.5 (/ (random 31) 100))))
    (list col col col)))

(defun random-color ()
  (let ((col
	 (+ 0.5 (/ (random 31) 100))))
    (list col 0 col)))

(defmethod calculate-color ((obj gui-enclosure))
  (random-gray-color))

(defmethod calculate-color-for-drawn-enclosure ((obj gui-drawn-enclosure))
  (random-gray-color))

(defmethod calculate-color-for-inner-enclosure ((obj gui-polygon))
  (random-color))

(defmethod calculate-color-for-outer-enclosure ((obj gui-polygon))
  (random-color))

(defmethod calculate-color-for-epsilon-enclosure ((obj gui-query-object))
  (random-color))

(defun make-grid-in (color &optional (background-ink +transparent-ink+))
  (make-rectangular-tile   
   (make-pattern +grid+
		 (list color
		       background-ink))
   8 8))

(defun get-gray-pattern-in (col &key (opaque-p *opaque-enclosures-p*)
				     pattern-id)
  (get-pattern-in (make-gray-color (first col)) :opaque-p opaque-p :pattern-id pattern-id))


(defun get-pattern-in (col &key (opaque-p *opaque-enclosures-p*) 
				pattern-id)
  (if opaque-p
      col
    (let ((array
	   (if pattern-id
	       (let ((pattern
		      (nth pattern-id *arrays*)))
		 (setf *array-ring*
		   (nthcdr (1+ (position pattern *array-ring*))
			   *array-ring*))
		 pattern)
	     (pop *array-ring*))))
      (values 
       (make-pattern-in col
			array
			opaque-p)
       (position array *arrays*)))))


(defun get-next-pattern-id ()
  (position (first *array-ring*) *arrays*))

(defun make-pattern-in (col array opaque-p)
  (make-rectangular-tile   
   (make-pattern array
		 (list (if opaque-p
			   +background-ink+
			 +transparent-ink+)
		       col))
   8 8))

(defun make-inverse-pattern-in (col array opaque-p)
  (make-rectangular-tile   
   (make-pattern 
    (let* ((x (first (array-dimensions array)))
	   (y (second (array-dimensions array)))
	   (array2 (make-array (list x y))))
      (dotimes (x x)
	(dotimes (y y)
	  (setf (aref array2 x y)
	    (- 1 (aref array x y)))))
      array2)
    (list (if opaque-p
	      +background-ink+
	    +transparent-ink+)
	  col))
   8 8))

(defun inverse-pattern (pattern col opaque-p)
  (make-inverse-pattern-in 
   col
   (slot-value (slot-value pattern 'design) 'array)
   opaque-p))

(defparameter *arrays*
    (list 	
     (make-array '(8 8)
		 :initial-contents
		 '((1 0 0 0 1 0 0 0)
		   (0 1 0 0 0 1 0 0)
		   (0 0 1 0 0 0 1 0)
		   (0 0 0 1 0 0 0 1)
		   (1 0 0 0 1 0 0 0)
		   (0 1 0 0 0 1 0 0)
		   (0 0 1 0 0 0 1 0)
		   (0 0 0 1 0 0 0 1)))
     
     (make-array '(8 8)
		 :initial-contents
		 '((1 0 0 0 1 0 0 0)
		   (0 1 0 0 0 1 0 0)
		   (0 0 1 0 0 0 1 0)	
		   (0 0 0 1 0 0 0 1)
		   (0 0 1 0 0 0 1 0)
		   (0 1 0 0 0 1 0 0)
		   (1 0 0 0 1 0 0 0)
		   (0 0 0 1 0 0 0 1)))
     
     (make-array '(8 8)
		 :initial-contents
		 '((0 0 0 1 0 0 0 1)
		   (0 0 1 0 0 0 1 0)
		   (0 1 0 0 0 1 0 0)
		   (1 0 0 0 1 0 0 0)
		   (0 0 0 1 0 0 0 1)
		   (0 0 1 0 0 0 1 0)	
		   (0 1 0 0 0 1 0 0)
		   (1 0 0 0 1 0 0 0)))
     
     (make-array '(8 8)
		 :initial-contents
		 '((1 0 0 0 1 0 0 0)
		   (1 0 0 0 1 0 0 0)
		   (0 1 0 0 0 1 0 0)
		   (0 1 0 0 0 1 0 0)			  
		   (0 0 1 0 0 0 1 0)
		   (0 0 1 0 0 0 1 0)
		   (0 0 0 1 0 0 0 1)
		   (0 0 0 1 0 0 0 1)))
     
     (make-array '(8 8)
		 :initial-contents
		 '((0 0 0 1 0 0 0 1)
		   (0 0 0 1 0 0 0 1)
		   (0 0 1 0 0 0 1 0)
		   (0 0 1 0 0 0 1 0)
		   (0 1 0 0 0 1 0 0)
		   (0 1 0 0 0 1 0 0)			  
		   (1 0 0 0 1 0 0 0)
		   (1 0 0 0 1 0 0 0)))

     (make-array '(8 8)
		 :initial-contents
		 '((0 0 0 1 0 0 0 0)
		   (0 0 1 0 1 0 0 0)
		   (0 1 0 0 0 1 0 0)
		   (1 0 0 0 0 0 1 0)
		   (0 0 0 0 0 1 0 0)
		   (0 0 0 0 1 0 0 0)
		   (0 0 0 1 0 0 0 0)
		   (0 0 1 0 0 0 0 0)))
     
     (make-array '(8 8)
		 :initial-contents
		 '((0 0 0 0 1 0 0 0)
		   (1 0 0 0 0 0 0 0)
		   (0 0 0 0 0 1 0 0)
		   (0 1 0 0 0 0 0 0)
		   (0 0 0 0 0 0 1 0)
		   (0 0 1 0 0 0 0 0)
		   (0 0 0 0 0 0 0 1)
		   (0 0 0 1 0 0 0 0)))))



#|

(defun generate-random-array ()
  (labels ((check-neighbourhood (arr x y radius)
	     (dotimes (xi (1+ (* 2 radius)))
	       (dotimes (yi (1+ (* 2 radius)))
		 (let ((x (mod (+ x (- xi radius)) 8))
		       (y (mod (+ y (- yi radius)) 8)))
		   (unless (zerop (aref arr x y))
		     (return-from check-neighbourhood nil)))))
	     t))
    (let ((arr (make-array '(8 8) :initial-element 0))
	  (counter 0))
      (loop while (< counter 4) do
	    (let ((x (random 8))
		  (y (random 8)))
	      (when (check-neighbourhood arr x y 2)
		(setf (aref arr x y) 1)
		(incf counter))))
      arr)))

(defparameter *arrays*
    (loop as i from 1 to 100 collect (generate-random-array)))

|#


(defparameter *array-ring*
    (let ((x (copy-list *arrays*)))
      (setf (rest (last x)) x)))
