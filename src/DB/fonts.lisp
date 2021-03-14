;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: FONTS; Base: 10 -*-

(cl:in-package fonts)

;;;
;;;
;;;

(defparameter *char-to-font-function*
    (make-hash-table))

(defmacro compile-character (char)
  (princ char) (terpri)
  (let* ((namex (first char))
	 (name
	  (typecase namex
	    (string 
	     (elt namex 0))
	    (character namex)))
	 (vectors (second char))
	 (measures (third char))
	 (x-measures (first measures))
	 #| (y-measures (second measures)) |#
	 (xstart (first x-measures))
	 (xend (second x-measures))
	 
	 #| (ystart (first y-measures))
	 (yend (second y-measures)) |#
	 
	 (delta (abs (- xstart xend)))
	 (code nil)
	 
	 (dummy-var1 (gensym))
	 (dummy-var2 (gensym))
	 (dummy-var3 (gensym)))

    (dolist (line vectors)
      (let* ((p1 (first line))
	     (p2 (second line))
	     (x1 (- (first p1) xstart))
	     (x2 (- (first p2) xstart))
	     (y1  (- (second p1)))
	     (y2  (- (second p2))))
	(push 	 
	 `(draw-line*
	   stream
	   (+ xoff ,x1) (+ yoff ,y1) (+ xoff ,x2) (+ yoff ,y2))
	 code)))
    
    
    `(progn
       (setf (gethash ,(char-code name) *char-to-font-function*)
	 #'(lambda (stream xoff yoff)
	     (let ((,dummy-var1 stream)
		   (,dummy-var2 xoff)
		   (,dummy-var3 yoff))
	       (declare (ignore ,dummy-var1 ,dummy-var2 ,dummy-var3))
	       (progn
		 ,@code
		 ,delta)))))))

(defmacro compile-font ()
  (let ((code nil))
    
    (dolist (char 		
		(eval-when (:compile-toplevel :load-toplevel :execute)
		  (with-open-file (stream
                                   "visco:fonts;char3n"
				   :direction :input)
		    (read stream))))

      (push `(compile-character ,char) code))     
    `(progn 
       ,@code)))


(compile-font)

(defparameter *y-to-x* 1.5)


(defun draw-vector-text (string stream)
  (let ((x 0))
    (with-scaling (stream 1.0 *y-to-x*)
      (dotimes (i (length string))
	(let* ((char-code (char-code (elt string i)))
	       (fn (gethash char-code *char-to-font-function*)))
	  (incf x 0.3)
	  (incf x
		(if fn
		    (funcall
		     fn
		     stream x 1.2)
		  (funcall (gethash (char-code #\space)
				    *char-to-font-function*)
			   stream x 1.2))))))))

