;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: SQD; Base: 10 -*-

(in-package sqd)

(defun decode-digit (char)
  (let ((code (char-code char)))
    (if (<= 48 code 57)
	(- code 48)
      (if (<= 65 code 70)
	  (- code 55)
	(if (<= 97 code 102)
	    (- code 87)
	  'error)))))

(defun num-to-bin (zahl count &optional (res 0))  
  (if (zerop zahl)
      res
    (let ((mod (mod zahl 2)))
      (num-to-bin (floor zahl 2) (1- count)
		  (if (zerop mod)
		      res
		    (+ res (expt 2 (- count))))))))


(defun read-hex (string)  
  (when (> (length string) 2)
    (let ((afterpoint-digits 
	   (decode-digit (elt string 1)))
	  (beforepoint-digits
	   (decode-digit (elt string 2)))
	  (negative-p nil)
	  (n (length string)))
      
      (unless (or (eq afterpoint-digits 'error)
		  (eq beforepoint-digits 'error))
        
        (when (>= afterpoint-digits 8)
	  (decf afterpoint-digits 8)
	  (setf negative-p t))
        
        (when (= beforepoint-digits 15)	; !!!!! Fehler in den Daten ?
	  (setf beforepoint-digits 2))
        
        (let* ((beforepoint
	        (if (not (zerop beforepoint-digits))
		  (let ((*read-base* 16))
		    (read-from-string
		     (subseq string 3 
			     (+ 3 beforepoint-digits))))
		  0))
	       
	       (afterpoint
	        (if (not (zerop afterpoint-digits))
		  (num-to-bin 
		   (let ((*read-base* 16))
		     (read-from-string
		      (subseq string 
			      (+ 3 beforepoint-digits) 
			      (min n (+ 3 beforepoint-digits afterpoint-digits)))))
		   (* 4 afterpoint-digits))
		  0)))
	  (let ((res      
	         (+ beforepoint afterpoint)))
	    (if negative-p
	      (- res)
	      res)))))))



