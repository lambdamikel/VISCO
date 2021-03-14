;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: CL-USER; Base: 10 -*-

(in-package cl-user)

(defconstant +vector-size+ 100000)
(defconstant +hash-table-size+ 100000)
(defconstant +rehash-size+ 40000)

(defconstant +pi/2+ (/ pi 2))
(defconstant +2pi+ (* 2 pi))

(defconstant +epsilon+ 1e-4)

(defun yes (&rest args)
  (declare (ignore args))
  t)

(defun no (&rest args)
  (declare (ignore args))
  nil)

(defun get-lambda-args (fn)
  (nreverse
   (mapcar #'(lambda (arg)
	       (if (consp arg)
		   (first arg)
		 arg))
	   (set-difference
            #+:allegro
	    (excl:arglist fn)
            #+:mcl
	    (arglist fn)
	    '(&rest &optional &key)))))


(defun set-equal (a b)
  (and (every #'(lambda (i)
		  (member i b))
	      a)
       (every #'(lambda (i)
		  (member i a))
	      b)))

(defun set-disjoint (a b)
  (and (not (some #'(lambda (i)
		      (member i b))
		  a))
       (not (some #'(lambda (i)
		      (member i a))
		  b))))       

(defmacro zerop-eps (a)
  `(=-eps 0 ,a))

(defmacro =-eps (a b &optional (epsilon +epsilon+))
  `(<= (abs (- ,a ,b)) ,epsilon))

(defmacro <=-eps (a b &optional (epsilon +epsilon+))
  `(<=  ,a (+ ,b ,epsilon)))

(defmacro >=-eps (a b &optional (epsilon +epsilon+))
  `(>=  ,a (- ,b ,epsilon)))

(defun tree-reverse (liste)
  (labels ((do-it (liste akku)
	     (cond ((null liste) akku)
		   (t (let ((first (first liste)))
			(do-it (rest liste)
			  (cons
			   (if (listp first)
			       (do-it first nil) first)
			   akku)))))))
    (do-it liste nil)))

(defun tree-remove (item tree)
  (cond ((not (consp tree))
	 (if (eq item tree) nil tree))
	(t
	 (let ((car (tree-remove item (car tree)))
	       (cdr (tree-remove item (cdr tree))))
	   (if car
	       (cons car cdr)
	     cdr)))))


(defun transform-xy-list (xylist)
  (loop for x in xylist by #'cddr 
      for y in (cdr xylist) by #'cddr 
      collect (list x y)))

(defmacro => (i j)
  `(or (not ,i) ,j))

(defun <=> (i j)
  (eq i j))

(defun intervall-intersects-p (a b c d)
  (not (or (< b c) 
	   (< d a))))

(defun lies-in-circle-intervall-p (x a b)
  (if (= a b) 
      t
    (if (<= a b)
	(<= a x b)
      (or (>= b x) (>= x a)))))

(defun circle-intervall-intersects-p (a b c d)
  (or (lies-in-circle-intervall-p b c d)
      (lies-in-circle-intervall-p d a b)))

(defun circle-intervall-length (a b &optional (modulo +2pi+))
  (if (<= a b)
      (- b a)
    (+ (- modulo a) b)))

(defun rad-to-deg (phi)
  (* 180 (/ phi pi)))

(defun deg-to-rad (phi)
  (* pi (/ phi 180)))


(defmacro pushend (obj list)
  `(setf ,list (nconc ,list (list ,obj))))

(defmacro mynconc (lista listb)
  `(setf ,lista 
     (nconc ,lista ,listb)))

(defmacro my-format (stream indent-level string &rest args)
  `(progn 
     (dotimes (i (* 7 ,indent-level))
       (princ " " ,stream))
     (format ,stream ,string ,@args)))

(defmacro my-read (stream)
  #+:allegro
  `(read ,stream)
  #+:mcl
  `(read-from-string (read-line ,stream)))

#+:allegro
(defun circle-subseq (list from to)
  (let* ((copy (copy-list list)))
    (setf (rest (last copy)) copy)
    (subseq copy from to)))

#+:mcl
(defun circle-subseq (list from to)
  (let* ((copy (copy-list list)))
    (setf (rest (last copy)) copy)
    (let ((start copy))
      (loop repeat from do
            (setf start (cdr start)))
      (loop repeat (- to from)
	  collect (car start) do
            (setf start (cdr start))))))

#+:allegro
(defun recode-german-characters (string)

  (nsubstitute (character 228) (character 204) string) ; dos aeh -> unix aeh
  (nsubstitute (character 246) (character 224) string) ; dos oeh -> unix oeh
  (nsubstitute (character 252) (character 201) string) ; dos ueh -> unix ueh
  
  (nsubstitute (character 196) (character 216) string) ; dos AEH -> unix AEH
  (nsubstitute (character 214) (character 231) string) ; dos OEH -> unix OEH
  (nsubstitute (character 220) (character 232) string) ; dos UEH -> unix UEH
  
  (nsubstitute (character 223) #\· string) ; dos sz -> unix sz
  
  (nsubstitute (character 228) (character 132) string) ; siemens aeh -> unix aeh ; dxf
  (nsubstitute (character 246) (character 148) string) ; siemens oeh -> unix oeh ; dxf
  (nsubstitute (character 252) (character 129) string) ; siemens ueh -> unix ueh ; dxf
  
  (nsubstitute (character 214) (character 153) string) ; siemens OEH -> unix OEH ; andere Code unbekannt! ; dxf
  
  (nsubstitute (character 228) (character #xbf) string) ; siemens aeh -> unix aeh ; sqd
  (nsubstitute (character 246) (character #xd0) string) ; siemens oeh -> unix oeh ; sqd
  (nsubstitute (character 252) (character #xdd) string) ; siemens ueh -> unix ueh ; sqd
  
  (nsubstitute (character 214) (character #xf0) string) ; siemens OEH -> unix OEH ; andere Codes sind unbekannt! ; dxf

  (nsubstitute (character 223) (character #xc5) string) ; dos sz -> unix sz ; dxf

  string)

#+:mcl
(defun recode-german-characters (string)
  string)

;;;
;;;
;;;

#|
(defvar *read-objects* 
    (make-hash-table :test #'eql
		     :size +hash-table-size+
		     :rehash-size +rehash-size+))

|#

(defvar *read-objects*
  (make-array +vector-size+ :initial-element nil))

(defvar *writen-objects* 
    (make-hash-table :test #'eql
		     :size +hash-table-size+
		     :rehash-size +rehash-size+))

;;;
;;;
;;;

#|
(defun store (io-id obj)
  (setf (gethash io-id *read-objects*) obj))

(defun retrieve (io-id)
  (gethash io-id *read-objects*))
|#

(defvar *io-id-counter* 0)

(defun get-io-id-for (object)
  (gethash object *writen-objects*))

(defun get-io-id ()
  (incf *io-id-counter*))

(defun store (io-id obj)
  (setf *io-id-counter* (max io-id *io-id-counter*)) 
  (setf (svref *read-objects* io-id) obj))

(defun retrieve (io-id)
  (svref *read-objects* io-id))

#|
(defun reset ()
  (setf *io-id-counter* 0)
  (clrhash *writen-objects*)
  (clrhash *read-objects*))
|#

(defun reset ()
  (setf *io-id-counter* 0)
  (clrhash *writen-objects*)
  (setf *read-objects* (make-array +vector-size+ :initial-element nil)))

;;;
;;;
;;;

(defclass persistent-object ()
  ())

(defmethod write-object-constructor ((obj persistent-object) stream)
  (declare (ignore stream))
  nil)

(defmethod write-object-initializer ((obj persistent-object) stream)
  (declare (ignore stream))
  nil)

(defmethod fill-persistent-object ((obj persistent-object) stream)
  (declare (ignore stream))
  nil)


;;; verhindere, da"s initialize-instance fuer Subklassen aufgerufen wird!

(defmethod initialize-instance :around ((obj persistent-object) 
					&rest initargs
					&key (dont-initialize nil))
  (if (not dont-initialize)
      (progn
	#+:allegro
        (clos::validate-make-instance-initargs (find-class (type-of obj))
                                               initargs) 
	(call-next-method))		; normale Initialisierung
    (apply #'shared-initialize obj t initargs)))

(defmethod initialize-loaded-persistent-object ((obj persistent-object))
  nil)

;;;
;;;
;;;

(defconstant +already-present-marker+ #\!)
(defconstant +string-marker+ #\s)
(defconstant +array-marker+ #\a)
(defconstant +list-marker+ #\l)
(defconstant +number-marker+ #\n)
(defconstant +symbol-marker+ #\i)
(defconstant +object-marker+ #\o)
(defconstant +otherwise-marker+ #\?)
(defconstant +unbound-marker+ #\*)
(defconstant +section-marker+ #\+)

;;;
;;;
;;;

(defmacro with-only-once-constructed-object ((object marker stream) &body body)
  `(unless (gethash ,object *writen-objects*)
     (let ((io-id (get-io-id)))
       (setf (gethash ,object *writen-objects*) io-id)
       (format ,stream "~A~A~%" ,marker io-id)
       ,@body)))

(defmethod write-constructor ((object t) stream)
  (declare (ignore stream))
  nil)

(defmethod write-constructor ((object cons) stream)
  (with-only-once-constructed-object (object +list-marker+ stream)
    (format stream "~A~%" (length object))
    (dolist (obj object)
      (write-constructor obj stream))))

(defmethod write-constructor ((object string) stream)
  (declare (ignore stream))
  nil)

(defmethod write-constructor ((object array) stream)
  (with-only-once-constructed-object (object +array-marker+ stream)
    (format stream "~A~%" (array-dimensions object))
    (dotimes (i (array-total-size object))
      (write-constructor (row-major-aref object i) stream))))

(defmethod write-constructor ((object persistent-object) stream)
  (with-only-once-constructed-object (object +object-marker+ stream)
    (format stream "~S~%" (type-of object))
    (write-object-constructor object stream)))

;;;
;;;
;;;

(defmacro with-complex-object-header ((id stream) &body body)
  `(progn 
     (format ,stream "~A~%" ,id)
     ,@body))

(defun write-referencer (object stream)
  (typecase object
    ((and 
      (or cons array persistent-object) 
      (not string))
     (format stream "~A~A~%" +object-marker+ (get-io-id-for object)))
    (otherwise
     (typecase object	       
       (string (format stream "~A~A~%" +string-marker+ object))
       (number (format stream "~A~A~%" +number-marker+ object))
       (symbol (format stream "~A~S~%" +symbol-marker+ object))
       (otherwise (format stream "~A~S~%" +otherwise-marker+ object))))))

(defmethod write-initializer ((object cons) id stream)
  (with-complex-object-header (id stream)
    (dolist (obj object)
      (write-referencer obj stream))))

(defmethod write-initializer ((object array) id stream)
  (with-complex-object-header (id stream)
    (dotimes (i (array-total-size object))
      (write-referencer (row-major-aref object i)
			stream))))

(defmethod write-initializer ((object persistent-object) id stream)
  (with-complex-object-header (id stream)
    (write-object-initializer object stream)))

;;;
;;;
;;;


(defmethod fill-object ((object cons) stream)
  (let ((length (first object)))
    (setf (first object)
      (read-value stream))
    (setf (rest object)
      (loop as i from 1 to (1- length)
	  collect (read-value stream))))
  object)

(defmethod fill-object ((object array) stream)
  (dotimes (i (array-total-size object))
    (setf (row-major-aref object i)
      (read-value stream)))
  object)

(defmethod fill-object ((object persistent-object) stream)
  (fill-persistent-object object stream)
  object)

;;;
;;;
;;;

(defun read-value (stream)
  (let ((marker (read-char stream)))
    (cond ((char= marker +object-marker+)
	   (values 
	    (retrieve (parse-integer (read-line stream)))
	    nil))
	  ((char= marker +symbol-marker+)
	   (values 
	    (my-read stream)
	    nil))
	  ((char= marker +string-marker+)
	   (values
	    (read-line stream)
	    nil))
	  ((char= marker +number-marker+)
	   (values 
	    (my-read stream)
	    nil))
	  ((char= marker +otherwise-marker+)
	   (values 
	    (my-read stream)
	    nil))
	  ((char= marker +unbound-marker+)
	   (values nil t))
	  (t (error "Bad marker ~A in read-value!" marker)))))

(defun construct-object (stream)
  (loop 
    (let ((marker (read-char stream)))
      (if (or (char= marker +section-marker+)
	      (eq marker 'eof))
	  (return)
	(store (parse-integer (read-line stream))
	       (cond ((char= marker +list-marker+)
		      (list (read-from-string (read-line stream))))
		     ((char= marker +array-marker+)
		      (make-array (my-read stream)))
		     ((char= marker +object-marker+)
		      (make-instance (my-read stream)
                        :allow-other-keys t 
			:dont-initialize t))))))))

(defun initialize-object (stream)
  (loop 
    (let ((io-id (read-line stream nil 'eof)))
      (if (eq io-id 'eof)
	  (return)
	(fill-object (retrieve (parse-integer io-id)) stream)))))

;;;
;;;
;;;

(defmacro defpersistentclass (&rest rest)
  `(progn ,@(let* ((name (first rest))
		   (superclasses (append (second rest) '(persistent-object)))
		   (body (third rest))
		   (slotnames (mapcar #'first body)))
	      (list 
	       `(defclass ,name ,superclasses 
		  ,(loop for slotspec in body collect
			 (remove :not-persistent slotspec)))

	       `(defmethod write-object-constructor ((obj ,name) stream)
                  (declare (ignore-if-unused stream))
		  (with-slots ,slotnames obj
		    ,@(mapcan #'(lambda (slot)
				  (let ((name (first slot)))
				    (unless (member :not-persistent slot)
				      `((when (slot-boundp obj ',name)
					  (write-constructor ,name stream))))))
			      (reverse body)))
		  (call-next-method))

	       `(defmethod write-object-initializer ((obj ,name) stream)
                  (declare (ignore-if-unused stream))
		  (with-slots ,slotnames obj
		    ,@(mapcan #'(lambda (slot)
				  (let ((name (first slot)))
				    (unless (member :not-persistent slot)
				      `((if (slot-boundp obj ',name)
					    (write-referencer ,name stream)
					  (princ +unbound-marker+ stream))))))
			      (reverse body)))
		  (call-next-method))
	       
	       `(defmethod fill-persistent-object ((obj ,name) stream)
                  (declare (ignore-if-unused stream))
		  (let (val unbound)
		    (declare (ignore-if-unused val unbound))
		    (with-slots ,slotnames obj
		      ,@(mapcan #'(lambda (slot)
				    (let ((name (first slot)))
				      (unless (member :not-persistent slot)
					`((multiple-value-setq (val unbound)
					    (read-value stream))
					  (unless unbound
					    (setf ,name val))))))
				(reverse body)))
		    (call-next-method)))))))

;;;
;;;
;;;

(defun write-section-separator (stream)
  (format stream "~A" +section-marker+))

(defun make-object-persistent (obj fn)
  (with-open-file (stream fn :direction :output :if-exists :supersede
		   :if-does-not-exist :create)
    (let ((*package* (find-package "CL-USER")))
      (reset)
      (write-constructor (list obj) stream)
      (write-section-separator stream)
      (maphash #'(lambda (key value)
		   (write-initializer key value stream))
	       *writen-objects*))))

(defun load-persistent-object (fn)
  (with-open-file (stream fn :direction :input)
    (let ((*package* (find-package "CL-USER")))
      (reset)
      (construct-object stream)
      (initialize-object stream)
      (dotimes  (i (+ 2 *io-id-counter*))
        (let ((obj (svref *read-objects* i)))
          (when (typep obj 'persistent-object)
            (initialize-loaded-persistent-object obj))))
      (first (retrieve 1)))))

;;;
;;;
;;;

#|

(defpersistentclass test ()
  ((a :accessor a :initarg :a)
   (b :accessor b :initarg :b)))

(defpersistentclass test2 (test)
  ((c :accessor c :initarg :c)))


(setf x (make-instance 'test
	  :a (list 1 2 3)))


(setf y (make-instance 'test :a x
		       :b
		       (make-array '(10))))

(setf z (make-instance 'test2 :c (list x y
				       (make-array '(3)
						   :initial-contents (list x y x)))))


(make-persistent (vector x y z (list x (vector x z y) x z)) "test")

|#
