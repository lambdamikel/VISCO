;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: GEOMETRY; Base: 10 -*-

(in-package geometry)

(define-condition geom-error (simple-error) 
  ((descr :initarg :descr :accessor descr)))


(defgeneric mark-obj (obj value)
  (:documentation "Mark object with value."))

(defgeneric primary-p (obj)
  (:documentation "Returns T, if obj is no part of any other object."))

(defgeneric component-p (obj1 obj2)
  (:documentation "If obj1 is a direct or indirect part of obj2, returns T."))

(defgeneric get-all-masters (obj)
  (:documentation "Returns all objects which have obj as a component object (part)."))

(defgeneric get-direct-components (obj)
  (:documentation "Returns all objects which are a direct component of  obj."))



(defgeneric common-root-p (obj1 obj2)
  (:documentation "Returns T if obj1 and obj2 are both components of the same object."))

(defgeneric point-=-p (point1 point2)
  (:documentation "Returns T if point1 and point2 have the same position."))

(defgeneric line-=-p (line1 line2)
  (:documentation "Returns T if line1 and line2 are congruent."))

(defgeneric point->=-p (point1 point2)
  (:documentation "Returns T if point1 is right and above point2."))

(defgeneric point-<=-p (point1 point2)
  (:documentation "Returns T if point1 is left and below point2."))

(defgeneric x (point)
  (:documentation "Returns the x position of point with respect to *matrix*"))

(defgeneric y (point)
  (:documentation "Returns the y position of point with respect to *matrix*"))

(defgeneric delete-object (obj &key)
  (:method-combination progn)
  (:documentation "Delete object from *objects-with-relations* and all stored relations."))

#|
(defgeneric translate (matrix xtrans ytrans)
  (:documentation "Translate matrix by (xtrans,ytrans)."))

(defgeneric scale (matrix sx sy)
  (:documentation "Scale matrix by (sx,sy)."))

(defgeneric rotate (matrix rot)
  (:documentation "Rotate matrix by rot."))
|#

(defgeneric compose (matrix1 matrix2)
  (:documentation "Multiply matrix1 with matrix2. Returns the modified matrix1."))



(defgeneric joins-p (line1 line2)
  (:documentation "Returns T, if line1 and line2 have a common point (point-=-p)."))

(defgeneric parallel-p (line1 line2)
  (:documentation "Determine whether line1 and line2 are parallel."))

(defgeneric upright-p (line1 line2)
  (:documentation "Determine whether line1 and line2 are upright."))

(defgeneric ccw (point1 point2 point3)
  (:documentation "Counterclockwise way from point1 over point2 to point3. See Sedgewick for details!"))

#|
(defgeneric angle-between (line1 line2)
  (:documentation "Calculate the angle between line1 and line2."))
|#

(defgeneric distance-between (obj1 obj2 &key sqrt sx sy)
  (:documentation "Calculate the smallest distance between obj1 and obj2."))

(defgeneric global-orientation (line)
  (:documentation "Calculate the angle between the x-axis and the line."))

(defgeneric calculate-intersection-point (line1 line2)
  (:documentation "Returns (x,y) intersection point (if a zero dimensional intersection between
line1 and line2 holds)."))

(defgeneric calculate-intersection-line (line1 line2)
  (:documentation "Returns (x1,y1,x2,y2) intersection line (if a one dimensional intersection between
line1 and line2 holds)."))

(defgeneric calculate-area (polygon)
  (:documentation "Calulates the area of the polygon."))


(defgeneric make-point (class-symbol x y &rest initargs)
  (:documentation "Make a point of class class-symbol."))

(defgeneric make-line (class-symbol p1 p2 &rest initargs)
  (:documentation "Make a line of class class-symbol."))

(defgeneric make-chain (class-symbol segment-list &rest initargs)
  (:documentation "Make a chain of class class-symbol."))

(defgeneric make-polygon (class-symbol segment-list &rest initargs)
  (:documentation "Make a polygon of class class-symbol."))

(defgeneric make-aggregate (class-symbol part-list &rest initargs)
  (:documentation "Make a aggregate of class class-symbol."))




;;;
;;; Various Spatial Relations for Polygons, Lines and Points
;;;

(defconstant +big-int+ 10000)

(defparameter *matrix* nil)

(defparameter *mark-counter* 0)

(defun get-new-mark ()
  (incf *mark-counter*))

;;;
;;;
;;;

(defpersistentclass bounding-box-mixin () ; abstrakt
  ((pmin :initarg :pmin :writer (setf pmin) :initform nil)
   (pmax :initarg :pmax :writer (setf pmax) :initform nil) 

   (pcenter :initarg :pcenter :writer (setf pcenter) :initform nil)
   (radius :initarg :radius :writer (setf radius) :initform nil)

   (last-trafo-id :accessor last-trafo-id :initarg :last-trafo-id 
		  :initform (if *matrix* (trafo-id *matrix*) -2))
   (bounding-box-p :initform t :initarg :bounding-box-p :accessor bounding-box-p)))

(defpersistentclass bounding-box (bounding-box-mixin) ; instantiierbar, aber NICHT TRANSFORMIERBAR !!!
  ((pmin :reader pmin)
   (pmax :reader pmax)
   (pcenter :reader pcenter)
   (radius :reader radius)))

;;;
;;;
;;;

(defvar *id-counter* 0)

(defpersistentclass geom-thing ()	; abstrakt
  ((part-of :initform nil :initarg :part-of :accessor part-of)
   (id :accessor id :initform (incf *id-counter*))
   
   (class-of-internal-points :accessor class-of-internal-points 
			     :initarg :class-of-internal-points 
			     :initform 'geom-point)   
   (affected-by-matrix-p :accessor affected-by-matrix-p :initform t :initarg :affected-by-matrix-p)
   ;;; bei Punkten: T <=> nicht von Matrix beruehrt
   ;;; bei anderen Objekten: entscheidet, ob bei Bedarf neu erzeugte berechnete Punkte (centroid obj)
   ;;; das Flag gesetzt bekommen oder nicht
   
   (check-p :accessor check-p :initarg :check-p :initform t)
   
   (bound-to :accessor bound-to :initform nil)
   
   (cp-flag :accessor cp-flag :initform nil)
   (mark-value :accessor mark-value :initform nil)))


(defpersistentclass geom-point (geom-thing)
  ((x :writer (setf x) :initarg :x)
   (y :writer (setf y) :initarg :y)
   
   (centroid-of :accessor centroid-of :initarg :centroid-of :initform nil)
   (pcenter-of :accessor pcenter-of :initarg :pcenter-of :initform nil)
   (p1-of :accessor p1-of :initarg :p1-of :initform nil)
   (p2-of :accessor p2-of :initarg :p2-of :initform nil)))


(defmethod pcenter ((obj geom-point))
  obj)


(defpersistentclass geom-line (geom-thing bounding-box-mixin)
  ((p1 :initarg :p1 :accessor p1)
   (p2 :initarg :p2 :accessor p2)   
   
   (point-list :accessor point-list)
   (centroid-p :initform t :initarg :centroid-p :accessor centroid-p)
   (centroid :initform nil :initarg :centroid :writer (setf centroid))))


(defpersistentclass geom-chain-or-polygon (geom-thing bounding-box-mixin)
  ((segments :initarg :segments :accessor segments)   
   (centroid :initform nil :initarg :centroid :writer (setf centroid))
   (centroid-p :initform t :initarg :centroid-p :accessor centroid-p)

   (point-list :accessor point-list :initarg :point-list)))


(defpersistentclass geom-polygon (geom-chain-or-polygon)
  ())


(defpersistentclass geom-chain (geom-chain-or-polygon)
  ((p1 :initarg :p1 :accessor p1)
   (p2 :initarg :p2 :accessor p2)))


(defpersistentclass geom-aggregate (geom-thing bounding-box-mixin)
  ((has-parts :initarg :has-parts :accessor has-parts)
   (centroid-p :initform t :initarg :centroid-p :accessor centroid-p)
   (centroid :initform nil :initarg :centroid :writer (setf centroid))))

;;;
;;;
;;;

(defmethod trafo-id ((obj null))
  -1)

(defmethod pmin ((obj bounding-box-mixin))
  (with-slots (pmin last-trafo-id) obj
    (unless (and pmin (= last-trafo-id (trafo-id *matrix*)))
      (recalculate-bounding-box obj))
    pmin))

(defmethod pmax ((obj bounding-box-mixin))
  (with-slots (pmax last-trafo-id) obj
    (unless (and pmax (= last-trafo-id (trafo-id *matrix*)))
      (recalculate-bounding-box obj))
    pmax))

(defmethod pcenter ((obj bounding-box-mixin))
  (with-slots (pcenter last-trafo-id) obj
    (unless (and pcenter (= last-trafo-id (trafo-id *matrix*)))
      (recalculate-bounding-box obj))
    pcenter))

(defmethod radius ((obj bounding-box-mixin))
  (with-slots (radius last-trafo-id) obj
    (unless (and radius (= last-trafo-id (trafo-id *matrix*)))
      (recalculate-bounding-box obj))
    radius))

;;;
;;;
;;;

(defmethod bb-width ((obj bounding-box-mixin))
  (- (x (pmax obj)) (x (pmin obj))))

(defmethod bb-height ((obj bounding-box-mixin))
  (- (y (pmax obj)) (y (pmin obj))))

;;;
;;;
;;;

(defun check-for-centroid (obj)
  (with-slots (centroid) obj
    (unless centroid
      (calculate-centroid obj))
    centroid))

(defmethod centroid ((obj geom-line))
  (check-for-centroid obj))

(defmethod centroid ((obj geom-chain-or-polygon))
  (check-for-centroid obj))

(defmethod centroid ((obj geom-aggregate))
  (check-for-centroid obj))

;;;
;;;
;;;

(defmethod print-object ((obj geom-thing) stream) 
  (format stream "#<~A ~A>"
	  (type-of obj)
	  (id obj)))

;;;
;;;
;;;

(defmethod mark-object ((obj geom-thing) value)
  (setf (mark-value obj) value))

;;;
;;;
;;;

(defmethod point-=-p ((p1 geom-point) (p2 geom-point))
  (or (eq p1 p2)
      (and (=-eps (x p1) (x p2))
	   (=-eps (y p1) (y p2)))))

(defmethod point->=-p ((p1 geom-point) (p2 geom-point))
  (and (>=-eps (x p1) (x p2))
       (>=-eps (y p1) (y p2))))

(defmethod point-<=-p ((p1 geom-point) (p2 geom-point))
  (and (<=-eps (x p1) (x p2))
       (<=-eps (y p1) (y p2))))

;;;
;;;
;;;

(defmethod line-=-p ((l1 geom-line) (l2 geom-line))
  (or (eq l1 l2)
      (and (point-=-p (p1 l1) (p1 l2))
	   (point-=-p (p2 l1) (p2 l2)))
      (and (point-=-p (p1 l1) (p2 l2))
	   (point-=-p (p2 l1) (p1 l2)))))

;;;
;;;
;;;


(defmethod x ((point geom-point))    
  (with-slots (x y affected-by-matrix-p) point
    (if (and *matrix* affected-by-matrix-p)
	(with-slots (a b tx) *matrix*
	  (+ (* a x) (* b y) tx))
      x)))

(defmethod y ((point geom-point))    
  (with-slots (x y affected-by-matrix-p) point
    (if (and *matrix* affected-by-matrix-p)
	(with-slots (c d ty) *matrix*
	  (+ (* c x) (* d y) ty))
      y)))

;;;
;;;
;;;

(defvar *trafo-id* 0)

(defpersistentclass matrix ()
  ((trafo-id :accessor trafo-id :initform (incf *trafo-id*))
					; z.B. muss die BB nach Rotationen
					; neuberechnet werden (=> Id aendert sich)
   (a :accessor a :initarg :a :initform 1)
   (b :accessor b :initarg :b :initform 0)
   (tx :accessor tx :initarg :tx :initform 0)
   
   (c :accessor c :initarg :c :initform 0)
   (d :accessor d :initarg :d :initform 1)
   (ty :accessor ty :initarg :ty :initform 0)))

(defmacro make-matrix (&rest rest)
  `(make-instance 'matrix ,@rest))

(defmacro reset (matrix) 
  `(with-slots (trafo-id a b tx c d ty) ,matrix
     (setf trafo-id (incf *trafo-id*))
     (setf a 1
	   b 0
	   tx 0
	   c 0
	   d 1
	   ty 0)
     ,matrix))

(defmacro translate (matrix x y)
  `(with-slots (trafo-id tx ty) ,matrix
     (setf trafo-id (incf *trafo-id*))
     (incf tx ,x)
     (incf ty ,y)
     ,matrix))

(defmacro scale (matrix sx sy)
  (let ((sx1 sx)
	(sy1 sy))
    `(with-slots (trafo-id a b c d tx ty) ,matrix
       (setf trafo-id (incf *trafo-id*))
       (psetf a (* a ,sx1)
	      b (* b ,sx1)
	      tx (* tx ,sx1)
	      ty (* ty ,sy1)
	      c (* c ,sy1)	   
	      d (* d ,sy1))
       ,matrix)))


(defmacro rotate (matrix r)
  `(let ((rot ,r))
     (with-slots (trafo-id a b c d tx ty) ,matrix
       (let ((pcos (cos rot))
	     (msin (- (sin rot)))
	     (psin (sin rot)))
	 (setf trafo-id (incf *trafo-id*))
	 (psetf
	     a (+ (* a pcos) (* c msin))
	     b (+ (* b pcos) (* d msin))
	     tx (+ (* tx pcos) (* ty msin))
	     c (+ (* a psin) (* c pcos))
	     d (+ (* b psin) (* d pcos))
	     ty (+ (* tx psin) (* ty pcos)))
	 ,matrix))))

(defmacro with-matrix ((matrix) &body body)
  `(let ((*matrix* ,matrix))
     ,@body))

(defmacro with-no-matrix-at-all (&body body)
  `(let ((*matrix* nil))
     ,@body))

;;;
;;;
;;;

(defmacro with-saved-matrix ((matrix) &body body)
  (let ((a (gensym))
	(b (gensym))
	(tx (gensym))
	(c (gensym))
	(d (gensym))
	(ty (gensym)))
    `(let ((,a (a ,matrix))
	   (,b (b ,matrix))
	   (,c (c ,matrix))
	   (,d (d ,matrix))
	   (,tx (tx ,matrix))
	   (,ty (ty ,matrix)))
       (prog1
	   (progn
	     ,@body)
	 (with-slots (a b c d tx ty) ,matrix
	   (setf a ,a
		 b ,b
		 c ,c
		 d ,d
		 tx ,tx
		 ty ,ty))))))

(defmacro with-translation ((tx ty) &body body)
  `(if *matrix*
       (with-saved-matrix (*matrix*)
	 (translate *matrix* ,tx ,ty)
	 ,@body)
     (let ((*matrix* (make-matrix :tx ,tx :ty ,ty)))
       ,@body)))

(defmacro with-scaling ((sx sy) &body body)
  `(if *matrix*
       (with-saved-matrix (*matrix*)
	 (scale *matrix* ,sx ,sy)
	 ,@body)
     (let ((*matrix* (make-matrix :a ,sx :d ,sy)))
       ,@body)))

(defmacro with-rotation ((rot) &body body)
  `(if *matrix*
       (with-saved-matrix (*matrix*)
	 (rotate *matrix* ,rot)
	 ,@body)
     (let ((*matrix* (make-matrix)))
       (rotate *matrix* ,rot)
       ,@body)))

;;;
;;;

(defmethod ccw ((p0 geom-point) (p1 geom-point) (p2 geom-point))
  (let* ((x0 (x p0))
	 (x1 (x p1))
	 (x2 (x p2))
	 (y0 (y p0))
	 (y1 (y p1))
	 (y2 (y p2)))
    (ccw* x0 y0 x1 y1 x2 y2)))

(defun ccw* (x0 y0 x1 y1 x2 y2)
  (let* ((dx1 (- x1 x0))
	 (dy1 (- y1 y0))
	 (dx2 (- x2 x0))
	 (dy2 (- y2 y0)))
    
    (cond ((> (* dx1 dy2) (* dy1 dx2)) 1)
	  ((< (* dx1 dy2) (* dy1 dx2)) -1)
	  ((or (< (* dx1 dx2) 0)
	       (< (* dy1 dy2) 0))
	   -1)
	  ((< (+ (* dx1 dx1) (* dy1 dy1))
	      (+ (* dx2 dx2) (* dy2 dy2)))
	   1)
	  (t 0))))


;;;
;;; Konstruktoren
;;;

(defun make-bounding-box (xmin ymin xmax ymax &rest initargs) ; externe Dienstleistung
  (apply #'make-instance 'bounding-box
	 :xmin xmin :ymin ymin
	 :xmax xmax :ymax ymax	 
	 initargs))

;;;
;;;
;;;


(defmethod make-point ((class (eql 'geom-point))
		       x y &rest initargs)
  (apply #'make-instance class 
	 :x x :y y
	 initargs))

(defmethod make-line ((class (eql 'geom-line))
		      p1 p2 &rest initargs 
		      &key (check-p t) (affected-by-matrix-p t)
		      &allow-other-keys)
  (when (and check-p (point-=-p p1 p2))
    (error 'geom-error :descr "Bad line!"))

  (apply #'make-instance class
	 :p1 p1
	 :p2 p2
	 :affected-by-matrix-p affected-by-matrix-p
	 initargs))

(defmethod make-chain ((class (eql 'geom-chain))
		       segment-list
		       &rest initargs 
		       &key (check-p t) (affected-by-matrix-p t)
			    must-be-simple-p
		       &allow-other-keys)
  (when (and check-p
	     (not (segment-list-ok-p segment-list 'chain must-be-simple-p)))
    (error 'geom-error :descr "Bad chain!"))

  (apply #'make-instance class 
	 :segments segment-list
	 :affected-by-matrix-p affected-by-matrix-p
	 initargs))

(defmethod make-polygon ((class (eql 'geom-polygon))
			 segment-list
			 &rest initargs 
			 &key (check-p t) (affected-by-matrix-p t)
			      must-be-simple-p
			 &allow-other-keys)
  (when (and check-p
	     (not (segment-list-ok-p segment-list 'polygon must-be-simple-p)))
    (error 'geom-error :descr "Bad polygon!"))

  (apply #'make-instance class 
	 :segments segment-list
	 :affected-by-matrix-p affected-by-matrix-p
	 initargs))

(defmethod make-aggregate ((class (eql 'geom-aggregate))
			   has-parts
			   &rest initargs 
			   &key (check-p t) (affected-by-matrix-p t)
			   &allow-other-keys)	       
  (when (and check-p (not has-parts))
    (error 'geom-error :descr "Bad aggregate!"))
  
  (apply #'make-instance class 
	 :has-parts has-parts
	 :affected-by-matrix-p affected-by-matrix-p
	 initargs))

;;;
;;;
;;;


(defun bb (xmin ymin xmax ymax &rest initargs)
  (apply #'make-bounding-box xmin ymin xmax ymax initargs))

(defun p (x y &rest initargs)
  (apply #'make-point 'geom-point x y initargs))

(defun l (p1 p2 &rest initargs)  
  (apply #'make-line 'geom-line p1 p2 initargs))

(defun chain (segment-list &rest initargs)
  (apply #'make-chain 'geom-chain segment-list initargs))

(defun chain-from-xy-list (xy-list &rest initargs)
  (let ((points
	 (mapcar #'(lambda (c)
		     (p (first c) 
			(second c)))
		 xy-list)))
    (apply #'chain
	   (mapcar #'(lambda (p1 p2)
		       (l p1 p2))
		   points (rest points))
	   initargs)))

(defun poly (segment-list &rest initargs)
  (apply #'make-polygon 'geom-polygon segment-list initargs))

(defun agg (has-parts &rest initargs)
  (apply #'make-aggregate 'geom-aggregate has-parts initargs))

;;;
;;;
;;;

(defmethod joins-p ((i geom-line) (j geom-line))
  "joins-p(i,j) <=> haben mind. einen wertgleichen oder identischen Endpunkt"
  (or (and (point-=-p (p1 i) (p1 j)))
      (and (point-=-p (p1 i) (p2 j)))
      (and (point-=-p (p2 i) (p1 j)))
      (and (point-=-p (p2 i) (p2 j)))))

;;;
;;;
;;;


(defun segment-list-ok-p (segments type must-be-simple-p)
  "Bedingung: mind. 2 Segmente, keine (wert)gleichen Strecken, keine Selbstueberschneidungen"
  (let ((first (first segments))
	(last (first (last segments)))
	(n (length segments)))
    
    (and (>= n 2)
	 (=> (eq type 'polygon)
	     (>= n 3))
	 
	 (every #'(lambda (i)
		    (= (count i segments) 1))
		segments)
	 
	 (if (eq type 'chain)
	     (and (every #'(lambda (i j)  				  
			     (joins-p i j))
			 segments (rest segments))
		  (=> (> n 2)
		      (not (joins-p (first segments)
				    (first (last segments))))))
	   (and (every #'(lambda (i j)			  
			   (joins-p i j))
		       (cons last segments) segments)
		(every #'(lambda (i)
			   (= 2 (count-if #'(lambda (j)
					      (and (not (eq i j))
						   (joins-p i j)))
					  segments)))
		       segments)))
	 
	 (=> must-be-simple-p
	     (every #'(lambda (i)
			(let ((count	; Schnitte zaehlen
			       (count-if
				#'(lambda (j)
				    (and (not (eq i j))
					 (intersects-p i j)))
				segments)))
			  (if (or (eq first i) (eq last i)) ; erstes od. letztes Segment ? 
			      (if (eq type 'chain)
				  (= count 1)
				(= count 2))
			    (= count 2))))
		    segments)))))

;;;
;;;
;;;


(defmethod initialize-instance :after ((chain-or-polygon geom-chain-or-polygon) 
				       &rest initargs &key (hierarchicly-p t))
  (declare (ignore initargs))
  (with-slots (segments) chain-or-polygon
    
    (setf (point-list chain-or-polygon)
      (let* ((first (first segments))
	     (second (second segments))
	     (points
	      (if (or (point-=-p (p1 first)
				 (p1 second))
		      (point-=-p (p1 first)
				 (p2 second)))
		  (list (p2 first))
		(list (p1 first)))))
	(loop as segment in segments 
	    do
	      (progn (pushnew (p1 segment) points :test #'point-=-p)
		     (pushnew (p2 segment) points :test #'point-=-p)))
	(nreverse points)))
    
    (let ((points (point-list chain-or-polygon)))
      #|  
      (if (typep chain-or-polygon 'geom-chain)
	  (mapc #'(lambda (s p1 p2)
		    (setf (p1 s) p1
			  (p2 s) p2))
		segments
		points
		(rest points))
	(mapc #'(lambda (s p1 p2)
		  (setf (p1 s) p1
			(p2 s) p2))
	      segments		
	      (append points (list (first points)))
	      (append (rest points) (list (first points)))))
      |#
      
      (when (and (typep chain-or-polygon 'geom-chain)
		 hierarchicly-p)		      
	(let ((p1 (first points))
	      (p2 (first (last points))))
	  (setf (p1 chain-or-polygon) p1
		(p2 chain-or-polygon) p2)
	  (push chain-or-polygon (p1-of p1))
	  (push chain-or-polygon (p2-of p2))))
      
      (when hierarchicly-p 
	(dolist (segment segments)
	  (push chain-or-polygon (part-of segment)))))))


(defmethod delete-object progn ((obj geom-chain-or-polygon) &key)
  (dolist (segment (segments obj))
    (setf (part-of segment)
      (delete obj (part-of segment))))
  (when (typep obj 'geom-chain)
    (let ((p1 (p1 obj))
	  (p2 (p2 obj)))
      (setf (p1-of p1)
	(delete obj (p1-of p1)))
      (setf (p2-of p2)
	(delete obj (p2-of p2))))))

;;;
;;;
;;;


(defmethod initialize-instance :after ((line geom-line) 
				       &rest initargs
				       &key (hierarchicly-p t))	
  (declare (ignore initargs))
  (with-slots (p1 p2 point-list) line
    (setf point-list (list p1 p2))
    (when hierarchicly-p 
      (push line (part-of p1))
      (push line (part-of p2))
      (push line (p1-of p1))
      (push line (p2-of p2)))))


(defmethod delete-object progn ((obj geom-line) &key)
  (let ((p1 (p1 obj))
	(p2 (p2 obj)))
    (setf (p1-of p1)
      (delete obj (p1-of p1)))
    (setf (p2-of p2)
      (delete obj (p2-of p2)))
    (setf (part-of p1)
      (delete obj (part-of p1)))
    (setf (part-of p2)
      (delete obj (part-of p2)))))

;;;
;;;
;;;

(defmethod initialize-instance :after ((agg geom-aggregate) 				      
				       &rest initargs
				       &key (hierarchicly-p t))
  (declare (ignore initargs))
  (when hierarchicly-p
    (with-slots (has-parts) agg
      (dolist (part has-parts)
	(push agg (part-of part))))))


(defmethod delete-object progn ((obj geom-aggregate) &key)
  (dolist (part (has-parts obj))
    (setf (part-of part)
      (delete obj (part-of part)))))

;;;
;;;
;;;

(defmethod initialize-instance :after ((bounding-box bounding-box) 
				       &rest initargs
				       &key xmin ymin xmax ymax)
  (declare (ignore initargs))
  (setf (pmin bounding-box)
    (make-point 'geom-point 
		(min xmin xmax) 
		(min ymin ymax)
		:affected-by-matrix-p nil))
  
  (setf (pmax bounding-box)
    (make-point 'geom-point
		(max xmin xmax)
		(max ymin ymax)
		:affected-by-matrix-p nil))
  
  (setf (radius bounding-box)
    (sqrt
     (+ (expt (/ (- xmax xmin) 2) 2)
	(expt (/ (- ymax ymin) 2) 2))))
  
  (setf (pcenter bounding-box)
    (make-point 'geom-point 
		(/ (+ xmin xmax) 2)
		(/ (+ ymin ymax) 2)
		:affected-by-matrix-p nil)))

;;;
;;;
;;;


(defmethod (setf centroid) :after (centroid (master geom-thing))
  (push master
	(centroid-of centroid)))


(defmethod (setf pcenter) :after (center (master bounding-box-mixin))
  (push master
	(pcenter-of center)))


(defun centroid-of-pointlist (pointlist) 
  (let ((n (length pointlist)))
    (values (/ (reduce #'+ (mapcar #'x pointlist)) n)
	    (/ (reduce #'+ (mapcar #'y pointlist)) n))))


(defmethod calculate-centroid ((obj geom-line))
  (when (centroid-p obj)
    (multiple-value-bind (x y)
	(centroid-of-pointlist
	 (list (p1 obj) (p2 obj)))
      (setf (centroid obj)
	(make-point (class-of-internal-points obj)
		    x y
		    :affected-by-matrix-p 
		    (affected-by-matrix-p obj))))))


(defmethod calculate-centroid ((obj geom-chain-or-polygon))
  (when (centroid-p obj)
    (multiple-value-bind (x y)
	(centroid-of-pointlist
	 (point-list obj))
      (setf (centroid obj)
	(make-point (class-of-internal-points obj)
		    x y
		    :affected-by-matrix-p 
		    (affected-by-matrix-p obj))))))

(defmethod calculate-centroid ((obj geom-aggregate))
  (when (centroid-p obj)
    (multiple-value-bind (x y)
	(centroid-of-pointlist
	 (mapcar #'centroid (has-parts obj)))
      (setf (centroid obj)
	(make-point (class-of-internal-points obj)
		    x y
		    :affected-by-matrix-p 
		    (affected-by-matrix-p obj))))))


;;;
;;;
;;;

(defmethod calculate-bounding-box :after ((obj bounding-box-mixin)
					  &key reuse-internal-points-p)
  (when (bounding-box-p obj)
    (with-slots (radius pmin pmax pcenter) obj
      (let ((xc (/ (+ (x pmin) (x pmax)) 2))
	    (yc (/ (+ (y pmin) (y pmax)) 2)))
	(if (and reuse-internal-points-p pcenter)
	    (with-slots (x y) pcenter
	      (setf xc x 
		    yc y))
	  (setf (pcenter obj)		; weg. Writer-After-Methode !
	    (make-point 
	     (class-of-internal-points obj)
	     xc yc 
	     :affected-by-matrix-p nil)))
	
	
	(setf radius
	  (sqrt
	   (+ (expt (/ (- (x pmax) (x pmin)) 2) 2)
	      (expt (/ (- (y pmax) (y pmin)) 2) 2))))))))

(defmethod calculate-bounding-box ((obj geom-line) 					  
				   &key reuse-internal-points-p)
  (when (bounding-box-p obj)
    (with-slots (pmin pmax p1 p2) obj
      (let ((xmax (max (x p1) (x p2)))
	    (ymax (max (y p1) (y p2)))
	    (xmin (min (x p1) (x p2)))
	    (ymin (min (y p1) (y p2))))
	(if (and reuse-internal-points-p pmin pmax)
	    (setf (x pmax) xmax
		  (y pmax) ymax
		  (x pmin) xmin
		  (y pmin) ymin)
	  (setf pmax (make-point 
		      (class-of-internal-points obj)
		      xmax ymax
		      :affected-by-matrix-p nil)

		pmin (make-point
		      (class-of-internal-points obj)
		      xmin ymin
		      :affected-by-matrix-p nil)))))))


(defmethod calculate-bounding-box-for-complex-object ((obj bounding-box-mixin) parts
						      &key reuse-internal-points-p)
  (when (bounding-box-p obj)
    (with-slots (pmin pmax) obj
      (let ((xmin nil)
	    (ymin nil)
	    (xmax nil)
	    (ymax nil))
	(dolist (part parts)
	  (dolist (point
		      (if (typep part 'bounding-box-mixin)
			  (list (pmin part)
				(pmax part))
			(list part)))
	    (let ((x (x point))
		  (y (y point)))
	      (when (or (not xmin) (< x xmin)) (setf xmin x))
	      (when (or (not xmax) (> x xmax)) (setf xmax x))
	      (when (or (not ymin) (< y ymin)) (setf ymin y))
	      (when (or (not ymax) (> y ymax)) (setf ymax y)))))
	(if (and reuse-internal-points-p pmin pmax)
	    (setf (x pmin) xmin
		  (y pmin) ymin
		  (x pmax) xmax
		  (y pmax) ymax)
	  (setf pmin (make-point (class-of-internal-points obj)
				 xmin ymin
				 :affected-by-matrix-p nil)

		pmax (make-point (class-of-internal-points obj)
				 xmax ymax
				 :affected-by-matrix-p nil)))))))



(defmethod calculate-bounding-box ((obj geom-chain-or-polygon)
				   &key reuse-internal-points-p)
  (calculate-bounding-box-for-complex-object obj (segments obj)
					     :reuse-internal-points-p reuse-internal-points-p))

(defmethod calculate-bounding-box ((obj geom-aggregate)
				   &key reuse-internal-points-p)
  (calculate-bounding-box-for-complex-object obj (has-parts obj)
					     :reuse-internal-points-p reuse-internal-points-p))


;;;
;;;
;;;

(defmethod recalculate-bounding-box ((obj bounding-box-mixin))
  (calculate-bounding-box obj :reuse-internal-points-p t)
  (setf (last-trafo-id obj) (trafo-id *matrix*)))


;;;
;;;
;;;

(defmethod invalidate-bounding-box ((obj bounding-box-mixin))
  (setf (last-trafo-id obj) -2))

(defmethod invalidate-bounding-box ((obj geom-chain-or-polygon))
  (dolist (segment (segments obj))
    (invalidate-bounding-box segment))
  (call-next-method))

(defmethod invalidate-bounding-box ((obj geom-aggregate))
  (dolist (part (has-parts obj))
    (invalidate-bounding-box part))
  (call-next-method))

;;;
;;;
;;;

(defmethod parallel-p ((line1 geom-line) (line2 geom-line))
  (let ((dx1 (- (x (p1 line1)) (x (p2 line1))))
	(dx2 (- (x (p1 line2)) (x (p2 line2))))
	(dy1 (- (y (p1 line1)) (y (p2 line1))))
	(dy2 (- (y (p1 line2)) (y (p2 line2)))))        
    (or (zerop-eps (- (* dx1 dy2) (* dx2 dy1)))
	(zerop-eps (- (* dx2 dy1) (* dx1 dy2))))))

(defmethod upright-p ((line1 geom-line) (line2 geom-line))
  (let ((dx1 (- (x (p1 line1)) (x (p2 line1))))
	(dx2 (- (x (p1 line2)) (x (p2 line2))))
	(dy1 (- (y (p1 line1)) (y (p2 line1))))
	(dy2 (- (y (p1 line2)) (y (p2 line2)))))        
    (zerop-eps
     (+ (* dx1 dx2) (* dy1 dy2))))) 

;;;
;;;
;;;

(defmethod calculate-intersection-point ((line1 geom-line) (line2 geom-line))
  (when (member (calculate-relation line1 line2) 
		'(crosses touches))
    (let* ((x1l1 (x (p1 line1)))
	   (y1l1 (y (p1 line1)))
	   (x2l1 (x (p2 line1)))
	   (y2l1 (y (p2 line1)))
	   (x1l2 (x (p1 line2))) 
	   (y1l2 (y (p1 line2))) 
	   (x2l2 (x (p2 line2)))
	   (y2l2 (y (p2 line2))) 
	   (m1 (if (= x2l1 x1l1) 'infinit (/ (- y2l1 y1l1) (- x2l1 x1l1)))) 
	   (m2 (if (= x2l2 x1l2) 'infinit (/ (- y2l2 y1l2) (- x2l2 x1l2)))))
      
      (cond	    
       ((or (and (eq m1 'infinit)
		 (eq m2 'infinit))
	    (and (not (or (eq m1 'infinit)
			  (eq m2 'infinit))) ; (= ..) schuetzen
		 (= m1 m2)))
	(let ((p
	       (if (or (point-=-p (p1 line1)
				  (p1 line2))
		       (point-=-p (p1 line1)
				  (p2 line2)))
		   (p1 line1)
		 (p2 line1))))
	  (values (x p) (y p))))      
       ((eq m1 'infinit) 
	(let* ((xs x2l1) 
	       (ys (+ y1l2 (* m2 (- xs x1l2)))))
	  (values xs ys)))
       ((eq m2 'infinit) 
	(let* ((xs x2l2) 
	       (ys (+ y1l1 (* m1 (- xs x1l1)))))
	  (values xs ys)))
       (t
	(let* ((xs (/ (- (* m1 x1l1) (* m2 x1l2) (- y1l1 y1l2))
		      (- m1 m2)))
	       (ys (+ y1l1 (* m1 (- xs x1l1)))))
	  (values xs ys)))))))


(defmethod calculate-intersection-line ((line1 geom-line) (line2 geom-line))
  (let* ((x1l1 (x (p1 line1)))
	 (y1l1 (y (p1 line1)))
	 (x2l1 (x (p2 line1)))
	 (y2l1 (y (p2 line1)))
	 (x1l2 (x (p1 line2))) 
	 (y1l2 (y (p1 line2))) 
	 (x2l2 (x (p2 line2)))
	 (y2l2 (y (p2 line2))))
    (multiple-value-bind (rel l1p1-l2 l1p2-l2 l2p1-l1 l2p2-l1)
	(calculate-relation line1 line2 :detailed t)
      (case rel
	(equal (values x1l1 y1l1 x2l1 y2l1))
	((inside covered-by) (values x1l1 y1l1 x2l1 y2l1))
	((contains covers) (values x1l2 y1l2 x2l2 y2l2))
	(overlaps (cond ((and l1p1-l2 l2p1-l1)
			 (values x1l1 y1l1 x1l2 y1l2))
			((and l1p1-l2 l2p2-l1)
			 (values x1l1 y1l1 x2l2 y2l2))
			((and l1p2-l2 l2p1-l1)
			 (values x2l1 y2l1 x1l2 y1l2))
			((and l1p2-l2 l2p2-l1)
			 (values x2l1 y2l1 x2l2 y2l2))))))))

;;;
;;;
;;;

(defun distance-between* (x1 y1 x2 y2 &key (sqrt t) (sx 1) (sy 1))
  (let* ((dx (/ (- x2 x1) sx))
	 (dy (/ (- y2 y1) sy)))
    (if sqrt
	(sqrt (+ (* dx dx) (* dy dy)))
      (+ (* dx dx) (* dy dy)))))

(defmethod distance-between-xy ((x number) (y number) 
				(point geom-point)
				&key (sqrt t) (sx 1) (sy 1))
  (distance-between* x y (x point) (y point) 
		     :sqrt sqrt :sx sx :sy sy))

(defmethod distance-between ((point1 geom-point)
			     (point2 geom-point)
			     &key (sqrt t) (sx 1) (sy 1))
  (distance-between* (x point1) (y point1)
		     (x point2) (y point2)
		     :sqrt sqrt :sx sx :sy sy))

(defun distance-between-point-and-line (px py
					lx1 ly1
					lx2 ly2
					&key (sqrt t) (sx 1) (sy 1))
  (flet ((betw-0-and-1 (number)
	   (and (not (minusp number))
		(<= number 1.0))))
    
    (let* ((ax (/ (- lx2 lx1) sx))
	   (ay (/ (- ly2 ly1) sy))
	   (dx (/ (- lx1 px) sx))
	   (dy (/ (- ly1 py) sy))
	   (a2 (+ (* ax ax) (* ay ay)))
	   (scalar
	    (if (zerop-eps a2)
		+big-int+
	      (/ (+ (* ax (- dx))
		    (* ay (- dy)))
		 a2)))                         
	   (x (+ dx
		 (* scalar ax)))
	   (y  (+ dy
		  (* scalar ay)))
	   (res
	    (if (betw-0-and-1 scalar)
		(+ (* x x) (* y y))
	      (min (distance-between* px py lx1 ly1 :sqrt nil :sx sx :sy sy)
		   (distance-between* px py lx2 ly2 :sqrt nil :sx sx :sy sy)))))
      (if sqrt
	  (sqrt res)
	res))))


(defmethod distance-between ((point geom-point)
			     (line geom-line)
			     &key (sqrt t) (sx 1) (sy 1))
  (distance-between-point-and-line (x point) 
				   (y point)
				   (x (p1 line))
				   (y (p1 line))
				   (x (p2 line))
				   (y (p2 line))
				   :sqrt sqrt :sx sx :sy sy))

(defmethod distance-between-xy ((x number) (y number)
				(line geom-line)
				&key (sqrt t) (sx 1) (sy 1))				
  (distance-between-point-and-line x 
				   y
				   (x (p1 line))
				   (y (p1 line))
				   (x (p2 line))
				   (y (p2 line))
				   :sqrt sqrt :sx sx :sy sy))

(defmethod distance-between ((line geom-line)
			     (point geom-point)
			     &key (sqrt t) (sx 1) (sy 1))
  (distance-between point line :sqrt sqrt :sx sx :sy sy))


(defmethod distance-between ((line1 geom-line)
			     (line2 geom-line)
			     &key (sqrt t) (sx 1) (sy 1))
  (let* ((d1
	  (distance-between (p1 line1) line2 :sqrt sqrt :sx sx :sy sy))
	 (d2 
	  (distance-between (p2 line1) line2 :sqrt sqrt :sx sx :sy sy))
	 (d3 
	  (distance-between (p1 line2) line1 :sqrt sqrt :sx sx :sy sy))
	 (d4
	  (distance-between (p2 line2) line1 :sqrt sqrt :sx sx :sy sy)))
    (if (intersects-p line1 line2)
	0
      (min d1 d2 d3 d4))))


(defmethod distance-between ((line geom-line)
			     (poly geom-chain-or-polygon)
			     &key (sqrt t) (sx 1) (sy 1))
  (loop as i in (segments poly) minimize
	(distance-between i line :sqrt sqrt :sx sx :sy sy)))

(defmethod distance-between ((poly geom-chain-or-polygon)
			     (line geom-line)
			     &key (sqrt t) (sx 1) (sy 1))	
  (distance-between line poly :sqrt sqrt :sx sx :sy sy))


(defmethod distance-between ((poly1 geom-chain-or-polygon)
			     (poly2 geom-chain-or-polygon)
			     &key (sqrt t) (sx 1) (sy 1))
  (loop as i in (segments poly1) minimize
	(distance-between i poly2 :sqrt sqrt :sx sx :sy sy)))


(defmethod distance-between ((point geom-point)
			     (poly geom-chain-or-polygon)
			     &key (sqrt t) (sx 1) (sy 1))
  (loop as i in (segments poly) minimize
	(distance-between point i :sqrt sqrt :sx sx :sy sy)))

(defmethod distance-between-xy ((x number) (y number)
				(poly geom-chain-or-polygon)
				&key (sqrt t) (sx 1) (sy 1))
  (loop as i in (segments poly) minimize
	(distance-between-xy x y i :sqrt sqrt :sx sx :sy sy)))


(defmethod distance-between ((poly geom-chain-or-polygon)
			     (point geom-point)
			     &key (sqrt t) (sx 1) (sy 1))
  (distance-between point poly :sqrt sqrt :sx sx :sy sy))


;;;
;;; 
;;;

(defun angle-between* (x1 y1 x2 y2)
  (let* ((dx (- x2 x1))
	 (dy (- y2 y1))       
	 (phi 
	  (phase (complex dx dy))))
    (if (minusp phi)
	(+ +2pi+ phi)
      phi)))

(defun distance-and-orientation* (x1 y1 x2 y2)
  (multiple-value-call
      #'values
    (distance-between* x1 y1 x2 y2)
    (angle-between* x1 y1 x2 y2)))

(defun normalize (angle)
  (mod angle +2pi+))

(defun angle-difference (a b)
  (min
   (normalize
    (- a b))
   (normalize
    (- b a))))

;;;
;;;
;;;

(defmethod length-of-line ((line geom-line))
  (distance-between (p1 line) (p2 line)))

(defmethod global-orientation ((line geom-line))
  (angle-between* (x (p1 line)) (y (p1 line)) (x (p2 line)) (y (p2 line))))

#|

(defmethod angle-between ((line1 geom-line) (line2 geom-line))
  "Draw line1 around nearest point w.r.t. line2 counterclockwise and measure angle!"
  (let* ((d11 (distance-between (p1 line1) (p1 line2)))
	 (d21 (distance-between (p2 line1) (p1 line2)))
	 (d12 (distance-between (p1 line1) (p2 line2)))
	 (d22 (distance-between (p2 line1) (p2 line2)))
	 (dmin (min d11 d21 d12 d22)))
    (multiple-value-bind (as ae bs be)
	(cond ((= dmin d11) (values (p1 line1) (p2 line1) (p1 line2) (p2 line2)))
	      ((= dmin d12) (values (p1 line1) (p2 line1) (p2 line2) (p1 line2)))
	      ((= dmin d21) (values (p2 line1) (p1 line1) (p1 line2) (p2 line2)))
	      ((= dmin d22) (values (p2 line1) (p1 line1) (p2 line2) (p1 line2))))
      (normalize (- (angle-between* (x as) (y as) (x ae) (y ae))
		    (angle-between* (x bs) (y bs) (x be) (y be)))))))
|#

;;;
;;;
;;;

(defmethod calculate-area ((obj geom-polygon)) 
  (with-slots (point-list) obj    
    (let ((f 0)
	  (first (first point-list))
	  (last (first (last point-list))))
      (mapc #'(lambda (pj pj+1 pj-1)
		(let ((xj (x pj))
		      (yj+1 (y pj+1))
		      (yj-1 (y pj-1)))			    
		  (incf f (* xj (- yj+1 yj-1)))))
	    point-list
	    (append (rest point-list) (list first))
	    (cons last point-list))
      (/ f 2))))

;;;
;;;
;;;

(defun det-0-vecs (x1 y1 x2 y2)
  (- (* x1 y2) 
     (* x2 y1)))

(defun det (x1s y1s x1e y1e
	    x2s y2s x2e y2e)
  (let ((x1 (- x1e x1s))
	(y1 (- y1e y1s))
	(x2 (- x2e x2s))
	(y2 (- y2e y2s)))
    (- (* x1 y2) 
       (* x2 y1))))

(defun my-asin (length-of-a length-of-hypothenuse signum-x signum-y)
  (let ((angle (asin (/ length-of-a 
			length-of-hypothenuse))))
    (cond ((and (plusp signum-x) (plusp signum-y)) angle)
	  ((and (minusp signum-x) (plusp signum-y)) (- pi angle))
	  ((and (minusp signum-x) (minusp signum-y)) (- (* 3/2 pi) angle))
	  (t (- +2pi+ angle)))))

(defun proportional-2-point (o1x o1y a1x a1y
			     o2x o2y a2x a2b)
  (let* ((rot (normalize (- (angle-between* o2x o2y a2x a2b)
			    (angle-between* o1x o1y a1x a1y))))	 
	 (scale (/ (distance-between* o2x o2y a2x a2b) 
		   (distance-between* o1x o1y a1x a1y))))
    (values scale rot)))

#| fehlerhaft !

(defun fixed-sy-2-point (o1x o1y a1x a1y
			 o2x o2y a2x a2y)
  (labels ((do-it (o1x o1y a1x a1y
		   o2x o2y a2x a2y inv-p)
	     (let ((x  (- a1x o1x))
		   (y  (- a1y o1y))
		   (a  (- a2x o2x))
		   (b  (- a2y o2y)))	       
	       (unless (zerop x)
		 (let* ((r1 (sqrt (+ (* a a) (* b b))))
			(alpha
			 (my-asin y
				  r1
				  (signum x) (signum y)))
			(cos-alpha (cos alpha)))
		   (unless (zerop cos-alpha)
		     (let* ((alpha2 (angle-between* o2x o2y a2x a2y))
			    (gamma (- alpha2 alpha))		    
			    (sx (abs (/ (+ a (* (sin gamma) y))
					(* x (cos gamma))))))
		       (multiple-value-bind (sx gamma)
			   (if inv-p
			       (progn (princ "!")
				      (values (/ 1 sx) (- gamma)))
			     (values sx gamma))
			 
			 (let ((m (make-matrix))
			       (o1 (p o1x o1y))
			       (a1 (p a1x a1y)))
			   
			   (with-matrix (m)
			     (reset m)	
			     (translate m (- o1x) (- o1y))
			     (scale m sx 1)
			     (rotate m gamma)
			     (translate m o2x o2y)
			     
			     (mapc #'(lambda (l)
				       (format t "~% ~A ~A -> ~A ~A" (first l) (second l) (third l) (fourth l)))
				   (list (list (x o1) (y o1) o2x o2y)
					 (list (x a1) (y a1) a2x a2y)))))
			 (values sx gamma)))))))))
    (let* ((a  (- a2x o2x))
	   (b  (- a2y o2y))
	   (y  (- a1y o1y)))
      (if (< (abs y) (sqrt (+ (* a a) (* b b))))
	  (do-it o1x o1y a1x a1y
		 o2x o2y a2x a2y nil)
	(do-it 
	    o2x o2y a2x a2y
	    o1x o1y a1x a1y t)))))


|#
