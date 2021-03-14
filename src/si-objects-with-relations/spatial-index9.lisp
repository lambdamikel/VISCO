;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: SPATIAL-INDEX; Base: 10 -*-

(in-package spatial-index)

(defgeneric install-as-current-index (obj)
  (:documentation "Let the spatial index obj be the current index."))

(defgeneric reset-spatial-index (obj)
  (:documentation "Resets the spatial index obj."))

(defgeneric insert-into-spatial-index (obj)
  (:documentation "Insert obj into current spatial index."))

(defgeneric remove-from-spatial-index (obj)
  (:documentation "Remove obj from current spatial index."))

(defgeneric candidate-selected-p (reference-obj candidate mode &key &allow-other-keys)
  (:documentation "Determine whether a relation << candidate MODE reference-obj >> holds."))

(defgeneric bucket-selected-p (reference-obj bucket mode &key &allow-other-keys)
  (:documentation "Determine whether the objects of BUCKET are relevant candidates to check 
whether a << candidate MODE reference-obj >> relation holds."))

(defgeneric get-range-for-object (reference-obj mode &key &allow-other-keys)
  (:documentation "Returns range of relevant buckets."))

;;;
;;;
;;;

(defparameter *mark-counter* 0)

(defparameter *cur-index* nil)

(defun get-new-mark-value ()
  (incf *mark-counter*))

;;;
;;;
;;;

(defpersistentclass spatial-index ()
  ((grid :accessor grid :initform (make-array (list 20 20) :initial-element nil)
	 :initarg :grid)
   (elements :accessor elements :initform nil)
	     
   (xdiv :accessor xdiv :initarg :xdiv)
   (ydiv :accessor ydiv :initarg :ydiv)
   (xmin :accessor xmin :initarg :xmin)
   (ymin :accessor ymin :initarg :ymin)
   (xmax :accessor xmax :initarg :xmax)
   (ymax :accessor ymax :initarg :ymax)

   (xres :accessor xres :initarg :xres :initform 20)
   (yres :accessor yres :initarg :yres :initform 20)))

;;;
;;; ACHTUNG: alle si-Klassen sind abstrakt => keine Konstruktoren
;;;

(defpersistentclass si-geom-thing (geom-thing)
  ((level-array :accessor level-array
		:initform (make-array +no-of-levels+ :initial-element nil)
		:not-persistent)
   (in-bucket :accessor in-bucket :initform nil)))

(defmethod initialize-loaded-persistent-object ((obj si-geom-thing))
  (setf (level-array obj) (make-array +no-of-levels+ :initial-element nil)))

(defpersistentclass si-geom-point (si-geom-thing geom-point)
  ())

(defpersistentclass si-geom-line (si-geom-thing geom-line)
  ())

(defpersistentclass si-geom-chain-or-polygon (si-geom-thing geom-chain-or-polygon)
  ())

(defpersistentclass si-geom-polygon (si-geom-chain-or-polygon geom-polygon)
  ())

(defpersistentclass si-geom-chain (si-geom-chain-or-polygon geom-chain)
 ())

;;;
;;;
;;;

(defun set-value-for (obj level value)
  (setf (svref (level-array obj) level) value))

(defun get-value-for (obj level)
  (svref (level-array obj) level))

;;;
;;;
;;;

(defmethod initialize-instance :after ((obj si-geom-thing) &rest initargs)
  (declare (ignore initargs))
  (insert-into-spatial-index obj))

(defmethod delete-object progn ((obj si-geom-thing) &key)
  (remove-from-spatial-index obj))
    
;;;
;;;
;;;

(defpersistentclass bucket ()
  ((bbox :accessor bbox :initarg :bbox)
   (elements :accessor elements :initarg :elements
	     :initform nil)))
;;;
;;;
;;;

(defun make-spatial-index (xmin1 ymin1 xmax1 ymax1)
  (let ((index
	 (make-instance 'spatial-index)))
    (with-slots (xdiv ydiv xmin ymin xmax ymax xres yres grid) index	
      (setf xmin xmin1
	    ymin ymin1
	    xmax xmax1
	    ymax ymax1)
      (setf xdiv (/ (- (1+ xmax1) xmin1) xres)
	    ydiv (/ (- (1+ ymax1) ymin1) yres))
      (dotimes (x xres)
	(dotimes (y yres)
	  (setf (aref grid x y)
	    (make-instance 'bucket
	      :bbox 
	      (make-bounding-box 
	       (+ xmin (* x xdiv))
	       (+ ymin (* y ydiv))
	       (+ xmin (* (1+ x) xdiv))
	       (+ ymin (* (1+ y) ydiv))))))))
    index))

(defun init-spatial-index (xmin ymin xmax ymax)
  (install-as-current-index
   (make-spatial-index xmin ymin xmax ymax)))


(defmethod install-as-current-index ((obj spatial-index))
  (setf *cur-index* obj))

;;;
;;;
;;;

(defmethod reset-spatial-index ((obj spatial-index))
  (with-slots (grid xres yres) obj
    (dotimes (x xres)
      (dotimes (y yres)
	(let ((bucket (aref grid x y)))
	  (when bucket
	    (setf
		(elements bucket) nil)))))))

(defmethod get-indizes-for-point* ((obj spatial-index) x y)
  (with-slots (xmin ymin xdiv ydiv) obj
    (values
     (truncate (- x xmin) xdiv)
     (truncate (- y ymin) ydiv))))

(defmethod get-bucket-for-point* ((obj spatial-index) x y)
  (multiple-value-call #'(lambda (x y)
			   (when (and (<= 0 x (1- (xres obj)))
				      (<= 0 y (1- (yres obj))))
			     (aref (grid obj) x y)))
    (get-indizes-for-point* obj x y)))

(defmethod get-elements-at* ((obj spatial-index) x y)
  (elements 
   (get-bucket-for-point* obj x y)))


(defun get-current-bucket (ix iy)
  (aref (grid *cur-index*) ix iy))


;;;
;;;
;;;


(defmethod insert-into-spatial-index ((obj si-geom-thing))    
  (push obj (elements *cur-index*))
  (with-selected-buckets (cur-bucket obj :intersects)    
    (push obj (elements cur-bucket))
    (setf (in-bucket obj) cur-bucket))
  (unless (in-bucket obj)
    (error "No bucket(s) for object ~A!" obj)))

;;;
;;;
;;;

(defmethod remove-from-spatial-index ((obj si-geom-thing))
  (with-selected-buckets (cur-bucket obj :intersects)    
    (setf (elements cur-bucket)
      (delete obj (elements cur-bucket))))
  (setf (elements *cur-index*)
    (delete obj (elements *cur-index*))))

;;;
;;; -----------------------------
;;;

;;;
;;; Grobeinstellung der zu untersuchenden Buckets:
;;;


(defun get-range-for-bb-object (obj &optional (offset 0))
  (if (bounding-box-p obj)
      (let* ((pmin (pmin obj))
	     (pmax (pmax obj))
	     (xmin (x pmin))
	     (ymin (y pmin))
	     (xmax (x pmax))
	     (ymax (y pmax)))
	(multiple-value-bind (ixmin iymin)
	    (get-indizes-for-point* *cur-index* 
				    (- xmin offset) 
				    (- ymin offset))
	  (multiple-value-bind (ixmax iymax)
	      (get-indizes-for-point* *cur-index* 
				      (+ xmax offset)
				      (+ ymax offset))
	    (with-slots (xres yres) *cur-index*
	      (values (max 0 ixmin)
		      (max 0 iymin)
		      (min ixmax (1- xres))
		      (min iymax (1- yres)))))))    
    (error "No bounding box!")))

;;;
;;;
;;;

(defmethod get-range-for-object ((obj si-geom-point) (mode (eql :intersects)) &key)
  (multiple-value-bind (ix iy)
      (get-indizes-for-point* *cur-index*
			      (x obj) (y obj))
    (values ix iy ix iy)))

(defmethod get-range-for-object ((obj si-geom-point) (mode (eql :epsilon)) 
				 &key epsilon-r (epsilon-a 1) (epsilon-b 1))
  (with-slots (xres yres) *cur-index*
    (let ((epsilon-r (* epsilon-r (max epsilon-a epsilon-b))))
      (multiple-value-bind (ixmin iymin)
	  (get-indizes-for-point* *cur-index*
				  (- (x obj) epsilon-r)
				  (- (y obj) epsilon-r))
	(multiple-value-bind (ixmax iymax)
	    (get-indizes-for-point* *cur-index*
				    (+ (x obj) epsilon-r)
				    (+ (y obj) epsilon-r))
	  (values (max 0 ixmin)
		  (max 0 iymin)
		  (min ixmax (1- xres))
		  (min iymax (1- yres))))))))

;;;
;;;
;;;

(defmethod get-range-for-object ((obj bounding-box-mixin) 
				 (mode (eql :intersects)) &key)
  (get-range-for-bb-object obj))


(defmethod get-range-for-object ((obj bounding-box-mixin) 
				 (mode (eql :inside)) &key)
  (get-range-for-bb-object obj))

(defmethod get-range-for-object ((obj bounding-box-mixin) 
				 (mode (eql :outside)) &key)
  (with-slots (xres yres) *cur-index*
    (values 0 0 (1- xres) (1- yres))))


(defmethod get-range-for-object ((obj bounding-box-mixin)
				 (mode (eql :epsilon)) 
				 &key epsilon-r (epsilon-a 1) (epsilon-b 1))
  (let ((offset (* epsilon-r (max epsilon-a epsilon-b))))
    (get-range-for-bb-object obj offset)))

;;;
;;; -----------------------------
;;;

;;;
;;; Grobtest: Bucket untersuchen ?
;;;

(defmethod bucket-selected-p ((obj bounding-box-mixin) (bucket bucket) (mode (eql :inside)) &key)
  (box-overlaps-box-p obj (bbox bucket)))

;;;
;;; PUNKTE
;;;

(defmethod bucket-selected-p ((obj geom-point) (bucket bucket) (mode (eql :intersects)) &key)
  (eq (get-bucket-for-point* *cur-index* (x obj) (y obj)) bucket))

(defmethod bucket-selected-p ((obj geom-point) (bucket bucket) (mode (eql :epsilon)) 
			      &key epsilon-r (epsilon-a 1) (epsilon-b 1))
  (or (bucket-selected-p obj bucket :intersects)
      (let ((radius (radius (bbox bucket)))
	    (center (pcenter (bbox bucket)))
	    (epsilon-r (* epsilon-r (max epsilon-a epsilon-b))))
	(<= (distance-between obj center)
	    (+ radius epsilon-r)))))

;;;
;;; LINIEN
;;;

(defmethod bucket-selected-p ((obj geom-line) (bucket bucket) (mode (eql :intersects)) &key)
  (box-overlaps-box-p obj (bbox bucket)))


(defmethod bucket-selected-p ((obj geom-line) (bucket bucket) (mode (eql :epsilon)) 
			      &key epsilon-r (epsilon-a 1) (epsilon-b 1))
  (let ((epsilon-r (* epsilon-r (max epsilon-a epsilon-b))))
    (enlarged-box-overlaps-box-p obj 
				 (bbox bucket)
				 epsilon-r)))

;;;
;;; KETTEN und POLYGONE
;;;
 
(defmethod bucket-selected-p ((obj geom-chain-or-polygon) (bucket bucket) (mode (eql :intersects)) &key)
  (some #'(lambda (segment)
	    (bucket-selected-p segment bucket :intersects))
	(segments obj)))

(defmethod bucket-selected-p ((obj geom-chain-or-polygon) (bucket bucket) (mode (eql :epsilon)) 
			      &key epsilon-r (epsilon-a 1) (epsilon-b 1))
  (let ((epsilon-r (* epsilon-r (max epsilon-a epsilon-b)))
	(box (bbox bucket)))
    (some #'(lambda (segment)
	      (enlarged-box-overlaps-box-p segment box epsilon-r))
	  (segments obj))))

;;;
;;; POLYGONE
;;;

(defmethod bucket-selected-p ((obj geom-polygon) (bucket bucket) (mode (eql :inside)) &key)
  (box-overlaps-box-p obj (bbox bucket)))


(defmethod bucket-selected-p ((obj geom-polygon) (bucket bucket) (mode (eql :outside)) &key)
  t)

;;;
;;; -----------------------------
;;;

;;;
;;; Feintest: Objekt selektiert?
;;; INSIDE f. Bounding Boxes
;;; spez. fuer den Map-Viewer
;;;

(defmethod candidate-selected-p ((reference-obj bounding-box) (candidate bounding-box-mixin) 
				 (mode (eql :inside)) &key)
  (box-overlaps-box-p candidate reference-obj))


(defmethod candidate-selected-p ((reference-obj bounding-box) (candidate geom-point) 
				 (mode (eql :inside)) &key)
  (point-truly-inside-box-p candidate reference-obj))

;;;
;;; INTERSECTS
;;; 

(defmethod candidate-selected-p ((reference-obj geom-thing) (candidate geom-thing) 
				 (mode (eql :intersects)) &key)
  (intersects-p candidate reference-obj))

;;;
;;; INSIDE
;;; 

(defmethod candidate-selected-p ((reference-obj geom-polygon) (candidate geom-thing) 
				 (mode (eql :inside)) &key)
  (inside-p candidate reference-obj))


;;;
;;; OUTSIDE
;;; 

(defmethod candidate-selected-p ((reference-obj geom-polygon) (candidate geom-thing) 
				 (mode (eql :outside)) &key)
  (outside-p candidate reference-obj))


;;;
;;; EPSILON
;;; 

(defmethod candidate-selected-p ((reference-obj geom-thing) (candidate geom-thing)
				 (mode (eql :epsilon))
				 &key epsilon-r (epsilon-a 1) (epsilon-b 1))
  (inside-epsilon-p candidate reference-obj epsilon-r :epsilon-a epsilon-a :epsilon-b epsilon-b))

;;;
;;; -----------------------------
;;;

;;;
;;; Retrieval: 
;;;

(defun get-point-from-spatial-index* (x y &optional (epsilon 0.0001))
  (let ((bucket (get-bucket-for-point* *cur-index* x y)))
    (when bucket
      (dolist (obj (elements bucket))
	(when (and (typep obj 'si-geom-point)
		   (=-eps x (x obj) epsilon)
		   (=-eps y (y obj) epsilon))
	  (return-from get-point-from-spatial-index* obj))))))

