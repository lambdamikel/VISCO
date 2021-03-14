;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: DATABASE; Base: 10 -*-

(in-package database)

(defparameter *cur-db* nil)

;;;
;;;
;;;

(defpersistentclass db ()
  ((objects :accessor objects :initarg :objects :initform nil)
   (spatial-index :accessor spatial-index :initarg :spatial-index :initform nil)
   (name :accessor name :initarg :name)))

;;;
;;;
;;;

(defmethod install-as-current-db ((obj db))
  (setf *cur-db* obj)
  (when (spatial-index obj)
    (install-as-current-index 
   (spatial-index obj)))
  *cur-db*)


(defmethod reset-db ((obj db))
  (with-slots (objects spatial-index) obj
    (setf objects nil)
    (reset-spatial-index spatial-index)))

(defmethod store-db ((obj db))
  (with-slots (objects name) obj
    (make-object-persistent (list obj)
                            (format nil "~A.db" name))
    t))

(defun load-db (fn)  
  (setf *cur-db* nil)
  (install-as-current-db
   (first (load-persistent-object fn))))

		     
(defun make-db-from-sqd-file (fn)
  (let ((db 
	 (install-as-current-db
	  (make-instance 'db :name fn))))
    (read-sqd-file fn)
    (dolist (elem (elements (spatial-index db)))
      (when (and (typep elem 'si-geom-thing-with-relations)
		 (not (typep elem 'db-object))
		 (not (typep elem 'geom-point))) ; loesche alle Objekte nicht DB-Objekte (bis auf SI-Punkte)
	(delete-object elem)))
    (calculate-inside-relations (objects db))
    db))

(defun get-current-db ()
  *cur-db*)
		     
;;;
;;; DB-Objekte
;;;

(defpersistentclass db-object (si-geom-thing-with-relations) ; abstrakt
  ((dont-display :accessor dont-display :initarg :dont-display :initform nil)
   (all-os :accessor all-os :initarg :all-os :initform nil) ; Liste von OS-Schluesseln
   (enum :accessor enum :initarg :enum :initform nil)
   (line-no :accessor line-no :initarg :line-no :initform nil)))

(defmethod print-object ((obj db-object) stream)
  (format stream "#<~A ~A ~A>"
	  (type-of obj)
	  (enum obj)
	  (all-os obj)))
;;; 
;;; 
;;;


(defpersistentclass db-point (db-object si-geom-point-with-relations)
  ())

(defpersistentclass db-line (db-object si-geom-line-with-relations)
  ())

(defpersistentclass db-chain (db-object si-geom-chain-with-relations)
  ())

(defpersistentclass db-polygon (db-object si-geom-polygon-with-relations)
  ())
   
#|
(defpersistentclass db-aggregate (...)
  ())
|#

;;;
;;;
;;;

(defmethod update-instance-for-different-class :after ((old si-geom-thing-with-relations)
						       (new db-object)
						       &key (os nil)
						       &allow-other-keys)
  (when os 
    (pushnew os (all-os new)))
  (unless (typep old 'db-object)
    (push new (objects *cur-db*))))


;;;
;;;
;;;

(defmethod make-point ((class (eql 'db-point))
		       x y &rest initargs)
  (let ((point
	 (apply #'make-point 'si-geom-point-with-relations x y 
		:affected-by-matrix-p nil
		initargs)))
    (apply #'change-class point 'db-point :allow-other-keys t initargs)))

(defun db-p (x y &rest initargs)
  (apply #'make-point 'db-point x y initargs))


;;;
;;;
;;;


(defmethod make-line ((class (eql 'db-line))
		      p1 p2 &rest initargs)
  (let ((line
	 (apply #'make-line 'si-geom-line-with-relations p1 p2 
		:affected-by-matrix-p nil
		initargs)))
    (centroid line)			; "call-if-needed"-Berechnung anstossen
    (pcenter line)			; s.o.
    (apply #'change-class line 'db-line :allow-other-keys t initargs)))


(defun db-l (p1 p2 &rest initargs)
  (apply #'make-line 'db-line p1 p2 initargs))


;;;
;;;
;;;


(defmethod make-chain ((class (eql 'db-chain))
		       segment-list 
		       &rest initargs)
  (let ((chain
	 (apply #'make-chain 'si-geom-chain-with-relations segment-list 
		:affected-by-matrix-p nil
		initargs)))
    (centroid chain)			; "call-if-needed"-Berechnung anstossen
    (pcenter chain)			; s.o.
    (apply #'change-class chain 'db-chain :allow-other-keys t initargs)))


(defun db-chain (segment-list &rest initargs)
  (apply #'make-chain 'db-chain segment-list initargs))



(defmethod make-polygon ((class (eql 'db-polygon))
		       segment-list 
		       &rest initargs)
  (let ((polygon
	 (apply #'make-polygon 'si-geom-polygon-with-relations segment-list 
		:affected-by-matrix-p nil
		initargs)))
    (centroid polygon)			; "call-if-needed"-Berechnung anstossen
    (pcenter polygon)			; s.o.
    (apply #'change-class polygon 'db-polygon :allow-other-keys t initargs)))


(defun db-poly (segment-list &rest initargs)
  (apply #'make-polygon 'db-polygon segment-list initargs))
 

#|
(defun make-db-aggregate (objects &optional (os nil) (line-no nil))
  (make-instance 'db-aggregate :components objects :os os :line-no line-no))
|#

;;;
;;;
;;;


(defun set-client-bb (xmin ymin xmax ymax) ; wird vom SQD-Reader aufgerufen => Bounding Box!  
  (setf (spatial-index *cur-db*)
    (init-spatial-index xmin ymin xmax ymax)))

;;;
;;; Schnittstelle zum SQD-Reader: dieser ruft die transform-sqd...-Routinen auf!
;;;

;;; ACHTUNG! 

(defun get-renamed-os (os)
  os)

(defun transform-sqd-pg (enum line-no os x y)
  (let ((fn (get-db-transform-function os 'pg))
	(os (get-renamed-os os)))
    (funcall fn os x y line-no enum)))

(defun transform-sqd-kr (enum line-no os x y r)
  (declare (ignore r))
  (let ((fn (get-db-transform-function os 'kr))
	(os (get-renamed-os os)))
    (funcall fn os x y line-no enum)))


(defun transform-sqd-sy (enum line-no os x y text)
  (let ((fn (get-db-transform-function os 'sy))
	(os (get-renamed-os os)))
    (funcall fn os x y line-no enum text)))

(defun transform-sqd-tx (enum line-no os tx ty h w text)
  (let ((fn (get-db-transform-function os 'tx))
	(os (get-renamed-os os)))
    (funcall fn os tx ty h w text line-no enum)))

(defun transform-sqd-li (enum line-no os ps pe)
  (let ((fn (get-db-transform-function os 'li))
	(os (get-renamed-os os)))
    (funcall fn os ps pe line-no enum)))

(defun transform-sqd-sn (enum line-no os ps pe points)
  (let ((fn (get-db-transform-function os 'sn))
	(os (get-renamed-os os)))
    (funcall fn os ps pe points line-no enum)))

(defun transform-sqd-bo (enum line-no os x y r w ps pe)
  (let ((fn (get-db-transform-function os 'bo))
	(os (get-renamed-os os)))
    (funcall fn os ps pe x y r w line-no enum)))

(defun transform-sqd-fl (enum line-no os components)
  (let ((fn (get-db-transform-function os 'fl))
	(os (get-renamed-os os)))
    (funcall fn os components line-no enum)))

;;;
;;; TX-Konstruktoren
;;;

(defun tx-create-map-text (os tx ty h w text line-no enum)
  (declare (ignore enum line-no os))
  (make-instance 'map-text
    :tx tx :ty ty
    :h h :w w
    :text text))

;;;
;;; PG-Konstruktoren
;;;

(defun pg-create-point (os x y line-no enum)
  (db-p x y :os os :line-no line-no :enum enum))

;;;
;;; LI-Konstruktoren
;;;

(defun li-create-line (os ps pe line-no enum)
  (db-l ps pe :os os :line-no line-no :enum enum))

;;;
;;; BO-Konstruktoren
;;;

(defun bo-create-line (os ps pe x y r w line-no enum)
  (declare (ignore w r x y))
  (db-l ps pe :os os :line-no line-no :enum enum))

;;;
;;; SN-Konstruktoren
;;;

(defun sn-create-segment-list (os ps pe points line-no enum)
  (let* ((points (transform-xy-list points)))
    (if points
	(append
	 (list (db-l 
		ps 
		(db-p (first (first points)) 
		      (second (first points)))
		:os os :line-no line-no :enum enum))
	 (mapcar #'(lambda (p1 p2)
		     (db-l 
		      (db-p (first p1) (second p1))
		      (db-p (first p2) (second p2))
		      :os os :line-no line-no :enum enum))
		 points (rest points))
	 (list (db-l
		(db-p (first (first (last points)))
		      (second (first (last points))))
		pe
		:os os :line-no line-no :enum enum)))
      (db-l
       ps pe 
       :os os :line-no line-no :enum enum))))

;;;
;;; FL-Konstruktoren
;;; 

(defun fl-create-containing-polygon (os components line-no enum)
  (let ((polygons
	 (create-polygons
	  (mapcan #'(lambda (i)
		      (if (listp i)	; zerlegte Kette ?
			  i
			(list i)))
		  components)))
	(db-polygons nil))
    (dolist (i polygons)		; gibt es Polygon j, das i enthaelt ? => i verwerfen
      (unless (some #'(lambda (j)			  
			(and (not (eq i j)) ; Arbeit sparen
			     (inside-p i j)))
		    polygons)
	(push (db-poly (segments i)
		       :os os 
		       :line-no line-no
		       :enum enum)
	      db-polygons)))
    db-polygons))
	    

(defun fl-do-nothing (os components line-no enum)
  (declare (ignore os components line-no enum))
  nil)					; da die Segmente schon angelegt wurden => alles ok

;;;
;;; SY-Konstruktoren
;;;

(defun sy-create-point (os x y line-no enum txt)
  (declare (ignore txt))
  (db-p x y :os os :line-no line-no :enum enum))

(defun sy-create-church (os x y line-no enum txt)
  (declare (ignore txt))
  (db-p x y :os os :line-no line-no :enum enum))

(defun sy-create-postoffice (os x y line-no enum txt)
  (declare (ignore txt))
  (db-p x y :os os :line-no line-no :enum enum))

(defun sy-create-hole (os x y line-no enum txt)
  (declare (ignore txt))
  (db-p x y :os os :line-no line-no :enum enum))

(defun sy-create-allotment (os x y line-no enum txt)
  (declare (ignore txt))
  (db-p x y :os os :line-no line-no :enum enum))

(defun sy-create-chapel (os x y line-no enum txt)
  (declare (ignore txt))
  (db-p x y :os os :line-no line-no :enum enum))

(defun sy-create-firestation (os x y line-no enum txt)
  (declare (ignore txt))
  (db-p x y :os os :line-no line-no :enum enum))

;;;
;;; Hilfsfunktionen
;;;

(defun create-polygons (components)
  (let ((poly-segments (list (first components)))
	(components (rest components)))
    (catch 'done
      (loop     
	(catch 'next
	  (progn
	    (dolist (i components)
	      (let ((j (first poly-segments)))
		(when (joins-p i j) 
		  (push i poly-segments)
		  (setf components (delete i components))
		  (throw 'next nil))))
	    (throw 'done nil)))))
    (let ((poly
	   (make-polygon 'si-geom-polygon-with-relations poly-segments :affected-by-matrix-p nil)))
      (if components
	  (cons poly
		(create-polygons components))
	(list poly)))))

