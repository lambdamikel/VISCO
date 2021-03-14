;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: GUI; Base: 10 -*-

(in-package gui)

;;;
;;;
;;;

(defpersistentclass gui-object ()
  ((tick :accessor tick :initform (incf *tick-counter*))
   (already-drawn :accessor already-drawn :initform nil)
   
   (history-entry :accessor history-entry :initform nil
		  :initarg :history-entry)
   
   (ink :accessor ink :initarg :ink :initform +black+
	:not-persistent)
   (ink-rgb-list :accessor ink-rgb-list :initform (list 0 0 0)
		 :initarg :ink-rgb-list)
   
   (inactive :accessor inactive :initarg :inactive :initform nil)))

(defmethod initialize-loaded-persistent-object :after ((obj gui-object))
  (when (slot-boundp obj 'ink-rgb-list)
    (setf (ink obj)
      (apply #'make-rgb-color (ink-rgb-list obj)))))

(defmethod tick-object ((obj gui-object))
  (incf (tick obj)))

;;;
;;;
;;;

(defpersistentclass gui-label (gui-object)
  ((x-off :accessor x-off :initform 10 :initarg :x-off)
   (y-off :accessor y-off :initform 10 :initarg :y-off)
   (text :accessor text :initform nil :initarg :text)
   (object :accessor object :initform nil :initarg :object)))

(defmethod reinitialize ((obj gui-label))
  (setf (x-off obj) 10
	(y-off obj) 10))

(defmethod print-object ((obj gui-label) stream) 
  (format stream "~A Of ~A"
	  (etypecase obj
	    (gui-status-label "Status Label")
	    (gui-semantics-label "Semantics Label")
	    (gui-at-most-label "At Most Label"))
	  (object obj)))

(defpersistentclass gui-status-label (gui-label)
  nil)

(defpersistentclass gui-semantics-label (gui-label)
  ((ink-rgb-list 
    :initform 
    #+visco-demo-mode
    (list 0 0 0)
    #-visco-demo-mode
    (list 1 0 0))))

(defpersistentclass gui-at-most-label (gui-label)
  ((ink-rgb-list 
    :initform 
    #+visco-demo-mode
    (list 0 0 0)
    #-visco-demo-mode
    (list 1 0 0))))

;;;
;;;
;;;

(defpersistentclass gui-orientation-arrow (gui-object)
  ((object :accessor object :initform nil :initarg :object)
   (rho-off :accessor rho-off :initform pi :initarg :rho-off) ; Offset in Polarcoordinaten
   (r-off :accessor r-off :initform 0 :initarg :r-off)
   
   (r :accessor r :initform 30 :initarg :r)
   (alpha-off :accessor alpha-off :initform +pi/2+ :initarg :alpha-off)
   
   (ink-rgb-list :initform (list 1 0 0))))

(defmethod reinitialize ((obj gui-orientation-arrow))
  (setf (rho-off obj) pi
	(r-off obj) 0
	(r obj) 30
	(alpha-off obj) +pi/2+))

(defmethod print-object ((obj gui-orientation-arrow) stream) 
  (format stream "Orientation Constraint Of ~A"
	  (object obj)))

(defmethod get-origin-for ((obj gui-orientation-arrow))
  (multiple-value-bind (x y)
      (get-label-origin-for (object obj))
    (typecase (object obj)
      (line 
       (let* ((alpha (+ (rho-off obj)
			(global-orientation (object obj))))
	      (length (/ (distance-between (p1 (object obj))
					   (p2 (object obj)))
			 2))
	      (x (+ x (* length (r-off obj)
			 (cos alpha))))
	      (y (+ y (* length (r-off obj)
			 (sin alpha)))))
	 (values x y)))
      (otherwise
       (let* ((alpha (rho-off obj))
	      (x (+ x (* (r-off obj)
			 (cos alpha))))
	      (y (+ y (* (r-off obj)
			 (sin alpha)))))
	 (values x y))))))

(defmethod set-origin-to ((obj gui-orientation-arrow) xo yo)
  (multiple-value-bind (x y)
      (get-label-origin-for (object obj))
    (typecase (object obj)
      (line 
       (multiple-value-bind (r alpha)
	   (distance-and-orientation* x y xo yo)
	 (setf (r-off obj)
	   (/ r (/ (distance-between (p1 (object obj))
				     (p2 (object obj))) 2))
	   (rho-off obj)
	   (- alpha
	      (global-orientation (object obj))))))
      (otherwise
       (multiple-value-bind (r alpha)
	   (distance-and-orientation* x y xo yo)
	 (setf (r-off obj) r
	       (rho-off obj) alpha))))))

;;;
;;;
;;;

(defpersistentclass gui-relative-orientation-circle (gui-object)
  ((object1 :accessor object1 :initform nil :initarg :object1)
   (object2 :accessor object2 :initform nil :initarg :object2)   
   
   (r :accessor r :initform 20 :initarg :r)
   (allowed-derivation :accessor allowed-derivation :initform 0 :initarg :allowed-derivation)
   
   (ink-rgb-list :initform (list 1 0 1))))

(defmethod reinitialize ((obj gui-relative-orientation-circle))
  (setf (r obj) 20
	(allowed-derivation obj) 0))

(defmethod print-object ((obj gui-relative-orientation-circle) stream) 
  (format stream "Rel. Orient. Constr. Between ~A And ~A"	  
	  (object1 obj)
	  (object2 obj)))

;;;
;;;
;;;

(defpersistentclass gui-visco-object (gui-object visco-object)
  ((dummy-p :accessor dummy-p :initarg :dummy-p :initform nil)))

(defmethod print-object ((obj gui-visco-object) stream) 
  (let ((descr (subseq (format nil "~A"
			       (type-of obj))
		       4)))
    (if (name obj)
	(format stream "~A-~A"
		descr
		(name obj))
      (format stream "~A" descr))))

(defpersistentclass gui-query-object-or-enclosure (gui-visco-object query-object-or-enclosure)
  ((label-line-counter :initform 0 :accessor label-line-counter)))   

(defpersistentclass gui-query (gui-object query)
  ())

(defpersistentclass gui-transparency (gui-visco-object transparency)
  ((current-transparency-ink :accessor current-transparency-ink :initarg :current-transparency-ink 
			     :initform +blue+
			     :not-persistent)
   (properties-set :accessor properties-set :initform nil)))

(defmethod initialize-loaded-persistent-object :after ((obj gui-transparency))
  (setf (current-transparency-ink obj)
    +blue+))

(defpersistentclass gui-query-object (gui-query-object-or-enclosure query-object)
  ((status-label :accessor status-label :initform (make-instance 'gui-status-label))
   (semantics-label :accessor semantics-label :initform (make-instance 'gui-semantics-label))))

(defpersistentclass gui-at-least-1d-query-object (gui-query-object at-least-1d-query-object)
  ())

(defpersistentclass gui-point (gui-query-object point)
  ())

(defpersistentclass gui-marble (gui-point marble)
  ())

(defpersistentclass gui-nail (gui-point nail)
  ())

(defpersistentclass gui-origin (gui-nail origin)
  ((orientation-arrow :accessor orientation-arrow :initform (make-instance 'gui-orientation-arrow))))

(defpersistentclass gui-line (gui-at-least-1d-query-object line)
  ())

(defpersistentclass gui-rubberband (gui-line rubberband)
  ((at-most-label :accessor at-most-label :initform (make-instance 'gui-at-most-label))))

(defpersistentclass gui-atomic-rubberband (gui-line atomic-rubberband)
  ((orientation-arrow :accessor orientation-arrow :initform (make-instance 'gui-orientation-arrow))
   (relative-orientation-circles :accessor relative-orientation-circles :initform nil)))


(defpersistentclass gui-atomic-<=-rubberband (gui-atomic-rubberband atomic-<=-rubberband)
  ())

(defpersistentclass gui-atomic->=-rubberband (gui-atomic-rubberband atomic->=-rubberband)
  ())

(defpersistentclass gui-beam (gui-atomic-rubberband beam)
  ())

(defpersistentclass gui-chain-or-polygon (gui-at-least-1d-query-object chain-or-polygon)
  ((orientation-arrow :accessor orientation-arrow :initform (make-instance 'gui-orientation-arrow))
   (at-most-label :accessor at-most-label :initform (make-instance 'gui-at-most-label))))

(defpersistentclass gui-chain (gui-chain-or-polygon chain)
  ())

(defpersistentclass gui-polygon (gui-chain-or-polygon polygon)
  ())

(defpersistentclass gui-enclosure (gui-query-object-or-enclosure enclosure) ; abstrakt
  ((inside-ink :accessor inside-ink :initarg :inside-ink 
	       :not-persistent)
   (pattern-id :accessor pattern-id :initarg :pattern-id :initform nil)
   
   (at-most-label :accessor at-most-label :initform (make-instance 'gui-at-most-label))))

(defmethod initialize-loaded-persistent-object :after ((obj gui-enclosure))
  (setf (ink obj)
    (get-pattern-in (apply #'make-rgb-color (ink-rgb-list obj))
		    :opaque-p (opaque-p obj)
		    :pattern-id (pattern-id obj)))
  (setf (inside-ink obj)
    (get-pattern-in +green+ :opaque-p (opaque-p obj) 
		    :pattern-id (pattern-id obj))))

(defpersistentclass gui-derived-enclosure (gui-enclosure derived-enclosure)
  ())

(defpersistentclass gui-drawn-enclosure (gui-enclosure drawn-enclosure)
  ((drawable-pointlist :accessor drawable-pointlist :initarg :drawable-pointlist)))

(defpersistentclass gui-inner-enclosure (gui-derived-enclosure inner-enclosure)
  ((drawable-pointlist :accessor drawable-pointlist :initarg :drawable-pointlist)))

(defpersistentclass gui-outer-enclosure (gui-derived-enclosure outer-enclosure)
  ((drawable-pointlist :accessor drawable-pointlist :initarg :drawable-pointlist)))

(defpersistentclass gui-epsilon-enclosure (gui-derived-enclosure epsilon-enclosure)
  ())

(defpersistentclass gui-epsilon-p-enclosure (gui-epsilon-enclosure epsilon-p-enclosure)
  ())

(defpersistentclass gui-epsilon-m-enclosure (gui-epsilon-enclosure epsilon-m-enclosure)
  ())

;;;
;;;
;;;

(defun find-named-visco-object (name)
  (or (find name (visco-objects
		  (get-current-query))
	    :key #'name)
      (error "~A not found!" name)))

(defun find-named-history-entry (name)
  (with-visco-frame (visco)
    (or (find name (history visco)
	      :key #'name)
	(error "~A not found!" name))))

;;;
;;;
;;;

(defpersistentclass history-entry ()
  ((name :accessor name :initarg :name :initform (incf *history-name-counter*))
   (operation-descr :accessor operation-descr :initarg :operation-descr)
   (source :accessor source :initarg :source)
   (code :accessor code :initarg :code 
	 :not-persistent)
   
   (arg-objects :accessor arg-objects :initarg :arg-objects :initform nil)))

(defpersistentclass visually-relevant-history-entry (history-entry)
  ((object-to-draw :accessor object-to-draw :initarg :object-to-draw :initform nil)))

(defpersistentclass constructor-history-entry (visually-relevant-history-entry)
  ())

(defmethod print-object ((obj history-entry) stream) 
  (format stream "~A"
	  (operation-descr obj)))

;;;
;;;
;;;

(defmethod make-history-entry ((operation-descr string)
			       (code list)
			       (args list) &key name (class 'history-entry))
  (let ((obj (make-instance class
	       :operation-descr operation-descr
	       :source code
	       :arg-objects (mapcar #'name args)
	       #| :code (compile nil
	       `(lambda ()
	       ,code))))) |# )))
    (when name
      (setf (name obj) name))
    obj))


(defmethod make-constructor-history-entry ((operation-descr string)
					   (code list)
					   (args list) &key name)
  (make-history-entry operation-descr code args :name name :class 'constructor-history-entry))

(defmethod make-visually-relevant-history-entry ((operation-descr string)
						 (code list)
						 (args list) &key name)
  (make-history-entry operation-descr code args :name name :class 'visually-relevant-history-entry))


(defmethod register ((entry history-entry))
  (with-visco-frame (visco)
    (pushend entry (history visco))
    entry))

(defmethod register-history-entry ((operation-descr string)
				   (code list)
				   (args list) &key name)
  (register (make-history-entry operation-descr code args :name name)))

(defmethod register-visually-relevant-history-entry ((operation-descr string)
						     (code list)
						     (args list) &key name)
  (register (make-visually-relevant-history-entry operation-descr code args :name name)))


(defmethod register-constructor-history-entry ((operation-descr string)
					       (code list)
					       (args list) &key name)
  (register (make-constructor-history-entry operation-descr code args :name name)))

(defmethod delete-history-entry ((entry history-entry))
  (with-visco-frame (visco)
    (setf (history visco)
      (delete entry (history visco)))))

(defmethod delete-history-entries ((entries list))
  (dolist (entry entries)
    (delete-history-entry entry)))

;;;
;;;
;;;

(defmethod initialize-loaded-persistent-object :after ((history-entry history-entry))
  (setf (code history-entry)
    #| (compile nil |#
    (source history-entry)))

;;;
;;;
;;;
#|
(defmethod execute ((history-entry history-entry))
  (values (ignore-errors
	   (eval (source history-entry)))
	  history-entry))
|#

(defmethod execute ((history-entry history-entry))
  (values 
   (eval (source history-entry))
   history-entry))

(defmethod execute :around ((history-entry visually-relevant-history-entry))
  (let ((res (call-next-method)))
    (cond (res
	   (setf (object-to-draw history-entry) res
		 (history-entry res) history-entry)
	   (values res history-entry))
	  (t 
	   (values nil history-entry)))))


(defun get-status-string (symbol)
  (case symbol
    (db "DB")
    (db-component "DB-C")
    (universe "U")
    (otherwise "")))
