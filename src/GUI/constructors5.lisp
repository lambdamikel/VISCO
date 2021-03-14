;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: GUI; Base: 10 -*-

(in-package gui)

;;;
;;; Konstruktoren
;;;

(defun make-visco-gui-query ()
  (make-instance 'gui-query))

;;;
;;;
;;;

(defmethod make-visco-marble ((transparency gui-transparency) (status symbol) (x number) (y number) &rest initargs)
  (let ((obj (call-next-method)))
    (apply #'change-class obj 'gui-marble :allow-other-keys t initargs)))

(defmethod make-visco-nail ((transparency gui-transparency) (status symbol) (x number) (y number) &rest initargs)
  (let ((obj (call-next-method)))
    (apply #'change-class obj 'gui-nail :allow-other-keys t initargs)))

(defmethod make-visco-origin ((transparency gui-transparency) (status symbol) (x number) (y number) &rest initargs)
  (let ((obj (call-next-method)))
    (apply #'change-class obj 'gui-origin :allow-other-keys t initargs)))

;;;
;;;
;;;

(defmethod change-class ((obj geom-point) (type (eql 'marble)) &rest initargs)
  (apply #'change-class obj 'gui-marble :allow-other-keys t initargs))

(defmethod change-class ((obj geom-point) (type (eql 'nail)) &rest initargs)
  (apply #'change-class obj 'gui-nail :allow-other-keys t initargs))

(defmethod change-class ((obj geom-point) (type (eql 'origin)) &rest initargs)
  (apply #'change-class obj 'gui-origin :allow-other-keys t initargs))

(defmethod change-class ((obj geom-line) (type (eql 'rubberband)) &rest initargs)
  (apply #'change-class obj 'gui-rubberband :allow-other-keys t initargs))

(defmethod change-class ((obj geom-line) (type (eql 'atomic-rubberband)) &rest initargs)
  (apply #'change-class obj 'gui-atomic-rubberband :allow-other-keys t initargs))

(defmethod change-class ((obj geom-line) (type (eql 'atomic-<=-rubberband)) &rest initargs)
  (apply #'change-class obj 'gui-atomic-<=-rubberband :allow-other-keys t initargs))

(defmethod change-class ((obj geom-line) (type (eql 'atomic->=-rubberband)) &rest initargs)
  (apply #'change-class obj 'gui-atomic->=-rubberband :allow-other-keys t initargs))

(defmethod change-class ((obj geom-line) (type (eql 'beam)) &rest initargs)
  (apply #'change-class obj 'gui-beam :allow-other-keys t initargs))

;;;
;;;
;;;

(defmethod make-visco-rubberband ((transparency gui-transparency) (status symbol) (p1 point) (p2 point) &rest initargs)
  (let ((obj (call-next-method)))
    (apply #'change-class obj 'gui-rubberband :allow-other-keys t initargs)))

(defmethod make-visco-atomic-rubberband ((transparency gui-transparency) (status symbol) (p1 point) (p2 point) &rest initargs)
  (let ((obj (call-next-method)))
    (apply #'change-class obj 'gui-atomic-rubberband :allow-other-keys t initargs)))

(defmethod make-visco-atomic-<=-rubberband ((transparency gui-transparency) (status symbol) (p1 point) (p2 point) &rest initargs)
  (let ((obj (call-next-method)))
    (apply #'change-class obj 'gui-atomic-<=-rubberband :allow-other-keys t initargs)))

(defmethod make-visco-atomic->=-rubberband ((transparency gui-transparency) (status symbol) (p1 point) (p2 point) &rest initargs)
  (let ((obj (call-next-method)))
    (apply #'change-class obj 'gui-atomic->=-rubberband initargs)))

(defmethod make-visco-beam ((transparency gui-transparency) (status symbol) (p1 point) (p2 point) &rest initargs)
  (let ((obj (call-next-method)))
    (apply #'change-class obj 'gui-beam :allow-other-keys t initargs)))

;;;
;;;
;;;

(defmethod make-visco-chain ((transparency gui-transparency) (status symbol) (segments list) &rest initargs)
  (let ((obj (call-next-method)))
    (apply #'change-class obj 'gui-chain :allow-other-keys t initargs)))

(defmethod make-visco-polygon ((transparency gui-transparency) (status symbol) (segments list) &rest initargs)
  (let ((obj (call-next-method)))
    (apply #'change-class obj 'gui-polygon :allow-other-keys t initargs)))

;;;
;;;
;;;

(defmethod make-visco-drawn-enclosure ((transparency gui-transparency) (segments list) opaque-p &rest initargs)
  (declare (ignore opaque-p))
  (let ((obj (call-next-method)))
    (apply #'change-class obj 'gui-drawn-enclosure :allow-other-keys t initargs)))

(defmethod make-visco-inner-enclosure ((arg gui-polygon) opaque-p &rest initargs)
  (declare (ignore opaque-p))
  (let ((obj (call-next-method)))
    (apply #'change-class obj 'gui-inner-enclosure :allow-other-keys t initargs)))

(defmethod make-visco-outer-enclosure ((arg gui-polygon) opaque-p &rest initargs)
  (declare (ignore opaque-p))
  (let ((obj (call-next-method)))
    (apply #'change-class obj 'gui-outer-enclosure :allow-other-keys t initargs)))

(defmethod make-visco-epsilon-enclosure ((arg gui-query-object) (radius number) opaque-p &rest initargs)
  (declare (ignore opaque-p))
  (let ((obj (call-next-method)))
    (apply #'change-class obj 'gui-epsilon-enclosure :allow-other-keys t initargs)))

(defmethod make-visco-epsilon-p-enclosure ((arg gui-polygon) (radius number) opaque-p &rest initargs)
  (declare (ignore opaque-p))
  (let ((obj (call-next-method)))
    (apply #'change-class obj 'gui-epsilon-p-enclosure :allow-other-keys t initargs)))

(defmethod make-visco-epsilon-m-enclosure ((transparency gui-polygon) (radius number) opaque-p &rest initargs)
  (declare (ignore opaque-p))
  (let ((obj (call-next-method)))
    (apply #'change-class obj 'gui-epsilon-m-enclosure :allow-other-keys t initargs)))

;;;
;;;
;;;

(defmethod make-visco-transparency ((query gui-query) xmin ymin xmax ymax &rest initargs)
  (declare (ignore xmin ymin xmax ymax))
  (let ((obj (call-next-method)))
    (apply #'change-class obj 'gui-transparency :allow-other-keys t initargs)))

;;;
;;;
;;;

(defmethod initialize-instance :after ((new gui-object) &rest initargs)
  (declare (ignore initargs))
  (setf (ink new)
    (apply #'make-rgb-color (ink-rgb-list new))))

(defmethod update-instance-for-different-class :after (old
						       (new gui-object)
						       &rest initargs)
  (declare (ignore initargs))
  (unless (typep old 'gui-object)
    (unless (ink-rgb-list new)
      (setf (ink-rgb-list new)
	(calculate-gray-color new)))
    (setf (ink new)
      (apply #'make-rgb-color (ink-rgb-list new)))))

(defmethod update-instance-for-different-class :after (old
						       (new gui-query-object)
						       &rest initargs)
  (declare (ignore initargs))
  (unless (typep old 'gui-object)
    (setf (object (status-label new)) new)
    (setf (object (semantics-label new)) new)))

(defmethod update-instance-for-different-class :after (old
						       (new gui-enclosure)
						       &rest initargs)
  (declare (ignore initargs))
  (unless (typep old 'gui-object)
    (multiple-value-bind (pattern id)
	(get-pattern-in (apply #'make-rgb-color (ink-rgb-list new))
			:pattern-id (pattern-id new)
			:opaque-p (opaque-p new))
      (setf (ink new) pattern)
      (setf (pattern-id new)
	id)
      
      (when (typep new '(or gui-drawn-enclosure
			 gui-inner-enclosure
			 gui-outer-enclosure))
	(setf (drawable-pointlist new)
	  (get-drawable-pointlist-for new)))

      (setf (inside-ink new)
	(get-pattern-in (apply #'make-rgb-color (list 0 (/ (apply #'+ (ink-rgb-list new)) 3) 0))
			:pattern-id (pattern-id new)
			:opaque-p (opaque-p new))))
    (setf (object (at-most-label new)) new)))


(defmethod update-instance-for-different-class :after (old
						       (new gui-transparency)
						       &rest initargs)
  (declare (ignore initargs))
  (unless (typep old 'gui-object)
    (set-current-transparency-to new))
  #| (setf (object (properties new)) new)) |# )


(defmethod update-instance-for-different-class :after (old
						       (new gui-atomic-rubberband)
						       &rest initargs)
  (declare (ignore initargs))
  (unless (typep old 'gui-object)
    (setf (object (orientation-arrow new)) new)))

(defmethod update-instance-for-different-class :after (old
						       (new gui-origin)
						       &rest initargs)
  (declare (ignore initargs))
  (unless (typep old 'gui-object)
    (setf (object (orientation-arrow new)) new)))

(defmethod update-instance-for-different-class :after (old
						       (new gui-rubberband)
						       &rest initargs)
  (declare (ignore initargs))
  (unless (typep old 'gui-object)
    (setf (object (at-most-label new)) new)))

(defmethod update-instance-for-different-class :after (old
						       (new gui-chain-or-polygon)
						       &rest initargs)
  (declare (ignore initargs))
  (unless (typep old 'gui-object)
    (setf (object (at-most-label new)) new)
    (setf (object (orientation-arrow new)) new)))


;;;
;;;
;;;

(defmethod initialize-instance :after ((obj gui-relative-orientation-circle) &rest initargs)
  (declare (ignore initargs))
  (push obj (relative-orientation-circles (object1 obj)))
  (push obj (relative-orientation-circles (object2 obj))))

;;;
;;;
;;;

(defmethod delete-object progn ((obj gui-visco-object) &key &allow-other-keys)
  (with-visco-frame (visco)
    
    (dolist (entry (history visco))
      (when (and (eq (type-of entry) 'history-entry)
		 (member (name obj) (arg-objects entry)))
	(setf (history visco)
	  (delete entry
		  (history visco)))))
    
    (setf (history visco)
      (delete (history-entry obj)
	      (history visco)))
    (when (eq (current-focus visco) (history-entry obj))
      (set-current-focus-to nil))))


(defmethod delete-object progn ((obj gui-transparency) &key &allow-other-keys)
  (with-visco-frame (visco)
    (when (eq (current-transparency visco) obj)
      (set-current-transparency-to nil))))

;;;
;;;
;;;

(defmethod delete-object progn ((obj gui-object) &key &allow-other-keys)
  nil)

(defmethod delete-object progn ((obj gui-relative-orientation-circle) &key &allow-other-keys)
  (setf (relative-orientation-circles (object1 obj))
    (delete obj (relative-orientation-circles (object1 obj))))
  (setf (relative-orientation-circles (object2 obj))
    (delete obj (relative-orientation-circles (object2 obj)))))

;;;
;;;
;;;

(defun get-negated-drawable-pointlist (points xmin ymin xmax ymax)
  (let ((x1 (first points))
	(y1 (second points)))
    (append points
	    (list x1 y1)
	    (list xmin ymin)
	    (list xmin ymax)
	    (list xmax ymax)
	    (list xmax ymin)
	    (list xmin ymin))))

(defmethod get-negated-drawable-pointlist-for ((obj geom-polygon))
  (let* ((transparency (on-transparency obj))
	 (xmin (x (pmin transparency)))
	 (ymin (y (pmin transparency)))
	 (xmax (x (pmax transparency)))
	 (ymax (y (pmax transparency))))
    (get-negated-drawable-pointlist (mapcan #'(lambda (p) 
						(list (x p) (y p)))
					    (point-list obj))
				    xmin ymin xmax ymax)))


(defmethod get-drawable-pointlist-for ((obj geom-polygon))
  (mapcan #'(lambda (p) 
	      (list (x p) (y p)))
	  (point-list obj)))

(defmethod get-drawable-pointlist-for ((obj gui-drawn-enclosure))
  (if (negated-p obj)
      (get-negated-drawable-pointlist-for obj)
    (call-next-method)))

(defmethod get-drawable-pointlist-for ((obj gui-inner-enclosure))
  (get-drawable-pointlist-for (polygon obj)))

(defmethod get-drawable-pointlist-for ((obj gui-outer-enclosure))
  (let* ((transparency (on-transparency obj))
	 (xmin (x (pmin transparency)))
	 (ymin (y (pmin transparency)))
	 (xmax (x (pmax transparency)))
	 (ymax (y (pmax transparency))))
    (get-negated-drawable-pointlist 
     (get-drawable-pointlist-for (polygon obj))
     xmin ymin xmax ymax)))


