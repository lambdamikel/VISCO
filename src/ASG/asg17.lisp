;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: QUERY-COMPILER; Base: 10 -*-

(in-package query-compiler)

(defconstant +intersects-threshold+ 4)

(defconstant +inner-enclosure-r-fac+ 8)

(defconstant +outer-enclosure-r-fac+ 8)

;;;
;;;
;;;

(defpersistentclass compiler-object (name-mixin) ; abstrakt !!!!
  ((exec-fn :accessor exec-fn :initform nil
	    :not-persistent)
   (exec-source :accessor exec-source
		:not-persistent)
   (activated-p :accessor activated-p :initform nil
		:not-persistent)
   (saved-group-state :accessor saved-group-state :initform nil
		      :not-persistent)
   (compiler-conditions :accessor compiler-conditions :initform nil
			:not-persistent)
   (sorted-generators :accessor sorted-generators 
		      :not-persistent)
   (sorted-testers :accessor sorted-testers 
		   :not-persistent)
   (activated-and-sorted-compiler-conditions :accessor activated-and-sorted-compiler-conditions 
					     :not-persistent)))

(defmethod initialize-loaded-persistent-object :after ((obj compiler-object))
  (setf (exec-fn obj) nil))

;;;
;;;
;;;


(defparameter *name-counter* 0)

(defun get-next-name ()
  (prog1 *name-counter*
    (incf *name-counter*)))

;;;
;;;
;;;

(defpersistentclass name-mixin ()
  ((name :accessor name :initform nil
	 :initarg :name)))

(defpersistentclass on-transparency-mixin ()
  ((on-transparency :accessor on-transparency :initarg :on-transparency)))

(defpersistentclass possible-operator-result-mixin ()
  ((res-of-operators :accessor res-of-operators :initarg :res-of-operators :initform nil)))

(defpersistentclass possible-operator-argument-mixin ()   
  ((arg-of-operators :accessor arg-of-operators :initform nil)))

(defpersistentclass orientation-constraint-mixin ()
  ((orientation-constraint :accessor orientation-constraint :initarg :orientation-constraint
			   :initform nil)))

(defpersistentclass at-most-constraint-mixin ()
  ((at-most-constraint :accessor at-most-constraint :initarg :at-most-constraint :initform nil)))

(defpersistentclass at-most-has-segments-constraint-mixin (at-most-constraint-mixin)
  ())

(defpersistentclass at-most-contains-constraint-mixin (at-most-constraint-mixin)
  ())

;;;
;;;
;;;

(defpersistentclass query (compiler-object) ; unvollstaendig: nur ein Transparency erlaubt!
  ((visco-objects :accessor visco-objects :initform nil)
   (all-compiler-conditions :accessor all-compiler-conditions :initform nil)))

(defmethod  get-transparencies ((query query))
  (remove-if-not #'(lambda (obj)
		     (typep obj 'transparency))
		 (visco-objects query)))

(defmethod visco-objects ((obj null))
  nil)

;;;
;;;
;;;

(defpersistentclass visco-object (compiler-object) ; abstrakt
  ((query :accessor query :initarg :query)))

(defmethod print-object ((obj visco-object) stream) 
  (if (name obj)
      (format stream "~A-~A"
	      (type-of obj)
	      (name obj))
    (format stream "~A" (type-of obj))))

(defpersistentclass transparency (visco-object geom-polygon)
  ((transparency-query-objects-and-enclosures :accessor transparency-query-objects-and-enclosures :initform nil)      
   
   (child-transparencies :accessor child-transparencies :initform nil)
   (parent-transparency :accessor parent-transparency)

   (sx :accessor sx :initarg :sx :initform 1.0) ; Parameter der aktuellen Transparency-Transformation (Matrix, Matrix-1)
   (sy :accessor sy :initarg :sy :initform 1.0)
   (r :accessor r :initarg :r :initform 0)
   
   (sx-s->w :accessor sx-s->w :initform 1.0) ; INITIALE Skalierungsfaktoren Screenkoordinaten -> Weltkoordinaten 
   (sy-s->w :accessor sy-s->w :initform 1.0) ; sxmin, symin, sxmax, symax beziehen sich auf sx-s->w, sy-s->w
					; (z.B. BB-Width=1oo Pixel und Width=20 m. => sx-s->w = 20/100 = 1/5)
   
					; Unterschied: berechnete totale Skalierung:
					; sx = Weltkoordinaten / Pixelkoordinaten
					; sy = entsp. 
					; (sx, sy) f. Matrix : Pixel -> Welt
					; Constraints fuer sx-s->w, sy-s->w: 
					; sxmin <= (/ sx sx-s->w) <= sxmax
					; symin <= (/ sy sy-s->w) <= symax
   
   (epsilon-a :accessor epsilon-a)	; Metrik: r(dx,dy) = (sqrt (+ (exp (/ dx epsilon-a) 2) (exp (/ dy epsilon-b) 2))), 
   (epsilon-b :accessor epsilon-b)	; epsilon-a = (- (* sx (cos r)) (* sy (sin r)))
					; epsilon-b = (+ (* sx (sin r)) (* sy (cos r)))
   
   (matrix :accessor matrix :initform (make-matrix)) ; Bildkoordinaten (Pixel) =>  Weltkoordinaten (Meter)
   (inverse-matrix :accessor inverse-matrix :initform (make-matrix)) ; Weltkoordinaten (Meter) => Bildkoordinaten
   
   (width :accessor width :initarg :width :initform nil) ; in Metern
   (height :accessor height :initarg :height :initform nil)
   
   (sxmin :accessor sxmin :initarg :sxmin :initform nil)
   (sxmax :accessor sxmax :initarg :sxmax :initform nil)
   
   (symin :accessor symin :initarg :symin :initform nil)
   (symax :accessor symax :initarg :symax :initform nil)
   
   (sxsy-constraint :accessor sxsy-constraint :initarg :sxsy-constraint :initform nil)
   
   (origin :accessor origin :initform nil) ; wird vom Compiler gefuellt
   (1st-nail :accessor 1st-nail :initform nil) ; s.o.
   (2nd-nail :accessor 2nd-nail :initform nil))) ; s.o.   

(defpersistentclass query-object-or-enclosure (visco-object constraints-mixin on-transparency-mixin) ; abstrakt
  ((transparency :accessor transparency :initarg :transparency)))

(defpersistentclass query-object (query-object-or-enclosure) ; abstrakt
  ((status :accessor status :initarg :status)
   (semantics :accessor semantics :initarg :semantics :initform nil)   
   (bound-to :accessor bound-to)))

(defpersistentclass at-least-1d-query-object (query-object) ; abstrakt
  ())

;;;
;;;
;;;

(defpersistentclass point (query-object geom-point) ; abstrakt
  ((ignore-disjoint-and-intersects-relations-p 
    :accessor ignore-disjoint-and-intersects-relations-p :initform nil 
    :initarg :ignore-disjoint-and-intersects-relations-p)))


(defpersistentclass marble (point 
			    possible-operator-result-mixin 
			    possible-operator-argument-mixin)
  ())


(defpersistentclass nail (point 
			  possible-operator-result-mixin 
			  possible-operator-argument-mixin)
  ())


(defpersistentclass origin (nail orientation-constraint-mixin)
  ((parent-fixed-p :accessor parent-fixed-p :initform nil)))   

;;;
;;;
;;;

(defpersistentclass line (at-least-1d-query-object geom-line) ; abstrakt
  ((ignore-disjoint-and-intersects-relations-p 
    :accessor ignore-disjoint-and-intersects-relations-p :initform nil
    :initarg :ignore-disjoint-and-intersects-relations-p)
   
   (1d-intersects-other-lines-p :accessor 1d-intersects-other-lines-p :initform nil)))

(defpersistentclass rubberband (line
				at-most-has-segments-constraint-mixin
				possible-operator-argument-mixin)
  ())

(defpersistentclass atomic-rubberband (line
				       possible-operator-argument-mixin orientation-constraint-mixin)
  ())

(defpersistentclass atomic-<=-rubberband (atomic-rubberband)
  ())

(defpersistentclass atomic->=-rubberband (atomic-rubberband)
  ())

(defpersistentclass beam (atomic-rubberband)
  ())

;;;
;;;
;;;

(defpersistentclass chain-or-polygon (at-least-1d-query-object ; abstrakt
				      possible-operator-argument-mixin
				      at-most-has-segments-constraint-mixin
				      orientation-constraint-mixin
				      geom-chain-or-polygon)
  ())

(defpersistentclass chain (chain-or-polygon geom-chain)			   
  ())


(defpersistentclass polygon (chain-or-polygon geom-polygon)
  ())

;;;
;;;
;;;

(defpersistentclass enclosure (query-object-or-enclosure ; abstrakt
			       at-most-contains-constraint-mixin)
  ((opaque-p :accessor opaque-p :initarg :opaque-p :initform nil)))

(defpersistentclass derived-enclosure (enclosure possible-operator-result-mixin) ; abstrakt
  ((arg-object :accessor arg-object :initform nil :initarg :arg-object)))

(defmethod print-object ((obj derived-enclosure) stream)
  (format stream "~A Of ~A" (apply #'call-next-method obj nil nil) (arg-object obj)))

(defpersistentclass drawn-enclosure (enclosure geom-polygon)
  ((negated-p :accessor negated-p :initarg :negated-p :initform nil)))

(defpersistentclass inner-enclosure (derived-enclosure)
  ((polygon :accessor polygon :initarg :polygon)))

(defpersistentclass outer-enclosure (derived-enclosure)
  ((polygon :accessor polygon :initarg :polygon)))

(defpersistentclass epsilon-enclosure (derived-enclosure)
  ((radius :accessor radius :initarg :radius)))

;;;
;;;
;;;

(defun get-shrinked-polygon (arg-obj &optional (r +inner-enclosure-r-fac+))
  (let ((pl (point-list arg-obj)))
    (poly 
     (make-segments
      (mapcar #'(lambda (p1 p2 p3)
		  (let* ((s1 (angle-between* (x p1) (y p1)
					     (x p2) (y p2)))
			 (s2 (angle-between* (x p2) (y p2)
					     (x p3) (y p3)))
			 (s1s2 (/ (+ s1 s2) 2))
			 (x (x p2))
			 (y (y p2))
			 
			 (x1a (+ x (* r (cos (+ +pi/2+ s1s2)))))
			 (y1a (+ y (* r (sin (+ +pi/2+ s1s2)))))
			 
			 (x1b (+ x (* r (cos (+ s1s2 (- +pi/2+))))))
			 (y1b (+ y (* r (sin (+ s1s2 (- +pi/2+)))))))
		    
		    (if (inside-p* x1a y1a arg-obj)
			(if (plusp r) 
			    (list x1a y1a)
			  (list x1b y1b))
		      (if (plusp r) 			
			  (list x1b y1b)
			(list x1a y1a)))))
	      pl
	      (append (cdr pl) (list (first pl)))
	      (append (cddr pl) (list (first pl) (second pl))))))))


(defun get-expanded-polygon (arg-obj &optional (r +outer-enclosure-r-fac+))
  (let ((pl (point-list arg-obj)))
    (poly
     (make-segments 
      (mapcar #'(lambda (p1 p2 p3)
		  (let* ((s1 (angle-between* (x p1) (y p1)
					     (x p2) (y p2)))
			 (s2 (angle-between* (x p2) (y p2)
					     (x p3) (y p3)))
			 (s1s2 (/ (+ s1 s2) 2))
			 (x (x p2))
			 (y (y p2))
			 
			 (x1a (+ x (* r (cos (+ +pi/2+ s1s2)))))
			 (y1a (+ y (* r (sin (+ +pi/2+ s1s2)))))
			 
			 (x1b (+ x (* r (cos (+ s1s2 (- +pi/2+))))))
			 (y1b (+ y (* r (sin (+ s1s2 (- +pi/2+)))))))
		    
		    (if (not (inside-p* x1a y1a arg-obj))
			(if (plusp r) 
			    (list x1a y1a)
			  (list x1b y1b))
		      (if (plusp r) 			
			  (list x1b y1b)
			(list x1a y1a)))))
	      pl
	      (append (cdr pl) (list (first pl)))
	      (append (cddr pl) (list (first pl) (second pl))))))))


(defun make-segments (points)
  (mapcar #'(lambda (p1 p2)
	      (let ((p1 (p (first p1) (second p1)))
		    (p2 (p (first p2) (second p2))))
		(l p1 p2)))
	  (cons (first (last points)) points) 
	  points))

;;;
;;; Konstruktoren
;;;

(defun make-visco-query ()
  (make-instance 'query))

(defmethod make-visco-marble ((transparency transparency) (status symbol) (x number) (y number) &rest initargs)
  (apply #'make-instance 'marble :query (query transparency) :on-transparency transparency :x x :y y :status status
	 initargs))

(defmethod make-visco-nail ((transparency transparency) (status symbol) (x number) (y number) &rest initargs)
  (apply #'make-instance 'nail :query (query transparency) :on-transparency transparency :x x :y y :status status
	 initargs))

(defmethod make-visco-origin ((transparency transparency) (status symbol) (x number) (y number) &rest initargs)
  (apply #'make-instance 'origin :query (query transparency) :on-transparency transparency :x x :y y :status status
	 initargs))

(defmethod make-visco-op-derived-point ((transparency transparency) (status symbol) (x number) (y number) &rest initargs)
  (apply (get-constructor-for-derived-point x y (query transparency))
	 transparency status x y initargs))

(defun get-constructor-for-derived-point (x y query)  
  (if (inside-any-enclosure-p* x y query)
      #'make-visco-marble
    #'make-visco-nail))

(defun get-classname-for-derived-point (x y query)
  (if (inside-any-enclosure-p* x y query)
      'marble
    'nail))

;;;
;;;
;;;

(defmethod make-visco-rubberband ((transparency transparency) (status symbol) (p1 point) (p2 point) &rest initargs)
  (apply #'make-instance 'rubberband :query (query transparency) :on-transparency transparency :p1 p1 :p2 p2 :status status
	 initargs))

(defmethod make-visco-atomic-rubberband ((transparency transparency) (status symbol) (p1 point) (p2 point) &rest initargs)
  (apply #'make-instance 'atomic-rubberband :query (query transparency) :on-transparency transparency :p1 p1 :p2 p2 :status status
	 initargs))

(defmethod make-visco-atomic-<=-rubberband ((transparency transparency) (status symbol) (p1 point) (p2 point) &rest initargs)
  (apply #'make-instance 'atomic-<=-rubberband :query (query transparency) :on-transparency transparency :p1 p1 :p2 p2 :status status
	 initargs))

(defmethod make-visco-atomic->=-rubberband ((transparency transparency) (status symbol) (p1 point) (p2 point) &rest initargs)
  (apply #'make-instance 'atomic->=-rubberband :query (query transparency) :on-transparency transparency :p1 p1 :p2 p2 :status status
	 initargs))

(defmethod make-visco-beam ((transparency transparency) (status symbol) (p1 point) (p2 point) &rest initargs)
  (apply #'make-instance 'beam :query (query transparency) :on-transparency transparency :p1 p1 :p2 p2 :status status
	 initargs))

;;;
;;;
;;;

(defmethod make-visco-chain ((transparency transparency) (status symbol) (segments list) &rest initargs)
  (apply #'make-instance 'chain :query (query transparency) :on-transparency transparency :segments segments :status status
	 initargs))

(defmethod make-visco-polygon ((transparency transparency) (status symbol) (segments list) &rest initargs)
  (apply #'make-instance 'polygon :query (query transparency) :on-transparency transparency :segments segments :status status
	 initargs))

;;;
;;;
;;;

(defmethod make-visco-drawn-enclosure ((transparency transparency) (segments list) opaque-p &rest initargs)
  (apply #'make-instance 'drawn-enclosure 
	 :query (query transparency)
	 :on-transparency transparency
	 :segments segments
	 :opaque-p opaque-p
	 initargs))

(defmethod make-visco-inner-enclosure ((arg polygon) opaque-p &rest initargs)
  (apply #'make-instance 'inner-enclosure 
	 :query (query arg)
	 :on-transparency (on-transparency arg)
	 :arg-object arg
	 :polygon (get-shrinked-polygon arg)
	 :opaque-p opaque-p
	 initargs))

(defmethod make-visco-outer-enclosure ((arg polygon) opaque-p &rest initargs)
  (apply #'make-instance 'outer-enclosure
	 :query (query arg)
	 :on-transparency (on-transparency arg)
	 :arg-object arg
	 :polygon (get-expanded-polygon arg)
	 :opaque-p opaque-p
	 initargs))

(defmethod make-visco-epsilon-enclosure ((arg query-object) (radius number) opaque-p &rest initargs)
  (apply #'make-instance 'epsilon-enclosure 
	 :query (query arg)	 
	 :on-transparency (on-transparency arg)
	 :arg-object arg
	 :radius radius
	 :opaque-p opaque-p
	 initargs))

;;;
;;;
;;;

(defmethod make-visco-transparency ((query query) xmin ymin xmax ymax &rest initargs)
  (apply #'make-instance 'transparency 	 
	 :segments (let ((p1 (p xmin ymin))
			 (p2 (p xmax ymin))
			 (p3 (p xmax ymax))
			 (p4 (p xmin ymax)))
		     (list (l p1 p2)
			   (l p2 p3)
			   (l p3 p4)
			   (l p4 p1)))
	 #| :bounding-box-p nil |#	; funktioniert nun! bei Matrixaenderung => reset-bb 
	 #| :xmin xmin :ymin ymin 
	 :xmax xmax :ymax ymax |#
	 :query query
	 initargs))

;;;
;;;
;;;

(defmethod get-objects-above ((obj query-object-or-enclosure))
  (rest (member obj (get-all-query-objects-and-enclosures obj))))

(defmethod get-all-query-objects-and-enclosures ((obj visco-object))
  (remove-if-not #'(lambda (obj)
		     (typep obj 'query-object-or-enclosure))
		 (visco-objects 
		  (query obj))))

(defmethod get-all-query-objects-and-enclosures ((obj query))
  (remove-if-not #'(lambda (obj)
		     (typep obj 'query-object-or-enclosure))
		 (visco-objects obj)))

(defmethod get-all-query-objects-and-enclosures ((obj null))
  nil)

;;;
;;;
;;;

(defmethod initialize ((new-obj query-object-or-enclosure) &rest initargs)
  (declare (ignore initargs))
  (dolist (obj2 (get-all-query-objects-and-enclosures new-obj))
    (calculate-and-store-relation new-obj obj2))
  (push new-obj (transparency-query-objects-and-enclosures (on-transparency new-obj))))

(defmethod initialize ((new-obj chain-or-polygon) &rest initargs)
  (declare (ignore initargs))  
  (dolist (segment (segments new-obj))
    (dolist (cs (constraints segment))
      (let ((obj2 (2nd-arg cs)))
	(when (and (typep cs 'intersects)
		   (not (eq obj2 new-obj))
		   (not (component-p obj2 new-obj)))
	  (make-and-memoize-binary-constraint 'intersects new-obj obj2)))))
  
  (when (every #'(lambda (segment)
		   (some #'(lambda (cs)
			     (typep cs 'inside))
			 (constraints segment)))
	       (segments new-obj))
    (let ((containing-enclosures
	   (reduce #'intersection 
		   (mapcar #'(lambda (segment)
			       (mapcar #'2nd-arg
				       (remove-if-not #'(lambda (cs)
							  (typep cs 'inside))
						      (constraints segment))))
			   (segments new-obj)))))
      (dolist (enclosure containing-enclosures)
	(make-and-memoize-binary-constraint 'inside new-obj enclosure))))
  
  (when (every #'(lambda (segment)
		   (some #'(lambda (cs)
			     (typep cs 'disjoint))
			 (constraints segment)))
	       (segments new-obj))
    (let ((disjoint-objects
	   (reduce #'intersection 
		   (mapcar #'(lambda (segment)
			       (mapcar #'2nd-arg
				       (remove-if-not #'(lambda (cs)
							  (typep cs 'disjoint))
						      (constraints segment))))
			   (segments new-obj)))))
      (dolist (obj2 disjoint-objects)
	(make-and-memoize-binary-constraint 'disjoint new-obj obj2))))
  
  (push new-obj (transparency-query-objects-and-enclosures (on-transparency new-obj))))

(defmethod initialize ((new-obj derived-enclosure) &rest initargs)
  (declare (ignore initargs))
  (let ((arg-obj (arg-object new-obj)))
    (dolist (obj2 (get-all-query-objects-and-enclosures new-obj))
      (unless (eq obj2 arg-obj)
	(calculate-and-store-relation new-obj obj2))))
  (push new-obj (transparency-query-objects-and-enclosures (on-transparency new-obj))))

(defmethod initialize ((new-obj visco-object) &rest initargs)
  (declare (ignore initargs))
  t)

;;;
;;;
;;;

(defmethod initialize :after ((new-obj visco-object) &rest initargs)
  (declare (ignore initargs))
  (invalidate (query new-obj))
  (pushend new-obj (visco-objects (query new-obj))))

(defmethod initialize :after ((new-obj origin) &rest initargs)
  (declare (ignore initargs))
  (setf (origin (on-transparency new-obj)) new-obj))

(defmethod initialize :after ((new-obj name-mixin) &rest initargs)
  (declare (ignore initargs))
  (unless (name new-obj)
    (setf (name new-obj) 
      (get-next-name))))

(defmethod initialize :after ((new-obj chain-or-polygon) &rest initargs &key ignore-disjoint-and-intersects-component-relations-p)
  (declare (ignore initargs))
  (when ignore-disjoint-and-intersects-component-relations-p
    (dolist (s (segments new-obj))
      (setf (ignore-disjoint-and-intersects-relations-p s) t))))

(defmethod initialize :after ((new-obj line) &rest initargs &key ignore-disjoint-and-intersects-component-relations-p)
  (declare (ignore initargs))
  (when ignore-disjoint-and-intersects-component-relations-p   
    (setf (ignore-disjoint-and-intersects-relations-p (p1 new-obj)) t
	  (ignore-disjoint-and-intersects-relations-p (p2 new-obj)) t)))

;;;
;;;
;;;

(defmethod initialize-instance :after ((new-obj visco-object) &rest initargs &key &allow-other-keys)
  (apply #'initialize new-obj :allow-other-keys t initargs))

;;;
;;;
;;;


(defmethod update-instance-for-different-class :after ((old geom-thing)
						       (new query-object-or-enclosure)
						       &key
						       &allow-other-keys)
  (unless (typep old 'visco-object)
    (initialize new)))

;;;
;;;
;;;

(defmethod delete-object progn ((obj constraints-mixin) &key)
  (dolist (constraint (constraints obj))
    (let ((obj2 (2nd-arg constraint)))
      (remove-eventually-present-constraints obj obj2))))

(defmethod delete-object progn ((obj query-object) &key (delete-component-objects-p t))
  (dolist (part (get-direct-components obj))
    (setf (part-of part)
      (delete obj (part-of part)))
    (when (and delete-component-objects-p 
	       (primary-p part)
	       (=> (typep part 'possible-operator-result-mixin)
		   (not (res-of-operators part))))
      (delete-object part))))

(defmethod delete-object progn ((obj drawn-enclosure) &key)
  (dolist (part (get-direct-components obj))
    (setf (part-of part)
      (delete obj (part-of part)))))

(defmethod delete-object progn ((obj origin) &keY)
  (setf (origin (on-transparency obj)) nil))

(defmethod delete-object progn ((obj transparency) &key (delete-component-objects-p t))
  (when delete-component-objects-p
    (dolist (transparency-obj (transparency-query-objects-and-enclosures obj))
      (delete-object transparency-obj))))

(defmethod delete-object progn ((obj visco-object) &key)
  (let ((query (query obj)))
    (invalidate query)
    (setf (visco-objects query)
      (delete obj (visco-objects query)))))

(defmethod delete-object progn ((obj on-transparency-mixin) &key)
  (let ((transparency (on-transparency obj)))
    (setf (transparency-query-objects-and-enclosures transparency)
      (delete obj (transparency-query-objects-and-enclosures transparency)))))

(defmethod delete-object progn ((obj possible-operator-result-mixin) &key)
  (dolist (op (res-of-operators obj))
    (dolist (arg (args op))
      (setf (arg-of-operators arg)
	(delete op
		(arg-of-operators arg))))))

(defmethod delete-object progn ((obj possible-operator-argument-mixin) &key)
  (dolist (dependent (mapcar #'res (arg-of-operators obj)))
    (delete-object dependent)))

;;;
;;; Achtung: new-obj ist stets vollstaendig sichtbar!
;;; Hier werden zunaechst alle sichtbaren Relationen erzeugt, unabh. davon, ob
;;; die Relation relevant ist oder redundant. Die Oberflaeche macht KEINE INFERENZEN! (bis auf WYSIWYG-Bestimmung)
;;; Erst der Compiler nimmt dann evtl. eine Normalisierung vor und entfernt redundante
;;; Constraints. Die einzigen Relationen, die nicht angelegt werden, sind Relationen
;;; zwischen Master-Komponente-Objekten. 
;;;

(defmethod calculate-and-store-relation ((new-obj point) (obj2 point))
  #| (make-and-memoize-binary-constraint 'disjoint new-obj obj2)) |# )

(defmethod calculate-and-store-relation ((new-obj point) (obj2 line))
  (if (and (< (distance-between new-obj obj2) +intersects-threshold+)
	   (fully-visible-p new-obj (get-objects-above obj2)))
      (make-and-memoize-binary-constraint 'intersects new-obj obj2)
    (when (fully-visible-p obj2)
      (make-and-memoize-binary-constraint 'disjoint new-obj obj2))))

(defmethod calculate-and-store-relation ((new-obj line) (obj2 point))
  (when (fully-visible-p obj2)
    (if (< (distance-between new-obj obj2) +intersects-threshold+)
	(unless (component-p obj2 new-obj)
	  (make-and-memoize-binary-constraint 'intersects new-obj obj2))
      (make-and-memoize-binary-constraint 'disjoint new-obj obj2))))

(defmethod calculate-and-store-relation ((new-obj line) (obj2 line))
  (let ((rel (calculate-relation new-obj obj2 :detailed nil)))
    (case rel
      (disjoint
       (if (or (find-constraint 'intersects (p1 new-obj) obj2)
	       (find-constraint 'intersects (p2 new-obj) obj2)
	       (< (distance-between new-obj obj2) +intersects-threshold+))
	   (make-and-memoize-binary-constraint 'intersects new-obj obj2)
	 (when (fully-visible-p obj2)
	   (make-and-memoize-binary-constraint 'disjoint new-obj obj2))))
      ((touches crosses)
       (multiple-value-bind (ix iy)
	   (calculate-intersection-point new-obj obj2)
	 (when (point-visible-p* ix iy
				 (get-objects-above obj2))
	   (make-and-memoize-binary-constraint 'intersects new-obj obj2))))
      (1d-intersects
       (setf (1d-intersects-other-lines-p new-obj) t)
       (multiple-value-bind (x1 y1 x2 y2)
	   (calculate-intersection-line new-obj obj2)
	 (when (one-part-visible-p* x1 y1 x2 y2 
				    (get-objects-above obj2)
				    (query obj2))
	   (make-and-memoize-binary-constraint 'intersects new-obj obj2)))))))

;;;
;;; ENCLOSURES
;;;


(defmethod calculate-and-store-relation ((new-obj query-object) (obj2 enclosure))
  (when (and (or (typep new-obj 'point)
		 (typep new-obj 'line))
	     (asg-inside-p new-obj obj2)
	     (fully-visible-p new-obj (get-objects-above obj2)))
    (make-and-memoize-binary-constraint 'inside new-obj obj2)))

(defmethod calculate-and-store-relation ((new-obj enclosure) (obj2 query-object))
  (when (and (not (opaque-p new-obj))
	     (or (typep obj2 'point)
		 (typep obj2 'line))
	     (asg-inside-p obj2 new-obj)
	     (fully-visible-p obj2)
	     (=> (typep new-obj 'derived-enclosure)
		 (and (not (eq obj2 (arg-object new-obj)))
		      (not (component-p obj2 
					(arg-object new-obj))))))
    (make-and-memoize-binary-constraint 'contains new-obj obj2)))

;;;
;;;
;;;

(defmethod calculate-and-store-relation ((new-obj enclosure) (obj2 chain-or-polygon))
  (when (every #'(lambda (segment)	; da die Relationen fuer die Segment vorher berechnet werden => OK!
		   (find-constraint 'inside segment new-obj))
	       (segments obj2))
    (make-and-memoize-binary-constraint 'contains new-obj obj2)))

(defmethod memoize-constraints-to-chain-or-polygon ((new-obj query-object) (obj2 chain-or-polygon))
  (when (some #'(lambda (segment)
		  (find-constraint 'intersects segment new-obj))
	      (segments obj2))
    (make-and-memoize-binary-constraint 'intersects new-obj obj2))  
  (when (every #'(lambda (segment)
		   (find-constraint 'disjoint segment new-obj))
	       (segments obj2))
    (make-and-memoize-binary-constraint 'disjoint new-obj obj2)))

(defmethod calculate-and-store-relation ((new-obj point) (obj2 chain-or-polygon))
  (memoize-constraints-to-chain-or-polygon new-obj obj2))

(defmethod calculate-and-store-relation ((new-obj line) (obj2 chain-or-polygon))
  (memoize-constraints-to-chain-or-polygon new-obj obj2))

;;;
;;;
;;;

(defmethod calculate-and-store-relation ((new-obj origin) (obj2 transparency))
  nil)					; Folienhierarchie aufbauen ! 

(defmethod calculate-and-store-relation ((new-obj visco-object) (obj2 visco-object))
  nil)					; nichts tun !

;;;
;;;
;;;

(defmethod asg-inside-p ((obj geom-thing) (enclosure drawn-enclosure))
  (if (negated-p enclosure)
      (outside-p obj enclosure)
    (inside-p obj enclosure)))

(defmethod asg-inside-p ((obj geom-thing) (enclosure inner-enclosure))
  (inside-p obj (polygon enclosure)))

(defmethod asg-inside-p ((obj geom-thing) (enclosure outer-enclosure))
  (outside-p obj (polygon enclosure)))

(defmethod asg-inside-p ((obj geom-thing) (enclosure epsilon-enclosure))
  (inside-epsilon-p obj (arg-object enclosure) (radius enclosure)))

;;;
;;;
;;;

(defmethod asg-inside-p* (x y (enclosure drawn-enclosure))
  (if (negated-p enclosure)   
      (outside-p* x y enclosure)
    (inside-p* x y enclosure)))

(defmethod asg-inside-p* (x y (enclosure inner-enclosure))
  (inside-p* x y (polygon enclosure)))

(defmethod asg-inside-p* (x y (enclosure outer-enclosure))
  (outside-p* x y (polygon enclosure)))

(defmethod asg-inside-p* (x y (enclosure epsilon-enclosure))
  (inside-epsilon-p* x y (arg-object enclosure) (radius enclosure)))

;;;
;;;
;;;

(defmethod asg-outside-p ((obj geom-thing) (enclosure drawn-enclosure))
  (if (negated-p enclosure)
      (inside-p obj enclosure)
    (outside-p obj enclosure)))

(defmethod asg-outside-p ((obj geom-thing) (enclosure inner-enclosure))
  (outside-p obj (polygon enclosure)))

(defmethod asg-outside-p ((obj geom-thing) (enclosure outer-enclosure))
  (inside-p obj (polygon enclosure)))

(defmethod asg-outside-p ((obj geom-thing) (enclosure epsilon-enclosure))
  (not (inside-epsilon-p obj (arg-object enclosure) (radius enclosure))))

;;;
;;;
;;;

(defmethod asg-outside-p* (x y (enclosure drawn-enclosure))
  (if (negated-p enclosure)
      (inside-p* x y enclosure)
    (outside-p* x y enclosure)))

(defmethod asg-outside-p* (x y (enclosure inner-enclosure))
  (outside-p* x y (polygon enclosure)))

(defmethod asg-outside-p* (x y (enclosure outer-enclosure))
  (inside-p* x y (polygon enclosure)))

(defmethod asg-outside-p* (x y (enclosure epsilon-enclosure))
  (not (inside-epsilon-p* x y (arg-object enclosure) (radius enclosure))))

;;;
;;;
;;;

(defmethod point-visible-p ((point point) &optional (objects-above (get-objects-above point)))
  (point-visible-p* (x point) (y point)
		    objects-above))


(defun point-visible-p* (x y objects-above)
  (dolist (obj objects-above)
    (when (and (typep obj 'enclosure)
	       (opaque-p obj)
	       (asg-inside-p* x y obj))
      (return-from point-visible-p* nil)))
  t)


(defmethod fully-visible-p ((obj point) &optional (objects-above (get-objects-above obj)))
  (point-visible-p obj objects-above))

(defmethod fully-visible-p ((line geom-line) &optional (objects-above (get-objects-above line)))
  (dolist (obj objects-above)
    (when (and (typep obj 'enclosure)
	       (opaque-p obj)
	       (asg-inside-p line obj))
      (return-from fully-visible-p nil)))
  t)


(defun one-part-visible-p* (x1 y1 x2 y2 objects-above &optional (threshold 2.0))
  (let ((xm (/ (+ x1 x2) 2))
	(ym (/ (+ y1 y2) 2)))
    (or (point-visible-p* xm ym objects-above)
	(and (>= (distance-between* x1 y1 x2 y2) threshold)
	     (or (one-part-visible-p* x1 y1 xm ym objects-above threshold)
		 (one-part-visible-p* xm ym x2 y2 objects-above threshold))))))


(defun inside-any-enclosure-p* (x y query)
  (some #'(lambda (obj)
	    (when (typep obj 'enclosure)
	      (asg-inside-p* x y obj)))
	(get-all-query-objects-and-enclosures query)))

;;;
;;;
;;;

(defmethod get-present-point-at ((query query) (x number) (y number))
  (dolist (obj2 (get-all-query-objects-and-enclosures query))
    (when (and (typep obj2 'geom-point)
	       (= x (x obj2))
	       (= y (y obj2)))
      (return-from get-present-point-at obj2))))

;;;
;;;
;;;

(defmethod sx-type ((transparency transparency))
  (when (or (not (width transparency))
	    (not (sxmin transparency))
	    (not (sxmax transparency))
	    (and (sxmin transparency)
		 (sxmax transparency)
		 (not (= (sxmin transparency) (sxmax transparency)))))
    'free))

(defmethod sy-type ((transparency transparency))
  (when (or (not (height transparency))
	    (not (symin transparency))
	    (not (symax transparency))
	    (and (symin transparency)
		 (symax transparency)
		 (not (= (symin transparency) (symax transparency)))))
    (if (and (sxsy-constraint transparency)
	     (eq (sx-type transparency) 'free))
	'fac
      'free)))

(defmethod rot-type ((transparency transparency))
  (unless (equal '(0) (orientation-constraint (origin transparency)))
    'free))

(defmethod no-of-required-nails-without-origin ((transparency transparency))
  (let ((sxt (sx-type transparency))
	(syt (sy-type transparency))
	(rt (rot-type transparency)))
    (cond ((or (and (not sxt) (not syt) (not rt))
	       (not (rest (transparency-query-objects-and-enclosures transparency))))
	   0)
	  ((or (and (not sxt) (not syt) rt)
	       (and sxt (not syt))
	       (and (not sxt) syt)
	       (and sxt (eq syt 'fac))
	       (and sxt syt (not rt)))	; Achtung: es wird verlangt, dass hier ueber einen Punkt instantiiert wird!
	   1)
	  ((and sxt syt rt)
	   2)	  
	  (t (error "!")))))

;;;
;;;
;;;

(defmethod bound-to ((obj transparency))
  obj)

(defmethod bound-to ((obj derived-enclosure))
  (arg-object obj))

(defmethod bound-to ((obj drawn-enclosure))
  obj)

(defmethod on-transparency ((obj transparency)) ; ???
  obj) 

;;;
;;; Achtung: unvollstaendig, da die Constraints z.B. Marbles zu Nails machen koennen, etc. 
;;; Normalisierung (Inferenz) fehlt!
;;;

(defmethod may-vary-p ((obj point))
  (typep obj 'marble))

(defmethod may-vary-p ((obj line))
  (or (may-vary-p (p1 obj))
      (may-vary-p (p2 obj))
      (typep obj 'rubberband)))

(defmethod may-vary-p ((obj chain-or-polygon))
  (some #'may-vary-p (segments obj)))

(defmethod may-vary-p ((obj drawn-enclosure))
  nil)

(defmethod may-vary-p ((obj derived-enclosure))
  (may-vary-p (arg-object obj)))

