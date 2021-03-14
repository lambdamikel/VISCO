;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: GUI; Base: 10 -*-

(in-package gui)

(defconstant +label-text-style+
    (parse-text-style '(:sans-serif nil :very-small)))

(defconstant +highlighted-label-text-style+
    (parse-text-style '(:sans-serif (:bold :italic) :small)))

(defconstant +at-most-label-text-style+
    (parse-text-style '(:sans-serif nil :very-large)))

(defconstant +at-most-highlighted-label-text-style+
    (parse-text-style '(:sans-serif (:bold :italic) :huge)))

(defconstant +object-without-disjoint-and-intersects-relations-ink+ (make-gray-color 0.7))

(defconstant +my-yellow+ (make-rgb-color 1 0.5 0.2))

(defconstant +overlapping-line-dashes+ '(3 5))
(defconstant +transparency-properties-dashes+ '(2 2))

(defconstant +beam-thickness+ 2)
(defconstant +rubberband-thickness+ 1)
(defconstant +rubberband-waveness+ 3)

(defconstant +marble-size+ 5)
(defconstant +nail-size+ 5)
(defconstant +bullet-size+ 4)
(defconstant +arrow-head-size+ 5)

(defconstant +marble-gravity+ 10)
(defconstant +nail-gravity+ 10)
(defconstant +line-gravity+ 15)
(defconstant +drawn-enclosure-gravity+ 30)
(defconstant +chain-and-polygon-gravity+ 30)

(defconstant +ar-1st-arrow-offset+ 12)	; ar = atomic rubberband
(defconstant +ar-2nd-arrow-offset+ 17)
(defconstant +two-guiding-lines-length+ 20) ; fwo = transparency without origin

(defconstant +chain-and-polygon-icon-gravity+ 10)
(defconstant +oc-gravity+ 10)
(defconstant +bullet-gravity+ 10)

(defconstant +marble-highlight+ 10)
(defconstant +nail-highlight+ 10)
(defconstant +line-highlight+ 6)
(defconstant +transparency-highlight+ 5)
(defconstant +enclosure-highlight+ 2)

(defconstant +chain-and-polygon-icon-highlight+ 6)
(defconstant +oc-highlight+ 3)

(defconstant +bullet-highlight+ 5)
(defconstant +intervall-highlight+ 2)
(defconstant +arrow-head-highlight+ 10)


(defconstant +label-text-style-line-height+ 10)
(defconstant +transparency-arrow-length+ 15)

;;;
;;;
;;;

(defgeneric draw (obj stream &key &allow-other-keys))

(defmethod draw :around ((obj gui-object) stream 
			 &key
			 ink
			 (top-level t)
			 subobjects-draw-only-gravity-field
			 (draw-component-objects t)
			 (draw-relative-orientation-circles t)
			 (single-box (typep obj '(or gui-enclosure gui-transparency)))
			 (draw-label t)
			 border-for
			 (gravity-field t)
			 handling-active
			 highlight
			 (allow-sensitive-inferiors t)
			 (draw-chain-or-polygon-icon-p t)
			 label-x 
			 label-y
			 (output-recording t))
  (let ((gravity-field (and (not handling-active)
			    (not highlight)
			    gravity-field)))
    (labels ((do-it ()
	       (with-visco-frame (visco)			
		 (labels ((do-it ()
			    (with-drawing-options
				(stream 
				 :ink 
				 (cond ((and border-for
					     (not (eq border-for 'focus)))
					(ecase border-for
					  (intersects +my-yellow+)
					  (inside/contains 
					   (if (typep obj 'gui-enclosure)
					       (inside-ink obj)
					     +yellow+))
					  (disjoint +blue+)))
				       ((or handling-active
					    highlight 
					    *sceleton-view*)
					+flipping-ink+)		       
				       ((eq (current-transparency visco) obj)
					(current-transparency-ink obj))
				       (t (if (and (typep obj '(or gui-point 
								gui-line))
						   (ignore-disjoint-and-intersects-relations-p obj))
					      +object-without-disjoint-and-intersects-relations-ink+
					    (or ink (ink obj))))))
			      (apply #'call-next-method obj stream
				     :ink ink
				     :top-level top-level
				     :subobjects-draw-only-gravity-field subobjects-draw-only-gravity-field
				     :draw-component-objects draw-component-objects
				     :draw-relative-orientation-circles draw-relative-orientation-circles
				     :single-box single-box
				     :draw-label draw-label
				     :border-for border-for
				     :gravity-field gravity-field
				     :handling-active handling-active
				     :highlight highlight
				     :allow-sensitive-inferiors allow-sensitive-inferiors
				     :draw-chain-or-polygon-icon-p draw-chain-or-polygon-icon-p
				     :label-x label-x 
				     :label-y label-y
				     :output-recording output-recording
				     nil))))
		   (if (and top-level
			    (eq border-for 'focus))
		       (with-border (stream :offset 5)
			 (do-it))
		     (do-it))))))
      (if (and output-recording (or *ignore-inactive-flag*
				    (not (inactive obj))))
	  (with-output-as-presentation      
	      (stream obj (type-of obj) 
		      :single-box single-box
		      :allow-sensitive-inferiors allow-sensitive-inferiors)
	    (do-it))
	(with-output-recording-options (stream :draw t :record nil)
	  (do-it))))))

(defmethod draw-label ((obj gui-label) stream &key highlight
						   label-x
						   label-y
						   handling-active
						   (label-text-style +label-text-style+)
						   (highlighted-label-text-style +highlighted-label-text-style+)
						   (label-text-style-line-height +label-text-style-line-height+))
  (multiple-value-bind (x y)
      (get-label-origin-for (object obj))
    (let ((x (or label-x (+ x (x-off obj))))
	  (y (or label-y (+ y (y-off obj)))))
      (labels ((draw-it ()
		 (let ((line y))
		   (dolist (string (text obj))
		     (draw-text* stream 
				 string
				 x line)
		     (incf line label-text-style-line-height)))))
	
	(when highlight
	  (with-drawing-options (stream
				 :ink (when (or handling-active highlight)
					+flipping-ink+)
				 :text-style
				 (case highlight
				   (:highlight label-text-style)
				   (otherwise highlighted-label-text-style)))
	    (draw-it)))
	
	(with-drawing-options (stream
			       :ink (when (or handling-active highlight)
				      +flipping-ink+)
			       :text-style
			       (case highlight
				 (:highlight highlighted-label-text-style)
				 (otherwise label-text-style)))
	  (draw-it))))))


(defmethod draw-label* (text x y stream &key (label-text-style +label-text-style+)
					     (label-text-style-line-height +label-text-style-line-height+))
  (with-drawing-options (stream
			 :text-style
			 label-text-style) 	
    (let ((line y))
      (dolist (string text)
	(draw-text* stream 
		    string
		    x line)
	(incf line label-text-style-line-height)))))

(defun draw-arrow-head (stream p dir-fac rot &rest args)
  (apply #'draw-arrow-head* stream (x p) (y p) dir-fac rot args))

(defun draw-arrow-head* (stream x y dir-fac rot &key (offset 0)
						     (filled t)
						     (arrow-head-size +arrow-head-size+))
  (let ((trans 
	 (make-rotation-transformation* 
	  rot x y)))
    (with-drawing-options (stream :transformation trans)
      (let* ((x (+ x offset))
	     (x1 x)
	     (y1 (+ y arrow-head-size))
	     (x2 (+ x (* dir-fac arrow-head-size)))
	     (y2 y)
	     (x3 x)
	     (y3 (- y arrow-head-size)))
	(draw-polygon* stream (list x1 y1 x2 y2 x3 y3)
		       :filled filled)))))

(defmethod draw-gui-line ((obj gui-line) stream r 
			  &rest args
			  &key (draw-component-objects t) 
			  &allow-other-keys)
  (unless (already-drawn obj)
    (draw-line* stream 
		(x (p1 obj))
		(y (p1 obj))
		(x (p2 obj))
		(y (p2 obj))
		:line-dashes (when (1d-intersects-other-lines-p obj) 
			       +overlapping-line-dashes+)
		:line-thickness r))
  (when draw-component-objects
    (apply #'draw (p1 obj) stream :top-level nil args)
    (apply #'draw (p2 obj) stream :top-level nil args)))


(defmethod draw-gui-rubberband ((obj gui-line) stream r 
				&rest args
				&key (draw-component-objects t)
				&allow-other-keys)
  (unless (already-drawn obj)
    (let* ((l (/ (length-of-line obj) 9))
	   (intersects (1d-intersects-other-lines-p obj) )
	   (alpha (global-orientation obj))
	   (alpha-orth1 (+ alpha +pi/2+))
	   (alpha-orth2 (- alpha +pi/2+))
	   (xs (x (p1 obj)))
	   (ys (y (p1 obj)))
	   (xe (x (p2 obj)))
	   (ye (y (p2 obj)))	       
	   (xi (* l (cos alpha)))
	   (yi (* l (sin alpha)))
	   (xii-orth1 (* +rubberband-waveness+ (cos alpha-orth1)))
	   (xii-orth2 (* +rubberband-waveness+ (cos alpha-orth2)))
	   (yii-orth1 (* +rubberband-waveness+ (sin alpha-orth1)))
	   (yii-orth2 (* +rubberband-waveness+ (sin alpha-orth2)))
	   (lx xs)
	   (ly ys)
	   (cx xs)
	   (cy ys))
      (dotimes (n 8)
	(incf cx xi)
	(incf cy yi)
	(let ((x (+ cx (if (evenp n) xii-orth1 xii-orth2)))
	      (y (+ cy (if (evenp n) yii-orth1 yii-orth2))))
	  (draw-line* stream lx ly x y
		      :line-thickness r
		      :line-dashes (when intersects
				     +overlapping-line-dashes+))
	  (setf lx x
		ly y)))
      (draw-line* stream
		  lx ly xe ye
		  :line-thickness r
		  :line-dashes (when intersects
				 +overlapping-line-dashes+))
      (draw-line* stream
		  xs ys xe ye
		  :line-thickness r
		  :line-dashes (when intersects
				 +overlapping-line-dashes+))))
  (when draw-component-objects
    (apply #'draw (p1 obj) stream :top-level nil args)
    (apply #'draw (p2 obj) stream :top-level nil args)))


(defmethod draw-gravity-field ((obj geom-point) stream r)
  (draw-circle* stream (x obj) (y obj) r
		:ink +transparent-ink+
		:filled t))

(defmethod draw-gravity-field ((obj geom-line) stream r)
  (draw-gravity-field-for-line stream 
			       (x (p1 obj))
			       (y (p1 obj))
			       (x (p2 obj))
			       (y (p2 obj))
			       r))

(defun draw-gravity-field-for-line (stream x1 y1 x2 y2 r)
  (draw-line* stream x1 y1 x2 y2
	      :line-thickness r
	      :ink +transparent-ink+))

(defun draw-gravity-field-for-circle (stream x y radius r)
  (draw-circle* stream x y radius
		:line-thickness r
		:filled nil
		:ink +transparent-ink+))

;;;
;;;
;;;

(defun draw-epsilon-body (obj r stream &rest args)
  (multiple-value-bind (p1fx p1fy
			p1tx p1ty
			p2fx p2fy
			p2tx p2ty)
      (get-epsilon-enclosure-relevant-points* obj r)
    (apply #'draw-polygon* stream (list 
				   p1fx p1fy
				   p2fx p2fy
				   p2tx p2ty
				   p1tx p1ty
				   p1fx p1fy)
	   args)))

(defun draw-epsilon-circle (obj r stream &rest args)
  (apply #'draw-circle* stream 
	 (x obj) (y obj)
	 r args))

(defun draw-epsilon-segment (obj r stream &rest args)
  (apply #'draw-epsilon-circle (p1 obj) r stream args)
  (apply #'draw-epsilon-circle (p2 obj) r stream args)
  (apply #'draw-epsilon-body obj r stream args))


;;;
;;;
;;;

(defgeneric highlight (obj stream state)
  (:method-combination progn))

(defmethod highlight progn ((obj gui-chain-or-polygon) stream state)
  (dolist (segment (segments obj)) 
    (draw segment stream :draw-component-objects nil :highlight state
	  :output-recording nil))
  (dolist (point (point-list obj))
    (draw point stream :highlight state
	  :output-recording nil))
  (draw (status-label obj) stream :highlight state :output-recording nil)
  (when (at-most-constraint obj)
    (draw (at-most-label obj) stream :highlight state :output-recording nil)))


(defmethod highlight progn ((obj gui-object) stream state)
  (unless (typep obj 'gui-chain-or-polygon)
    (draw obj stream :highlight state
	  :output-recording nil)))

;;;
;;;
;;;

(defmethod object-selected-position-test ((obj gui-enclosure) record x y)
  (declare (ignore record))
  (asg-inside-p* x y obj))

(define-presentation-method highlight-presentation ((obj gui-chain-or-polygon) record stream state)
  (highlight (presentation-object record) stream state))

(define-presentation-method highlight-presentation ((obj gui-label) record stream state)  
  (highlight (presentation-object record) stream state))

(define-presentation-method highlight-presentation ((obj gui-point) record stream state)  
  (highlight (presentation-object record) stream state))

(define-presentation-method highlight-presentation ((obj gui-line) record stream state)  
  (highlight (presentation-object record) stream state))

(define-presentation-method highlight-presentation ((obj gui-transparency) record stream state)
  (highlight (presentation-object record) stream state))

(define-presentation-method highlight-presentation ((obj gui-orientation-arrow) record stream state)  
  (highlight (presentation-object record) stream state))

(define-presentation-method highlight-presentation ((obj gui-relative-orientation-circle) record stream state)  
  (highlight (presentation-object record) stream state))

;;;
;;;
;;;

(defmethod get-label-origin-for ((obj gui-point))
  (values (x obj) (y obj)))

(defmethod get-label-origin-for ((obj gui-transparency))
  (values (x (pcenter obj)) (y (pcenter obj))))

(defmethod get-label-origin-for ((obj gui-line))
  (values (x (pcenter obj)) (y (pcenter obj))))

(defmethod get-label-origin-for ((obj gui-chain-or-polygon))
  (values (x (centroid obj)) (y (centroid obj))))

(defmethod get-label-origin-for ((obj gui-drawn-enclosure))
  (values (x (centroid obj)) (y (centroid obj))))

(defmethod get-label-origin-for ((obj gui-derived-enclosure))
  (get-label-origin-for (arg-object obj)))

;;;
;;;
;;;

(defmethod draw ((obj gui-status-label) stream 
		 &key handling-active highlight label-x label-y
		      gravity-field draw-chain-or-polygon-icon-p)
  (setf (text obj)
    (list
     (if (and (typep (object obj) 'possible-operator-result-mixin)
	      (res-of-operators (object obj)))
	 (concatenate 'string (get-status-string (status (object obj))) " (D)")
       (get-status-string (status (object obj))))))
  (draw-label obj stream
	      :handling-active handling-active
	      :highlight highlight
	      :label-x label-x :label-y label-y)
  (labels ((draw-it ()
	     (let ((object (object obj)))
	       (multiple-value-bind (x y)
		   (get-label-origin-for object)			  
		 (when (typep object 'gui-chain-or-polygon)
		   (with-translation (stream (or label-x
						 (+ (x-off obj) x 20))
					     (or label-y 
						 (+ (y-off obj) y 20)))
		     (with-scaling (stream 0.1)
		       (with-translation (stream (- x) (- y))
			 (dolist (segment (segments object))
			   (draw-line* stream 
				       (x (p1 segment))
				       (y (p1 segment))
				       (x (p2 segment))
				       (y (p2 segment))
				       :line-thickness (when highlight +chain-and-polygon-icon-highlight+))
			   (when gravity-field
			     (draw-gravity-field-for-line stream 
							  (x (p1 segment))
							  (y (p1 segment))
							  (x (p2 segment))
							  (y (p2 segment))
							  +chain-and-polygon-icon-gravity+)))))))))))
    (when draw-chain-or-polygon-icon-p
      (draw-it))))

(defmethod draw ((obj gui-semantics-label) stream &key highlight handling-active label-x label-y)
  (setf (text obj)
    (mapcar #'(lambda (s) (first (lookup-os s))) (semantics (object obj))))
  (draw-label obj stream 
	      :highlight highlight
	      :handling-active handling-active
	      :label-x label-x :label-y label-y))

(defmethod draw ((obj gui-at-most-label) stream &key highlight handling-active label-x label-y)
  (setf (text obj)
    (list (format nil "~A" (at-most-constraint (object obj)))))
  (draw-label obj stream 
	      :highlight highlight
	      :handling-active handling-active
	      :label-x label-x :label-y label-y
	      :label-text-style +at-most-label-text-style+
	      :highlighted-label-text-style +at-most-highlighted-label-text-style+))

(defmethod draw ((obj gui-orientation-arrow) stream &key highlight gravity-field)
  (multiple-value-bind (x y)
      (get-origin-for obj)
    (let* ((alpha (+ (alpha-off obj)
		     (if (typep (object obj) 'geom-line)
			 (global-orientation (object obj))
		       0)))
	   (r (r obj))
	   (r-offset 3))
      (labels ((draw-scale-mark (mark)
		 (let ((x (+ x (* (cos (+ alpha mark)) r)))
		       (y (+ y (* (sin (+ alpha mark)) r))))
		   (draw-circle* stream
				 x y
				 (if highlight 
				     +bullet-highlight+
				   +bullet-size+)))))
	(let ((x1 (+ x (* (cos alpha) (- r 5))))
	      (y1 (+ y (* (sin alpha) (- r 5)))))
	  (draw-line* stream
		      x y
		      x1 y1
		      :line-thickness (when highlight +oc-highlight+))
	  (draw-arrow-head* stream 
			    x1 y1
			    1.0 alpha
			    :offset (- +arrow-head-size+)
			    :arrow-head-size (if highlight 
						 +arrow-head-highlight+
					       +arrow-head-size+))
	  (when gravity-field
	    (draw-gravity-field-for-line stream 
					 x y 
					 x1 y1
					 +oc-gravity+))
	  (let ((cs (orientation-constraint (object obj))))
	    (unless (and (null (rest cs))
			 (numberp (first cs))
			 (zerop (first cs)))
	      (draw-circle* stream
			    x y r			   
			    :filled nil
			    :line-thickness (when highlight +oc-highlight+))
	      (when gravity-field
		(draw-gravity-field-for-circle stream 
					       x y r +oc-gravity+))
	      (dolist (entry cs)
		(if (listp entry)	
		    (progn 
		      (draw-circle* stream x y (+ r r-offset)
				    :end-angle (- +2pi+ (+ alpha (first entry)))
				    :start-angle (- +2pi+ (+ alpha (second entry)))
				    :line-thickness (when highlight +oc-highlight+)
				    :filled nil)
		      (draw-circle* stream x y (+ r r-offset)
				    :end-angle (- +2pi+ (+ alpha pi (first entry)))
				    :start-angle (- +2pi+ (+ alpha pi (second entry)))
				    :line-thickness (when highlight +oc-highlight+)
				    :filled nil))
		  (progn
		    (draw-scale-mark (+ pi entry))
		    (draw-scale-mark entry)))))))))))


(defmethod draw ((obj gui-relative-orientation-circle) stream &key highlight gravity-field )
  (let* ((obj1 (object1 obj))
	 (obj2 (object2 obj))
	 (r (r obj))
	 (r-offset 3)
	 (w (/ (allowed-derivation obj) 2))
	 (alpha1 (global-orientation obj1))
	 (alpha2 (global-orientation obj2)))
    (multiple-value-bind (x y)
	(calculate-intersection-point obj1 obj2)	   		   
      (when (and x y)
	(draw-circle* stream 
		      x y r	   
		      :filled nil
		      :line-thickness (when highlight +oc-highlight+))
	(when gravity-field
	  (draw-gravity-field-for-circle stream x y r +oc-gravity+))
	(labels ((draw-it (alpha)
		   (if (=-eps w 0)
		       (draw-circle* stream 
				     (+ x (* (cos alpha) r))
				     (+ y (* (sin alpha) r)) 
				     (if highlight 
					 +bullet-highlight+
				       +bullet-size+))
		     (draw-circle* stream x y (+ r r-offset)
				   :end-angle (- +2pi+ (- alpha w))
				   :start-angle (- +2pi+ (+ alpha w))
				   :line-thickness (when highlight +oc-highlight+)
				   :filled nil))))
	  (with-drawing-options (stream :line-thickness 2)
	    (draw-it alpha1)
	    (incf r-offset 2)		     
	    (draw-it (+ pi alpha1)))
	  
	  (incf r-offset 2)
	  (draw-it alpha2) 
	  (incf r-offset 2)
	  (draw-it (+ pi alpha2)))))))

;;;
;;;
;;;

(defmethod draw :after ((obj gui-query-object) stream &rest args &key (draw-label t) (output-recording t))
  (labels ((draw-it ()
	     (apply #'draw (status-label obj) stream
		    :top-level nil
		    args)
	     (when (semantics obj)
	       (apply #'draw (semantics-label obj) stream
		      :border-for nil
		      :ink nil
		      :top-level nil
		      args))))
    (when (and draw-label
	       (not (already-drawn obj)))
      (if output-recording
	  (with-output-as-presentation (stream obj (type-of obj))
	    (draw-it))
	(draw-it)))))


(defmethod draw :after ((obj orientation-constraint-mixin) stream &rest args &key (output-recording t))
  (labels ((draw-it ()
	     (apply #'draw (orientation-arrow obj) stream 
		    :border-for nil
		    :ink nil
		    :top-level nil 
		    args)))
    (when (and (orientation-constraint obj)
	       (not (already-drawn obj)))
      (if output-recording
	  (with-output-as-presentation (stream obj (type-of obj))
	    (draw-it))
	(draw-it)))))

(defmethod draw :after ((obj at-most-constraint-mixin) stream &rest args &key (draw-label t) (output-recording t))
  (labels ((draw-it ()
	     (apply #'draw (at-most-label obj) stream
		    :border-for nil
		    :ink nil
		    :top-level nil 
		    args)))
    (when (and draw-label 
	       (at-most-constraint obj)
	       (not (already-drawn obj)))
      (if output-recording
	  (with-output-as-presentation (stream obj (type-of obj))
	    (draw-it))
	(draw-it)))))

(defmethod draw :after ((obj gui-atomic-rubberband) stream &rest args &key (output-recording t)
									   (draw-relative-orientation-circles t)
									   highlight)  
  (labels ((draw-it ()
	     (dolist (circle (relative-orientation-circles obj))
	       (when (eq obj (object1 circle))
		 (apply #'draw circle stream 
			:border-for nil
			:ink nil
			:top-level nil args)))))
    (when (and (relative-orientation-circles obj)
	       draw-relative-orientation-circles
	       (not highlight)
	       (not (already-drawn obj)))
      (if output-recording
	  (with-output-as-presentation (stream obj (type-of obj))
	    (draw-it))
	(draw-it)))))

;;;
;;;
;;;

(defmethod draw ((obj gui-transparency) stream &key highlight)
  (labels ((draw-it (r)
	     (draw-rectangle* stream
			      (x (pmin obj))
			      (y (pmin obj))
			      (x (pmax obj))
			      (y (pmax obj))
			      :line-thickness r
			      :filled nil)
	     (let* ((xmin (x (pmin obj)))
		    (ymin (y (pmin obj)))
		    (xmax (x (pmax obj)))
		    (ymax (y (pmax obj)))
		    (xmiddle (/ (+ xmin xmax) 2))
		    (ymiddle (/ (+ ymin ymax) 2))
		    (sxt (sx-type obj))
		    (syt (sy-type obj)))
	       
	       (with-drawing-options (stream :line-dashes +transparency-properties-dashes+)
		 (when (height obj)
		   (if (or (not syt)
			   (and (symin obj)
				(symax obj)
				(= (symin obj)
				   (symax obj))))
		       (draw-text* stream (format nil "~A m."
						  (round (height obj)))
				   (+ xmin 4) (- ymiddle 4))
		     (draw-text* stream (format nil "~A ... ~A"
						(if (symin obj)
						    (format nil "~A m." (round (* (symin obj) (height obj))))
						  "0")
						(if (symax obj)
						    (format nil "~A m." (round (* (symax obj) (height obj))))
						  "infinity"))
				 (+ xmin 4) (- ymiddle 4))))
		 
		 (when (width obj)
		   (if (or (not sxt)
			   (and (sxmin obj)
				(sxmax obj)
				(= (sxmin obj)
				   (sxmax obj))))
		       (draw-text* stream (format nil "~A m."
						  (round (width obj)))
				   (+ xmiddle 4)
				   (- ymax 4))
		     (draw-text* stream (format nil "~A ... ~A"
						(if (sxmin obj)
						    (format nil "~A m." (round (* (sxmin obj) (width obj))))
						  "0")
						(if (sxmax obj)
						    (format nil "~A m." (round (* (sxmax obj) (width obj))))
						  "infinity"))
				 (+ xmiddle 4)
				 (- ymax 4))))
		 
		 (when sxt
		   (when (=> (sxmax obj)
			     (not (= 1.0 (sxmax obj))))		     
		     (draw-arrow* stream
				  xmin ymiddle
				  (- xmin +transparency-arrow-length+) ymiddle)
		     (draw-arrow* stream
				  xmax ymiddle
				  (+ xmax +transparency-arrow-length+) ymiddle))
		   (when (=> (sxmin obj)
			     (not (= 1.0 (sxmin obj))))
		     (draw-arrow* stream
				  xmin ymiddle
				  (+ xmin +transparency-arrow-length+) ymiddle)
		     (draw-arrow* stream
				  xmax ymiddle
				  (- xmax +transparency-arrow-length+) ymiddle)))
		 
		 
		 (when syt
		   (when (=> (symax obj)
			     (not (= 1.0 (symax obj))))
		     (draw-arrow* stream
				  xmiddle ymin
				  xmiddle (- ymin +transparency-arrow-length+))
		     (draw-arrow* stream 
				  xmiddle ymax
				  xmiddle (+ ymax +transparency-arrow-length+)))
		   (when (=> (symin obj)
			     (not (= 1.0 (symin obj))))
		     (draw-arrow* stream
				  xmiddle ymin
				  xmiddle (+ ymin +transparency-arrow-length+))
		     (draw-arrow* stream 
				  xmiddle ymax
				  xmiddle (- ymax +transparency-arrow-length+))))
		 
		 (when (sxsy-constraint obj)
		   (if (origin obj)
		       (progn
			 (draw-line* stream
				     (x (origin obj))
				     (y (origin obj))
				     xmin ymin)
			 (draw-line* stream
				     (x (origin obj))
				     (y (origin obj))
				     xmin ymax)			 
			 (draw-line* stream
				     (x (origin obj))
				     (y (origin obj))
				     xmax ymin)
			 (draw-line* stream
				     (x (origin obj))
				     (y (origin obj))
				     xmax ymax))
		     (progn
		       (draw-line* stream				    
				   xmin ymin
				   (+ xmin +two-guiding-lines-length+)
				   (+ ymin +two-guiding-lines-length+))
		       (draw-line* stream
				   xmin ymax
				   (+ xmin +two-guiding-lines-length+)
				   (- ymax +two-guiding-lines-length+))
		       (draw-line* stream
				   xmax ymin
				   (- xmax +two-guiding-lines-length+)
				   (+ ymin +two-guiding-lines-length+))
		       (draw-line* stream
				   xmax ymax
				   (- xmax +two-guiding-lines-length+)
				   (- ymax +two-guiding-lines-length+)))))))))
    (if highlight
	(draw-it +transparency-highlight+)
      (draw-it 0))))

;;;
;;;
;;;

(defmethod draw ((obj gui-marble) stream 
		 &key top-level gravity-field subobjects-draw-only-gravity-field highlight)
  (labels ((draw-it (r)
	     (when (or (not (already-drawn obj))
		       top-level
		       (and (not top-level)
			    (not subobjects-draw-only-gravity-field)))
	       (draw-circle* stream (x obj) (y obj) r
			     :filled nil))))
    (if highlight
	(draw-it +marble-highlight+)
      (progn 
	(when gravity-field
	  (draw-gravity-field obj stream +marble-gravity+))
	(draw-it +marble-size+)))))

(defmethod draw ((obj gui-nail) stream
		 &key top-level gravity-field subobjects-draw-only-gravity-field highlight)
  (labels ((draw-it (r)
	     (when (or (not (already-drawn obj))
		       top-level
		       (and (not top-level)
			    (not subobjects-draw-only-gravity-field)))
	       (draw-marker* stream (x obj) (y obj) r))))
    (if highlight
	(draw-it +nail-highlight+)
      (progn 
	(when gravity-field
	  (draw-gravity-field obj stream +nail-gravity+))
	(draw-it +nail-size+)))))

(defmethod draw ((obj gui-origin) stream
		 &key top-level gravity-field subobjects-draw-only-gravity-field highlight)

  (labels ((draw-it (r)
	     (when (or (not (already-drawn obj))
		       top-level
		       (and (not top-level)
			    (not subobjects-draw-only-gravity-field)))
	       (draw-rectangle* stream 
				(- (x obj) r)
				(- (y obj) r)
				(+ (x obj) r)
				(+ (y obj) r)
				:filled t))))
    (if highlight
	(draw-it +nail-highlight+)
      (progn
	(when gravity-field
	  (draw-gravity-field obj stream +nail-gravity+))
	(draw-it +nail-size+)))))

;;;
;;;
;;;

(defmethod draw ((obj gui-beam) stream &rest args &key highlight gravity-field)
  (if highlight
      (apply #'draw-gui-line obj stream (+ +beam-thickness+ +line-highlight+) args)
    (progn
      (when gravity-field
	(draw-gravity-field obj stream +line-gravity+))
      (apply #'draw-gui-line obj stream +beam-thickness+ args))))

(defmethod draw ((obj gui-atomic-rubberband) stream &rest args &key highlight gravity-field)
  (unless (already-drawn obj)
    (let ((alpha (geometry::global-orientation obj)))
      (draw-arrow-head stream (p1 obj) -1.0 alpha :offset +ar-1st-arrow-offset+ :filled nil)
      (draw-arrow-head stream (p2 obj) -1.0 (+ pi alpha) :offset +ar-1st-arrow-offset+ :filled nil)
      (draw-arrow-head stream (p1 obj) 1 alpha :offset +ar-2nd-arrow-offset+ :filled nil)
      (draw-arrow-head stream (p2 obj) 1 (+ pi alpha) :offset +ar-2nd-arrow-offset+ :filled nil)))
  (if highlight
      (apply #'draw-gui-line obj stream +line-highlight+ args)
    (progn
      (when gravity-field
	(draw-gravity-field obj stream +line-gravity+))
      (apply #'draw-gui-line obj stream +rubberband-thickness+ args))))

(defmethod draw ((obj gui-atomic->=-rubberband) stream &rest args &key highlight gravity-field)
  (unless (already-drawn obj)
    (let ((alpha (geometry::global-orientation obj)))
      (draw-arrow-head stream (p1 obj) -1 alpha :offset +ar-1st-arrow-offset+ :filled nil)
      (draw-arrow-head stream (p2 obj) -1 (+ pi alpha) :offset +ar-1st-arrow-offset+ :filled nil)))
  (if highlight
      (apply #'draw-gui-line obj stream +line-highlight+ args)
    (progn
      (when gravity-field
	(draw-gravity-field obj stream +line-gravity+))
      (apply #'draw-gui-line obj stream +rubberband-thickness+ args))))

(defmethod draw ((obj gui-atomic-<=-rubberband) stream &rest args &key highlight gravity-field)
  (unless (already-drawn obj)
    (let ((alpha (geometry::global-orientation obj)))
      (draw-arrow-head stream (p1 obj) 1 alpha :offset +ar-1st-arrow-offset+ :filled nil)
      (draw-arrow-head stream (p2 obj) 1 (+ pi alpha) :offset +ar-1st-arrow-offset+ :filled nil)))
  (if highlight
      (apply #'draw-gui-line obj stream +line-highlight+ args)
    (progn
      (when gravity-field
	(draw-gravity-field obj stream +line-gravity+))
      (apply #'draw-gui-line obj stream +rubberband-thickness+ args))))

(defmethod draw ((obj gui-rubberband) stream &rest args &key highlight gravity-field)
  (if highlight
      (apply #'draw-gui-line obj stream (+ +rubberband-thickness+ +line-highlight+) args)
    (progn
      (when gravity-field
	(draw-gravity-field obj stream +line-gravity+))
      (apply #'draw-gui-rubberband obj stream +rubberband-thickness+ args))))

;;;
;;;
;;;


(defmethod draw ((obj gui-chain-or-polygon) stream &rest args
		 &key gravity-field draw-component-objects)
  (unless (already-drawn obj)
    (when gravity-field
      (dolist (segment (segments obj))
	(draw-gravity-field segment stream +chain-and-polygon-gravity+)))
    (dolist (segment (segments obj))
      (if draw-component-objects
	  (apply #'draw segment stream :single-box nil :top-level nil args)
	(draw-line* stream 
		    (x (p1 segment))
		    (y (p1 segment))
		    (x (p2 segment))
		    (y (p2 segment)))))))

;;;
;;;
;;;

(defmethod draw ((obj gui-enclosure) stream &key)
  (draw-polygon* stream
		 (drawable-pointlist obj)
		 :closed t))

(defmethod draw ((obj gui-epsilon-enclosure) stream &key)
  (let ((r (radius obj))
	(obj (arg-object obj)))
    (typecase obj
      (point 
       (draw-epsilon-circle obj r stream ))
      (line 
       (draw-epsilon-segment obj r stream))
      (chain-or-polygon
       (dolist (segment (segments obj))
	 (draw-epsilon-body segment r stream))
       (dolist (point (point-list obj))
	 (draw-epsilon-circle point r stream))))))

