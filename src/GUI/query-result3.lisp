;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: GUI; Base: 10 -*-

(in-package gui)

;;;
;;;
;;;

(defun show-search-progress (percent &key force-p)
  (with-visco-inspector-frame (frame)
    (let* ((stream (get-frame-pane frame 'progress-bar))
	   (time (get-universal-time)))
      (with-slots (last-percent last-time) frame 
	(when (or (not last-percent)
		  (and (not (= last-percent percent))
		       (or force-p 
                           (> (- time last-time) 1))))
	  (multiple-value-bind (width height)
	      (window-inside-size stream)
	    (with-output-recording-options (stream :draw t :record nil)
	      (when last-percent
		(draw-rectangle* stream 0 0 (* last-percent width) height
				 :ink +background-ink+))
	      (setf last-percent percent)
	      (draw-rectangle* stream 0 0 (* percent width) height
			       :ink +red+)
	      (setf last-time time))))))))

(defmacro with-pretty-map-output (&body body)
  `(let ((database::*display-map-text-mode* nil)
	 (database::*display-binding-names-mode* nil)
	 (database::*display-nodes-mode* nil)
	 (database::*sensitive-objects-mode* nil))
     ,@body))

;;;
;;;
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  
  (defclass query-result ()
    ((bindings :accessor bindings :initarg :bindings)
     (selected :accessor selected :initform nil)
     
     (map-radius :accessor map-radius :initarg :map-radius)
     (map-xcenter :accessor map-xcenter :initarg :map-xcenter)
     (map-ycenter :accessor map-ycenter :initarg :map-ycenter)))
  
  (defclass query-page ()
    ((objects :accessor objects :initarg :objects :initform nil)))
  )


(defmethod full ((obj query-page))
  (= (length (objects obj))
     (* +x-no-of-thumbnails+
	+y-no-of-thumbnails+)))

(defun make-new-page ()
  (with-visco-inspector-frame (frame)
    (let ((obj (make-instance 'query-page)))
      (pushend obj (pages frame))
      (setf (active-page frame) obj)
      (setf (selected-page frame) obj)
      (update-buttons)
      obj)))

(defun get-position (obj)
  (with-visco-inspector-frame (frame)
    (when (and obj (selected-page frame))
      (let ((pos (position obj (objects (selected-page frame)))))
	(when pos
	  (multiple-value-bind (w h)
	      (thumbnail-size frame)
	    (let ((y (floor pos +x-no-of-thumbnails+))
		  (x (mod pos +x-no-of-thumbnails+)))
	      (values (+ (* w x) +spacing+) 
		      (+ (* h y) +spacing+)
		      (- (* w (1+ x)) +spacing+)
		      (- (* h (1+ y)) +spacing+)))))))))

(defmethod thumbnail-size ((frame visco-inspector))
  (multiple-value-bind (width height)
      (window-inside-size (get-frame-pane frame 'query-results))
    (values (/ (- width 1) +x-no-of-thumbnails+)
	    (/ (- height 1) +y-no-of-thumbnails+))))

(defmethod draw-result ((result query-result) stream w h &key full-view)
  (set-current-map-position-and-radius (map-xcenter result)
				       (map-ycenter result)
				       (+ (map-radius result) 50))
  (labels ((draw-it ()	    
	     (when *draw-thumbnails*
	       (with-pretty-map-output 
		   (with-output-recording-options (stream :draw t 
							  :record #+visco-demo-mode t)
		     (draw-current-map-to-foreign-stream stream
							 :overview t
							 :width w
							 :height h
							 :show-types-p t
							 :show-bindings (bindings result)))))))
    (if (not full-view)	
	(with-output-as-presentation (stream result 'query-result
					     :single-box t
					     :allow-sensitive-inferiors nil)
	  (draw-rectangle* stream 1 1 (1- w) (1- h)
			   :ink (if (selected result)
				    +yellow+
				  +background-ink+))
	  (draw-it)
	  (draw-rectangle* stream 0 0 w h
			   :filled nil))
      (draw-it))))



(defmethod draw-thumbnail ((result query-result))
  (with-visco-inspector-frame (frame)
    (let ((stream (get-frame-pane frame 'query-results)))
      (multiple-value-bind (xf yf xt yt)
	  (get-position result)
	(with-translation (stream xf yf)
	  (draw-result result stream (- xt xf) (- yt yf)))))))

(define-presentation-method highlight-presentation ((obj query-result) record stream state)
  (declare (ignore state))
  (let* ((obj (presentation-object record)))
    (multiple-value-bind (xf yf xt yt)
	(get-position obj)
      (when xf				; get-position kann NIL liefern
	(draw-rectangle* stream 
			 xf yf xt yt
			 :filled nil
			 :ink +flipping-ink+
			 :line-thickness 10)))))

(defmethod object-selected-position-test ((obj query-result) record x y)  
  (declare (ignore record))
  (multiple-value-bind (xf yf xt yt)
      (get-position obj)
    (when xf
      (and (<= xf x xt)
	   (<= yf y yt)))))

(defun get-page-nr ()
  (with-visco-inspector-frame (frame)
    (if (selected-page frame)
	(1+ (position (selected-page frame) (pages frame)))
      0)))

;;;
;;;
;;;

(defun abort-search (button)
  (declare (ignore button))
  (setf query-compiler::*abort-search* t))

(defun previous-page (button)
  (declare (ignore button))
  (with-visco-inspector-frame (frame)
    (when (selected-page frame)
      (let ((pos (position (selected-page frame) (pages frame))))
	(unless (zerop pos)
	  (setf (selected-page frame)
	    (nth (1- pos) (pages frame)))
	  (update-buttons))))))

(defun next-page (button)
  (declare (ignore button))
  (with-visco-inspector-frame (frame)
    (let ((rest (rest (member (selected-page frame) (pages frame)))))
      (when rest	
	(setf (selected-page frame) (first rest))
	(update-buttons)))))

(defun delete-all-pages (button)
  (declare (ignore button))
  (with-visco-inspector-frame (frame)
    (setf (pages frame) nil
	  (active-page frame) nil
	  (selected-page frame) nil)
    (update-buttons)))

(defun delete-page (button)
  (declare (ignore button))
  (with-visco-inspector-frame (frame)
    (when (selected-page frame)      
      (setf (pages frame) 
	(delete (selected-page frame)
		(pages frame)))
      (setf (selected-page frame)
	(first (pages frame)))
      (update-buttons))))


(defun delete-selected (button)
  (declare (ignore button))
  (with-visco-inspector-frame (frame)
    (when (selected-page frame)
      (dolist (obj (remove-if-not #'selected (objects (selected-page frame))))
	(setf (objects (selected-page frame))
	  (delete obj (objects (selected-page frame)))))
      (if (objects (selected-page frame))
	  (update-buttons)
	(delete-page +button-delete-page+)))))

(defun delete-unselected (button)
  (declare (ignore button))
  (with-visco-inspector-frame (frame)
    (when (selected-page frame)
      (dolist (obj (remove-if #'selected (objects (selected-page frame))))
	(setf (objects (selected-page frame))
	  (delete obj (objects (selected-page frame)))))
      (if (objects (selected-page frame))
	  (update-buttons)
	(delete-page +button-delete-page+)))))

(defun unselect-all (button)
  (declare (ignore button))
  (with-visco-inspector-frame (frame)
    (when (selected-page frame)
      (dolist (obj (objects (selected-page frame)))
	(setf (selected obj) nil))
      (update-buttons))))

(defun button-draw-thumbnails (button value)
  (declare (ignore button))
  (setf *draw-thumbnails* value))

(defun button-permutations (button value)
  (declare (ignore button))
  (setf *permutations* value))

;;;
;;;
;;;


(defun lock-buttons ()
  (dolist (button (list +button-previous+ +button-next+
			+button-delete-all-pages+ +button-delete-page+
			+button-delete-selected+ +button-delete-unselected+
			+button-unselect-all+))
    (deactivate-gadget button))
  (setf *searching-active* t))

(defun unlock-buttons ()
  (setf *searching-active* nil)
  (update-buttons) 
  (show-search-progress 0 :force-p t))

(defun update-buttons ()
  (with-visco-inspector-frame (frame)
    (unless *searching-active*
      (cond ((pages frame)
	     (let ((nr (get-page-nr)))
	       (if (= 1 nr)
		   (deactivate-gadget +button-previous+)
		 (activate-gadget +button-previous+))
	       (if (= nr (length (pages frame)))
		   (deactivate-gadget +button-next+)
		 (activate-gadget +button-next+))
	       (activate-gadget +button-delete-selected+)
	       (activate-gadget +button-delete-unselected+)	     
	       (activate-gadget +button-delete-page+)
	       (activate-gadget +button-delete-all-pages+)
	       (activate-gadget +button-unselect-all+)))
	    (t (deactivate-gadget +button-previous+)
	       (deactivate-gadget +button-next+)	     
	       (deactivate-gadget +button-delete-all-pages+)
	       (deactivate-gadget +button-delete-page+)
	       (deactivate-gadget +button-delete-selected+)
	       (deactivate-gadget +button-delete-unselected+)
	       (deactivate-gadget +button-unselect-all+))))
    (window-clear (get-frame-pane frame 'query-results))
    (redisplay-frame-pane frame (get-frame-pane frame 'page-nr) :force-p t)
    (draw-query-results frame (get-frame-pane frame 'query-results))))


(defmethod show-page-nr ((frame visco-inspector) stream)  
  #-visco-demo-mode (format stream " ~A /~% ~A" (get-page-nr) (length (pages frame)))
  #+visco-demo-mode (format stream " Page No. ~A of ~A Pages." (get-page-nr) (length (pages frame))))

;;;
;;;
;;;


(defun make-query-result ()
  (with-visco-inspector-frame (inspector-frame)
    (let* ((query (get-current-query))	       
	   (objects
	    (remove-if-not #'matches-with-database-object-p    
			   (visco-objects query)))
	   (bindings 
	    (mapcan #'(lambda (visco-object)
			(when (primary-p visco-object)
			  (if (and (typep visco-object 'rubberband)
				   (typep (bound-to visco-object) 'geom-chain))
			      (segments (bound-to visco-object))
			    (list (bound-to visco-object)))))
		    objects)))
      (when (or *permutations*
		(not
		 (some #'(lambda (match)
			   (set-equal bindings match))
		       (all-matches inspector-frame))))
	(push bindings (all-matches inspector-frame))
	
	(when (or (not (active-page inspector-frame))
                  (full (active-page inspector-frame)))
	  (make-new-page))
	(let* ((agg (make-aggregate 'geom-aggregate
				    (mapcar #'bound-to objects)
				    :hierarchicly-p nil))
	       (xcenter (x (pcenter agg)))
	       (ycenter (y (pcenter agg)))
	       (radius (max (+ 30 (/ (- (x (pmax agg)) (x (pmin agg))) 2))
			    (+ 30 (/ (- (y (pmax agg)) (y (pmin agg))) 2))))
	       (res-object (make-instance 'query-result
			     :bindings
			     (mapcan #'(lambda (visco-object)
					 (if (and (typep visco-object 'rubberband)
						  (typep (bound-to visco-object) 'geom-chain))
					     (mapcar #'(lambda (segment)
							 (list segment
							       visco-object
							       (mapcar #'(lambda (os)
									   (first (lookup-os os)))
								       (all-os segment))))
						     (segments (bound-to visco-object)))
					   (list (list (bound-to visco-object)
						       visco-object
						       (mapcar #'(lambda (os)
								   (first (lookup-os os)))
							       (all-os (bound-to visco-object)))))))
				     objects)
			     :map-radius radius
			     :map-xcenter xcenter
			     :map-ycenter ycenter)))
	  
	  (pushend res-object (objects (active-page inspector-frame)))
	  
	  (when (eq (selected-page inspector-frame)
		    (active-page inspector-frame))
	    (draw-thumbnail res-object)))))))

;;;
;;;
;;;

(defmethod draw-selected-query-result ((frame visco-inspector) stream)
  (let ((result (selected-query-result frame)))
    (multiple-value-bind (w h)
	(window-inside-size (get-frame-pane frame 'selected-query-result))
      (when result
	(let ((*draw-thumbnails* t))
	  (draw-result result stream (- w 2) (- h 2) :full-view t))))))

(defmethod draw-query-results ((frame visco-inspector) stream)
  (declare (ignore stream))
  (when (selected-page frame)
    (dolist (result (objects (selected-page frame)))
      (draw-thumbnail result))))

;;;
;;;
;;;

(define-visco-inspector-command (com-inspect-query-result)
    ((object 'query-result))
  (with-visco-inspector-frame (frame)    
    (setf (selected-query-result frame) object)
    (dolist (obj (objects (get-current-db)))
      (setf (bound-to obj) nil))
    (dolist (binding (bindings object))
      (setf (bound-to (first binding)) (second binding)))))

(define-presentation-to-command-translator inspect-query-result
    (query-result
     com-inspect-query-result visco-inspector
     :tester (() (not *searching-active*))
     :echo nil
     :maintain-history nil
     :gesture :select)
  (object)
  (list object))

;;;
;;;
;;;

(define-visco-inspector-command (com-select-query-result)
    ((object 'query-result))
  (setf (selected object) (not (selected object)))
  (com-inspect-query-result object)
  (draw-thumbnail object))

(define-presentation-to-command-translator select-query-result
    (query-result
     com-select-query-result visco-inspector
     :echo nil
     :tester (() (not *searching-active*))
     :maintain-history nil
     :gesture :move)
  (object)
  (list object))

;;;
;;;
;;;

#+(and mcl antiker-mac)
(defmethod draw-overview ((frame visco-inspector) stream)
  nil)

(defmethod draw-overview ((frame visco-inspector) stream)
  (with-slots (overview-pattern current-map-range
	                        overview-transformation) frame
    (multiple-value-bind (width height)
                         (bounding-rectangle-size (bounding-rectangle (window-viewport stream)))
      (when (get-current-db)
        (cond (overview-pattern
               (draw-pixmap* stream
                             overview-pattern
                             0 0)
               (database::show-current-map-range :record t
						 :coordinates nil
						 :stream stream
						 :transformation (database::calculate-transformation stream)))
              (t (setf overview-pattern 
                       (with-output-to-pixmap (stream2
				               (sheet-medium stream)
				               :width width
				               :height height)
                         (with-pretty-map-output
                           (database::unhighlight-all)
		           (database::full-map-range)
		           (database::recalculate-transformation stream :width width :height height)
		           (database::with-map-viewer-frame (map-viewer)
			     (database::draw-current-map map-viewer stream2 
						         :overview t
						         :clear-p nil)))))
                 (draw-pixmap* stream
                               overview-pattern
                               0 0)))))))
           
