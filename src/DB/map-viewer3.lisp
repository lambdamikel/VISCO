;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: DATABASE; Base: 10 -*-

(in-package database)

(defvar *map-viewer-frame*)

(defvar *image-map* nil)

(defconstant +info-text-style+
    (make-text-style :serif :roman :small))

(defconstant +text-height+ 10)

(defconstant +command-listener-text-style+
    (make-text-style
     :sans-serif :roman :normal))

(defconstant +map-viewer-text-style+
    (make-text-style :sans-serif
		     :roman
		     :normal))

;;;
;;;
;;;

(defconstant +ink-bound-to+
    (make-rgb-color 1.0 0.1 0.2))

(defconstant +grayed-item-ts+
    (parse-text-style '(:sans-serif nil :small)))

(defconstant +grayed-item-i+ 
    (make-gray-color 0.7))

(defconstant +selected-item-ts+
    (parse-text-style '(:sans-serif (:bold :italic) :small)))

(defconstant +selected-item-i+ 
    (make-gray-color 0.0))

(defconstant +unselected-item-ts+
    (parse-text-style '(:sans-serif :bold  :small)))

(defconstant +unselected-item-i+ 
    (make-gray-color 0.4))

;;;
;;;
;;;

(defparameter *unknown-os-keys* nil)

(defparameter *highlight-line-thickness* 3)

;;;
;;;
;;;

(defparameter *display-binding-names-mode* nil)

(defparameter *warn-mode* nil)

(defparameter *decompose-mode* nil)

(defparameter *autotracking-mode* nil)

(defparameter *display-map-text-mode* nil)

(defparameter *display-nodes-mode* nil)

(defparameter *sensitive-objects-mode* t)

(defparameter *single-box-mode* nil)

(defmethod dont-display ((obj t))
  t)

;;;
;;;
;;;

(defmethod label-pos ((obj geom-thing))
  (values (x (pcenter obj)) (y (pcenter obj))))


(defmacro with-map-viewer-frame ((name) &body body)
  `(let ((,name *map-viewer-frame*))
     ,@body))

(defmethod highlight ((obj t))
  t)

(defmethod unhighlight ((obj t))
  t)

(defmethod highlight ((obj db-object))
  (setf (bound-to obj) t))

(defmethod unhighlight ((obj db-object))
  (setf (bound-to obj) nil))

(defun unhighlight-all ()
  (with-map-viewer-frame (frame)
    (with-slots (db) frame
      (when db 
	(dolist (obj (objects db))
	  (unhighlight obj))))))

(defun highlight-all (list)
  (dolist (obj list)
    (highlight obj)))

;;;
;;;
;;;

(define-presentation-type os-key-item ())

(define-presentation-type current-map-range ())

;;;
;;;
;;;

#+:mcl
(progn
  (define-gesture-name :adjust :pointer-button (:left :shift)) 

  (define-gesture-name :unselect-os-keys-of-object :pointer-button (:left :shift))
  
  (define-gesture-name :unselect-all-but-os-keys-of-object :pointer-button (:left :control))
  
  (define-gesture-name :hide-object :pointer-button (:left :meta)))

#+:allegro
(progn
  (define-gesture-name :adjust :pointer-button (:middle))
  
  (define-gesture-name :unselect-os-keys-of-object :pointer-button (:left :shift))
  
  (define-gesture-name :unselect-all-but-os-keys-of-object :pointer-button (:middle :shift))
  
  (define-gesture-name :hide-object :pointer-button (:right :shift)))

;;;
;;;
;;;


(defpersistentclass map-text ()
  ((matrix :accessor matrix :initarg :matrix :not-persistent)
   (text :accessor text :initarg :text)
   (bbox :accessor bbox :not-persistent)
   (tx :accessor tx :initarg :tx)
   (ty :accessor ty :initarg :ty)
   (h :accessor h :initarg :h)
   (w :accessor w :initarg :w)))

(defmethod initialize-instance :after ((obj map-text) &rest initargs)
  (declare (ignore initargs))
  (init obj))

(defmethod initialize-loaded-persistent-object ((obj map-text))
  (init obj))

(defmethod init ((obj map-text))
  (with-slots (h tx ty w matrix) obj
    (let* ((sx (/ h 4))
	   (sy (/ h 4))
	   (r (geometry::deg-to-rad w))
	   (trans1 (make-translation-transformation tx ty))
	   (trans2 (make-scaling-transformation sx sy))
	   (trans3 (make-rotation-transformation r))
	   (transx (compose-transformations trans2 trans3)))
      (setf matrix
	(compose-transformations trans1 transx))))
  (with-map-viewer-frame (map-viewer-frame)
    (with-slots (text-objects) map-viewer-frame
      (let ((stream (get-frame-pane map-viewer-frame 'display)))
	(multiple-value-bind (xf yf xt yt)	
	    (bounding-rectangle*
	     (with-output-to-output-record (stream)
	       (let ((*display-map-text-mode* t))
		 (draw obj stream))))
	  (setf (bbox obj)
	    (bb xf yf xt yt))
	  (push obj text-objects))))))

;;;
;;;
;;;

(define-command-table file-table
    :menu (("Load SQD Map" :command (com-load-sqd-map))
	   ("divide1" :divider nil)
	   ("Load Map" :command (com-load-map))
	   ("Save Map" :command (com-save-map))
	   ("divide2" :divider nil)
           ("Install Image Map" :command (com-install-image-map))
           ("divide3" :divider nil)
	   ("Quit" :command (com-quit))))


(define-command-table map-table
    :menu (("Kill Bindings" :command (com-kill-bindings))
	   ("Clear Display" :command (com-clear-display))
	   ("Reset Map" :command (com-reset-map))
	   ("Redraw Map" :command (com-redraw))
	   ("Show Map Infos" :command (com-show-map-infos))
	   #+:allegro ("Print Whole Map" :command (com-print-whole-map))))


(define-command-table keys-table
    :menu (("Undo Nothing" :command (com-undo))
	   ("divide1" :divider nil)
	   ("Lookup Key" :command (com-lookup-os-key))
	   ("Select All Keys" :command (com-select-all-os-keys))
	   ("Unselect All Keys" :command (com-unselect-all-os-keys))
	   #+:allegro ("divide2" :divider nil)
	   #+:allegro ("Print Keylist" :command (com-print-keylist))))

(define-command-table spatial-table
    :menu (("Highlight Intersecting Objects" :command (com-highlight-intersecting-objects*))
	   ("Highlight Containing Objects" :command (com-highlight-containing-objects*))
	   ("Highlight Contained Objects" :command (com-highlight-contained-objects*))))

;;;
;;;
;;;

(defun generate-name-for-undo-command ()  
  (remove-command-from-command-table 
   'com-undo 'keys-table)

  (with-map-viewer-frame (map-viewer-frame)
    (with-slots (unselect-stack) map-viewer-frame
      (let ((first (first unselect-stack)))
	(if first
	    (add-command-to-command-table 'com-undo
					  'keys-table
					  :menu (list 
						 (if (typep first 'db-object)
						     (format nil "Undo Hide ~A"
							     (id first))
						   "Undo Last Selection")
						 :after :start))
	  (add-command-to-command-table 'com-undo
					'keys-table
					:menu (list 
					       "Undo Nothing"
					       :after :start)))))))

(defun register-selected-keys-for-undo ()
  (with-map-viewer-frame (map-viewer-frame)
    (with-slots (unselect-stack selected-os-keys) map-viewer-frame	
      (push (copy-list selected-os-keys) unselect-stack)
      (generate-name-for-undo-command))))


(defun register-hide-for-undo (object)
  (with-map-viewer-frame (map-viewer-frame)
    (with-slots (unselect-stack) map-viewer-frame	
      (push object unselect-stack)
      (generate-name-for-undo-command))))

;;;
;;;
;;;

(defclass overview-pane
    (application-pane)
  ())

(defclass display-pane
    (application-pane)
  ())


(define-application-frame map-viewer ()
  ( (db :initform nil)
    (text-objects :initform nil)
    
    (map-filename :initform nil)
    
    (unselect-stack :initform nil)
    
    (current-map-range :initform nil :accessor current-map-range)
    
    (current-map-radius :initform 100)
    (current-map-position :initform nil)
    (current-map-transformation :initform nil)

    (overview-pattern :initform nil)
    (old-overview-vp-width :initform nil)
    (old-overview-vp-height :initform nil)    
    (overview-transformation :initform nil)

    (present-os-keys :initform nil)
    (selected-os-keys :initform nil))
  
  (:command-table (map-viewer
		   :inherit-from (file-table map-table keys-table spatial-table)
		   :menu (("File" :menu file-table)
			  ("Map" :menu map-table)
			  ("Key Control" :menu keys-table)
			  ("Spatial Querying" :menu spatial-table))))

  (:panes         
   

   (display (make-pane 'display-pane		       
		       :scroll-bars nil
		       :end-of-line-action :allow
		       :end-of-page-action :allow
		       :textcursor nil))

   (button-undo (make-pane 'push-button
			   :label "Undo"	
			   :activate-callback 'button-undo))

   (button-kill-bindings (make-pane 'push-button
				    :label #+:allegro "Kill Bindings" #+:mcl "Kill Bs"
				    :activate-callback 'button-kill-bindings))

   (button-clear (make-pane 'push-button
			    :label "Clear"			   
			    :activate-callback 'button-clear))
   
   (button-redraw (make-pane 'push-button
			     :label "Redraw"			   
			     :activate-callback 'button-redraw))

   (button-infos (make-pane 'push-button
			    :label "Infos"			   
			    :activate-callback 'button-infos))
   
   (button-reset (make-pane 'push-button
			    :label "Reset"
			    :activate-callback 'button-reset))
   
   (overview (make-pane 'overview-pane			
			:scroll-bars nil
			:end-of-line-action :allow
			:end-of-page-action :allow
			:textcursor nil
                        :display-after-commands nil
			:display-function #'draw-overview))
   
   (infos :application
	  :label "Map Infos"
	  :textcursor nil
	  :end-of-line-action :allow
	  :end-of-page-action :allow
	  :text-style +info-text-style+
	  :scroll-bars :both)
   
   (coordinates :application
		:label "Map Coordinates"
		:textcursor nil
		:end-of-line-action :allow
		:end-of-page-action :allow
		:text-style +info-text-style+
		:scroll-bars nil		
		:display-function #'show-coordinates)

   (buttons :accept-values
	    :scroll-bars nil
 	    :min-height :compute :height :compute :max-height :compute 
	    :display-function
	    `(accept-values-pane-displayer
	      :displayer ,#'(lambda (frame stream)
			      (accept-buttons
			       frame stream))))

   (os-selector :application
		:incremental-redisplay t
		:scroll-bars :both
		:end-of-line-action :allow
		:end-of-page-action :allow
		:display-function #'accept-os-selection)
   
   (command :interactor
	    :label nil
	    :text-style +command-listener-text-style+
	    :scroll-bars :vertical
	    :min-height '(4 :line)
	    :max-height '(4 :line)
	    :height '(4 :line))
   
   (pointer-documentation-pane
    (make-clim-stream-pane :type 'pointer-documentation-pane
			   :foreground +white+
			   :background +black+
			   :text-style (make-text-style
					:sans-serif :bold :small)
			   :scroll-bars nil
			   :min-height '(1 :line)
			   :max-height '(1 :line)
			   :height '(1 :line))))
  
  (:layouts
   (:default
       (vertically ()	 
	 #+:allegro buttons
         #+:mcl (mcl-pane buttons)
	 (horizontally ()
	   #+:allegro
           (1/4 os-selector)
           #+:mcl
           (1/4 (mcl-pane os-selector))
	   (2/4
            #+:allegro
	    (vertically ()
              (outlining ()
		(labelling (:label "Current Map")
		  display))
	      (30 (horizontally ()
		    (1/6 button-undo)		    
		    (1/6 button-kill-bindings)		    
		    (1/6 button-clear)
		    (1/6 button-reset)
		    (1/6 button-redraw)
		    (1/6 button-infos))))
            #+:mcl
            (vertically ()
              (mcl-pane 
               (labelling (:label "Current Map")
                 display))
              (mcl-pane
               (horizontally ()
                 (1/6 button-undo)		    
                 (1/6 button-kill-bindings)		    
                 (1/6 button-clear)
                 (1/6 button-reset)
                 (1/6 button-redraw)
                 (1/6 button-infos)))))
           (1/4 
            #+:allegro
            (vertically ()
	      (1/7 coordinates)
	      (3/7 (outlining ()
		     (labelling (:label "Map Overview")
		       overview)))
	      (3/7 infos))
            #+:mcl
            (vertically ()
              (1/7 (mcl-pane coordinates))
              (3/7 (mcl-pane overview))
	      (3/7 (mcl-pane infos)))))
	 #+:allegro
         command
         #+:mcl
         (mcl-pane command)
	 #+:allegro
         pointer-documentation-pane
         #+:mcl 
         (mcl-pane pointer-documentation-pane)))))


;;;
;;;
;;;

(defun button-kill-bindings (button)
  (declare (ignore button))
  (com-kill-bindings))

(defun button-undo (button)
  (declare (ignore button))
  (com-undo))

(defun button-clear (button)
  (declare (ignore button))
  (com-clear-display))

(defun button-redraw (button)
  (declare (ignore button))
  (com-redraw))

(defun button-infos (button)
  (declare (ignore button))
  (com-show-map-infos))

(defun button-reset (button)
  (declare (ignore button))
  (com-reset-map))

;;;
;;;
;;;

(define-map-viewer-command (com-reset-map :name "Reset Map")
    ()  
  (reset-map-range)
  (com-redraw))

(define-map-viewer-command (com-kill-bindings :name "Kill Bindings")
    ()  
  (unhighlight-all)
  (com-clear-display)
  (com-redraw))

;;;
;;;
;;;

(define-map-viewer-command (com-undo :name "Undo")
    ()
  (with-map-viewer-frame (map-viewer-frame)
    (with-slots (unselect-stack selected-os-keys) map-viewer-frame	
      (let ((first (pop unselect-stack)))
	(when first
	  (cond ((typep first 'db-object) ; undo hide object
		 (setf (dont-display first) nil)
		 (com-clear-display)
		 (com-draw-current-map))
		(t			
		 (setf selected-os-keys first)))
	  (generate-name-for-undo-command)
	  (com-draw-current-map)
	  (redisplay-frame-pane map-viewer-frame 'os-selector))))))


(defmethod accept-buttons ((frame map-viewer) stream)
  (let ((changed nil))
    
    (formatting-table (stream :x-spacing '(2 :character))
      
      (formatting-row (stream)
	
	(formatting-cell (stream :align-x :center)
	  (multiple-value-bind (bool ptype changed1)
	      (accept 'boolean
		      :prompt "Autotracking"
		      :stream stream :default *autotracking-mode*		    
		      :query-identifier 'autotracking)
	    (declare (ignore ptype))
	    (setf changed (or changed changed1))
	    (when changed1 (setf *autotracking-mode* bool))))
	
	(formatting-cell (stream :align-x :center)
	  (multiple-value-bind (bool ptype changed2)
	      (accept 'boolean
		      :prompt "Map Text"
		      :stream stream :default *display-map-text-mode*		    
		      :query-identifier 'display-text)
	    (declare (ignore ptype))
	    (setf changed (or changed changed2))	
	    (when changed2 (setf *display-map-text-mode* bool))))
	
	(formatting-cell (stream :align-x :center)
	  (multiple-value-bind (bool ptype changed3)
	      (accept 'boolean
		      :prompt "Nodes"
		      :stream stream :default *display-nodes-mode*		    
		      :query-identifier 'display-nodes)
	    (declare (ignore ptype))
	    (setf changed (or changed changed3))	
	    (when changed3 (setf *display-nodes-mode* bool))))

	(formatting-cell (stream :align-x :center)	
	  (multiple-value-bind (bool ptype changed4)
	      (accept 'boolean
		      :prompt "Sensitive Objects"
		      :stream stream :default *sensitive-objects-mode*		    
		      :query-identifier 'sensitive-objects-mode)
	    (declare (ignore ptype))
	    (setf changed (or changed changed4))
	    (when changed4 (setf *sensitive-objects-mode* bool))))
	
	
	(formatting-cell (stream :align-x :center)	
	  (multiple-value-bind (bool ptype changed5)
	      (accept 'boolean
		      :prompt "Single Boxes"
		      :stream stream :default (and *sensitive-objects-mode* *single-box-mode*)
		      :query-identifier 'single-box-mode)
	    (declare (ignore ptype))
	    (setf changed (or changed changed5))
	    (when changed5 
	      (setf *single-box-mode*
		(if bool :position nil)))))
	
	
	(formatting-cell (stream :align-x :center)	
	  (multiple-value-bind (bool ptype changed6)
	      (accept 'boolean
		      :prompt "Warnings"
		      :stream stream :default *warn-mode*		    
		      :query-identifier 'warn-mode)
	    (declare (ignore ptype))	    
	    (setf changed (or changed changed6))
	    (when changed6 (setf *warn-mode* bool))))
	
	(formatting-cell (stream :align-x :center)	
	  (multiple-value-bind (bool ptype changed7)
	      (accept 'boolean
		      :prompt "Decompose"
		      :stream stream :default *decompose-mode*		    
		      :query-identifier 'decompose-mode)
	    (declare (ignore ptype))
	    (setf changed (or changed changed7))
	    (when changed7 (setf *decompose-mode* bool))))
	
	(formatting-cell (stream :align-x :center)
	  (multiple-value-bind (bool ptype changed8)
	      (accept 'boolean
		      :prompt "Show Bindings"
		      :stream stream :default *display-binding-names-mode*
		      :query-identifier 'display-bindings)
	    (declare (ignore ptype))	    
	    (setf changed (or changed changed8))
	    (when changed8 (setf *display-binding-names-mode* bool))))
	
	(when (and changed *autotracking-mode*)
	  (com-draw-current-map))))))

(defmethod accept-os-selection ((frame map-viewer) stream)
  (with-map-viewer-frame (map-viewer-frame)        
    (let ((y 0))
      (flet ((set-item (item)
	       (let ((item (lookup-os item)))
		 (draw-text* stream 
			     (format nil "~A ~A ~A" (first item) (second item) 
				     (let ((third (third item)))
				       (if (listp third)
					   third
					 (list third))))
			     0 y)))
	     (set-unknown-item (item)
	       (draw-text* stream 
			   (format nil "??? ~A ???" item)
			   0 y)))
	
	(with-slots (present-os-keys selected-os-keys) map-viewer-frame
	  (dolist (item *unknown-os-keys*)
	    (incf y 14)
	    (let ((selected  (member item selected-os-keys :test #'equal)))	    	
	      (updating-output (stream :unique-id item
				       :cache-value (list y selected)
				       :cache-test #'equal)
		(with-output-as-presentation 
		    (stream item 'os-key-item)
		  (if selected
		      (with-drawing-options (stream :text-style +selected-item-ts+
						    :ink +selected-item-i+)
			(set-unknown-item 
			 item))
		    (with-drawing-options (stream :text-style +unselected-item-ts+
						  :ink +unselected-item-i+)
		      (set-unknown-item item)))))))
	  
	  (dolist (item *os*)
	    (let ((item (second item)))
	      (incf y 14)
	      (let ((selected (member item selected-os-keys :test #'equal)))
		(updating-output (stream :unique-id item
					 :cache-value (list y selected) 
					 :cache-test #'equal)
		  (if (member item present-os-keys :test #'equalp)
		      (with-output-as-presentation 
			  (stream item 'os-key-item)
			(if selected
			    (with-drawing-options (stream :text-style +selected-item-ts+
							  :ink +selected-item-i+)
			      (set-item item))
			  (with-drawing-options (stream :text-style +unselected-item-ts+
							:ink +unselected-item-i+)
			    (set-item item))))
		    (with-drawing-options (stream :text-style +grayed-item-ts+
						  :ink +grayed-item-i+)	    
		      (set-item item))))))))))))

;;;
;;;
;;;

(define-map-viewer-command (com-select-os-key-item :name "Select Key")
    ((object 'os-key-item))  
  (with-map-viewer-frame (map-viewer-frame)
    (with-slots (present-os-keys selected-os-keys) map-viewer-frame	
      (setf selected-os-keys	
	(if (member object selected-os-keys :test #'equalp)
	    (remove object selected-os-keys :test #'equalp)
	  (cons object selected-os-keys)))
      (when *autotracking-mode*
	(com-draw-current-map)))))

(define-presentation-to-command-translator select-os-key-item
    (os-key-item com-select-os-key-item map-viewer
		 :gesture :select)		   
  (object)
  (list object))

;;;
;;;
;;;

(define-map-viewer-command (com-select-all-os-keys :name "Select All Keys")
    ()
  (with-map-viewer-frame (map-viewer-frame)
    (with-slots (present-os-keys selected-os-keys) map-viewer-frame
      (register-selected-keys-for-undo)
      (setf selected-os-keys present-os-keys)
      (com-draw-current-map))))


(define-map-viewer-command (com-unselect-all-os-keys :name "Unselect All Keys")
    ()
  (with-map-viewer-frame (map-viewer-frame)
    (with-slots (present-os-keys selected-os-keys) map-viewer-frame	
      (register-selected-keys-for-undo)
      (setf selected-os-keys nil)
      (com-draw-current-map))))

;;;
;;;
;;;


(define-map-viewer-command (com-unselect-os-keys-of-object :name "Unselect OS Keys Of Object")
    ((object 'db-object))  
  (with-map-viewer-frame (map-viewer-frame)
    (with-slots (present-os-keys selected-os-keys) map-viewer-frame	
      (register-selected-keys-for-undo)
      (dolist (os (all-os object))
	(setf selected-os-keys
	  (delete os selected-os-keys :test #'equalp)))
      (com-draw-current-map))))

(define-map-viewer-command (com-unselect-all-but-os-keys-of-object :name "Unselect All But OS Keys Of Object")
    ((object 'db-object))
  (with-map-viewer-frame (map-viewer-frame)
    (with-slots (present-os-keys selected-os-keys) map-viewer-frame
      (register-selected-keys-for-undo)
      (setf selected-os-keys
	(all-os object))
      (com-draw-current-map))))


(define-presentation-to-command-translator unselect-os-keys-of-object
    (db-object com-unselect-os-keys-of-object map-viewer
	       :gesture :unselect-os-keys-of-object)		   
  (object)
  (list object))

(define-presentation-to-command-translator unselect-all-but-os-keys-of-object
    (db-object com-unselect-all-but-os-keys-of-object map-viewer
	       :gesture :unselect-all-but-os-keys-of-object)
  (object)
  (list object))

;;;
;;;
;;;

(define-map-viewer-command (com-lookup-os-key :name "Lookup OS Key")
    ()  
  (with-map-viewer-frame (map-viewer-frame)
    (with-slots (unselect-stack) map-viewer-frame	
      (let* ((stream (get-frame-pane map-viewer-frame 'display))
	     (os-number
	      (accepting-values (stream :own-window t :label "Lookup OS Key:")
		(accept 'integer :stream stream :view 'gadget-dialog-view))))
	(window-clear *standard-output*)
	(terpri)
	(format t "  ~A~%" 
		(lookup-os os-number))))))

;;;
;;;
;;;

(define-map-viewer-command (com-hide-object :name "Hide Object")
    ((object 'db-object))
  (register-hide-for-undo object) 
  (setf (dont-display object) t)
  (com-clear-display)
  (com-draw-current-map))

(define-presentation-to-command-translator hide-object
    (db-object com-hide-object map-viewer
	       :gesture :hide-object)   
  (object)
  (list object))

;;;
;;;
;;;


(defun show-current-map-range (&key (record t)
				    (coordinates t)
				    stream
				    transformation)
  (with-map-viewer-frame (frame)
    (let ((stream (or stream (get-frame-pane frame 'overview))))
      (with-slots  (db overview-transformation
		    current-map-range 
		    current-map-position current-map-radius) frame
	(flet ((draw-it ()
		 (with-drawing-options (stream :transformation 
					       (or transformation overview-transformation))
		   
		   (draw-circle* stream
				 (first current-map-position)
				 (second current-map-position)
				 (- current-map-radius 4)
				 :ink +flipping-ink+
				 :line-thickness 2
				 :filled nil)
		   
		   (draw-rectangle* stream
				    (x (pmin current-map-range))
				    (y (pmin current-map-range))
				    (x (pmax current-map-range))
				    (y (pmax current-map-range))			  
				    :line-thickness 2
				    :ink +flipping-ink+
				    :filled t))))
	  
	  (when db
	    (when coordinates 
	      (show-coordinates frame (get-frame-pane frame 'coordinates)))
	    (if record
		(with-output-as-presentation (stream current-map-range
						     'current-map-range)
		  (draw-it))
	      (with-output-recording-options (stream :draw t :record nil)
		(draw-it)))))))))

(define-map-viewer-command (com-adjust-map-center :name "Adjust Map Center")
    ((object 'current-map-range))
  (declare (ignore object))
  (adjust-map-center))

(define-map-viewer-command (com-adjust-map-extent :name "Adjust Map Extent")
    ((object 'current-map-range))
  (declare (ignore object))
  (adjust-map-extent))


(define-presentation-to-command-translator adjust-map-center
    (current-map-range com-adjust-map-center map-viewer
		       :gesture :select)
  (object)
  (list object))


(define-presentation-to-command-translator adjust-map-extent
    (current-map-range com-adjust-map-extent map-viewer
		       :gesture :adjust)
  (object)
  (list object))


(defmethod set-current-map-position-and-radius (xcenter ycenter r)
  (with-map-viewer-frame (frame)
    (with-slots (current-map-position current-map-radius) frame
      (setf current-map-position 
	(list xcenter ycenter))
      (setf current-map-radius r))
    (let ((*autotracking-mode* nil))
      (adjust-current-map-range))))

(defun full-map-range ()
  (with-map-viewer-frame (frame)
    (with-slots (db current-map-range) frame
      (when db 
	(with-slots (xmin ymin xmax ymax) (spatial-index db)
	  (setf current-map-range
	    (make-bounding-box xmin ymin xmax ymax)))
  	(recalculate-transformation (get-frame-pane frame 'display))))))

(defun reset-map-range ()
  (with-map-viewer-frame (frame)
    (with-slots (db current-map-radius current-map-position) frame
      (when db 
	(with-slots (xmin ymin xmax ymax) (spatial-index db)
	  (setf current-map-radius (floor (- xmax xmin) 5))
	  (setf current-map-position
	    (list (floor (+ xmin xmax) 2)
		  (floor (+ ymin ymax) 2)))
	  (let ((*autotracking-mode* nil))
	    (adjust-current-map-range)))))))

(defun adjust-current-map-range ()
  (with-map-viewer-frame (frame)
    (with-slots (current-map-range 
		 current-map-position current-map-radius) frame
      (let ((xcenter (first current-map-position))
	    (ycenter (second current-map-position)))
	
	(if (not current-map-range)
	    (setf current-map-range
	      (make-bounding-box 
	       (- xcenter current-map-radius) (- ycenter current-map-radius)
	       (+ xcenter current-map-radius) (+ ycenter current-map-radius)))
	  (with-slots (pmin pmax) current-map-range
	    (setf (x pmin) (- xcenter current-map-radius) 
		  (y pmin) (- ycenter current-map-radius)
		  (x pmax) (+ xcenter current-map-radius) 
		  (y pmax) (+ ycenter current-map-radius))))
	
	(recalculate-transformation (get-frame-pane frame 'display))
	
	(when *autotracking-mode*
	  (com-draw-current-map))))))


(defun adjust-map-center ()  
  (with-map-viewer-frame (map-viewer-frame)
    (let ((stream (get-frame-pane map-viewer-frame 'overview)))
      (with-slots (current-map-position 
		   db
		   old-overview-vp-width 
		   old-overview-vp-height) map-viewer-frame  
	
	(when db
	  
	  (block track-pointer
	    (with-output-recording-options (stream :draw t :record nil)
	      (tracking-pointer (stream)
		(:pointer-motion (x y)
				 (with-slots (xmin ymin xmax ymax) (spatial-index db)
				   (let* ((xscale (/ old-overview-vp-width  (- xmax xmin)))
					  (yscale (/ old-overview-vp-height (- ymax ymin)))
					  (xcenter	     
					   (+ xmin (* (/ 1 xscale) x)))
					  (ycenter
					   (+ ymax (* (- (/ 1 yscale)) y))))
				     (show-current-map-range :record nil)
				     (setf current-map-position 
				       (list xcenter ycenter))
				     (adjust-current-map-range)				 				 
				     (show-current-map-range :record nil))))
		(:pointer-button-press ()
				       (return-from track-pointer))))))))))


(defun adjust-map-extent ()  
  (with-map-viewer-frame (map-viewer-frame)
    (let ((stream (get-frame-pane map-viewer-frame 'overview)))
      (with-slots (current-map-radius
		   current-map-position
		   db
		   old-overview-vp-width 
		   old-overview-vp-height) map-viewer-frame  
	
	(when db
	  (block track-pointer
	    (with-output-recording-options (stream :draw t :record nil)
	      (tracking-pointer (stream)
		(:pointer-motion (x y)			       
				 (with-slots (xmin ymin xmax ymax) (spatial-index db)
				   (let* ((xscale (/ old-overview-vp-width  (- xmax xmin)))
					  (yscale (/ old-overview-vp-height (- ymax ymin)))
					  (xcenter (first current-map-position))
					  (ycenter (second current-map-position))
					  (xr	     
					   (- xcenter (+ xmin (* (/ 1 xscale) x))))
					  (yr		       
					   (- ycenter (+ ymax (* (- (/ 1 yscale)) y))))
					  (r (sqrt (+ (* xr xr) (* yr yr)))))
				     (unless (zerop r)
				       (show-current-map-range :record nil)
				       (setf current-map-radius r)
				       (adjust-current-map-range)					       
				       (show-current-map-range :record nil)))))
		(:pointer-button-press ()			
				       (return-from track-pointer))))))))))


(defmethod show-coordinates ((frame map-viewer) stream)
  (with-slots (current-map-range db) frame
    (when db
      (window-clear stream)
      (terpri stream)
      (with-slots (xmin ymin xmax ymax) (spatial-index db)
	(format stream "  Full Map: (~A,~A) - (~A,~A)"
		(round xmin) (round ymin)
		(round xmax) (round ymax)))
      (terpri stream)
      (with-slots (pmin pmax) current-map-range
	(format stream "  Map Range: (~A,~A) - (~A,~A)"
		(round (x pmin)) (round (y pmin))
		(round (x pmax)) (round (y pmax)))
	(terpri stream)
	(let ((size (abs (round (- (x pmin) (x pmax))))))
	  (format stream "  Range: ~A * ~A meters" size size))))))

;;;
;;;
;;;

(defun selectedp (object)
  (with-map-viewer-frame (map-viewer-frame)
    (with-slots (selected-os-keys) map-viewer-frame
      (some #'(lambda (os)
		(member os selected-os-keys :test #'equal))
	    (all-os object)))))

(defmethod draw-current-map ((frame map-viewer) stream &key 
						       overview 
						       show-types-p 
						       (clear-p t) (show-bindings t))
  (with-slots (current-map-range
	       current-map-transformation
	       selected-os-keys db text-objects) frame    
    (with-slots (pmin pmax) current-map-range
      (when clear-p (window-clear stream))
      (when db
	(with-drawing-options (stream :transformation current-map-transformation
				      :clipping-region 
				      (make-bounding-rectangle (x pmin) (y pmin)
							       (x pmax) (y pmax)))
	  (with-selected-objects (cur-obj current-map-range
					  (primary-p cur-obj) :inside)
	    (when (not (dont-display cur-obj))
	      (cond 
	       ((or
		 (and *decompose-mode* 
		      (typep cur-obj 'db-polygon))
		 (selectedp cur-obj))
		(draw cur-obj stream 
		      :show-bindings show-bindings
		      :overview overview
		      :show-types-p show-types-p))
	       (t (when *warn-mode* (warn "~%Object not drawn! ~A" cur-obj))))))
	  
	  (dolist (cur-obj text-objects)
	    (when (box-overlaps-box-p (bbox cur-obj)
				      current-map-range)
	      (draw cur-obj stream :overview overview))))))))

(defun draw-current-map-to-foreign-stream (stream &key width height show-bindings
						       show-types-p
						       overview)
  (with-map-viewer-frame (map-viewer)
    (recalculate-transformation stream :width width :height height)
    (draw-current-map map-viewer stream :show-bindings show-bindings 
		      :clear-p nil
		      :show-types-p show-types-p
		      :overview overview)
    (recalculate-transformation (get-frame-pane map-viewer 'display))))



#+(and mcl antiker-mac)
(defmethod draw-overview ((frame map-viewer) stream)
  (with-slots (overview-pattern current-map-range
	       overview-transformation 
	       db) frame
    (show-current-map-range)))


(defmethod draw-overview ((frame map-viewer) stream)
  (with-slots (overview-pattern current-map-range
	       overview-transformation 
	       db) frame
    (when db
      
      (multiple-value-bind (viewport-width viewport-height)
	  (bounding-rectangle-size (bounding-rectangle (window-viewport stream)))
	(unless overview-pattern
	  (setf overview-pattern 
	        (with-output-to-pixmap (stream 
				        (sheet-medium (get-frame-pane frame 'overview))
				        :width viewport-width
				        :height viewport-height)
	          (with-drawing-options (stream :transformation overview-transformation)		    
		    (let ((*display-nodes-mode* nil))
		      (dolist (obj (objects db))
		        (when (primary-p obj)
		          (draw obj stream :overview t))))))))
	(draw-pixmap* stream
                      overview-pattern
                      0 0)
	(show-current-map-range)))))

;;;
;;;
;;;

(defmethod draw :around ((object db-object) stream &key show-bindings
							show-types-p)
  (labels ((draw-it ()
	     (if *sensitive-objects-mode*
		 (with-output-as-presentation
		     (stream object (type-of object)
			     :single-box *single-box-mode* :allow-sensitive-inferiors t)
		   (call-next-method))
	       (call-next-method))))
    (if (consp show-bindings)
	(let* ((entry (assoc object show-bindings))
	       (text1 (second entry))
	       (text2 (third entry)))
	  (if text1
	      (with-drawing-options (stream :ink +ink-bound-to+ 
					    :line-thickness *highlight-line-thickness*)
		(draw-it)
		(when *display-binding-names-mode*
		  (multiple-value-bind (lx ly)
		      (label-pos object)
		    (draw-text* stream 
				(format nil "~A" text1)
				lx ly)))
		(when show-types-p 
		  (multiple-value-bind (lx ly)
		      (label-pos object)		    
		    (let ((i ly))
		      (dolist (os-text text2)
			(draw-text* stream 
				    (format nil "~A" os-text)
				    lx i)
			(incf i +text-height+)))))
		(draw-it))
	    (draw-it)))
      (if show-bindings
	  (if (bound-to object)
	      (with-drawing-options (stream :ink +ink-bound-to+ 
					    :line-thickness *highlight-line-thickness*)
		(draw-it)
		(when (and (not (eq (bound-to object) t))
			   *display-binding-names-mode*)
		  (multiple-value-bind (lx ly)
		      (label-pos object)
		    (draw-text* stream 
				(format nil "~A" (bound-to object))
				lx ly))))
	    (with-drawing-options (stream :ink +black+)
	      (draw-it)))
	(with-drawing-options (stream :ink +black+)
	  (draw-it))))))


(defmethod draw ((object t) stream &key &allow-other-keys)
  (declare (ignore stream))
  ())

(defun draw-chain-or-polygon (object stream &key overview show-bindings)
  (if *decompose-mode*
      (progn
	(when (and (selectedp object)
		   (not overview))
	  (draw-marker* stream
			(x (pcenter object))
			(y (pcenter object))
			5 
			:ink +magenta+))
	(dolist (segment (segments object))
	  (when (not (dont-display segment))
	    (when (selectedp segment)
	      (draw segment stream
		    :show-bindings show-bindings 
		    :overview overview)))))
    (when (selectedp object)
      (unless overview
	(draw-marker* stream
		      (x (pcenter object))
		      (y (pcenter object))
		      5 
		      :ink +magenta+))
      (dolist (segment (segments object))
	(when (not (dont-display segment))
	  (draw segment stream
		:show-bindings show-bindings
		:overview overview))))))

(defmethod draw ((object db-line) stream &key overview show-bindings)
  (with-slots (p1 p2) object
    (draw-line*
     stream
     (x p1) (y p1)
     (x p2) (y p2))
    (when *display-nodes-mode*
      (draw p1 stream :overview overview :show-bindings show-bindings)
      (draw p2 stream :overview overview :show-bindings show-bindings))))


(defmethod draw ((object db-chain) stream &key overview show-bindings &allow-other-keys)
  (draw-chain-or-polygon object stream :overview overview :show-bindings show-bindings))

(defmethod draw ((object db-polygon) stream &key overview show-bindings &allow-other-keys)
  (draw-chain-or-polygon object stream :overview overview :show-bindings show-bindings))

(defmethod draw ((object db-point) stream &key &allow-other-keys)
  (with-slots (x y name) object
    (if (primary-p object)
	(progn
	  (draw-circle*
	   stream
	   x y 8
	   :filled nil)
	  (draw-circle*
	   stream
	   x y 5
	   :filled t))
      (draw-circle*
       stream
       x y (if (bound-to object) 
	       8
	     4)
       :filled t))))


(defmethod draw ((object map-text) stream &key &allow-other-keys)
  (when *display-map-text-mode*
    (with-drawing-options (stream :transformation (matrix object)
				  :ink +blue+)
      (with-slots (text) object
	(draw-vector-text text stream)))))

;;;
;;;
;;;

(defmethod run-frame-top-level :before ((frame map-viewer) &key)
  (initialize-map-viewer frame))

(defmethod frame-standard-input ((frame map-viewer))
  (get-frame-pane frame 'command))

(defmethod frame-standard-output ((frame map-viewer))
  (get-frame-pane frame 'infos))

(defmethod frame-error-output ((frame map-viewer))
  (get-frame-pane frame 'infos))

(defmethod initialize-map-viewer ((frame map-viewer))
  (setf *unknown-os-keys* nil))

;;;
;;;
;;;

(defmethod measure-and-draw-map ((frame map-viewer))
  (with-slots (overview-transformation
	       present-os-keys 
	       overview-pattern
	       current-map-position current-map-radius db) frame
    
    (with-slots (xmin ymin xmax ymax) (spatial-index db)
      (window-clear *standard-output*)
      
      (setf overview-pattern nil)
      (setf present-os-keys nil)
      
      (setf *unknown-os-keys* nil)
      
      (labels ((insert-os-key (object)	      
		 (dolist (item (all-os object))
		   (if (lookup-os item)
		       (pushnew item
				present-os-keys)
		     (pushnew item 
			      *unknown-os-keys*)))
		 
		 (if (or (typep object 'db-polygon)
			 (typep object 'db-chain))
		     (dolist (object (segments object))
		       (insert-os-key object)))))
	
	(dolist (obj (objects db))
	  (insert-os-key obj))
	
	(show-map-infos)
	
	(change-os-key-selection)
	
	(recalculate-transformation (get-frame-pane frame 'overview) :force t)
	
	(reset-map-range)
	(com-draw-current-map)
	(draw-overview frame (get-frame-pane frame 'overview))))))



(defmethod recalculate-transformation (sheet &key width height)
  (with-map-viewer-frame (map-viewer-frame)
    (with-slots (db current-map-range current-map-transformation) map-viewer-frame
      (when db
	(let* ((vbb
		(window-viewport sheet))
	       (vb-width
		(or width (bounding-rectangle-width vbb)))
	       (vb-height
		(or height (bounding-rectangle-height vbb)))
	       
	       (xscale (/ vb-width (- (x (pmax current-map-range)) (x (pmin current-map-range)))))
	       
	       (yscale (/ vb-height (- (y (pmax current-map-range)) (y (pmin current-map-range)))))
	       
	       (trans1 (make-translation-transformation (- (x (pmin current-map-range)))
							(- (y (pmax current-map-range)))))
	       (trans2 (make-scaling-transformation  
			xscale 
			(- yscale)))
	       (trans 
		(compose-transformations trans2 trans1)))
	  
	  (setf current-map-transformation trans))))))


(defmethod recalculate-transformation ((pane overview-pane) &key force)
  (with-map-viewer-frame (map-viewer-frame)
    (with-slots (db
		 overview-transformation 
		 old-overview-vp-width 
		 old-overview-vp-height) map-viewer-frame  
      (when db
	(with-slots (xmin ymin xmax ymax) (spatial-index db)
	  (let* ((vpbb
		  (window-viewport 
		   (get-frame-pane map-viewer-frame 'overview)))
		 (vp-width
		  (bounding-rectangle-width vpbb))
		 (vp-height
		  (bounding-rectangle-height vpbb)))
	    
	    (when (or force
		      (not old-overview-vp-width) (not old-overview-vp-height)
		      (not (and (= old-overview-vp-width vp-width)
				(= old-overview-vp-height vp-height))))
	      (let*
		  ((trans1 (make-translation-transformation (- xmin) (- ymax)))
		   (trans2 (make-scaling-transformation  
			    (/ vp-width (- xmax xmin))
			    (- (/ vp-height (- ymax ymin)))))		   
		   (trans (compose-transformations trans2 trans1)))
		
		(setf old-overview-vp-width vp-width
		      old-overview-vp-height vp-height)
		(setf overview-transformation trans)))))))))

(defmethod calculate-transformation ((pane application-pane))
  (with-map-viewer-frame (map-viewer-frame)
    (with-slots (db) map-viewer-frame  
      (when db
	(with-slots (xmin ymin xmax ymax) (spatial-index db)
	  (let* ((vpbb
		  (window-viewport pane))
		 (vp-width
		  (bounding-rectangle-width vpbb))
		 (vp-height
		  (bounding-rectangle-height vpbb))
		 
		 (trans1 (make-translation-transformation (- xmin) (- ymax)))
		 (trans2 (make-scaling-transformation  
			  (/ vp-width (- xmax xmin))
			  (- (/ vp-height (- ymax ymin))))))
	    (compose-transformations trans2 trans1)))))))

;;;
;;;
;;;


(define-map-viewer-command (com-show-map-infos :name "Show Map Infos")
    ()
  (window-clear *standard-output*)
  (show-map-infos))

(defun show-map-infos (&optional (stream *standard-output*))    
  (with-map-viewer-frame (map-viewer-frame)
    (with-slots (map-filename db
		 present-os-keys) map-viewer-frame
      (when db
	(fresh-line stream)
	(terpri stream)
	(format stream "  Infos For Map ~A:~%" map-filename)
	(format stream "  ~A Objects Loaded, ~%" (length (objects db)))
	(format stream "  ~A Unknown Object OS Keys,~%" (length *unknown-os-keys*))
	(format stream "  ~A Known Object OS Keys. " (length present-os-keys))))))

;;;
;;;
;;;

(define-map-viewer-command (com-load-sqd-map :name "Load SQD Map")
    ()
  (with-map-viewer-frame (map-viewer-frame)
    (let ((file (file-selector "Load SQD Map" "visco:maps;" "sqd")))
      (when file	
	(with-slots (db map-filename text-objects) map-viewer-frame
	  (setf text-objects nil)
	  (setf db (make-db-from-sqd-file file))
	  (setf map-filename file)
	  (measure-and-draw-map map-viewer-frame))))))


(defun change-os-key-selection ()  
  (with-map-viewer-frame (map-viewer-frame)
    (with-slots (present-os-keys selected-os-keys) map-viewer-frame
      (setf selected-os-keys present-os-keys))))

;;;
;;;
;;;

(define-map-viewer-command (com-load-map :name "Load Map")
    ()
  (with-map-viewer-frame (map-viewer-frame)
    (let ((file (file-selector "Load Map" "visco:maps;" "db")))      
      (when file
	(with-slots (db map-filename text-objects) map-viewer-frame
	  (let ((loaded-objects
                 (load-persistent-object file)))
            (setf *image-map* loaded-objects)
	    (setf db (first loaded-objects))
	    (install-as-current-db (first loaded-objects))
	    (setf text-objects (second loaded-objects))
	    (setf map-filename file)
	    (measure-and-draw-map map-viewer-frame)))))))


(define-map-viewer-command (com-install-image-map :name "Install Image Map")
    ()
  (with-map-viewer-frame (map-viewer-frame)
    (with-slots (db map-filename text-objects) map-viewer-frame
      (let ((loaded-objects *image-map*))
        (when loaded-objects
          (setf db (first loaded-objects))
          (install-as-current-db (first loaded-objects))
          (setf text-objects (second loaded-objects))
          (setf map-filename "Image Map")
          (measure-and-draw-map map-viewer-frame))))))


(define-map-viewer-command (com-save-map :name "Save Map")
    ()
  (with-map-viewer-frame (map-viewer-frame)
    (let ((file (file-selector "Save Map" "visco:maps;" "db" :save t)))
      (when file	
	(with-slots (db text-objects) map-viewer-frame
	  (make-object-persistent (list db text-objects)
                                   file))))))

;;;
;;;
;;;

(defun com-draw-current-map ()
  (with-map-viewer-frame (map-viewer-frame)
    (draw-current-map map-viewer-frame (get-frame-pane map-viewer-frame 'display))))

;;;
;;;
;;;

(define-map-viewer-command (com-describe-object :name "Describe Object")
    ((object 'db-object))
  (labels ((describe-this-object (object level &key slave)
	     (dolist (os (all-os object))
	       (multiple-value-bind (string found)
		   (lookup-os os)
		 (my-format *standard-output* level
			    "~A  "
			    (if found
				(first string)
			      "Unknown Object!"))))
	     (when (and (not slave)
			(or (typep object 'db-chain)
			    (typep object 'db-polygon)))
	       (fresh-line *standard-output*)
	       (my-format *standard-output* level "Has Segments:")
	       (dolist (segment (segments object))
		 (terpri *standard-output*)
		 (describe-this-object segment (1+ level)
				       :slave slave)))
	     (when (and slave (part-of object))
	       (dolist (part (part-of object))
		 (fresh-line *standard-output*)
		 (my-format *standard-output* level "Part Of:~%")
		 (describe-this-object part (1+ level)
				       :slave slave)))))
    
    (window-clear *standard-output*)
    (terpri)
    (describe-this-object object 0
			  :slave
			  (part-of object))))

(define-presentation-to-command-translator describe
    ((db-object) com-describe-object map-viewer 
		 :gesture :select)
  (object)
  (list object))


;;;
;;;
;;;

(define-map-viewer-command (com-clear-display :name "Clear Map Display")
    ()
  (with-map-viewer-frame (map-viewer-frame)  
    (let ((stream (get-frame-pane map-viewer-frame 'display)))
      (window-clear stream))))

(define-map-viewer-command (com-redraw :name "Redraw Current Map")
    ()
  (with-map-viewer-frame (frame)
    (redisplay-frame-pane frame 'overview)
    (com-clear-display)
    (com-draw-current-map)))

;;;
;;;
;;;

#+:allegro
(define-map-viewer-command (com-print-whole-map :name "Print Whole Map")
    ()
  (with-map-viewer-frame (map-viewer-frame)
    (with-open-stream 
	(pipe (excl:run-shell-command  (format nil "lpr -P~A -h" '|r131_hp|)
				       :input :stream :wait nil))
      (with-output-to-postscript-stream (stream pipe 
						:orientation :landscape
						:scale-to-fit t)
	(let ((*sensitive-objects-mode* nil))		     
	  
	  (draw-current-map map-viewer-frame stream :clear-p nil))))))

#+:allegro
(define-map-viewer-command (com-print-keylist :name "Print Keylist")
    ()
  (with-map-viewer-frame (map-viewer-frame)
    (with-slots (present-os-keys map-filename) map-viewer-frame
      (with-open-stream 
	  (pipe (excl:run-shell-command  (format nil "lpr -P~A -h" '|r131_hp|)
					 :input :stream :wait nil))
	(with-output-to-postscript-stream (stream pipe 
						  :orientation :landscape
						  :scale-to-fit t)
	  
	  (format stream "OS Keylist For File ~A." map-filename)
	  (show-map-infos stream)
	  (format stream "~%~%Known OS Keys:~%~%")
	  
	  (dolist (item present-os-keys)
	    (princ item stream)
	    (terpri stream))
	  
	  (format stream "~%Unknown OS Keys:~%~%") 
	  
	  (dolist (item *unknown-os-keys*)
	    (princ item stream)
	    (terpri stream)))))))

;;;
;;;
;;;

(define-map-viewer-command (com-quit :name "Quit")
    ()
  (with-map-viewer-frame (map-viewer-frame)
    (let ((yes-or-no (notify-user map-viewer-frame
				  "Quit selected! Are you sure?"
				  :style :question)))
      (when yes-or-no
	(frame-exit map-viewer-frame)))))

;;;
;;;
;;;

(define-map-viewer-command (com-highlight-intersecting-objects* :name "Highlight Intersecting Objects")
    ()
  (let ((object
	 (accept 'db-object)))
    (show-intersecting-objects object)))

(define-map-viewer-command (com-highlight-intersecting-objects)
    ((object 'db-object :gesture nil))  
  (show-intersecting-objects object))

(defmethod show-intersecting-objects ((object db-object))
  (highlight-all
   (intersects object))
  (com-draw-current-map))

;;;
;;;
;;;

(define-map-viewer-command (com-highlight-containing-objects* :name "Highlight Containing Objects")
    ()
  (let ((object
	 (accept 'db-object)))
    (show-containing-objects object)))

(define-map-viewer-command (com-highlight-containing-objects)
    ((object 'db-object :gesture nil))  
  (show-containing-objects object))

(defmethod show-containing-objects ((object db-object))
  (highlight-all
   (inside object))
  (com-draw-current-map))

;;;
;;;
;;;

(define-map-viewer-command (com-highlight-contained-objects* :name "Highlight Contained Objects")
    ()
  (let ((object
	 (accept 'db-polygon)))
    (show-contained-objects object)))

(define-map-viewer-command (com-highlight-contained-objects)
    ((object 'db-polygon :gesture nil))  
  (show-contained-objects object))

(defmethod show-contained-objects ((object db-object))
  (highlight-all
   (contains object))
  (com-draw-current-map))


;;;
;;;
;;;


#|
(defmethod resize-sheet :after ((sheet display-pane) width height)
  (declare (ignore width height))
  (recalculate-transformation sheet)
  (com-draw-current-map))

(defmethod resize-sheet :after ((sheet overview-pane) width height)
  (declare (ignore width height))
  (recalculate-transformation sheet)
  (draw-overview (pane-frame sheet) sheet))

|#

(defun map-viewer (&key (force t)
			(process t)
			left
			top width height
		   &allow-other-keys)
  (let ((port (find-port)))
    #+:allegro
    (setf (clim:text-style-mapping port 
				   +map-viewer-text-style+)
      "-*-lucida-medium-r-normal-*-12-*-*-*-*-*-*-*")
    (when (or force (null *map-viewer-frame*))
      (unless left
	(multiple-value-bind (screen-width screen-height)
	    (bounding-rectangle-size 
	     (sheet-region (find-graft :port port)))
	  (setf left 0
		top 0
		width screen-width 
		height screen-height)))
      #+:Allegro
      (if process
	  (mp:process-run-function
	   "Map-Viewer"
	   #'(lambda ()
	       (setf *map-viewer-frame*
		 (make-application-frame
		  'map-viewer
		  :left (+ 40 left)
		  :top (+ 40 top)
		  :width (- width 80)
		  :height (- height 80)))
	       
	       (run-frame-top-level *map-viewer-frame*)))
	(run-frame-top-level *map-viewer-frame*))
      #+:mcl
      (if process
	  (ccl:process-run-function
	   "Map Viewer"
	   #'(lambda ()
	       (run-frame-top-level *map-viewer-frame*)))
	(run-frame-top-level *map-viewer-frame*))
      #-(or :Allegro :mcl)
      (run-frame-top-level *map-viewer-frame*)
      *map-viewer-frame*)))
