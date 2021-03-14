;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: GUI; Base: 10 -*-

(in-package gui)

(defconstant +x-no-of-thumbnails+ 
    #-visco-demo-mode 8
    #+visco-demo-mode 4)

(defconstant +y-no-of-thumbnails+ 
    #-visco-demo-mode 8
    #+visco-demo-mode 4)

(defconstant +spacing+ 10)

(defconstant +visco-text-style+
    (make-text-style :sans-serif
		     :roman
		     :normal))

(defconstant +info-text-style+ 
    (make-text-style :serif :roman :small))

(defconstant +command-listener-text-style+
    (make-text-style
     :sans-serif :roman :normal))

(defconstant +inspector-text-style+ 
    (make-text-style :fix nil :small))

(defconstant +pointer-doc-text-style+
    (make-text-style :sans-serif :bold :small))

(defvar +button-abort+)

(defvar +button-draw-thumbnails+)

(defvar +button-permutations+)

(defvar +button-next+)

(defvar +button-previous+)

(defvar +button-delete-all-pages+)

(defvar +button-delete-page+)

(defvar +button-delete-selected+)

(defvar +button-delete-unselected+)

(defvar +button-unselect-all+)

;;;
;;;
;;;

(defmethod output-record-refined-position-test ((record standard-presentation) x y)
  (let ((obj (presentation-object record)))
    (if (typep obj '(or gui-enclosure query-result))
	(object-selected-position-test obj record x y)
      (call-next-method))))

;;;
;;;
;;;

(define-application-frame visco-buttons ()
  ()
  (:panes 
   (pointer-documentation-pane
    (make-clim-stream-pane :type 'pointer-documentation-pane
			   :foreground +white+
			   :background +black+
			   :text-style +pointer-doc-text-style+
			   :scroll-bars nil
			   :min-height '(1 :line)
			   :max-height '(1 :line)
			   :height '(1 :line)))
   
   (object-buttons :application
		   :label "VISCO Objects"		 
		   :incremental-redisplay t		   
		   :borders nil
		   :scroll-bars nil
		   :textcursor nil
		   :initial-cursor-visibility :inactive
		   :display-function #'accept-object-buttons)
   
   (operator-buttons :application
		     :label "VISCO Operators"
		     :incremental-redisplay t
		     :scroll-bars nil
		     :textcursor nil
		     :initial-cursor-visibility :inactive
		     :display-function #'accept-choose-operator)
   
   (option-buttons :accept-values
		   :label "VISCO Options"
		   :scroll-bars nil
		   :display-function
		   `(accept-values-pane-displayer
		     :displayer ,#'(lambda (frame stream)
				     (accept-option-buttons
				      frame stream)))))

  (:layouts 
   (:default
       (vertically ()
         #+:mcl
	 (mcl-pane pointer-documentation-pane)
         #+:allegro
         pointer-documentation-pane
	 #+:mcl
         (vertically ()	 
	   (7/14 (mcl-pane object-buttons))
	   (3/14 (mcl-pane operator-buttons))
	   (4/14 (mcl-pane option-buttons)))
         #+:allegro
         (vertically ()	 
	   (7/14 object-buttons)
	   (3/14 operator-buttons)
	   (4/14 option-buttons))))))

(define-application-frame visco ()
  ((history :accessor history :initform nil)
   (undo-stack :accessor undo-stack :initform nil)
   (redo-stack :accessor redo-stack :initform nil)
   
   (current-focus :accessor current-focus :initform nil)
   (current-query :accessor current-query :initform (make-visco-gui-query))
   (current-transparency :accessor current-transparency :initform nil))
  
  (:command-table (visco
		   :inherit-from (file-table query-table)
		   :menu (("File" :menu file-table)
			  ("Control" :menu control-table)
			  ("Operators" :menu operator-table)
			  ("Query" :menu query-table))))
  
  (:panes         
   
   (display :application
	    :label "VISCO Query"
	    :scroll-bars nil
	    :end-of-line-action :allow
	    :end-of-page-action :allow
	    :textcursor nil
	    :incremental-redisplay t	    
	    :display-function #'draw-display)
   
   (infos :application
	  :label "VISCO Infos"
	  :end-of-line-action :allow
	  :text-style +info-text-style+
	  :scroll-bars :both)
   
   (control :application
	    :incremental-redisplay t	    
	    :label "VISCO Control"
	    :scroll-bars :both
	    :end-of-line-action :allow
	    :display-function #'accept-control-buttons)
   
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
			   :text-style +pointer-doc-text-style+
			   :scroll-bars nil
			   :min-height '(1 :line)
			   :max-height '(1 :line)
			   :height '(1 :line))))

  
  (:layouts
   (:default       
       #+:allegro
       (vertically ()	
	 (horizontally ()
	   (3/4 display)
	   (1/4 (vertically ()
		  (1/2 infos)
		  (1/2 control))))
	 command
	 pointer-documentation-pane)
       #+:mcl
       (vertically ()	
	 (horizontally ()
	   (3/4 (mcl-pane display))
	   (1/4 (vertically ()
		  (1/2 (mcl-pane infos))
		  (1/2 (mcl-pane control)))))
	 (mcl-pane command)
	 (mcl-pane pointer-documentation-pane)))))

(defclass overview-pane
    (application-pane)
  ())

(define-application-frame visco-inspector ()
  ((pages :accessor pages :initform nil)
   (selected-page :accessor selected-page :initform nil)
   (active-page :accessor active-page :initform nil)
   (selected-query-result :accessor selected-query-result :initform nil)
   
   (all-matches :accessor all-matches :initform nil) 
   
   (last-percent :initform nil)
   (last-time :initform nil)
   
   #+visco-demo-mode 
   (overview-pattern :initform nil)

   )
  (:panes            
   (query-results :application
		  :label 
		  #-visco-demo-mode "VISCO Query Results"
		  #+visco-demo-mode nil
		  :initial-cursor-visibility :inactive
		  :display-after-commands nil
		  :textcusor nil
		  :scroll-bars nil
		  :display-function #'draw-query-results)
   (selected-query-result :application
			  :label "VISCO Inspect Query Result"
			  :initial-cursor-visibility :inactive
			  :textcusor nil
			  :scroll-bars nil
			  :display-function #'draw-selected-query-result)
   #+visco-demo-mode
   (overview (make-pane 'overview-pane
			:scroll-bars nil
			:end-of-line-action :allow
			:end-of-page-action :allow
			:textcursor nil
			:display-function #'draw-overview))

   (inspector :application
	      :label "VISCO Compiler Infos"
	      :text-style +inspector-text-style+
	      :scroll-bars :both
	      :textcursor nil
	      :initial-cursor-visibility :inactive
	      :end-of-line-action :allow
	      :end-of-page-action :allow)

   (progress-bar :application
		 :label nil
		 :borders nil
		 :scroll-bars nil
		 :textcursor nil
		 :initial-cursor-visibility :inactive)
   (button-abort (setf +button-abort+
		   (make-pane 'push-button
			      :label "Abort Search"			   
			      :activate-callback 'abort-search)))
   (button-permutations (setf +button-permutations+
			  (make-pane 'toggle-button
				     :value *permutations*
				     :label "Permutations"
				     :value-changed-callback 'button-permutations)))

   (button-draw-thumbnails (setf +button-draw-thumbnails+
			     (make-pane 'toggle-button
					:value *draw-thumbnails*
					:label "Draw QRs"
					:value-changed-callback 'button-draw-thumbnails)))
   
   (button-previous (setf +button-previous+
		      (make-pane 'push-button
				 :label "<< Previous Page <<"
				 :activate-callback 'previous-page)))
   (button-next (setf +button-next+
		  (make-pane 'push-button
			     :label ">> Next Page >>"
			     :activate-callback 'next-page)))
   (button-delete-all-pages (setf +button-delete-all-pages+
			      (make-pane 'push-button
					 :label "Delete All Pages"
					 :activate-callback 'delete-all-pages)))
   (button-delete-page (setf +button-delete-page+
			 (make-pane 'push-button
				    :label "Delete Page"
				    :activate-callback 'delete-page)))
   
   (button-delete-selected (setf +button-delete-selected+
			     (make-pane 'push-button
					:label "Delete Selected"
					:activate-callback 'delete-selected)))
   (button-delete-unselected (setf +button-delete-unselected+
			       (make-pane 'push-button
					  :label "Delete Unselected"
					  :activate-callback 'delete-unselected)))   
   (button-unselect-all (setf +button-unselect-all+
			  (make-pane 'push-button
				     :label "Unselect All"
				     :activate-callback 'unselect-all)))
   
   (page-nr :application
	    :label nil
	    :borders nil
	    :scroll-bars nil
	    :textcursor nil
	    :initial-cursor-visibility :inactive
	    :display-function #'show-page-nr))
  (:layouts
   (:default    
     #+(and (not visco-demo-mode) allegro)
     (vertically ()
	 (1/2 
	  (horizontally ()
	    (1/2 (vertically ()
		   query-results
		   (15 progress-bar)
		   (30 (horizontally ()
			 (6/10 button-abort)
			 (2/10 button-permutations)
			 (2/10 button-draw-thumbnails)))
		   (30 (horizontally ()
			 (3/10 button-previous)
			 (3/10 button-next)
			 (3/10 button-unselect-all)
			 (1/10 page-nr)))
		   (30 (horizontally ()
			 (1/4 button-delete-page)
			 (1/4 button-delete-all-pages)
			 (1/4 button-delete-selected)
			 (1/4 button-delete-unselected)))))
	    (1/2 selected-query-result)))
	 (1/2 inspector))
       #+(and (not visco-demo-mode) mcl) ; NOT TESTED !
       (vertically ()
	 (1/2 
	  (horizontally ()
	    (1/2 (vertically ()
		   query-results
		   progress-bar
		   (horizontally ()
                     (6/10 button-abort)
                     (2/10 button-permutations)
                     (2/10 button-draw-thumbnails))
		   (horizontally ()
                     (3/10 button-previous)
                     (3/10 button-next)
                     (3/10 button-unselect-all)
                     (1/10 page-nr))
		   (horizontally ()
                     (1/4 button-delete-page)
                     (1/4 button-delete-all-pages)
                     (1/4 button-delete-selected)
                     (1/4 button-delete-unselected))))
	    (1/2 selected-query-result)))
	 (1/2 inspector))
       #+(and visco-demo-mode allegro)
       (horizontally ()
         (1/2 (labelling (:label "VISCO Query Results")	      
	      (vertically ()
		(15 progress-bar)
		(30 button-abort)	      
		(30 (horizontally ()
		      (1/2 button-previous)
		      (1/2 button-next)))
		(30 page-nr)
		query-results
		(30 (horizontally ()
		      (1/2 button-delete-page)
		      (1/2 button-delete-all-pages)))
		(30 (horizontally ()
		      (1/3 button-unselect-all)
		      (1/3 button-delete-selected)
		      (1/3 button-delete-unselected)))
		(30 (horizontally ()
		      (1/2 button-permutations)
		      (1/2 button-draw-thumbnails))))))
         (1/2 
	  (vertically ()
	    (1/2 selected-query-result)
	    (1/2 (outlining ()
		   (labelling (:label "VISCO Map Overview")
		     overview))))))
       #+(and visco-demo-mode mcl)
       (horizontally ()
         (1/2 (vertically ()                
		(outlining ()
		  (labelling (:label "VISCO Query Results")	      
		    (vertically ()
		      (15 progress-bar)
		      button-abort
		      (horizontally ()
			(1/2 button-previous)
			(1/2 button-next))
		      (15 page-nr))))
                (outlining () query-results)
                (outlining ()
		  (vertically ()
		    (horizontally ()
		      (1/2 button-delete-page)
		      (1/2 button-delete-all-pages))
		    (horizontally ()
		      (1/3 button-unselect-all)
		      (1/3 button-delete-selected)
		      (1/3 button-delete-unselected))
		    (horizontally ()
		      (1/2 button-permutations)
		      (1/2 button-draw-thumbnails))))))
         (1/2 
	  (vertically ()
	    (1/2 (outlining () selected-query-result))
	    (1/2 (outlining ()
		   (labelling (:label "VISCO Map Overview")
		     overview)))))))))
;;;
;;;
;;;

(defun get-current-query ()
  (with-visco-frame (visco)
    (and visco (current-query visco))))

(defun get-current-transparency ()
  (with-visco-frame (visco)
    (and visco (current-transparency visco))))

(defmethod set-current-transparency-to ((obj gui-transparency))
  (with-visco-frame (visco)
    (setf (current-transparency visco) obj)))

(defmethod set-current-transparency-to ((obj null))
  (with-visco-frame (visco)
    (setf (current-transparency visco) nil)))

(defun set-current-focus-to-newest-history-entry ()
  (with-visco-frame (visco)
    (let ((he (get-latest-history-entry)))
      (when he
	(setf (current-focus visco) he)))))

(defun current-focus-on-newest-history-entry-p ()
  (with-visco-frame (visco)
    (let ((he (get-latest-history-entry)))
      (eq (current-focus visco) he))))

(defmethod set-current-focus-to ((obj history-entry))
  (with-visco-frame (visco)
    (setf (current-focus visco) obj)))

(defmethod set-current-focus-to ((obj null))
  (with-visco-frame (visco)
    (setf (current-focus visco) nil)))

(defun get-latest-history-entry ()
  (with-visco-frame (visco)
    (first (last (remove-if-not #'(lambda (he)
				    (typep he 'visually-relevant-history-entry))
				(history visco))))))

;;;
;;;
;;;

(defmethod current-transparency-is-transparency-p ((transparency gui-transparency))
  (with-visco-frame (visco)
    (eq (current-transparency visco) transparency)))

(defun set-current-transparency-to-newest-transparency ()
  (with-visco-frame (visco)
    (setf (current-transparency visco)
      (find-if #'(lambda (obj)
		   (typep obj
			  'gui-transparency))
	       (visco-objects (current-query visco))
	       :from-end t))))	

(defun get-youngest-object (objects)
  (when objects 
    (find-named-visco-object 
     (apply #'min (mapcar #'name objects)))))

(defun get-youngest-object-older-than (objects object)
  (when objects 
    (dolist (obj objects)
      (unless (minusp (- (name obj) (name object)))
	(return obj)))))

(defmethod get-history-entry-before ((entry history-entry))
  (with-visco-frame (visco)
    (let ((pos (position entry (history visco))))
      (when (and pos (not (zerop pos)))
	(nth (1- pos) (history visco))))))

;;;
;;;
;;;

#+:allegro
(progn
  (define-gesture-name :create :pointer-button :left)
  (define-gesture-name :apply-operator :pointer-button (:control :left))
  (define-gesture-name :delete :pointer-button :middle)
  (define-gesture-name :move :pointer-button (:shift :left))
  (define-gesture-name :set-focus :pointer-button (:shift :middle))
  (define-gesture-name :set-transparency :pointer-button (:control :middle))
  (define-gesture-name :switch-button-off :pointer-button (:middle)))

#+:mcl
(progn
  (define-gesture-name :create :pointer-button :left)
  (define-gesture-name :apply-operator :pointer-button (:control :left))
  (define-gesture-name :delete :pointer-button (:meta :left))
  (define-gesture-name :move :pointer-button (:shift :left))
  ; (define-gesture-name :set-focus :pointer-button (:shift :middle))
  ; (define-gesture-name :set-transparency :pointer-button (:control :middle))
  (define-gesture-name :switch-button-off :pointer-button (:shift :left)))

;;;
;;;
;;;

(defvar *frame-standard-output* nil)

(defmethod frame-standard-input ((frame visco))
  (get-frame-pane frame 'command))

(defmethod frame-standard-output ((frame visco))
  (or *frame-standard-output*
      (get-frame-pane frame 'infos)))

(defmethod frame-error-output ((frame visco))
  (get-frame-pane frame 'infos))


;;;
;;;
;;;

#+:allegro
(define-presentation-translator my-testing-command->command-translator
    (command command visco :tester ((object frame) (command-enabled (first object) frame)))
  (object)
  object)

(defmethod run-frame-top-level :before ((frame visco-buttons) &key)
  (mapc #'release-dummy *dummies*)
  (initialize-object-buttons)
  (initialize-status-buttons)
  (initialize-operator-buttons))

#+:allegro
(defmethod run-frame-top-level :before ((frame visco) &key)
  (generate-name-for-undo-and-redo-command)
  (let ((translators (find-presentation-translators 'command 'command 'visco)))
    (delete (second translators) translators)))

(defmethod run-frame-top-level :before ((frame visco-inspector) &key)
  (lock-buttons)
  (deactivate-gadget +button-abort+))

;;;
;;;
;;;

(defun draw-display (frame stream)
  (let ((history (history frame)))
    (let* ((current-focus (current-focus frame))
	   (focus-visco-object 
	    (when (typep current-focus 'constructor-history-entry)
	      (object-to-draw current-focus))))
      (loop
	(let* ((history-entry (pop history)))
	  (if history-entry
	      (when (typep history-entry 'visually-relevant-history-entry)
		(let* ((obj (object-to-draw history-entry))
		       (focus-p
			(eq history-entry current-focus))
		       (current-transparency-p
			(and (typep obj 'gui-transparency)
			     (current-transparency-is-transparency-p obj))))
		  (if (and focus-visco-object
			   (not focus-p)
			   (typep focus-visco-object 'constraints-mixin)
			   (typep obj 'constraints-mixin))
		      (let ((constraint
			     (dolist (cs-type *visualized-relations* nil)
			       (when (=> (or (eq cs-type 'intersects)
					     (eq cs-type 'disjoint))
					 (and (=> (typep focus-visco-object '(or point line))
						  (not (ignore-disjoint-and-intersects-relations-p focus-visco-object)))
					      (=> (typep obj '(or point line))
						  (not (ignore-disjoint-and-intersects-relations-p obj)))))
				 (let ((look-for (case cs-type
						   (inside/contains '(inside contains))
						   (otherwise (list cs-type)))))
				   (when (dolist (cs-type look-for nil)
					   (when (find-constraint cs-type focus-visco-object obj)
					     (return t)))
				     (return cs-type)))))))
			(updating-output (stream :unique-id history-entry
						 :cache-value (list (tick obj)
								    focus-p
								    current-transparency-p
								    constraint)
						 :cache-test #'equal)
			  (if constraint
			      (draw obj stream
				    :border-for constraint
				    :subobjects-draw-only-gravity-field t)
			    (draw obj stream 
				  :subobjects-draw-only-gravity-field t))))
		    (updating-output (stream :unique-id history-entry
					     :cache-value (list (tick obj)
								focus-p
								current-transparency-p)
					     :cache-test #'equal)
		      (draw obj stream 
			    :subobjects-draw-only-gravity-field t
			    :border-for (when (and focus-p
						   *focus-box*)
					  'focus))))
		  (setf (already-drawn obj) t)
		  (when focus-p
		    (return))))
	    (return))))
      (dolist (he (history frame))
	(when (typep he 'visually-relevant-history-entry)
	  (setf (already-drawn (object-to-draw he)) nil))))))

;;;
;;;
;;;

(defun redraw-buttons ()
  (with-visco-buttons-frame (buttons)    
    (redisplay-frame-pane buttons (get-frame-pane buttons 'object-buttons))    
    (update-operator-button)))

;;;
;;;
;;;

(defvar *visco-process* nil)
(defvar *visco-buttons-process* nil)
(defvar *visco-inspector-process* nil)


(defun kill-visco-processes ()
  (progn
    (when *visco-process*
      (#+:allegro mp:process-kill #+:mcl ccl:process-kill *visco-process*))
    (when *visco-inspector-process*
      (#+:allegro mp:process-kill #+:mcl ccl:process-kill *visco-inspector-process*))
    (when *visco-buttons-process*
      (#+:allegro mp:process-kill #+:mcl ccl:process-kill *visco-buttons-process*))))

(defun visco ()
  (let ((port (find-port)))
    #+:allegro
    (setf (clim:text-style-mapping port 
				   +visco-text-style+)
      "-*-lucida-medium-r-normal-*-12-*-*-*-*-*-*-*")
    
    (multiple-value-bind (width height)
	(bounding-rectangle-size 
	 (sheet-region (find-graft :port port)))
      
      
      
      (kill-visco-processes)
      
      (setf *visco-process* 
	(#+:allegro mp:process-run-function
		    #+:mcl ccl:process-run-function
		    "Visco"
		    #'(lambda ()
			(setf *visco-frame*
			  (make-application-frame
			   'visco
			   :pretty-name "VISCO"
			   :left 10
			   :top 20
			   :width (- width 230)
			   :height (- height 30)))

			(run-frame-top-level *visco-frame*))))  
      
      (setf *visco-inspector-process* 
	(#+:allegro mp:process-run-function
		    #+:mcl ccl:process-run-function
		    "Visco Inspector"
		    #'(lambda ()
			#+(and (not visco-demo-mode) allegro)
			(setf *visco-inspector-frame*
			  (make-application-frame
			   'visco-inspector
			   :pretty-name "VISCO Inspector"
			   :left 10
			   :top 10
			   :width (- width 250)
			   :height 820))
			
			#+(and visco-demo-mode allegro)
			(setf *visco-inspector-frame*
			  (make-application-frame
			   'visco-inspector
			   :width width
			   :height height
			   :pretty-name "VISCO Inspector"))
			
			#+(and visco-demo-mode mcl)
			(setf *visco-inspector-frame*
			  (make-application-frame
			   'visco-inspector
			   :left 10
			   :top 20
			   :width (- width 20)
			   :height (- height 30)
			   :pretty-name "VISCO Inspector"))

			(run-frame-top-level *visco-inspector-frame*))))
      
      (setf *visco-buttons-process*
	(#+:allegro mp:process-run-function
		    #+:mcl ccl:process-run-function
		    "Visco Buttons"
		    #'(lambda ()
			
			(setf *visco-buttons-frame*
			  (make-application-frame
			   'visco-buttons
			   :pretty-name "VISCO Buttons"
			   :left (- width 210)
			   :top 20
			   :width 200
			   :height (- height 30)))
			(run-frame-top-level *visco-buttons-frame*)))))))

