;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: GUI; Base: 10 -*-

(in-package gui)

(defconstant +button-text-style+
    (parse-text-style '(:sans-serif nil :large)))

(defconstant +on-ink+
    +yellow+)

(defconstant +grayed-on-ink+
    (make-grid-in +yellow+ +background-ink+))

(defconstant +activated-ink+
    +green+)

(defconstant +grayed-activated-ink+
    (make-grid-in +green+ +background-ink+))

(defconstant +not-activated-ink+
    (make-gray-color 0.8))

;;;
;;;
;;;

(defclass button (gui-object)		; abstrakt
  ((on :accessor on :initform nil :initarg :on)
   (deactivated :accessor deactivated :initform nil :initarg :deactivated) 
   (id :accessor id :initform nil :initarg :id)
   (tester :accessor tester :initform #'yes :initarg :tester)
   (valid :accessor valid :initform #'yes :initarg :valid)
   (when-selected :accessor when-selected :initform #'toggle :initarg :when-selected)
   (when-action-begins :accessor when-action-begins :initform #'yes :initarg :when-action-begins)
   (when-action-ends :accessor when-action-ends :initform #'yes :initarg :when-action-ends)
   (initialize :accessor initialize :initform #'yes :initarg :initialize)
   
   (doc :accessor doc :initform "No documentation available" :initarg :doc)))

(defclass icon-button (button)		; abstrakt
  ((icon :accessor icon :initarg :icon)
   (scale-fac :accessor scale-fac :initarg :scale-fac :initform 0.8)))

(defclass text-button (button)		; abstrakt
  ((text :accessor text :initarg :text)))

;;;
;;;
;;;

(defun get-button-for-icon (icon &optional (buttons *buttons*))
  (find icon buttons :key #'(lambda (obj)
			      (when (typep obj 'icon-button)
				(icon obj)))))

(defmethod initialize-instance :after ((obj button) &rest initargs)
  (declare (ignore initargs))
  (push obj *buttons*)
  (funcall (initialize obj) obj))

(defmethod reinitialize ((obj button))
  (funcall (initialize obj) obj))

;;;
;;;
;;;

(defmethod inform-button-action-ends ((button button))
  (funcall (when-action-ends button) button))

(defmethod inform-button-action-begins ((button button))
  (funcall (when-action-begins button) button))

(defmethod deactivate ((button button))
  (setf (deactivated button) t))

(defmethod deactivate ((buttons list))
  (dolist (button buttons)
    (deactivate button)))

(defmethod activate ((button button))
  (setf (deactivated button) nil))

(defmethod activate ((buttons list))
  (dolist (button buttons)
    (activate button)))

(defmethod toggle ((button button))
  (setf (on button)
    (not (on button))))

(defmethod switch-on ((button button))
  (setf (on button) t))

(defmethod switch-off ((button button))
  (setf (on button) nil))

(defmethod switch-off ((buttons list))
  (dolist (button buttons)
    (switch-off button)))

(defmethod switch-on ((buttons list))
  (dolist (button buttons)
    (switch-on button)))

(defmethod toggle ((buttons list))
  (dolist (button buttons)
    (toggle button)))

;;;
;;;
;;;

(defmethod valid-p ((button button))
  (funcall (valid button) button))

;;;
;;;
;;;

(defun get-button-state ()
  (list *primary-mode*
	*segment-mode*
	*point-mode*
	
	*point-status*
	*segment-status*
	*chain-or-polygon-status*
	
	(mapcar #'deactivated *buttons*)
	(mapcar #'on *buttons*)))

(defun install-button-state (state)
  (setf *primary-mode* (first state)
	*segment-mode* (second state)
	*point-mode* (third state)
	
	*point-status* (fourth state)
	*segment-status* (fifth state)
	*chain-or-polygon-status* (sixth state))
  
  (mapc #'(lambda (flag button)
	    (if flag
		(deactivate button)
	      (activate button)))
	(seventh state) *buttons*)
  (mapc #'(lambda (flag button)
	    (setf (on button) flag))
	(eighth state) *buttons*)
  (redraw-buttons))

;;;
;;;
;;;



(defmethod draw-button :before ((button button) stream cell-size)
  (let* ((half-cell-size (/ cell-size 2)))
    (draw-rectangle* stream 
		     (- half-cell-size)
		     (- half-cell-size)
		     half-cell-size
		     half-cell-size
		     :filled t
		     :ink
		     (let ((active (funcall (tester button) button))
			   (deactivated (deactivated button))
			   (on (on button)))
		       (if (not deactivated)
			   (if on
			       (if active 
				   +on-ink+
				 +grayed-on-ink+)
			     (if active 
				 +activated-ink+
			       +not-activated-ink+))
			 (if on
			     +grayed-on-ink+
			   (if active 
			       +grayed-activated-ink+
			     +not-activated-ink+)))))
    (draw-rectangle* stream 
		     (- half-cell-size)
		     (- half-cell-size)
		     half-cell-size
		     half-cell-size
		     :filled nil)))

(defmethod draw-button ((button button) stream cell-size)
  (declare (ignore stream cell-size))
  nil)

(defmethod underlabel-button ((button button) stream cell-size)
  (declare (ignore stream cell-size))
  nil)

(defmethod output-draw-button ((button button) stream cell-size)
  (declare (ignore stream cell-size))
  nil)


(defun accept-buttons (buttons rows columns frame stream &key width height
							      (x-spacing 4)
							      (y-spacing 4))
  (declare (ignore frame))
  (#+:allegro excl:without-interrupts
   #+:mcl ccl:without-interrupts
    (let* ((cell-size 50)
	   (x1 nil)
	   (y1 nil)
	   (x2 nil)
	   (y2 nil))    
      (labels ((draw-it (button sx sy)
		 (let* ((or
			 (with-output-to-output-record (stream)
			   (output-draw-button button stream cell-size)))
			(width  (bounding-rectangle-width or))
			(height (bounding-rectangle-height or)))
		   
		   (with-scaling (stream (* sx (/ cell-size width))
					 (* sy (/ cell-size height)))
		     (output-draw-button button stream cell-size)
		     (underlabel-button button stream cell-size)))))
	
	(multiple-value-bind (xc yc)
	    (stream-cursor-position stream)
	  
	  (multiple-value-bind (x y)
	      (multiple-value-bind (a b)
		  (window-inside-size stream)
		(values (or width (- a xc))
			(or height (- b yc))))
	    (let ((sx (/ x (+ (* columns cell-size)
			      (* columns x-spacing))))
		  (sy (/ y (+ (* rows cell-size)
			      (* rows y-spacing))))
		  (xcoord nil)
		  (ycoord nil))
	      
	      (dotimes (i rows)      
		(dotimes (j columns)
		  (setf xcoord (+ xc (* x-spacing j) (* cell-size sx j))
			ycoord (+ yc (* y-spacing i) (* cell-size sy i)))
		  (let* ((button (pop buttons)))
		    (stream-set-cursor-position stream xcoord ycoord)
		    (with-room-for-graphics (stream)
		      (declare (ignore stream))
		      (draw-it button sx sy)))
		  (when (and (= (1+ i) rows) (zerop j))
		    (setf x1 xcoord
			  y1 (+ ycoord (* cell-size sy))))
		  (when (and (zerop i) (= (1+ j) columns))
		    (setf x2 (+ xcoord (* cell-size sx))
			  y2 ycoord))))

	      (values x1 y1
		      x2 y2))))))))

;;;
;;;
;;;

(define-visco-buttons-command (com-select-mode :name nil)
    ((object 'button))  
  #| (excl:without-interrupts  |#
  (funcall (when-selected object)
	   object))

(define-presentation-to-command-translator select-mode
    (button com-select-mode visco-buttons
	    :gesture :select 
	    :tester ((object) (and (not (deactivated object))
				   (funcall (tester object) object)))
	    :echo nil :maintain-history nil
	    :documentation ((stream object) 					     
			    (let ((doc (doc object)))
			      (etypecase doc 
				(string (princ doc stream))
				(list (princ (first doc) stream))))))
  (object)
  (list object))

