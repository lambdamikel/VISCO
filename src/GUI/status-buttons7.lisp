;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: GUI; Base: 10 -*-

(in-package gui)

(defvar *last-pushed-button* nil)

(defclass status-button (text-button)
  ())

;;;
;;;
;;;

(defvar +legal-combinations+ nil)

(defmethod initialize-instance :after ((obj status-button) &rest initargs)
  (declare (ignore initargs))
  (push obj *status-buttons*))

(defun initialize-status-buttons ()
  (switch-off *status-buttons*)  
  (activate *status-buttons*)
  (setf *point-status* nil
	*segment-status* nil
	*chain-or-polygon-status* nil))

(defun make-status-button (text &rest args)
  (apply #'make-instance 'status-button :text text args))

;;;
;;;
;;;

(defun point-status-selected (button)
  (switch-on button)
  (setf *point-status* (id button))
  (select-legal-combination button))

(defun segment-status-selected (button)
  (switch-on button)
  (setf *segment-status* (id button))
  (select-legal-combination button))

(defun chain-or-polygon-status-selected (button)  
  (switch-on button)
  (setf *chain-or-polygon-status* (id button))
  (select-legal-combination button))

;;;
;;;
;;;

(defun point-status-tester (button)
  (let ((res
	 (let ((*point-status*
		(id button)))
	   (find-next-combination button))))
    (and res
	 (not (equal (list *point-status*
			   *segment-status*
			   *chain-or-polygon-status*)
		     res)))))


(defun segment-status-tester (button)
  (let ((res
	 (let ((*segment-status*
		(id button)))
	   (find-next-combination button))))
    (and res
	 (not (equal (list *point-status*
			   *segment-status*
			   *chain-or-polygon-status*)
		     res)))))

(defun chain-or-polygon-status-tester (button)
  (let ((res
	 (let ((*chain-or-polygon-status*
		(id button)))
	   (find-next-combination button))))
    (and res
	 (not (equal (list *point-status*
			   *segment-status*
			   *chain-or-polygon-status*)
		     res)))))

;;;
;;;
;;;

(defun smaller-p (a b)
  (case b
    (universe t)
    (db (eq a 'db-component))
    (db-component (eq a 'db-component))))

(defun legal-combination-p ()
  (and (=> *segment-status*
	   (=> *point-status* (smaller-p *point-status* *segment-status*)))
       (=> *chain-or-polygon-status*
	   (and (=> *point-status* 
		    (smaller-p *point-status* *chain-or-polygon-status*))
		(=> *segment-status* 
		    (smaller-p *segment-status* *chain-or-polygon-status*))))))

(defun select-legal-combination (button)
  (let ((same-button-pushed-twice
	 (eq *last-pushed-button* button)))
    (if (and (not same-button-pushed-twice)
	     (legal-combination-p))
	(select-current-combination)
      (select-combination
       (find-next-combination button
			      :keep-as-much-as-possible-p 
			      (not same-button-pushed-twice))))
    (setf *last-pushed-button* button)))


(defun select-current-combination ()
  (select-combination (list *point-status*
			    *segment-status*
			    *chain-or-polygon-status*)))

(defun select-combination (comb)
  (dolist (button *status-buttons*)
    (switch-off button))
  (setf *point-status* (first comb)
	*segment-status* (second comb)
	*chain-or-polygon-status* (third comb))
  
  (dolist (button (list *button-origin* *button-nail* *button-marble*))
    (setf (status (icon button)) *point-status*))
  
  (dolist (button (list *button-rubberband* *button-beam*
			*button-atomic-rubberband* *button-atomic-<=-rubberband*
			*button-atomic->=-rubberband*))
    (setf (status (icon button)) *segment-status*))
  
  (dolist (button (list *button-chain* *button-polygon*))
    (setf (status (icon button)) *chain-or-polygon-status*))
  
  (mapc #'(lambda (status-var buttons)
	    (when status-var
	      (dolist (button buttons)
		(when (eq (id button) status-var)
		  (switch-on button)))))
	(list *point-status*
	      *segment-status*
	      *chain-or-polygon-status*)
	(list *point-buttons*
	      *segment-buttons*
	      *chain-or-polygon-buttons*))
  (set-object-modes))

(defun find-next-combination (button &key keep-as-much-as-possible-p)
  (let* ((res 	  
	  (loop as comb in +legal-combinations+
	      when (cond ((member button *point-buttons*)
			  (eq (first comb)
			      *point-status*))
			 ((member button *segment-buttons*)
			  (eq (second comb)
			      *segment-status*))
			 ((member button *chain-or-polygon-buttons*)
			  (eq (third comb)
			      *chain-or-polygon-status*)))
	      collect comb))
	 (pos (position (list *point-status*
			      *segment-status*
			      *chain-or-polygon-status*)
			res 
			:test #'equal))
	 (res2 (if pos 
		   (if keep-as-much-as-possible-p
		       (list (nth pos res))
		     (circle-subseq res (1+ pos) (+ 3 pos)))
		 res)))
    (values (first res2)
	    (rest res2))))

;;;
;;;
;;;

(defparameter *button-point-status-db* 
    (make-status-button "DB"
			:id 'db
			:tester #'point-status-tester
			:when-selected #'point-status-selected
			:doc "Primary DB Point"))


(defparameter *button-segment-status-db* 
    (make-status-button "DB"
			:id 'db							     
			:tester #'segment-status-tester
			:when-selected #'segment-status-selected
			:doc "Primary DB Segment"))


(defparameter *button-chain-or-polygon-status-db*
    (make-status-button "DB"
			:id 'db
			:tester #'chain-or-polygon-status-tester
			:when-selected #'chain-or-polygon-status-selected
			:doc "Primary DB Chain / Polygon"))


(defparameter *button-point-status-db-component* 
    (make-status-button "DB-C"
			:id 'db-component
			:tester #'point-status-tester
			:when-selected #'point-status-selected
			:doc "DB Point"))

(defparameter *button-segment-status-db-component* 
    (make-status-button "DB-C"
			:id 'db-component
			:tester #'segment-status-tester
			:when-selected #'segment-status-selected
			:doc "DB Segment"))


(defparameter *button-chain-or-polygon-status-db-component* 
    (make-status-button "DB-C"
			:id 'db-component
			:tester #'chain-or-polygon-status-tester
			:when-selected #'chain-or-polygon-status-selected
			:doc "DB Chain / Polygon"))


(defparameter *button-point-status-universe* 
    (make-status-button "U"
			:id 'universe
			:tester #'point-status-tester
			:when-selected #'point-status-selected
			:doc "UNIVERSE Points"))


(defparameter *button-segment-status-universe* 
    (make-status-button "U"
			:id 'universe
			:tester #'segment-status-tester
			:when-selected #'segment-status-selected
			:doc "UNIVERSE Segment"))


(defparameter *button-chain-or-polygon-status-universe* 
    (make-status-button "U"
			:id 'universe
			:tester #'chain-or-polygon-status-tester
			:when-selected #'chain-or-polygon-status-selected
			:doc "UNIVERSE Chain / Polygon"))


;;;
;;;
;;;


(defparameter *point-buttons* (list *button-point-status-db*
				    *button-point-status-db-component*
				    *button-point-status-universe*))

(defparameter *segment-buttons* (list *button-segment-status-db*
				      *button-segment-status-db-component*
				      *button-segment-status-universe*))

(defparameter *chain-or-polygon-buttons* (list *button-chain-or-polygon-status-db*
					       *button-chain-or-polygon-status-db-component*
					       *button-chain-or-polygon-status-universe*))


;;;
;;;
;;;


(defmethod output-draw-button ((button status-button) stream cell-size)
  (updating-output (stream 
		    :cache-value (list (on button)
				       (deactivated button)
				       (funcall (tester button) button))
		    :cache-test #'equal)
    (with-output-as-presentation
	(stream button 'status-button
		:single-box t
		:allow-sensitive-inferiors nil)
      (draw-button button stream cell-size))))


(defmethod draw-button ((button status-button) stream cell-size)
  (declare (ignore cell-size))
  (draw-text* stream
	      (text button)
	      -20 -10
	      :text-style +button-text-style+))

;;;
;;;
;;;

(define-visco-buttons-command (com-switch-off :name nil)
    ((object 'status-button))  
  (when (on object)
    (cond ((member object *point-buttons*)
	   (setf *point-status* nil))
	  ((member object *segment-buttons*)
	   (setf *segment-status* nil))
	  ((member object *chain-or-polygon-buttons*)
	   (setf *chain-or-polygon-status* nil)))
    (setf *last-pushed-button* nil)
    (if (legal-combination-p)
	(select-current-combination)
      (select-legal-combination object))))

(define-presentation-to-command-translator switch-off
    (status-button com-switch-off visco-buttons
		   :gesture :switch-button-off
		   :tester ((object) (on object))
		   :echo nil :maintain-history nil)
  (object)
  (list object))

;;;
;;;
;;;

(let ((status-types '(db db-component universe nil)))
  (dolist (*point-status* status-types)
    (dolist (*segment-status* status-types)
      (dolist (*chain-or-polygon-status* status-types)
	(when (legal-combination-p)
	  (push (list *point-status*
		      *segment-status*
		      *chain-or-polygon-status*)
		
		+legal-combinations+))))))
