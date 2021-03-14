;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: GUI; Base: 10 -*-

(in-package gui)

#+:allegro
(define-visco-command (com-print-query :name "Print Query")
    ()
  (with-open-stream 
      (pipe (excl:run-shell-command  (format nil "lpr -P~A -h" '|r131_hp|)
				     :input :stream :wait nil))
    (with-output-to-postscript-stream (stream pipe 
					      :orientation :landscape
					      :scale-to-fit t)
      (dolist (obj (visco-objects (get-current-query)))
	(draw obj stream)))))

#+:allegro
(defmethod command-enabled ((obj (eql 'com-print-query)) (frame visco))
  (with-visco-frame (visco)
    (and visco		     
	 (visco-objects (current-query visco)))))

;;;
;;;
;;;

(define-visco-command (com-save-query :name nil)
    nil
  (with-visco-frame (visco)
    (let ((file (file-selector "Save Query" "visco:queries;" "vis" :save t)))
      (when file
	(with-slots (history undo-stack current-focus 
		     current-query current-transparency) visco
	  (make-object-persistent (list history undo-stack current-focus
                                        current-query current-transparency
                                        (get-button-state))
                                  file))))))

(defmethod command-enabled ((obj (eql 'com-save-query)) (frame visco)
                            #+:mcl &optional #+:mcl unused)
  #+:mcl (declare (ignore unused))
  (with-visco-frame (visco)
    (and visco		     
	 (visco-objects (current-query visco)))))

(define-visco-command (com-load-query :name nil)
    nil
  (with-visco-frame (visco)
    (let ((file (file-selector "Load Query" "visco:queries;" "vis")))
      (when file
	(with-slots (history undo-stack current-focus 
		     current-query current-transparency) visco
	  (let ((result
		 (load-persistent-object file)))
	    (setf history (first result)
		  undo-stack (second result)
		  current-focus (third result)
		  current-query (fourth result)
		  current-transparency (fifth result))
	    (install-button-state (sixth result))
	    (refresh)))))))


(define-visco-command (com-quit :name "Quit")
    ()
  (with-visco-frame (visco)
    (let ((yes-or-no (notify-user visco
				  "Quit selected! Are you sure?"
				  :style :question)))
      (when yes-or-no
	(kill-visco-processes)))))

;;;
;;;
;;;


(define-visco-command (com-execute-query :name "Execute Query")
    ()
  (with-visco-inspector-frame (inspector)
    (handler-case 	  
      #+visco-demo-mode
      (let (#+:MCL (ccl::*suppress-compiler-warnings* t))
        (compile-query (get-current-query) 
                       :infos nil
                       :debug nil
                       :check-for-abort t))
      #-visco-demo-mode
      (let* ((*standard-output* 
              (get-frame-pane inspector 'inspector))             
             (*error-output* *standard-output*)
             (*print-case* :downcase))
        (window-clear *standard-output*)
        (with-output-as-presentation (*standard-output* nil 'null)
          (compile-query (get-current-query) 
                         :debug nil
                         :check-for-abort t)))
      (error (error)
	     (format t "~%*** ERROR ***~%Can't Compile: Bad Query!~%")
	     (error error) ; remove this in the final version !!!
	     (beep))
      (:no-error (res)
	         (declare (ignore res))
	         (make-new-page)
	         (setf (all-matches inspector) nil)
	         (lock-buttons)
	         (activate-gadget +button-abort+)
	         (execute-query (get-current-query))
	         (deactivate-gadget +button-abort+)
	         (unlock-buttons)))))


(defmethod command-enabled ((obj (eql 'com-execute-query)) (frame visco)
                            #+:mcl &optional #+:mcl unused)
  #+:mcl (declare (ignore unused))
  (with-visco-frame (visco)
    (and visco
	 (visco-objects (current-query visco))
	 (get-current-db))))

;;;
;;;
;;;

(define-visco-command (com-get-query-semantics :name "Get Query Semantics")
    ()
  (with-visco-inspector-frame (inspector)
    (handler-case
      #+visco-demo-mode 
      (pprint (prog1 (get-query-semantics (get-current-query))
                (terpri) (terpri)))
      #-visco-demo-mode
      (let* ((*standard-output* 
              (get-frame-pane inspector 'inspector))
             (*error-output* *standard-output*)
             (*print-case* :downcase)
             (*package* (find-package 'query-compiler))
             (*print-case* :downcase))
        (window-clear *standard-output*)
        (with-output-as-presentation (*standard-output* nil 'null)
          (pprint (prog1 (get-query-semantics (get-current-query))
                    (terpri) (terpri)))))
      (error ()
	     (format t "~%*** ERROR ***~%No Semantics: Bad Query!~%")
	     (beep)))))


;;;
;;;
;;;


(define-command-table file-table
    :menu (("Load Query" :command (com-load-query))
	   ("Save Query" :command (com-save-query))
	   #+:allegro ("divide1" :divider nil)
	   #+:allegro ("Print Query" :command (com-print-query))
	   ("divide2" :divider nil)
	   ("Quit" :command (com-quit))))

(define-command-table query-table
    :menu (("Execute Query" :command (com-execute-query))
	   ("Get Query Semantics" :command (com-get-query-semantics))))

;;;
;;;
;;;


(define-visco-command (refresh :name "Refresh")
    ()
  (with-visco-frame (visco)
    (set-object-modes)
    (redisplay-frame-pane visco 'display :force-p t)))

(defun ready ()
  (show-search-progress 1 :force-p t)
  (make-query-result))

