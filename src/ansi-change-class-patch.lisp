; -*- Mode: Lisp; Package: CCL; -*-

;; change class takes initargs per ANSI CL
;; this patch incorporates a previous change-class patch
;; 11/19/97 akh

(in-package :ccl)

(let ((*warn-if-redefine-kernel* nil)
      (*warn-if-redefine* nil)
      (*defmethod-congruency-override* t))

#+ppc-target
(defun %change-class (object new-class initargs)
  ; The spec doesn't say whether to run update-instance-for-different-class
  ; if the class isn't really different.  I choose not to.  So there. - HMM
  (unless (and (eq (class-of object) new-class) (null initargs))
    ; Try to prevent this from happenning inside of without-interrupts
    (maybe-update-obsolete-instance object)
    ; uncomment this as soon as it works.
    ;; should really check initargs before changing the class
    ;; update-instance-... will do this too 
    (when initargs (check-initargs t new-class initargs t #'update-instance-for-different-class #'shared-initialize))
    (with-managed-allocation
      (let (copy
            (instance (%maybe-gf-instance object)))
        (without-interrupts
         (maybe-update-obsolete-instance object)   ; probably a nop, but you never know.
         (let* ((forwarded-instance instance)
                (new-wrapper (or (%class-own-wrapper new-class)
                                 (initialize-class-and-wrapper new-class)))
                (new-slots (%wrapper-instance-slots new-wrapper))
                old-slots
                (old-size (%i- (uvsize instance) 2))
                (new-size (uvsize new-slots))
                (eql-methods (let ((eql-specializer (list 'eql object)))
                               (declare (dynamic-extent eql-specializer))
                               (specializer-direct-methods eql-specializer))))
           ; Remove any combined-methods for the old class
           (mapc #'remove-obsoleted-combined-methods eql-methods)
           ; If it wasn't forwarded, need to copy.  This really should be stack-allocated.
           (when (eq instance (setq copy (%maybe-forwarded-instance instance)))
             (let ((size (%i+ old-size 2)))
               (setq copy (%make-temp-uvector size 
                                              #-ppc-target $v_instance
                                              #+ppc-target ppc::subtag-instance))
               (dotimes (i size)
	         (declare (fixnum i))
                 (setf (%svref copy i) (%svref instance i)))
               (setf (%forwarded-instance copy) copy)))
           (setq old-slots (%wrapper-instance-slots (%instance-class-wrapper copy)))
           ; If the new class won't fit, need to forward instance.
           (if (%i> new-size old-size)
             (progn
               (when (typep object 'std-class)
                 (error "Implementation restriction.~%Can't add slots to an instance of ~s" 'std-class))
               (setq forwarded-instance (%allocate-instance new-class))
               (%forward-instance instance forwarded-instance))
             (progn
               (setf (%instance-class-wrapper instance) new-wrapper
                     (%forwarded-instance instance) instance)
               ; Make all instance's slots unbound
               (let ((size (%i+ new-size 2)))
                 (do ((i 2 (%i+ i 1)))
                     ((%i>= i size))
                   (setf (%svref instance i) (%unbound-marker-8))))))
           ; Copy the shared slots
           (dotimes (i new-size)
	     (declare (fixnum i))
             (let* ((slot-name (%svref new-slots i))
                    (old-index (position slot-name old-slots :test 'eq)))
               (when old-index
                 (setf (%svref forwarded-instance (%i+ i 2))
                       (%svref copy (%i+ old-index 2))))))
           ; Remove any combined-methods for the new class.
           (mapc #'remove-obsoleted-combined-methods eql-methods)))
        ; And let the user & shared-initialize do what they will
        (if (functionp object)          ; was a generic-function
          (let ((temp-gf *temp-gf*))
            (setq *temp-gf* nil)
            (unwind-protect
              (progn
                (let ((gf (or temp-gf (make-gf nil 0))))
                  (setf (%gf-instance gf) copy)
                  (apply #'update-instance-for-different-class gf object initargs)))
              (if temp-gf (setq *temp-gf* temp-gf))))
          (apply #'update-instance-for-different-class copy object initargs)))))
  object)

#-ppc-target
(defun %change-class (object new-class initargs)
  ; The spec doesn't say whether to run update-instance-for-different-class
  ; if the class isn't really different.  I choose not to.  So there.
  (unless (and (eq (class-of object) new-class) (null initargs))
    ; Try to prevent this from happenning inside of without-interrupts
    (maybe-update-obsolete-instance object)
    (when initargs (check-initargs t new-class initargs t #'update-instance-for-different-class #'shared-initialize))
    ; uncomment this as soon as it works.
   (with-managed-allocation
      (let (copy
            (instance (%maybe-gf-instance object)))
        (without-interrupts
         (maybe-update-obsolete-instance object)   ; probably a nop, but you never know.
         (let* ((forwarded-instance instance)
                (new-wrapper (or (%class-own-wrapper new-class)
                                 (initialize-class-and-wrapper new-class)))
                (new-slots (%wrapper-instance-slots new-wrapper))
                old-slots
                (old-size (%i- (uvsize instance) 1))
                (new-size (uvsize new-slots))
                (eql-methods (let ((eql-specializer (list 'eql object)))
                               (declare (dynamic-extent eql-specializer))
                               (specializer-direct-methods eql-specializer))))
           ; Remove any combined-methods for the old class
           (mapc #'remove-obsoleted-combined-methods eql-methods)
           ; If it wasn't forwarded, need to copy.  This really should be stack-allocated.
           (when (eq instance (setq copy (%maybe-forwarded-instance instance)))
             (let ((size (%i+ old-size 1)))
               (setq copy (%make-temp-uvector size $v_instance))
               (dotimes (i size)
	         (declare (fixnum i))
                 (setf (%svref copy i) (%svref instance i)))))
           (setq old-slots (%wrapper-instance-slots (%instance-class-wrapper copy)))
           ; If the new class won't fit, need to forward instance.
           (if (%i> new-size old-size)
             (progn
               (when (typep object 'std-class)
                 (error "Implementation restriction.~%Can't add slots to an instance of ~s" 'std-class))
               (setq forwarded-instance (%allocate-instance new-class))
               (%forward-instance instance forwarded-instance))
             (progn
               (setf (%instance-class-wrapper instance) new-wrapper)
               ; Make all instance's slots unbound
               (do ((i 1 (%i+ i 1)))
                   ((%i> i new-size))
                 (setf (%svref instance i) (%unbound-marker-8)))))
           ; Copy the shared slots
           (dotimes (i new-size)
	     (declare (fixnum i))
             (let* ((slot-name (%svref new-slots i))
                    (old-index (position slot-name old-slots :test 'eq)))
               (when old-index
                 (setf (%svref forwarded-instance (%i+ i 1))
                       (%svref copy (%i+ old-index 1))))))
           ; Remove any combined-methods for the new class.
           (mapc #'remove-obsoleted-combined-methods eql-methods)))
        ; And let the user & shared-initialize do what they will
        (if (functionp object)          ; was a generic-function
          (let ((temp-gf *temp-gf*))
            (setq *temp-gf* nil)
            (unwind-protect
              (progn
                (let ((gf (or temp-gf (make-gf nil 0))))
                  (setf (%gf-instance gf) copy)
                  (apply #'update-instance-for-different-class gf object initargs)))
              (if temp-gf (setq *temp-gf* temp-gf))))
          (apply #'update-instance-for-different-class copy object initargs)))))
  object)



(defmethod change-class ((from structure-class) (to class) &rest initargs &key)
  (declare (dynamic-extent initargs))
  (declare (ignore initargs))
  (let ((class-name (class-name from)))
    (unless (eq from to)                  ; shouldn't be
      (remove-structure-defs class-name)
      (remhash class-name %defstructs%)))
  (call-next-method))

(defmethod change-class (instance (new-class symbol) &rest initargs &key)
  (declare (dynamic-extent initargs))  
  (apply #'change-class instance (find-class new-class) initargs))

(defmethod change-class ((instance standard-object) (new-class standard-class) &rest initargs &key)
  (declare (dynamic-extent initargs))
  (%change-class instance new-class initargs))

(defmethod change-class ((instance standard-object) (new-class funcallable-standard-class) &rest initargs &key)
  (declare (dynamic-extent initargs))
  (%change-class instance new-class initargs))

(defmethod change-class ((instance standard-generic-function) (new-class standard-class) &rest initargs &key)
  (declare (dynamic-extent initargs))
  (unless (inherits-from-standard-generic-function-p new-class)
    (%badarg new-class 'standard-generic-function))
  (unless (or (%gf-instance instance) 
              (eq new-class *standard-generic-function-class*))
    (let ((i (allocate-instance new-class)))
      (shared-initialize i t)
      (setf (%gf-instance instance) i)))
  (%change-class instance new-class initargs))


)

(setq *clos-initialization-functions* (append *clos-initialization-functions* (list #'change-class)))

#|
(defclass foo () ((a :initarg :a)))
(defclass fie (foo)((b :initarg :b)))
(setq f (make-instance 'foo))
(change-class f 'fie :b 4)
|#
