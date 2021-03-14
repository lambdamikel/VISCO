;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: CL-USER -*-

(cl:in-package :cl-user)

;;; ----------------------------------------------------------------------
;;;
;;; Portable system declaration expanding into vendor-specific versions.
;;;
;;; DEFINE System = DEFsystem Is Now Expanded System
;;;
;;; ----------------------------------------------------------------------

;;; Make load-system and compile-system available in cl-user.
;;; Both systems accept a system-name (symbol or string) as a parameter.
;;; Please notice that in some native defsystems, symbols used for
;;; system names might be package-sensitive.
;;; Therefore it is recommended that cl-user is used for defsystem definitions.

#+:Genera
(import '(scl:load-system scl:compile-system) 'fcl-user)

(defun load-system-definition (system-name)
  (let ((name (if (symbolp system-name)
		  (string-downcase (symbol-name system-name))
		(string-downcase system-name))))
    (load-logical-pathname-translations name)
    (load (concatenate 'string
	    name
	    ":system-declarations;"
	    name
	    "-sysdcl.lisp"))))  

#+(and :aclpc :cl-http)
(defvar *logical-translations-directory*
   (merge-pathnames "Translations\\"
      allegro::*application-directory*))
      
#+(and :aclpc :cl-http)
(defun LOAD-LOGICAL-PATHNAME-TRANSLATIONS (host)
  "Loads the logical pathname translations for host named HOST if the logical 
   pathname translations are not already defined. First checks for a file
   with the same name as the host (lowercase) and type \"translations\" in
   the current directory, then the translations directory. If it finds such
   a file it loads it and returns T, otherwise it signals an error."
  (let* ((trans-fname (concatenate 'string (string-downcase host)
				   ".translations"))
	 (pathname (when *logical-translations-directory*
			 (merge-pathnames trans-fname
					  *logical-translations-directory*))))
    (cond ((probe-file trans-fname)
	   (load trans-fname)
	   t)
	  ((and *logical-translations-directory*
		(probe-file pathname))
	   (load pathname)
	   t)
	  (t
	   (error "Logical pathname translations for host ~A not found."
		  host)))))

#+(and :aclpc :cl-http)
(defvar *systems-directory*
   (merge-pathnames "Systems\\"
      allegro::*application-directory*))

#+(and :aclpc :cl-http)
(defun load-system-declaration-file (system-name)
   (load (merge-pathnames 
            (concatenate 'string
                         (string-downcase (symbol-name system-name))
                         "-sysdcl.lisp")
            *systems-directory*)))

#+(and :aclpc :cl-http)
(unless (fboundp '%%compile-system)
   (setf (symbol-function '%%compile-system) #'compile-system)
   (defun compile-system (sym &rest args)
      (load-system-declaration-file sym)
      (apply #'%%compile-system sym args)))

#+(and :aclpc :cl-http)
(unless (fboundp '%%load-system)
   (setf (symbol-function '%%load-system) #'load-system)
   (defun load-system (sym &rest args)
      (load-system-declaration-file sym)
      (apply #'%%load-system sym args)))

#+:mcl
(defun delete-logical-host (name)
  (setf ccl::%logical-host-translations%
        (delete name
                ccl::%logical-host-translations%
                :key #'first :test #'string-equal)))

(defmacro define-system (name
                         (&key
                          (pretty-name (symbol-name name))
                          default-pathname
                          (default-package nil)
                          (subsystem nil))
                         components)
  #+(or :Allegro :mcl :Lispworks (and :ACLPC (not :CL-HTTP)))
  (declare (ignore subsystem 
                   #+(or :mcl :Lispworks)
                   pretty-name
                   #-:mcl default-package))
  (labels ((host-substring (logical-pathname)
             (let ((position (position #\: logical-pathname)))
               (if position 
                 (subseq logical-pathname 0 position)
                 nil)))
           #+(or :Genera :mcl :Lispworks :ACLPC)
           (flatten-serial-parallel-descriptions
               (description)
             (if (consp description)
               (mapcan #'flatten-serial-parallel-descriptions
                       (rest description))
               (list description))))
    
    (unless default-pathname
      (error "A default pathname must be supplied in a system definition."))
    
    (let ((logical-host (host-substring default-pathname))
          #+:Genera
          (systems-depending-on
           (remove-if-not #'symbolp
                          (flatten-serial-parallel-descriptions
                           components))))
      
      (unless logical-host
        (error "Systems must be given a logical pathname as default pathname."))
      
      `(progn
         #|#+:mcl
         (delete-logical-host ,logical-host)
         (load-logical-pathname-translations ,logical-host)|#
         
         #+:Allegro
         (excl:defsystem ,name
                         (:default-pathname ,default-pathname
                           :pretty-name ,pretty-name)
           ,components)
         
         #+(and :ACLPC (not :cl-http))
         (define-system-1 
           :name ',name
           :source-dir (translate-logical-pathname ,default-pathname)
           :items ',(flatten-serial-parallel-descriptions
                     components))
         
         #+(and :ACLPC :CL-HTTP)
         (defsystem ,name
                    (:pretty-name ,pretty-name
                                  :default-pathname ,default-pathname)
           ,components)
         #+:Lispworks
         (lw:defsystem ,name
                       (:default-pathname ,(string-upcase default-pathname))
           :members
           ,(mapcar #'(lambda (component)
                        (if (symbolp component)
                          `(,component :type :system)
                          component))
                    (flatten-serial-parallel-descriptions
                     components))
           :rules
           ((:in-order-to :compile :all
                          (:requires (:load :previous)))))
         
         #+:Genera
         (,(if subsystem
             'sct:defsubsystem
             'sct:defsystem)
          ,name
          (:default-pathname ,default-pathname
            :pretty-name ,pretty-name)
          ,@(mapcar #'(lambda (system)
                        `(:module ,system (,system) (:type :system)))
                    systems-depending-on)
          ,components)
         
         #+(and :mcl (not (and :mk-defsystem :ui-lib)))
         (defsystem:defserial-system ,name
                                     (:default-pathname ,default-pathname
                                       . ,(if default-package
                                            `(:default-package ,default-package)
                                            ()))
           . ,(flatten-serial-parallel-descriptions components))
         #+(and :mcl :mk-defsystem :ui-lib)
         (cl-user::defsystem ,name
                             :source-pathname ,default-pathname
                             :binary-pathname ,default-pathname
                             :depends-on ,systems-depending-on
                             :components
                             ,(mapcar #'(lambda (component)
                                          `(:file ,component))
                                      (remove-if
                                       #'symbolp
                                       (flatten-serial-parallel-descriptions
                                        components))))))))


#+:MCL
(pushnew '("ccl:*.pathname-translations" "ccl:translations;*.translations")
         (logical-pathname-translations "ccl")
         :test #'equal)

#|


(define-system :test
  (:default-pathname "test:default;"
   :pretty-name "Test"
   :subsystem t)
  (:serial (:parallel :bar :bvyy) "test1" "test2"))

|#
