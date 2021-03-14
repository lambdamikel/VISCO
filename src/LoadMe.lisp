;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: CL-USER -*-

(in-package :cl-user)

(let ((current-dir (directory-namestring
                    (truename *loading-file-source-file*))))
  (setf (logical-pathname-translations "defsystem")
        (list (list "**;*.*.*"
                    (full-pathname
                     (concatenate 'string
                                  current-dir
                                  "defsystem:**:*.*")))))
  
  (compile-file "defsystem:Defsystem")
  (load "defsystem:Defsystem")
  (compile-file "defsystem:define-system")
  (load "defsystem:define-system")
  
  (setf (logical-pathname-translations "visco")
        (list (list "fonts;*.*"
                    (full-pathname
                     (concatenate 'string
                                  current-dir
                                  "db:*.*")))
              (list "spatial-index;*.*"
                    (full-pathname
                     (concatenate 'string
                                  current-dir
                                  "si-objects-with-relations:*.*")))
              (list "maps;*.*"
                    (full-pathname
                     (concatenate 'string
                                  current-dir
                                  "maps:*.*")))
              (list "queries;*.*"
                    (full-pathname
                     (concatenate 'string
                                  current-dir
                                  "queries:*.*")))
              (list "**;*.*"
                    (full-pathname
                     (concatenate 'string
                                  current-dir
                                  "**:*.*")))
              (list "*.*"
                    (full-pathname
                     (concatenate 'string
                                  current-dir
                                  "*.*"))))))

(load "visco:visco-sysdcl")
(defsystem:compile-load-system 'visco)
