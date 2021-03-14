;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: GUI; Base: 10 -*-

(in-package gui)

(defmacro with-visco-frame ((name) &body body)
  `(let ((,name *visco-frame*))
     ,@body))

(defmacro with-visco-buttons-frame ((name) &body body)
  `(let ((,name *visco-buttons-frame*))
     ,@body))

(defmacro with-visco-inspector-frame ((name) &body body)
  `(let ((,name *visco-inspector-frame*))
     ,@body))
  
