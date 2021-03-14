(in-package CL-USER)

(require "clim")

(setf (logical-pathname-translations "visco")
  '(("spatial-index;*.*" 
      "C:\\Users\\Michael\\Desktop\\Diplomarbeit\\Visco\\si-objects-with-relations\\*.*")
    ("fonts;*.*"
     "visco:db;*.*")
    ("aux;*.*"
     "visco:aux2;*.*")
    ("**;*.*"       "C:\\Users\\Michael\\Desktop\\Diplomarbeit\\Visco\\**\\*.*")))

(load "visco:define-system.lisp")


(push :visco-demo-mode *features*)
;;; (push :antiker-mac *features*)

;;;
;;; Systems
;;;

(define-system aux 
    (:default-pathname "visco:aux;")
  (:serial "aux6.lisp"))


(define-system sqd
    (:default-pathname "visco:sqd;")
  (:serial 
   "sqd-reader2.lisp"
   "hex.lisp"
   "def1.sqd"
   "def2.sqd"
   "sqd-transformer.lisp"
   ))


(define-system geometry
    (:default-pathname "visco:geometry;")
  (:serial 
   "relatio16.lisp"
   "tree-structure.lisp"
   "box-relations.lisp"
   "interse4.lisp"
   "inside3.lisp"
   "epsilon2.lisp"
   "equal.lisp"
   "reldef3.lisp"))

(define-system objects-with-relations
    (:default-pathname "visco:objects-with-relations;")
  (:serial 
   "obj-w-rel5.lisp"))


(define-system spatial-index
    (:default-pathname "visco:spatial-index;")
  (:serial 
   "spatial-index-macros5.lisp"
   "spatial-index9.lisp"))


(define-system si-objects-with-relations
    (:default-pathname "visco:si-objects-with-relations;")
  (:serial 
   "si-obj-w-rel3.lisp"))


(define-system database
    (:default-pathname "visco:db;")
  (:serial 
   "os2.lisp"
   "db8.lisp"
   "fonts.lisp"
   "gui-aux.lisp"
   "map-viewer3.lisp"))

(define-system query-compiler
    (:default-pathname "visco:query-compiler;")
  (:serial  
   "compiler-macros8.lisp" 	       
   "query-compiler27.lisp"
   "db-knowledge2.lisp"
   "compiler-pieces17.lisp"
   "optimizer10.lisp"))

(define-system asg				; "abstract syntax graph"
    (:default-pathname "visco:asg;")
  (:serial 
   "constraints4.lisp"
   "asg17.lisp"
   "status2.lisp"
   "ops10.lisp"))

(define-system gui
    (:default-pathname "visco:gui;")
  (:serial 
   "macros.lisp"
   "specials.lisp"
   "classes10.lisp"
   "constructors5.lisp"
   "frame9.lisp"
   "query-result3.lisp"
   "other-commands2.lisp"
   "patterns4.lisp"
   "buttons.lisp"       
   "option-buttons4.lisp"
   "object-buttons5.lisp"
   "operators9.lisp"
   "operator-buttons6.lisp" 
   "status-buttons7.lisp"
   "control7.lisp"
   "creators7.lisp"
   "mover7.lisp"
   "draw15.lisp"))


(define-system visco 
    (:default-pathname "visco:")
  (:serial #+mcl "clim-mac-settings.lisp"
           #+mcl "ansi-change-class-patch.lisp"
           "visco-packages.lisp"
           aux 
	   sqd
	   geometry
	   objects-with-relations
	   spatial-index
	   si-objects-with-relations	   
	   database  
           asg
           query-compiler
	   gui))

(compile-system 'visco)
(load-system 'visco)



