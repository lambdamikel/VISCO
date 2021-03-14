;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: CL-USER; Base: 10 -*-

(in-package :CL-USER)

;;;
;;; Define Packages
;;;

(export '(transform-xy-list
	  tree-reverse
	  tree-remove
	  =>
	  <=>
	  intervall-intersects-p
	  circle-intervall-intersects-p
	  lies-in-circle-intervall-p
	  
	  circle-intervall-length
	  
	  rad-to-deg
	  deg-to-rad
	  recode-german-characters
	  
	  zerop-eps
	  =-eps
	  <=-eps
	  >=-eps
	  
	  yes
	  no
	  
	  +2pi+
	  +pi/2+
	  
	  defpersistentclass
	  persistent-object
	  
	  load-persistent-object
	  make-object-persistent
	  initialize-loaded-persistent-object

	  circle-subseq
	  mynconc
	  my-format
	  pushend
	  get-lambda-args
	  set-equal
	  set-disjoint))

(defpackage geometry 
  (:use common-lisp-user 
	common-lisp)
  (:export geom-error

	   infinit
	   
	   polygon
	   chain
	   
           geom-thing
           geom-point
	   geom-line
	   geom-chain-or-polygon
	   geom-chain	   
	   geom-polygon
	   geom-aggregate
	   bounding-box
	   bounding-box-mixin
	   
	   delete-object
	   
	   make-point
	   p
	   make-line
	   l
	   make-polygon
	   poly
	   make-chain
	   chain
	   make-aggregate
	   agg
	   make-bounding-box
	   bb
	   
	   bounding-box-p
	   
	   bound-to
	   
	   id
	   mark-value
	   mark-object
	   x
	   y
	   det
	   det-0-vecs
	   bb-width
	   bb-height
	   
	   p1 
	   p2
	   centroid
	   
	   p1-of
	   p2-of
	   centroid-of
	   pcenter-of
	   

	   pmin				; BB 
	   pmax				; BB
	   pcenter			; BB
	   radius			; BB
	   
	   segments	  
	   point-list
	   has-parts			; nur fuer Aggregate !
	   part-of
	   
	   point-=-p
	   point->=-p
	   point-<=-p
	   line-=-p
	   
	   joins-p
	   parallel-p
	   upright-p
	   
	   primary-p			; Primaerobjekt <=> keine Master
	   component-p 
	   common-root-p
	   get-all-masters		; fuer alle Objekte !
	   get-all-components 
	   get-direct-components	; fuer alle Objekte !
	   get-already-present-direct-master ; fuer alle Objekte !
	   get-topmost-master
	   get-topmost-common-master
	   get-direct-common-master
	   get-direct-common-master-from-list
	   
	   
	   for-each-master-holds-p
	   for-each-component-holds-p
	   
	   for-some-master-holds-p
	   for-some-component-holds-p

	   calculate-bounding-box
	   invalidate-bounding-box
	   
	   calculate-bounding-box-for-complex-object
	   calculate-centroid
	   
	   calculate-intersection-point
	   calculate-intersection-line
	   
	   angle-difference
	   
	   distance-between*
	   distance-between-point-and-line
	   distance-between
	   distance-between-xy

	   length-of-line 
	   
	   point-truly-inside-box-p
	   point-truly-inside-box-p*
	   
	   point-truly-outside-box-p
	   point-truly-outside-box-p*
	   
	   box-overlaps-box-p
	   box-overlaps-box-p*
	   
	   enlarged-box-overlaps-box-p
	   
	   box-truly-overlaps-box-p
	   box-truly-overlaps-box-p*
	   
	   box-touches-box-p
	   box-touches-box-p*

	   box-inside-box-p
	   box-inside-box-p*
	   
	   box-truly-inside-box-p
	   box-truly-inside-box-p*
	   
	   line-intersects-box-border-p
	   line-intersects-box-border-p*
	   
	   normalize
	   angle-between
	   angle-between*		   
	   distance-and-orientation
	   distance-and-orientation*
	   global-orientation
	   
	   
	   make-matrix
	   reset
	   translate
	   scale
	   rotate
	   compose
	   with-matrix	   
	   with-translation
	   with-scaling
	   with-rotation
	   with-no-matrix-at-all
	   
	   proportional-2-point
	   fixed-sy-2-point	   

	   lies-on-p
	   lies-on-p*	   
	   inside-p  
	   inside-p*
	   outside-p
	   outside-p*
	   intersects-p
	   crosses-p
	   crosses-p*
	   touches-p
	   0d-touches-p
	   1d-touches-p
	   1d-intersects-p
	   equal-p
	   inside-epsilon-p
	   inside-epsilon-p*

	   calculate-relation
	   inverse

	   get-epsilon-enclosure-relevant-points
	   get-epsilon-enclosure-relevant-points*

	   segment-list-ok-p
	   
					; Symbole (werden von calculate-relation geliefert)
	   disjoint
	   lies-on
	   crosses   
	   touches
	   0d-intersects	       
	   1d-intersects		
	   intersects
	   inside
	   contains
	   covers
	   covered-by
	   overlaps
	   ))

(defpackage sqd 
  (:use common-lisp-user 
	common-lisp)
  (:export read-sqd-file
					; vom Client zu implementieren!
	   set-client-bb
	   recognized-os-p
	   transform-sqd-pg
	   transform-sqd-sy
	   transform-sqd-tx
	   transform-sqd-li
	   transform-sqd-sn
	   transform-sqd-bo	   
	   transform-sqd-fl))

(defpackage objects-with-relations
  (:use common-lisp-user 
	common-lisp 
	geometry)
  (:export geom-thing-with-relations
	   geom-point-with-relations
	   geom-line-with-relations	   
	   geom-chain-or-polygon-with-relations
	   geom-chain-with-relations
	   geom-polygon-with-relations
	   
	   inside
	   
	   intersects
	   intersects-points
	   intersects-lines
	   intersects-chains
	   intersects-polygons
	   
	   contains
	   contains-points
	   contains-lines
	   contains-chains
	   contains-polygons
	   
	   
	   calculate-relations
	   remove-all-relations
	   store-intersection-between
	   store-inside-between
	   delete-intersection-between
	   delete-inside-between))


(defpackage spatial-index
  (:use common-lisp-user 
	common-lisp 
	geometry)
  (:export spatial-index
	   
           si-geom-thing
	   si-geom-point
	   si-geom-line
	   si-geom-chain-or-polygon
	   si-geom-chain
	   si-geom-polygon

	   install-as-current-index
	   make-spatial-index
           init-spatial-index
	   reset-spatial-index
	   insert-into-spatial-index
	   remove-from-spatial-index

	   elements
	   set-value-for
	   get-value-for
	   in-bucket

	   with-new-level
	   with-selected-buckets
	   with-selected-objects
	   
	   bucket-selected-p
	   candidate-selected-p

	   get-point-from-spatial-index*
	   
	   xmin
	   ymin 
	   xmax 
	   ymax))


(defpackage si-objects-with-relations
  (:use common-lisp-user 
	common-lisp 
	geometry 
	objects-with-relations 
	spatial-index)
  (:export si-geom-thing-with-relations
	   si-geom-point-with-relations
	   si-geom-line-with-relations
	   si-geom-chain-with-relations
	   si-geom-polygon-with-relations
	   
	   calculate-inside-relations))

(defpackage fonts
  (:use clim
	clim-lisp)
  (:export draw-vector-text))

(defpackage database 
  (:use common-lisp-user	
	common-lisp
	clim
	clim-lisp
	geometry	
	spatial-index
	fonts
	objects-with-relations
	si-objects-with-relations
	sqd)
  (:shadowing-import-from geometry
			  polygon chain
			  make-point
			  make-line
			  make-polygon)
  (:shadowing-import-from clim
			  with-translation
			  with-rotation
			  with-scaling
			  interactive-stream-p 
			  pathname
                          elements
			  truename)
  (:shadowing-import-from clim-user
                          write-string
                          write-char
                          write-byte
                          with-open-stream
                          unread-char
                          terpri
                          stream-element-type
                          read-line
                          read-char-no-hang
                          read-char
                          read-byte
                          peek-char
                          listen
                          fresh-line
                          format
                          force-output
                          finish-output
                          close
                          clear-output
                          clear-input
                          streamp
                          output-stream-p
                          open-stream-p
                          input-stream-p)
  (:export db
	   db-object
	   db-point
	   db-line
	   db-chain
	   db-polygon
   
	   get-current-db
           install-as-current-db
	   reset-db
	   load-db
	   store-db
	   make-db-from-sqd-file
	   
	   objects
	   all-os
	   ready
	   lookup-os
	   *os*
	   map-viewer
	   set-current-map-position-and-radius
	   draw-current-map-to-foreign-stream
	   
	   file-selector
	   mcl-pane
	   inverse-highlighter
	   with-centering
	   with-border
	   draw-marker*
	   get-size-of-graphic))
  

(defpackage query-compiler
  (:use common-lisp-user	
	common-lisp
	spatial-index
	geometry
	database 
	objects-with-relations)
  (:export *abort-search*
	   show-search-progress
	   
	   get-expanded-polygon
	   get-shrinked-polygon
	   
	   visco-object
	   query-object-or-enclosure
	   query
	   transparency
	   query-object
	   at-least-1d-query-object
	   point
	   marble
	   nail
	   origin
	   line
	   rubberband
	   atomic-rubberband
	   atomic-<=-rubberband
	   atomic->=-rubberband
	   beam
	   chain-or-polygon
	   chain
	   polygon
	   enclosure
	   derived-enclosure
	   drawn-enclosure
	   inner-enclosure
	   outer-enclosure
	   epsilon-enclosure
	   epsilon-p-enclosure
	   epsilon-m-enclosure
	   
	   disjoint
	   inside
	   contains
	   outside
	   excludes
	   inside-epsilon
	   epsilon-contains
	   intersects
	   angle-between
	   get-next-name
	   
	   sx
	   sy
	   r
	   sxmin
	   symin
	   sxmax
	   symax
	   width
	   height
	   sxsy-constraint
	   sx-type
	   sy-type
	   rot-type
	   
	   asg-inside-p
	   asg-outside-p
	   asg-inside-p*
	   asg-outside-p*
	   
	   1d-intersects-other-lines-p
	   ignore-disjoint-and-intersects-relations-p
	   
	   make-visco-query
	   make-visco-transparency
	   make-visco-marble
	   make-visco-nail
	   make-visco-origin
	   make-visco-rubberband
	   make-visco-atomic-rubberband
	   make-visco-atomic-<=-rubberband
	   make-visco-atomic->=-rubberband
	   make-visco-beam
	   make-visco-chain
	   make-visco-polygon
	   make-visco-drawn-enclosure
	   make-visco-inner-enclosure
	   make-visco-outer-enclosure
	   make-visco-epsilon-enclosure
	   make-visco-epsilon-p-enclosure
	   make-visco-epsilon-m-enclosure
	   
	   orientation-constraint-mixin
	   at-most-constraint-mixin
	   
	   find-constraint
	   
	   normalize-orientation-constraint
	   orientation-constraint
	   at-most-constraint
	   on-transparency
	   status
	   
	   status-of-line-object-ok-p
	   status-of-point-object-ok-p
	   status-of-chain-or-polygon-object-ok-p
	   
	   semantics
	   constraints
	   org-constraints
	   inside-any-enclosure-p*
	   fully-visible-p
	   negated-p
	   
	   matches-with-database-object-p
	   transparency-query-objects-and-enclosures
	   args
	   1st-arg
	   2nd-arg
	   
	   db
	   db-component
	   universe

	   change-status-applicable-p
	   get-dependents
	   
	   possible-operator-result-mixin
	   possible-operator-argument-mixin
	   constraints-mixin	   
	   
	   arg-of-operators
	   res-of-operators
	   arg-object
	   
	   get-successors-of
	   get-transparencies
	   origin
	   visco-objects
	   opaque-p
	   name
	   
	   get-all-query-objects-and-enclosures
	   
	   show-query-result
	   
	   get-query-semantics
	   compile-query
	   
	   execute-query
	   executable-p
	   invalidate
	   exec-fn
	   
	   ;;;
	   ;;;
	   ;;;
	   
	   not-applicable

	   apply-create-transparency
	   create-transparency-applicable-p
	   
	   apply-create-marble
	   create-marble-applicable-p
	   
	   apply-create-nail
	   create-nail-applicable-p
	   
	   apply-create-origin
	   create-origin-applicable-p
	   
	   apply-create-rubberband
	   create-rubberband-applicable-p
	   
	   apply-create-atomic-rubberband
	   create-atomic-rubberband-applicable-p
	   
	   apply-create-atomic-<=-rubberband
	   create-atomic-<=-rubberband-applicable-p
	   
	   apply-create-atomic->=-rubberband
	   create-atomic->=-rubberband-applicable-p
	   
	   apply-create-beam
	   create-beam-applicable-p
	   
	   apply-create-chain
	   create-chain-applicable-p
	   
	   apply-create-polygon
	   create-polygon-applicable-p
	   
	   apply-create-drawn-enclosure
	   create-drawn-enclosure-applicable-p
	   
	   apply-create-negate-drawn-enclosure
	   create-negate-drawn-enclosure-applicable-p
	   
	   apply-delete-object
	   delete-object-applicable-p
	   
	   apply-create-inner-enclosure
	   create-inner-enclosure-applicable-p
	   
	   apply-create-outer-enclosure
	   create-outer-enclosure-applicable-p
	   
	   apply-create-epsilon-enclosure
	   create-epsilon-enclosure-applicable-p
	   
	   apply-create-epsilon-p-enclosure
	   create-epsilon-p-enclosure-applicable-p
	   
	   apply-create-epsilon-m-enclosure
	   create-epsilon-m-enclosure-applicable-p
	   
	   apply-create-centroid
	   create-centroid-applicable-p
	   
	   apply-create-intersection-point
	   create-intersection-point-applicable-p
	   
	   #| apply-set-status
	   set-status-applicable-p
	   
	   apply-declare-to-marble
	   declare-to-marble-applicable-p
	   
	   apply-declare-to-nail
	   declare-to-nail-applicable-p
	   
	   apply-declare-to-origin
	   declare-to-origin-applicable-p
	   
	   apply-declare-to-rubberband
	   declare-to-rubberband-applicable-p
	   
	   apply-declare-to-atomic-rubberband
	   declare-to-atomic-rubberband-applicable-p
	   
	   apply-declare-to-atomic-<=-rubberband
	   declare-to-atomic-<=-rubberband-applicable-p
	   
	   apply-declare-to-atomic->=-rubberband
	   declare-to-atomic->=-rubberband-applicable-p
	   
	   apply-declare-to-beam
	   declare-to-beam-applicable-p
	   
	   |#
	   
	   apply-set-semantics
	   set-semantics-applicable-p
	   
	   apply-delete-semantics
	   delete-semantics-applicable-p

	   apply-set-at-most-constraint
	   set-at-most-constraint-applicable-p
	   
	   apply-delete-at-most-constraint
	   delete-at-most-constraint-applicable-p
	   
	   apply-set-orientation-constraint
	   set-orientation-constraint-applicable-p
	   
	   apply-delete-orientation-constraint
	   delete-orientation-constraint-applicable-p
	   
	   apply-set-relative-orientation-constraint
	   set-relative-orientation-constraint-applicable-p
	   
	   apply-delete-relative-orientation-constraint
	   delete-relative-orientation-constraint-applicable-p
	   
	   apply-set-transparency-properties
	   set-transparency-properties-applicable-p))


(defpackage gui
  (:use common-lisp-user 
	common-lisp       
	clim-lisp
	clim
	geometry	
	database
	query-compiler)
  (:export visco)
  (:shadowing-import-from query-compiler
			  line
			  point
			  polygon
			  enclosure)
  (:shadowing-import-from geometry
			  make-point
			  make-line
			  make-polygon)
  (:shadowing-import-from clim
			  with-translation
			  with-rotation
			  with-scaling
			  interactive-stream-p 
			  pathname
			  truename)
  (:shadowing-import-from clim-user
                          write-string
                          write-char
                          write-byte
                          with-open-stream
                          unread-char
                          terpri
                          stream-element-type
                          read-line
                          read-char-no-hang
                          read-char
                          read-byte
                          peek-char
                          listen
                          fresh-line
                          format
                          force-output
                          finish-output
                          close
                          clear-output
                          clear-input
                          streamp
                          output-stream-p
                          open-stream-p
                          input-stream-p))
			  

(defpackage visco 
  (:use common-lisp-user 
	common-lisp     
	gui
	database
	query-compiler))

