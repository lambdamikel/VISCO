;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: QUERY-COMPILER; Base: 10 -*-

(in-package query-compiler)

;;;
;;; 
;;;

(defun get-sorted-and-activated-ccs-no-generator-required (obj)
  (remove-if #'(lambda (cc)
		 (or (not (activated-p cc))
		     (ignored-p cc)))
	     (sorted-testers obj)))

(defmethod get-sorted-and-activated-ccs ((obj enclosure))
  (get-sorted-and-activated-ccs-no-generator-required obj))

(defmethod get-sorted-and-activated-ccs ((obj transparency))
  (get-sorted-and-activated-ccs-no-generator-required obj))

(defmethod get-sorted-and-activated-ccs ((obj visco-object))
  (let ((generator
	 (find-if #'(lambda (cc)
		      (and (activated-p cc) 
			   (not (ignored-p cc)))) ; evtl. ist ein ignorierter Generator dennoch sehr gut ?!
		  (sorted-generators obj))))
    (cons generator
	  (remove-if #'(lambda (cc)
			 (or (eq cc generator)
			     (ignored-p cc)
			     (not (activated-p cc))))
		     (sorted-testers obj)))))

;;;
;;;
;;;

(defun get-object-rank-no-generator-required (obj)
  (loop as cc in (sorted-testers obj)    
      when (and (activated-p cc)
		(not (ignored-p cc)))
      sum (tester-rank cc)))

(defmethod get-object-rank ((obj enclosure))
  (get-object-rank-no-generator-required obj))

(defmethod get-object-rank ((obj transparency))
  (get-object-rank-no-generator-required obj))

(defmethod get-object-rank ((obj visco-object))
  (let ((generator
	 (find-if #'activated-p (sorted-generators obj))))
    (* (1+ (generator-rank generator))
       (loop as cc in (sorted-testers obj)
	   when (and (not (eq cc generator))
		     (activated-p cc)
		     (not (ignored-p cc)))
	   sum (tester-rank cc)))))

;;;
;;;
;;;

(defmethod compare-nodes ((node1 visco-object) (node2 visco-object))
  (typecase node1
    (transparency t)
    (enclosure (activated-p (on-transparency node1)))
    (otherwise (typecase node2
		 (transparency nil)
		 (enclosure (not (activated-p (on-transparency node2))))
		 (otherwise
		  (> (get-object-rank node1) ; echte Kosten
		     (get-object-rank node2)))))))

;;;
;;;
;;;

(defun finalize-and-install-plan (plan)
  (dolist (obj plan)
    (setf (activated-and-sorted-compiler-conditions obj) 
      (get-sorted-and-activated-ccs obj))
    (activate obj))
  (deactivate-all (reverse plan))
  plan)

(defun determine-best-plan (plans)
  (finalize-and-install-plan
   (get-best plans)))

(defun get-best (plans)
  (let ((max-rank nil)
	(max-plan nil))
    (dolist (plan plans max-plan)
      (let ((rank (get-plan-rank plan)))
	(when (or (not max-rank)
		  (> rank max-rank))
	  (setf max-rank rank
		max-plan plan))))))

(defun get-plan-rank (plan)
  (let ((count 0))
    (prog1
	(loop as obj in plan
	    sum (float (* (get-object-rank obj)
                          (exp count)))
	    do (progn 
		 (decf count)
		 (activate obj)))
      (deactivate-all (reverse plan)))))

;;;
;;;
;;;

(defconstant +generator-hash-pos+
    (let ((ht (make-hash-table))
	  (order '(cc-construct-chain-or-polygon-from-components		 
		   cc-construct-atomic-rubberband-from-components
		   cc-construct-rubberband-from-components
		   cc-construct-intersection-point
		   cc-has-centroid
		   cc-centroid-of
		   cc-endpoint-of
		   cc-has-endpoint		 
		   cc-joins 
		   cc-position-is
		   cc-segment-of
		   cc-has-segment
		   cc-point-of
		   cc-has-point
		   cc-rubberband-of
		   cc-has-rubberband
		   cc-intersects
		   cc-inside
		   cc-contains
		   cc-inside-epsilon)))
      (dolist (g order)
	(setf (gethash g ht) (position g order)))
      ht))

(defmethod generator-is-better-than-p ((cc1 compiler-condition) (cc2 compiler-condition))
  (let ((pos1 (or (gethash (type-of cc1) +generator-hash-pos+) 1000))
	(pos2 (or (gethash (type-of cc2) +generator-hash-pos+) 1000)))
    (if (= pos1 pos2)
	(compare-equal-generators cc1 cc2)
      (< pos1 pos2))))

(defmethod compare-equal-generators ((cc1 compiler-condition) (cc2 compiler-condition))
  nil)

;;;
;;;
;;;

(defconstant +tester-hash-pos+
    (let ((ht (make-hash-table))
	  (order '(cc-type-is
		   cc-is-primary
		   cc-position-is
		   cc-position-is-inverse
		   cc-semantics-is
		   cc-length-is
		   cc-length-is-inverse
		   cc-orientation-is
		   cc-orientation-is-inverse		  
		   cc-angle-between
		   cc-has-centroid
		   cc-centroid-of
		   cc-endpoint-of
		   cc-has-endpoint
		   cc-joins
		   cc-point-of
		   cc-has-point
		   cc-segment-of
		   cc-has-segment
		   cc-construct-atomic-rubberband-from-components
		   cc-at-most-has-segments-is
		   cc-at-least-has-segments-is
		   cc-rubberband-of
		   cc-has-rubberband
		   cc-construct-rubberband-from-components
		   cc-construct-chain-or-polygon-from-components		  
		   cc-intersects
		   cc-disjoint
		   cc-inside
		   cc-contains
		   cc-outside
		   cc-excludes
		   cc-inside-epsilon
		   cc-epsilon-contains
		   #| cc-relations-exists-is |#)))
      (dolist (g order)
	(setf (gethash g ht) (position g order)))
      ht))

(defmethod tester-is-better-than-p ((cc1 compiler-condition) (cc2 compiler-condition))
  (let ((pos1 (or (gethash (type-of cc1) +tester-hash-pos+) 1000))
	(pos2 (or (gethash (type-of cc2) +tester-hash-pos+) 1000))) 	
    (if (= pos1 pos2)
	(compare-equal-testers cc1 cc2)
      (< pos1 pos2))))

(defmethod compare-equal-testers ((cc1 compiler-condition) (cc2 compiler-condition))
  nil)

(defmethod compare-equal-testers ((cc1 cc-type-is) (cc2 cc-type-is))
  (let ((c1 (candidate cc1))
	(c2 (candidate cc2)))
    (typecase c1
      (chain-or-polygon t)
      (line (or (typep c2 'point)
		(typep c2 'rubberband)))
      (point (not (typep c2 'rubberband)))
      (rubberband nil))))
