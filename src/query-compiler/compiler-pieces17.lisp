;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: QUERY-COMPILER; Base: 10 -*-

(in-package query-compiler)

;;;
;;;
;;;

(defun get-check-ticks-code (name-for-angle ticks)  
  (let ((ticks 
	 (remove '(nil nil)
		 (remove nil ticks)
		 :test #'equal)))
    (if ticks
      `(and (or ,@(mapcar #'(lambda (tick)
			      (if (listp tick)
				  (if (rest (rest tick))
				      (error "Bad intervall!")
				    (let ((first (first tick))
					  (second (second tick)))
				      (if first
					  (if second
					      (if (= first second)
						  `(=-eps ,first ,name-for-angle)
						(if (not (< first second))
						    (error "Bad intervall!")
						  `(and (<=-eps ,first
								,name-for-angle)
							(<=-eps ,name-for-angle
								,second))))
					    `(<=-eps ,first ,name-for-angle))
					(if second
					    `(<=-eps ,name-for-angle ,second)))))
				`(=-eps ,tick ,name-for-angle)))
			  ticks)))
      t)))

(defun get-check-circle-ticks-code (name-for-angle ticks)  
  (let ((ticks 
	 (remove '(nil nil)
		 (remove nil ticks)
		 :test #'equal)))
    (if ticks
      `(and (or ,@(mapcar #'(lambda (tick)
			      (if (listp tick)
				  (if (rest (rest tick))
				      (error "Bad circle intervall!")
				    (let ((first (first tick))
					  (second (second tick)))
				      (cond ((and first second)
					     (if (= first second)
						 `(=-eps ,name-for-angle ,first)
					       (if (< first second)
						   `(and (<=-eps ,first ,name-for-angle)
							 (<=-eps ,name-for-angle ,second))
						 `(or ; RICHTIG !!!, nicht AND !!!
						   (>=-eps ,second ,name-for-angle)
						   (>=-eps ,name-for-angle ,first)))))
					    (t (error "Bad circle intervall!")))))
				`(=-eps ,name-for-angle ,tick)))
			  ticks)))
      t)))


(defmacro spatial-select-and-bind ((candidate mode present &rest args) &body body)
  `(with-selected-objects (candidate ,present
				     (and (not (bound-to candidate))
					  (typep candidate 'db-object))
				     ,mode
				     ,@args)
     (with-binding (,candidate candidate)
       ,@body)))

;;;
;;;
;;;

(defproperty is-whole-DB-generator 
    (:generator-T
     `( (dolist (candidate (objects (get-current-db)))
	  (with-binding (,candidate candidate)
	    ,@(funcall continuation)))))
  (:logic `( (db-object ,candidate)
	     ,@(funcall continuation))))



(defproperty is-primary 
    (:tester-T `( (primary-p candidate)
		  ,@(funcall continuation)))
  (:logic `( (primary ,candidate)
	     ,@(funcall continuation))))

(defproperty type-is 
    (:tester-T `( ,(typecase candidate
		     (point 
		      `(typep candidate 'db-point))
		     (atomic-rubberband 
		      `(typep candidate 'db-line))
		     (rubberband 
		      `(or (typep candidate 'db-line)
			   (typep candidate 'db-chain)))
		     (chain 
		      `(typep candidate 'db-chain))
		     (polygon 
		      `(typep candidate 'db-polygon)))
		    
		    ,@(funcall continuation)))
  (:logic `(  ,(typecase candidate
		 (point 
		  `(point ,candidate))
		 (atomic-rubberband 
		  `(line ,candidate))
		 (rubberband 
		  `(or (line ,candidate)
		       (chain ,candidate)))
		 (chain 
		  `(chain ,candidate))
		 (polygon 
		  `(polygon ,candidate)))
	       
	       ,@(funcall continuation))))

#|

(defproperty legal-binding-is 
    (:tester-T
     `( (legal-binding-p ,candidate candidate)
	,@(funcall continuation))))

|#


(defproperty relations-exists-is
    (:additional-slots constraints)
  (:tester-T
   (let ((res
	  (remove nil 
		  (mapcar #'(lambda (constraint-type)
			      (let ((num (count-if (first constraint-type)
						   (remove-if-not #'stored-relation-p
								  constraints))))
				(unless (zerop num)
				  (funcall (second constraint-type) num))))
			  
			  (list 
			   (list #'(lambda (obj) (typep obj 'inside)) 
				 #'(lambda (num)
				     `(>= (length (inside candidate)) ,num)))
			   
			   (list #'(lambda (obj) (and (typep obj 'contains)
						      (typep (2nd-arg obj) 'point)))
				 #'(lambda (num)
				     `(>= (length (contains-points candidate)) ,num)))
			   
			   
			   (list #'(lambda (obj) (and (typep obj 'contains)
						      (typep (2nd-arg obj) 'atomic-rubberband)))
				 #'(lambda (num)
				     `(>= (length (contains-lines candidate)) ,num)))

			   (list #'(lambda (obj) (and (typep obj 'contains)
						      (typep (2nd-arg obj) 'rubberband)))
				 #'(lambda (num)
				     `(>= (+ (length (contains-lines candidate))
					     (length (contains-chains candidate)))
					  ,num)))
			   
			   (list #'(lambda (obj) (and (typep obj 'contains)
						      (typep (2nd-arg obj) 'chain)))
				 #'(lambda (num)
				     `(>= (length (contains-chains candidate)) ,num)))
			   
			   (list #'(lambda (obj) (and (typep obj 'contains)
						      (typep (2nd-arg obj) 'polygon)))
				 #'(lambda (num)
				     `(>= (length (contains-polygons candidate)) ,num)))
			   
			   (list #'(lambda (obj) (and (typep obj 'intersects)
						      (typep (2nd-arg obj) 'point)))
				 #'(lambda (num)
				     `(>= (length (intersects-points candidate)) ,num)))

			   
			   (list #'(lambda (obj) (and (typep obj 'intersects)
						      (typep (2nd-arg obj) 'atomic-rubberband)))
				 #'(lambda (num)
				     `(>= (length (intersects-lines candidate)) ,num)))

			   (list #'(lambda (obj) (and (typep obj 'intersects)
						      (typep (2nd-arg obj) 'rubberband)))
				 #'(lambda (num)
				     `(>= (+ (length (intersects-lines candidate))
					     (length (intersects-chains candidate)))
					  ,num)))
			   
			   (list #'(lambda (obj) (and (typep obj 'intersects)
						      (typep (2nd-arg obj) 'chain)))
				 #'(lambda (num)
				     `(>= (length (intersects-chains candidate)) ,num)))
			   
			   (list #'(lambda (obj) (and (typep obj 'intersects)
						      (typep (2nd-arg obj) 'polygon)))
				 #'(lambda (num)
				     `(>= (length (intersects-polygons candidate)) ,num))))))))
     (when res
       `( ,@res		
	  ,@(funcall continuation))))))


(defproperty semantics-is 
    (:additional-slots keys)
  (:tester-T `( (with-slots (all-os) candidate
		  (and
		   ,@(mapcar 
		      #'(lambda (descriptor)
			  `(member ',descriptor all-os))
		      keys)))
		  ,@(funcall continuation)))
  (:logic `( ,@(mapcar #'(lambda (descriptor)
			   `(semantics ,candidate ,descriptor))
		       keys)
	       ,@(funcall continuation))))

(defproperty at-most-has-segments-is 
    (:additional-slots at-most)
  (:tester `( (=> (typep candidate 'geom-chain-or-polygon) ; absichern weg. Rubberbands !
		  (<= (length (segments candidate))
		      ,at-most))
	      ,@(funcall continuation)))
  (:logic `( (=> (or (chain ,candidate)
		     (polygon ,candidate))
		 (<= (no-of-segments ,candidate) ,at-most))
	     ,@(funcall continuation))))


(defproperty at-least-has-segments-is	; redundant, aber gut fuer Effizienz! => :logic fehlt
    (:additional-slots at-least)
  (:tester-T `( (=> (typep candidate 'geom-chain-or-polygon) ; s.o.
		    (>= (length (segments candidate))
			,at-least))
		,@(funcall continuation))))

;;;
;;;
;;;


(defdependency depends-on		; erzwingt *NUR* Abhaengigkeit: "present" vor "candidate" suchen!
    nil)

;;;
;;;
;;;

(defdependency position-is		; CANDIDATE = Nail, PRESENT = Transparency
    (:generator-T `( (with-matrix (,(matrix present))
		       (let ((candidate (get-point-from-spatial-index* (x ,candidate) (y ,candidate))))
			 (with-binding (,candidate candidate)
			   ,@(funcall continuation))))))
  (:generator-NIL `( (with-matrix (,(matrix present))
		       (let ((candidate (p (x ,candidate) (y ,candidate)
					   :affected-by-matrix-p nil)))
			 (with-binding (,candidate candidate)
			   ,@(funcall continuation))))))
  (:tester `( (with-matrix (,(matrix present))
		(and (=-eps (slot-value candidate 'x) 
			    (x ,candidate)) ; with-matrix wirkt ueber ACCESSORS
		     (=-eps (slot-value candidate 'y) 
			    (y ,candidate))))
		,@(funcall continuation)))
  (:logic `( (= ,candidate ,(get-transformed-object candidate))
	     ,@(funcall continuation))))


(defdependency position-is-inverse	; CANDIDATE = Transparency (ist bereits instantiiert, TESTER !!!!), PRESENT = Nail
    (:tester `( (with-matrix (,(matrix candidate))
		  (and (=-eps (slot-value (bound-to ,present) 'x) 
			      (x ,present)) ; with-matrix wirkt ueber ACCESSORS
		       (=-eps (slot-value (bound-to ,present) 'y) 
			      (y ,present))))
		  ,@(funcall continuation)))
  (:logic `( (= ,present ,(get-transformed-object present))
	     ,@(funcall continuation))))


;;;
;;;
;;;

(defdependency length-is
    (:tester `(( ,(typecase candidate	   
		    (beam '=-eps)
		    (atomic-<=-rubberband '<=-eps)
		    (atomic->=-rubberband '>=-eps))
		   (with-matrix (,(inverse-matrix present))
		     (length-of-line ,candidate))
		 ,(length-of-line candidate))
		  ,@(funcall continuation)))
  (:logic `( ( ,(typecase candidate
		  (beam '=-eps)
		  (atomic-<=-rubberband '<=-eps)
		  (atomic->=-rubberband '>=-eps))
		 (length-of-line ,(get-inverse-transformed-object candidate))
	       (length-of-line ,(get-const-name candidate)))
	       ,@(funcall continuation))))


(defdependency length-is-inverse
    (:tester `(( ,(typecase present	   
		    (beam '=-eps)
		    (atomic-<=-rubberband '<=-eps)
		    (atomic->=-rubberband '>=-eps))
		   (with-matrix (,(inverse-matrix candidate))
		     (length-of-line (bound-to ,present)))
		 ,(length-of-line present))
		  ,@(funcall continuation)))
  (:logic `( (,(typecase present 
		 (beam '=-eps)
		 (atomic-<=-rubberband '<=-eps)
		 (atomic->=-rubberband '>=-eps))
		 (length-of-line ,(get-inverse-transformed-object present))
	       (length-of-line ,(get-const-name present)))
	       ,@(funcall continuation))))

;;;
;;;
;;;

(defdependency orientation-is
    (:additional-slots ticks)
  (:tester `( (let* ((global-orientation-of-query-element 
		      ,(global-orientation candidate))
		     (global-orientation-of-candidate
		      (with-matrix ( ,(inverse-matrix present) )
			(global-orientation candidate)))
		     (angle-difference1	; die Differenz muss Tick-Wert (bzw. Intervall) entsprechen!
		      (normalize (- global-orientation-of-query-element
				    global-orientation-of-candidate)))
		     (angle-difference2 
		      (normalize (- angle-difference1 pi))))
		
		(or ,(get-check-circle-ticks-code 'angle-difference1 ticks)
		    ,(get-check-circle-ticks-code 'angle-difference2 ticks))
		,@(funcall continuation))))
  (:logic `( (exists (orientation)
		     (and (real orientation)
			  (or (= orientation 
				 (normalize (- (orientation ,(get-inverse-transformed-object candidate))
					       (orientation ,(get-const-name candidate)))))
			      (= orientation 
				 (normalize (- (orientation ,(get-inverse-transformed-object candidate)) 
					       (orientation ,(get-const-name candidate))
					       pi))))
			  ,(third (get-check-circle-ticks-code 'orientation ticks))))
	     ,@(funcall continuation))))


(defdependency orientation-is-inverse
    (:additional-slots ticks)
  (:tester `( (let* ((global-orientation-of-query-element 
		      ,(global-orientation present))
		     (global-orientation-of-present
		      (with-matrix ( ,(inverse-matrix candidate) )
			(global-orientation (bound-to ,present))))
		     (angle-difference1	; die Differenz muss Tick-Wert (bzw. Intervall) entsprechen!
		      (normalize (- global-orientation-of-query-element
				    global-orientation-of-present)))
		     (angle-difference2 
		      (normalize (- angle-difference1 pi))))
		
		(or ,(get-check-circle-ticks-code 'angle-difference1 ticks)
		    ,(get-check-circle-ticks-code 'angle-difference2 ticks))		
		,@(funcall continuation))))
  (:logic `( (exists (orientation)
		     (and (real orientation)
			  (or (= orientation 
				 (normalize (- (orientation ,(get-inverse-transformed-object present))
					       (orientation ,(get-const-name present)))))
			      (= orientation
				 (normalize (- (orientation ,(get-inverse-transformed-object present))
					       (orientation ,(get-const-name present))
					       pi))))
			  ,(third (get-check-circle-ticks-code 'orientation ticks))))
	     ,@(funcall continuation))))

;;;
;;; fuer Segmente X Punkte
;;;



(defdependency endpoint-of		; candidate = point, present = Line or Chain
    (:generator-T->T `( (let ((candidate (p1 (bound-to ,present))))
			  (with-binding (,candidate candidate)
			    ,@(funcall continuation)))
			  (let ((candidate (p2 (bound-to ,present))))
			    (with-binding (,candidate candidate)
			      ,@(funcall continuation)))))		       
  (:tester-T->T `( (or (eq candidate (p1 (bound-to ,present)))
		       (eq candidate (p2 (bound-to ,present))))
		   ,@(funcall continuation)))
  (:logic `( (endpoint-of ,candidate ,present)
	     ,@(funcall continuation))))


(defdependency has-endpoint		; candidate = line or chain, present = point
    (:generator-T->T `( (dolist (candidate (p1-of (bound-to ,present)))
			  (with-binding (,candidate candidate)
			    ,@(funcall continuation)))
			  (dolist (candidate (p2-of (bound-to ,present)))
			    (with-binding (,candidate candidate)
			      ,@(funcall continuation)))))
  (:tester-T->T `( (or (member candidate
			       (p1-of (bound-to ,present)))
		       (member candidate
			       (p2-of (bound-to ,present))))
		   ,@(funcall continuation)))
  (:logic `( (has-endpoint ,candidate ,present)
	     ,@(funcall continuation))))


;;;
;;; fuer Ketten/Polygone X Punkte
;;;

(defdependency point-of			; candidate = point, present = Polygon oder Kette
    (:generator-T->T `( (dolist (candidate (point-list (bound-to ,present)))
			  (with-binding (,candidate candidate)
			    ,@(funcall continuation)))))
  (:tester-T->T `( (member candidate (point-list (bound-to ,present)))
		   ,@(funcall continuation))))


;;; reine Effizienz-Kante: logic redundant!


(defdependency has-point		; candidate = Polygon oder Kette, present = point
    (:generator-T->T `( (dolist (candidate (part-of (bound-to ,present)))
			  (dolist (candidate (part-of candidate))
			    (with-binding (,candidate candidate)
			      ,@(funcall continuation))))))
  (:tester-T->T `( (member (bound-to ,present) (point-list candidate))
		   ,@(funcall continuation))))

;;;
;;;
;;;

(defdependency segment-of		; candidate ===segment-of====> present
    (:generator-T->T `( (dolist (candidate (segments (bound-to ,present)))		       
			  (with-binding (,candidate candidate)
			    ,@(funcall continuation)))))
  (:tester-T->T `( (member candidate (segments (bound-to ,present)))
		   ,@(funcall continuation)))
  (:logic `( (segment-of ,candidate ,present)
	     ,@(funcall continuation))))

(defdependency has-segment 
    (:generator-T->T `( (dolist (candidate (part-of (bound-to ,present)))
			  (with-binding (,candidate candidate)
			    ,@(funcall continuation)))))
  (:tester-T->T `( (member (bound-to ,present) (segments candidate))
		   ,@(funcall continuation)))
  (:logic `( (has-segment ,candidate ,present)
	     ,@(funcall continuation))))


;;;
;;;
;;;

(defdependency rubberband-of		; master ist Kette o. Polygon
    (:generator
     `( (let* ((master (bound-to ,present))
	       (length (length (segments master))))
	  (dotimes (chain-length
		       (let ((n (- length ; fuer die anderen Segmente noch was uebriglassen!
				   ,(1- (length (segments present))))))
			 ,(if (at-most-constraint candidate)
			      `(min n ,(at-most-constraint candidate))
			    `n)))
	    (dotimes (start-pos length)
	      (let ((segments
		     (circle-subseq (segments master)
				    start-pos (+ 1 start-pos chain-length))))
		(unless (some #'(lambda (s)
				  (bound-to s))
			      segments)
		  (let ((candidate 
			 (if (null (rest segments))
			     (first segments)
			   (progn 
			     (dolist (s segments)
			       (setf (bound-to s) ,candidate))
			     (chain segments :check-p nil :affected-by-matrix-p nil)))))
		    (unwind-protect
			(with-binding (,candidate candidate)
			  ,@(funcall continuation))
		      (when (typep candidate 'geom-chain)
			(delete-object candidate)
			(dolist (s segments)
			  (setf (bound-to s) nil))))))))))))
  (:tester `( ,(etypecase candidate
		 (atomic-rubberband 
		  `(member candidate (segments (bound-to ,present))))
		 (rubberband 
		  `(let ((segments (segments (bound-to ,present))))
		     (if (typep candidate 'geom-chain)
			 (every #'(lambda (s)
				    (member s segments))
				(segments candidate))
		       (member candidate segments)))))
		,@(funcall continuation)))
  (:logic `( (rubberband-of ,candidate ,present)
	     ,@(funcall continuation))))


(defdependency has-rubberband 
    (:generator-T `( (etypecase (bound-to ,present)
		       (geom-line
			(dolist (candidate (part-of (bound-to ,present)))
			  (with-binding (,candidate candidate)	
			    ,@(funcall continuation))))
		       (geom-chain
			(dolist (candidate (get-direct-common-master-from-list 
					    (segments (bound-to ,present))))
			  (with-binding (,candidate candidate)
			    ,@(funcall continuation))))) ))
  (:tester-T `( ,(etypecase present
		   (atomic-rubberband 
		    `(member (bound-to present) (segments candidate)))
		   (rubberband 
		    `(let ((segments (segments candidate)))
		       (if (typep (bound-to ,present) 'geom-chain)
			   (every #'(lambda (s)
				      (member s segments))
				  (segments (bound-to ,present)))
			 (member (bound-to present) segments)))))
		  ,@(funcall continuation)))
  (:logic `( (has-rubberband ,candidate ,present)
	     ,@(funcall continuation))))


;;;
;;;
;;;

(defdependency joins
    (:generator `( (let ((p1 (p1 (bound-to ,present)))
			 (p2 (p2 (bound-to ,present))))
		     (dolist (candidate (part-of p1))
		       (with-binding (,candidate candidate)
			 ,@(funcall continuation)))
		     (dolist (candidate (part-of p2))
		       (with-binding (,candidate candidate)
			 ,@(funcall continuation))))))
  (:tester `( (joins-p ,candidate ,present)
	      ,@(funcall continuation))))

;;;
;;;
;;;


(defdependency inside			; candidate is-inside present; present ist vom Typ Polygon
    (:generator-T->T `( (dolist (candidate (contains (bound-to ,present)))
			  (with-binding (,candidate candidate)
			    ,@(funcall continuation)))))
  (:generator-T->NIL (if (or (typep present 'drawn-enclosure)
			     (typep present 'transparency))
			 `( (with-matrix (,(matrix (on-transparency present)))
			      (spatial-select-and-bind (,candidate :inside (bound-to ,present))
						       ,@(funcall continuation))))
		       `( (spatial-select-and-bind (,candidate :inside (bound-to ,present))
						   ,@(funcall continuation)))))
  (:tester-T->T `( (member candidate
			   (contains (bound-to ,present)))
		   ,@(funcall continuation)))
  (:tester (if (or (typep present 'drawn-enclosure)
		   (typep present 'transparency))
	       `( (with-matrix (,(matrix (on-transparency candidate)))
		    (candidate-selected-p (bound-to ,present) candidate :inside))
		    ,@(funcall continuation))
	     `( (candidate-selected-p (bound-to ,present) candidate :inside)
		,@(funcall continuation))))
  (:logic `( ,(if (typep present 'drawn-enclosure)
		  `(inside ,candidate ,(get-transformed-object present))
		`(inside ,candidate ,present))
	       ,@(funcall continuation))))

					; Syntax: candidate "is :inside" (bound-to ,present) !!



(defdependency contains			; candidate ist vom Typ Polygon
    (:generator-T->T `( (dolist (candidate (inside (bound-to ,present)))
			  (with-binding (,candidate candidate)
			    ,@(funcall continuation)))))
  (:tester-T->T `( (member candidate
			   (inside (bound-to ,present)))
		   ,@(funcall continuation)))
  (:tester (if (or (typep candidate 'drawn-enclosure)
		   (typep candidate 'transparency))
	       `( (with-matrix (,(matrix (on-transparency candidate)))
		    (candidate-selected-p candidate (bound-to ,present) :inside))
		    ,@(funcall continuation))
	     `( (candidate-selected-p candidate (bound-to ,present) :inside)
		,@(funcall continuation))))
  (:logic `( ,(if (typep candidate 'drawn-enclosure)
		  `(contains ,(get-transformed-object candidate) ,present)
		`(contains ,candidate ,present))
	       ,@(funcall continuation))))



(defdependency outside			; present ist vom Typ Polygon
    (:tester-T->T `( (not (or (member candidate (intersects (bound-to ,present)))
			      (member candidate (contains (bound-to ,present)))))
		     ,@(funcall continuation)))
  (:tester (if (or (typep present 'drawn-enclosure)
		   (typep present 'transparency))
	       `( (with-matrix (,(matrix (on-transparency present)))
		    (candidate-selected-p (bound-to ,present) candidate :outside))
		    ,@(funcall continuation))
	     `( (candidate-selected-p (bound-to ,present) candidate :outside)
		,@(funcall continuation))))
  (:logic `( ,(if (typep present 'drawn-enclosure)
		  `(outside ,candidate ,(get-transformed-object present))
		`(outside ,candidate ,present))
	       ,@(funcall continuation))))


(defdependency excludes			; candidate ist vom Typ Polygon
    (:tester-T->T `( (typep candidate 'geom-polygon)
		     (not (or (member candidate (inside (bound-to ,present)))
			      (member candidate (intersects (bound-to ,present)))))
		     ,@(funcall continuation)))
  (:tester (if (or (typep candidate 'drawn-enclosure)
		   (typep candidate 'transparency))
	       `( (with-matrix (,(matrix (on-transparency present)))
		    (candidate-selected-p candidate (bound-to ,present) :outside))
		    ,@(funcall continuation))
	     `( (candidate-selected-p candidate (bound-to ,present) :outside)
		,@(funcall continuation))))
  (:logic `( ,(if (typep candidate 'drawn-enclosure)
		  `(excludes ,(get-transformed-object candidate) ,present)
		`(excludes ,candidate ,present))
	       ,@(funcall continuation))))

;;;
;;;
;;;

(defdependency disjoint			; disjoint <=> not(intersects)
    (:tester-T->T `( (not (or (member candidate (intersects (bound-to ,present)))
			      (component-p candidate (bound-to ,present))
			      (component-p (bound-to ,present) candidate)))
		     ,@(funcall continuation)))
  (:tester `( (not (or (candidate-selected-p (bound-to ,present) candidate :intersects)
		       (component-p candidate (bound-to ,present))
		       (component-p (bound-to ,present) candidate)))
	      ,@(funcall continuation)))
  (:logic `( (disjoint ,candidate ,present)
	     ,@(funcall continuation))))

;;;
;;;
;;;

(defdependency intersects 
    (:generator-T->T `( (dolist (candidate (intersects (bound-to ,present)))
			  (with-binding (,candidate candidate)
			    ,@(funcall continuation)))))
  (:generator-T->NIL `( (spatial-select-and-bind (,candidate :intersects (bound-to ,present))
						 ,@(funcall continuation))))
  (:tester-T->T `( (member candidate (intersects (bound-to ,present)))
		   ,@(funcall continuation)))
  (:tester `( (candidate-selected-p (bound-to ,present) candidate :intersects)
	      ,@(funcall continuation)))
  (:logic `( (intersects ,candidate ,present)
	     ,@(funcall continuation))))


;;;
;;;
;;;


(defdependency has-centroid		; candidate : line or chain or polygon, present : point
    (:generator-T->T `( (dolist (candidate (centroid-of (bound-to ,present)))
			  (with-binding (,candidate candidate)
			    ,@(funcall continuation)))))
  (:tester-T->T `( (eq candidate (centroid-of (bound-to ,present)))
		   ,@(funcall continuation)))
  (:logic `( (has-centroid ,candidate ,present)
	     ,@(funcall continuation))))


(defdependency centroid-of
    (:generator-T->T `( (let ((candidate (centroid (bound-to ,present))))
			  (with-binding (,candidate candidate)
			    ,@(funcall continuation)))))
  (:generator-T->NIL `( (let ((candidate
			       (let ((c (centroid (bound-to ,present))))
				 (get-point-from-spatial-index* (x c) (y c)))))
			  (with-binding (,candidate candidate)
			    ,@(funcall continuation)))))
  (:generator-NIL `( (let ((candidate (centroid (bound-to ,present))))
		       (with-binding (,candidate candidate)
			 ,@(funcall continuation)))))
  (:tester-T->T `( (eq candidate (centroid (bound-to ,present)))
		   ,@(funcall continuation)))
  (:tester `( (=-eps (slot-value (centroid (bound-to ,present)) 'x)
		     (slot-value candidate 'x))
	      (=-eps (slot-value (centroid (bound-to ,present)) 'y)
		     (slot-value candidate 'y))
	      ,@(funcall continuation)))
  (:logic `( (centroid-of ,candidate ,present)
	     ,@(funcall continuation))))

;;;
;;;
;;;

(defdependency inside-epsilon		; "candidate" ist in der epsilon-Umgebung von "present" enthalten!
    (:additional-slots radius)
  (:generator-T `( (spatial-select-and-bind (,candidate :epsilon (bound-to ,present)
							:epsilon-r ,radius 
							:epsilon-a (epsilon-a ,(on-transparency present))
							:epsilon-b (epsilon-b ,(on-transparency present)))
					    ,@(funcall continuation))))
  (:tester `( (candidate-selected-p (bound-to ,present) candidate :epsilon
				    :epsilon-r ,radius
				    :epsilon-a (epsilon-a ,(on-transparency present))
				    :epsilon-b (epsilon-b ,(on-transparency present)))
	      ,@(funcall continuation)))
  (:logic `( (inside-epsilon ,(get-inverse-transformed-object candidate) 
			     ,(get-inverse-transformed-object present))
	     ,@(funcall continuation))))


(defdependency epsilon-contains		; epsilon-Umgebung von "candidate" enthaelt "present"
    (:additional-slots radius)
  (:tester `( (candidate-selected-p (bound-to ,present) candidate :epsilon
				    :epsilon-r ,radius
				    :epsilon-a (epsilon-a ,(on-transparency present))
				    :epsilon-b (epsilon-b ,(on-transparency present)))
	      ,@(funcall continuation)))
  (:logic `( (epsilon-contains ,(get-inverse-transformed-object candidate)
			       ,(get-inverse-transformed-object present))
	     ,@(funcall continuation))))


;;;
;;;
;;;

(defdependency angle-between		; klaeren: "angle-between" lokal oder nicht?
    (:additional-slots allowed-derivation)
  (:tester `((let* ((global-orientation-diff-of-query-elements
		     ,(angle-difference
		       (global-orientation present)
		       (global-orientation candidate)))
		    
		    (global-orientation-diff-of-T-elements
		     (with-matrix ( ,(inverse-matrix (on-transparency candidate)) )
		       (angle-difference 
			(global-orientation (bound-to ,present))
			(global-orientation candidate))))
		    
		    (angle-difference
		     (angle-difference 
		      global-orientation-diff-of-query-elements
		      global-orientation-diff-of-T-elements)))
	       
	       (<=-eps angle-difference ,allowed-derivation))
	     ,@(funcall continuation)))
  (:logic `( (<= (angle-difference 
		  (angle-difference ,(get-const-name candidate) ,(get-const-name present))
		  (angle-difference ,(get-inverse-transformed-object candidate)
				    ,(get-inverse-transformed-object present)))
		 ,allowed-derivation)
	     ,@(funcall continuation))))

;;;
;;;
;;;

(defmultidependency construct-atomic-rubberband-from-components      
    (:generator-NIL
     `( (let ((candidate
	       (handler-case (l (bound-to (p1 ,candidate))
				(bound-to (p2 ,candidate))
				:affected-by-matrix-p nil)
		 (geom-error nil nil)
		 (error (cond) (error cond)))))
	  (when candidate
	    (with-binding (,candidate candidate)
	      ,@(funcall continuation))))))
  (:generator-T
   `( (dolist (candidate (get-direct-common-master 
			  (bound-to (p1 ,candidate))
			  (bound-to (p2 ,candidate))))
	(with-binding (,candidate candidate)
	  ,@(funcall continuation)))))
  (:tester `( (or (and (eq (p1 candidate)
			   (bound-to ,(first args)))
		       (eq (p2 candidate)
			   (bound-to ,(second args))))
		  (and (eq (p2 candidate)
			   (bound-to ,(first args)))
		       (eq (p1 candidate)
			   (bound-to ,(second args)))))
	      ,@(funcall continuation))))


(defmultidependency construct-rubberband-from-components
    (:generator-NIL			; status(candidate) = db-component oder universe
     (ecase (status candidate)
       (db-component
	`( (dolist (candidate (get-topmost-common-master (bound-to (p1 ,candidate))
							 (bound-to (p2 ,candidate))))
	     (typecase candidate
	       (geom-line
		(with-binding (,candidate candidate)
		  ,@(funcall continuation)))   
	       (geom-chain-or-polygon
		(dolist (segments	; die Segmente sind in der DB!!!
			    (list (get-segments-from-to candidate
							(bound-to (p1 ,candidate))
							(bound-to (p2 ,candidate)))
				  (get-segments-from-to candidate
							(bound-to (p2 ,candidate))
							(bound-to (p1 ,candidate)))))
		  (unless (some #'(lambda (s)
				    (bound-to s))
				segments)
		    (let ((candidate (if (null (rest segments))
					 (first segments)
				       (chain segments :check-p nil :affected-by-matrix-p nil))))
		      (with-binding (,candidate candidate)
			(dolist (segment segments)
			  (setf (bound-to segment) candidate))
			(unwind-protect
			    (progn ,@(funcall continuation))
			  (when (typep candidate 'geom-chain)
			    (delete-object candidate))
			  (dolist (segment segments)
			    (setf (bound-to segment) nil))))))))))))
       (universe (error "Not implemented!"))))
  (:generator-T				; status(candidate) = DB
   `( (dolist (candidate (get-direct-common-master 
			  (bound-to (p1 ,candidate))
			  (bound-to (p2 ,candidate))))
	(with-binding (,candidate candidate)
	  ,@(funcall continuation)))))
  (:tester `( (or (and (eq (p1 candidate)
			   (bound-to ,(first args)))
		       (eq (p2 candidate)
			   (bound-to ,(second args))))
		  (and (eq (p2 candidate)
			   (bound-to ,(first args)))
		       (eq (p1 candidate)
			   (bound-to ,(second args)))))
	      ,@(funcall continuation))))


(defmultidependency construct-chain-or-polygon-from-components
    (:generator-NIL
     `( (let ((candidate
	       (handler-case ( ,(if (typep candidate 'chain) ; Konstruktor-Funktion
				    'chain
				  'poly)
				 (append
				  ,@(mapcar #'(lambda (segment)
						(etypecase segment
						  (atomic-rubberband
						   `(list (bound-to ,segment)))
						  (rubberband
						   `(if (typep (bound-to segment)
							       'geom-chain)
							`(segments (bound-to ,segment))
						      (list (bound-to ,segment))))))
					    (segments candidate))))
		 (geom-error nil nil))))
	  (when candidate
	    (unwind-protect
		(with-binding (,candidate candidate)
		  ,@(funcall continuation))
	      (delete-object candidate))))))
  (:generator-T
   `( (dolist (candidate (get-direct-common-master-from-list
			  (nconc ,@(mapcar #'(lambda (segment)
					       (etypecase segment
						 (atomic-rubberband
						  `(list (bound-to ,segment)))
						 (rubberband
						  `(if (typep (bound-to segment)
							      'geom-chain)
						       `(segments (bound-to ,segment))
						     (list (bound-to ,segment))))))
					   (segments candidate)))))
	(with-binding (,candidate candidate)
	  ,@(funcall continuation)))))
  (:tester
   `( ,@(mapcar #'(lambda (segment)
		    (etypecase segment
		      (atomic-rubberband
		       `(member (bound-to ,segment)
				(segments ,candidate)))
		      (rubberband
		       `(if (typep (bound-to segment)
				   'geom-chain)
			    `(every #'(lambda (s)
					(member (bound-to s)
						(segments ,candidate)))
				    (segments segment))
			  `(member (bound-to ,segment)
				   (segments ,candidate))))))
		(segments candidate))
	,@(funcall continuation)))
  (:logic `( ,@(mapcar #'(lambda (s)
			   (etypecase s
			     (atomic-rubberband `(has-segment ,candidate ,s))
			     (rubberband `(has-rubberband ,candidate ,s))))
		       (segments candidate))
	       ,@(funcall continuation))))

;;;
;;;
;;;

(defmultidependency construct-intersection-point
    (:generator-nil
     `( (let ((first-bound-to (bound-to ,(first args)))
	      (second-bound-to (bound-to ,(second args))))
	  (etypecase first-bound-to
	    (geom-line 
	     (etypecase second-bound-to
	       (geom-line
		(multiple-value-bind (ix iy)
		    (calculate-intersection-point first-bound-to
						  second-bound-to)
		  (let ((candidate (p ix iy :affected-by-matrix-p nil)))
		    (with-binding (,candidate candidate) 
		      ,@(funcall continuation)))))
	       (geom-chain-or-polygon
		(dolist (segment (segments second-bound-to))
		  (multiple-value-bind (ix iy)
		      (calculate-intersection-point first-bound-to
						    segment)
		    (let ((candidate (p ix iy :affected-by-matrix-p nil)))
		      (with-binding (,candidate candidate) 
			,@(funcall continuation))))))))
	    (geom-chain-or-polygon
	     (etypecase second-bound-to		
	       (geom-line
		(dolist (segment (segments first-bound-to))
		  (multiple-value-bind (ix iy)
		      (calculate-intersection-point second-bound-to
						    segment)
		    (let ((candidate (p ix iy :affected-by-matrix-p nil)))
		      (with-binding (,candidate candidate) 
			,@(funcall continuation))))))
	       (geom-chain-or-polygon
		(dolist (segment1 (segments first-bound-to))
		  (dolist (segment2 (segments second-bound-to))
		    (multiple-value-bind (ix iy)
			(calculate-intersection-point segment1
						      segment2)
		      (let ((candidate (p ix iy :affected-by-matrix-p nil)))
			(with-binding (,candidate candidate) 
			  ,@(funcall continuation)))))))))))))

  (:generator-T
   `( (let ((first-bound-to (bound-to ,(first args)))
	    (second-bound-to (bound-to ,(second args))))
	(etypecase first-bound-to
	  (geom-line 
	   (etypecase second-bound-to
	     (geom-line
	      (multiple-value-bind (ix iy)
		  (calculate-intersection-point first-bound-to
						second-bound-to)
		(let ((candidate (get-point-from-spatial-index* ix iy)))
		  (with-binding (,candidate candidate) 
		    ,@(funcall continuation)))))
	     (geom-chain-or-polygon
	      (dolist (segment (segments second-bound-to))
		(multiple-value-bind (ix iy)
		    (calculate-intersection-point first-bound-to
						  segment)
		  (let ((candidate (get-point-from-spatial-index* ix iy)))
		    (with-binding (,candidate candidate) 
		      ,@(funcall continuation))))))))
	  (geom-chain-or-polygon
	   (etypecase second-bound-to		
	     (geom-line
	      (dolist (segment (segments first-bound-to))
		(multiple-value-bind (ix iy)
		    (calculate-intersection-point second-bound-to
						  segment)
		  (let ((candidate (get-point-from-spatial-index* ix iy)))
		    (with-binding (,candidate candidate) 
		      ,@(funcall continuation))))))
	     (geom-chain-or-polygon
	      (dolist (segment1 (segments first-bound-to))
		(dolist (segment2 (segments second-bound-to))
		  (multiple-value-bind (ix iy)
		      (calculate-intersection-point segment1
						    segment2)
		    (let ((candidate (get-point-from-spatial-index* ix iy)))
		      (with-binding (,candidate candidate) 
			,@(funcall continuation)))))))))))))
  (:logic `( (intersection-point-of ,candidate ,(first args) ,(second args))
	     ,@(funcall continuation))))

;;;
;;;
;;;

(defmethod get-segments-from-to ((obj geom-chain-or-polygon) (from geom-point) (to geom-point))
  (with-slots (point-list) obj
    (let* ((n (length point-list))
	   (point-list (append point-list point-list))
	   (pos1 (position from point-list :test #'point-=-p))
	   (pos2 (position to point-list :test #'point-=-p)))
      (when (and pos1 pos2)
	(when (< pos2 pos1)
	  (incf pos2 n))
	(let ((pos2 (1+ pos2)))
	  (if (and (= pos1 0)
		   (= pos2 (length point-list)))
	      (butlast (segments obj))
	    (let* ((points
		    (subseq point-list 
			    pos1 pos2))
		   (segments
		    (mapcar #'(lambda (p1 p2)
				(first
				 (intersection (append (p1-of p1)
						       (p2-of p1))
					       (append (p1-of p2)
						       (p2-of p2)))))
			    points
			    (rest points))))
	      segments)))))))
