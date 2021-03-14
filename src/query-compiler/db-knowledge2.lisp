;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: QUERY-COMPILER; Base: 10 -*-

(in-package query-compiler)

;;; 
;;; Ab hier ist nun Wissen ueber die Datenstrukturen des Datenbestandes notwendig: welche Objekte sind implizit, welche explizit; 
;;; z.B.: gibt es im Datenbestand EXPLIZIT vorhandene (modellierte) Polygone, oder muessen Polygone durch Aggregation gebildet 
;;; werden, sind also implizit?
;;; matches-with-database-object-p <=> Objekt kann DIREKT (ohne weitere Unterteilungen etc.) aus der DB instantiiert werden, da
;;; das Objekte explizit im Datenbestand vorliegt!
;;;

(defmethod matches-with-database-object-p ((object query-object))
  (member (status object) '(db db-component))) ; fuer Punkte und Segmente !!!

(defmethod matches-with-database-object-p ((object rubberband))
  (eq (status object) 'db))		; ganze Kettenobjekte (stets primaer) oder (primaere) Linien(segmente).
					; DB-C-Rubberbands: Teilketten von Polygonen oder Ketten 
					; sind nicht explizit im Datenbestand vorhanden, 
					; sondern muessen zur Suchzeit durch segmentweise Aggregation gebildet werden. 

(defmethod matches-with-database-object-p ((object chain))
  (eq (status object) 'db))		; echte db-component chains sind nicht in 
					; der DB als Ketten ausgezeichnet - 
					; nur ganze Ketten sind in der DB

(defmethod matches-with-database-object-p ((object polygon))
  (member (status object) '(db db-component))) ; jedes Polygon im Datenbestand ist primaer (es gibt keine Komponentenpolygone); 
					; in diesem Fall ist DB = DB-C fuer Polygone (da DB => DB-C und Komponentenpolygone
					; keine besondere Bedeutung haben wie Komponenten-Ketten)

(defmethod matches-with-database-object-p ((object visco-object))
  nil)

;;;
;;; stored-relation-p <=> keine aufwendigen geometrischen Berechnung notwendig,
;;; da im Datenmodell explizit vermerkt
;;;

(defmethod stored-relation-p ((cs binary-constraint))
  (with-slots (1st-arg 2nd-arg) cs
    (and (matches-with-database-object-p 2nd-arg)
	 (matches-with-database-object-p 1st-arg))))

(defmethod stored-relation-p ((cs intersects))
  (call-next-method))

(defmethod stored-relation-p ((cs disjoint))
  nil)

(defmethod stored-relation-p ((cs angle-between))
  nil)

(defmethod stored-relation-p ((cs inside))
  (and (matches-with-database-object-p (1st-arg cs))
       (typecase (2nd-arg cs)
	 ((or inner-enclosure outer-enclosure)
	  (matches-with-database-object-p (arg-object (2nd-arg cs))))
	 (otherwise nil))))

(defmethod stored-relation-p ((cs contains))
  (and (matches-with-database-object-p (2nd-arg cs))
       (typecase (1st-arg cs)
	 ((or inner-enclosure outer-enclosure)
	  (matches-with-database-object-p (arg-object (1st-arg cs))))
	 (otherwise nil))))

