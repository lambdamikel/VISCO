;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: GUI; Base: 10 -*-

(in-package gui)

;;;
;;;
;;;

(defparameter *tick-counter* 0)

(defparameter *history-name-counter* 0)

;;;
;;;
;;;

(defparameter *visco-frame* nil)
(defparameter *visco-buttons-frame* nil)
(defparameter *visco-inspector-frame* nil)

(defparameter *dummies* nil)

(defparameter *searching-active* nil)

(defparameter *buttons* nil)

(defparameter *draw-thumbnails* t)

(defparameter *permutations* nil)

(defparameter *focus-box* t)

(defparameter *visualized-relations* nil)

(defparameter *grid-resolution* 10)

(defparameter *opaque-enclosures-p* t)

(defparameter *ignore-disjoint-and-intersects-component-relations-p* nil)

(defparameter *ignore-inactive-flag* nil)

(defparameter *sceleton-view* nil)

;;;
;;;
;;;

(defparameter *object-buttons* nil)

(defparameter *creating-active* nil)

(defparameter *primary-mode* nil)

(defparameter *segment-mode* nil)

(defparameter *point-mode* nil)

(defvar *button-transparency* )
(defvar *button-origin*)
(defvar *button-nail*)
(defvar *button-marble*)
(defvar *button-atomic-rubberband*)
(defvar *button-atomic-<=-rubberband* )
(defvar *button-atomic->=-rubberband*)
(defvar *button-rubberband*)
(defvar *button-beam*)
(defvar *button-chain* )
(defvar *button-polygon* )
(defvar *button-drawn-enclosure*)	

;;;
;;;
;;;

(defparameter *operator-buttons* nil)

(defparameter *operator-mode* nil)

(defvar *button-no-operator*)
(defvar *button-create-centroid*)
(defvar *button-create-intersection-point*)
(defvar *button-create-inverse-drawn-enclosure*)
(defvar *button-create-inner-enclosure*)
(defvar *button-create-outer-enclosure*)
(defvar *button-create-epsilon-enclosure*)
(defvar *button-recreate*)
(defvar *button-set-semantics*)
(defvar *button-set-at-most-constraint*)
(defvar *button-set-orientation-constraint*)
(defvar *button-create-orientation-constraint-mark*)
(defvar *button-create-orientation-constraint-intervall*)
(defvar *button-create-relative-orientation-constraint*)
(defvar *button-set-transparency-properties*)

;;;
;;;
;;;

(defparameter *point-status* nil)

(defparameter *segment-status* nil)

(defparameter *chain-or-polygon-status* nil)

(defparameter *status-buttons* nil)

(defvar *button-point-status-db*)
(defvar *button-segment-status-db*)
(defvar *button-chain-or-polygon-status-db*)
(defvar *button-point-status-db-component*)
(defvar *button-segment-status-db-component*)
(defvar *button-chain-or-polygon-status-db-component*)
(defvar *button-point-status-universe*)
(defvar *button-segment-status-universe*)
(defvar *button-chain-or-polygon-status-universe*)
(defvar *point-buttons*)
(defvar *segment-buttons*)
(defvar *chain-or-polygon-buttons*)

