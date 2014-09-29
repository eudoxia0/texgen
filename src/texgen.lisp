(in-package :cl-user)
(defpackage texgen
  (:use :cl :anaphora)
  (:shadow :+ :- :* :/ := :set)
  (:export :emit
           :tup))
(in-package :texgen)

;;; Aliases

(defparameter *aliases* (make-hash-table))

(defun lookup-alias (alias)
  (aif (gethash alias *aliases*)
       it
       (symbol-name alias)))

;;; Default aliases

(defun defalias (name value)
  (setf (gethash name *aliases*) value))

(defalias :alpha "α")

;;; Emit

(defun emit (val)
  (if (atom val)
      (typecase val
        (keyword
         (lookup-alias val))
        (symbol
         (symbol-name val))
        (string
         val)
        (number
         (write-to-string val)))
      (mapcar #'(lambda (elem) (emit elem)) val)))

;;; Utilities

(defun comma-list (values)
  (format nil "~{~A~#[~:;, ~]~}" values))

;;; Expressions

(defun tup (&rest values)
  (format nil "\\langle ~A \\rangle" (comma-list (emit values))))

(defun set (&rest values)
  (format nil "\\left\\{ ~A \\right\\}" (comma-list (emit values))))

(defun expr (op values)
  (let ((fmt (format nil "~~{~~A~~#[~~:; ~A ~~]~~}" op)))
    (format nil fmt (mapcar #'(lambda (v) (emit v)) values))))

(defun + (&rest values) (expr "+" values))
(defun * (&rest values) (expr "\\times" values))
(defun - (&rest values) (expr "-" values))

(defun / (numerator denominator)
  (format nil "\\frac{~A}{~A}" (emit numerator) (emit denominator)))

(defun = (left right)
  (format nil "~A = ~A" (emit left) (emit right)))
