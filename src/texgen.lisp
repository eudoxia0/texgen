(in-package :cl-user)
(defpackage texgen
  (:use :cl :anaphora)
  (:export :emit))
(in-package :texgen)

;;; Aliases

(defparameter *aliases* (make-hash-table :test #'equalp))

(defun lookup-alias (alias)
  (gethash (symbol-name alias) *aliases*))

;;; Default aliases

(defun defalias (name value)
  (setf (gethash (symbol-name name) *aliases*) value))

(defalias :alpha "α")
(defalias :inf "\\infty")

;;; Operations

(defparameter *ops* (make-hash-table :test #'equalp))

(defmacro defop (name args &rest body)
  `(setf (gethash ,(symbol-name name) *ops*)
         (lambda ,args ,@body)))

;;; Emit

(defun emit-expr (fn args)
  (apply (gethash (symbol-name fn) *ops*) args))

(defun emit (val)
  (typecase val
    (keyword
     (lookup-alias val))
    (symbol
     (symbol-name val))
    (string
     val)
    (number
     (write-to-string val))
    (list
     (emit-expr (first val) (rest val)))))

;;; Utilities

(defun comma-list (values)
  (format nil "~{~A~#[~:;, ~]~}" values))

;;; Operations

(defop tup (&rest values)
  (format nil "\\langle ~A \\rangle" (comma-list (emit values))))

(defop tup (&rest values)
  (format nil "\\langle ~A \\rangle" (comma-list (emit values))))

(defop set (&rest values)
  (format nil "\\left\\{ ~A \\right\\}" (comma-list (emit values))))

(defun expr (op values)
  (let ((fmt (format nil "~~{~~A~~#[~~:; ~A ~~]~~}" op)))
    (format nil fmt (mapcar #'(lambda (v) (emit v)) values))))

(defop + (&rest values) (expr "+" values))
(defop * (&rest values) (expr "\\times" values))
(defop - (&rest values) (expr "-" values))

(defop / (numerator denominator)
  (format nil "\\frac{~A}{~A}" (emit numerator) (emit denominator)))

(defop ^ (base exponent)
  (format nil "~A^{~A}" (emit base) (emit exponent)))

(defop = (left right)
  (format nil "~A = ~A" (emit left) (emit right)))

(defop sum (lower upper body)
  (format nil "\\sum_{~A}^{~A} ~A" (emit lower) (emit upper) (emit body)))
