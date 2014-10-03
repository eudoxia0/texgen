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

(defalias :alpha "\\alpha")
(defalias :theta "\\theta")
(defalias :pi "\\pi")
(defalias :inf "\\infty")

;;; Operations

(defparameter *ops* (make-hash-table :test #'equalp))

(defmacro defop (name args &rest body)
  `(setf (gethash ,(symbol-name name) *ops*)
         (lambda ,args ,@body)))

;;; Emit

(defun emit-expr (fn args)
  (aif (gethash (symbol-name fn) *ops*)
       (apply it args)
       ""))

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

;;; Aggregate notation

(defop tup (&rest values)
  (format nil "\\langle ~A \\rangle" (comma-list (emit values))))

(defop set (&rest values)
  (format nil "\\left\\{ ~A \\right\\}" (comma-list (emit values))))

(defop p (content)
  (format nil "\\left( ~A \\right)"
          (emit content)))

;;; Operations

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

;;; Comparison operators

(defop < (left right) (expr "\\lt" (list left right)))
(defop <= (left right) (expr "\\leq" (list left right)))
(defop = (left right) (expr "=" (list left right)))
(defop > (left right) (expr "\\gt" (list left right)))
(defop >= (left right) (expr "\\geq" (list left right)))

;;; Aggregate operations

(defop sum (lower upper body)
  (format nil "\\sum_{~A}^{~A} ~A" (emit lower) (emit upper) (emit body)))

;;; Sub and superscript

(defop sub (left right) (expr "_" (list left right)))
(defop sup (left right) (expr "^" (list left right)))

;;; Common functions

(defop abs (n) (format nil "|~A|" (emit n)))
(defop sqrt (n) (format nil "\\sqrt{~A}" (emit n)))
(defop root (p n) (format nil "\\sqrt[~A]{~A}" (emit p) (emit n)))

;;; Complex numbers

(defop cpol (p alpha)
  "Polar notation"
  (format nil "~A \\vert \\underline{~A}" (emit p) (emit alpha)))

;;; Trigonometry

(defun fun (op args)
  (let ((fmt "\\~A(~{~A~#[~:;, ~]~})"))
    (format nil fmt op (mapcar #'(lambda (v) (emit v)) args))))

(defop sin (alpha) (fun "sin" (list alpha)))
(defop cos (alpha) (fun "cos" (list alpha)))
(defop tan (alpha) (fun "tan" (list alpha)))
