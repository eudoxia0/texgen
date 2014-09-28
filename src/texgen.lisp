(in-package :cl-user)
(defpackage texgen
  (:use :cl :anaphora)
  (:export :tup))
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
