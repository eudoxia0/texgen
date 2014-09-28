(in-package :cl-user)
(defpackage texgen-asd
  (:use :cl :asdf))
(in-package :texgen-asd)

(defsystem texgen
  :version "0.1"
  :author "Fernando Borretti"
  :license "MIT"
  :depends-on (:anaphora)
  :components ((:module "src"
                :components
                ((:file "texgen"))))
  :description "Generate TeX from Common Lisp code."
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md")
  :in-order-to ((test-op (test-op texgen-test))))
