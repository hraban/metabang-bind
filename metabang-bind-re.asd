(defpackage #:metabang.bind-system (:use #:cl #:asdf))
(in-package #:metabang.bind-system)

(defsystem metabang-bind-re
  :version "0.8.0"
  :author "Gary Warren King <gwking@metabang.com>"
  :licence "MIT License"    
  :description "Cl-ppcre support for bind"
  :components ((:module
		"dev"
		:components
		((:file "bind-cl-ppcre"))))
  :depends-on (cl-ppcre metabang-bind))
