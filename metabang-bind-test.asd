(in-package common-lisp-user)

(defpackage #:metabang-bind-test-system
  (:use #:common-lisp #:asdf))
(in-package #:metabang-bind-test-system)

(defsystem metabang-bind-test
  :version "0.1"
  :author "Gary Warren King <gwking@metabang.com>"
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "MIT Style License"
  :description "Tests for metabang-bind"
  :components ((:module "unit-tests"
                        :components ((:file "package")
                                     (:file "test-bind" 
                                            :depends-on ("package"))
				     (:file "structures"
					    :depends-on ("test-bind"))
				     (:file "classes"
					    :depends-on ("test-bind"))
				     (:file "plists"
					    :depends-on ("test-bind")))))
  :depends-on (:metabang-bind :lift))