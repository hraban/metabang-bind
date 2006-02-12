(in-package common-lisp-user)

(defpackage "METABANG-BIND-TEST-SYSTEM" (:use #:common-lisp #:asdf))
(in-package "METABANG-BIND-TEST-SYSTEM")

(defsystem METABANG-BIND-test
  :version "0.1"
  :author "Gary Warren King <gwking@metabang.com>"
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "MIT Style License"
  :description "Tests for METABANG-BIND"
  :components ((:module "unit-tests"
                        :components ((:file "package")
                                     (:file "test-bind" 
                                            :depends-on ("package")))))
  :depends-on (METABANG-BIND lift))