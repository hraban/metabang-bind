(defpackage #:metabang.bind-system (:use #:cl #:asdf))
(in-package #:metabang.bind-system)

(when (find-system 'asdf-system-connections nil)
  (asdf:operate 'asdf:load-op 'asdf-system-connections))

(defsystem metabang-bind
  :version "0.6.1"
  :author "Gary Warren King <gwking@metabang.com>"
  :licence "MIT License"    
  :description "Bind is a macro that generalizes multiple-value-bind, let, let* and destructuring-bind."
  :components ((:module "dev"
	            :components ((:file "bind"))))
  :depends-on (#+asdf-system-connections :asdf-system-connections)
  :in-order-to ((test-op (load-op metabang-bind-test)))
  :perform (test-op :after (op c)
		    (funcall
		      (intern (symbol-name '#:run-tests) :lift)
		      :config :generic))
  :depends-on ()) 

(defmethod operation-done-p 
           ((o test-op) (c (eql (find-system 'metabang-bind))))
  (values nil))

#+asdf-system-connections 
(asdf:defsystem-connection bind-and-metatilities
  :requires (metabang-bind metatilities-base)
  :perform (load-op :after (op c)
                    (use-package (find-package 'metabang.bind) 
                                 (find-package 'metatilities))
                    (funcall (intern 
                              (symbol-name :export-exported-symbols)
                              'metatilities)
                             'bind 'metatilities))) 


