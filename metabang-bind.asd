(defpackage #:metabang.bind-system (:use #:cl #:asdf))
(in-package #:metabang.bind-system)

;; try hard
(unless (find-system 'asdf-system-connections nil)
 (when (find-package 'asdf-install)
   (funcall (intern (symbol-name :install) :asdf-install)
   	     'asdf-system-connections)))
;; give up with a useful (?) error message
(unless (find-system 'asdf-system-connections nil)
  (terpri)
  (format t "~&;; Warning: The bind system requires ASDF-system-connections. See 
http://www.cliki.net/asdf-system-connections for details and download
instructions."))

(when (find-system 'asdf-system-connections nil)
  (asdf:operate 'asdf:load-op 'asdf-system-connections))

(defsystem metabang-bind
  :version "0.2.2"
  :author "Gary Warren King <gwking@metabang.com>"
  :licence "MIT License"    
  :description "Bind is a macro that generalizes multiple-value-bind, let, let* and destructuring-bind."
  :components ((:module "dev"
	            :components ((:file "bind")))
               (:module "website"
                        :components ((:module "source"
                                              :components ((:static-file "index.lml"))))))
  :in-order-to ((test-op (load-op metabang-bind-test)))
  :perform (test-op :after (op c)
                    (describe 
		     (funcall (intern (symbol-name '#:run-tests) :lift) 
			      :suite '#:metabang-bind-test)))
  :depends-on (#+asdf-system-connections asdf-system-connections))

#+asdf-system-connections 
(defsystem-connection bind-and-metatilities
  :requires (metabang-bind metatilities-base)
  :perform (load-op :after (op c)
                    (use-package (find-package '#:metabang.bind)
                                 (find-package '#:metatilities))
                    (eval (let ((*package* (find-package :common-lisp)))
                            (read-from-string
                             "(progn
                                (metatilities:export-exported-symbols '#:bind '#:metatilities)
                                (when (eq metabang.bind:*defclass-macro-name-for-dynamic-context* 'cl:defclass)
                                  (setf metabang.bind:*defclass-macro-name-for-dynamic-context* 'metatilities:defclass*)))")))))

#+asdf-system-connections 
(defsystem-connection bind-and-metatilities
  :requires (metabang-bind defclass-star)
  :perform (load-op :after (op c)
                    (eval (let ((*package* (find-package :common-lisp)))
                            (read-from-string
                             "(progn
                                (when (member metabang.bind:*defclass-macro-name-for-dynamic-context* '(defclass metatilities:defclass*))
                                  (setf metabang.bind:*defclass-macro-name-for-dynamic-context* 'defclass-star:defclass*)))")))))
