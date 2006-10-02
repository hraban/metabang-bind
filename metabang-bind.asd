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
  (print ";; Warning: The bind system requires ASDF-system-connections. See 
http://www.cliki.net/asdf-system-connections for details and download
instructions."))

(when (find-system 'asdf-system-connections nil)
  (asdf:operate 'asdf:load-op 'asdf-system-connections))

(defsystem metabang-bind
  :version "0.2"
  :author "Gary Warren King <gwking@metabang.com>"
  :licence "MIT License"    
  :description "Bind is a macro that generalizes multiple-value-bind, let, let* and destructuring-bind."
  :components ((:module "dev"
	            :components ((:file "bind")))
               (:module "website"
                        :components ((:module "source"
                                              :components ((:static-file "index.lml"))))))
  :depends-on (#+asdf-system-connections asdf-system-connections))

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


