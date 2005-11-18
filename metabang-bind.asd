#| simple-header

$Id: bind.lisp,v 1.5 2004/08/30 16:57:28 gwking Exp $

http://www.opensource.org/licenses/mit-license.php

Copyright (c) 2004-2005 Gary Warren King, metabang.com

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
Author: Gary King

DISCUSSION

|#

(defpackage "METABANG.BIND-SYSTEM" (:use #:cl #:asdf))
(in-package "METABANG.BIND-SYSTEM")

(asdf:operate 'asdf:load-op 'asdf-system-connections)

(defsystem metabang-bind
  :version "0.2"
  :author "Gary Warren King <gwking@metabang.com>"
  :licence "MIT License"    
  :description "Bind is a macro that generalizes multiple-value-bind, let, let* and destructuring-bind."
  :components ((:module "dev"
	            :components ((:file "bind")))))

;;; ---------------------------------------------------------------------------

(asdf:defsystem-connection bind-and-metatilities
  :requires (metabang-bind metatilities-base)
  :perform (load-op :after (op c)
                    (use-package (find-package "METABANG.BIND") 
                                 (find-package "METATILITIES")))) 


