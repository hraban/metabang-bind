(in-package common-lisp-user)

(defpackage "METABANG-BIND-TEST"
  (:use "COMMON-LISP" "LIFT" "METABANG-BIND")
  (:import-from "METABANG-BIND"
                #:bind-fix-nils-destructured))