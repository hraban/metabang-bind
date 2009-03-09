(in-package #:common-lisp-user)

(defpackage #:metabang.bind
    (:use #:common-lisp)
    (:nicknames #:bind #:metabang-bind)
    (:intern 
     #:bind-generate-bindings
     #:bind-filter-declarations
     #:bind-macro-helper
     #:bind-fix-nils)
    (:export 
     #:bind
     #:fluid-bind

     #:binding-forms
     #:binding-form-synonyms
     #:binding-form-groups
     #:binding-form-docstring

     #:*bind-all-declarations*
     #:*bind-non-var-declarations*
     #:*bind-lambda-list-markers*

     ;; this will be removed ... someday
     #:*bind-treat-values-as-values*

     #:bind-error
     #:bind-keyword/optional-nil-with-default-error))

(defpackage #:metabang.bind.developer
    (:use #:common-lisp #:metabang-bind)
    (:import-from #:metabang-bind
		  #:bind-generate-bindings
		  #:bind-filter-declarations
		  #:bind-macro-helper
		  #:bind-fix-nils)
    (:export 
     #:bind-generate-bindings
     #:bind-filter-declarations
     #:bind-macro-helper
     #:bind-fix-nils))

