(in-package #:metabang-bind-test)

(deftestsuite metabang-bind-test () ())

(deftestsuite test-bind-fix-nils-destructured (metabang-bind-test)
  ())

(addtest (test-bind-fix-nils-destructured)
  simple-list
  (ensure-same (bind-fix-nils-destructured '(a b c)) (values '(a b c) nil)
	       :test #'equal))
  
(addtest (test-bind-fix-nils-destructured)
  simple-list-with-nil
  (multiple-value-bind (vars ignores)
                       (bind-fix-nils-destructured '(a nil c)) 
    (ensure-same (first vars) 'a)
    (ensure-same (third vars) 'c)
    (ensure-same (second vars) (first ignores))))

(addtest (test-bind-fix-nils-destructured)
  dotted-list
  (multiple-value-bind (vars ignores)
                       (bind-fix-nils-destructured '(a . b)) 
    (ensure-same (car vars) 'a)
    (ensure-same (cdr vars) 'b)
    (ensure-same ignores nil)))

(addtest (test-bind-fix-nils-destructured)
  dotted-list-with-nil-1
  (multiple-value-bind (vars ignores)
                       (bind-fix-nils-destructured '(nil . b)) 
    (ensure-same (car vars) (first ignores))
    (ensure-same (cdr vars) 'b)
    (ensure-same (length ignores) 1)))

(addtest (test-bind-fix-nils-destructured)
  keyword-list
  (multiple-value-bind (vars ignores)
                       (bind-fix-nils-destructured '(a b &key (c 1) d (e x y))) 
    (ensure-same (length vars) 6)
    (ensure-same (length ignores) 0)
    (ensure-same vars '(a b &key (c 1) d (e x y)) :test #'equal)))

(addtest (test-bind-fix-nils-destructured)
  keyword-list-with-nil-non-keyword
  (multiple-value-bind (vars ignores)
                       (bind-fix-nils-destructured '(nil b &key (c 1) d (e x y))) 
    (ensure-same (length ignores) 1)
    (ensure-same (rest vars) '(b &key (c 1) d (e x y)) :test #'equal)
    (ensure-same (first vars) (first ignores))))

(addtest (test-bind-fix-nils-destructured)
  keyword-list-with-nil-keyword
  (multiple-value-bind (vars ignores)
                       (bind-fix-nils-destructured '(a b &key (c 1) nil (e x y))) 
    (ensure-same (length ignores) 1)
    (ensure-same (subseq vars 0 3) '(a b &key) :test #'equal)
    (ensure-same (fifth vars) (first ignores))
    (ensure-same (fourth vars) '(c 1) :test 'equal)))

#+Ignore
;;?? not yet
(addtest (test-bind-fix-nils-destructured)
  keyword-list-with-bad-nil-keyword-syntax
  (ensure-condition 'bind-keyword/optional-nil-with-default-error
    (bind-fix-nils-destructured '(a b &key (nil 1) d (e x y)))))

(addtest (test-bind-fix-nils-destructured)
  keyword-list-with-allow-other-keys
  (multiple-value-bind (vars ignores)
                       (bind-fix-nils-destructured '(a b &key (c 1) d (e x y)
                                                     &allow-other-keys)) 
    (ensure-same (length ignores) 0)
    (ensure-same vars '(a b &key (c 1) d (e x y)
                        &allow-other-keys) :test #'equal)))

;;;;

(deftestsuite test-treat-values-as-values (metabang-bind-test)
  ())

(deftestsuite test-treat-values-as-values-true (test-treat-values-as-values)
  ()
  (:dynamic-variables 
   (*bind-treat-values-as-values* t)))

(addtest (test-treat-values-as-values-true)
  generate-warning
  (ensure-warning 
    (macroexpand '(bind (((values a b) (foo)))
		   (list a b)))))

(addtest (test-treat-values-as-values-true)
  generate-no-warning-on-simple-binding
  (ensure-no-warning 
    (macroexpand '(bind ((values 42))
		   (list values)))))

(addtest (test-treat-values-as-values-true)
  generate-no-warning-on-simple-binding-works
  (ensure-same
   (eval '(bind ((values 42))
	   (list values)))
   '(42)
   :test 'equal))

(addtest (test-treat-values-as-values-true)
  generate-destructuring-if-atom
  (ensure-same 
   (eval '(let ((foo (list 0 1 2)))
	   (bind (((values a b) foo))
	     (list values a b))))
   (list 0 1 2) :test 'equal))

(addtest (test-treat-values-as-values-true)
  generate-values-if-cons
  (ensure-same 
   (eval '(bind (((values a b) (values 1 2)))
	     (list a b)))
   (list 1 2) :test 'equal))

(deftestsuite test-treat-values-as-values-false (test-treat-values-as-values)
  ()
  (:dynamic-variables 
   (*bind-treat-values-as-values* nil)))

(addtest (test-treat-values-as-values-false)
  generate-no-warning
  (handler-case
      (macroexpand '(bind (((values a b) (foo)))
		     (list a b)))
    (warning (c) (declare (ignore c))
	     (ensure nil))))

(addtest (test-treat-values-as-values-false)
  generate-destructuring-if-cons
  (ensure-same 
   (eval '(bind (((values a b) (list 0 1 2)))
	     (list values a b)))
   (list 0 1 2) :test 'equal))
