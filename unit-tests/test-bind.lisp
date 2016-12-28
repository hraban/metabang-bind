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
  simple-list-with-_
  (multiple-value-bind (vars ignores)
                       (bind-fix-nils-destructured '(a _ c))
    (ensure-same (first vars) 'a)
    (ensure-same (third vars) 'c)
    (ensure-same (second vars) (first ignores))))

(addtest (test-bind-fix-nils-destructured)
  simple-list-with-_-2
  (multiple-value-bind (vars ignores)
                       (bind-fix-nils-destructured '(a _ c _ _))
    (ensure-same (first vars) 'a)
    (ensure-same (third vars) 'c)
    (ensure (member (second vars) ignores))
    (ensure (member (fourth vars) ignores))
    (ensure (member (fifth vars) ignores))))

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

(addtest (test-bind-fix-nils-destructured)
  keyword-list-with-nil-default
  (multiple-value-bind (vars ignores)
                       (bind-fix-nils-destructured '(a b &key (c nil c?)))
    (ensure-same (length ignores) 0)
    (ensure-same (subseq vars 0 3) '(a b &key) :test #'equal)
    (ensure-same (fourth vars) '(c nil c?) :test 'equal)))

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

(deftestsuite test-bind-style-warnings (metabang-bind-test)
  ())

(addtest (test-bind-style-warnings)
  missing-value-1
  (ensure-condition metabang-bind:bind-missing-value-form-warning
    (macroexpand '(bind (((:values a b))) (list a b)))))

(addtest (test-bind-style-warnings)
  missing-value-2
  (ensure-no-warning
    (macroexpand '(bind (((:values a b) (foo))) (list a b)))))

(addtest (test-bind-style-warnings)
  missing-value-3
  (ensure-no-warning
    (macroexpand '(bind (a) (list a)))))

(addtest (test-bind-style-warnings)
  missing-value-4
  (ensure-no-warning
    (macroexpand '(bind ((a nil)) (list a)))))

(addtest (test-bind-style-warnings)
  two-many-value-forms-error
  (ensure-cases (form)
      '((a b c)
	((:values a b) 1 2 3))
    (ensure-condition metabang-bind:bind-too-many-value-forms-error
      (macroexpand `(bind (,form) (list a))))))

(addtest (test-bind-style-warnings)
  two-many-value-forms-warnings-with-flet
  (ensure-no-warning
    (macroexpand `(bind (((:flet x (a)) (setf a (* 2 a)) (list a))) (x 2)))))

;;;;

(deftestsuite test-ignore-underscores (metabang-bind-test)
  ()
  (:equality-test (lambda (a b)
		    (equalp (remove-gensyms a) (remove-gensyms b)))))

(addtest (test-ignore-underscores)
  test-simple-destructuring
  (ensure-same
   (macroexpand '(bind (((nil a b) (foo)))
		  (list a b)))
   (macroexpand '(bind (((_ a b) (foo)))
		  (list a b)))))

(addtest (test-ignore-underscores)
  test-multiple-values
  (ensure-same
   (macroexpand '(bind (((:values a nil b) (foo)))
		  (list a b)))
   (macroexpand '(bind (((:values a _ b) (foo)))
		  (list a b)))))

(addtest (test-ignore-underscores)
  test-array
  (ensure-same
   (macroexpand '(bind ((#(a nil b) (foo)))
		  (list a b)))
   (macroexpand '(bind ((#(a _ b) (foo)))
		  (list a b)))))

;;;

(deftestsuite test-for-unused-declarations (metabang-bind-test)
  ())

(addtest (test-for-unused-declarations)
  test-error-1
  (let ((bind:*unused-declarations-behavior* :error))
    (ensure-condition bind::bind-unused-declarations-error
      (eval '(bind:bind ((a 2) (b 3))
	      (declare (type fixnum a b c) (optimize (speed 3)))
	      a b)))))

(addtest (test-for-unused-declarations)
  test-error-2
  (let ((bind:*unused-declarations-behavior* :error))
    (ensure-condition bind:bind-unused-declarations-error
      (eval '(bind:bind (((:values _ b ) (values 1 2)))
	      (declare (type fixnum b) (ignorable b)
	       (simple-vector d) (optimize (speed 3)))
	      b)))))

(addtest (test-for-unused-declarations)
  test-warning-1
  (let ((bind:*unused-declarations-behavior* :warn))
    (ensure-condition bind::bind-unused-declarations-warning
      (eval '(bind:bind ((a 2) (b 3))
	      (declare (type fixnum a b c) (optimize (speed 3)))
	      a b)))))

(addtest (test-for-unused-declarations)
  test-warning-2
  (let ((bind:*unused-declarations-behavior* :warn))
    (ensure-condition bind::bind-unused-declarations-warning
      (eval '(bind:bind (((:values _ b ) (values 1 2)))
	      (declare (type fixnum b) (ignorable b)
	       (simple-vector d) (optimize (speed 3)))
	      b)))))

(addtest (test-for-unused-declarations)
  test-no-warning-1
  (let ((bind:*unused-declarations-behavior* nil))
    (ensure-no-warning
      (eval '(bind:bind (((:values _ b ) (values 1 2)))
	      (declare (type fixnum b) (ignorable b)
	       (simple-vector d) (optimize (speed 3)))
	      b)))))

;;;

#|

(defun x (a b)
  (declare (fixnum a b))
  (+ a b))

(defun x (c)
  (bind (((:structure/rw c- a b) c))
    (declare (fixnum a b))
    (declare (optimize (speed 3) (safety 0)))
    (+ a b)))

(disassemble 'x)

(bind (((:structure/rw foo- a b c) (bar)))
  (declare (type fixnum a) (double b))
  (declare (optimize (speed 3)))
  )

|#
