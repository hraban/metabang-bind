(in-package #:metabang-bind-test)

(defstruct (metabang-bind-test-1)
  a
  b 
  c)

(defstruct (metabang-bind-test-2 (:conc-name bind-test-))
  d
  e)

(deftestsuite test-structures (metabang-bind-test)
  ())

(addtest (test-structures)
  basic-access
  (ensure-same
   (bind (((:struct metabang-bind-test-1- a c)
	   (make-metabang-bind-test-1 :a 1 :b 2 :c 3)))
     (list a c))
   '(1 3) :test 'equal))

(addtest (test-structures)
  changed-variable-name
  (ensure-same
   (bind (((:struct metabang-bind-test-1- (my-a a) c)
	   (make-metabang-bind-test-1 :a 1 :b 2 :c 3)))
     (list c my-a))
   '(3 1) :test 'equal))

(addtest (test-structures)
  changed-variable-name-2
  (ensure-same
   (bind (((:structure metabang-bind-test-1- (my-a a) c)
	   (make-metabang-bind-test-1 :a 1 :b 2 :c 3)))
     (list c my-a))
   '(3 1) :test 'equal))
