(in-package #:metabang-bind-test)

(defclass metabang-bind-class-1 ()
  ((a :initarg :a :accessor a)
   (b :initarg :b :accessor b) 
   (c :initarg :c :accessor c)))

(defclass metabang-bind-class-2 (metabang-bind-class-1)
  ((d :initarg :d :accessor the-d)
   (e :initarg :e :accessor e)))

(deftestsuite test-classes (metabang-bind-test)
  ())

(addtest (test-classes)
  basic-slots
  (ensure-same
   (bind (((:slots a c)
	   (make-instance 'metabang-bind-class-1 :a 1 :b 2 :c 3)))
     (list a c))
   '(1 3) :test 'equal))

(addtest (test-classes)
  slots-new-variable-names
  (ensure-same
   (bind (((:slots a (my-c c) (the-b b))
	   (make-instance 'metabang-bind-class-1 :a 1 :b 2 :c 3)))
     (list a the-b my-c))
   '(1 2 3) :test 'equal))

(addtest (test-classes)
  basic-accessors
  (ensure-same
   (bind (((:accessors a c e)
	   (make-instance 'metabang-bind-class-2 :a 1 :b 2 :c 3 :d 4 :e 5)))
     (list e c a))
   '(5 3 1) :test 'equal))

(addtest (test-classes)
  accessors-new-variable-names
  (ensure-same
   (bind (((:accessors (my-a a) (my-c c) (d the-d))
	   (make-instance 'metabang-bind-class-2 :a 1 :b 2 :c 3 :d 4 :e 5)))
     (list d my-c my-a))
   '(4 3 1) :test 'equal))


