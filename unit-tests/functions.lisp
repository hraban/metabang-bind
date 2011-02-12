(in-package #:metabang-bind-test)

(deftestsuite test-flet (metabang-bind-test)
  ())

(addtest (test-flet)
  basic-access
  (bind (((:flet doit (x))
	  (setf x (* 2 x)) 
	  (setf x (+ x 3))
	  x))
    (ensure-same (doit 1) 5)
    (ensure-same (doit 2) 7)))

(addtest (test-flet)
  declarations
  (bind (((:flet doit (x))
	  (declare (type fixnum x))
	  (setf x (* 2 x)) 
	  (setf x (+ x 3))
	  x))
    (ensure-same (doit 1) 5)
    (ensure-same (doit 2) 7)))

(deftestsuite test-labels (metabang-bind-test)
  ())

(addtest (test-labels)
  basic-access
  (bind (((:labels my-oddp (x))
	  (cond ((<= x 0) nil)
		((= x 1) t)
		(t (my-oddp (- x 2))))))
    (ensure (my-oddp 1))
    (ensure (my-oddp 7))
    (ensure-null (my-oddp 2))))
