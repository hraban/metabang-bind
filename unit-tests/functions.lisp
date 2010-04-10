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
