(in-package #:metabang.bind.developer)

(defmethod bind-generate-bindings ((kind (eql :re)) variable-form value-form)
  ;; (:re "re" vars)
  (bind (((regex &rest vars) variable-form)
	 (gok (gensym "ok"))
	 (gblock (gensym "block"))
	 ((:values vars ignores) (bind-fix-nils vars)))
    `(let* ((,gok nil))
	(block ,gblock
	  (flet ((doit (,@vars)
		   ,@(when ignores `((declare (ignore ,@ignores))))
		   ,metabang-bind::+decl-marker+
		   (return-from ,gblock ,metabang-bind::+code-marker+)))
	    (excl:re-let ,regex ,(first value-form)
		,(loop for var in vars for i from 1 collect
		      `(,var ,i))
	      (setf ,gok t)
	      (doit ,@vars))
	    (unless ,gok
	      (doit ,@(make-list (length vars) :initial-element nil))))))))

#+(or)
(bind (((:re "(\\w+)\\s+(\\w+)\\s+(\\d{1,2})\\.(\\d{1,2})\\.(\\d{4})"
	     fname lname date month year) "Frank Zappa 21.12.1940"))
  (list fname lname date month year))

(defmethod bind-generate-bindings ((kind (eql :regex)) variable-form value-form)
  ;; (:regex "re" vars)
  (bind (((regex &rest vars) variable-form))
    `(excl:re-let ,regex ,(first value-form)
	 ,(loop for var in vars for i from 1 collect
	       `(,var ,i)))))
