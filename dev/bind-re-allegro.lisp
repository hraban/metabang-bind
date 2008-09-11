(in-package #:metabang.bind.developer)

(defmethod bind-generate-bindings
    ((kind (eql :re)) variable-form value-form
     body declarations remaining-bindings)
  ;; (:re "re" vars)
  (bind (((regex &rest vars) variable-form)
	 ((:values vars ignores) (bind-fix-nils vars)))
    `((excl:re-let ,regex ,value-form
	  ,(loop for var in vars for i from 1 collect
		`(,var ,i))
	(declare (ignore ,@ignores))
	,(bind-filter-declarations
	  declarations variable-form)
	,@(bind-macro-helper
	   remaining-bindings declarations body)))))

#+(or)
(bind (((:re "(\\w+)\\s+(\\w+)\\s+(\\d{1,2})\\.(\\d{1,2})\\.(\\d{4})"
	     fname lname date month year) "Frank Zappa 21.12.1940"))
  (list fname lname date month year))
