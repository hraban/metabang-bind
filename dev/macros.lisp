(in-package #:metabang.bind)

(defmacro defbinding-form ((name/s &optional docstring) &body body)
  (declare (ignorable docstring))
  (let ((multiple-names? (consp name/s))
	(main-method-name nil)
	#+(or)
	(gignores (gensym "ignores")))    
    (cond (multiple-names?
	   (setf main-method-name (gensym "binding-generator"))
	   )
	  (t
	   (setf main-method-name 'bind-generate-bindings)
	   ))
    (flet ((form-keyword (name)
	     (intern (symbol-name name)
		     (load-time-value (find-package :keyword)))))
      `(progn
	 ,@(when multiple-names?
		 (loop for name in name/s collect
		      `(defmethod bind-generate-bindings 
			   ((kind (eql ,(form-keyword name)))
			    variable-form value-form body declarations 
			    remaining-bindings)
			 (,main-method-name 
			  variable-form value-form body declarations 
			  remaining-bindings))))
	 (defmethod ,main-method-name 
	     (,@(unless multiple-names?
			`((kind (eql ,(form-keyword name/s)))))
	      variable-form value-form body declarations remaining-bindings)
	   `((let ((values ,value-form))
	       (,@,(if (symbolp (first body))
		       `(,(first body) variable-form 'values)
		       `(funcall (lambda (variables values) ,@body)
				 variable-form 'values))
					;		 ,@(when ,gignores `((declare (ignore ,@gignores))))
		   ,(bind-filter-declarations declarations variable-form)
		   ,@(bind-macro-helper 
		      remaining-bindings declarations body)))))))))

