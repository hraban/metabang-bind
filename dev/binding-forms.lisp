(in-package #:metabang.bind)

(defgeneric bind-generate-bindings (kind variable-form value-form
					 body declarations remaining-bindings)
  )


(defmethod bind-generate-bindings ((kind array) variable-form value-form
				   body declarations remaining-bindings)
  (let ((array-size (array-total-size variable-form))
	(gvalue (gensym "value")))
    `((let* ((,gvalue ,value-form)
	    ,@(loop for i below array-size
		 for var = (row-major-aref variable-form i)
		 unless (eq var nil) collect
		   `(,var (row-major-aref ,gvalue ,i))))
      ,@(bind-macro-helper remaining-bindings declarations body)))))	

(defmethod bind-generate-bindings ((kind symbol) variable-form value-form
				   body declarations remaining-bindings)
  (assert (not (keywordp kind))
	  nil
	  "Unable to understand binding specification ~s" kind)
  `((let (,@(if value-form
		`((,variable-form ,value-form))
		`(,variable-form)))
      ,(bind-filter-declarations declarations variable-form)
      ,@(bind-macro-helper remaining-bindings declarations body))))

(defmethod bind-generate-bindings ((kind cons) variable-form value-form
				   body declarations remaining-bindings)
  (bind-handle-destructuring variable-form value-form 
			     body declarations remaining-bindings))

(defun bind-handle-destructuring (variable-form value-form 
				  body declarations remaining-bindings)
  (multiple-value-bind (vars ignores)
      (bind-fix-nils-destructured variable-form)
    `((destructuring-bind ,vars ,value-form
	,@(when ignores `((declare (ignore ,@ignores))))
	,(bind-filter-declarations declarations variable-form)
	,@(bind-macro-helper 
	   remaining-bindings declarations body)))))

(defmethod bind-generate-bindings ((kind (eql :values)) variable-form value-form
				   body declarations remaining-bindings)
  (bind-handle-values variable-form value-form
		      body declarations remaining-bindings))

(defmethod bind-generate-bindings 
    ((kind (eql 'cl:values)) variable-form value-form
     body declarations remaining-bindings)
  (cond ((eq variable-form 'values)
	 (call-next-method))
	((and (consp value-form) *bind-treat-values-as-values*)
	 (simple-style-warning "The use of cl:values in bind is deprecated.
Please change to the unambiguous :values instead.")
	 (bind-handle-values variable-form value-form
			     body declarations remaining-bindings))
	(t
	 (bind-handle-destructuring (append (list kind) variable-form)
				    value-form 
				    body declarations remaining-bindings))))

(defun bind-handle-values (variable-form value-form
			   body declarations remaining-bindings)
  (multiple-value-bind (vars ignores)
      (bind-fix-nils variable-form)
    `((multiple-value-bind ,vars ,value-form
	,@(when ignores `((declare (ignore ,@ignores))))
	,(bind-filter-declarations declarations variable-form)
	,@(bind-macro-helper
	   remaining-bindings declarations body)))))


#+(or)
(bind (((:plist a (b _) (c _ 2) (dd d)) '(:b "B" :a "A" :d "D")))
  (list a b c dd))

#+(or)
(bind (((:plist- a (b _) (c _ 2) (dd d)) '(b "B" a "A" d "D")))
  (list a b c dd))

(defbinding-form ((:struct :structure))
  (let ((conc-name (first variables))
	(vars (rest variables)))
    (assert conc-name)
    (assert vars)
    `(let* ,(loop for var in vars collect
		 (let ((var-var (or (and (consp var) (first var))
				    var))
		       (var-conc (or (and (consp var) (second var))
				     var)))
		   `(,var-var (,(intern 
				 (format nil "~a~a" 
					 conc-name var-conc)) 
				,values)))))))

(defbinding-form (:assoc)
  `(let* ,(loop for spec in variables collect
	       (let* ((spec (if (consp spec) spec (list spec)))
		      (var-name (first spec))
		      var-key var-default)
		 (case (length spec)
		   (1 (setf var-key (first spec)))
		   (2 (setf var-key (second spec)))
		   (3 (setf var-key (second spec)
			    var-default (third spec)))
		   (t
		    (error "bad properly list variable specification: ~s" 
			   spec)))
		 (when (string= (symbol-name var-key) "_")
		   (setf var-key var-name))
		 `(,var-name (or (cdr (assoc ',var-key ,values)) 
				 ,@(when var-default `(,var-default))))))))

(defbinding-form ((:read-only-slots :slots-read-only))
  `(let* (,@(loop for var in variables collect
		 (let ((var-var (or (and (consp var) (first var))
				    var))
		       (var-slot (or (and (consp var) (second var))
				     var)))
		   `(,var-var (slot-value ,values ',var-slot)))))))

(defbinding-form (slots)
  `(with-slots 
	 (,@(loop for var in variables collect
		 (let ((var-var (or (and (consp var) (first var))
				    var))
		       (var-accessor (or (and (consp var) (second var))
					 var)))
		   `(,var-var ,var-accessor))))
       ,values))

(defbinding-form ((:read-only-accessors 
		   :accessors-read-only
		   :accessors-r/o))
  `(let* ,(loop for var in variables collect
	       (let ((var-var (or (and (consp var) (first var))
				  var))
		     (var-accessor (or (and (consp var) (second var))
				       var)))
		 `(,var-var (,var-accessor ,values))))))

(defbinding-form ((:accessors :writable-accessors))
  `(with-accessors 
	 (,@(loop for var in variables collect
		 (let ((var-var (or (and (consp var) (first var))
				    var))
		       (var-accessor (or (and (consp var) (second var))
					 var)))
		   `(,var-var ,var-accessor))))
       ,values))

(defbinding-form ((:plist :property-list :properties))
  (handle-plist variables values t))

(defbinding-form (:plist-)
  (handle-plist variables values nil))

(defun handle-plist (variables values form-keywords?)
  `(let* ,(loop for spec in variables collect
	       (let* ((spec (if (consp spec) spec (list spec)))
		      (var-name (first spec))
		      var-key var-default)
		 (case (length spec)
		   (1 (setf var-key (first spec)))
		   (2 (setf var-key (second spec)))
		   (3 (setf var-key (second spec)
			    var-default (third spec)))
		   (t
		    (error "bad properly list variable specification: ~s" 
			   spec)))
		 (when (string= (symbol-name var-key) "_")
		   (setf var-key var-name))
		 (when form-keywords?
		   (setf var-key (intern (symbol-name var-key) :keyword)))
		 `(,var-name (getf ,values
				   ,(if form-keywords? 
					var-key `',var-key)  
				   ,@(when var-default
					   `(,var-default))))))))
