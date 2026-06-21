(in-package #:metabang.bind)

#|

use

(defmethod documentation (object doc-type)
  body...)

instead

(documentation :plist 'binding-form)

|#

(defmethod documentation (what (doc-type (eql 'metabang.bind:binding-form)))
  (binding-form-docstring what))

(defun binding-form-docstring (name)
  "Returns the docstring for a binding form named `name`."
  (let* ((docstrings (get 'bind :docstrings))
	 (forms (get 'bind :binding-forms))
	 (canonical-name (first (assoc name forms)))
	 )
    (and canonical-name
	 (assoc canonical-name docstrings))))

(defun (setf binding-form-docstring) (docstring name/s)
  (when (atom name/s)
    (setf name/s (list name/s)))
  (let* ((docstrings (get 'bind :docstrings))
	 (forms (get 'bind :binding-forms))
	 (canonical-name (first name/s))
	 (current-docstring-pair (assoc canonical-name docstrings)))
    (loop for name in name/s do
	 (let ((names-pair (assoc name forms)))
	   (if names-pair
	       (setf (cdr names-pair) name/s)
	       (push (cons name name/s) forms))))
    (if current-docstring-pair
	(setf (cdr current-docstring-pair) docstring)
	(push (cons canonical-name docstring) docstrings))
    (setf (get 'bind :docstrings) docstrings)
    (setf (get 'bind :binding-forms) forms)
    docstring))

(defmacro defbinding-form ((name/s &key docstring remove-nils-p
				   description (use-values-p t)
				   (accept-multiple-forms-p nil)) &body body)
  "Describe how `bind` should expand particular binding-forms.

`defbinding-form` links a name or type with an expansion. These
definitions are used by `bind` at macro-expansion time to generate
the code that actually does the bindings for you.  For example:

    (defbinding-form (symbol :use-values-p nil)
      (if (keywordp kind)
          (error \"Don't have a binding form for ~s\" kind)
          `(let (,@(if values
                     `((,variables ,values))
                     `(,variables))))))

This binding form tells to expand clauses whose first element is
a symbol using `let`. (It also gets `bind` to signal an error if
the first element is a keyword that doesn't have a defined binding
form.)

If `use-values-p` is T, first bind value forms to gensyms to ensure
they are evaluated only once. If `accept-multiple-forms-p` is T, allow
passing multiple value forms, otherwise doing so results in an error.
"
  (declare (ignorable remove-nils-p description))
  (let* ((multiple-names? (consp name/s))
	 (main-method-name nil)
	 (force-keyword? (or multiple-names?
			     (eq (symbol-package name/s) 
				 (load-time-value (find-package :keyword)))))
	 (gnew-form (gensym "new-form")))
    (cond (multiple-names?
	   (setf main-method-name (gensym (symbol-name '#:binding-generator))))
	  (t
	   (setf main-method-name 'bind-generate-bindings)))
    (flet ((form-keyword (name)
	     (intern (symbol-name name)
		     (load-time-value (find-package :keyword)))))
      (when force-keyword?
	(setf name/s (if multiple-names? 
			 (mapcar #'form-keyword name/s)
			 (form-keyword name/s))))
      `(let ()
	 (setf (binding-form-docstring ',name/s) ,docstring)
	 ,@(loop for name in (if multiple-names? name/s (list name/s)) 
	         collect
		 `(defmethod binding-form-accepts-multiple-forms-p
		      ((binding-form ,(if (keywordp name) `(eql ,name) name)))
		    ,accept-multiple-forms-p))
	 (,(if multiple-names? 'defun 'defmethod) ,main-method-name
           (,@(unless multiple-names?
                      (if force-keyword?
                          `((kind (eql ,name/s)))
                          `((kind ,name/s))))
	      variable-form value-form)
	   ;;?? Can (symbolp (first body)) ever be true?
	   ,(if use-values-p
		`(let* ((gvalues (next-value "values-"))
			(,gnew-form (funcall (lambda (variables values) ,@body)
					     variable-form gvalues)))
		   (destructuring-bind (TAG . REST)
		       ,gnew-form
		     ;;?? CASE 
		     (if (or (eq TAG 'let) (eq TAG 'let*))
			 (destructuring-bind (let-bindings . after-bindings)
			     REST
			   (values `(let* ((,gvalues ,,(if accept-multiple-forms-p 
							   `value-form
							   `(first value-form)))
					   ,@let-bindings)
				      (declare (ignorable ,gvalues))
				      ,@after-bindings)
				   nil))
			 (values `(let* ((,gvalues ,,(if accept-multiple-forms-p 
							`value-form
							`(first value-form))))
				   (declare (ignorable ,gvalues))
				   ,,gnew-form)
				 t))))
		`(let ((,gnew-form (funcall (lambda (variables values) ,@body)
					    variable-form ,(if accept-multiple-forms-p
							       `value-form
							       `(first value-form)))))
		   (values ,gnew-form nil))))
	 ,@(when multiple-names?
		 (loop for name in name/s collect
		      `(defmethod bind-generate-bindings ((kind (eql ,name)) variable-form value-form)
			 (,main-method-name variable-form value-form))))))))

(defun next-value (x)
  (gensym x))

(defmacro lambda-bind ((&rest instrs) &body body)
  "Use `bind' to allow restructuring of argument to lambda expressions.

This lets you funcall and destructure simultaneously. For example

    (let ((fn (lambda-bind ((a b) c) (cons a c))))
      (funcall fn '(1 2) 3))
    ;; => (1 . 3)

Via eschulte (see git://gist.github.com/902174.git).
"
  #+(or)
  (declare (indent 1))
  (let* ((evald-instrs instrs)
         (syms (mapcar (lambda (_)
			 (declare (ignore _))
			 (gensym))
		       evald-instrs)))
    `(lambda ,syms (bind ,(mapcar #'list evald-instrs syms) ,@body))))

