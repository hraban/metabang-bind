;;;-*- Mode: Lisp; Package: bind -*-

#| simple-header

See the file COPYING for details

|#

(defpackage #:metabang.bind
    (:use #:common-lisp)
    (:nicknames #:bind #:metabang-bind)
    (:intern 
     #:bind-generate-bindings
     #:bind-filter-declarations
     #:bind-macro-helper
     #:bind-fix-nils)
    (:export 
     #:bind
     #:fluid-bind

     #:*bind-all-declarations*
     #:*bind-non-var-declarations*
     #:*bind-lambda-list-markers*

     ;; this will be removed ... someday
     #:*bind-treat-values-as-values*

     #:bind-error
     #:bind-keyword/optional-nil-with-default-error))

(defpackage #:metabang.bind.developer
    (:use #:common-lisp #:metabang-bind)
    (:import-from #:metabang-bind
		  #:bind-generate-bindings
		  #:bind-filter-declarations
		  #:bind-macro-helper
		  #:bind-fix-nils)
    (:export 
     #:bind-generate-bindings
     #:bind-filter-declarations
     #:bind-macro-helper
     #:bind-fix-nils))

(in-package #:metabang.bind) 
           
(defparameter *bind-all-declarations*
  '(dynamic-extent ignore optimize ftype inline 
    special ignorable notinline type))

(defparameter *bind-treat-values-as-values* t
  "If true, then bind will treat cl:values in the first position of 
a binding form as if it was :values and convert the binding form into
a multipl-value-bind. If false, then bind will treat the binding form
as a destructuring-bind and use values as a variable. E.g., if 
\\*bind-treat-values-as-values\\* is true, then the following will
not compile \(because values is not lexically bound\).

    \(bind \(\(\(values a b\) \(foo\)\)\)
      \(list values a b\)\)

If \\*bind-treat-values-as-values\\* was nil, then the binding form
would be converted into a destructuring-bind and all would be well.

Bind's original behavior was as if this variable was set to true. At
some point in the future, this variabe will vanish and bind will 
always treat cl:values as destructuring.")

(defparameter *bind-non-var-declarations*
  '(optimize ftype inline notinline))

(defparameter *bind-simple-var-declarations*
  (remove 'type
          (set-difference *bind-all-declarations* *bind-non-var-declarations*)))

(defparameter *bind-lambda-list-markers* 
  '(&key &body &rest &args &optional))

(define-condition bind-error (error)
                  ((binding
		    :initform nil
		    :initarg :binding
		    :reader binding)))

(define-condition bind-keyword/optional-nil-with-default-error (bind-error)
                  ((bad-variable 
		    :initform nil
		    :initarg :bad-variable
		    :reader bad-variable))
  (:report (lambda (c s)
             (format s "Bad binding '~S' in '~A'; cannot use a default value for &key or &optional arguments."
                     (bad-variable c) (binding c)))))

(defmacro bind ((&rest bindings) &body body)
  "Bind is a replacement for let*, destructuring-bind and multiple-value-bind. An example is probably the best way to describe its syntax:

    \(bind \(\(a 2\)
           \(\(b &rest args &key \(c 2\) &allow-other-keys\) '\(:a :c 5 :d 10 :e 54\)\)
           \(\(:values d e\) \(truncate 4.5\)\)\)
         \(list a b c d e args\)\)

Simple bindings are as in let*. Destructuring is done if the first item
in a binding is a list. Multiple value binding is done if the first item
in a binding is a list and the first item in the list is ':values'."
  (let (declarations)
    (loop while (and (consp (car body)) (eq (caar body) 'declare)) do
          (push (first body) declarations)
          (setf body (rest body)))
    (first (bind-macro-helper 
            bindings 
            (bind-expand-declarations (nreverse declarations)) body))))

(defun bind-macro-helper (bindings declarations body)
  (if bindings
      (let ((binding (first bindings))
	    (remaining-bindings (rest bindings))
	    variable-form value-form)
	(if (consp binding)
	    (setf variable-form (first binding)
		  value-form (second binding))
	    (setf variable-form binding))
	(if (and (consp variable-form)
		 (or (eq (first variable-form) 'cl:values)
		     (and (symbolp (first variable-form))
			  (eq (symbol-package (first variable-form))
			      (load-time-value (find-package :keyword))))))
	    (bind-generate-bindings 
	     (first variable-form)
	     (rest variable-form)
	     value-form body declarations remaining-bindings)
	    (bind-generate-bindings
	     nil
	     variable-form
	     value-form body declarations remaining-bindings)))
      body))

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
	  "Unable binding specification ~s" kind)
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
  (cond ((and (consp value-form) *bind-treat-values-as-values*)	 
	 (warn "The use of cl:values in smu:bind is deprecated. Please change to the unambiguous :values instead.")
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

(defmethod bind-generate-bindings ((kind (eql :plist)) variable-form value-form
				   body declarations remaining-bindings)
  (bind-handle-plist variable-form value-form
		     body declarations remaining-bindings))

(defmethod bind-generate-bindings ((kind (eql :property-list))
				   variable-form value-form
				   body declarations remaining-bindings)
  (bind-handle-plist variable-form value-form
		     body declarations remaining-bindings))

(defmethod bind-generate-bindings ((kind (eql :properties))
				   variable-form value-form
				   body declarations remaining-bindings)
  (bind-handle-plist variable-form value-form
		     body declarations remaining-bindings))

(defun bind-handle-plist (variable-form value-form
			  body declarations remaining-bindings)
  (let ((vars variable-form)
	(plist-gensym (gensym "plist")))
    (assert vars)
    `((let* ((,plist-gensym ,value-form)
	     ,@(loop for spec in vars collect
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
		      (setf var-key (intern (symbol-name var-key) :keyword))
		      `(,var-name (getf ,plist-gensym ,var-key 
					,@(when var-default
						`(,var-default)))))))
	,(bind-filter-declarations declarations variable-form)
	,@(bind-macro-helper 
	   remaining-bindings declarations body)))))

#+(or)
(bind (((:plist a (b _) (c _ 2) (dd d)) '(:b "B" :a "A" :d "D")))
  (list a b c dd))

(defmethod bind-generate-bindings 
    ((kind (eql :structure)) variable-form value-form
     body declarations remaining-bindings)
  (bind-handle-structure variable-form value-form
			 body declarations remaining-bindings))

(defmethod bind-generate-bindings
    ((kind (eql :struct)) variable-form value-form
     body declarations remaining-bindings)
  (bind-handle-structure variable-form value-form
			 body declarations remaining-bindings))

(defun bind-handle-structure (variable-form value-form
			      body declarations remaining-bindings)
  (let ((conc-name (first variable-form))
	(vars (cdr variable-form))
	(structure-gensym (gensym "struct")))
    (assert conc-name)
    (assert vars)
    `((let* ((,structure-gensym ,value-form)
	     ,@(loop for var in vars collect
		    (let ((var-var (or (and (consp var) (first var))
				       var))
			  (var-conc (or (and (consp var) (second var))
					var)))
		      `(,var-var (,(intern 
				    (format nil "~a~a" 
					    conc-name var-conc)) 
				   ,structure-gensym)))))
	,(bind-filter-declarations declarations variable-form)
	,@(bind-macro-helper 
	   remaining-bindings declarations body)))))

(defmethod bind-generate-bindings 
    ((kind (eql :slots)) variable-form value-form
     body declarations remaining-bindings)
  (bind-handle-slots variable-form value-form
		     body declarations remaining-bindings))

(defun bind-handle-slots (variable-form value-form
			      body declarations remaining-bindings)
  (let ((vars variable-form)
	(class-gensym (gensym "class")))
    (assert vars)
    `((let* ((,class-gensym ,value-form)
	     ,@(loop for var in vars collect
		    (let ((var-var (or (and (consp var) (first var))
				       var))
			  (var-slot (or (and (consp var) (second var))
					var)))
		      `(,var-var (slot-value ,class-gensym ',var-slot)))))
	,(bind-filter-declarations declarations variable-form)
	,@(bind-macro-helper 
	   remaining-bindings declarations body)))))

(defmethod bind-generate-bindings 
    ((kind (eql :read-only-accessors)) variable-form value-form
     body declarations remaining-bindings)
  (bind-handle-r/o-accessors variable-form value-form
		     body declarations remaining-bindings))

(defmethod bind-generate-bindings 
    ((kind (eql :accessors-read-only)) variable-form value-form
     body declarations remaining-bindings)
  (bind-handle-r/o-accessors variable-form value-form
		     body declarations remaining-bindings))

(defmethod bind-generate-bindings 
    ((kind (eql :accessors-r/o)) variable-form value-form
     body declarations remaining-bindings)
  (bind-handle-r/o-accessors variable-form value-form
		     body declarations remaining-bindings))

(defun bind-handle-r/o-accessors (variable-form value-form
			      body declarations remaining-bindings)
  (let ((vars variable-form)
	(class-gensym (gensym "class")))
    (assert vars)
    `((let* ((,class-gensym ,value-form)
	     ,@(loop for var in vars collect
		    (let ((var-var (or (and (consp var) (first var))
				       var))
			  (var-accessor (or (and (consp var) (second var))
					var)))
		      `(,var-var (,var-accessor ,class-gensym)))))
	,(bind-filter-declarations declarations variable-form)
	,@(bind-macro-helper 
	   remaining-bindings declarations body)))))

(defmethod bind-generate-bindings 
    ((kind (eql :accessors)) variable-form value-form
     body declarations remaining-bindings)
  (bind-handle-with-accessors variable-form value-form
			      body declarations remaining-bindings))

(defmethod bind-generate-bindings 
    ((kind (eql :writable-accessors)) variable-form value-form
     body declarations remaining-bindings)
  (bind-handle-with-accessors variable-form value-form
			      body declarations remaining-bindings))

(defun bind-handle-with-accessors
    (variable-form value-form
     body declarations remaining-bindings)
  (let ((vars variable-form))
    (assert vars)
    `((with-accessors 
	    (,@(loop for var in vars collect
		    (let ((var-var (or (and (consp var) (first var))
				       var))
			  (var-accessor (or (and (consp var) (second var))
					    var)))
		      `(,var-var ,var-accessor))))
	  ,value-form
	,(bind-filter-declarations declarations variable-form)
	,@(bind-macro-helper 
	   remaining-bindings declarations body)))))


;;;;

(defun bind-fix-nils (var-list)
  (let (vars ignores)
    (loop for v in var-list do
          (cond (v (push v vars))
                (t (let ((ignore (gensym "BIND-IGNORE-")))
                     (push ignore vars)
                     (push ignore ignores)))))
    (values (nreverse vars) ignores)))

(defun bind-fix-nils-destructured (var-list)
  (let ((ignores nil))
    (labels (;; adapted from metatilities 
             (tree-map (fn tree)
               "Maps FN over every atom in TREE."
               (cond
                ;; ((null tree) nil)
                ((atom tree) (funcall fn tree))
                (t
                 (cons
                  (tree-map fn (car tree))
                  (when (cdr tree) (tree-map fn (cdr tree))))))))
      
      (values (tree-map
               (lambda (x)
                 (cond (x x)
                       (t (let ((ignore (gensym "BIND-IGNORE-")))
                            (push ignore ignores)
                            ignore))))
               var-list)
              ignores))))

(defun dotted-pair-p (putative-pair)
  "Returns true if and only if `putative-pair` is a dotted-list. I.e., if `putative-pair` is a cons cell with a non-nil cdr."
  (and (consp putative-pair)
       (cdr putative-pair)
       (not (consp (cdr putative-pair)))))

(defun bind-get-vars-from-lambda-list (lambda-list)
  (let ((result nil))
    (labels ((do-it (thing)
	       (cond ((atom thing) 
		      (unless (or (member thing *bind-lambda-list-markers*)
				  (null thing))
			(push thing result)))
		     ((dotted-pair-p thing)
		      (do-it (car thing)) 
		      (do-it (cdr thing)))
		     (t
		      (do-it (car thing))
		      (do-it (cdr thing))))))
      (do-it lambda-list))
    (nreverse result)))

#+(or)
(loop for item in lambda-list 
   unless (member item *bind-lambda-list-markers*) collect
     (if (consp item) (first item) item))

(defun bind-expand-declarations (declarations)
  (loop for declaration in declarations append
        (loop for decl in (rest declaration) append
              (cond ((member (first decl) *bind-non-var-declarations*)
                     (list decl))
                    ((member (first decl) *bind-simple-var-declarations*)
                     (loop for var in (rest decl) collect
                           `(,(first decl) ,var)))
                    (t
                     ;; a type spec
                     (when (eq (first decl) 'type)
                       (setf decl (rest decl)))
                     (loop for var in (rest decl) collect
                           `(type ,(first decl) ,var)))))))

(defun bind-filter-declarations (declarations var-names)
  (setf var-names (if (consp var-names) var-names (list var-names)))  
  (setf var-names (bind-get-vars-from-lambda-list var-names))
  ;; each declaration is separate
  (let ((declaration
         (loop for declaration in declarations 
               when (or (member (first declaration)
				*bind-non-var-declarations*)
                        (and (member (first declaration)
				     *bind-simple-var-declarations*)
                             (member (second declaration) var-names))
                        (member (third declaration) var-names)) collect
               declaration))) 
    (when declaration 
      `(declare ,@declaration))))

;;; ---------------------------------------------------------------------------
;;; fluid-bind
;;; ---------------------------------------------------------------------------

(defmacro fluid-bind ((&rest bindings) &body body)
  "Fluid-bind is an extension of bind that handles setting and resetting places. For example, suppose that an object of class foo has a slot named bar whose value is currently 3. The following code would evaluate the inner body with bar bound to 17 and restore it when the inner body is exited. 

\(fluid-bind \(\(\(bar foo\) 17\)\)
  \(print \(bar foo\)\)\)
\(print \(bar foo\)\)
==> \(prints 17, then 3\)

This is similar to dynamic-binding but _much_ less robust."
  ;; does not handle declarations correctly
  (let ((setup-forms nil)
        (cleanup-forms nil)
        (gensyms nil))
    (loop for binding in bindings collect
          (destructuring-bind 
		(setup-form cleanup-form)
	      (cond ((consp binding)
		     (destructuring-bind (var value) binding
		       (let ((g (gensym)))
			 (push g gensyms)
			 (cond ((atom var)
				`((:bind (,var ,value)) nil)
				#+(or)
				;; lexical or special?
				(if (boundp var)
				    `((:bind (,var ,value)) nil)
				    `((:setf (setf ,g ,var ,var ,value))
				      (setf ,var ,g))))
			       ((and (fboundp (first var))
				     (not (eq (first var) 'values)))
				;; putative place
				`((:setf (setf ,g ,var ,var ,value))
				  (setf ,var ,g)))
			       (t
				`((:bind (,var ,value)) nil))))))
		    (t
		     `((:bind (,binding nil)) nil)))
            (push setup-form setup-forms)
            (push cleanup-form cleanup-forms)))
    (let ((result body))
      (mapc (lambda (setup cleanup)
              (setf result
                    (ecase (first setup)
                      (:setf `((unwind-protect
                                 (progn
                                   ,(second setup)
                                   ,@result)
                                 ,cleanup)))
                      (:bind `((bind (,(second setup))
                                 ,@result)))))
              result)
            setup-forms cleanup-forms)
      `(let ,gensyms
         (declare (ignorable ,@gensyms))
         ,@result))))

#|
(let ((a 2))
  (fluid-bind ((a 3))
    (print a))
  (print a))

(fluid-bind (((population (current-world-state)) t))
  (print (population (current-world-state))))

(fluid-bind ((a 3)
             (*last-world* t)
             (*foo* nil))
  (declare (fixnum a))
  (print (list *last-world* *foo* a))
  (error "Ouch"))

(defvar *foo* 3)

(unwind-protect
  (bind ((#:g1 *last-world*))
    (setf *last-world* t)
    (unwind-protect
      (bind ((#:2 *foo*))
        (setf *foo* nil)
        (bind ((a 3))
          (list *last-world* *foo* a)))
      (setf *foo #:2)))
  (set *last-world* #:g1))      
    
(fluid-bind (a b)
  (+ a a))
|#


