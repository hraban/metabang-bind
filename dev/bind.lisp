;;;-*- Mode: Lisp; Package: BIND -*-

#| simple-header

http://www.opensource.org/licenses/mit-license.php

Copyright (c) 2004-2006 Gary Warren King, metabang.com

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
Author: Gary King

DISCUSSION

|#
(defpackage #:metabang.bind
    (:use #:common-lisp)
    (:nicknames #:bind #:metabang-bind)
    (:export 
     #:bind
     #:fluid-bind
     #:define-dynamic-context
     #:parent-context-of))

(in-package #:metabang.bind) 
           
(defparameter *bind-all-declarations*
  '(dynamic-extent ignore optimize ftype inline special 
    ignorable notinline type))

(defparameter *bind-non-var-declarations*
  '(optimize ftype inline notinline))

(defparameter *bind-simple-var-declarations*
  (remove 'type
          (set-difference *bind-all-declarations* *bind-non-var-declarations*)))


(defparameter *lambda-list-markers* 
  '(&key &body &rest &args &optional))

(define-condition bind-error (error)
                  ((binding :initform nil :initarg :binding :reader binding)))

(define-condition bind-keyword/optional-nil-with-default-error (bind-error)
  ((bad-variable :initform nil :initarg :bad-variable :reader bad-variable))
  (:report (lambda (c s)
             (format s "Bad binding '~S' in '~A'; cannot use a default value for &key or &optional arguments."
                     (bad-variable c) (binding c)))))

(defmacro bind ((&rest bindings) &body body)
  "Bind is a replacement for let*, destructuring-bind and multiple-value-bind.
An example is probably the best way to describe its syntax:

    \(bind \(\(a 2\)
           \(\(b &rest args &key \(c 2\) &allow-other-keys\) '\(:a :c 5 :d 10 :e 54\)\)
           \(\(values d e\) \(truncate 4.5\)\)\)
      \(list a b c d e args\)\)

Simple bindings are as in let*. Destructuring is done if the first item
in a binding is a list. Multiple value binding is done if the first item
in a binding is a list and the first item in the list is 'values'."
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
      
      (cond ((and (consp variable-form)
                  (or (eq (first variable-form) 'cl:values)
		      (eq (first variable-form) ':values)))
             (multiple-value-bind (vars ignores)
                                  (bind-fix-nils (rest variable-form))
               `((multiple-value-bind ,vars ,value-form
                   ,@(when ignores `((declare (ignore ,@ignores))))
                   ,(bind-filter-declarations declarations (rest variable-form))
                   ,@(bind-macro-helper remaining-bindings declarations body)))))
            ((consp variable-form)
             (multiple-value-bind (vars ignores)
                                  (bind-fix-nils-destructured variable-form)
               `((destructuring-bind ,vars ,value-form
                   ,@(when ignores `((declare (ignore ,@ignores))))
                   ,(bind-filter-declarations declarations variable-form)
                   ,@(bind-macro-helper remaining-bindings declarations body)))))
            (t
             `((let (,@(if value-form
                         `((,variable-form ,value-form))
                         `(,variable-form)))
                 ,(bind-filter-declarations declarations variable-form)
                 ,@(bind-macro-helper remaining-bindings declarations body))))))
    body))

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
    (labels ((do-it (thing doing-defaults?)
	       (cond ((atom thing) 
		      (unless (or (member thing *lambda-list-markers*)
				  (null thing))
			(push thing result)))
		     ((dotted-pair-p thing)
		      (do-it (car thing) doing-defaults?) 
		      (do-it (cdr thing) doing-defaults?))
		     (t
		      (dolist (it thing)
			(cond ((member it '(&optional &key))
			       (setf doing-defaults? t))
			      ((consp it)
			       (if doing-defaults? 
				   (do-it (first it) nil)
				   (do-it it nil)))
			      (t
			       (do-it it nil))))))))
      (do-it lambda-list nil))
    (nreverse result)))

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
               when (or (member (first declaration) *bind-non-var-declarations*)
                        (and (member (first declaration) *bind-simple-var-declarations*)
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
          (destructuring-bind (setup-form cleanup-form)
                              (cond ((consp binding)
                                     (destructuring-bind (var value) binding
                                       (let ((g (gensym)))
                                         (push g gensyms)
                                         (cond ((atom var)
                                                ;; lexical or special?
                                                (if (boundp var)
                                                  `((:setf (setf ,g ,var ,var ,value))
                                                    (setf ,var ,g))
                                                  `((:bind (,var ,value)) nil)))
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

(defmacro define-dynamic-context
    (name direct-slots &key direct-superclasses
     export-symbols (class-name name) chain-parents
     (create-struct nil) (create-class (not create-struct))
     struct-options
     (defclass-macro-name 'defclass))
  "The purpose of this macro is to provide an easy way to access a group of related special variables. To do so, it generates
   with-NAME/in-NAME/current-NAME/has-NAME macros to access either a CLOS instance or a defstruct in a special variable.
   Optionally it can chain the \"parent\" bindings (use :CHAIN-PARENTS T and access with PARENT-CONTEXT-OF)."
  (assert (and (or create-class
                   create-struct
                   (not (or direct-slots direct-superclasses chain-parents)))
               (or (not create-struct)
                   (not direct-superclasses)))
          () "Invalid combination of DIRECT-SLOTS, DIRECT-SUPERCLASSES, CHAIN-PARENTS and CREATE-CLASS/CREATE-STRUCT.")
  (assert (or (not struct-options) create-struct) () "STRUCT-OPTIONS while no CREATE-STRUCT?")
  (assert (not (and create-class create-struct)) () "Only one of CREATE-CLASS and CREATE-STRUCT is allowed.")
  (flet ((concatenate-symbol (&rest args)
           (let* ((package nil)
                  (symbol-name (string-upcase
                                (with-output-to-string (str)
                                  (dolist (arg args)
                                    (typecase arg
                                      (string (write-string arg str))
                                      (package (setf package arg))
                                      (symbol (unless package
                                                (setf package (symbol-package arg)))
                                              (write-string (symbol-name arg) str))
                                      (integer (write-string (princ-to-string arg) str))
                                      (character (write-char arg) str)
                                      (t (error "Cannot convert argument ~S to symbol" arg))))))))
             (if package
                 (intern symbol-name package)
                 (intern symbol-name))))
         (strcat (&rest string-designators)
           (with-output-to-string (str)
             (dolist (s string-designators)
               (when s (princ s str))))))
    (let ((special-var-name (concatenate-symbol "%" name "%"))
          (extractor-name (concatenate-symbol "current-" name))
          (has-checker-name (concatenate-symbol "has-" name))
          (with-new-macro-name (concatenate-symbol "with-new-" name))
          (with-macro-name (concatenate-symbol "with-" name))
          (struct-constructor-name (when create-struct
                                     (or (second (assoc :constructor struct-options))
                                         (concatenate-symbol "make-" name))))
          (struct-conc-name (when create-struct
                              (or (second (assoc :conc-name struct-options))
                                  (concatenate-symbol class-name "-")))))
      `(progn
        ,(when export-symbols
               `(export (list ',extractor-name
                         ',with-new-macro-name
                         ',with-macro-name
                         ',(concatenate-symbol "in-" name))))
        ;; generate the context class definition
        ,(when create-class
               `(,defclass-macro-name ,class-name ,direct-superclasses
                 ,(if chain-parents
                      (append `((parent-context nil :accessor parent-context-of)) direct-slots) ; accessor is explicitly given to force it to be interned in this package
                      direct-slots)))
        ,(when create-struct
               `(defstruct (,name ,@struct-options)
                 ,@(if chain-parents
                       (append `((parent-context nil :type (or null ,class-name))) direct-slots)
                       direct-slots)))
        ;; generate the with-new-... macro
        (defmacro ,with-new-macro-name ((&rest initargs &key &allow-other-keys)
                                        &body forms)
          `(,',with-macro-name ,,(if create-struct
                                   ``(,',struct-constructor-name ,@initargs)
                                   ``(make-instance ',',class-name ,@initargs))
            ,@forms))
        ;; generate the with-... macro
        (defmacro ,with-macro-name (context &body forms)
          (let ((context-instance (gensym "CONTEXT-INSTANCE"))
                (parent (gensym "PARENT")))
            (declare (ignorable parent))
            `(let* ((,context-instance ,context)
                    ,@,(when chain-parents
                             ``((,parent (when (,',has-checker-name)
                                           (,',extractor-name)))))
                    (,',special-var-name ,context-instance))
              (declare (special ,',special-var-name))
              ,@,(when chain-parents
                       ``((setf (,',(if create-struct
                                       (concatenate-symbol struct-conc-name "parent-context")
                                       'parent-context-of) ,context-instance)
                           ,parent)))
              (unless ,context-instance
                (error ,',(strcat "Called with nil " (string-downcase name))))
              ,@forms)))
        ;; generate the in-... macro
        (defmacro ,(concatenate-symbol "in-" name) (var-name-or-slot-name-list &body forms)
          (let ((slots (when (listp var-name-or-slot-name-list)
                         var-name-or-slot-name-list)))
            (if slots
                `(with-slots ,slots (,',extractor-name)
                  ,@forms)
                `(let ((,var-name-or-slot-name-list (,',extractor-name)))
                  ,@forms))))
        ;; generate the current-... function
        (declaim (inline ,extractor-name))
        (defun ,extractor-name ()
          (symbol-value ',special-var-name))
        ;; generate the has-... function
        (declaim (inline ,has-checker-name))
        (defun ,has-checker-name ()
          (boundp ',special-var-name))))))

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


