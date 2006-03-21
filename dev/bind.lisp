;;;-*- Mode: Lisp; Package: BIND -*-

#| simple-header

http://www.opensource.org/licenses/mit-license.php

Copyright (c) 2004-2005 Gary Warren King, metabang.com

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
     #:fluid-bind))

#+NO
(unless (find-package "BIND")
  (defpackage "BIND"
    (:use "COMMON-LISP")
    (:export 
     #:bind
     #:fluid-bind
     
     #:bind-error
     #:bind-keyword/optional-nil-with-default-error
     #:bad-variable
     #:binding)))

(in-package metabang.bind) 
           
(defparameter *bind-all-declarations*
  '(dynamic-extent ignore optimize ftype inline special ignorable notinline type))

;;; ---------------------------------------------------------------------------

(defparameter *bind-non-var-declarations*
  '(optimize ftype inline notinline))

;;; ---------------------------------------------------------------------------

(defparameter *bind-simple-var-declarations*
  (remove 'type
          (set-difference *bind-all-declarations* *bind-non-var-declarations*)))

;;; ---------------------------------------------------------------------------

(define-condition bind-error (error)
                  ((binding :initform nil :initarg :binding :reader binding)))

;;; ---------------------------------------------------------------------------

(define-condition bind-keyword/optional-nil-with-default-error (bind-error)
                  ((bad-variable :initform nil :initarg :bad-variable :reader bad-variable))
  (:report (lambda (c s)
             (format s "Bad binding '~S' in '~A'; cannot use a default value for &key or &optional arguments."
                     (bad-variable c) (binding c)))))

;;; ---------------------------------------------------------------------------

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

;;; ---------------------------------------------------------------------------

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
                  (eq (first variable-form) 'cl:values))
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

;;; ---------------------------------------------------------------------------

(defun bind-fix-nils (var-list)
  (let (vars ignores)
    (loop for v in var-list do
          (cond (v (push v vars))
                (t (let ((ignore (gensym "BIND-IGNORE-")))
                     (push ignore vars)
                     (push ignore ignores)))))
    (values (nreverse vars) ignores)))

;;; ---------------------------------------------------------------------------

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

;;; ---------------------------------------------------------------------------

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

;;; ---------------------------------------------------------------------------

(defun bind-filter-declarations (declarations var-names)
  (setf var-names (if (consp var-names) var-names (list var-names)))
  
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

;;; ***************************************************************************
;;; *                              End of File                                *
;;; ***************************************************************************

