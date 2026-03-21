;;; main.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Your Name

(in-package #:interceptor)




;;;; ------------------
;;;; define-interceptor
;;;; ------------------


(defclass interceptor-template ()
  ((name :reader interceptor-name)
   (lambda-list :reader interceptor-lambda-list)
   (lambda-list-length :reader interceptor-lambda-list-length)
   (enter :reader interceptor-template-enter :initform nil)
   (leave :reader interceptor-template-leave :initform nil)
   (error :reader interceptor-template-error :initform nil)))

(defclass interceptor-template.internal ()
  ((name :initarg :name :reader interceptor-name)
   (enter :initarg :enter :reader interceptor-template-enter :initform nil)
   (leave :initarg :leave :reader interceptor-template-leave :initform nil)
   (error :initarg :error :reader interceptor-template-error :initform nil)))


;;
;; make-interceptor-template.internal

(defgeneric make-interceptor-template.internal (name))


;;
;; make-template

(defun %make-action (lambda-list list)
  (values `#'(lambda ,lambda-list ,@list) `'(lambda ,lambda-list ,@list)))

(defun %make-action/error (lambda-list list)
  (let ((elambda-list (first list)))
    (assert (= 1 (length elambda-list)) ()
	    "The first argument of the :error action must be a list containing exactly one symbol."))
  (let* ((c (pop list))
	 (lambda-list (append c lambda-list)))
    (values `#'(lambda ,lambda-list (declare (ignorable ,@c)) ,@list)
	    `'(lambda ,lambda-list (declare (ignorable ,@c)) ,@list))))


(defun make-action (lambda-list list)
  (ecase (pop list)
    (:enter (bind (((:values fn fnlist) (%make-action lambda-list list)))
	      (list `(enter :initform ,fn) `(enter/list :initform ,fnlist))))
    (:leave (bind (((:values fn fnlist) (%make-action lambda-list list)))
	      (list `(leave :initform ,fn) `(leave/list :initform ,fnlist))))
    (:error (bind (((:values fn fnlist) (%make-action/error lambda-list list)))
	      (list `(error :initform ,fn) `(error/list :initform ,fnlist))))))

;;
;; define-interceptor

(defmacro define-interceptor (name lambda-list &rest actions)
  (let* ((actions (loop :for act :in actions
			:append (make-action lambda-list act))))
    `(defclass ,name (interceptor-template)
       ((name :initform ',name)
	(lambda-list :initform ',lambda-list)
	(lambda-list-length :initform ,(length lambda-list))
	,@actions))))






;;;; ---------------
;;;; define-executor
;;;; ---------------


(defstruct interceptor enter leave error)

(defclass executor ()
  ((name :reader executor-name)
   (contexts :reader executor-contexts)
   (data :reader executor-data)
   (queue :accessor executor-queue)
   (stack :accessor executor-stack :initform nil)
   (return :accessor executor-return)))


;;
;; make-contexts-ht, make-contexts


(defun make-contexts (list)
  (loop :for i :in list
	:if (symbolp i)
	  :collect i :into contexts
	  :and :collect nil :into initial-contents
	:else :if (listp i)
		:collect (first i) :into contexts
		:and :collect (second i) :into initial-contents
	:finally (return (values contexts initial-contents))))

(defun make-contexts-ht (list)
  (let ((ht (make-hash-table :size (length list))))
    (loop :for sym :in list
	  :for i :from 0
	  :do (setf (gethash (intern (symbol-name sym) :keyword) ht) i))
    ht))


;;
;; subindex, make-macrolet-bind


(defun subindex (list contexts sym)
  (loop :for i :from 0
	:for context :in contexts
	:with list := (copy-list list)
	:do (nsubstitute `(aref ,sym ,i) context list)
	:finally (return list)))

(defun make-macrolet-bind  (contexts sym)
  (let ((substed-contexts (subindex contexts contexts sym)))
    (loop :for sym :in contexts
	  :for index :in substed-contexts
	  :collect `(,sym ,index))))

;;
;; make-definitions


(defun get-interceptor-functions (interceptor)
  (values (interceptor-template-enter interceptor)
	  (interceptor-template-leave interceptor)
	  (interceptor-template-error interceptor)))

(defun %%make-definition (interceptor input output sym)
  (bind ((sym/error (gensym "DATA"))
	 ((:flet fn (function lambda-list input output))
	  (when function
	    (cond
	      ((null output) `#'(lambda ,lambda-list (funcall #',function ,@input)))
	      ((= 1 (length output)) `#'(lambda ,lambda-list (setf ,@output (funcall #',function ,@input))))
	      (t `#'(lambda ,lambda-list (setf (values ,@output) (funcall #',function ,@input))))))))
    (values (fn (interceptor-template-enter/list interceptor)
		`(,sym) input output)
	    (fn (interceptor-template-leave/list interceptor)
		`(,sym) input output)
	    (fn (interceptor-template-error/list interceptor)
		`(,sym/error ,sym) `(,sym/error ,@input) output))))

(defun %make-definition (interceptor input output contexts &optional alias)
  (bind ((sym (gensym "DATA"))
	 (input (subindex input contexts sym))
	 (output (subindex output contexts sym))
	 ((:values enter leave error)
	  (%%make-definition interceptor input output sym)))
    `(,(or alias (interceptor-name interceptor))
      (make-interceptor :enter ,enter :leave ,leave :error ,error))))

(defun make-definition (definition contexts)
  (bind (((interceptor input output) definition)
	 (interceptor (make-instance interceptor))
	 (definition (%make-definition interceptor input output contexts)))
    definition))

(defun make-definition/with-alias (definition contexts)
  (bind (((alias interceptor input output) definition)
	 (interceptor (make-instance interceptor))
	 (definition (%make-definition interceptor input output contexts alias)))
    definition))

(defun make-definitions (definitions contexts)
  (loop :for definition :in definitions
	:if (= 3 (length definition))
	  :collect (make-definition definition contexts)
	:else :if (= 4 (length definition))
		:collect (make-definition/with-alias definition contexts)
	:else :do (assert nil () "The definition ~A must consist of 3 or 4 elements." definition)))


;;
;; make-return


(defun make-return (return returnp contexts)
  (when returnp
    (let* ((sym (gensym "DATA"))
	   (for-macrolet (make-macrolet-bind contexts sym)))
      `(symbol-macrolet ,for-macrolet
	 #'(lambda (,sym) ,return)))))


;;
;; define-executor


(defmacro define-executor (name (contexts &rest definitions)
		    (actions &key (return nil returnp)))
  (bind (((:values contexts initial-contents) (make-contexts contexts))
	 (definitions (make-definitions definitions contexts))
	 (contexts-ht (make-contexts-ht contexts))
	 (return (make-return return returnp contexts))
	 (executor `(defclass ,name (executor)
		      ((name :initform ',name)
		       (queue :initform (list ,@actions))
		       (contexts :initform ,contexts-ht)
		       (data :initform
			     (make-array ,(length contexts)
					 :initial-contents ',initial-contents))
		       (return :initform ,return)))))
    `(let ,definitions ,executor)))






;;;; ----------------
;;;; push-interceptor
;;;; ----------------

;;
;; make-injectable-interceptor


(defstruct (injectable-interceptor
	    (:include interceptor)
	    (:constructor %make-injectable-interceptor)))

(defun symbol->key (sym)
  (intern (symbol-name sym) :keyword))

(define-condition invalid-context-slot (error)
  ((sym :initarg :sym :reader sym))
  (:report (lambda (c s) (format s "~A is an invalid context slot" (sym c)))))

(defun get-context-data (data contexts sym
		     &aux (key (symbol->key sym)))
  (bind (((:values index present-p) (gethash key contexts)))
    (if present-p (aref data index)
	(error 'invalid-context-slot :sym sym))))

(defun make-injectable-interceptor-input (data contexts input)
  (loop :for i :in input
	:if (symbolp i)
	  :collect (get-context-data data contexts i)
	:else
	  :collect i))

(defun %execute-injectable-interceptor (length input conditionp)
  (assert (= (if conditionp (1+ length) length) (length input)) ()
	  "The number of arguments provided ~A does not match the number of arguments required for execution."
	  input))

(defun execute-injectable-interceptor (fn length data contexts input output
				       &optional (condition nil conditionp))
  (when fn (%execute-injectable-interceptor length input conditionp)
	(let* ((input/data (make-injectable-interceptor-input data contexts input))
	       (output/key (mapcar #'symbol->key output))
	       (output/list (mapcar #'(lambda (a) (gethash a contexts)) output/key))
	       (result (multiple-value-list
			(apply fn (if conditionp (cons condition input/data) input/data)))))
	  (loop :for i := (pop result)
		:for index :in output/list
		:do (setf (aref data index) i)))))

(defun make-injectable-interceptor (interceptor-name input output)
  (bind ((interceptor (make-instance interceptor-name))
	 ((:accessors (enter interceptor-template-enter)
		      (leave interceptor-template-leave)
		      (error interceptor-template-error)
		      (length interceptor-lambda-list-length))
	  interceptor))
    (%make-injectable-interceptor
     :enter #'(lambda (data contexts)
		(execute-injectable-interceptor enter length data contexts input output))
     :leave #'(lambda (data contexts)
		(execute-injectable-interceptor leave length data contexts input output))
     :error #'(lambda (condition data contexts)
		(execute-injectable-interceptor error length data contexts input output condition)))))


;;
;; push-interceptor



(define-condition push-interceptor (condition)
  ((interceptor :initarg :interceptor :reader interceptor)))

(defun push-interceptor (interceptor-name input output)
  (restart-case
      (signal 'push-interceptor
	      :interceptor (make-injectable-interceptor interceptor-name input output))
    (return () nil)))





;;;; ---------------
;;;; pop-interceptor
;;;; ---------------


(define-condition pop-interceptor (condition) ())

(defun pop-interceptor ()
  (restart-case (signal 'pop-interceptor)
    (return () nil)))




;;;; --------------------
;;;; with-dynamic-handler
;;;; --------------------

;;
;; execute-error


(define-condition interceptor-condition (condition) ())
(define-condition interceptor-error (error interceptor-condition) ())

(defun execute-error (condition executor-stack executor-data executor-contexts)
  (handler-bind ((interceptor-error #'(lambda (c) (error c))))
    (handler-case
	(loop :for i := (pop executor-stack)
	      :while i
	      :if (injectable-interceptor-p i)
	  :do (let ((fn (interceptor-error i)))
		(when fn (funcall fn condition executor-data executor-contexts)))
	      :else
	  :do (let ((fn (interceptor-error i)))
		(when fn (funcall fn condition executor-data))))
      (interceptor-condition (c) (execute-error c executor-stack executor-data executor-contexts)))
    (signal condition)))


;;
;; with-dynamic-handler



(defmacro with-dynamic-handler (handle-for &body body)
  `(handler-bind ((push-interceptor #'(lambda (c) (push (interceptor c) ,handle-for)
					(invoke-restart 'return)))
		  (pop-interceptor #'(lambda (c) (declare (ignore c))
				       (pop ,handle-for) (invoke-restart 'return)))
		  (interceptor-condition #'(lambda (c) (invoke-restart 'execute-error c))))
     ,@body))





;;;; -------
;;;; execute
;;;; -------

(defun make-executor (name)
  (assert (subtypep name 'executor)
	  (name) "~A is not a executor." name)
  (let ((executor (make-instance name)))
    executor))

(defstruct (initvalues (:type list) (:constructor %make-initvalues))
  index val)

(defun make-initvalues (executor initvalues)
  (bind (((:accessors (contexts executor-contexts)) executor))
    (loop :for (key val) :on initvalues :by #'cddr
	  :collect (%make-initvalues :index (gethash key contexts) :val val))))

(defun initialize-executor-data (executor initvalues)
  (when initvalues
    (bind (((:accessors (data executor-data)) executor)
	   (list (make-initvalues executor initvalues)))
      (loop :for i :from 0
	    :for (index val) :in list
	    :do (setf (aref data index) val)))))


;;
;; execute-enter, execute-leave


(defun execute-enter (executor)
  (bind (((:accessors executor-queue executor-stack executor-data executor-contexts) executor))
    (with-dynamic-handler executor-queue
      (restart-case
	  (loop :for i := (pop executor-queue)
		:while i
		:do (push i executor-stack)
		:if (injectable-interceptor-p i)
		  :do (let ((fn (interceptor-enter i)))
			(when fn (funcall fn executor-data executor-contexts)))
		:else
		  :do (let ((fn (interceptor-enter i)))
			(when fn (funcall fn executor-data))))
	(execute-error (c) (execute-error c executor-stack executor-data executor-contexts))))))

(defun execute-leave (executor)
  (bind (((:accessors executor-queue executor-stack executor-data executor-contexts) executor))
    (with-dynamic-handler executor-stack
      (restart-case
	  (loop :for i := (pop executor-stack)
		:while i
		:if (injectable-interceptor-p i)
		  :do (let ((fn (interceptor-leave i)))
			(when fn (funcall fn executor-data executor-contexts)))
		:else
		  :do (let ((fn (interceptor-leave i)))
			(when fn (funcall fn executor-data))))
	(execute-error (c) (execute-error c executor-stack executor-data executor-contexts))))))


;;
;; execute


(defun execute (executor-name &rest initvalues)
  (let ((executor (make-executor executor-name)))
    (progn (initialize-executor-data executor initvalues)
	   (execute-enter executor)
	   (execute-leave executor))
    (bind (((:accessors executor-return executor-data) executor))
      (if executor-return
	  (funcall executor-return executor-data)
	  executor-data))))







