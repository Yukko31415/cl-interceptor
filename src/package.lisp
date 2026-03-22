;;; package.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Your Name

(defpackage #:interceptor
  (:use #:cl)
  (:import-from #:bind #:bind)
  (:export #:define-executor
	   #:define-interceptor
	   #:execute
	   #:pop-interceptor
	   #:push-interceptor)
  (:documentation "the cl-interceptor package."))

(in-package #:interceptor)
