;;; package.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Your Name

(defpackage #:interceptor
  (:use #:cl)
  (:import-from #:bind #:bind)
  (:export #:define-interceptor
	   #:push-interceptor
	   #:pop-interceptor
	   #:define-executor
	   #:execute)
  (:documentation "the cl-interceptor package."))

(in-package #:interceptor)
