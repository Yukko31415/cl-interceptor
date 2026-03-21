;;; cl-interseptor.asd
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Your Name

(asdf:defsystem #:cl-interceptor
  :description "A basic application."
  :author      "Yukko"
  :license     "MIT"
  :version     "0.2.0"
  :depends-on  ("metabang-bind" "alexandria")
  :serial t
  :components ((:file "src/package")
               (:file "src/main")))
