;;;; package.lisp

(cl:in-package :cl-user)

(defpackage :ull1
  (:use)
  (:export :ull))

(defpackage :ull1-internal
  (:use :ull1 :cl :fiveam))

