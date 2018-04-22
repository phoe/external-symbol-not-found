;;;; package.lisp

(defpackage #:external-symbol-not-found
  (:use #:cl)
  (:export #:external-symbol-not-found
           #:external-symbol-not-found-p
           #:external-symbol-not-found-symbol-name
           #:external-symbol-not-found-package))
