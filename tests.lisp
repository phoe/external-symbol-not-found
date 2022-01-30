
(defpackage #:external-symbol-not-found.test
  (:documentation "Tests for external-symbol-not-found.")
  (:use #:cl)
  (:import-from
   #:external-symbol-not-found
   #:external-symbol-not-found-p)
  (:import-from
   #:parachute
   #:define-test
   #:true))

(in-package #:external-symbol-not-found.test)

(defmacro with-temp-package ((name) &body body)
  `(unwind-protect
        (progn
          (make-package ',name :use nil)
          ,@body)
     (delete-package ',name)))

(define-test external-symbol-not-found
  #+(not (or sbcl ccl ecl abcl)) (not-supported-error))

(define-test symbol-not-defined
  :parent external-symbol-not-found
  (with-temp-package (test-package-12345)
    (handler-case (read-from-string "test-package-12345:symbol")
      (error (e)
        (true (external-symbol-not-found-p e))))))

(define-test symbol-not-exported
  :parent external-symbol-not-found
  (with-temp-package (test-package-12345)
    (handler-case (progn
                    (intern "SYMBOL" 'test-package-12345)
                    (read-from-string "test-package-12345:symbol"))
      (error (e)
        (true (external-symbol-not-found-p e))))))
