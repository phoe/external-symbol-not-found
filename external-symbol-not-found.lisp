;;;; external-symbol-not-found.lisp

(in-package #:external-symbol-not-found)

(deftype external-symbol-not-found ()
  '(and condition (satisfies external-symbol-not-found-p)))

(defun external-symbol-not-found-p (condition)
  #+(not (or sbcl ccl ecl abcl)) (not-supported-error)
  (typecase condition
    #+sbcl (sb-int:simple-reader-package-error
            (or (search "Symbol ~S not found in the ~A package."
                        (simple-condition-format-control condition))
                (search "The symbol ~S is not external in the ~A package."
                        (simple-condition-format-control condition))))
    #+ccl (simple-error
           (search "No external symbol named ~S in package ~S"
                   (simple-condition-format-control condition)))
    #+ecl (simple-error
           (search "Cannot find the external symbol ~A in ~S."
                   (simple-condition-format-control condition)))
    #+abcl (reader-error
            (let ((string (princ-to-string condition)))
              (and (search "The symbol \"" string)
                   (or (search "\" is not external in package " string)
                       (search "\" was not found in package " string)))))))

(defun external-symbol-not-found-symbol-name (condition)
  #+(not (or sbcl ccl ecl abcl)) (not-supported-error)
  #+(or sbcl ccl ecl) (first (simple-condition-format-arguments condition))
  #+abcl (let ((string (princ-to-string condition)))
           (read-from-string string t nil :start 11)))

(defun external-symbol-not-found-package (condition)
  #+(not (or sbcl ccl ecl abcl)) (not-supported-error)
  #+(or sbcl ccl ecl) (second (simple-condition-format-arguments condition))
  #+abcl (let* ((string (princ-to-string condition))
                (position (if (search "\" was not found in package " string)
                            (+ (search "\" was not found in package " string)
                               (length "\" was not found in package "))
                            (+ (search "\" is not external in package " string)
                               (length "\" is not external in package "))))
                (package-name (read-from-string string t nil
                                                :start position
                                                :end (1- (length string)))))
           (find-package package-name)))

#+(not (or sbcl ccl ecl abcl))
(defun not-supported-error ()
  (error "Your implementation, ~A, is not supported. Please consider writing ~
a patch to this library." (lisp-implementation-type)))

(defun test ()
  #+(not (or sbcl ccl ecl abcl)) (not-supported-error)
  (unwind-protect
       (handler-case (progn
                       (make-package 'test-package-12345 :use nil)
                       (read-from-string "test-package-12345:symbol"))
         (error (e)
           (assert (external-symbol-not-found-p e))))
    (delete-package 'test-package-12345))
  (unwind-protect
       (handler-case (progn
                       (make-package 'test-package-12345 :use nil)
                       (intern "SYMBOL" 'test-package-12345)
                       (read-from-string "test-package-12345:symbol"))
         (error (e)
           (assert (external-symbol-not-found-p e))))
    (delete-package 'test-package-12345)))
