;;;; external-symbol-not-found.asd

(asdf:defsystem #:external-symbol-not-found
  :description #.(format nil "Portability library for detecting reader ~
errors coming from reading non-existing or non-external symbols in packages")
  :author "Michał \"phoe\" Herda <phoe@teknik.io>"
  :license  "Unlicense"
  :version "0.0.2"
  :in-order-to ((test-op (load-op #:external-symbol-not-found/test)))
  :perform
  (test-op (o c)
           (symbol-call
            '#:parachute '#:test
            (find-symbol
             (symbol-name '#:external-symbol-not-found)
             (find-package '#:external-symbol-not-found.test))
            :report (find-symbol "INTERACTIVE"
                                 "PARACHUTE")))
  :serial t
  :components ((:file "package")
               (:file "external-symbol-not-found")))



(asdf:defsystem #:external-symbol-not-found/test
  :description "Tests for external-symbol-not-found"
  :author ("Michał \"phoe\" Herda <phoe@teknik.io>"
           "Francis St-Amour <fr.stamour@gmail.com")
  :depends-on (#:parachute)
  :license  "Unlicense"
  :version "0.0.2"
  :serial t
  :components ((:file "tests")))
