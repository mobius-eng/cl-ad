(asdf:defsystem :cl-ad-tests
  :description "Automatic Differentiation Library: tests"
  :version "0.1.0"
  :author "Alexey Cherkaev (mobius-eng)"
  :license "LGPLv3"
  :depends-on (:fiveam :cl-ad)
  :serial t
  :components ((:file "cl-ad-tests")))
