(asdf:defsystem :cl-ad
  :description "Automatic Differentiation Library"
  :version "0.1.0"
  :author "Alexey Cherkaev (mobius-eng)"
  :license "LGPLv3"
  :depends-on (:alexandria)
  :serial t
  :components ((:file "package")
               (:file "ad")))
