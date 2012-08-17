(asdf:defsystem #:lofn
  :serial t
  :description "Web framework for Hunchentoot"
  :author "Elias Martenson <lokedhs@gmail.com>"
  :license "BSD"
  :depends-on (:hunchentoot
               :alexandria
               :st-json
               :parse-number
               :yacc
               :string-case)
  :components ((:file "package")
               (:file "util")
               (:file "parser")
               (:file "server")))
