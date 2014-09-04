(asdf:defsystem #:lofn
  :description "Web framework for Hunchentoot"
  :author "Elias Martenson <lokedhs@gmail.com>"
  :license "BSD"
  :serial t
  :depends-on (:hunchentoot
               :alexandria
               :st-json
               :parse-number
               :yacc
               :string-case
               :secure-random
               :containers
               :closer-mop
               :html5-notification)
  :components ((:file "package")
               (:file "util")
               (:file "parser")
               (:file "template")
               (:file "server")
               (:file "polling-server")))
