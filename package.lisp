(defpackage #:lofn
  (:use #:cl)
  (:export #:start-server
           #:parse-template
           #:server-acceptor
           #:define-handler-fn
           #:hunchentoot-stream-as-text
           #:with-hunchentoot-stream
           #:show-template-stream
           #:define-json-handler-fn))

(in-package #:lofn)

(defparameter *compile-decl* '(optimize (speed 0) (safety 3) (debug 3)))
