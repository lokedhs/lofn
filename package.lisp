(defpackage #:lofn
  (:use #:cl)
  (:export #:start-server
           #:parse-template
           #:server-acceptor-mixin
           #:define-handler-fn
           #:hunchentoot-stream-as-text
           #:with-hunchentoot-stream
           #:show-template-stream
           #:define-json-handler-fn
           #:exec-template-file
           #:with-parameters
           #:*files-base-dir*
           #:*template-files-base-dir*
           #:*simple-files-base-dir*
           #:*template-cache-min-check*
           #:make-server-ssl
           #:server-acceptor
           #:start-poll-loop-thread
           #:polling-server-acceptor
           #:start-polling
           #:remove-listener
           #:start-polling-with-sources
           #:blocking-queue))

(in-package #:lofn)

(defparameter *compile-decl* '(optimize (speed 0) (safety 3) (debug 3)))
