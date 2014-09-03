(in-package :lofn)

(declaim #.*compile-decl*)

(define-condition request-polling ()
  ()
  (:documentation "Condition that is raised when the request should be
processed using polling."))

(defclass polling-server-acceptor (lofn:server-acceptor)
  ()
  (:documentation "Acceptor that can optionally use polling instead of
one thread per connection."))

(defparameter *out* *standard-output*)

(defmethod hunchentoot:process-connection ((acceptor polling-server-acceptor) socket)
  (let ((result (block process                  
                  (handler-bind ((request-polling #'(lambda (condition)
                                                      (declare (ignore condition))
                                                      (setq hunchentoot:*close-hunchentoot-stream* nil)
                                                      (return-from process :polling))))
                    (call-next-method)
                    nil))))
    (when (eq result :polling)
      (register-polling-socket socket))))

(defvar *master-poll-waiting-sockets* (containers:make-blocking-queue))
(defvar *active-sockets* nil)
(defvar *poll-loop-thread* nil)

(define-condition socket-updated-condition ()
  ())

(defun register-polling-socket (socket)
  (containers:queue-push *master-poll-waiting-sockets* socket)
  ;; TODO: tell the listener to listen
  (bordeaux-threads:interrupt-thread *poll-loop-thread* #'(lambda () (signal 'socket-updated-condition))))

(defun poll-loop ()
  (loop
     do (block wait-for-disconnect
          (handler-bind ((socket-updated-condition #'(lambda (condition)
                                                       (declare (ignore condition))
                                                       (loop
                                                          for socket = (containers:queue-pop *master-poll-waiting-sockets*
                                                                                             :if-empty nil)
                                                          while socket
                                                          do (push socket *active-sockets*))
                                                       (return-from wait-for-disconnect))))
            (if *active-sockets*
                (multiple-value-bind (sockets remaining)
                    (usocket:wait-for-input *active-sockets* :ready-only t)
                  (declare (ignore remaining))
                  (dolist (socket sockets)
                    (handler-case
                        (close (usocket:socket-stream socket))
                      (error () (format t "Error closing socket: ~s~%" socket)))
                    (setq *active-sockets* (delete socket *active-sockets*))))
                ;; No active sockets, just sleep for a while
                (sleep 10))))))

(defun poll-loop-start ()
  (poll-loop))

(defun start-poll-loop-thread ()
  (setq *poll-loop-thread* (bordeaux-threads:make-thread #'poll-loop-start :name "Poll loop")))

#+nil(lofn:define-handler-fn (foo-screen "/foo" nil ())
  (signal 'request-polling))
