(in-package :lofn)

(declaim #.*compile-decl*)

(define-condition request-polling ()
  ((callback :type function
             :initarg :callback
             :initform nil
             :reader request-polling/callback))
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
                                                      (setq hunchentoot:*close-hunchentoot-stream* nil)
                                                      (return-from process (request-polling/callback condition)))))
                    (call-next-method)
                    nil))))
    (when result
      (register-polling-socket socket result))))

(defvar *master-poll-waiting-sockets* (containers:make-blocking-queue))
(defvar *active-sockets* nil)
(defvar *poll-loop-thread* nil)

(define-condition socket-updated-condition ()
  ())

(defun register-polling-socket (socket callback)
  (containers:queue-push *master-poll-waiting-sockets* (cons socket callback))
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
                    (usocket:wait-for-input (mapcar #'car *active-sockets*) :ready-only t)
                  (declare (ignore remaining))
                  (dolist (socket sockets)
                    (handler-case
                        (close (usocket:socket-stream socket))
                      (error () (format t "Error closing socket: ~s~%" socket)))
                    (let ((pair (find socket *active-sockets* :key #'car)))
                      (setq *active-sockets* (delete socket *active-sockets* :key #'car))
                      (when (cdr pair)
                        (funcall (cdr pair))))))
                ;; No active sockets, just sleep for a while
                (sleep 10))))))

(defun poll-loop-start ()
  (poll-loop))

(defun start-poll-loop-thread ()
  (setq *poll-loop-thread* (bordeaux-threads:make-thread #'poll-loop-start :name "Poll loop")))

(defun start-polling (callback)
  (check-type callback function)
  (signal 'request-polling :callback callback))

#+nil(lofn:define-handler-fn (foo-screen "/foo" nil ())
  (signal 'request-polling))
