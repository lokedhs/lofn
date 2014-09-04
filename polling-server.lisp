(in-package :lofn)

(declaim #.*compile-decl*)

(define-condition request-polling ()
  ((init-fn :type function
            :initarg :init-fn
            :initform nil
            :reader request-polling/init-fn)
   (disconnect-callback :type function
                        :initarg :disconnect-callback
                        :initform nil
                        :reader request-polling/disconnect-callback))
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
                                                      (return-from process (list (request-polling/init-fn condition)
                                                                                 (request-polling/disconnect-callback condition))))))
                    (call-next-method)
                    nil))))
    (when result
      (register-polling-socket socket (first result) (second result)))))

(defvar *master-poll-waiting-sockets* (containers:make-blocking-queue))
(defvar *active-sockets* nil)
(defvar *poll-loop-thread* nil)

(define-condition socket-updated-condition ()
  ())

(defclass opened-socket ()
  ((socket :initarg :socket
           :reader opened-socket/socket)
   (disconnect-callback :type function
                        :initarg :disconnect-callback
                        :reader opened-socket/disconnect-callback)))

(defun register-polling-socket (socket init-fn disconnect-callback)
  (let ((opened-socket (make-instance 'opened-socket
                                      :socket socket
                                      :disconnect-callback disconnect-callback)))
    (when init-fn
      (funcall init-fn opened-socket))
    (containers:queue-push *master-poll-waiting-sockets* opened-socket)
    (bordeaux-threads:interrupt-thread *poll-loop-thread* #'(lambda () (signal 'socket-updated-condition)))))

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
                    (usocket:wait-for-input (mapcar #'opened-socket/socket *active-sockets*) :ready-only t)
                  (declare (ignore remaining))
                  (dolist (socket sockets)
                    (handler-case
                        (close (usocket:socket-stream socket))
                      (error () (format t "Error closing socket: ~s~%" socket)))
                    (let ((pair (find socket *active-sockets* :key #'opened-socket/socket)))
                      (setq *active-sockets* (delete socket *active-sockets* :key #'opened-socket/socket))
                      (alexandria:when-let ((callback(opened-socket/disconnect-callback pair)))
                        (funcall callback socket)))))
                ;; No active sockets, just sleep for a while
                (sleep 10))))))

(defun poll-loop-start ()
  (poll-loop))

(defvar *push-queue* (containers:make-blocking-queue))
(defvar *push-queue-thread* nil)

(defun push-queue-loop ()
  (loop
     for callback = (containers:queue-pop-wait *push-queue*)
     do (funcall callback)))

(defun start-poll-loop-thread ()
  (setq *poll-loop-thread* (bordeaux-threads:make-thread #'poll-loop-start :name "Poll loop"))
  (setq *push-queue-thread* (bordeaux-threads:make-thread #'push-queue-loop :name "Notification push queue")))

(defun start-polling (init-fn disconnect-callback)
  (check-type init-fn function)
  (check-type disconnect-callback function)
  (signal 'request-polling
          :init-fn init-fn
          :disconnect-callback disconnect-callback))

(defun start-polling-with-sources (sources)
  (let ((subscription (make-instance 'html5-notification:subscription
                                     :sources sources
                                     :http-event (hunchentoot:header-in* :last-event-id))))
    (with-slots (html5-notification::entries) subscription
      (labels ((push-update (e socket)
                 (multiple-value-bind (prefixed new-id) (html5-notification:updated-objects-from-entry e)
                   (html5-notification:with-locked-instance (subscription)
                     (setf (html5-notification:subscription-entry-last-id e) new-id)
                     (when prefixed
                       (containers:queue-push *push-queue*
                                              #'(lambda ()
                                                  (let ((stream (usocket:socket-stream (opened-socket/socket socket))))
                                                    (write-sequence (babel:string-to-octets "polle~%" :encoding :utf-8)
                                                                    stream)
                                                    (finish-output stream))))))))
               
               (init (socket)
                 (dolist (e html5-notification::entries)
                   (let ((entry e))
                     (html5-notification:add-listener entry #'(lambda () (push-update e socket))))))

               (remove-subscription (socket)
                 (declare (ignore socket))
                 (dolist (e html5-notification::entries)
                   (html5-notification:remove-listener e))))

        (start-polling #'init #'remove-subscription)))))
