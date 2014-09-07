(in-package :lofn)

(declaim #.*compile-decl*)

(alexandria:define-constant +CRLF+ (format nil "~c~c" #\Return #\Newline) :test 'equal)

(define-condition request-polling ()
  ((init-fn             :type function
                        :initarg :init-fn
                        :initform nil
                        :reader request-polling/init-fn)
   (disconnect-callback :type function
                        :initarg :disconnect-callback
                        :initform nil
                        :reader request-polling/disconnect-callback)
   (stream              :type stream
                        :initarg :stream
                        :reader request-polling/stream))
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
                                                      (return-from process (list (request-polling/stream condition)
                                                                                 (request-polling/init-fn condition)
                                                                                 (request-polling/disconnect-callback condition))))))
                    (call-next-method)
                    nil))))
    (when result
      (destructuring-bind (stream init-fn disconnect-callback) result
          (register-polling-socket socket stream init-fn disconnect-callback)))))

(defvar *master-poll-waiting-sockets* (containers:make-blocking-queue))
(defvar *active-sockets* nil)
(defvar *poll-loop-thread* nil)
(defvar *timer-block-thread* nil)

(define-condition socket-updated-condition ()
  ())

(defclass opened-socket ()
  ((socket :initarg :socket
           :reader opened-socket/socket)
   (disconnect-callback :type function
                        :initarg :disconnect-callback
                        :reader opened-socket/disconnect-callback)
   (timer               :type trivial-timers:timer
                        :reader opened-socket/timer)
   (stream              :type stream
                        :initarg :stream
                        :reader opened-socket/stream)))

(defmethod initialize-instance :after ((obj opened-socket) &key &allow-other-keys)
  (setf (slot-value obj 'timer) (trivial-timers:make-timer #'(lambda ()
                                                               (push-ping-message obj))
                                                           :name "Ping timer"
                                                           :thread *timer-block-thread*)))

(defun register-polling-socket (socket stream init-fn disconnect-callback)
  (let ((opened-socket (make-instance 'opened-socket
                                      :socket socket
                                      :disconnect-callback disconnect-callback
                                      :stream stream)))
    (when init-fn
      (funcall init-fn opened-socket))
    (containers:queue-push *master-poll-waiting-sockets* opened-socket)
    (bordeaux-threads:interrupt-thread *poll-loop-thread* #'(lambda () (signal 'socket-updated-condition)))))

(defvar *poll-loop-wait-flag* (containers:make-atomic-variable nil))

(defun poll-loop ()
  (labels ((copy-socket-list ()
             (loop
                for socket = (containers:queue-pop *master-poll-waiting-sockets*
                                                   :if-empty nil)
                while socket
                do (push socket *active-sockets*))))
    (loop
       do (block wait-for-disconnect
            (handler-bind ((socket-updated-condition #'(lambda (condition)
                                                         (declare (ignore condition))
                                                         (containers:with-atomic-variable (v *poll-loop-wait-flag*)
                                                           (when v
                                                             (return-from wait-for-disconnect))))))
              
              (unwind-protect
                   (progn
                     (containers:with-atomic-variable (v *poll-loop-wait-flag*)
                       (setq v t)
                       (copy-socket-list))
                     (let ((s (if *active-sockets*
                                  (multiple-value-bind (sockets remaining)
                                      (usocket:wait-for-input (mapcar #'opened-socket/socket *active-sockets*) :ready-only t)
                                    (declare (ignore remaining))
                                    sockets)
                                  ;; No active sockets, just sleep for a while
                                  (progn
                                    (sleep 10)
                                    nil))))
                       (dolist (socket s)
                         (handler-case
                             (close (usocket:socket-stream socket))
                           (error () (format t "Error closing socket: ~s~%" socket)))
                         (let ((pair (find socket *active-sockets* :key #'opened-socket/socket)))
                           (setq *active-sockets* (delete socket *active-sockets* :key #'opened-socket/socket))
                           (alexandria:when-let ((callback (opened-socket/disconnect-callback pair)))
                             (funcall callback pair))))))
                (setf (containers:atomic/value *poll-loop-wait-flag*) nil)))))))

(defun poll-loop-start ()
  (poll-loop))

(defvar *push-queue* (containers:make-blocking-queue))
(defvar *push-queue-thread* nil)

(defun push-queue-loop ()
  (loop
     for callback = (containers:queue-pop-wait *push-queue*)
     do (handler-case
            (funcall callback)
          (error (condition) (format *out* "Error pushing message: ~s~%" condition)))))

(defun timer-block-loop ()
  (loop
     do (sleep 10000)))

(defun start-poll-loop-thread ()
  (setq *poll-loop-thread* (bordeaux-threads:make-thread #'poll-loop-start :name "Poll loop"))
  (setq *push-queue-thread* (bordeaux-threads:make-thread #'push-queue-loop :name "Notification push queue"))
  (setq *timer-block-thread* (bordeaux-threads:make-thread #'timer-block-loop :name "Timer block loop")))

(defun push-ping-message (opened-socket)
  (containers:queue-push *push-queue*
                         #'(lambda ()
                             (let ((out (opened-socket/stream opened-socket)))
                               (html5-notification:send-ping-message (opened-socket/stream opened-socket))
                               (finish-output out)))))

(defun start-polling (stream init-fn disconnect-callback)
  (check-type init-fn function)
  (check-type disconnect-callback function)
  (signal 'request-polling
          :init-fn init-fn
          :disconnect-callback disconnect-callback
          :stream stream))

(defun start-polling-with-sources (sources &key after-write-callback disconnect-callback)
  (setf (hunchentoot:header-out :cache-control) "no-cache")
  (setf (hunchentoot:content-type*) "text/event-stream")
  (let ((stream (flexi-streams:make-flexi-stream (hunchentoot:send-headers) :external-format :utf8))
        (subscription (make-instance 'html5-notification:subscription
                                     :sources sources
                                     :http-event (hunchentoot:header-in* :last-event-id))))
    (with-slots (html5-notification::entries) subscription
      (labels ((push-update (e)
                 (multiple-value-bind (prefixed new-id) (html5-notification:updated-objects-from-entry e)
                   (html5-notification:with-locked-instance (subscription)
                     (setf (html5-notification:subscription-entry-last-id e) new-id)
                     (when prefixed
                       (let ((message (html5-notification:format-update-message-text subscription prefixed)))
                         (containers:queue-push *push-queue*
                                                #'(lambda ()
                                                    (write-string message stream)
                                                    (finish-output stream)))
                         (when after-write-callback
                           (funcall after-write-callback)))))))
               
               (init (socket)
                 (dolist (e html5-notification::entries)
                   (let ((entry e))
                     (html5-notification:add-listener entry #'(lambda () (push-update e)))))
                 (trivial-timers:schedule-timer (opened-socket/timer socket) 30
                                                :repeat-interval 30))

               (remove-subscription (socket)
                 (trivial-timers:unschedule-timer (opened-socket/timer socket))
                 (dolist (e html5-notification::entries)
                   (html5-notification:remove-listener e)
                   (when disconnect-callback
                     (funcall disconnect-callback)))))

        (start-polling stream #'init #'remove-subscription)))))
