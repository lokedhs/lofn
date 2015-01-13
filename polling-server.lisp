(in-package :lofn)

(alexandria:define-constant +CRLF+ (format nil "~c~c" #\Return #\Newline) :test 'equal)

(define-condition request-polling ()
  ((init-fn               :type function
                          :initarg :init-fn
                          :initform nil
                          :reader request-polling/init-fn)
   (disconnect-callback   :type function
                          :initarg :disconnect-callback
                          :initform nil
                          :reader request-polling/disconnect-callback)
   (after-empty-callback  :type function
                          :initarg :after-empty-callback
                          :initform nil
                          :reader request-polling/after-empty-callback)
   (stream                :type stream
                          :initarg :stream
                          :reader request-polling/stream)
   (disconnect-after-send :type t
                          :initarg :disconnect-after-send
                          :initform nil
                          :reader request-polling/disconnect-after-send))
  (:documentation "Condition that is raised when the request should be
processed using polling."))

(defclass polling-server-acceptor (lofn:server-acceptor)
  ()
  (:documentation "Acceptor that can optionally use polling instead of
one thread per connection."))

(defmethod hunchentoot:process-connection ((acceptor polling-server-acceptor) socket)
  (let ((result (block process                  
                  (handler-bind ((request-polling #'(lambda (condition)
                                                      (hunchentoot:detach-socket acceptor)
                                                      (return-from process (list (request-polling/stream condition)
                                                                                 (request-polling/init-fn condition)
                                                                                 (request-polling/disconnect-callback condition)
                                                                                 (request-polling/after-empty-callback condition)
                                                                                 (request-polling/disconnect-after-send condition)
                                                                                 hunchentoot:*session*)))))
                    (call-next-method)
                    nil))))
    (when result
      (destructuring-bind (stream init-fn disconnect-callback after-empty-callback disconnect-after-send session) result
        (register-polling-socket socket stream init-fn disconnect-callback after-empty-callback disconnect-after-send session)))))

(defvar *master-poll-waiting-sockets* (dhs-sequences:make-blocking-queue))
(defvar *active-sockets* nil)
(defvar *poll-loop-thread* nil)
(defvar *timer-block-thread* nil
  "The thread that is responsible for processing timeout callbacks.")

(defvar *push-queue* (dhs-sequences:make-blocking-queue)
  "Queue containing notifications that data needs to be pushed to a
socket. Each element is a cons where car is the socket and cdr is a
function to be called on the socket.")
(defvar *push-queue-threads* nil
  "A list of all threads that push writes to the clients.")

(define-condition socket-updated-condition ()
  ())

(defclass opened-socket ()
  ((socket                 :type usocket:usocket
                           :initarg :socket
                           :reader opened-socket/socket)
   (disconnect-callback    :type function
                           :initarg :disconnect-callback
                           :reader opened-socket/disconnect-callback)
   (after-empty-callback   :type function
                           :initarg :after-empty-callback
                           :reader opened-socket/after-empty-callback)
   (timer                  :type trivial-timers:timer
                           :reader opened-socket/timer)
   (stream                 :type stream
                           :initarg :stream
                           :reader opened-socket/stream)
   (session                :type (or null hunchentoot:session)
                           :initarg :session
                           :reader opened-socket/session)
   (queue                  :type dhs-sequences:blocking-queue
                           :initform (dhs-sequences:make-blocking-queue)
                           :reader opened-socket/queue)
   (in-progress-p          :type dhs-sequences:atomic-variable
                           :initform (dhs-sequences:make-atomic-variable nil)
                           :reader opened-socket/in-progress-p)
   (disconnect-after-send  :type t
                           :initarg :disconnect-after-send
                           :initform nil
                           :reader opened-socket/disconnect-after-send)
   (discarded-p            :type t
                           :initform nil
                           :accessor opened-socket/discarded-p)))

(defmethod initialize-instance :after ((obj opened-socket) &rest rest)
  (declare (ignore rest))
  (setf (slot-value obj 'timer)
        (trivial-timers:make-timer #'(lambda ()
                                       (push-ping-message obj)
                                       (let ((after-empty-callback (opened-socket/after-empty-callback obj)))
                                         (log:trace "After ping, callback: ~s" after-empty-callback)
                                         (when after-empty-callback
                                           (funcall after-empty-callback obj))))
                                   :name "Ping timer"
                                   :thread *timer-block-thread*)))

(defun notify-poll-loop-updates ()
  (bordeaux-threads:interrupt-thread *poll-loop-thread* #'(lambda () (signal 'socket-updated-condition))))

(defun register-polling-socket (socket stream init-fn disconnect-callback after-empty-callback disconnect-after-send session)
  (let ((opened-socket (make-instance 'opened-socket
                                      :socket socket
                                      :disconnect-callback disconnect-callback
                                      :after-empty-callback after-empty-callback
                                      :stream stream
                                      :session session
                                      :disconnect-after-send disconnect-after-send)))
    (when init-fn
      (funcall init-fn opened-socket))
    (dhs-sequences:queue-push *master-poll-waiting-sockets* opened-socket)
    (notify-poll-loop-updates)))

(defun opened-socket/close (socket)
  (usocket:socket-close (opened-socket/socket socket)))

(defvar *poll-loop-wait-flag* (dhs-sequences:make-atomic-variable nil))

(defun poll-loop ()
  (labels ((copy-socket-list ()
             (loop
                for socket = (dhs-sequences:queue-pop *master-poll-waiting-sockets* :if-empty nil)
                while socket
                do (push socket *active-sockets*))))
    (loop
       do (block wait-for-disconnect
            (handler-bind ((socket-updated-condition #'(lambda (condition)
                                                         (declare (ignore condition))
                                                         (dhs-sequences:with-atomic-variable (v *poll-loop-wait-flag*)
                                                           (when v
                                                             (return-from wait-for-disconnect))))))
              
              (let ((s (unwind-protect
                            (progn
                              (dhs-sequences:with-atomic-variable (v *poll-loop-wait-flag*)
                                (setq v t)
                                (copy-socket-list))
                              (if *active-sockets*
                                  (multiple-value-bind (active discarded)
                                      (loop
                                         for opened in *active-sockets*
                                         for s = (opened-socket/socket opened)
                                         if (opened-socket/discarded-p opened)
                                         collect s into discarded
                                         else collect s into active
                                         finally (return (values active discarded)))
                                    (append (if active
                                                (multiple-value-bind (sockets remaining)
                                                    (usocket:wait-for-input active :ready-only t)
                                                  (declare (ignore remaining))
                                                  sockets)
                                                ;; ELSE: No active sockets
                                                nil)
                                            discarded))
                                  ;; ELSE: No active sockets, just sleep for a while
                                  (progn
                                    (sleep 10)
                                    nil)))
                         ;; Unwind form
                         (setf (dhs-sequences:atomic/value *poll-loop-wait-flag*) nil))))
                ;; s now contains a list of sockets that should be closed and dropped from
                ;; the list of active sockets.
                (dolist (socket s)
                  (handler-case
                      (usocket:socket-close socket)
                    (error () (log:error "Error closing socket: ~s~%" socket)))
                  (let ((pair (find socket *active-sockets* :key #'opened-socket/socket)))
                    (setq *active-sockets* (delete pair *active-sockets*))
                    (alexandria:when-let ((callback (opened-socket/disconnect-callback pair)))
                      (funcall callback pair))))))))))

(defun poll-loop-start ()
  (poll-loop))

(defun push-queue-loop ()
  (loop
     for socket = (dhs-sequences:queue-pop-wait *push-queue*)
     do (let ((should-send nil))
          (dhs-sequences:with-atomic-variable (v (opened-socket/in-progress-p socket))
            (unless v
              (setq should-send t)
              (setq v t)))
          (when should-send
            (loop
               for first = t then nil
               for callback = (dhs-sequences:with-atomic-variable (v (opened-socket/in-progress-p socket))
                                (let ((value (dhs-sequences:queue-pop (opened-socket/queue socket) :if-empty nil)))
                                  (or value
                                      (progn
                                        (unless first
                                          (finish-output (opened-socket/stream socket)))
                                        (when (opened-socket/disconnect-after-send socket)
                                          (setf (opened-socket/discarded-p socket) t)
                                          (notify-poll-loop-updates))
                                        (setf v nil)
                                        nil))))
               while callback
               do (handler-case
                      (funcall callback socket)
                    (error (condition)
                      (log:error "Error pushing message: ~s~%" condition)
                      (setf (opened-socket/discarded-p socket) t))))))))

(defun enqueue-on-push-queue (socket callback)
  (check-type socket opened-socket)
  (check-type callback function)
  (dhs-sequences:queue-push (opened-socket/queue socket) callback)
  (dhs-sequences:queue-push *push-queue* socket))

(defun timer-block-loop ()
  (loop
     do (sleep 10000)))

(defun start-poll-loop-thread ()
  (setq *poll-loop-thread* (bordeaux-threads:make-thread #'poll-loop-start :name "Poll loop"))
  (setq *push-queue-threads* (loop
                               repeat 10
                               for i from 0
                               collect (bordeaux-threads:make-thread #'push-queue-loop :name (format nil "Notification push queue: ~a" i))))
  (setq *timer-block-thread* (bordeaux-threads:make-thread #'timer-block-loop :name "Timer block loop")))

(defun push-ping-message (opened-socket)
  (enqueue-on-push-queue opened-socket
                         #'(lambda (socket)
                             (let ((out (opened-socket/stream socket)))
                               (html5-notification:send-ping-message out)
                               (finish-output out)
                               (alexandria:when-let ((session (opened-socket/session socket)))
                                 (setf (slot-value session 'hunchentoot::last-click) (get-universal-time)))))))

(defun start-polling (stream init-fn disconnect-callback after-empty-callback disconnect-after-send)
  (check-type init-fn function)
  (check-type disconnect-callback function)
  (signal 'request-polling
          :init-fn init-fn
          :disconnect-callback disconnect-callback
          :after-empty-callback after-empty-callback
          :stream stream
          :disconnect-after-send disconnect-after-send))

(defun start-polling-with-sources (sources &key after-write after-disconnect after-empty disconnect-after-send)
  (setf (hunchentoot:header-out :cache-control) "no-cache")
  (setf (hunchentoot:content-type*) "text/event-stream")
  (let ((stream (flexi-streams:make-flexi-stream (hunchentoot:send-headers) :external-format :utf8))
        (subscription (make-instance 'html5-notification:subscription
                                     :sources sources
                                     :http-event (hunchentoot:header-in* :last-event-id))))
    (with-slots (html5-notification::entries) subscription
      (labels ((push-update (socket e)
                 (multiple-value-bind (prefixed new-id) (html5-notification:updated-objects-from-entry e)
                   (html5-notification:with-locked-instance (subscription)
                     (setf (html5-notification:subscription-entry-last-id e) new-id)
                     (when prefixed
                       (let ((message (html5-notification:format-update-message-text subscription prefixed)))
                         (enqueue-on-push-queue socket
                                                #'(lambda (opened-socket)
                                                    (let ((s (opened-socket/stream opened-socket)))
                                                      (write-string message s))))
                         (when after-write
                           (funcall after-write)))))))
               
               (init (socket)
                 (dolist (e html5-notification::entries)
                   (let ((entry e))
                     (html5-notification:add-listener entry #'(lambda () (push-update socket e)))))
                 (log:trace "Scheduling timer for socket: ~s" socket)
                 (trivial-timers:schedule-timer (opened-socket/timer socket) 30
                                                :repeat-interval 30))

               (remove-subscription (socket)
                 (trivial-timers:unschedule-timer (opened-socket/timer socket))
                 (dolist (e html5-notification::entries)
                   (html5-notification:remove-listener e)
                   (when after-disconnect
                     (funcall after-disconnect))))

               (after-send-empty (socket)
                 (declare (ignore socket))
                 (log:trace "after-send-empty called, callback: ~s" after-empty)
                 (when after-empty
                   (funcall after-empty))))

        (start-polling stream #'init #'remove-subscription #'after-send-empty disconnect-after-send)))))
