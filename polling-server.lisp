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

(defvar *socket* nil)
(defvar *inhibit-close-usocket* nil)

#+nil
(defmethod hunchentoot:process-connection ((hunchentoot:*acceptor* polling-server-acceptor) (socket t))
  (let* ((socket-stream (hunchentoot::make-socket-stream socket hunchentoot:*acceptor*))
         (hunchentoot::*hunchentoot-stream* (hunchentoot:initialize-connection-stream hunchentoot:*acceptor* socket-stream))
         (*inhibit-close-usocket* nil))
    (unwind-protect
         ;; process requests until either the acceptor is shut down,
         ;; *CLOSE-HUNCHENTOOT-STREAM* has been set to T by the
         ;; handler, or the peer fails to send a request
         (loop
            (let ((hunchentoot::*close-hunchentoot-stream* t))
              (when (hunchentoot::acceptor-shutdown-p hunchentoot:*acceptor*)
                (return))
              (multiple-value-bind (headers-in method url-string protocol)
                  (hunchentoot::get-request-data hunchentoot::*hunchentoot-stream*)
                ;; check if there was a request at all
                (unless method
                  (return))
                ;; bind per-request special variables, then process the
                ;; request - note that *ACCEPTOR* was bound above already
                (let ((hunchentoot:*reply* (make-instance (hunchentoot:acceptor-reply-class hunchentoot:*acceptor*)))
                      (hunchentoot:*session* nil)
                      (transfer-encodings (cdr (hunchentoot::assoc* :transfer-encoding headers-in))))
                  (when transfer-encodings
                    (setq transfer-encodings
                          (hunchentoot::split "\\s*,\\s*" transfer-encodings))
                    (when (member "chunked" transfer-encodings :test #'equalp)
                      (cond ((hunchentoot::acceptor-input-chunking-p hunchentoot:*acceptor*)
                             ;; turn chunking on before we read the request body
                             (setf hunchentoot::*hunchentoot-stream* (hunchentoot::make-chunked-stream hunchentoot::*hunchentoot-stream*)
                                   (hunchentoot::chunked-stream-input-chunking-p hunchentoot::*hunchentoot-stream*) t))
                            (t (hunchentoot:hunchentoot-error "Client tried to use ~
chunked encoding, but acceptor is configured to not use it.")))))
                  (hunchentoot::with-acceptor-request-count-incremented (hunchentoot:*acceptor*)
                                                                        (hunchentoot:process-request (hunchentoot::acceptor-make-request hunchentoot:*acceptor* socket
                                                                                                                                         :headers-in headers-in
                                                                                                                                         :content-stream hunchentoot::*hunchentoot-stream*
                                                                                                                                         :method method
                                                                                                                                         :uri url-string
                                                                                                                                         :server-protocol protocol))))
                (finish-output hunchentoot::*hunchentoot-stream*)
                (setq hunchentoot::*hunchentoot-stream* (hunchentoot:reset-connection-stream hunchentoot:*acceptor* hunchentoot::*hunchentoot-stream*))
                (when hunchentoot::*close-hunchentoot-stream*
                  (return)))))
      (unless (eql socket-stream hunchentoot::*hunchentoot-stream*)
        ;; as we are at the end of the request here, we ignore all
        ;; errors that may occur while flushing and/or closing the
        ;; stream.
        (hunchentoot::ignore-errors*
         (finish-output hunchentoot::*hunchentoot-stream*))
        (hunchentoot::ignore-errors*
         (unless *inhibit-close-usocket*
           (close hunchentoot::*hunchentoot-stream* :abort t))))
      (when socket-stream
        ;; as we are at the end of the request here, we ignore all
        ;; errors that may occur while flushing and/or closing the
        ;; stream.
        (hunchentoot::ignore-errors*
         (finish-output socket-stream))
        (hunchentoot::ignore-errors*
         (unless *inhibit-close-usocket*
           (close socket-stream :abort t)))))))

(defclass wrapper-stream (trivial-gray-streams:trivial-gray-stream-mixin
                          trivial-gray-streams:fundamental-binary-input-stream
                          trivial-gray-streams:fundamental-binary-output-stream)
  ((delegate :initarg :delegate
             :initform (error "~s is a required argument for class ~s" :delegate 'wrapper-stream)
             :reader wrapper-stream-delegate)
   (context  :initarg :context
             :initform (error "~s is a required argument for class ~s" :context 'wrapper-stream)
             :reader wrapper-stream-context)
   (description :type string
                :initarg :description
                :initform ""
                :reader wrapper-stream-description)
   (inhibit-close :type t
                  :initform nil
                  :accessor wrapper-stream-inhibit-close)))

(defmethod trivial-gray-streams:stream-read-byte
    ((stream wrapper-stream))
  (read-byte (wrapper-stream-delegate stream)))

(defmethod trivial-gray-streams:stream-write-byte
    ((stream wrapper-stream) char)
  (write-byte char (wrapper-stream-delegate stream)))

(defmethod trivial-gray-streams:stream-read-sequence
    ((stream wrapper-stream) seq start end &key)
  (read-sequence seq (wrapper-stream-delegate stream)
                 :start start :end end))

(defmethod trivial-gray-streams:stream-write-sequence
    ((stream wrapper-stream) seq start end &key)
  (write-sequence seq (wrapper-stream-delegate stream)
                  :start start :end end))

(defmethod trivial-gray-streams:stream-finish-output
    ((stream wrapper-stream))
  (finish-output (wrapper-stream-delegate stream)))

(defmethod trivial-gray-streams:stream-force-output
    ((stream wrapper-stream))
  (force-output (wrapper-stream-delegate stream)))

(defmethod close ((stream wrapper-stream) &key abort)
  (unless (wrapper-stream-inhibit-close stream)
    (close (wrapper-stream-delegate stream) abort)))

(defun make-wrapped-stream (stream)
  (make-instance 'wrapper-stream :delegate stream :context nil))

(defclass stream-usocket-wrapper (usocket:stream-usocket)
  ((inhibit-close :type t
                  :initform nil
                  :accessor stream-usocket-wrapper-inhibit-close)))

(defgeneric mark-socket-as-inhibit (socket value))
(defmethod mark-socket-as-inhibit ((socket stream-usocket-wrapper) value)
  (setf (stream-usocket-wrapper-inhibit-close socket) value)
  (setf (wrapper-stream-inhibit-close (usocket:socket-stream socket)) value))

(defun make-wrapped-socket (socket)
  (check-type socket usocket:stream-usocket)
  ;;(closer-common-lisp:change-class socket 'stream-usocket-wrapper)
  (let* ((class (class-of socket))
         (wrapper-class (find-class 'stream-usocket-wrapper))
         (s (allocate-instance wrapper-class)))
    (dolist (slot (closer-mop:class-slots class))
      (setf (closer-mop:slot-value-using-class wrapper-class s slot)
            (closer-mop:slot-value-using-class class socket slot)))
    (initialize-instance s)
    (setf (usocket:socket-stream s) (make-wrapped-stream (usocket:socket-stream s)))
    s))

(defmethod hunchentoot:process-connection ((acceptor polling-server-acceptor) socket)
  (let ((wrapped-socket (make-wrapped-socket socket)))
    (let ((result (block process                  
                    (handler-bind ((request-polling #'(lambda (condition)
                                                        (declare (ignore condition))
                                                        (mark-socket-as-inhibit wrapped-socket t)
                                                        (return-from process :polling))))
                      (call-next-method acceptor (setq *socket* wrapped-socket))
                      nil))))
      (when (eq result :polling)
        (mark-socket-as-inhibit wrapped-socket nil)
        (register-polling-socket socket)))))

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
                    (usocket:wait-for-input *active-sockets*)
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
