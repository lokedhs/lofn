(in-package :lofn)

(declaim #.*compile-decl*)

(defvar *files-base-dir*
  (asdf:component-pathname (asdf:find-system :lofn)))

(defvar *template-files-base-dir* nil)

(defun make-template-files-base-dir ()
  (or *template-files-base-dir* (merge-pathnames #p"template/" *files-base-dir*)))

(defvar *simple-files-base-dir* nil)

(defun make-simple-files-base-dir ()
  (or *simple-files-base-dir* (merge-pathnames #p"files/" *files-base-dir*)))

(defvar *url-handlers* (make-hash-table :test 'equal)
  "A hash table keyed on the base URL that maps to the underlying handler function")

(defvar *regex-handlers* nil
  "A list of regex handlers. Each element is a list of the
form \(NAME REGEX)")

(defclass server-acceptor-mixin ()
  ((files-dispatcher :type list
                     :initarg :dispatcher-list
                     :reader files-dispatcher
                     :initform nil
                     :documentation "List of fallback dispatchers"))
  (:documentation "Main hunchentoot acceptor"))

(defclass server-acceptor (server-acceptor-mixin hunchentoot:acceptor)
  ())

(defclass server-acceptor-ssl (server-acceptor-mixin hunchentoot:ssl-acceptor)
  ())

(defun append-slash (dir)
  (if (char/= (aref dir (1- (length dir))) #\/)
      (concatenate 'string dir "/")
      dir))

(defun make-dispatcher-list (file-dirs dispatcher-list)
  (append dispatcher-list
          (mapcar #'(lambda (p)
                      (let ((base (if (stringp p) p (car p))))
                        (hunchentoot:create-folder-dispatcher-and-handler
                         (if (and (listp p) (cadr p))
                             (cadr p)
                             (format nil "/~a/" base))
                         (merge-pathnames (append-slash base)
                                          (make-simple-files-base-dir)))))
                  file-dirs)))

(defun make-server (address port file-dirs dispatcher-list)
  (make-instance 'server-acceptor
                 :address address
                 :port port
                 :dispatcher-list (make-dispatcher-list file-dirs dispatcher-list)))

(defun make-server-ssl (address port file-dirs dispatcher-list certificate-file ssl-privatekey-file &optional ssl-privatekey-password)
  (make-instance 'server-acceptor-ssl
                 :address address
                 :port port
                 :dispatcher-list (make-dispatcher-list file-dirs dispatcher-list)
                 :certificate-file certificate-file
                 :ssl-privatekey-file ssl-privatekey-file
                 :ssl-privatekey-password ssl-privatekey-password))

(defmethod hunchentoot:acceptor-dispatch-request ((acceptor server-acceptor-mixin) request)
  (let* ((script-name (hunchentoot:script-name request))
         (handler (gethash script-name *url-handlers*)))
    (if handler
        (funcall handler)
        (block check-for-handlers
          (loop
             for (name regex) in *regex-handlers*
             do (multiple-value-bind (match strings)
                    (cl-ppcre:scan-to-strings regex script-name)
                  (when match
                    (return-from check-for-handlers (apply name (coerce strings 'list))))))
          (loop
             for dispatcher in (files-dispatcher acceptor)
             for dis = (funcall dispatcher request)
             when dis
             do (return-from check-for-handlers (funcall dis))
             finally (call-next-method))))))

(defun %make-define-handler-fn-form (docstring name bind-vars body)
  `(defun ,name ,bind-vars
     ,@(when docstring (list docstring))
     ,@body))

(defmacro define-handler-fn ((name url regex (&rest bind-vars)) &body body)
  (check-type name symbol)
  (check-type url string)
  (check-type regex (or null (eql t)))
  (check-type bind-vars list)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,(multiple-value-bind (rem-forms declarations docstring)
                           (alexandria:parse-body body :documentation t)
                           (%make-define-handler-fn-form docstring name bind-vars (append declarations rem-forms)))
     ,(if regex
          `(setq *regex-handlers* (cons (list ',name (cl-ppcre:create-scanner (concatenate 'string "^" ,url "$")))
                                        (remove ',name *regex-handlers* :key #'car)))
          `(setf (gethash ,url *url-handlers*) ',name))))

(defun hunchentoot-stream-as-text (&key (content-type "text/html") (append-charset t))
  "Sends the appropriate headers to ensure that all data is sent back using
the correct encoding and returns a text stream that the result can be
written to."
  (when content-type
    (setf (hunchentoot:content-type*)
          (if append-charset
              (format nil "~a;charset=UTF-8" content-type)
              content-type)))
  (flexi-streams:make-flexi-stream (hunchentoot:send-headers) :external-format :utf8))

(defmacro with-hunchentoot-stream ((out &optional (content-type "text/html") (append-charset t)) &body body)
  `(let ((,out (hunchentoot-stream-as-text :content-type ,content-type :append-charset ,append-charset)))
     ,@body))

(defun show-template (out file data)
  (exec-template-file (merge-pathnames file (make-template-files-base-dir))
                      data out
                      :binary t
                      :encoding :utf-8))

(defun show-template-stream (file data)
  (with-hunchentoot-stream (out)
    (show-template out file data)))

(defun process-json (fn)
  (check-type fn function)
  (let* ((data (st-json:read-json (hunchentoot:raw-post-data :want-stream t)))
         (result (funcall fn data)))
    (with-hunchentoot-stream (out "application/json")
      (st-json:write-json result out))))

(defun process-json-no-data (fn)
  (check-type fn function)
  (let ((result (funcall fn)))
    (with-hunchentoot-stream (out "application/json")
      (st-json:write-json result out))))

(defmacro define-json-handler-fn ((name url data-symbol regex (&rest bind-vars)) &body body)
  (check-type name symbol)
  (check-type url string)
  (check-type data-symbol symbol)
  (check-type regex (or null (eql t)))
  (check-type bind-vars list)
  (multiple-value-bind (rem-forms declarations docstring)
      (alexandria:parse-body body :documentation t)
    `(define-handler-fn (,name ,url ,regex ,bind-vars)
       ,@(when docstring (list docstring))
       ,(if data-symbol
            `(process-json #'(lambda (,data-symbol) ,@declarations ,@rem-forms))
            `(process-json-no-data #'(lambda () ,@declarations ,@rem-forms))))))

(defmacro with-parameters (params &body body)
  `(let ,(mapcar #'(lambda (v)
                     (let ((var (if (symbolp v) v (car v))))
                       (unless (and (not (null var)) (symbolp var))
                         (error "Parameter specification ~s is not valid" v))
                       (let ((param (if (and (listp v) (cadr v))
                                        (cadr v)
                                        (string-downcase (symbol-name var)))))
                         `(,var (hunchentoot:parameter ,param)))))
                 params)
     ,@body))

(defvar *global-acceptor* nil
  "The acceptor for the currently running server.")

(defun create-random-key ()
  (with-output-to-string (s)
    (dotimes (i (/ 128 4))
      (format s "~vr" 16 (secure-random:number 16)))))

(defun start-server (&key (address nil) (port 8080) dirs dispatcher-list)
  "Start lofn server with a HTTP listener on port PORT."
  (when *global-acceptor*
    (error "Server is already running"))

  (setq hunchentoot:*session-secret* (create-random-key))

  (let ((a (make-server address port dirs dispatcher-list)))
    (hunchentoot:start a)
    (setq *global-acceptor* a))

  (setq hunchentoot:*show-lisp-errors-p* t)
  (setq hunchentoot:*log-lisp-warnings-p* t)
  (setq hunchentoot:*log-lisp-backtraces-p* t)
  (setf (hunchentoot:acceptor-access-log-destination *global-acceptor*) (make-broadcast-stream))
  (values))
