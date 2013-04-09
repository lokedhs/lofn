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

(defclass server-acceptor (hunchentoot:acceptor)
  ((files-dispatcher :type list
                     :initarg :dispatcher-list
                     :reader files-dispatcher
                     :initform nil
                     :documentation "List of fallback dispatchers"))
  (:documentation "Main hunchentoot acceptor"))

(defun make-server (port file-dirs dispatcher-list)
  (make-instance 'server-acceptor
                 :port port
                 :dispatcher-list (append dispatcher-list
                                          (mapcar #'(lambda (p)
                                                      (let ((base (if (stringp p) p (car p))))
                                                        (hunchentoot:create-folder-dispatcher-and-handler
                                                         (if (and (listp p) (cadr p))
                                                             (cadr p)
                                                             (format nil "/~a/" base))
                                                         (merge-pathnames (format nil "~a/" base)
                                                                          (make-simple-files-base-dir)))))
                                                  file-dirs))))

(defmethod hunchentoot:acceptor-dispatch-request ((acceptor server-acceptor) request)
  (let ((handler (gethash (hunchentoot:script-name request) *url-handlers*)))
    (if handler
        (funcall handler)
        (loop
           for dispatcher in (files-dispatcher acceptor)
           for dis = (funcall dispatcher request)
           when dis
           return (funcall dis)
           finally (call-next-method)))))

(defun %make-define-handler-fn-form (docstring name body)
  `(defun ,name ()
     ,@(when docstring (list docstring))
     ,@body))

(defmacro define-handler-fn ((name url) &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (gethash ,url *url-handlers*) ',name)
     ,(multiple-value-bind (rem-forms declarations docstring)
                           (alexandria:parse-body body :documentation t)
                           (%make-define-handler-fn-form docstring name (append declarations rem-forms)))))

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

(defmacro define-json-handler-fn ((name url data-symbol) &body body)
  (check-type data-symbol symbol)
  (multiple-value-bind (rem-forms declarations docstring)
      (alexandria:parse-body body :documentation t)
    `(define-handler-fn (,name ,url)
       ,@(when docstring (list docstring))
       (process-json #'(lambda (,data-symbol) ,@declarations ,@rem-forms)))))

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

(defun start-server (&key (port 8080) dirs dispatcher-list)
  "Start lofn server with a HTTP listener on port PORT."
  (when *global-acceptor*
    (error "Server is already running"))

  (let ((a (make-server port dirs dispatcher-list)))
    (hunchentoot:start a)
    (setq *global-acceptor* a))

  (setq hunchentoot:*show-lisp-errors-p* t)
  (setq hunchentoot:*log-lisp-warnings-p* t)
  (setq hunchentoot:*log-lisp-backtraces-p* t)
  (setf (hunchentoot:acceptor-access-log-destination *global-acceptor*) (make-broadcast-stream))
  (values))
