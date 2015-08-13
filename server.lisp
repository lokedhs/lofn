(in-package :lofn)

(defvar *url-handlers* (make-hash-table :test 'equal)
  "A hash table keyed on the base URL that maps to the underlying handler function")

(defvar *regex-handlers* nil
  "A list of regex handlers. Each element is a list of the
form \(NAME REGEX)")

(defvar *current-acceptors* nil
  "A list of all active acceptors")

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

(defun make-server (address port file-dirs dispatcher-list acceptor-name)
  (make-instance acceptor-name
                 :address address
                 :port port
                 :dispatcher-list (make-dispatcher-list file-dirs dispatcher-list)))

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

(defmacro case-method (&body cases)
  (destructuring-bind (new-cases has-default-p)
      (loop
         with found = nil
         for c in cases
         unless (and (listp c)
                     (>= (length c) 2))
         do (error "Incorrectly formatted clause: ~s" c)
         when (eq (car c) t)
         do (setq found t)
         collect c into result-list
         finally (return (list result-list found)))
    (let ((method-sym (gensym)))
      `(let ((,method-sym (hunchentoot:request-method*)))
         (case ,method-sym
           ,@new-cases
           ,@(unless has-default-p
                     (list `(t (error "Illegal method")))))))))

(defun %make-define-handler-fn-form (docstring name bind-vars body)
  `(defun ,name ,bind-vars
     ,@(when docstring (list docstring))
     ,@body))

(defmacro define-handler-fn-internal ((name url regex bind-vars) &body body)
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

(defmacro define-handler-fn ((name url regex (&rest bind-vars)) &body body)
  `(define-handler-fn-internal (,name ,url ,regex ,bind-vars)
     ,@body))

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
  (log:trace "Showing template file: ~a, data: ~s" file data)
  (with-hunchentoot-stream (out)
    (show-template out file data)))

(defun process-json (fn)
  (check-type fn function)
  (let ((json-text (hunchentoot:raw-post-data :force-text t)))
    (let* ((data (st-json:read-json-from-string json-text))
           (result (funcall fn data)))
      (with-hunchentoot-stream (out "application/json")
        (st-json:write-json result out)))))

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
    `(define-handler-fn-internal (,name ,url ,regex ,bind-vars)
       ,@(when docstring (list docstring))
       ,(if data-symbol
            `(if (eq (hunchentoot:request-method*) :post)
                 (process-json #'(lambda (,data-symbol) ,@declarations ,@rem-forms))
                 ;; ELSE: Illegal request method
                 (progn
                   (setf (hunchentoot:return-code*) hunchentoot:+http-method-not-allowed+)
                   (st-json:jso "result" "error"
                                "message" "wrong request method")))
            `(process-json-no-data #'(lambda () ,@declarations ,@rem-forms))))))

(defmacro with-parameters ((&rest params) &body body)
  `(let ,(mapcar #'(lambda (v)
                     (let ((var (if (symbolp v) v (car v))))
                       (unless (and (not (null var)) (symbolp var))
                         (error "Parameter specification ~s is not valid" v))
                       (let ((param (if (and (listp v) (cadr v))
                                        (cadr v)
                                        (string-downcase (symbol-name var)))))
                         (let ((type (if (listp v) (third v) nil)))
                           (cond ((or (null type) (eq type :string))
                                  `(,var (hunchentoot:parameter ,param)))
                                 ((eq type :integer)
                                  `(,var (parse-integer (hunchentoot:parameter ,param))))
                                 ((eq type :integer-allow-null)
                                  (let ((sym (gensym)))
                                    `(,var (let ((,sym (hunchentoot:parameter ,param))) (if ,sym (parse-integer ,sym) nil)))))
                                 (t
                                  (error "Illegal type specifier: ~s" type)))))))
                 params)
     ,@body))

(alexandria:define-constant +BLANK-CHARS+ (map 'string #'identity '(#\Space #\Newline #\Return)) :test 'equal)

(defmacro with-checked-parameters ((&rest params) &body body)
  `(let ,(loop
            for param-spec in params
            collect (destructuring-bind (sym &key name required (type :string) (allow-blank t) trimmed) param-spec
                      (let ((name (or name (string-downcase (symbol-name sym))))
                            (value-sym (gensym)))
                        `(,sym (let ((,value-sym (hunchentoot:parameter ,name)))
                                 ,@(if required `((unless ,value-sym
                                                    (error "Missing value for: ~s" ,name))))
                                 ,@(if trimmed `((when ,value-sym
                                                   (setq ,value-sym (string-trim +BLANK-CHARS+ ,value-sym)))))
                                 ,@(if (not allow-blank) `((when (string= ,value-sym "")
                                                             (error "Value for ~s is empty" ,name))))
                                 ,(ecase type
                                         (:string value-sym)
                                         (:integer `(and ,value-sym (parse-integer ,value-sym)))
                                         (:boolean `(equal ,value-sym "1"))))))))
     ,@body))

(defun create-random-key ()
  (with-output-to-string (s)
    (dotimes (i (/ 128 4))
      (format s "~vr" 16 (secure-random:number 16)))))

(defun start-server (&key (address nil) (port 8080) dirs dispatcher-list
                       (acceptor-name 'server-acceptor))
  "Start lofn server with a HTTP listener on port PORT."

  (unless (subtypep acceptor-name 'server-acceptor)
    (error "Acceptor must be a subtype of SERVER-ACCEPTOR"))

  (setq hunchentoot:*session-secret* (create-random-key))

  (let ((a (make-server address port dirs dispatcher-list acceptor-name)))
    (hunchentoot:start a)
    (setq hunchentoot:*show-lisp-errors-p* t)
    (setq hunchentoot:*log-lisp-warnings-p* t)
    (setq hunchentoot:*log-lisp-backtraces-p* t)
    (setf (hunchentoot:acceptor-access-log-destination a) (make-broadcast-stream))
    (push a *current-acceptors*)
    (values)))
