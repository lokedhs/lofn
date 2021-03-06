(in-package :lofn)

(defvar *begin-code* "<%"
  "The string that opens a script segment. The end of a script segment is
specified by *END-CODE*.")
(defvar *end-code* "%>"
  "The string that ends a script segment.")

(defvar *current-line-num* nil
  "Dynamic variable used to track the current line number during parsing")

(defvar *output-binary* nil)
(defvar *output-encoding* nil)
(defvar *subtemplate-list* nil)
(defvar *variables-list* nil)
(defvar *include-root-dir* nil)
(defvar *current-stream* nil "The stream that the template output is written to.")
(defvar *files* nil)
(defvar *index-values* nil)

(define-condition template-error (error)
  ((line          :type integer
                  :initarg :line
                  :initform (error "~s required when creating ~s" :line 'template-error)
                  :reader template-error-line
                  :documentation "The line number where the error occurred.")
   (column        :type (or nil integer)
                  :initarg :column
                  :initform nil
                  :reader template-error-column
                  :documentation "The column index of the line where the error
occurred, if available. Otherwise NIL.")
   (message       :type string
                  :initarg :message
                  :initform (error "~s required when creating ~s" :message 'template-error)
                  :reader template-error-message
                  :documentation "The error message")
   (content       :type (or nil string)
                  :initarg :content
                  :initform nil
                  :reader template-error-content
                  :documentation "The actual template content where the error
occurred. Either the entire line, or part of it.")
   (content-index :type (or nil integer)
                  :initarg :content-index
                  :initform nil
                  :reader template-error-content-index
                  :documentation "The position in content closest to the actual error,
or NIL if the information is not available."))
  (:documentation "Error that is raised if there is an error parsing a template")
  (:report (lambda (condition stream)
             (with-slots (line column message content content-index) condition
               (format stream "Line ~a" line)
               (when column
                 (format stream ", column ~a" column))
               (format stream ": ~a" message)
               (when content
                 (format stream "~%~a~%~,,v@a" content content-index "^"))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-lexer-actions-list (definitions)
    (mapcar #'(lambda (definition)
                (destructuring-bind (regex action)
                    definition
                  (list (cl-ppcre:create-scanner (concatenate 'string "^" regex))
                        (etypecase action
                          (symbol (constantly (list action nil)))
                          (function action)))))
            definitions))

  (defmacro make-lexer-actions (&rest definitions)
    `(make-lexer-actions-list (list ,@(mapcar #'(lambda (definition)
                                                  `(list ,(car definition) ,(cadr definition)))
                                              definitions))))  
) ; EVAL-WHEN

(defparameter *actions*
  (make-lexer-actions ("[ \\n]+"      (constantly :blank))
                      ("#\\|.*\\|#"   (constantly :blank))
                      ("if"           'if)
                      ("else"         'else)
                      ("end"          'end)
                      ("while"        'while)
                      ("repeat"       'repeat)
                      ("for"          'for)
                      ("with"         'with)
                      ("template"     'deftemplate)
                      ("call"         'call)
                      ("include"      'include)
                      ("var"          'var)
                      ("includeindex" 'json-index)
                      ("index"        'json-index-value)
                      ("and"          'and)
                      ("or"           'or)
                      (","            '|,|)
                      ("="            '|=|)
                      ("=="           '|==|)
                      ("<"            '|<|)
                      (">"            '|>|)
                      ("<="           '|<=|)
                      (">="           '|>=|)
                      ("\\+"          '|+|)
                      ("-"            '|-|)
                      ("\\*"          '|*|)
                      ("\\("          '|(|)
                      ("\\)"          '|)|)
                      ("@"            '|@|)
                      ("([a-z]?)#"    (lambda (exprs) (list 'print (aref exprs 0))))
                      ("\\."          '|.|)
                      ("/"            '|/|)
                      (":"            '|:|)
                      ("!"            '|!|)
                      ("\\$"          '|$|)
                      (":([a-zA-Z][a-zA-Z_0-9-]*)" (lambda (exprs) (list 'quoted-keyword (aref exprs 0))))
                      ("([a-zA-Z_][a-zA-Z_0-9-]*)" (lambda (exprs) (list 'symbol (aref exprs 0))))
                      ("\"((?:(?:\\\\\")|[^\"])*)\"" (lambda (exprs)
                                                       (list 'string (escape-string-slashes (aref exprs 0)))))
                      ("([0-9]+)" (lambda (exprs) (list 'number (parse-number:parse-number (aref exprs 0)))))))

(defun signal-template-error (message &optional column content content-index)
  (error 'template-error
         :line *current-line-num*
         :column column
         :message message
         :content content
         :content-index content-index))

(defun escape-string-slashes (string)
  (with-output-to-string (out)
    (with-input-from-string (in string)
      (loop
         for ch = (read-char in nil nil)
         for i from 0
         while ch
         do (cond ((char= ch #\\)
                   (case (prog1 (read-char in) (incf i))
                     ((#\") (write-char #\" out))
                     (t     (signal-template-error "Illegal escape sequence"
                                                   nil (format nil "\"~a\"" string) (1+ i)))))
                  (t
                   (write-char ch out)))))))

(defun make-stream-template-lexer (input-stream)
  (let ((lexer-actions *actions*)
        (state :template)
        (current-line nil)
        (current-position 0)
        (input-finish nil))

    #'(lambda ()
        (labels ((read-template ()
                   (let ((pos (search *begin-code* current-line :start2 current-position)))
                     (if (null pos)
                         ;; no start code on this line, return the entire line
                         (let ((result (subseq current-line current-position)))
                           (setq current-position (length current-line))
                           (list 'template result))
                         ;; start code found, return the prefix and switch state
                         (let ((result (subseq current-line current-position pos)))
                           (setq current-position (+ pos (length *begin-code*)))
                           (setq state :code)
                           (list 'template result)))))

                 (read-code ()
                   (cond ((and (>= (length current-line)
                                   (+ current-position (length *end-code*)))
                               (string= current-line *end-code*
                                        :start1 current-position
                                        :end1 (+ current-position (length *end-code*))))
                          ;; End code was found
                          (incf current-position (length *end-code*))
                          (setq state :template)
                          :blank)
                         (t
                          ;; No end code found, check the actions
                          (loop
                             with longest-match-length = 0
                             with longest-match-exprs = nil
                             with longest-match-action = nil
                             for (regex action) in lexer-actions                      
                             do (multiple-value-bind (result exprs)
                                    (cl-ppcre:scan-to-strings regex current-line :start current-position)
                                  (when (and result
                                             (> (length result) longest-match-length))
                                    (setq longest-match-length (length result))
                                    (setq longest-match-exprs exprs)
                                    (setq longest-match-action action)))
                             finally (cond ((plusp longest-match-length)
                                            (incf current-position longest-match-length)
                                            (return (funcall longest-match-action longest-match-exprs)))
                                           (t
                                            (signal-template-error "Syntax error"
                                                                   current-position
                                                                   current-line
                                                                   current-position)))))))

                 (read-next-line ()
                   (unless input-finish
                     (setq current-line (read-line input-stream nil nil))
                     (incf *current-line-num*)
                     (setq current-position 0)
                     (cond (current-line
                            :blank)
                           (t
                            (setq input-finish t)
                            nil))))

                 (parse-token ()
                   (cond ((null current-line)
                          (read-next-line))
                         ((>= current-position (length current-line))
                          (if (eq state :template)
                              ;; If processing the template part, return a newline
                              (progn
                                (read-next-line)
                                (list 'template (string #\Newline)))
                              ;; Else, simply process the next line
                              (read-next-line)))
                         (t
                          (ecase state
                            (:template (read-template))
                            (:code (read-code)))))))

          (loop
             for token = (parse-token)
             while token
             unless (eq token :blank)
             return (apply #'values token))))))

(defun debug-lexer (string)
  (with-input-from-string (s string)
    (let ((lex (make-stream-template-lexer s)))
      (loop
         for (a b) = (multiple-value-list (funcall lex))
         while a
         do (format t "~s ~s~%" a b)))))

(defun string->symbol (symbol-name &optional (package *package*))
  (intern (string-upcase (string symbol-name)) package))

(defmacro short-define-parser (name initials &body definitions)
  (labels ((process-row (row)
             (let* ((arguments (car row))
                    (param-list (mapcar (lambda (arg) (if (listp arg) (cadr arg) arg)) arguments)))
               (append (mapcar (lambda (arg) (if (listp arg) (car arg) arg)) arguments)
                       (when (cadr row) (list `(lambda ,param-list
                                                 (declare (ignorable ,@param-list))
                                                 ,@(cdr row)))))))

           (process-definition (definition)
             (append (list (car definition))
                     (mapcar #'process-row (cdr definition)))))

    `(yacc:define-parser ,name
       ,@initials
       ,@(mapcar #'process-definition definitions))))

(short-define-parser *template-parser* ((:start-symbol document)
                                        (:terminals (template symbol string if end else while repeat number
                                                              for with deftemplate call include print var
                                                              and or
                                                              quoted-keyword json-index json-index-value
                                                              |,| |=| |(| |)| |@| |.| |/| |:| |!| |==|
                                                              |<| |>| |<=| |>=| |+| |-| |*| |$|))
                                        (:precedence ((:right template))))
                     
  (document
   ((document-nodes)
    `(progn ,@document-nodes (values))))

  (document-nodes
   ((document-node document-nodes)
    (if document-node
        (append (list document-node) document-nodes)
        document-nodes))
   (nil))

  (template-list
   ((template template-list)
    (concatenate 'string template template-list))
   ((template)
    template))

  (document-node
   ((template-list)
    (when (plusp (length template-list))
      (if *output-binary*
          `(write-sequence ,(babel:string-to-octets template-list :encoding *output-encoding*) current-output)
          `(princ ,template-list current-output))))

   ((if expression document-nodes else-statement end)
    `(if ,expression (progn ,@document-nodes) ,else-statement))

   ((while expression document-nodes end)
    `(loop while ,expression do (progn ,@document-nodes)))

   ((repeat number-expr (optional-variable-assignment var-name) document-nodes end)
    (let ((sym (if var-name
                   (string->symbol var-name)
                   (gensym))))
      `(loop for ,sym from 0 below ,number-expr do (progn ,@document-nodes))))

   ((for data (optional-variable-assignment var-name) document-nodes end)
    (let ((sym (if var-name
                   (string->symbol var-name)
                   (gensym))))
      `(loop
          for ,sym in ,data
          do (let ((current-content ,sym)) ,@document-nodes))))

   (((print modifier) data)
    (string-case:string-case (modifier)
      ("r" `(princ ,data current-output))
      ("e" `(princ (url-rewrite:url-encode (princ-to-string ,data)) current-output))
      ("" `(escape-string-minimal-plus-quotes (princ-to-string ,data) current-output))
      (t (signal-template-error (format nil "Unknown #-modifier: \"~a\"" modifier)))))

   ((deftemplate symbol document-nodes end)
    (let ((function-sym (gensym)))
      (setf (gethash symbol *subtemplate-list*) (cons function-sym document-nodes))
      nil))

   ((call symbol)
    (let ((function-code (gethash symbol *subtemplate-list*)))
      (unless function-code
        (signal-template-error (format nil "Attempting to call subtemplate \"~a\" which has not been defined." symbol)))
      `(,(car function-code) current-content)))

   ((include string)
    (let ((filename (merge-pathnames (pathname string) *include-root-dir*)))
      (pushnew filename *files* :test #'equal)
      (with-open-file (file-in filename :if-does-not-exist nil :external-format :utf-8)
        (unless file-in
          (signal-template-error (format nil "Failed to open include file \"~a\", file does not exist." filename)))
        (inner-parse-stream-to-form file-in))))

   ((var (symbol varname) |=| expression)
    (let ((symbol (string->symbol varname)))
      (pushnew symbol *variables-list*)
      `(setq ,symbol ,expression)))

   ((json-index string)
    (let ((filename (merge-pathnames (pathname string) *include-root-dir*)))
      (pushnew filename *files* :test #'equal)
      (with-open-file (file-in filename :if-does-not-exist nil)
        (unless file-in
          (signal-template-error (format nil "Failed to open index file \"~a\", file does not exist." filename)))
        (let ((*read-eval* nil))
          (let ((data (read file-in)))
            (loop
              for (key . value) in data
              unless (stringp key)
                do (signal-template-error (format nil "In index file \"~a\", key ~s is not a string."
                                                  filename key))
              unless (stringp value)
                do (signal-template-error (format nil "In index file \"~a\", value for key \"~a\" is not a string."
                                                  filename key))
              do (setf (gethash (format nil "~a.~a" string key) *index-values*) value)))))))

   ) ; end of DOCUMENT-NODE

  (else-statement
   ((else document-nodes)
    `(progn ,@document-nodes))
   nil)

  (optional-variable-assignment
   ((with symbol) symbol)
   nil)

  (json-index-default-marker
   ((|$| string) string)
   nil)

  (data
   ((symbol)         `(cdr (assoc ,(string->symbol symbol "KEYWORD") current-content)))
   ((quoted-keyword) (string->symbol quoted-keyword "KEYWORD"))
   ((|.|)            'current-content)
   ((|,| symbol)     (string->symbol symbol))
   ((string)         string)
   (((|/| v1) interned-symbol (|/| v3) expression)
    `(funcall '      ,interned-symbol ,expression))
   ((|!| expression) `(not ,expression))
   ((number-expr)    number-expr)
   ((json-index-value (string key) json-index-default-marker)
    (multiple-value-bind (value exists-p) (gethash key *index-values*)
      (if exists-p value (or json-index-default-marker key)))))

  (expression
   ((data) data)
   ((|(| wrap-expression |)|) wrap-expression))

  (wrap-expression
   ((expression)
    expression)
   (((expression e1) and (expression e2))
    `(and ,e1 ,e2))
   (((expression e1) or (expression e2))
    `(or ,e1 ,e2))
   (((expression e1) |==| (expression e2))
    `(equal ,e1 ,e2))
   (((expression e1) |<| (expression e2))
    `(< ,e1 ,e2))
   (((expression e1) |>| (expression e2))
    `(> ,e1 ,e2))
   (((expression e1) |<=| (expression e2))
    `(<= ,e1 ,e2))
   (((expression e1) |>=| (expression e2))
    `(>= ,e1 ,e2))
   (((expression e1) |+| (expression e2))
    `(+ ,e1 ,e2))
   (((expression e1) |-| (expression e2))
    `(- ,e1 ,e2))
   (((expression e1) |*| (expression e2))
    `(* ,e1 ,e2))
   (((expression e1) |/| (expression e2))
    `(/ ,e1 ,e2)))

  (number-expr
   ((number) number))

  (interned-symbol
   (((symbol symbol1) |:| (symbol symbol2))
    (intern (string-upcase symbol2) (string-upcase symbol1)))
   (((symbol symbol1))
    (intern (string-upcase symbol1) "CL-USER")))

) ; end of SHORT-DEFINE-PARSER

(defpackage :lofn-parse)

(defun inner-parse-stream-to-form (stream)
  (let ((*package* (find-package :lofn-parse))
        (*current-line-num* 0))
    (handler-case
        (let ((form (yacc:parse-with-lexer (make-stream-template-lexer stream) *template-parser*)))
          `(labels ,(loop
                       for value being each hash-value in *subtemplate-list*
                       collect `(,(car value) (current-content)
                                  (declare (ignorable current-content))
                                  ,@(cdr value)))
             ,@(loop
                  for value being each hash-value in *subtemplate-list*
                  collect `(declare (ignorable (function ,(car value)))))
             ,form))
      (yacc:yacc-parse-error (condition) (signal-template-error
                                          (format nil "Parse error: terminal=~s value=~s expected=~s"
                                                  (yacc:yacc-parse-error-terminal condition)
                                                  (yacc:yacc-parse-error-value condition)
                                                  (yacc:yacc-parse-error-expected-terminals condition)))))))

(defun parse-stream-to-form (stream binary encoding include-root-dir)
  (let ((*output-binary* binary)
        (*output-encoding* encoding)
        (*subtemplate-list* (make-hash-table :test 'equal))
        (*include-root-dir* (or include-root-dir *default-pathname-defaults*)))
    (inner-parse-stream-to-form stream)))

(defun parse-stream-and-build-toplevel (stream binary encoding include-root-dir)
  (let* ((*files* nil)
         (*variables-list* nil)
         (*index-values* (make-hash-table :test #'equal))
         (template-form (parse-stream-to-form stream binary encoding include-root-dir))
         (stream-sym (gensym "STREAM-"))
         (data-sym (gensym "DATA-")))
    (let ((result `(lambda (,data-sym ,stream-sym)
                     (let* ((current-content ,data-sym)
                            (*current-stream* ,(if binary
                                                   `(flexi-streams:make-flexi-stream ,stream-sym :external-format ,encoding)
                                                   stream-sym))
                            (current-output *current-stream*))
                       (declare (ignorable current-content))
                       (let ,(mapcar #'(lambda (symbol)
                                         (list symbol nil))
                                     *variables-list*)
                         (declare (ignorable ,@*variables-list*))
                         ,template-form
                         (finish-output *current-stream*))))))
      (list result *files*))))

(defun parse-template (stream &key binary (encoding :utf-8) include-root-dir)
  "Parses and compiles the template definition given as STREAM. If
BINARY is NIL, the generated template will output its data as strings
\(using PRINC), otherwise the output will be converted to binary using
the encoding specified by ENCODING. The binary output is the preferred
method as that will allow constant strings in the template to be
encoded during parsing instead of at runtime.

INCLUDE-ROOT-DIR is used to specify a base directory from where the \"include\"
command will search for files.

The return value is a function that takes two arguments, DATA and OUTPUT.
DATA is the data that will be used by the template, and OUTPUT is the
output stream to which the result should be written."
  (let* ((name (gensym))
         (result (parse-stream-and-build-toplevel stream binary encoding include-root-dir))
         (code-form (first result)))
    (compile name code-form)
    (list (symbol-function name) (second result))))

(defun debug-parser (s &optional binary)
  (with-input-from-string (stream s)
    (parse-stream-and-build-toplevel stream binary :utf-8 nil)))
