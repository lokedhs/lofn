(in-package :lofn)

(defclass parsed-file ()
  ((name            :type pathname
                    :reader parsed-file-pathspec
                    :initarg :name
                    :initform (error "name is a required parameter"))
   (modified-date   :type integer
                    :reader parsed-file-modified-date)
   (last-time-check :type integer
                    :initarg :last-time-check
                    :accessor parsed-file-last-time-check)
   (template        :type function
                    :reader parsed-file-template
                    :initarg :template
                    :initform (error "template is unset")))
  (:documentation "Cached parse result"))

(defmethod initialize-instance :after ((obj parsed-file) &key &allow-other-keys)
  (setf (slot-value obj 'modified-date) (file-write-date (parsed-file-pathspec obj)))
  (setf (slot-value obj 'last-time-check) (get-universal-time)))

(defvar *template-cache-min-check* 1
  "The minimum number of seconds between checks whether the template
file have been changed")

(defvar *default-include-root-dir* nil
  "The base dir from which include files are found")

(defvar *cached-templates* (make-hash-table :test 'equal))
(defvar *cached-templates-lock* (bordeaux-threads:make-lock "cached-templates-lock"))

(defun parse-template-file (pathname &key binary (encoding :utf-8) include-root-dir)
  (with-open-file (s pathname :external-format :utf-8)
    (parse-template s :binary binary :encoding encoding :include-root-dir include-root-dir)))

(defun exec-template-file (file data stream &key binary (encoding :utf-8) include-root-dir)
  "Load and compile FILE and put it into the template cache if it was not
already in the cache. Then run the template using DATA and write the
output to STREAM."
  (bordeaux-threads:with-lock-held (*cached-templates-lock*)
    (let* ((pathname (pathname file))
           (cached (gethash pathname *cached-templates*)))
      (funcall (parsed-file-template (if (or (null cached)
                                             (and (> (- (get-universal-time)
                                                        (parsed-file-last-time-check cached))
                                                     *template-cache-min-check*)
                                                  (> (file-write-date (parsed-file-pathspec cached))
                                                     (parsed-file-modified-date cached))))
                                         ;; The file needs to be reparsed
                                         (setf (gethash pathname *cached-templates*)
                                               (make-instance 'parsed-file
                                                              :name pathname
                                                              :template (parse-template-file pathname
                                                                                             :binary binary
                                                                                             :encoding encoding
                                                                                             :include-root-dir (or include-root-dir
                                                                                                                   *default-include-root-dir*
                                                                                                                   *template-files-base-dir*))
                                                              :last-time-check (get-universal-time)))
                                         ;; The cached file is up to date                                         
                                         cached))
               data stream))))

(defun exec-template-to-string (file data)
  (with-output-to-string (s)
    (exec-template-file file data s)))
