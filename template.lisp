(in-package :lofn)

(defclass parsed-file ()
  ((name            :type list
                    :reader parsed-file-pathspec
                    :initarg :name
                    :initform (error "name is a required parameter")
                    :documentation "List of file names that was used to generate this file")
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
  (setf (slot-value obj 'modified-date) (reduce #'max (parsed-file-pathspec obj) :key #'file-write-date))
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

(defun files-up-to-date-p (parsed)
  (loop
     for file in (parsed-file-pathspec parsed)
     when (> (file-write-date file)
             (parsed-file-modified-date parsed))
     return nil
     finally (return t)))

(defun reparse-file (pathname binary encoding include-root-dir)
  (log:info "Reparsing file: ~s" pathname)
  (destructuring-bind (parsed files)
      (parse-template-file pathname
                           :binary binary
                           :encoding encoding
                           :include-root-dir (or include-root-dir
                                                 *default-include-root-dir*
                                                 *template-files-base-dir*))
    (setf (gethash pathname *cached-templates*)
          (make-instance 'parsed-file
                         :name (cons pathname files)
                         :template parsed
                         :last-time-check (get-universal-time)))))

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
                                                  (not (files-up-to-date-p cached))))
                                         ;; The file needs to be reparsed
                                         (reparse-file pathname binary encoding include-root-dir)
                                         ;; The cached file is up to date                                         
                                         cached))
               data stream))))

(defun exec-template-to-string (file data)
  (with-output-to-string (s)
    (exec-template-file file data s)))
