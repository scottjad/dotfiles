;;; helpers
(defparameter *hostname*
  (string-trim '(#\Newline) (run-shell-command "hostname" t)))

(defun str (&rest strings)
  (apply #'concatenate 'string strings))

(defparameter *home-dir* (stumpwm::getenv "HOME"))

(defun home (path)
  (str *home-dir* "/" path))

(defun global (key command)
  (define-key *top-map* (kbd key) command))

(defun group-rename (group name)
  (setf (group-name group) name))

(defun if-file-exists (function file)
  (if (probe-file file)
      (funcall function file)))

(defun read-file (file)
  (with-open-file (stream file)
    (read-line stream)))

(defvar *stumpwm-started* nil)

(defmacro on-start (&rest body)
  `(unless *stumpwm-starting*
     ,@body))

(defun random-elt (coll)
  (nth (random (length coll)) coll))
