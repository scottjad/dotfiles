(in-package :stumpwm)

(defparameter *cons-command* "cons")

(defun cons-listing (&optional flags)
  (let* ((command (concat *cons-command* " --list " flags))
         (results (run-shell-command command t))
         (no-tabs (cl-ppcre:regex-replace-all "\\t" results " ")))
    (cl-ppcre:split "\\n" no-tabs)))

;;; TODO this has bugs. if there are no marks, then splitting on space will
;;; give the folder name, which could have * in it.
(defun cons-unreadp (item)
  (let ((marks (second (cl-ppcre:split "\\s+" item))))
    (not (cl-ppcre:scan "\\*" (or marks "")))))

(defun cons-first-unread (items)
  (let ((unread-items (remove-if-not #'cons-unreadp items)))
    (when unread-items
      (first unread-items))))

(defun cons-item-id (item)
  (let ((id (first (cl-ppcre:split "\\s" item))))
    (when id (parse-integer id))))

(defun cons-first-unread-id (items)
  (or (cons-item-id (cons-first-unread items)) 0))

(defun cons-select-item (&optional flags)
  (let ((listing (cons-listing flags)))
    (select-from-menu (current-screen) listing nil (cons-first-unread-id listing))))

(defun consume-item (id &optional flags)
  (run-shell-command (format nil "~A --item ~A ~A" *cons-command* id flags)))

(defcommand cons-item-flags (flags) ((:string "flags: "))
  (let ((id (cons-select-item flags)))
    (when id
      (consume-item id flags))))

(defcommand cons-item-filtered (filter) ((:string "filter: "))
  (cons-item-flags (concat "--filter " filter)))

(defcommand cons-item () ()
  (cons-item-flags nil))

(defcommand cons-new () ()
  (cons-item-flags "--new"))

(global "s-'" "cons-item")
;; (global "Menu" "cons-item")
(global "s-Menu" "cons-new")
(global "s-\"" "cons-item-filtered")
