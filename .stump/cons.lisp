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

(defun cons-youtubep (item)
  (cl-ppcre:scan "youtube" item))

(defun cons-first-youtube (items)
  (let ((youtube-items (remove-if-not (lambda (item)
                                        (and (cons-youtubep item)
                                             (cons-unreadp item)))
                                      items)))
    (when youtube-items
      (first youtube-items))))

(defun cons-item-id (item)
  (let ((id (first (cl-ppcre:split "\\s" item))))
    (when id (parse-integer id))))

(defun cons-first-unread-id (items)
  (or (cons-item-id (cons-first-unread items)) 0))

(defun cons-first-youtube-id (items)
  (or (cons-item-id (cons-first-youtube items)) 0))

(defun cons-select-item (&optional flags)
  (let ((listing (cons-listing flags)))
    (select-from-menu (current-screen) listing nil (cons-first-youtube-id listing))))

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

(defcommand cons-next () ()
  (cons-item-flags "--next"))

(defcommand cons-next-mp4 () ()
  (cons-item-flags "--next --filter mp4"))

(defcommand cons-fast () ()
  (setf (getenv "CONS_PLAYBACK_SPEED") "1.61"))

(defcommand cons-normal () ()
  (setf (getenv "CONS_PLAYBACK_SPEED") "1.0"))

(defcommand cons-speed (speed) ((:string "speed (ex. 1.0)? "))
  (setf (getenv "CONS_PLAYBACK_SPEED") speed))


(global "s-'" "cons-item")
(global "Menu" "cons-new")
(global "s-\"" "cons-item-filtered")

