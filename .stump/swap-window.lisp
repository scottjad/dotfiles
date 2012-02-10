;;; from tycho
(in-package :stumpwm)

;; Window swapping tool
(defvar *swapping-window* nil
  "What window we're swapping out.  Do not customize by hand!")
(defvar *swapping-window-frame* nil
  "What frame we're swapping to.  Do not customize by hand!")

(defcommand swap-window () ()
  "Run first on one window and then the next to swap the two windows"
  (if *swapping-window*
      (let ((this-current-window (current-window)))
        (pull-window *swapping-window*)
        (setf *swapping-window* nil)
        (if *swapping-window-frame*
            (pull-window this-current-window *swapping-window-frame*)
            (setf *swapping-window-frame nil))
        (echo "Swapped!"))
      (progn
        (setf *swapping-window* (current-window))
        (setf *swapping-window-frame* (window-frame (current-window)))
        (echo "Window marked for swapping."))))

(defparameter *focus-frame-skip-list* '())

(defun focus-frame-after (group frames)
  "Given a list of frames focus the next one in the list after
the current frame, except for those in *focus-frame-skip-list*."
  (let ((next-frame (tile-group-current-frame group)))
    (do ((rest (cdr (member next-frame frames :test 'eq))
               (cdr (member next-frame frames :test 'eq)))
         (frames-left (length frames) (1- frames-left)))
        (nil)
      (setf next-frame (if rest (car rest) (car frames)))
      (unless (member next-frame *focus-frame-skip-list*)
        (return))
      (when (= frames-left 0) ; stop inifinite cycle
        (setf next-frame nil)
        (return)))
    (when next-frame
      (focus-frame group next-frame))))

(defun focus-skip-current-frame (group)
  (push (tile-group-current-frame group) *focus-frame-skip-list*))

(defun focus-unskip-current-frame (group)
  (setf *focus-frame-skip-list* (remove (tile-group-current-frame group)
  *focus-frame-skip-list*)))

(defcommand (fskip tile-group) () ()
            "Put the current frame into the fnext/fprev skip list."
            (focus-skip-current-frame (current-group)))

(defcommand (funskip tile-group) () ()
            "Take the current frame out of the fnext/fprev skip list."
            (focus-unskip-current-frame (current-group)))
