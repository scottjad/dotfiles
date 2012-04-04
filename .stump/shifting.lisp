;;; shifting extension
;;; by Scott Jaderholm

;;; overview: moves windows around sorting them by most recently accessed
;;; starting from a user designated primary frame that includes the active
;;; window. everytime you select a window it is placed in the primary frame and
;;; other windows are shifted around the other frames. if you have fewer frames
;;; than windows then all non-displayed windows will be placed in the primary
;;; frame behind the active window.
(in-package :stumpwm)

(defun primary-frame ()
  "Returns the frame where the active window is placed"
  (let ((frames (first (last (tile-group-current-frame (current-group))))))
    (if (consp maybe)
        (if (consp (second maybe))
            (first (second maybe))
            (first (last maybe)))
        maybe)))

(defun in-primary-frame? ()
  "returns true if in the primary-frame frame"
  (eq (primary-frame)
      ;; current frame
      (tile-group-current-frame (current-group))))

(defun first-match (props &optional (all-groups))
  "returns first window matching the props exp
Ex: (first-match '(:class \"Firefox\")"
  (first (find-matching-windows props all-groups t)))

(defun focus (win)
  (focus-frame (window-group win) (window-frame win)))

(defmacro no (object)                   ;arc FTW
  `(not ,object))

(defun window-in? (frame)
  "returns true of there's a window in frame"
  (frame-window frame))

(defun shift-window-from (frame target)
  "shifts window in frame to target frame, and then possibly shifts the window
in target"
  (let ((destination (neighbour :left frame (group-frames (current-group))))
        (win (window-in? frame)))
    (if win
        (if (no destination)
            (pull-window win (primary-frame))
            (when (not (eq target win))
              (let ((yeah (frame-window frame)))
                (shift-window-from destination target)
                (pull-window yeah destination)))))))

(defun already-in-primary-frame? (win)
  (eq (window-frame win) (primary-frame)))

(defun focus-matching-window
    (props &optional (all-groups *run-or-raise-all-groups*)
     (all-screens *run-or-raise-all-screens*))
  (labels
      ;; Raise the window win and select its frame.  For now, it
      ;; does not select the screen.
      ((goto-win (win)
         (let* ((group (window-group win))
                (frame (window-frame win))
                (old-frame (tile-group-current-frame group)))
           ;; (frame-raise-window group frame win)
           (focus-all win)
           ;; (unless (eq frame old-frame)
           ;;   (show-frame-indicator group))
           )))
    (let* ((matches (find-matching-windows props all-groups all-screens))
           ;; other-matches is list of matches "after" the current
           ;; win, if current win matches. getting 2nd element means
           ;; skipping over the current win, to cycle through matches
           (other-matches (member (current-window) matches))
           (win (if (> (length other-matches) 1)
                    (second other-matches)
                    (first matches))))
      (if win
          (goto-win win)))))

(defparameter moving nil)

(defmacro shifting-command (name props cmd &optional rat) 
  "a command that will shift windows around when selecting or running the
program"
  `(defcommand ,name () ()
     (if moving
         (let ((win (first-match ,props)))
           (if (no win)
               (progn (shift-window-from (primary-frame) win)
                                        ;(move-focus :right)
                      (run-shell-command ,cmd))
               (progn
                 (unless (and (already-in-primary-frame? win) (window-visible-p win))
                   (shift-window-from (primary-frame) win))
                 (pull-window win (primary-frame))
                 (focus win))))
         (if (first-match ,props t)
             (progn (focus-matching-window ,props)
                    (if ,rat (rattopleft)))
             (run-shell-command ,cmd)))))
