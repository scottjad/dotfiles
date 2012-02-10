(in-package :stumpwm)

(defun transparent-xwin (xwin)
  (xlib:change-property xwin
                        :_NET_WM_WINDOW_OPACITY
                        ;; '(4080218880)
                        '(3865470464)
                        :CARDINAL
                        32))

(defun make-window-transparent (window)
  (transparent-xwin (window-parent window)))

;; (add-hook *new-window-hook* 'make-window-transparent)

;;; make all ui elements transparent
;; (dolist (s *screen-list*)
;;   (transparent-xwin (screen-message-window s))
;;   (transparent-xwin (screen-input-window s))
;;   (transparent-xwin (screen-key-window s))
;;   (transparent-xwin (screen-focus-window s))
;;   (transparent-xwin (screen-frame-window s))
;;   (dolist (h (screen-heads s))
;;     (when (not (null (head-mode-line h)))
;;       (transparent-xwin (mode-line-window (head-mode-line h))))))

;; (run-shell-command "cairo-compmgr")
