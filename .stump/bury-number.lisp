(in-package :stumpwm)

(defcommand bury-number () ()
  "Set window number of current window higher than any other window."
  (let* ((highest-number (window-number
                          (first (last (sort (all-windows)
                                             #'< :key #'window-number))))))
    (setf (window-number (current-window)) (1+ highest-number))
    (message "Window number buried")))

(bind "u" "bury-number")
