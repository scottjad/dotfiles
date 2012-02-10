(in-package :stumpwm)

(defcommand echo-date () ()
  (message "^1*~a" (format-expand *time-format-string-alist*
                                  "^B%l^b:%M:%S %p ^B%a^b %Y-%m-^B%e^b")))
