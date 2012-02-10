(in-package :stumpwm)

(defcommand prev-window () ()
  (focus *last-window*))