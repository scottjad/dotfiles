(in-package :stumpwm)

(defcommand gopen (cmd) ((:rest "gnome-open "))
  "gnome-open"
  (run-prog *shell-program*
            :args (list "-c" (str "gnome-open " cmd))
            :wait nil))
