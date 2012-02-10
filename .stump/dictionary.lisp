(in-package :stumpwm)

;;; dictionary
(defcommand dictionary (word) ((:rest "dictionary: "))
  "conkeror"
  (run-prog *shell-program*
            :args (list "-c" (str "conkeror \"dictionary " word "\""))
            :wait nil))

(bind "d" "dictionary")
