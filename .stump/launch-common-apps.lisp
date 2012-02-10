(defcommand launch-common-apps () ()
  (run-shell-command "emacs")
  (run-shell-command *terminal*)
  (run-shell-command "conkeror")
  (run-shell-command "evolution"))

