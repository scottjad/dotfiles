(in-package :stumpwm)

;;; TODO has a bug if cmd is >1 word fails can't name command that way
(defmacro defprog (name key cmd props &key rat)
  "binds key to stumpwm command name that select window with props or runs cmd"
  (let ((name-focus (intern1 (concat (string name) "-FOCUS")))
        (name-pull (intern1 (concat "pull-" (string name)))))
    `(progn
       ;; s-key to launch or focus
       (shifting-command ,name ,props ,cmd ,rat)
       (define-key *top-map* (kbd ,(concatenate 'string "s-" key))
         ,(string name))

       ;; s-KEY to launch another
       (defcommand ,name-focus () ()
         (run-shell-command ,cmd)
         ;; (focus-matching-window ,props)
         )
       (define-key *top-map*
           (kbd ,(concatenate 'string "s-" (string-upcase key)))
         ,(string name-focus))
       ;; C-i C-key for pulling window
       (defcommand ,name-pull () ()
         (run-or-pull ,cmd ,props))
       (define-key *root-map*
           (kbd ,(concatenate 'string "C-" (string-upcase key)))
         ,(string name-pull)))))
