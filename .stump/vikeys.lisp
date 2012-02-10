;;; s-[hjkl] navigate through frames. If you press shift, it will move
;;; the current window in that direction.
(loop for (vi-key name) in
     '(("k" "up")
       ("j" "down")
       ("h" "left")
       ("l" "right"))
     do (let ((key-combo (format nil "s-~A" vi-key))
	      (shifted-key-combo (format nil "s-~A" (string-upcase vi-key))))
	  (define-key *top-map* (kbd key-combo)
	    (format nil "move-focus ~A" name))
	  (define-key *top-map* (kbd shifted-key-combo)
	    (format nil "move-window ~A" name))))
