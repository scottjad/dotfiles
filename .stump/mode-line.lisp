(in-package :stumpwm)

(load-module "cpu")
(load-module "disk")
;; (load-module "mpd")
(load-module "mem")
(load-module "net")
(load-module "battery-portable")
;; (load-module "notifications")

(dolist (i '("ml-wifi.sh" "ml-weather.sh"
	     ;; "ml-email.sh"
             ;; "ml-volume.sh" "ml-dropbox.sh" "ml-sensors.sh"
             ;; "ml-clock.sh"
             "ml-trash.sh" "ml-quotes.sh" ))
  (run-shell-command i))

(defun read-ml-file (s)
  (read-file (str "/dev/shm/" s)))

(defun color-ping (s)
  (if (equal s "")
      ""
      (let* ((words (cl-ppcre:split "\\s+" s))
             (ping (nth 5 words))
             (color (bar-zone-color (read-from-string ping)
                                    300 700 1000))
             (colored-ping (format nil "^[~A~3D^]" color ping)))
        (cl-ppcre:regex-replace ping s colored-ping))))

;; (defun top-programs)

(setf *time-modeline-string* "%a %m-%d ^4*^B%l:%M^b^n %p")

(setf stumpwm:*screen-mode-line-format*
      (list
       "[^B%n^b] " ; group num
       '(:eval (color-ping (read-ml-file ".ml-wifi")))
       "%B" ; battery
       ;; "%g" ;groups
       ;; "^B%w^b" ; window list
       ;; voicemail, sms, email
       '(:eval (read-ml-file ".ml-email"))
       ;; quotes
       '(:eval (read-ml-file ".ml-quotes"))
       ;; notifications
       " %Z"
       ;; FIXME add weather forecast
       ;; TODO add google reader unread
       ;; TODO add linphone status/incoming calls
       ;; TODO add irc alert
       ;; TOOD add current todo (from emacs/org, clocked in item)
       ;; " DRP: " '(:eval (read-ml-file ".ml-dropbox"))
       "^>" ; right align
       ;; pomodoro
       '(:eval (read-ml-file ".ml-pomodoro-msg")) " "
       '(:eval (read-ml-file ".ml-pomodoro-time")) " "
       '(:eval (read-ml-file ".ml-weather")) "F " ;; "°F "
       ;; ;; volume
       ;; '(:eval (read-file ".mode-line-volume")) " "
       "%c" ; cpu
       ;; '(:eval (read-ml-file ".ml-sensors")) " "
       ;; "%M" ; mem
       "NET: %l" ; net
       ;; "%D" ; disk

       "Trash: " '(:eval (read-ml-file ".ml-trash")) " "
       ;; "^> %M %c" ;; I like %c but not working last time I tries it's cpu.lisp
       ;; "%d"  ;; crappy default date
       "%d"
       ;; '(:eval (string-right-trim '(#\Newline) (run-shell-command
       ;;                                          ;; "date +'%a %m-%d ^4*^B%l:%M^b^n %p'|tr -d '\\n'"
       ;;                                          ;; uses date command so time can be bold
       ;;           "date +'%a %m-%d ^4*^B%l:%M^b^n %p'" t)))
       ))

(defcommand uaml () ()
  ""
  (update-all-mode-lines))

(dolist (head
          (list (first (screen-heads (current-screen)))) ; first
         ;; (screen-heads (current-screen)) ; all
         )
  (enable-mode-line (current-screen) head 
                    t *screen-mode-line-format*))
