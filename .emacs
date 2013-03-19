;;---------------------------------------------------------
;; .emacs by Scott Jaderholm
;;---------------------------------------------------------
(defconst emacs-start-time (current-time))
(add-to-list 'load-path "~/.elisp")
(defvar backup-dir (expand-file-name "~/.ebackup/"))
(setq backup-directory-alist (list (cons ".*" backup-dir)))
(defvar autosave-dir (expand-file-name "~/.eautosave/"))
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))
(setq visible-bell t)
(setq ring-bell-function (lambda ()))
(fset 'yes-or-no-p 'y-or-n-p)
(setq initial-scratch-message nil)
(setq inhibit-splash-screen t)
(defun startup-echo-area-message () "Ready")
;; (setq x-select-enable-clipboard t)

(show-paren-mode 1)
(menu-bar-mode 0)
(tool-bar-mode 0)
(column-number-mode t)
(global-font-lock-mode t)
;; (icomplete-mode t)
(auto-compression-mode t)
(setq font-lock-maximum-decoration t)
(add-hook 'text-mode-hook 'auto-fill-mode)
;; (setq transient-mark-mode nil)
(delete-selection-mode t)
(display-time-mode 0)
(global-auto-revert-mode t)

(setq comment-style 'indent)
(setq frame-title-format
      (list '("emacs ")
            '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))
(setq cursor-in-non-selected-windows nil)
(setq-default indent-tabs-mode nil)
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "conkeror")
(setq delete-by-moving-to-trash t) ; slow
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
;; (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
;; (add-hook 'lisp-mode-hook '(lambda () (eldoc-mode -1))) 
(setq ffap-require-prefix t) ;; C-u C-x C-f finds the file at point
;; so bit fonts split with vertical bar
(setq split-height-threshold 550)
(setq print-level nil
      eval-expression-print-level nil)

;;---------------------------------------------------------
;; use-package
;;---------------------------------------------------------
(add-to-list 'load-path "~/.elisp/use-package/")
(require 'use-package)
;; (setq use-package-verbose nil)

;;---------------------------------------------------------
;; my keys minor mode
;;---------------------------------------------------------
(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap")

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  t " my-keys" 'my-keys-minor-mode-map)

(my-keys-minor-mode 1)

(defmacro bind (key fn)
  "shortcut for my-keys binding"
  `(define-key my-keys-minor-mode-map
     (kbd ,key)
     ;; handle unquoted function names and lambdas
     ,(if (listp fn)
          fn 
        `',fn)))

(bind "M-a" ido-switch-buffer)
(bind "M-e" ido-find-file)
(bind "M-s" save-buffer)

(bind "C-c C-r" eval-region)
(bind "C-x C-b" eval-buffer)

;; (when window-system
;;   (add-hook 'after-init-hook 'server-start t))

(use-package ido
  :init (ido-mode t)
  :config
  (progn

    (defun jsj-ido-set-current-home ()
      (interactive)
      (ido-set-current-home)
      (setq ido-exit 'refresh)
      (exit-minibuffer))

    (defun ido-my-keys ()
      (define-key ido-completion-map "\t" 'ido-complete)
      (define-key ido-completion-map "\C-l" 'jsj-ido-set-current-home) ; _cool
      ;; same as isearch
      (define-key ido-completion-map "$" 'my-ido-use-bookmark-dir)
      (define-key ido-completion-map "\C-t" 'ido-toggle-regexp))

    (add-hook 'ido-setup-hook 'ido-my-keys) ; _cool

    (use-package my-bookmarks)
    
    (use-package ido-hacks
      :load-path  "~/.elisp/ido-hacks"
      :init
      (ido-hacks-mode t)
      :config
      (setq ido-hacks-highlight-flex-matches t))

    (use-package ido-better-flex
      :disabled t
      :load-path "~/.elisp/ido-better-flex-2"
      :init (progn (require 'smex) (smex-initialize) (ido-better-flex/enable)))
    ))

;;(load "my-ido")

(use-package tail
  :command (tail-file tail-command))

;; (use-package tramp
;;   :config (setq tramp-default-method "ssh"))

(use-package epa-file
  :commands epa-file
  :mode  ("\\.gpg\\(~\\|\\.~[0-9]+~\\)?\\'" . epa-file)
  :config (setq epa-file-cache-passphrase-for-symmetric-encryption t))

(use-package css-mode
  :mode ("\\.css\\'" . css-mode))

(use-package php-mode
  :commands php-mode
  :mode ("\\.php\\'" . php-mode))

;;---------------------------------------------------------
;; eshell
;;---------------------------------------------------------
(use-package eshell
  :defer t
  :config
  (progn
    ;; Fix annoying scrolling
    (add-hook 'eshell-output-filter-functions
              'eshell-postoutput-scroll-to-bottom)

    ;; Put a space above eshell prompt
    (setq eshell-prompt-function (lambda nil (concat "\n" (eshell/pwd) " $ ")))

    ;; Make ls output be RET and mouse-2 clickable
    (load-library "esh-clickable-ls.el")))

(use-package ahk-mode
  :commands ahk-mode
  :mode ("\\.ahk\\'" . ahk-mode))

(use-package csv-mode
  :comamnds csv-mode
  :mode ("\\.[Cc][Ss][Vv]\\'" . csv-mode))

;; Highlights search term section with no occurances
(eval-after-load "isearch" '(use-package isearch+))

;; occur from isearch
;; now supported in default emacs with 'M-s o' but doesn't go away with C-g
(use-package ska-occur-from-isearch)

;;---------------------------------------------------------
;; dired
;;---------------------------------------------------------
(defun dired-mouse-find-alternate-file (event)
  "In dired, visit the file or directory you click on instead of
the dired buffer."
  (interactive "e")
  (let (file)
    (save-excursion
      (set-buffer (window-buffer (posn-window (event-end event))))
      (save-excursion
        (goto-char (posn-point (event-end event)))
        (setq file (dired-get-filename))))
    (select-window (posn-window (event-end event)))
    (find-alternate-file (file-name-sans-versions file t))))

(use-package dired
  :defer t
  :config
  (progn (use-package dired-isearch
           :init (eval-after-load "dired"
                   '(progn
                      (define-key dired-mode-map (kbd "C-s") 'dired-isearch-forward)
                      (define-key dired-mode-map (kbd "C-r") 'dired-isearch-backward)
                      (define-key dired-mode-map (kbd "ESC C-s") 'dired-isearch-forward-regexp)
                      (define-key dired-mode-map (kbd "ESC C-r")
                        'dired-isearch-backward-regexp)

                      (setq dired-deletion-confirmer (lambda (prompt) t)
                            dired-clean-up-buffers-too nil)

                      (define-key dired-mode-map (kbd "r") 'dired-do-rename)
                      (define-key dired-mode-map (kbd "c") 'dired-do-copy)
                      (define-key dired-mode-map (kbd "b") 'dired-up-directory)



;;; ( and ) for hiding details
                      (eval-after-load "dired" '(use-package dired-details+))
                      
                      (defun hide-dot-files ()
                        (interactive)
                        (dired-mark-files-regexp "^\\.")
                        (dired-do-kill-lines))

                      (define-key dired-mode-map [?%?h] 'hide-dot-files)

                      )))))

;;---------------------------------------------------------
;; Rectangle regions
;;---------------------------------------------------------
(bind "C-x r C-SPC" rm-set-mark)
(bind "C-x r C-x" rm-exchange-point-and-mark)
(bind "C-x r C-w" rm-kill-region)
(bind "C-x r M-w" rm-kill-ring-save)
(autoload 'rm-set-mark "rect-mark"
  "Set mark for rectangle." t)
(autoload 'rm-exchange-point-and-mark "rect-mark"
  "Exchange point and mark for rectangle." t)
(autoload 'rm-kill-region "rect-mark"
  "Kill a rectangular region and save it in the kill ring." t)
(autoload 'rm-kill-ring-save "rect-mark"
  "Copy a rectangular region to the kill ring." t)

;; collapse indented content
(defun jao-toggle-selective-display ()
  (interactive)
  (set-selective-display (if selective-display nil 1)))

;; like C-k but reverse, _cool
(defun backwards-kill-line ()
  (interactive)
  (kill-region (point) (progn (beginning-of-line) (point))))

(bind "C-c u" backwards-kill-line)      ; C-u in zsh

;; C-x p is reverse of C-x o
(defun other-window-reverse (&optional x)
  (interactive "P")
  (if (equal x nil)
      (other-window -1)
    (other-window (- 0 x)) ))
(bind "C-x p" other-window-reverse)

;;---------------------------------------------------------
;; F3 open dired item with default app in Windows, _cool
;;---------------------------------------------------------
(defun browse-file-windows (file)
  "Run default Windows application associated with FILE.
If no associated application, then `find-file' FILE."
  (let ((windows-file-name (dired-replace-in-string
                           "/" "\\" (dired-get-filename))))
    (or (condition-case nil
            (w32-shell-execute nil windows-file-name)
          (error nil))
        (find-file windows-file-name))))

(defun browse-file-linux (file)
  (dired-do-shell-command "gnome-open" nil
                          (dired-get-marked-files t current-prefix-arg)))

(defun browse-file (file)
  (cond ((equal system-type 'gnu/linux)
         (browse-file-linux file))
        ((equal system-type 'windows-nt)
         (browse-file-windows file))))

(eval-after-load "dired"
  '(progn (define-key dired-mode-map [f3]
            (lambda () (interactive)
              (browse-file (dired-get-filename))))

          ;; e no longer does RET, instead does C-c C-x or whatever
          (add-hook 'dired-mode-hook
                    '(lambda ()
                       (define-key dired-mode-map "e" 'wdired-change-to-wdired-mode)
                       (define-key dired-mode-map "/" 'dired-isearch-filenames)))
          ))

;; indent entire buffer
(defun indent-whole-buffer ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))
(defalias 'iwb 'indent-whole-buffer)

;; convert file formats
(defun convert-unix-to-dos ()
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos))

(defun convert-dos-to-unix ()
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix))

;;---------------------------------------------------------
;; count words
;;---------------------------------------------------------
(defun count-words (start end)
  "Print number of words in the region."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (count-matches "\\sw+"))))

(defun count-words-analysis (start end)
  "Count how many times each word is used in the region.
    Punctuation is ignored."
  (interactive "r")
  (let (words)
    (save-excursion
      (goto-char start)
      (while (re-search-forward "\\w+" end t)
        (let* ((word (intern (match-string 0)))
               (cell (assq word words)))
          (if cell
              (setcdr cell (1+ (cdr cell)))
            (setq words (cons (cons word 1) words))))))
    (when (interactive-p)
      (message "%S" words))
    words))

(defalias 'word-count 'count-words)
(defalias 'word-count-analysis 'count-words-analysis)
(defalias 'tt 'toggle-truncate-lines)

(use-package install-elisp
  :commands install-elisp
  :init (setq install-elisp-repository-directory "~/.elisp/"))

(use-package recentf
  :init (recentf-mode 1))

;;---------------------------------------------------------
;; EMMS
;;---------------------------------------------------------
(use-package emms-setup
  :load-path "~/.elisp/emms/lisp"
  :commands (emms emms-add-directory-tree emms-start-at emms-speed)
  :init
  (progn (defun music ()
           (interactive)
              (if (equal "EMMS Playlist"  (buffer-name (current-buffer)))
                  (ido-switch-buffer)
                (progn (if (not (get-buffer "EMMS Playlist"))
                           (emms-add-directory-tree emms-source-file-default-directory))
                       (emms))))
         (defalias 'amusic 'emms-add-directory-tree)
         (bind "C-c m" music))
  :config
  (progn
    (emms-standard)
    (emms-default-players)
    (setq emms-source-file-default-directory "/home/scott/")

    (defun emms-delete-go-next ()
      (interactive)
      (delete-current-song)
      (emms-next))

    (defun emms-insert-progress ()
      (interactive)
      (save-window-excursion
        (switch-to-buffer ".progress")
        (goto-char (point-min))
        (insert (emms-track-description
                 (emms-playlist-current-selected-track))
                "\n")
        
        (message "progress updated")))

    (define-key emms-playlist-mode-map
      (kbd "<up>") 'emms-insert-progress)

    ;; for audiobooks
    (defvar emms-start-at nil)

    (defun emms-start-at ()
      (interactive)
      (setq emms-start-at `("-ss" ,(completing-read "Location (ex. 1:00): " '("1:00" "0:00"))))
      (update-emms-mplayer-params))

    (defun update-emms-mplayer-params ()
      (setq emms-player-mplayer-parameters
            `("-slave" "-quiet" "-really-quiet"
              ;; "-volume" "100"
              ,@emms-start-at
              "-ao" "alsa"
              "-ac" "mp3,"
              "-af" "scaletempo"
              "-speed" ,emms-speed)))

    (defvar emms-speed "1.00")

    (defun emms-speed ()
      "Switch between normal and fast speed. No arg for normal, any
arg for fast."
      (interactive)
      (setq emms-speed (completing-read "Speed (ex. 1.5): " '("1.5" "1.0")))
      (update-emms-mplayer-params))

;;; wanted emms to remember where I was, ended up doing it manually
    (add-hook 'emms-playlist-selection-changed-hook
              (lambda ()
                (setq emms-position (point))))

    (add-to-list 'desktop-globals-to-save 'emms-position)

    (defun emms-restore-position ()
      "call this after loading your playlist. it will restore point
to position it was at when you quit emacs last. needs desktop to
be persisting emms-position"
      (interactive)
      (goto-char emms-position))
(defun my-emms-playlist-total-time-below-point ()
  "Calculates the total time taken for all the tracks currently
  in playlist and below point" (interactive) (let ((move-forward
         t)
        (total-playlist-time 0))
    (save-excursion         
      (while move-forward
        (setq total-playlist-time
              (+ total-playlist-time
                 (or (emms-track-get (emms-playlist-track-at (point))
                                     'info-playing-time) 0)))
        (forward-line 1)
        (setq move-forward (next-single-property-change (point) 'emms-track))))
    (setq total-hour-only (/ total-playlist-time 3600)
          total-minutes-only (/ (% total-playlist-time 3600) 60)
          total-seconds-only (% total-playlist-time 60))
    (message "Total time is %dh:%dm:%ds" total-hour-only total-minutes-only total-seconds-only)))
    
    ))

(use-package markdown-mode
  :mode ("\\.\\(markdown\\|md\\)$" . markdown-mode))

(use-package tsv-mode
  :commands (tsv-mode tsv-normal-mode))

(use-package helm
  :disabled t
  :load-path "~/.elisp/helm"
  :commands (helm-mini helm-mode)
  :bind ("C-;" . helm-mini)
  :config (use-package helm-config))

(use-package color-theme
  :load-path "~/.elisp/color-theme"
  :commands (color-theme-select color-theme-initialize color-theme-install)
  :config
  (progn
    ;; (color-theme-initialize)
    (setq color-theme-is-cumulative t)
    (setq color-theme-is-global t)))

(use-package color-theme-colorful-obsolescence
  :commands color-theme-colorful-obsolescence)
(use-package color-theme-wombat+   :commands color-theme-wombat+)
(use-package color-theme-gnome+    :commands color-theme-gnome+)
(use-package color-theme-active    :commands color-theme-active)
(use-package color-theme-less      :commands color-theme-less)
(use-package color-theme-subdued   :commands color-theme-subdued)
(use-package color-theme-cl-frame  :commands color-theme-cl-frame)
(use-package color-theme-matrix    :commands color-theme-matrix)
(use-package color-theme-paper     :commands color-theme-paper)
(use-package color-theme-google    :commands color-theme-google)
(use-package color-theme-sanityinc
  :commands (color-theme-sanityinc-dark color-theme-sanityinc-light))

(use-package color-theme-zenburn
  :commands color-theme-zenburn)

(use-package zenburn
  :defer t
  :load-path "~/.elisp/zenburn-emacs/")

(use-package gunmetal
  :commands color-theme-gunmetal)

;; (use-package folio
;;   :comamnds color-theme-folio)

;; (use-package color-theme-folio-emacs
;;   :commands color-theme-folio-emacs)

(use-package color-theme-solarized
  :load-path "~/.elisp/emacs-color-theme-solarized/"
  :commands (color-theme-solarized-dark
             color-theme-solarized-light)
  :config (color-theme-initialize))

(use-package color-theme-sanityinc-solarized
  :load-path "~/.elisp/color-theme-sanityinc-solarized/"
  :commands color-theme-sanityinc-solarized)

(use-package color-theme-desert
  :commands color-theme-desert)

(use-package naquadah-theme
  :defer t
  :load-path "~/.elisp/naquadah-theme/")

(use-package color-theme-tomorrow
  :load-path "~/.elisp/tomorrow-theme/GNU Emacs/"
  :commands (color-theme-tomorrow-night-eighties))

(use-package color-theme-heroku
  :load-path "~/.elisp/color-theme-heroku/"
  :commands color-theme-heroku)

(use-package color-theme-monokai
  :load-path "~/.elisp/color-theme-monokai.el"
  :commands color-theme-monokai)

;; (color-theme-solarized-dark)
;; (color-theme-solarized-light)
;; (color-theme-subdued)
;; (color-theme-less)
;; (color-theme-late-night)
;; (color-theme-sanityinc-light)
;; (color-theme-sanityinc-dark)
;; (color-theme-gnome2)
;; (color-theme-andreas)
;; (color-theme-arjen)
;; (color-theme-dark-blue)
;; (color-theme-feng-shui)
;; (color-theme-resolve)
;; (color-theme-retro-orange)
;; (color-theme-ryerson)
;; (color-theme-gunmetal)
;; (color-theme-folio)

(use-package yasnippet
  :disabled t
  :load-path "~/.elisp/yasnippet"
  :commands (yas/minor-mode yas/expand)
  :init
  (dolist (mode-hook '(emacs-lisp-mode-hook
                       clojure-mode-hook
                       slime-repl-mode-hook
                       org-mode-hook
                       ruby-mode-hook
                       message-mode-hook))
    (add-hook mode-hook (lambda () (yas/minor-mode 1))))
  
  (progn
    (setq yas/snippet-dirs
          '("~/.elisp/my-snippets"
            "~/.elisp/rejeep-yasnippets"
            "~/.elisp/yasnippet/snippets" ))
    (yas/initialize)
    (yas/load-directory (expand-file-name "snippets/" user-emacs-directory))))

;;---------------------------------------------------------
;; auto recompile elisp files
;;---------------------------------------------------------
(setq font-lock-verbose nil
      byte-compile-verbose nil)

(defun pj/auto-recompile-file-always ()
  (when (equal mode-name "Emacs-Lisp")
    (let ((maximum-compile-log-height 8)
          (old-window-start (window-start))
          (old-window-point (window-point)))
      ;; pre-split for compile log buffer so that it does not modify the layout
      (set-window-buffer (split-window-vertically (- (window-height) maximum-compile-log-height)) (get-buffer-create "*Compile-Log*"))
      ;; byte compile the buffer
      (byte-compile-file buffer-file-name)
      (let ((buf (get-buffer "*Compile-Log*")))
        ;; scroll to the end to see if there's an error
        (set-window-point (get-buffer-window buf) (buffer-size buf))
        ;; auto close the compile log window and restore original display position
        (run-at-time 1.0 nil (lambda (buf)
                               (delete-windows-on buf)
                               (set-window-point (selected-window) old-window-point)
                               (set-window-start (selected-window) old-window-start))
                     buf)))))

;; (add-hook 'after-save-hook 'pj/auto-recompile-file-always)


;;---------------------------------------------------------
;; headings
;;---------------------------------------------------------
(defun jsj-insert-heading ()
  (interactive)
  (if (or (member major-mode '(emacs-lisp-mode clojure-mode lisp-mode scheme-mode)))
      (insert ";;---------------------------------------------------------
;; 
;;---------------------------------------------------------"))
  (if (member major-mode '(python-mode ruby-mode shell-mode))
      (insert "#----------------------------------------------------------
#
#----------------------------------------------------------"))
  (if (or (member major-mode '(css-mode java-mode javascript-mode js2-mode)))
      (insert "/* ------------------------------

   ------------------------------ */"))
  (if (equal major-mode 'org-mode)
      (insert "#+BEGIN_QUOTE

#+END_QUOTE")))
(bind "C-c h" jsj-insert-heading)

;;-----------------------------------------------------------------------------
;; F2: files
;;-----------------------------------------------------------------------------
(defmacro set-key-find-file (key file)
  "Defines a shortcut key to open a file."
  (let ((fname (intern (concat "open-" file))))
    `(progn (defun ,fname () (interactive) (find-file ,file))
            (global-set-key (kbd ,key) ',fname))))

(set-key-find-file "<f2> e" "~/.emacs")
(set-key-find-file "<f2> g" "~/.gnus.el")
(set-key-find-file "<f2> t" "~/org/todo.org")
(set-key-find-file "<f2> n" "~/org/notes.org")
(set-key-find-file "<f2> f" "~/org/feeds.org")
(set-key-find-file "<f2> z" "~/.zshrc")
(set-key-find-file "<f2> b" "~/.xbindkeysrc")
(set-key-find-file "<f2> r" "~/.Xresources")
(set-key-find-file "<f2> m" "~/.Xmodmap")
(set-key-find-file "<f2> s" "~/.stumpwmrc")
(set-key-find-file "<f2> w" "~/.stump-colors-wombat")
(set-key-find-file "<f2> c" "~/.conkerorrc/conkerorrc.js")

;;-----------------------------------------------------------------------------
;; F5: Org functions
;;-----------------------------------------------------------------------------
(bind "<f5> a" org-toggle-archive-tag)
(bind "<f5> b" org-ido-switchb)
(bind "<f5> i" org-clock-in)
(bind "<f5> o" org-clock-out)
(bind "<f5> r" org-refile)
(bind "<f5> f" org-occur)
;(bind "<f5> r" org-remember)
(bind "<f5> v" org-archive-subtree)
(bind "<f5> t" my-org-todo)
(bind "<f5> w" widen)
(bind "<f5> u" org-feed-update-all)
;;-----------------------------------------------------------------------------
;; F6: Emacs functions
;;-----------------------------------------------------------------------------
(bind "<f6> t" 'visit-tags-table)
(bind "<f6> h" 'jao-toggle-selective-display)
(bind "<f6> h" 'hs-org/minor-mode)
(bind "<f6> d" 'color-theme-wombat+)
(bind "<f6> g" 'color-theme-gnome+)
(bind "<f6> l" 'color-theme-active)
(bind "<f6> n" 'linum-mode)

;;-----------------------------------------------------------------------------
;; F7:
;;-----------------------------------------------------------------------------
(setq font-counter 0)
(setq fonts '(
              ;; "Envy Code R-10"
              "Dina-10:medium"
              "Dina-11:medium"
              "Dina-12:medium"
              "DejaVu Sans Mono-14"
              ;; "Bitocra-8"
              "MonteCarlo-10"
              "ProFont-9"
              ;; "Proggy Clean-10"
              "ProggyCleanTTSZ-12"
              "Boxxy-12"
              ;; "M+ 1m-12:thin"
              ;; "M+ 1m-32:thin"
              ;; "mped-10"
              "Fixed-10"
              "Unifont-12"
              "GohuFont-9"
              ;; "Ohsnap-10"
              "Ohsnap.icons-12"
              "GohuFont-12"
              "Terminus-32:bold"
              "Terminus-12"
              "Terminus-8"
              "Tamsyn-13"
              ;; "Anonymous Pro"
              "Crisp"
              ;; "mensch"
              ;; "smooth"
              "Fixedsys Excelsior 3.01-L2-12"
              "Ubuntu Mono-12"
              "Ubuntu Mono-14"
              ;; "DejaVu Sans Mono-8"
              ;; "DejaVu Sans Mono-9"
              ;; "DejaVu Sans Mono-28"
              ;; "DejaVu Sans Mono-10"
              "Ubuntu-12:light"
              ;; "DejaVu Sans-10"
              ;; "DejaVu Sans-16"
              "Consolas-12"
              "Courier 10 Pitch-13" ; 1 and l look the same
              ;; "Verdana-12"

              "Source Code Pro-10"
              ;; "Source Code Pro-14"
              ;; "Inconsolata-12:medium"
              "Inconsolata-14:medium"
              "Inconsolata-16:medium"
              "Inconsolata-18:medium"
              "Inconsolata-24:medium"
              "Inconsolata-36:medium"
              ;; "Liberation Mono-12"
              "Liberation Mono-14"
              ;; "Liberation Mono-24"
              ;; "Monaco-8"
              "Monaco-14"
              ;; "Monaco-18"
              "DejaVu Sans Mono-30"
              "EnvyPn-12"
              "Envy Code R-10"
              "Envy Code R-11"
              "Envy Code R-14"
              ;; "Envy Code R-18"
              "Envy Code R-32"
              ;; "Dina ttf 10px-24"
              ;; "Droid Sans Mono-24"
              "Ubuntu Mono-32"
              )
      font-length (length fonts))

(defun cycle-fonts (&optional direction)
  (interactive)
  (if (= -1 direction)
      ;; backward
      (progn
        (if (zerop font-counter)
            (setq font-counter (1- font-length))
          (decf font-counter)))
    ;; forward
    (if (= font-counter (1- font-length))
        (setq font-counter 0)
      (incf font-counter)))
  (let ((next-font (nth font-counter fonts)))
    (set-default-font next-font)
    (message (concat "Changed font to `" next-font "`"))))

(defun choose-font ()
  (interactive)
  (set-default-font (completing-read "Font: " fonts)))

(bind "<f7>" (lambda () (interactive) (cycle-fonts 1)))
(bind "S-<f7>" (lambda () (interactive) (cycle-fonts -1)))
(bind "C-<f7>" choose-font)

;;-----------------------------------------------------------------------------
;; F8:
;;-----------------------------------------------------------------------------


;;-----------------------------------------------------------------------------
;; F9: Emacs programs
;;-----------------------------------------------------------------------------
(bind "<f9> e" eshell)
(bind "<f9> f" rgrep)
(bind "<f9> h" (lambda () (interactive) (dired "~")))
(bind "<f9> c" calendar)
(bind "<f9> r" org-remember)
;; (bind "<f9> g" gnus)
(bind "<f9> n" notmuch)


;;-----------------------------------------------------------------------------
;; F11:
;;-----------------------------------------------------------------------------

(bind "<M-f11>" recentf-open-files)

;;-----------------------------------------------------------------------------
;; F12: Agenda
;;-----------------------------------------------------------------------------
(bind "<f12>" org-agenda)
(bind "C-<f12>" org-clock-goto)

;;---------------------------------------------------------
;; Random bindings
;;---------------------------------------------------------
(bind "C-x C-b" ibuffer)
(bind "C-x C-p" proced)
;; (bind "C-h" backward-delete-char-untabify)
;; (bind "M-h" backward-kill-word)
;; (bind "\C-cc" comment-region)
;; (bind "\C-cn" uncomment-region)
(bind "C-x M-f" find-file-other-window)
(bind "M-/" hippie-expand)

;; Useful function for inserting date quickly
;; (bind "C-c D" (lambda () (interactive)
;;                           (insert (format-time-string "%a, %b %e, %Y"))))
(bind "C-c D" (lambda () (interactive)
                (insert (format-time-string "%Y-%m-%d"))))
;; (bind "C-c t" (lambda () (interactive)
;;                           (insert (format-time-string "%H:%M"))))
;; (bind "C-c T" (lambda () (interactive)
;;                           (insert (format-time-string "%H:%M:%S"))))

(defun print-buffer2 ()
  (interactive)
  (hfyview-buffer))

(defun print-region2 ()
  (interactive)
  (hfyview-region))

;; (autoload 'egg-status "egg" nil t)
;; (bind "C-x g" egg-status)
;; (add-to-list 'load-path "~/.elisp/egg")
;; (require 'egg)

(use-package magit
  :load-path "~/.elisp/magit"
  :commands (magit-status)
  :bind (("C-x g" . magit-status)
         ("C-c git" . magit-status))
  :init (progn (setq magit-diff-refine-hunk 'all)
               ;; C-u C-x g
               (setq magit-repo-dirs '("~/.elisp" "~/src" "~/code" "~/c01")))
  :config (progn
            
            ;; full screen magit-status

            (defadvice magit-status (around magit-fullscreen activate)
              (window-configuration-to-register :magit-fullscreen)
              ad-do-it
              (delete-other-windows))

            (defun magit-quit-session ()
              "Restores the previous window configuration and kills the magit buffer"
              (interactive)
              (kill-buffer)
              (jump-to-register :magit-fullscreen))

            (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)
))

(use-package magit-blame
  :commands magit-blame-mode)

(defun insert-local-variables-spec ()
     "Insert a minimal local variables spec for this buffer."
     (interactive)
     (save-excursion
       (save-restriction
         (widen)
         (goto-char (point-min))
         ;; FIXME: only strip the last 5 characters when they're "-mode"
         (insert (format "-*- mode: %s; coding: %s -*-"
                         (substring (symbol-name major-mode) 0 -5)
                         buffer-file-coding-system))
         ;; If there's some kind of local comment syntax, ensure the local
         ;; variables spec lives in one.
         (when comment-start
           (comment-region (point-min) (point)))
         (insert "\n"))))


(defun make-backup-file-name (FILE)
  (let ((dirname (concat "~/.backups/emacs/"
                         (format-time-string "%y/%m/%d/"))))
    (if (not (file-exists-p dirname))
        (make-directory dirname t))
    (concat dirname (file-name-nondirectory FILE))))

(defun org-gcal-sync ()
  "Export org to ics to be uploaded to Google Calendar and import
an .ics file that has been downloaded from Google Calendar "
  (org-export-icalendar-combine-agenda-files)
  (icalendar-import-file "~/tmp/.basic.ics" "~/tmp/.gcal"))

;; Proper CSS indentation
(setq cssm-indent-function 'cssm-c-style-indenter)

(defun dec-to-hex (dec)
  (format "%02X" dec))

(defun rgb-to-hex (r g b)
  (concat "#" (mapconcat #'dec-to-hex (list r g b) "")))

(use-package jot
  :commands jot-mode)

(use-package moz
  :commands (run-mozilla inferior-moz-mode moz-minor-mode)
  :config
  (progn ;;; TODO delete I don't think I'll use this
    (define-key inferior-moz-mode-map (kbd "C-c l")
      (lambda () (interactive)
        (insert "repl.home(); repl.enter(content);")
        (comint-send-input)))

;;; repl.home(); repl.enter(content);
    (bind "C-c j" moz-eval-last-expression)
    (defun moz-eval-last-expression (&rest ignored)
      "Convert last expression from scriptjure to javascript use
mozrepl to evaluate in browser"
      (interactive)
      (destructuring-bind (output value) (slime-eval-last-expression-as-scriptjure)
        (if (equal output "")
            (progn
              (comint-send-string
               (inferior-moz-process)
               "(content.toString() === '[object Window]') ? null : (repl.home(), repl.enter(content), null);")
              (comint-send-string
               (inferior-moz-process)
               (concat (first (read-from-string value)) ";")))
          (message output))))
))

(use-package haskell-mode
  :commands haskell-mode
  :load-path "~/.elisp/haskell-mode/"
  :init
  (add-to-list 'auto-mode-alist '("\\.l?hs$" . haskell-mode))
  :config
  (progn
    (load "~/.elisp/haskell-mode/haskell-site-file")
    (use-package inf-haskell)
    (use-package hs-lint)
    (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
    (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)))

(use-package espresso-mode
  :commands espresso-mode)

;(setq sql-mysql-options (list "-P 3306" "-C" "-t" "-f" "-n"))

(put 'set-goal-column 'disabled nil)

(defun combine-lines (&optional arg)
  (interactive "P")
  (let ((rexp "\\(?:\\s-\\|\n\\)"))
    (progn
      (while (looking-back rexp)
        (delete-backward-char 1))
      (while (looking-at rexp)
        (delete-char 1))
      (unless arg
        (insert " ")))))

(use-package desktop
  :init (setq desktop-globals-to-save
              '((extended-command-history . 300)
                (file-name-history        . 1000)
                (grep-history             . 30)
                (compile-history          . 30)
                (minibuffer-history       . 500)
                (query-replace-history    . 600)
                (read-expression-history  . 60)
                (regexp-history           . 60)
                (regexp-search-ring       . 20)
                (search-ring              . 20)
                (shell-command-history    . 500)
                sql-mysql-schema
                desktop-missing-file-warning
                tags-file-name
                tags-table-list
                register-alist)))


;; mobile org
(setq org-mobile-directory "\\\\www.box.net@SSL\\DavWWWRoot\\dav\\org")
(setq org-mobile-force-id-on-agenda-items nil)

;; (require 'thingatpt)
;; (require 'thing-edit)

(use-package re-builder+
  :commands re-builder+)

;; Example regexps
;; Increment the numbers in brackets like [1] starting from 12
;; Regexp \[[0-9]+\]
;; Replace [\,(+ 12 \#)]

;; Change the sum(ifnull(.*,0)) to ifnull(sum(.*),0)

;; Change sum(a) as b to sum(b) as b

;; Cut (C-w) or copy (M-w) current (line/word/list/string/etc) if nothing is selected
(defun next-list-boundaries ()
  (list (progn (paredit-forward) (point))
        (progn (paredit-backward) (point))))

(defun no-region-default-behavior ()
  (cond (mark-active
         (list (region-beginning) (region-end)))
        ((and (bolp) (not (equal "(" (char-to-string (char-after)))) )
         (progn (message "Copy/cut line")
                (list (line-beginning-position) (line-beginning-position 2))))
        (t
         (progn (message "Copy/cut form")
                (next-list-boundaries)))))

(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive (no-region-default-behavior)))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive (no-region-default-behavior)))

;; I like better than M-| uniq
(defun uniq-lines (beg end)
  "Unique lines in region. Called from a program, there are two
arguments: BEG and END (region to sort)."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (not (eobp))
        (kill-line 1)
        (yank)
        (let ((next-line (point)))
          (while (re-search-forward
                  (format "^%s" (regexp-quote (car kill-ring))) nil t)
            (replace-match "" nil nil))
          (goto-char next-line))))))

;;; keywords hinge gutter wrap
;; prevent huge fringes w small font
(set-fringe-mode '(5 . 0))
;; these look unnecessary
;; (set-window-fringes nil 5 0)
(set-fringe-style '(5 . 0))

(defun transparent ()
  (interactive)
  (set-frame-parameter (selected-frame) 'alpha '(70 70)))
(defun opaque ()
  (interactive)
  (set-frame-parameter (selected-frame) 'alpha '(100 100)))

(use-package loccur
  :bind (("C-c C-o" . loccur-current)
         ("C-M-o" . loccur)
         ("C-S-o" . loccur-previous-match)))

;; (require 'anything-grep)

;; (autoload 'ack-same "full-ack" nil t)
;; (autoload 'ack "full-ack" nil t)
;; (autoload 'ack-find-same-file "full-ack" nil t)
;; (autoload 'ack-find-file "full-ack" nil t)
;; (setq ack-executable (executable-find "ack-grep"))
;; TODO write a function that cleans up current sexp
;; if I have (setq ack-executable | (executable-find "ack-grep" ))
;; and cursor is at | then the keystroke will remove extra whitespace at cursor and after grep"

(load "my-desktop")

;; auto fill comments but not code
(dolist (i '(emacs-lisp-mode-hook lisp-mode-hook clojure-mode-hook))
  (add-hook 'i '(lambda ()
                 (auto-fill-mode 1)
                 (set (make-local-variable 'fill-nobreak-predicate)
                      (lambda ()
                        (not (eq (get-text-property (point) 'face)
                                 'font-lock-comment-face)))))))

;;---------------------------------------------------------
;; Highlight symbol
;;---------------------------------------------------------

;; (require 'highlight-symbol)
;; (highlight-symbol-mode 1)
;; (add-hook 'clojure-mode-hook 'highlight-symbol-mode)

(defun sync ()
  "Easier OrgMobile syncing"
  (interactive)
  (org-mobile-pull)
  (org-mobile-push))

;;---------------------------------------------------------
;; Clojure-mode and Slime
;;---------------------------------------------------------
(use-package clojure-mode
  :defer nil
  :load-path "~/.elisp/clojure-mode"
  :commands (clojure-mode)
  :mode (("\\.clj\\'" . clojure-mode)
          ("\\.cljs\\'" . clojure-mode))
  :config
  (progn

    (defun jsj-clojure-example (name)
      (interactive "sFunction (ex. clojure.set/join): ")
      (browse-url (concat "http://clojuredocs.org/clojure_core/" name "#examples")))

         (define-key clojure-mode-map (kbd "C-c d") 'jsj-clojure-example)

         (autoload 'clojure-test-mode "clojure-test-mode" "Clojure test mode" t)
         (add-hook 'clojure-mode-hook
                   '(lambda ()
                      (save-excursion
                        (goto-char (point-min))
                        (if (or (search-forward "(deftest" nil t)
                                (search-forward "(with-test" nil t))
                            (clojure-test-mode t)
                          ;; (require 'clj-imports)
                          ;; (define-key clojure-mode-map (kbd "C-c i")
                          ;;   'clj-imports-insert-with-completion)
                          ;; (define-key clojure-mode-map (kbd "C-c C-n")
                          ;;   'clj-imports-eval-ns)
                          ))))

         (defun jsj-eval-threaded-up-to-point ()
           "When you have (-> foo bar tar) and want to eval (-> foo bar)
put cursor at (-> foo bar| tar) and use this."
           (interactive)
           (insert ")")
           (slime-eval-last-expression)
           (backward-delete-char 1))

         (define-key clojure-mode-map (kbd "C-x C-M-e") 'jsj-eval-threaded-up-to-point)


        
;;; insert or remove break point for clojure at point. Uses (break) which I
;;; defined for compojure bc swank.core/break doesn't work for me there.
(defun jsj-insert-or-remove-break-point (&optional alt)
  (interactive "p")
  (let ((s (if (= alt 4) "(break)" "(swank.core/break)")))
    (beginning-of-line)
    (if (search-forward s (line-end-position) t)
        (progn (beginning-of-line)
               (kill-line 1))
      (progn (open-line 1)
             (beginning-of-line)
             (insert s)))
    (indent-according-to-mode)))

(define-key clojure-mode-map  (kbd "C-c b") 'jsj-insert-or-remove-break-point)

;;; insert or remove print statement for variable at point. Invoke with C-u to
;;; put statement below current line (e.g. when at arg in function definition)
(defun jsj-insert-or-remove-print (&optional below)
  (interactive "p")
  (let ((word (thing-at-point 'filename)))
    (beginning-of-line)
    (if (search-forward "println" (line-end-position) t)
        (progn (beginning-of-line)
               (kill-line 1))
      (progn (when (= 4 below)
               (next-line))
             (open-line 1)
             (beginning-of-line)
             (insert "(println \"" (if (equal word "") "line" word)
                     ":\" " (if (equal word "") (int-to-string (line-number-at-pos)) word) ")"))))
  (indent-according-to-mode)) 

(define-key clojure-mode-map  (kbd "C-c p") 'jsj-insert-or-remove-print)



(defun define-function ()
  (interactive)
  (let ((name (symbol-at-point)))
    (backward-paragraph)
    (insert "\n(defn " (symbol-name name) "\n  [])\n" )
    (backward-char 3)))

(define-key clojure-mode-map (kbd "C-c f") 'define-function)

;; (load-file "~/.elisp/clojure-ignore-form.el/clojure-ignore-form.el")
;; (load-file "~/.elisp/clojure-fancy.el")

;;; clojure mode has no business messing up slime for CL
(remove-hook 'slime-connected-hook 'clojure-slime-remote-file-name-hook)
(eval-after-load "slime-repl" '(add-hook 'slime-repl-mode-hook (paren-face-add-support clojure-font-lock-keywords)))

(use-package elein
  :load-path "~/.elisp/elein")

;; symbols for some overlong function names
;; (dolist (mode '(clojure-mode slime-repl-mode))
;;   (eval-after-load mode `(font-lock-add-keywords
;;                           ',mode
;;                           (mapcar
;;                            (lambda (pair)
;;                              `(,(car pair)
;;                                (0 (progn (compose-region (match-beginning 1)
;;                                                          (match-end 1) ,(cadr pair))
;;                                          nil))))
;;                            `(("\\(#\\){" "∈")
;;                              ("\\(#\\)(""ƒ")
;;                              ("(\\(fn\\)[\[[:space:]]" "λ")
;;                              ("(\\(comp\\)[\[[:space:]]" "∘")
;;                              ("(\\(range\\)[\[[:space:]]" "ℝ")
;;                              ("(\\(apply \+\\)[\[[:space:]]" "∑")
;;                              ("(\\(Math/pi\\)[\[[:space:]]" "π")
;;                              ("(\\(->\\)[\[[:space:]]" "→")
;;                              ("(\\(partial\\)[\[[:space:]]" "þ")
;;                              ("(\\(complement\\)[\[[:space:]]" "¬")
;;                              ;; not working
;;                              ("Math/pi[:space:]" "π")
;;                              ("(\\(apply \+\\)[\[[:space:]]" "∑"))))))

;; (define-key slime-mode-map (kbd "M-a") 'slime-beginning-of-defun)
;; (define-key slime-mode-map (kbd "M-e") 'slime-end-of-defun)

;; (defun clojure-jump-to-function-test ()
;;   (interactive)
;;   (flet ((maybe-create-test-namespace
;;           (namespace)
;;           (unless (clojure-find-package)
;;             (goto-char (point-min))
;;             (insert (format "(ns %s\n  (:use [%s] [clojure.test]))\n\n"
;;                             (replace-regexp-in-string "\\(\\.\\)[^\\.]+$" ".test." namespace nil nil 1)
;;                             namespace)))))
;;     (let* ((fn (which-function))
;;            (fn-name (if (listp fn) (first fn) fn))
;;            (namespace (clojure-find-package)))
;;       (clojure-jump-to-test)
;;       (when fn-name
;;         (goto-char (point-min))
;;         (maybe-create-test-namespace namespace)
;;         (unless (search-forward-regexp (format "(deftest test-%s[ \t\n]*" fn-name) nil t)
;;           (goto-char (point-max))
;;           (insert (format "\n\n(deftest test-%s\n  )" fn-name))
;;           (goto-char (- (point) 1)))))))


))

;; (use-package slime
;;   :load-path ("~/.elisp/slime"
;;               "~/.elisp/slime/contrib")
;;   :commands (slime slime-connect slime-mode)
;;   :bind ("<f8>" . slime-selector)
;;   :init
;;   (progn

;;     (add-hook
;;      'slime-load-hook (lambda ()
;;                         (slime-setup '(slime-fancy))))
    
;;     (defmacro slime-local-connect (name port)
;;       `(defun ,name ()
;;          (interactive)
;;          (slime-connect "127.0.0.1" ,port)))

;;     (slime-local-connect sl5 4005)
;;     (slime-local-connect sl6 4006)
;;     (slime-local-connect sl7 4007)
;;     (slime-local-connect sl8 4008)
;;     (slime-local-connect sl9 4009)

;;     (slime-local-connect sl-stump 4006)
;;     (slime-local-connect sl-clojure 4005)
;;     )
;;   :config
;;   (progn

;;     (setq slime-net-coding-system 'utf-8-unix)

;;     (def-slime-selector-method ?j
;;       "most recently visited clojure-mode buffer."
;;       (slime-recently-visited-buffer 'clojure-mode))


;;     (define-key slime-mode-map (kbd "C-c p")
;;       'slime-pprint-eval-last-expression)

;;     (eval-after-load 'slime-repl
;;       '(define-key slime-repl-mode-map (kbd "C-c p")
;;          'slime-pprint-eval-last-expression))

;;     (define-key slime-mode-map (kbd "C-c C-s") nil)


;;     ;; michael blais
;;     (defun slime-eval-at-register (reg)
;;       "Take the cursor to a register's location and eval
;;   the expression there. Useful for testing stuff without
;;   having to 'go there' first."
;;       (interactive "cEval at register: ")
;;       (save-excursion
;;         (jump-to-register reg)
;;         (slime-eval-last-expression)))

;;     ;; Note: slime-interactive-eval is also available on C-c :,
;;     ;; so we override it for something that looks like C-x C-e.
;;     (define-key slime-mode-map "\C-c\C-e" 'slime-eval-at-register)

;;     ;; really need a slime-eval-last-register or slime-eval-last-expression

;;     ;; Clojure stack trace change
;;     (defface esk-clojure-trace-face
;;       '((((class color) (background dark))
;;          (:foreground "grey50"))
;;         (((class color) (background light))
;;          (:foreground "grey55")))
;;       "Face used to dim parentheses."
;;       :group 'starter-kit-faces)

;;     (setq esk-clojure-trace-face 'esk-clojure-trace-face)

;;     ;; This will make relevant lines stand out more in stack traces
;;     (defun sldb-font-lock ()
;;       (font-lock-add-keywords nil
;;                               '(("[0-9]+: \\(clojure\.\\(core\\|lang\\).*\\)"
;;                                  1 esk-clojure-trace-face)
;;                                 ("[0-9]+: \\(java.*\\)"
;;                                  1 esk-clojure-trace-face)
;;                                 ("[0-9]+: \\(swank.*\\)"
;;                                  1 esk-clojure-trace-face)
;;                                 ("\\[\\([A-Z]+\\)\\]"
;;                                  1 font-lock-function-name-face))))

;;     (add-hook 'sldb-mode-hook 'sldb-font-lock)
    
;;     (use-package ac-slime
;;       :load-path "~/.elisp/ac-slime"
;;       :init (progn (add-hook 'slime-mode-hook 'set-up-slime-ac)
;;                    (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)))
    
;;     (add-hook 'slime-inspector-mode-hook
;;               (lambda ()
;;                 (font-lock-add-keywords nil '(("\\(\\w+\\)(" 1
;;                                                font-lock-function-name-face)))))


;;     ))

             
(defun backward-up-list+ ()
  "Stupid backward-up-list doesn't work from inside a string and
I got tired of having to move outside the string to use it."
  (interactive)
  (if (in-string-p)
      (while (in-string-p)
        (backward-char))
    (backward-up-list)))

(global-set-key (kbd "C-M-u") 'backward-up-list+)

(defun up-list+ ()
  (interactive)
  (if (in-string-p)
      (while (in-string-p)
        (forward-char))
    (up-list)))

(global-set-key (kbd "C-M-n") 'up-list+)

(defun eval-parent-sexp ()
  "Cause sometimes you just want to eval just the immediate
form. not the top level, but without going to the closing paren
and evaling there."
  (interactive)
  (save-excursion
    ;; get out of string if in it
    (dotimes (c (if (in-string-p) 2 1))
      (up-list+))
    (let ((cmd (key-binding (kbd "C-x C-e"))))
      (if (eq cmd 'slime-eval-last-expression)
          (funcall cmd)
        (funcall cmd '())))))

(global-set-key (kbd "C-M-S-x") 'eval-parent-sexp)

(setq slime-protocol-version nil)

;;---------------------------------------------------------
;; Paredit
;;---------------------------------------------------------
(use-package paredit
  :commands paredit-mode
  :init
  (progn (dolist (i '(emacs-lisp-mode-hook lisp-mode-hook lisp-interaction-mode-hook clojure-mode-hook slime-repl-mode-hook inferior-lisp-mode-hook))
           (add-hook i (lambda () (paredit-mode +1)
                         (local-set-key "(" 'paredit-open-parenthesis)
                         (local-set-key ")" 'paredit-close-parenthesis)
                         (local-set-key "[" 'paredit-open-square)
                         (local-set-key "]" 'paredit-close-square)
                         (local-set-key "{" 'paredit-open-curly)
                         (local-set-key "}" 'paredit-close-curly)))

           (bind "M-S-s" paredit-split-sexp)

           (bind "M-W" (lambda ()
                         (interactive)
                         (save-excursion
                           (backward-up-list+)
                           (let ((beg (point)))
                             (paredit-forward)
                             (copy-region-as-kill beg (point))))
                         (message "Copied parent sexp")))))
  :config (progn
         (defun jsj-insert-above-parent-sexp ()
           (interactive)
           (backward-up-list+)
           (paredit-newline)
           (previous-line)
           (indent-according-to-mode))
            (define-key paredit-mode-map (kbd "C-S-j")
              'jsj-insert-above-parent-sexp)

            (define-key paredit-mode-map (kbd "M-S") 'paredit-split-sexp)
           (define-key paredit-mode-map (kbd "M-s") 'save-buffer)
           (define-key paredit-mode-map (kbd "C-M-n") nil)
                    
))

;;---------------------------------------------------------
;; clojure refactoring
;;---------------------------------------------------------
(add-to-list 'load-path "~/.elisp/clojure-refactoring/src/clojure_refactoring/payload/")
(autoload 'clojure-refactoring-prompt "clojure-refactoring-mode" nil t)
;; (global-set-key (kbd "C-c e") 'clojure-refactoring-ido)
(global-set-key (kbd "C-c e") 'clojure-refactoring-prompt)

(global-set-key (kbd "<f8>")
                (lambda () (interactive)
                  (if (eq major-mode 'clojure-mode)
                      (switch-to-buffer "*slime-repl clojure*")
                    (switch-to-buffer (slime-recently-visited-buffer 'clojure-mode)))))

(global-set-key (kbd "<S-f8>")
                (lambda () (interactive)
                  (switch-to-buffer "*inferior-lisp*")))

;;---------------------------------------------------------
;; Tramp
;;---------------------------------------------------------
;; (eval-after-load "tramp"
;;   '(progn
;;      (add-to-list 'tramp-default-proxies-alist
;;                   '(nil "\\`root\\'" "/ssh:%h:"))
;;      ;; (add-to-list 'tramp-default-proxies-alist
;;      ;;                   '((regexp-quote (system-name)) nil nil))
;;      ))

;; C-x C-j jump to current file in dired
;; Load Dired X when Dired is loaded.
(eval-after-load "dired"
  '(use-package dired-x
     :init
     (progn
       ;; Enable toggling of uninteresting files.
       (setq dired-omit-files-p t)

       (setq-default dired-omit-files-p t) ; this is buffer-local variable

       (setq dired-omit-files
             (concat dired-omit-files "\\|^\\..+$")))))

(defun xsteve-ido-choose-from-recentf ()
  "Use ido to select a recently opened file from the `recentf-list'"
  (interactive)
  (let ((home (expand-file-name (getenv "HOME"))))
    (find-file
     (ido-completing-read "Recentf open: "
                          (mapcar (lambda (path)
                                    (replace-regexp-in-string home "~" path))
                                  recentf-list)
                          nil t))))

(global-set-key (kbd "C-x f") 'xsteve-ido-choose-from-recentf)

(setq ibuffer-shrink-to-minimum-size t)
(setq ibuffer-always-show-last-buffer nil)
(setq ibuffer-sorting-mode 'recency)
(setq ibuffer-use-header-line t)

(autoload 'iflipb-next-buffer "iflipb" nil t)
(autoload 'iflipb-previous-buffer "iflipb" nil t)
(setq iflipb-ignore-buffers '(;; "*Messages*" "*Help*"
                              "*Completions*" "TAGS"))
(setq iflipb-always-ignore-buffers '("^ "))

(setq iflipb-boring-buffer-filter 'my-bs-ignore-buffer)

(bind "<C-tab>" iflipb-next-buffer)
(bind "<C-S-iso-lefttab>"  iflipb-previous-buffer)

(defun xsteve-save-current-directory ()
  "Save the current directory to the file ~/.emacs.d/current-directory"
  (interactive)
  (let ((dir default-directory))
    (with-current-buffer (find-file-noselect "~/.emacs.d/current-directory")
      (delete-region (point-min) (point-max))
;      (insert (concat dir "\n"))
      (insert dir)
      (save-buffer)
      (kill-buffer (current-buffer)))))
(bind "<S-f10>" xsteve-save-current-directory)

(defun djcb-duplicate-line (&optional commentfirst)
  "comment line at point; if COMMENTFIRST is non-nil, comment the original"
  (interactive)
  (beginning-of-line)
  (push-mark)
  (end-of-line)
  (let ((str (buffer-substring (region-beginning) (region-end))))
    (when commentfirst
    (comment-region (region-beginning) (region-end)))
    (insert-string
      (concat (if (= 0 (forward-line 1)) "" "\n") str "\n"))
    (forward-line -1)))

;; or choose some better bindings....

;; duplicate a line
(bind "C-c y" djcb-duplicate-line)

;; duplicate a line and comment the first
(bind "C-c M-y" (lambda () (interactive) (djcb-duplicate-line t)))

(setq whitespace-style '(trailing lines space-before-tab indentation
                                  space-after-tab) whitespace-line-column 80)
;(global-whitespace-mode 1)

(use-package uniquify
  :config (setq uniquify-buffer-name-style 'post-forward))

;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename1 (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive (list (completing-read "New name: " nil nil nil (buffer-name))))
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(defun move1 (dir)
  "Moves both current buffer and file it's visiting to DIR."
  (interactive "DNew directory: ")
  (let* ((name (buffer-name))
     (filename (buffer-file-name))
     (dir
          (if (string-match dir "\\(?:/\\|\\\\)$")
              (substring dir 0 -1) dir))
     (newname (concat dir "/" name)))

    (if (not filename)
    (message "Buffer '%s' is not visiting a file!" name)
      (progn (copy-file filename newname 1)
             (delete-file filename)
             (set-visited-file-name newname)
             (set-buffer-modified-p nil)
             t))))
(bind "<f3>" browse-url-at-point)

(bind "<M-wheel-up>" text-scale-increase)
(bind "<M-wheel-down>" text-scale-decrease)

(load "indent-yank")

(use-package js2-mode
  :commands js2-mode
  :load-path "~/.elisp/js2-mode/"
  :mode ("\\.js$" . js2-mode))

;; (use-package auto-complete-config
;;   :load-path "~/.elisp/auto-complete"
;;   :init (global-auto-complete-mode t)
;;   :config
;;   (progn (use-package pos-tip
;;            :config
;;            (progn (setq pos-tip-foreground-color "white")
;;                   (setq pos-tip-background-color "black")))

;;          (add-to-list 'ac-dictionary-directories "~/.elisp/auto-complete/dict")
;;          (ac-config-default)
;;          (bind "<M-tab>" ac-start)

;;          (use-package auto-complete-etags)
;;          (ac-flyspell-workaround)
;;          (ac-set-trigger-key "TAB")

;;          (define-key ac-mode-map
;;            (kbd "C-c H")
;;            'ac-last-help)

;;          (setq-default ac-sources '(ac-source-functions ac-source-abbrev ac-source-yasnippet ac-source-dictionary ac-source-words-in-buffer ac-source-words-in-same-mode-buffers))))

(defun jsj-transpose-sexps ()
  (interactive)
  (transpose-sexps 1)
  (backward-sexp 2))

(define-key lisp-mode-shared-map (kbd "C-M-T") 'jsj-transpose-sexps)

(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   (format "%s -f %s/TAGS -e -R %s" path-to-ctags dir-name dir-name)))

(use-package etags-select
  :bind (("\M-?" . etags-select-find-tag-at-point)
         ("\M-." . etags-select-find-tag)))

(use-package etags-update)

;; (add-to-list 'load-path "~/.elisp/drag-stuff/")
;; (require 'drag-stuff)
;; (drag-stuff-global-mode t)

;; (add-to-list 'load-path "~/.elisp/wrap-region/")
;; (require 'wrap-region)
;; (wrap-region-global-mode t)

(add-hook 'server-switch-hook
          (lambda ()
            (when (current-local-map)
              (use-local-map (copy-keymap (current-local-map))))
            (when server-buffer-clients
              ;; (local-set-key (kbd "C-x k") 'server-edit)
              (local-set-key (kbd "<f4>") 'server-edit)
              (local-set-key (kbd "M-k") 'server-edit))))

(defun jsj-kill-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

(defun jsj-kill-other-buffer ()
  (interactive)
  (other-window 1)
  (kill-buffer (current-buffer))
  (other-window -1))

(bind "M-k" jsj-kill-buffer)
(bind "M-K" 'jsj-kill-other-buffer)

(global-hl-line-mode)

(use-package centered-cursor-mode
  :commands (centered-cursor-mode global-centered-cursor-mode))

;(setq highline-vertical '(2 . 2))
;(setq line-move-visual nil)

(use-package auto-mark
  :config (progn (setq auto-mark-command-class-alist
                       '((helm . helm)
                         (goto-line . jump)
                         (indent-for-tab-command . ignore)
                         (undo . ignore)))
                 (setq auto-mark-command-classifiers
                       (list (lambda (command)
                               (if (and (eq command 'self-insert-command)
                                        (eq last-command-char ? ))
                                   'ignore))))
                 (global-auto-mark-mode 1)))

(use-package visible-mark
  :config (global-visible-mark-mode t))

;; (add-to-list 'load-path "~/.elisp/hideshow-org/")
;; (require 'hideshow-org)

(defun comment-or-uncomment-current-line-or-region ()
  "Comments or uncomments current current line or whole lines in region."
  (interactive)
  (save-excursion
    (let (min max)
      (if (and transient-mark-mode mark-active)
          (setq min (region-beginning) max (region-end))
        (setq min (point) max (point)))
      (comment-or-uncomment-region
       (progn (goto-char min) (line-beginning-position))
       (progn (goto-char max) (line-end-position))))))

(use-package comment-uncomment-line-or-region
  :bind ("C-c c" . comment-uncomment-line-or-region))

;; (use-package browse-kill-ring
;;   :bind ("M-y" . browse-kill-ring)
;;   :config (browse-kill-ring-default-keybindings))

;; (add-hook 'gnus-article-mode-hook 'longlines-mode)
;; (refill-mode)

(load "~/.elisp/my-indent.el")

(use-package session
  :disabled t
  :load-path "~/.elisp/session/lisp/"
  :init (add-hook 'after-init-hook 'session-initialize))

;;; TODO change so that moves by continuous region instead of character
;; (require 'point-undo)
;; (define-key global-map [left] 'point-undo)
;; (define-key global-map [right] 'point-redo)

;;; cool: C-u C-_ when a section is selected does selective undo on that region

(defadvice ido-complete (around space-inserts-hyphen activate compile)
  (let ((ido-cannot-complete-command
         `(lambda ()
            (interactive)
            (if (string= " " (this-command-keys))
                (insert ?-)
              (funcall #',ido-cannot-complete-command)))))
    ad-do-it))


(defun smex-update-after-load (unused)
  (when (boundp 'smex-cache)
    (smex-update)))
(add-hook 'after-load-functions 'smex-update-after-load)

(defun scroll-down-keep-cursor ()
  "Scroll the text one line down while keeping the cursor"
  (interactive)
  (scroll-down 1))

(defun scroll-up-keep-cursor ()
  "Scroll the text one line up while keeping the cursor"
  (interactive)
  (scroll-up 1))

(bind "C-S-p" scroll-down-keep-cursor)
(bind "C-S-n" scroll-up-keep-cursor)

(defun yank-with-newline ()
  "Yank, appending a newline if the yanked text doesn't end with one."
  (yank)
  (when (not (string-match "\n$" (current-kill 0)))
    (newline-and-indent)))

(defun yank-as-line-above ()
  "Yank text as a new line above the current line.

Also moves point to the beginning of the text you just yanked."
  (interactive)
  (let ((lnum (line-number-at-pos (point))))
    (beginning-of-line)
    (yank-with-newline)
    (goto-line lnum)))

(defun yank-as-line-below ()
  "Yank text as a new line below the current line.
Also moves point to the beginning of the text you just yanked."
  (interactive)
  (let* ((lnum (line-number-at-pos (point)))
         (lnum (if (eobp) lnum (1+ lnum))))
    (if (and (eobp) (not (bolp)))
        (newline-and-indent)
      (forward-line 1))
    (yank-with-newline)
    (goto-line lnum)))

(bind "\M-P" yank-as-line-above)
(bind "\M-p" yank-as-line-below)

(use-package org-velocity
  :bind ("C-c v" . org-velocity-read)
  :config (eval-after-load "org-mode" '(setq org-velocity-bucket (concat org-directory "/bucket.org"))))

(defadvice man (after switch-to-man-buffer-ad activate)
  (other-window 1))
(bind "C-h u" man)

(defun isearch-other-window ()
  (interactive)
  ;; thank you leo2007!
  (save-selected-window
    (other-window 1)
    (isearch-forward)))

(global-set-key (kbd "C-M-S") 'isearch-other-window)

(bind "C-h F" customize-face)

;;---------------------------------------------------------
;; Javascript with Slime/Clojure
;;---------------------------------------------------------
(defun slime-eval-last-expression-as-scriptjure ()
  "Assume the last expression is valid scriptjure and send it to
slime to get javascript back"
  (interactive)
  (slime-eval `(swank:eval-and-grab-output
                ,(concat "(just-js " (slime-last-expression) ")"))))

(defun blah () (interactive) (save-excursion (slime-eval `(swank:eval-and-grab-output ,(concat "(def " (symbol-at-point) (progn (forward-sexp) (symbol-at-point)) ")")))))

(use-package ruby-mode
  :mode ("\\.rb\\'" . ruby-mode)
  :config
  (progn (use-package inf-ruby
           :load-path "~/.emacs.d/elpa/inf-ruby-2.1/")
         (use-package inf-ruby
           :load-path "~/.emacs.d/elpa/ruby-electric-1.1/")
         (use-package ruby-electric
           :config (add-hook 'ruby-mode-hook (lambda () (ruby-electric-mode t))))))

(use-package rainbow-mode
  :disabled t
  :commands rainbow-mode
  :init (add-hook 'emacs-lisp-mode-hook (lambda () (rainbow-mode t))))

(use-package multi-term
  :commands (multi-term multi-term-next)
  :config
  (setq multi-term-program "/bin/zsh"))

;; (bind "C-c t" multi-term-next)
;; (bind "C-c T" multi-term)

(eval-after-load 'clojure-mode
  '(define-clojure-indent (describe 'defun) (it 'defun)
     (slice 'defun) (page 'defun) (rule 'defun)
     (case 'defun) (deftest 'defun)
     (testing 'defun)
     (given 'defun)
     (using 'defun)
     (test 'defun)
     (defs 0)
     (with 'defun)
     (do-it 'defun)))

(bind "C-c :" slime-interactive-eval)

(defun screencast-start ()
     "Prepare frame for screencast"
     (interactive)
     (set-default-font "DejaVu Sans Mono-12")
     (set-frame-size (selected-frame) 90 22))

(use-package coffee-mode
  :load-path "~/.elisp/coffee-mode"
  :commands coffee-mode
  :mode ("\\.coffee$" . coffee-mode)
  :config
  (progn (setq-default tab-width 4)
         (use-package autopair)
 
         (add-hook 'coffee-mode-hook
                   '(lambda()
                      (define-key coffee-mode-map (kbd "C-c C-l") 'coffee-compile-buffer)
                      (define-key coffee-mode-map (kbd "C-c C-k") 'coffee-compile-file)
                      (autopair-mode)
                      (set (make-local-variable 'tab-width) 2)))
         ))


(use-package contentswitch
  :bind ("<f1>" . contentswitch))

(bind "M-S-SPC" (lambda () (interactive) (just-one-space 0)))

(defun delete-current-song ()
  (interactive)
  (let* ((emms-show-format "%s")
         (song (emms-show))
         (ok-to-delete "feelgood"))
    (if (string-match ok-to-delete song)
        (progn (move-file-to-trash song)
               (message (concat "Deleted " song)))
      (message "Didn't delete %s because didn't match %s" song ok-to-delete))))

(put 'narrow-to-region 'disabled nil)

;; (defadvice slime-repl-emit (after sr-emit-ad activate)
;;   (with-current-buffer (slime-output-buffer)
;;     (add-text-properties slime-output-start slime-output-end
;;                          '(font-lock-face slime-repl-output-face
;;                                           rear-nonsticky (font-lock-face)))))

;; (defadvice slime-repl-insert-prompt (after sr-prompt-ad activate)
;;   (with-current-buffer (slime-output-buffer)
;;     (let ((inhibit-read-only t))
;;       (add-text-properties slime-repl-prompt-start-mark (point-max)
;;                            '(font-lock-face slime-repl-prompt-face
;;                                             rear-nonsticky
;;                                             (slime-repl-prompt
;;                                              read-only
;;                                              font-lock-face
;;                                              intangible))))))

(defun slime-clojure-repl-setup ()
  (when (string-match "clojure" (slime-connection-name))
    (message "Setting up repl for clojure")
    (when (slime-inferior-process)
      (slime-redirect-inferior-output))

    (set-syntax-table clojure-mode-syntax-table)
    (clojure-mode-font-lock-setup)

    (setq lisp-indent-function 'clojure-indent-function)))

;; (add-hook 'slime-repl-mode-hook 'slime-clojure-repl-setup)
(bind "C-M-r" org-capture)

(use-package scratch
  :comamnds scratch)

;; (require 'hl-sexp)
;; (global-hl-sexp-mode)

;; #EEEEDD lightgoldenrod2

(defun slime-re-eval-last-input ()
  (interactive)
  (slime-eval-defun)
  (slime-switch-to-output-buffer)
  (slime-repl-previous-input)
  (slime-repl-return)
  (other-window-reverse))

(bind "M-P" slime-re-eval-last-input)

(bind "C-c q" delete-indentation)

;;; cause M-r only keeps one sexp unless you count them and pass num as arg
;; (bind "M-R" (lambda ()
;;               (interactive)
;;               (let ((beg (point))
;;                     (end (progn (backward-up-list+)
;;                                 (forward-char)
;;                                 (point))))
;;                 (kill-region beg end)
;;                 (paredit-splice-sexp))))

(bind "M-R" paredit-splice-sexp-killing-backward) 

(bind "C-h a" apropos)
(setq apropos-do-all t) ; display non-interactive functions

;; (setq ac-use-menu-map t)
;; ;; Default settings
;; (define-key ac-menu-map "\C-n" 'ac-next)
;; (define-key ac-menu-map "\C-p" 'ac-previous)
;; (define-key ac-mode-map (kbd "C-c h") 'ac-last-quick-help)

;(global-visual-line-mode)

;; (require 'popup-pos-tip)
;; (defadvice popup-tip
;;   (around popup-pos-tip-wrapper
;;           (string &rest args)
;;           activate)
;;   (if (eq window-system 'x)
;;       (apply 'popup-pos-tip string args)
;;     ad-do-it))

(bind "<kp-subtract>" beginning-of-defun)
(bind "<kp-add>" end-of-defun)

(use-package parenface
  :load-path "~/.elisp/parenface"
  :init
  (progn
    (eval-after-load "clojure-mode" '(add-hook 'clojure-mode-hook (paren-face-add-support clojure-font-lock-keywords))))
  :config (progn
            (set-face-foreground 'paren-face "#888")
            (set-face-foreground 'punctuation-face "#999")
            ;; (set-face-foreground 'paren-face "#777")
            ;; (set-face-foreground 'paren-face "#444")
            ))


;; (add-to-list 'load-path "~/.elisp/google-weather-el")
;; (require 'google-weather)
;; (require 'org-google-weather)

;; (require 'http-post-simple)

; (display-buffer ".emacs" t)

;; (add-to-list 'load-path "~/.elisp/tabbar")
;; (require 'aquamacs-tabbar)
;; (tabbar-mode)

;; (require 'tabbar-ruler)

;; (defun tabbar-buffer-groups (buffer)
;;   "Return the list of group names BUFFER belongs to.
;;  Return only one group for each buffer."
;;   (with-current-buffer (get-buffer buffer)
;;     (cond
;;      ((string-equal "*" (substring (buffer-name) 0 1))
;;       "Emacs Buffer")
;;      ((eq major-mode 'dired-mode)
;;       "Dired")
;;      (t
;;       "User Buffer")
;;      )))

;; (setq tabbar-buffer-groups-function 'tabbar-buffer-groups)

;; (setq tabbar-buffer-groups-function
;;       (lambda ()
;;         (list "All"))) ;; code by Peter Barabas

;; ;; Tabbar
;; (require 'tabbar)
;; (tabbar-mode)
;; (setq tabbar-buffer-groups-function
;;       (lambda ()
;;         (list "All Buffers")))

;; (setq tabbar-buffer-list-function
;;       (lambda ()
;;         (remove-if
;;          (lambda(buffer)
;;            (find (aref (buffer-name buffer) 0) " *"))
;;          (buffer-list))))

;; ;; add a buffer modification state indicator in the tab label,
;; ;; and place a space around the label to make it looks less crowd
;; (defadvice tabbar-buffer-tab-label (after fixup_tab_label_space_and_flag activate)
;;   (setq ad-return-value
;;      (if (and (buffer-modified-p (tabbar-tab-value tab))
;;               (buffer-file-name (tabbar-tab-value tab)))
;;          (concat " + " (concat ad-return-value " "))
;;           (concat " " (concat ad-return-value " ")))))

;; ;; called each time the modification state of the buffer changed
;; (defun ztl-modification-state-change ()
;;   (tabbar-set-template tabbar-current-tabset nil)
;;   (tabbar-display-update))
;; ;; first-change-hook is called BEFORE the change is made
;; (defun ztl-on-buffer-modification ()
;;   (set-buffer-modified-p t)
;;   (ztl-modification-state-change))
;; (add-hook 'after-save-hook 'ztl-modification-state-change)
;; ;; this doesn't work for revert, I don't know
;; ;;(add-hook 'after-revert-hook 'ztl-modification-state-change)
;; (add-hook 'first-change-hook 'ztl-on-buffer-modification) 

(setq ibuffer-saved-filter-groups
      '(("default"      
         ("emailatask"
          (filename . "/c01/emailatask"))
         ("Clojure"
          (or (mode . clojure-mode)
              (mode . slime-repl-mode)
              (name . "*SLIME Compilation*")
              (name . "*slime-events*")))
         ("Programming"
          (or
           (mode . c-mode)
           (mode . perl-mode)
           (mode . python-mode)
           (mode . emacs-lisp-mode)))
         ("Dired"
          (mode . dired-mode))
         ("Music"
          (name . "EMMS Playlist"))
         ("Org"
          (mode . org-mode))  
         ("Mail"
          (or
           (mode . message-mode)
           (mode . mail-mode)))
         ("ERC"   (mode . erc-mode)))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

;; (add-hook 'text-mode-hook 'c-subword-mode)

;;; start search at top of buffer
(bind "C-S-s" (lambda () (interactive) (beginning-of-buffer) (isearch-forward)))

;;; cause I hate switching package and then going to repl
(defun jsj-slime-goto-current-ns ()
  (interactive)
  (slime-repl-set-package (slime-current-package))
  (slime-switch-to-output-buffer))
(bind "C-c C-S-z" jsj-slime-goto-current-ns)
(bind "C-S-c C-S-z" jsj-slime-goto-current-ns)

;; (define-key slime-mode-map (kbd "C-M-x")
;;   (lambda ()
;;     (interactive)
;;     (slime-compile-defun)))

(defun jsj-emms-show ()
  ;; too slow when switchign several songs! expire option -t doesn't seem to work
  (shell-command (concat "notify-send -t 1000 -i info \"emms\" \""
                         (emms-track-description
                          (emms-playlist-current-selected-track))
                         "\""))
  (emms-show))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-auto-show-menu 0.5)
 '(ac-delay 0.3)
 '(ac-modes (quote (slime-repl-mode emacs-lisp-mode lisp-interaction-mode c-mode cc-mode c++-mode java-mode clojure-mode scala-mode scheme-mode ocaml-mode tuareg-mode perl-mode cperl-mode python-mode ruby-mode ecmascript-mode javascript-mode js-mode js2-mode php-mode css-mode makefile-mode sh-mode fortran-mode f90-mode ada-mode xml-mode sgml-mode)))
 '(ac-quick-help-delay 0.6)
 '(ahk-syntax-directory "c:/Program Files/AutoHotkey/Extras/Editors/Syntax")
 '(appt-display-diary nil)
 '(appt-display-duration 5)
 '(appt-display-interval 5)
 '(appt-message-warning-time 10)
 '(calendar-font-lock-keywords (quote (("\\(A\\(?:pril\\|ugust\\)\\|December\\|February\\|J\\(?:anuary\\|u\\(?:ly\\|ne\\)\\)\\|Ma\\(?:rch\\|y\\)\\|\\(?:Novem\\|Octo\\|Septem\\)ber\\) -?[0-9]+" . font-lock-type-face) ("S[au]" . font-lock-comment-face) ("Fr\\|Mo\\|S[au]\\|T[hu]\\|We" . font-lock-string-face))) t)
 '(clojure-mode-use-backtracking-indent t)
 '(column-number-mode t)
 '(completion-show-help nil)
 '(contentswitch-jump-to-match-location t)
 '(css-indent-offset 2)
 '(dired-dwim-target t)
 '(dired-listing-switches "-haAl")
 '(dired-recursive-deletes (quote always))
 '(display-time-world-list (quote (("America/Los_Angeles" "Pacific") ("America/Denver" "Mountain") ("America/Chicago" "Midwest") ("America/New_York" "Eastern") ("Pacific/Honolulu" "Hawaii") ("Europe/London" "London") ("Europe/Paris" "Paris") ("Europe/Berlin" "Berlin") ("Asia/Calcutta" "Bangalore") ("Asia/Tokyo" "Tokyo") ("Asia/Shanghai" "Shanghai") ("Australia/Sydney" "Sydney"))))
 '(ecb-gzip-setup (quote cons))
 '(ecb-layout-name "left13")
 '(ecb-options-version "2.32")
 '(emms-player-list (quote (emms-player-mplayer-playlist emms-player-mplayer)))
 '(emms-player-mplayer-command-name "mplayer")
 '(emms-playlist-buffer-name "EMMS Playlist")
 '(emms-playlist-mode-center-when-go t)
 '(eshell-save-history-on-exit t)
 '(fill-column 72)
 '(helm-c-adaptive-history-length 500)
 '(hfyview-quick-print-in-files-menu t)
 '(ido-create-new-buffer (quote always))
 '(ido-enable-flex-matching t)
 '(ido-enter-matching-directory (quote first))
 '(ido-everywhere t)
 '(ido-max-directory-size 3000000)
 '(ido-max-prospects 10)
 '(ido-max-window-height 1)
 '(ido-max-work-directory-list 50)
 '(ido-max-work-file-list 100)
 '(ido-use-filename-at-point nil)
 '(imenu-max-items 250)
 '(js2-auto-indent-p t)
 '(js2-enter-indents-newline t)
 '(js2-indent-on-enter-key t)
 '(list-directory-verbose-switches "-l")
 '(ls-lisp-dirs-first t)
 '(ls-lisp-format-time-list (quote ("%Y-%m-%d" "%Y-%m-%d")))
 '(ls-lisp-ignore-case t)
 '(ls-lisp-use-localized-time-format t)
 '(ls-lisp-verbosity nil)
 '(notmuch-message-headers (quote ("Subject" "To" "Cc" "Date" "User-Agent" "X-Mailer")))
 '(notmuch-message-headers-visible t)
 '(notmuch-saved-searches (quote (("inbox+unread" . "tag:inbox and tag:unread") ("inbox" . "tag:inbox") ("unread" . "tag:unread"))))
 '(notmuch-search-oldest-first nil)
 '(notmuch-show-logo nil)
 '(nxhtml-skip-welcome t)
 '(org-agenda-files (quote ("~/org/strangeloop.org" "~/org/goals.org" "~/org/birthdays.org" "~/org/workout.org" "~/org/recipes.org" "~/org/someday.org" "~/org/quotes.org" "~/org/todo.org" "~/org/calendar.org" "~/org/shopping.org")))
 '(org-agenda-window-frame-fractions (quote (0.1 . 0.75)))
 '(org-atom-publish-content t)
 '(org-export-with-section-numbers nil)
 '(org-file-apps (quote ((auto-mode . emacs) ("\\.mm\\'" . default) ("\\.x?html?\\'" . default) ("\\.pdf\\'" . "zathura %s"))))
 '(org-log-repeat nil)
 '(org-return-follows-link t)
 '(recentf-max-menu-items 1000)
 '(recentf-max-saved-items nil)
 '(regex-tool-backend (quote perl))
 '(scroll-bar-mode (quote right))
 '(session-locals-include (quote (overwrite-mode buffer-undo-list)))
 '(show-paren-mode t)
 '(slime-autodoc-delay 0.1)
 '(slime-compilation-finished-hook (quote (slime-show-compilation-log slime-goto-first-note)))
 '(slime-complete-symbol*-fancy t)
 '(slime-complete-symbol-function (quote slime-fuzzy-complete-symbol))
 '(smex-auto-update t)
 '(smex-history-length 70)
 '(speedbar-hide-button-brackets-flag t)
 '(speedbar-indentation-width 2)
 '(speedbar-show-unknown-files t)
 '(speedbar-update-flag nil)
 '(speedbar-use-images nil)
 '(sql-product (quote mysql))
 '(sr-windows-locked nil)
 '(tabbar-home-button (quote ((" Modes") "")))
 '(tabbar-mwheel-mode-action (quote cycle-tabs))
 '(tabbar-scroll-left-button (quote ((" <") " <")))
 '(tabbar-scroll-right-button (quote (("  > ") " >")))
 '(todochiku-icons-directory "~/todochiku-icons")
 '(tooltip-delay 2)
 '(w32shell-shell (quote cmd))
 '(work-timer-relax-time 5)
 '(work-timer-working-time 25))


;; (setq browse-url-browser-function 'w3m-browse-url)
;; (autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
;; optional keyboard short-cut
(global-set-key "\C-xm" 'browse-url-at-point)

(defun insert-javadoc-call ()
  (interactive)
  (insert "(clojure.java.javadoc/javadoc )")
  (backward-char))

(bind "C-x j" insert-javadoc-call)

(defun djcb-find-file-as-root ()
  "Like `ido-find-file, but automatically edit the file with
root-privileges (using tramp/sudo), if the file is not writable by
user."
  (interactive)
  (let ((file (ido-read-file-name "Edit as root: ")))
    (unless (file-writable-p file)
      (setq file (concat "/sudo:root@localhost:" file)))
    (find-file file)))
;; or some other keybinding...
(global-set-key (kbd "C-x F") 'djcb-find-file-as-root)

;;; use print
;; (setq slime-message-function 'osd-slime-message)

(defun osd-slime-message (format-string &rest args)
  (shell-command (concat "notify-send -t 1000 -i clojure Clojure " (apply #'format format-string args) ""))
  (apply #'message format-string args ))

;; (use-package flex-isearch
;;     :load-path "~/.elisp/flex-isearch/"
;;     :init (flex-isearch-mode))

(use-package winner-mode
  :init (winner-mode 1))

;;; thanks tomoj converts (if x (foo x
;; (bind "C-c C-f" paredit-convolute-sexp)

;; fringe stuff
;; (setq-default indicate-empty-lines t)
;; (setq-default indicate-buffer-boundaries '((top . nil) (t . left)))
;; the following is size 7 for me...
;(set-face-font 'default "-unknown-Envy Code R-normal-normal-normal-*-13-*-*-*-m-0-iso10646-1")
;;(set-default-font "Envy Code R-7") ;; doesn't work consistently ;(

;; (defun ido-goto-symbol (&optional symbol-list)
;;   "Refresh imenu and jump to a place in the buffer using Ido."
;;   (interactive)
;;   (unless (featurep 'imenu)
;;     (require 'imenu nil t))
;;   (cond
;;    ((not symbol-list)
;;     (let ((ido-mode ido-mode)
;;           (ido-enable-flex-matching
;;            (if (boundp 'ido-enable-flex-matching)
;;                ido-enable-flex-matching t))
;;           name-and-pos symbol-names position)
;;       (unless ido-mode
;;         (ido-mode 1)
;;         (setq ido-enable-flex-matching t))
;;       (while (progn
;;                (imenu--cleanup)
;;                (setq imenu--index-alist nil)
;;                (ido-goto-symbol (imenu--make-index-alist))
;;                (setq selected-symbol
;;                      (ido-completing-read "Symbol? " symbol-names))
;;                (string= (car imenu--rescan-item) selected-symbol)))
;;       (unless (and (boundp 'mark-active) mark-active)
;;         (push-mark nil t nil))
;;       (setq position (cdr (assoc selected-symbol name-and-pos)))
;;       (pulse-momentary-highlight-one-line (point))
;;       (cond
;;        ((overlayp position)
;;         (goto-char (overlay-start position)))
;;        (t
;;         (goto-char position)))))
;;    ((listp symbol-list)
;;     (dolist (symbol symbol-list)
;;       (let (name position)
;;         (cond
;;          ((and (listp symbol) (imenu--subalist-p symbol))
;;           (ido-goto-symbol symbol))
;;          ((listp symbol)
;;           (setq name (car symbol))
;;           (setq position (cdr symbol)))
;;          ((stringp symbol)
;;           (setq name symbol)
;;           (setq position
;;                 (get-text-property 1 'org-imenu-marker symbol))))
;;         (unless (or (null position) (null name)
;;                     (string= (car imenu--rescan-item) name))
;;           (add-to-list 'symbol-names name)
;;           (add-to-list 'name-and-pos (cons name position))))))))

;; (global-set-key "\C-c." 'ido-goto-symbol)
(use-package idomenu
  :bind ("C-c ." . idomenu))

(use-package webjump
  :commands webjump
  :config (setq webjump-sites (append '(("Java API" . [simple-query "www.google.com" "http://www.google.ca/search?hl=en&as_sitesearch=http://java.sun.com/javase/6/docs/api/&q=" ""])) webjump-sample-sites)))

(add-hook 'align-load-hook
          (lambda ()
            (add-to-list 'align-rules-list
                         '(text-column-whitespace
                           (regexp . "\\(^\\|\\S-\\)\\([ \t]+\\)")
                           (group  . 2)
                           (modes  . align-text-modes)
                           (repeat . t)))
            (add-to-list 'align-lisp-modes 'clojure-mode)
            (add-to-list 'align-rules-list
                         '(clojure-keyword-map
                           (regexp . ":[^\s]+\\(\s+\\).+")
                           (group  . 1)
                           (modes  . align-lisp-modes)))))

(defun align-repeat (start end regexp)
  "Repeat alignment with respect to 
     the given regular expression."
  (interactive "r\nsAlign regexp: ")
  (align-regexp start end 
                (concat "\\(\\s-*\\)" regexp) 1 1 t))

(bind "C-c C-R" slime-eval-region)

(defun delete-this-buffer-and-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(global-set-key (kbd "C-c k") 'delete-this-buffer-and-file)

(defun backward-kill-word+1 ()
  (interactive)
  (backward-kill-word 1)
  (backward-delete-char 1))

(global-set-key (kbd "<M-S-backspace>") 'backward-kill-word+1)

(use-package javadoc-help
  :bind (("<f9> j" . javadoc-lookup)
         ("<f9> J" . javadoc-help))
  :config (javadoc-set-predefined-urls '("/usr/share/doc/openjdk-6-jre-headless/api/")))

(defun byte-recompile-elisp-directory ()
  (interactive)
  (byte-recompile-directory "~/.elisp" 0 t))

(defun dired-view-file ()
  "In Dired, examine a file in view mode, returning to Dired when done.
When file is a directory, show it in this buffer if it is inserted.
Otherwise, display it in another buffer."
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (if (file-directory-p file)
        (or (and (cdr dired-subdir-alist)
                 (dired-goto-subdir file))
            (dired file))
      (view-file-other-window file))))

(defun dired-view-file-next (&optional reverse)
  (interactive)
  (View-quit)
  (if reverse (previous-line)
    (next-line))
  (dired-view-file))

(defun dired-view-file-previous ()
  (interactive)
  (dired-view-file-next 1))

(add-hook 'view-mode-hook
          (lambda ()
            (define-key view-mode-map (kbd "n") 'dired-view-file-next)
            (define-key view-mode-map (kbd "p") 'dired-view-file-previous)))

;; (autoload 'align-let "align-let" nil t)
;; (autoload 'align-let-keybinding "align-let" nil t)
;; (add-hook 'emacs-lisp-mode-hook 'align-let-keybinding)
;; (add-hook 'scheme-mode-hook     'align-let-keybinding)
;; (let ((abcsadf 2)
;;       (b       2)))

;; (let [abc 3
;;           c 3])

;; (setq foo 1
;;       b   2)

(defun jsj-ac-show-help ()
  "show docs for symbol at point or at beginning of list if not on a symbol"
  (interactive)
  (let ((s (save-excursion
             (or (symbol-at-point)
                 (progn (backward-up-list)
                        (forward-char)
                        (symbol-at-point))))))
    (pos-tip-show (or (if (equal major-mode 'emacs-lisp-mode)
                          (ac-symbol-documentation s)
                        (ac-slime-documentation (symbol-name s))) "no docs")
                  'popup-tip-face
                  ;; 'alt-tooltip
                  (point)
                  nil
                  -1)))

(define-key lisp-mode-shared-map (kbd "C-c C-h") 'jsj-ac-show-help)

;; (defface alt-tooltip
;;   '((t
;;      :background "gray85"
;;      :foreground "black"
;;      :inherit variable-pitch))
;;   "Face for my tooltip.")

(defun backward-up-list+-1 ()
  "go to left of )"
  (interactive)
  (backward-up-list+)
  (forward-char))
;;; _cool
(define-key lisp-mode-shared-map (kbd "C-M-9") 'backward-up-list+-1)

(defun up-list+-1 ()
  "go to right of ("
  (interactive)
  (up-list+)
  (backward-char))
;;; _cool
(define-key lisp-mode-shared-map (kbd "C-M-0") 'up-list+-1)

(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))
(global-set-key (kbd "M-`") 'jump-to-mark)

;;; more precise move keys
(setq shift-select-mode nil)

(use-package misc)
(global-set-key (kbd "M-F") 'forward-to-word)
(global-set-key (kbd "M-B") 'backward-to-word)

;; (add-to-list 'align-rules-list
;;              '(text-column-whitespace
;;                (regexp . "\\(^\\|\\S-\\)\\([ \t]+\\)")
;;                (group  . 2)
;;                (modes  . align-text-modes)
;;                (repeat . t)))

;; (autoload 'align-cols "align" "Align text in the region." t)

(use-package align-cljlet
  :commands (align-cljlet align-map align-defroutes)
  :load-path "~/.elisp/align-cljlet")

;; (define-key my-keys-minor-mode-map (kbd "C-.") 'other-window)
;; (define-key my-keys-minor-mode-map (kbd "C-,") 'other-window-reverse)

;; (bind "C-." iflipb-next-buffer)
;; (bind "C-," iflipb-previous-buffer)

(defun insert-thread-first ()
  (interactive)
  (insert "(-> " ")" )
  (backward-char 1))

(defun insert-thread-last ()
  (interactive)
  (insert "(->> " ")" )
  (backward-char 1))

(defadvice he-substitute-string (after he-paredit-fix)
  "remove extra paren when expanding line in paredit"
  (if (and paredit-mode (equal (substring str -1) ")"))
      (progn (backward-delete-char 1) (forward-char))))

;; (defun he-substitute-string (str &optional trans-case)
;;   (let ((trans-case (and trans-case
;;                          case-replace
;;                          case-fold-search))
;;         (newpos (point-marker))
;;         (subst ()))
;;     (goto-char he-string-beg)
;;     (setq subst (if trans-case (he-transfer-case he-search-string str) str))
;;     (setq he-tried-table (cons subst he-tried-table))
;;     (insert subst)
;;     (delete-region (point) he-string-end)
;;     (goto-char newpos)
;;     ;; only lines that changed:
;;     (if (and paredit-mode (equal (substring subst -1) ")"))
;;         (progn (backward-delete-char 1) (forward-char)))))

;;; fix hippie expand bug when using paredit where extra paren
;; (require 'hippie-exp)

;; (fset 'he-substitute-string-orig (symbol-function 'he-substitute-string))

;; (defun he-substitute-string (str &optional trans-case)
;;   (he-substitute-string-orig str trans-case)
;;   (if (and paredit-mode (equal (substring str -1) ")"))
;;       (progn (backward-delete-char 1) (forward-char))))

;; (global-set-key "\C-h" 'ehelp-command)

;; (require 'secrets)

;;; _cool
;; (add-to-list 'load-path "~/.elisp/workgroups.el/")
;; (require 'workgroups)
;; (setq wg-prefix-key (kbd "C-c w"))
;; (workgroups-mode 1)

(setq display-buffer-reuse-frames t)

(use-package nexus
  :load-path "~/.elisp/emacs-nexus/"
  :commands nexus-search)

(use-package doc-view
  :commands doc-view-mode
  :config
  (progn (setq doc-view-resolution 160)
         (define-key doc-view-mode-map (kbd "C-v") 'doc-view-scroll-up-or-next-page)
         (define-key doc-view-mode-map (kbd "M-v") 'doc-view-scroll-down-or-previous-page)))

;;; thanks retroj for helping w/ this
(defun isearch-exit-at-opposite-end ()
  "by default isearch forward ends at end and isearch backward
  ends at beginning. this makes it do the opposite."
  (interactive)
  (add-hook 'isearch-mode-end-hook 'isearch-move-point-to-opposite-end)
  (isearch-exit))

(defun isearch-move-point-to-opposite-end ()
  (funcall (if isearch-forward #'backward-char #'forward-char)
           (length isearch-string))
  (remove-hook 'isearch-mode-end-hook 'isearch-move-point-to-opposite-end))

(define-key isearch-mode-map (kbd "<C-return>") 'isearch-exit-at-opposite-end)

(defun try-expand-flexible-abbrev (old)
  "Try to complete word using flexible matching.

Flexible matching works by taking the search string and then
interspersing it with a regexp for any character. So, if you try
to do a flexible match for `foo' it will match the word
`findOtherOtter' but also `fixTheBoringOrange' and
`ifthisisboringstopreadingnow'.

The argument OLD has to be nil the first call of this function, and t
for subsequent calls (for further possible completions of the same
string).  It returns t if a new completion is found, nil otherwise."
  (if (not old)
      (progn
        (he-init-string (he-lisp-symbol-beg) (point))
        (if (not (he-string-member he-search-string he-tried-table))
            (setq he-tried-table (cons he-search-string he-tried-table)))
        (setq he-expand-list
              (and (not (equal he-search-string ""))
                   (he-flexible-abbrev-collect he-search-string)))))
  (while (and he-expand-list
              (he-string-member (car he-expand-list) he-tried-table))
    (setq he-expand-list (cdr he-expand-list)))
  (if (null he-expand-list)
      (progn
        (if old (he-reset-string))
        ())
    (progn
      (he-substitute-string (car he-expand-list))
      (setq he-expand-list (cdr he-expand-list))
      t)))

(defun he-flexible-abbrev-collect (str)
  "Find and collect all words that flex-matches STR.
See docstring for `try-expand-flexible-abbrev' for information
about what flexible matching means in this context."
  (let ((collection nil)
        (regexp (he-flexible-abbrev-create-regexp str)))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp regexp nil t)
        ;; Is there a better or quicker way than using
        ;; `thing-at-point' here?
        (setq collection (cons (thing-at-point 'symbol) collection))))
    collection))

(defun he-flexible-abbrev-create-regexp (str)
  "Generate regexp for flexible matching of STR.
See docstring for `try-expand-flexible-abbrev' for information
about what flexible matching means in this context."
  (let ((word "\\w*-*"))
    (concat "\\b" (mapconcat (lambda (x) (concat word (list x))) str "")
            word "\\b")))

(setq hippie-expand-try-functions-list
      (cons 'try-expand-flexible-abbrev hippie-expand-try-functions-list))

(defun flex-match? (text search)
  (let ((re (mapconcat #'regexp-quote (split-string search "") ".*")))
    (string-match re text)))

(flex-match? "foo-bar" "f-b")

;;; thanks thunk
(defun flexible-yank ()
  (interactive)
  (insert (ido-completing-read "Select kill: " kill-ring)))

(global-set-key (kbd "M-Y") 'flexible-yank)

;; and the session
(setq desktop-restore-eager 20
      desktop-lazy-verbose nil)
;; (desktop-save-mode 1)
;; (add-hook 'auto-save-hook (lambda () (desktop-save-in-desktop-dir)))
;; doesn't appear to work
;; (setq desktop-clear-preserve-buffers
;;       (remove "\\*scratch\\*" desktop-clear-preserve-buffers))

(savehist-mode 1)

;;; I wish ido would use ffap if given prefix arg. Make it so! Btw
;;; ffap-require-prefix t and ido-use-filename-at-point t wouldn't do this
(defun ido-find-file (&optional arg)
  (interactive "p")
  (let ((ido-use-filename-at-point (if (= arg 4) t nil)))
    (ido-file-internal ido-default-file-method)))

(setq org-modules '(org-habit org-wikinodes))
(setq org-wikinodes-create-targets t
      org-link-search-create-targets t)

;; cheap wiki mode
;; (add-hook 'org-follow-link-hook 'org-narrow-to-subtree)

(use-package hide-mode-line
  :commands hide-mode-line)

(use-package regadhoc
  :bind (("\C-xrj" . regadhoc-jump-to-registers)
         ("\C-x/" . regadhoc-register))
  :config (setq regadhoc-register-char-list (list ?a ?s ?d ?f ?j ?k ?l)))

(use-package scpaste
  :commands (scpaste scpaste-region)
  :config (setq scpaste-http-destination "http://jaderholm.com/paste"
                scpaste-scp-destination "jaderholm.com:www/jaderholm.com/paste"))

(use-package erc
  :commands erc
  :config
  (progn (add-hook 'erc-mode-hook
                   (lambda ()
                     (auto-fill-mode 0)
                     (setq erc-fill-column 80)
                     (setq erc-timestamp-format "[%H:%M] ")
                     (setq erc-fill-function 'erc-fill-static)
                     (setq erc-fill-prefix "          ")
                     (setq erc-fill-static-center 12)))


         (use-package pp)
         (defun ted-erc-fill-elisp ()
           "If it appears to be elisp, pretty print region.
 Otherwise, fall back to `erc-fill-variable'."
           (let ((print-escape-newlines t)
                 (pp-escape-newlines t))
             (goto-char (point-min))
             (if (re-search-forward "Elisp: " nil t)
                 (let* ((start (point))
                        (end (progn (forward-sexp 1) (point)))
                        (psexp (pp-to-string
                                (car
                                 (read-from-string
                                  (buffer-substring-no-properties
                                   start end))))))
                   (delete-region start end)
                   (insert "\n" psexp))
               (erc-fill-variable))))

         (setq erc-fill-function 'ted-erc-fill-elisp)

         (and
          (use-package erc-highlight-nicknames)
          (add-to-list 'erc-modules 'highlight-nicknames)
          (erc-update-modules))

         (add-to-list 'erc-modules 'scrolltobottom)

         (setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK" "MODE"))
         ))




;;; might speed up emacs saves
(setq vc-handled-backends '(Git Hg CVS SVN))

(global-set-key (kbd "C-c ;") 'bury-buffer)

(use-package saveplace
  :config (setq-default save-place t))

(global-set-key (kbd "M-$") 'replace-string)
(global-set-key (kbd "C-c s") 'replace-string)

;;; make it more like vim I think, keep the char I zap to. like xemacs
;;; zap-up-to-char.
;;; TODO replace with zap-up-to-char from misc.el
(defadvice zap-to-char (after my-zap-to-char-advice (arg char) activate)
  "Kill up to the ARG'th occurence of CHAR, and leave CHAR.
  The CHAR is replaced and the point is put before CHAR."
  (insert char)
  (forward-char -1))

(defun jump-to-char (arg char)
  (interactive "p\ncJump to char: ")
  (forward-char)
  (let ((case-fold-search nil)); not sure if I want this
    (search-forward (char-to-string char) nil nil arg))
  (forward-char -1))

(global-set-key (kbd "C-z") 'jump-to-char)

(use-package linum-relativenumber
  :load-path "~/.elisp/linum-relativenumber")

(setq eval-expression-print-length 50)

(use-package djcb-cursor
  :disabled t)

(use-package re-builder
  :commands re-builder
  :config (setq reb-re-syntax 'string))

(use-package pomodoro
  :commands pomodoro)

;;---------------------------------------------------------
;; durendal
;;---------------------------------------------------------
;; (add-to-list 'load-path "~/.elisp/durendal/")
;; (require 'durendal)
;; (durendal-enable-slime-repl-font-lock)

(defun backward-kill-sexp ()
  (interactive)
  (kill-sexp -1))

(bind "C-M-S-k" backward-kill-sexp)

;;; similar to o and O in vim. Use M-1 C-o to go above and M-2 C-o to go
;;; below. C-o is normal emacs behavior
(defun jsj-open-line (&optional alt)
  (interactive "P")
  (case alt
    (1 (beginning-of-line)
       (newline)
       (forward-line -1)
       (indent-according-to-mode))
    (2 (end-of-line)
       (newline-and-indent))
    (t (save-excursion (paredit-newline)))))

;; (define-key global-map (kbd "C-o") 'jsj-open-line)

(defun jsj-open-line-after ()
  (interactive)
  (end-of-line)
  (newline-and-indent))

(define-key global-map (kbd "<C-return>") 'jsj-open-line-after)

(defun jsj-open-line-before ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-according-to-mode))

(define-key global-map (kbd "<M-C-return>") 'jsj-open-line-before)
(define-key global-map (kbd "<S-C-return>") 'jsj-open-line-before)

(defun jsj-insert-line-after ()
  "same as C-o when run at end of line but not when run elsewhere
because it doesn't mess with text on current line"
  (interactive)
  (save-excursion (next-line)
                  (beginning-of-line)
                  (newline)))

;; (global-set-key (kbd "M-o") 'jsj-insert-line-after)

(defun jsj-insert-line-before ()
  (interactive)
  (save-excursion (beginning-of-line)
                  (newline)))

(global-set-key (kbd "M-O") 'jsj-insert-line-before)

(defun jsj-remove-line-after ()
  "like C-x C-o without having to move to blank line"
  (interactive)
  (save-excursion (forward-paragraph)
                  (beginning-of-line)
                  (delete-blank-lines)))

(global-set-key (kbd "C-x M-o") 'jsj-remove-line-after)

(defun jsj-remove-line-before ()
  (interactive)
  (save-excursion (backward-paragraph)
                  (delete-blank-lines)))

(global-set-key (kbd "C-x M-O") 'jsj-remove-line-before)

(add-hook 'slime-load-hook
          (lambda ()
            (setq slime-protocol-version 'ignore)))
(setq slime-protocol-version 'ignore)

;; (setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; (global-unset-key (kbd "C-x b"))
;; (global-unset-key (kbd "C-x C-s"))



(define-key isearch-mode-map (kbd "M-o") 'isearch-occur)

;;; offby1
(defun search-all-buffers (regexp)
  (interactive "sRegexp: ")
  (multi-occur-in-matching-buffers "." regexp t))
(global-set-key (kbd "S-<f1>") 'search-all-buffers)

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; mpd music client
(add-to-list 'load-path "~/.elisp/mingus")
(autoload 'mingus "mingus-stays-home" nil t)

(defun async-shell-command-no-output (command)
  (shell-command (concat command " &")))

;; (helm-dired-bindings 1)

;; (defun highlight-clojure-test-mode ()
;;   (interactive)
;;   (set-face-background 'mode-line (cond
;;                                    (clojure-test-mode "red")
;;                                    (t "grey75"))
;;                        (selected-frame)))

;; (add-hook 'window-configuration-change-hook 'highlight-clojure-test-mode)

;; (global-set-key [f5] 'slime-js-reload)
;; (add-hook 'js2-mode-hook
;;           (lambda ()
;;             (slime-js-minor-mode 1)))

;; (add-hook 'css-mode-hook
;;           (lambda ()
;;             (define-key css-mode-map "\M-\C-x" 'slime-js-refresh-css)))

;; (defun my-javadoc-setup ()
;;   (setq slime-javadoc-local-paths
;;         (list (expand-file-name "/usr/share/doc/openjdk-6-doc"))))

;; (add-hook 'slime-connected-hook 'my-javadoc-setup)

(use-package ibuffer-vc
  :load-path "~/.elisp/ibuffer-vc"
  :init (progn (add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-vc-set-filter-groups-by-vc-root)
            (ibuffer-do-sort-by-alphabetic)))

(setq ibuffer-formats
      '((mark modified read-only vc-status-mini " "
              (name 18 18 :left :elide)
              " "
              (size 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " "
              (vc-status 16 16 :left)
              " "
              filename-and-process)))))

(use-package gen-pass
  :commands gen-pass)

(use-package find-func
  :init (use-package find-func-extension )
  :bind ("C-h C-f" . find-function-or-variable-at-point))

(add-hook 'grep-mode-hook
             (lambda ()
               (use-package grep-a-lot
                 :init (grep-a-lot-setup-keys))))

;; I only use M-0 style prefixes
(bind "C-0" delete-window)
(bind "C-1" delete-other-windows)
(bind "C-2" split-window-vertically)
(bind "C-3" split-window-horizontally)
(bind "C-5" make-frame-command)

(defalias 'fgd 'find-grep-dired)
(defalias 'dml 'delete-matching-lines)
(defalias 'dtw 'delete-trailing-whitespace)
(defalias 'lcd 'list-colors-display)
(defalias 'eb 'eval-buffer)
(defalias 'er 'eval-region)
(defalias 'om 'org-mode)
(defalias 'glm 'global-linum-mode)

(use-package pwsafe
  :commands pwsafe
  :config (setq pwsafe-keep-passwd t))

;; (add-hook 'diff-mode-hook (lambda () (diff-auto-refine-mode 1)))

(use-package mo-git-blame
  :commands (mo-git-blame-file mo-git-blame-current))

(setq inferior-lisp-program "sbcl")

;; (require 'ansi-color)
;; (setq ansi-color-names-vector
;;       (vector (frame-parameter nil 'background-color)
;;               "#f57900" "#8ae234" "#edd400" "#729fcf"
;;               "#ad7fa8" "cyan3" "#eeeeec")
;;       ansi-term-color-vector ansi-color-names-vector
;;       ansi-color-map (ansi-color-make-color-map))

;; (add-to-list 'load-path "~/.elisp/deft")
;; (require 'deft)
;; (setq deft-directory "~/Desktop/Dropbox/notes/")

(require 'eval-sexp-fu)
(eval-sexp-fu-flash-mode 1)

;; (setq inferior-lisp-program "script/repl")

(defun bcrepl ()
  (interactive)
  (let ((default-directory "/home/scott/src/clojurescript/"))
    (inferior-lisp "script/repl")
    (async-shell-command (concat "google-chrome file://"
                                 default-directory "samples/repl/index.html")))
  (insert "(require '[cljs.repl :as repl])
           (require '[cljs.repl.browser :as browser])
           (def env (browser/repl-env :port 8090))
           (repl/repl env)
           "))

(defun repljs ()
  (interactive)
  (let ((default-directory "/home/scott/src/clojurescript/"))
    (inferior-lisp "script/repljs")))

(use-package fixme-mode
  :init (fixme-mode))

;; FIXME something
;; TODO something

;;  _cool  C-M-h brings up list of killring

;; (setq mode-line-format
;;   (list
;;     ;; the buffer name; the file name as a tool tip
;;     '(:eval (propertize "%b " 'face 'font-lock-keyword-face
;;         'help-echo (buffer-file-name)))

;;     ;; line and column
;;     "(" ;; '%02' to set to 2 chars at least; prevents flickering
;;       (propertize "%02l" 'face 'font-lock-type-face) ","
;;       (propertize "%02c" 'face 'font-lock-type-face)
;;     ") "

;;     ;; relative position, size of file
;;     "["
;;     (propertize "%p" 'face 'font-lock-constant-face) ;; % above top
;;     "/"
;;     (propertize "%I" 'face 'font-lock-constant-face) ;; size
;;     "] "

;;     ;; the current major mode for the buffer.
;;     "["

;;     '(:eval (propertize "%m" 'face 'font-lock-string-face
;;               'help-echo buffer-file-coding-system))
;;     "] "


;;     "[" ;; insert vs overwrite mode, input-method in a tooltip
;;     '(:eval (propertize (if overwrite-mode "Ovr" "Ins")
;;               'face 'font-lock-preprocessor-face
;;               'help-echo (concat "Buffer is in "
;;                            (if overwrite-mode "overwrite" "insert") " mode")))

;;     ;; was this buffer modified since the last save?
;;     '(:eval (when (buffer-modified-p)
;;               (concat ","  (propertize "Mod"
;;                              'face 'font-lock-warning-face
;;                              'help-echo "Buffer has been modified"))))

;;     ;; is this buffer read-only?
;;     '(:eval (when buffer-read-only
;;               (concat ","  (propertize "RO"
;;                              'face 'font-lock-type-face
;;                              'help-echo "Buffer is read-only"))))
;;     "] "

;;     ;; add the time, with the date and the emacs uptime in the tooltip
;;     '(:eval (propertize (format-time-string "%H:%M")
;;               'help-echo
;;               (concat (format-time-string "%c; ")
;;                       (emacs-uptime "Uptime:%hh"))))
;;     " --"
;;     ;; i don't want to see minor-modes; but if you want, uncomment this:
;;     ;; minor-mode-alist  ;; list of minor modes
;;     "%-" ;; fill with '-'
;;     ))

(defun delete-indentation-backward ()
  (interactive)
  (delete-indentation 4))

(global-set-key (kbd "M-#") 'delete-indentation-backward)

(use-package elisp-slime-nav
  :commands elisp-slime-nav-mode
  :load-path "~/.elisp/elisp-slime-nav/"
  :init (add-hook 'emacs-lisp-mode-hook (lambda () (elisp-slime-nav-mode 1))))

(define-key global-map [(meta ?:)] 'pp-eval-expression)

(setq redisplay-dont-pause t)

(add-to-list 'load-path "~/.elisp/eproject/")
(require 'eproject)

;; not actually in the old version of slime I use w/ clojure
(setq slime-repl-history-remove-duplicates t
      slime-repl-history-trim-whitespaces t)

(setq sentence-end-double-space nil)

;; to encourage me to get used to M-a, M-e, M-s
;; (global-set-key (kbd "C-x C-f") nil)
;; (global-set-key (kbd "C-x b") nil)
;; (global-set-key (kbd "C-x C-s") nil)

;; keep appt/org from raising window in wm
(eval-after-load "appt" '(load "my-appt-disp-window"))

(setq enable-recursive-minibuffers t)

;; narrow scrollbars
;; (set-frame-parameter nil 'scroll-bar-width 10)
(add-to-list 'default-frame-alist '(scroll-bar-width . 10))
;; (add-to-list 'initial-frame-alist '(scroll-bar-width . 10))

(setq use-dialog-box nil
      diff-swithces "-u -w"
      ;; magit-diff-options "-u -w"
      )

(defun identifierify (start end)
  (interactive "r")
  (replace-regexp "[^[:alnum:]_ -.]" "" nil start end)
  (replace-regexp " +" "_" nil start end)
  (downcase-region start end)
  (goto-char end))

(setq mm-text-html-renderer 'shr)

;;; don't raise window
(setq appt-disp-window-function 'dont-raise-window)

(defun dont-raise-window (min-to-app new-time appt-msg)
  (flet ((raise-frame (&optional frame) (ignore)))
    (appt-disp-window min-to-app new-time appt-msg)))

(defun eval-sexp-comment-result ()
  (interactive)
  (save-excursion
    (slime-eval-print-last-expression (slime-last-expression)))
  (newline)
  (insert ";=> "))

;; M-x toggle-truncate-lines you are often looking for this as longlines-mode
;; or line-wrapr-mode
(setq truncate-lines t)

(use-package projectile
  :load-path "~/.elisp/projectile"
  :config (projectile-global-mode) )

;; projectile-jump-to-project-file (C-c p j)
;; projectile-grep-in-project (C-c p f)
;; projectile-replace-in-project (C-c p r)
;; projectile-switch-to-buffer (C-c p b)
;; projectile-multi-occur (C-c p o)
;; projectile-regenerate-tags (C-c p t)
;; projectile-invalidate-project-cache (C-c p i)

(setq echo-keystrokes 0.5)

(use-package key-chord
  :config (progn (key-chord-mode 1)
(key-chord-define-global ";s" 'scratch)
(key-chord-define-global ";f" 'insert-thread-first)
(key-chord-define-global ";l" 'insert-thread-last)
(key-chord-define-global ";g" 'magit-status)

(setq key-chord-two-keys-delay 0.3)
))

(use-package volatile-highlights
  :config (volatile-highlights-mode t))

(use-package cat-command
  :commands cat-command)

(add-to-list 'load-path "~/.elisp/mark-multiple.el/")
(global-set-key (kbd "C-<") 'mark-previous-like-this)
(global-set-key (kbd "C->") 'mark-next-like-this)

(use-package js2-refactor
  :load-path "~/.elisp/js2-refactor.el/"
  :commands js2-rename-var
  :init (eval-after-load "js2-mode" '(define-key js2-mode-map (kbd "C-c C-r") 'js2-rename-var)))

(use-package inline-string-rectangle
  :bind ("C-x r t" . inline-string-rectangle))

(use-package mark-more-like-this
  :bind (("C-<" . mark-previous-like-this)
         ("C->" . mark-next-like-this)
         ("C-M-m" . mark-more-like-this)))

(use-package rename-sgml-tag
  :bind ("C-c C-r" . rename-sgml-tag))

(when (fboundp 'winner-mode)
  (winner-mode 1))

;; (require 'highlight-parentheses)
;; (define-globalized-minor-mode global-highlight-parentheses-mode
;;   highlight-parentheses-mode
;;   (lambda ()
;;     (highlight-parentheses-mode t)))
;; (global-highlight-parentheses-mode t)
;; (setq hl-paren-colors '("yellow" "gold" "#777"))

;; (add-to-list 'load-path "~/.elisp/ethan-wspace/lisp/")
;; (require 'ethan-wspace)

(add-to-list 'auto-mode-alist '("\\.jsx$" . js-mode))

(bind "s-<next>" scroll-down-command)
(bind "s-<prior>" scroll-up-command)

(use-package evil
  :load-path "~/.elisp/evil/"
  :commands evil-mode)

(use-package ack
  :load-path "~/.elisp/ack-el"
  :commands ack
  :bind (("C-c g d" . ack-with-dir)
         ("C-c g g" . ack))
  :config
  (progn (setq ack-command "agg ")

         (defun ack-with-dir ()
           (interactive)
           (let ((current-prefix-arg '(16)))
             (call-interactively #'ack)))))

(use-package regex-tool
  :load-path "~/.elisp/ruby-tools"
  :commands regex-tool)

;; ;; Auto-revert-mode for version-controlled files
;; (defadvice vc-find-file-hook (after auto-revert-mode-for-vc activate)
;;   "vc-find-file-hook advice for activating auto-revert-mode"
;;   (when vc-mode (auto-revert-mode 1)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(use-package ace-jump-mode
  :load-path "~/.elisp/ace-jump-mode/"
  :bind ("<escape>" . ace-jump-mode))

;;; to use call M-x global-command-log-mode and M-x clm/open-command-log-buffer
(add-to-list 'load-path "~/.elisp/command-log-mode/")
(autoload 'command-log-mode "command-log-mode" nil t)

(add-to-list 'load-path "~/.elisp/hippie-expand-slime")
(autoload 'set-up-slime-hippie-expand "hippie-expand-slime")
(add-hook 'slime-mode-hook 'set-up-slime-hippie-expand)
(add-hook 'slime-repl-mode-hook 'set-up-slime-hippie-expand)

(define-key read-expression-map [(tab)] 'hippie-expand)

;; (setq hippie-expand-try-functions-list
;;       '(try-complete-file-name-partially
;;         try-complete-file-name
;;         try-expand-all-abbrevs
;;         try-expand-list
;;         try-expand-line
;;         try-expand-dabbrev
;;         try-expand-dabbrev-all-buffers
;;         try-expand-dabbrev-from-kill
;;         try-complete-lisp-symbol-partially
;;         try-complete-lisp-symbol))

;; (add-to-list 'load-path "~/.elisp/smart-tab/")
;; (require 'smart-tab)
;; (global-smart-tab-mode 1)

;; (add-to-list 'load-path "~/.elisp/popwin-el/")
;; (require 'popwin)
;; (setq display-buffer-function 'popwin:display-buffer)

;; (setq anything-samewindow nil)
;; (push '("*helm mini*" :height 20) popwin:special-display-config)
;; (push '("*Ido Completions*" :height 20) popwin:special-display-config)
;; (push '(dired-mode :position top) popwin:special-display-config)


;; (defadvice ac-expand-string (after kill-rest-of-symbol-ac-complete activate)
;;   "removes rest of string after expansion. useful when you edit
;; the middle of existing string using expansion this gets rid of
;; the old tail."
;;   (when (and (symbol-at-point)
;;              (not (equal (save-excursion (end-of-thing 'symbol))
;;                          (point))))
;;     (save-excursion (kill-sexp))))


(add-to-list 'load-path "~/.elisp/direx-el/")
(eval-after-load "dired" '(use-package direx))

(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

(add-to-list 'load-path "~/.elisp/sauron/")
(autoload 'sauron "sauron" nil t)  

(setq next-screen-context-lines 1)

(defun delete-region-and-yank (beg end)
  (interactive "r")
  (delete-region beg end)
  (yank-pop)
  (yank 2))

(bind "C-M-y" delete-region-and-yank)


(defun system-move-file-to-trash (file)
  (shell-command (concat "/usr/bin/trash " (shell-quote-argument file))))

;; (add-hook 'ido-make-file-list-hook 'ido-sort-mtime)

;; (add-to-list 'load-path "~/.elisp/emacs-async/")

;; (eval-after-load "dired-aux"
;;   '(require 'dired-async))

(defun revert-all-buffers ()
  "Revert all non-modified buffers associated with a file.
This is to update existing buffers after a Git pull of their underlying files."
  (interactive)
  (save-current-buffer
    (mapc (lambda (b)
            (set-buffer b)
            (unless (or (null (buffer-file-name)) (buffer-modified-p))
              (revert-buffer t t)
              (message "Reverted %s\n" (buffer-file-name))))
          (buffer-list))))

;; Override the default command to support optinal network connection instead of
;; always just starting an inferior process.
(defadvice slime (around slime-auto-net-connect activate)
  (if slime-default-is-connect
      (save-window-excursion
        (slime-connect slime-lisp-host slime-port)
        (sleep-for 0 300))
    ad-do-it))

;; Automatically connect when disconnected.
(setq slime-auto-connect 'always)

;; Indicate that the default connection should be via the network, not an
;; inferior process.
(setq slime-default-is-connect t)

(setq dired-async-use-native-commands t)

(use-package multiple-cursors
  :load "~/.elisp/multiple-cursors.el/"
  :bind ("C-S-c C-S-c" . mc/edit-lines))

;; (global-set-key (kbd "C->") 'mc/mark-next-like-this)
;; (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;; (global-set-key (kbd "M-<") 'mc/mark-all-like-this)

;; (add-to-list 'load-path "~/.elisp/test-case-mode/")
;; (require 'test-case-mode)
;; (add-hook 'find-file-hook 'enable-test-case-mode-if-test)
;; (add-hook 'compilation-finish-functions
;;           'test-case-compilation-finish-run-all)

;; (use-package nrepl
;;   :load-path "~/.elisp/nrepl.el/"
;;   :commands (nrepl)
;;   :config (progn (setq nrepl-lein-command "lein2")
;;                  (add-hook 'nrepl-interaction-mode-hook
;;                            'nrepl-turn-on-eldoc-mode)))

(setq ido-auto-merge-delay-time 0.5)

(setq ac-use-fuzzy t)

(use-package hide-comnt
  :bind ("C-M-;" . hide/show-comments-toggle))

;; python mode, bind this to C-c C-d
;; python-end-of-block
;; C-c C-u is python-beginning-of-block


(use-package rotate-text
  :commands rotate-word-at-point
  :bind ("C-c /" . rotate-word-at-point))

(use-package dictionary
  :commands dictionary-search
  :load-path "~/.elisp/dictionary-el/"
  :bind ("C-c d" . dictionary-search))

(defun odds (a b)
  "convert ratio given on betting sites to a percentage chance of
winning. It's slightly off because it doesn't take into account
he bookie's cut."
  (interactive "nA: \nnB: ")
  (let ((percent (/ (float b) (+ a b))))
    (if (called-interactively-p 'interactive )
        (message (format "%f" percent))
      percent)))

(use-package io-mode
    :load-path "~/src/io/extras/SyntaxHighlighters/Emacs/"
    :commands io-mode
    :mode ("\\.io\\'" . io-mode))

;; remember C-M-e end-of-defun closes all parens

(defun buffer-occur (regex)
  (interactive "sRegex: ")
  (multi-occur (buffer-list (selected-frame)) regex))

(setq blink-matching-paren t
      blink-matching-delay 0.2)

;;; maybe speeds up scrolling (?)
;; (setq jit-lock-defer-time 0.05)

(defun jsj-ack-buffer-name-function (mode-name)
  (concat "*" (car compilation-arguments) "*"))

(setq ack-buffer-name-function 'jsj-ack-buffer-name-function)

;;; try to keep erc input at bottom
(eval-after-load "erc" '(load "~/.elisp/damd-erc.el"))

;; (setq clojure-swank-command
;;       (if (or (locate-file "lein" exec-path) (locate-file "lein.bat" exec-path))
;;           "lein2 ritz-in %s"
;;         "echo \"lein2 ritz-in %s\" | $SHELL -l"))



;;; TODO rename buffer after pprint then turn on clojure-mode and paredit-mode in buffer

(defadvice slime-pprint-eval-last-expression (after enable-clojure-and-paredit)
  (clojure-mode)
  (paredit-mode))

;; Use clojure-mode SLIME Description popup buffer
(add-hook 'slime-popup-buffer-mode-hook
          (lambda ()
            (when (string-match "<clojure>\\*$"
                                (buffer-name))
              (clojure-mode)
              (paredit-mode))))

;; (add-hook 'slime-popup-buffer-mode-hook
;;           (lambda ()
;;             (when (string-match "<clojure>\\*$"
;;                                 (buffer-name))
;;               (setq font-lock-defaults
;;                     '(clojure-font-lock-keywords ; keywords
;;                       nil nil
;;                       (("+-*/.<>=!?$%_&~^:@" . "w")) ; syntax alist
;;                       nil
;;                       (font-lock-mark-block-function . mark-defun)
;;                       (font-lock-syntactic-face-function
;;                        . lisp-font-lock-syntactic-face-function)))
;;               (font-lock-fontify-buffer))))

(use-package hi-lock
  :bind (("M-o l" . highlight-lines-matching-regexp)
         ("M-o r" . highlight-regexp)
         ("M-o w" . highlight-phrase)))

(use-package package
  :disabled t
  :commands (package-install package-list-packages)
  :config
  (progn (add-to-list 'package-archives 
                      '("marmalade" .
                        "http://marmalade-repo.org/packages/"))
         (package-initialize)))

(use-package eval-expr
  :bind ("M-:" . eval-expr)
  :config
  (progn
    (setq eval-expr-print-function 'pp
          eval-expr-print-level 20
          eval-expr-print-length 100)

    (defun eval-expr-minibuffer-setup ()
      (set-syntax-table emacs-lisp-mode-syntax-table)
      (paredit-mode))))

(use-package gnus
  :commands (gnus)
  :load-path "~/src/gnus/lisp/")

(load "jsj-org")

;;; benchmark check
(when window-system
  (let ((elapsed (float-time (time-subtract (current-time)
                                            emacs-start-time))))
    (message "Loading %s...done (%.3fs)" load-file-name elapsed))

  (add-hook 'after-init-hook
            `(lambda ()
               (let ((elapsed (float-time (time-subtract (current-time)
                                                         emacs-start-time))))
                 (message "Loading %s...done (%.3fs) [after-init]"
                          ,load-file-name elapsed)))
            t))

(defadvice split-window-below (after balance-after-split activate) (balance-windows))
(defadvice split-window-right (after balance-after-split activate) (balance-windows))

(add-to-list 'load-path "~/.elisp/enhanced-ruby-mode/")
(setq enh-ruby-program "ruby")
(autoload 'enh-ruby-mode "enh-ruby-mode" "Major mode for ruby files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))

(add-to-list 'load-path "~/.elisp/ampc")
(add-to-list 'exec-path "~/.elisp/ampc")
(autoload 'ampc "ampc" nil t)

(setq ampc-tagger-music-directories '("/media/blobs/music/"))
(add-hook 'ampc-tagger-stored-hook 'jsj-ampc-tagger-rename-artist-title)

(defun jsj-ampc-tagger-rename-artist-title (_changed-tags data)
  "Rename music file according to its tags.
This function is meant to be inserted into
`ampc-tagger-stored-hook'.  The new file name is
artist/album/track title.extension."
  (let* ((artist (or (cdr (assq 'Artist (cdr data))) "Unknown"))
         (album (or (cdr (assq 'Album (cdr data))) "None"))
         (title (or (cdr (assq 'Title (cdr data))) ""))
         (track (or (format "%02d " (string-to-int (cdr (assq 'Track (cdr data))))) ""))
         (new-file (concat
                    (first ampc-tagger-music-directories)
                    artist "/" album "/" track title
                    (file-name-extension (car data) t))))
    (when (equal new-file "")
      (setf new-file (file-name-nondirectory (car data))))
    (unless (equal (car data) (expand-file-name
                               new-file (file-name-directory (car data))))
      (cl-loop for file = new-file then
               (replace-regexp-in-string "\\(\\(?:\\..+\\)?\\)$"
                                         (format "%d_\\1" index)
                                         new-file)
               for index from 1
               for expanded = (expand-file-name file (file-name-directory
                                                      (car data)))
               while (file-exists-p expanded)
               finally do (setf new-file expanded))
      (ampc-tagger-log "Renaming file " (abbreviate-file-name (car data))
                       " to " (abbreviate-file-name new-file) "\n")
      (mkdir (file-name-directory new-file))
      (rename-file (car data) new-file)
      (setf (car data) new-file))))

;; (add-to-list 'load-path "~/.elisp/ac-nrepl/")
;; (require 'ac-nrepl)
;; (add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
;; (add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
;; (eval-after-load "auto-complete"
;;   '(add-to-list 'ac-modes 'nrepl-mode))

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(defun rotate-windows ()
  "Rotate your windows"
  (interactive)
  (cond ((not (> (count-windows)1))
         (message "You can't rotate a single window!"))
        (t
         (setq i 1)
         (setq numWindows (count-windows))
         (while  (< i numWindows)
           (let* (
                  (w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i numWindows) 1)))

                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))

                  (s1 (window-start w1))
                  (s2 (window-start w2))
                  )
             (set-window-buffer w1  b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i)))))))

(defun build-ctags ()
  (interactive)
  (message "building project tags")
  (let ((root (eproject-root)))
    (shell-command (concat "ctags -e -R --extra=+fq --exclude=db --exclude=test --exclude=.git --exclude=public -f " root "TAGS " root)))
  (visit-project-tags)
  (message "tags built successfully"))

;; (defun my-find-tag ()
;;   (interactive)
;;   (if (file-exists-p (concat (eproject-root) "TAGS"))
;;       (visit-project-tags)
;;     (build-ctags))
;;   (etags-select-find-tag-at-point))

;; (global-set-key (kbd "M-.") 'my-find-tag)

;; (define-key (current-global-map) [remap find-tag] 'my-find-tag)

(defun twitch (channel)
  (interactive "sChannel: ")
  (erc :server (concat channel ".jtvirc.com")
       :nick twitch-username
       :password twitch-password)
  (erc-join-channel (concat "#" channel)))

(modify-syntax-entry ?_ "w")

(setq dired-guess-shell-alist-user
      '(("\\.mpe?g\\'\\|\\.avi\\'\\|\\.mp4\\'" "mplayer")
        ("\\.pdf\\'" "zathura")))

(add-to-list 'load-path "~/.elisp/gnuplot-mode/")
(require 'gnuplot)

(use-package deft
  :commands (deft)
  :load-path "~/.elisp/deft/")

(use-package git-gutter-fringe
  :load-path ("~/.elisp/emacs-git-gutter-fringe"
              "~/.elisp/emacs-git-gutter")
  :init (progn
          (global-git-gutter-mode t)

          (fringe-helper-define 'git-gutter-fr:added nil
            "........"
            "...XX..."
            "...XX..."
            ".XXXXXX."
            ".XXXXXX."
            "...XX..."
            "...XX..."
            "........")

          (fringe-helper-define 'git-gutter-fr:deleted nil
            "........"
            "........"
            "........"
            ".XXXXXX."
            ".XXXXXX."
            "........"
            "........"
            "........")

          (fringe-helper-define 'git-gutter-fr:modified nil
            "........"
            "..XXXX.."
            "..XXXX.."
            "..XXXX.."
            "..XXXX.."
            "..XXXX.."
            "..XXXX.."
            "........")))


(global-set-key (kbd "C-c E")
                (lambda ()
                  "Bring up a full-screen eshell or restore previous config."
                  (interactive)
                  (if (string= "eshell-mode" major-mode)
                      (jump-to-register :eshell-fullscreen)
                    (progn
                      (window-configuration-to-register :eshell-fullscreen)
                      (eshell)
                      (delete-other-windows)))))


(add-to-list 'load-path "~/.elisp/scala-mode2/")
(require 'scala-mode2)
(add-to-list 'load-path "~/.elisp/ensime/dist/elisp")
(require 'ensime)
(add-to-list 'exec-path '"~/src/scala-2.10.0/bin")

;; C-h C-f : find-function
;; C-h C-k : find-function-on-key
;; C-h C-v : find-variable
;; C-h C-l : find-library

(require 'org-protocol)

