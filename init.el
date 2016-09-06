;;; init.el --- Emacs initialization file
;;
;;
;;; Commentary:
;;  This is the Emacs initialization file.  Emacs reads it when
;;  starting up.  It takes care for loading the users' preferences.
;;
;; [ header

;; Info  : Emacs initialization file
;; Author: Matthias
;; Date  : 20.06.2014

;; ]

;;; Code:

;; [ personal information

(setq user-full-name "Matthias"
      user-mail-address "mpfeifer77@gmail.com")

;; ]

;; [ custom set variables

(setq custom-file "~/.emacs.d/custom.el")

(load custom-file)

;; do some initialization stuff when init.el was moved onto a new host

(let* ((emacs-dir (expand-file-name "~/.emacs.d/"))
       (autosave-dir (concat emacs-dir "auto-save/"))
       (boot nil))
  (progn
    (when (not (file-exists-p autosave-dir))
      (progn
        (setq boot t)
        (message (concat "[emacs boot] Creating directory " dir))
        (make-directory dir))))
  (when boot
    (let ((package-archives '(("melpa" . "http://melpa.org/packages/")))
          (package-enable-at-startup nil)
          (package-user-dir "~/.emacs.d/packages/"))
      (package-refresh-contents))))

;; ]


;; [ packaging

(require 'package)

(setq package-archives '(("melpa" . "http://melpa.org/packages/"))
      package-enable-at-startup nil
      package-user-dir "~/.emacs.d/packages/")

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(setq use-package-verbose t
      use-package-always-ensure t
      load-prefer-newer t) 

(global-set-key (kbd "C-c 5") #'package-list-packages)

;; ]


;; [ compilation

(add-hook 'compilation-mode-hook '(lambda ()
                                    (interactive)
                                    (next-error-follow-minor-mode)
                                    (local-set-key (kbd "q") 'quit-window)))

(use-package auto-compile
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

;; ]

;; [ General Emacs Behaviour

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)) )

(setq-default indent-tabs-mode nil)

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(setq auto-insert-directory "~/.emacs.d/templates/"
      auto-insert-query nil)

(auto-insert-mode)

(put 'set-goal-column 'disabled nil)
(put 'downcase-region 'disabled nil)

(global-set-key [S-down-mouse-1] 'ignore)

(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'symbol-to-string 'symbol-name)
(defalias 'string-to-symbol 'intern)

(recentf-mode 1)

(add-to-list 'load-path "~/.emacs.d/lib/")

(setq scroll-step 1
      scroll-conservatively 10000
      auto-window-vscroll nil
      delete-exited-processes t)

;; (setq gc-cons-threshold 5000000) ;; 

;; Kill buffers when done (M-x #)
(add-hook 'server-done-hook (lambda nil (kill-buffer nil)))

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.")

(global-set-key (kbd "M-Z") #'zap-up-to-char)

(add-to-list 'auto-mode-alist '("\.dll\\'" . hexl-mode))
(add-to-list 'auto-mode-alist '("\.exe\\'" . hexl-mode))
(add-to-list 'auto-mode-alist '("\.so\\'" . hexl-mode))

;; TODO: advicing has changed! This should be around to evaluate mouse-set-point parameter and wether region is set at
;; all. 

;; (setq mp/unhighlight-regexp nil)

;; (defadvice mouse-set-point (after mouse-highlight-selection activate)
;;   (progn
;;     (unhighlight-regexp t)
;;     (setq mp/unhighlight-regexp t)
;;     (highlight-regexp 
;;      (regexp-quote 
;;       (buffer-substring-no-properties (region-beginning) (region-end))))))

;; (add-hook 'deactivate-mark-hook '(lambda () (when mp/unhighlight-regexp (unhighlight-regexp t))))

;; ]

;; [ the mark

;; use C-u C-SPC to pop mark positions

(setq set-mark-command-repeat-pop t)

;; TODO want some nice stuff here that hooks into
;; activate-mark-hook and deactivate-mark-hook

;; ]

;; [ server mode

(setq server-use-tcp nil
      server-auth-dir "~/.emacs.d/"
      server-port 39246)

(server-start)

;; ]

;; [ isearch

;; 'C-w'     - Select the (rest of the) word the TextCursor is on as
;;             the search string; 
;; 'M-s C-e' - Select the text up to the end of the line as the search
;;             string (this was bound to ‘C-y’ up until Emacs 24.1). 
;; 'M-s h r' - Highlight regular expression (‘highlight-regexp’)
;; 'M-s h u' - Unhighlight regular expression
;; 'M-s o'   - call ‘occur’ with the current search term
;; 'C-M-s'   - isearch-forward-regexp

(defadvice isearch-forward-regexp (before kill-ring-save-before-search activate)
  "Save region (if active) to \"kill-ring\" before starting isearch.
This way region can be inserted into isearch easily with yank command."
  (when (region-active-p)
    (kill-ring-save (region-beginning) (region-end))))

(defadvice isearch-forward (before kill-ring-save-before-search activate)
  "Save region (if active) to \"kill-ring\" before starting isearch.
This way region can be inserted into isearch easily with yank command."
  (when (region-active-p)
    (kill-ring-save (region-beginning) (region-end))))

;; shamelessly stolen from http://goo.gl/JFRl1k
(defadvice isearch-update (before my-isearch-update activate)
  (sit-for 0)
  (if (and
       ;; not the scrolling command
       (not (eq this-command 'isearch-other-control-char))
       ;; not the empty string
       (> (length isearch-string) 0)
       ;; not the first key (to lazy highlight all matches w/o recenter)
       (> (length isearch-cmds) 2)
       ;; the point in within the given window boundaries
       (let ((line (count-screen-lines (point) (window-start))))
         (or (> line (* (/ (window-height) 4) 3))
             (< line (* (/ (window-height) 9) 1)))))
      (let ((recenter-position 0.3))
        (recenter '(4)))))


;; ]

;; [ s

(use-package s)

;; ]

;; [ encoding systems

;; (modify-coding-system-alist 'file ".*cygwin.*\.sh" 'utf-8-unix)
;; (modify-coding-system-alist 'file ".*cygwin.*\.py[23]?" 'utf-8-unix)

;; ]

;; [ global appearence

(when window-system
  (when (eq system-type 'windows-nt)
    (horizontal-scroll-bar-mode -1))
  (tool-bar-mode -1)
  (menu-bar-mode 1)
  (tooltip-mode -1)
  (scroll-bar-mode -1))

;; do not show startup screen
(setq inhibit-startup-screen t)

(use-package material-theme
  :config
  (load-theme 'material-light) )

(use-package volatile-highlights 
  :init
  (add-hook 'emacs-lisp-mode 'volatile-highlights-mode)
  (add-hook 'js2-mode-hook 'volatile-highlights-mode)
  (add-hook 'cperl-mode-hook 'volatile-highlights-mode)
  (add-hook 'c-mode-hook 'volatile-highlights-mode)
  (add-hook 'web-mode-hook 'volatile-highlights-mode)
  (add-hook 'css-mode-hook 'volatile-highlights-mode)
  (add-hook 'html-mode-hook 'volatile-highlights-mode)
  (add-hook 'java-mode-hook 'volatile-highlights-mode)
  (add-hook 'nxml-mode-hook 'volatile-highlights-mode)
  (add-hook 'python-mode-hook 'volatile-highlights-mode)
  (add-hook 'php-mode-hook 'volatile-highlights-mode)
  (add-hook 'shell-mode-hook 'volatile-highlights-mode) 
  )


(show-paren-mode 1)

;; [ backups

(setq backup-directory-alist '(("." . "~/.emacs.d/backups"))
      delete-old-versions t
      version-control t
      vc-make-backup-files t
      auto-save-interval 50
      backup-by-copying t
      kept-new-versions 10
      delete-old-versions t
      vc-make-backup-files t
      auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save/" t)))

;; ]

;; [ imenu

(setq imenu-auto-rescan t
      imenu-use-popup-menu nil
      imenu-space-replacement "-"
      imenu-sort-function 'imenu--sort-by-name) ;; sort only mouse menu

(defadvice imenu-recenter-advice (after mp/imenu-center activate)
  (recenter-top-bottom 2))


;; ]

;; [ ibuffer

(defun ibuffer-previous-line ()
  "Move point to last buffer when going before first buffer."
  (interactive)
  (previous-line)
  (if (<= (line-number-at-pos) 2)
      (goto-line (- (count-lines (point-min) (point-max)) 2))))

(defun ibuffer-next-line ()
  "Wrap point to first buffer when going after last buffer."
  (interactive)
  (next-line)
  (if (>= (line-number-at-pos) 
          (- (count-lines (point-min) (point-max)) 1))
      (goto-line 3)))

(use-package ibuffer-git)

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :config
  (setq ibuffer-show-empty-filter-groups nil
        ibuffer-formats '((mark modified read-only " "
                                (name 18 18 :left :elide)
                                " "
                                (size 9 -1 :right)
                                " "
                                (git-status-mini) ;;  8 8 :left)
                                " " filename-and-process)
                          (mark " "
                                (name 16 -1)
                                " " filename)))

  ;; use M-n, M-p to navigate between groups
  (setq ibuffer-saved-filter-groups
        (quote (("modes+projects"
                 ("Arithmetik Chef" (filename . "^/home/matthias/public_html/Arithmetik-Chef/.*"))
                 ("Emacs" (or
                           (name . "^\\*scratch\\*$")
                           (name . "^\\*Messages\\*$")
                           (filename . "^.*/.emacs.d/.*$")))
                 ("Emacs Lisp" (mode . emacs-lisp-mode))
                 ("Gnus" (or
                          (mode . message-mode)
                          (mode . bbdb-mode)
                          (mode . mail-mode)
                          (mode . gnus-group-mode)
                          (mode . gnus-summary-mode)
                          (mode . gnus-article-mode)
                          (name . "^\\.bbdb$")
                          (name . "^\\.newsrc-dribble")))
                 ("Graph Designer.js" (filename . "^/home/matthias/public_html/graphjs/.*"))
                 ("JDEE" (or
                          (name . "^\\*JDEE.*")
                          (name . "^\\*check style\\*")))
                 ("Customization" (name . "^\\*Customize.*"))
                 ("Nevelex Demo" (filename . "^/home/matthias/java/projects/nevelex/.*"))
                 ("Mailguard" (filename . "^.*nightly_build/.*"))
                 ("Organizer" (mode . org-mode))
                 ("OpenGL-Lab" (filename . "^/home/matthias/opengl/lab/.*"))
                 ("OpenGL-Maze" (filename . "^/home/matthias/opengl/openmaze/.*"))
                 ("Pocketmine" (filename . "^.*Minecraft/Pocketmine/git/.*"))
                 ("Snake.js" (filename . "^/home/matthias/public_html/snake/.*"))
                 ("Timelapse" (filename . "^/home/matthias/timelapse/.*"))
                 ("Dired" (mode . dired-mode)))
                 ("Perl" (mode . cperl-mode))
                 ("Python" (mode . python-mode)))))

  (add-hook 'ibuffer-mode-hook
            '(lambda ()
               (ibuffer-auto-mode 1)
               (define-key ibuffer-mode-map (kbd "C-p") 'ibuffer-previous-line)
               (define-key ibuffer-mode-map (kbd "C-n") 'ibuffer-next-line)
               (ibuffer-switch-to-saved-filter-groups "modes+projects"))))
;; ]

;; [ emacs lisp mode

;; ignore byte-compile warnings

(setq byte-compile-warnings '(not nresolved
                                  free-vars
                                  callargs
                                  redefine
                                  noruntime
                                  cl-functions
                                  interactive-only))

(defun mp/dotemacs-mode-hook ()
  (local-set-key (kbd "C-S-n") 'forward-paragraph)
  (local-set-key (kbd "C-S-p") 'backward-paragraph)
  (local-set-key (kbd "C-#") 'imenu)
  (auto-fill-mode 1)
  (setq-local comment-auto-fill-only-comments t)
  (setq-local comment-multi-line t)
  (make-local-variable 'paragraph-start)
  (make-local-variable 'paragraph-separate)
  (setq paragraph-start "^;; \\["
        paragraph-separate "^;; ]$")
  (setq-local imenu-create-index-function 'imenu-default-create-index-function)
  (setq imenu-generic-expression '((nil "^;; \\[ \\(.*\\)" 1))) )

(defun byte-compile-current-buffer ()
  (interactive)
  (byte-compile-file (buffer-file-name)))

(defun mp/emacs-lisp-mode-hook ()
  (when (string= (buffer-name) "init.el")
    (mp/dotemacs-mode-hook))
  (eldoc-mode 1)
  (local-set-key (kbd "C-/") 'comment-dwim)
  (local-set-key (kbd "C-c C-c") 'byte-compile-current-buffer)
  (electric-pair-mode)
  (setq fill-column 120) )

(add-hook 'emacs-lisp-mode-hook 'mp/emacs-lisp-mode-hook)

(use-package elisp-slime-nav
  :config
  (add-hook 'emacs-lisp-mode-hook 'turn-on-elisp-slime-nav-mode))

;; ]

;; [ save history

(setq savehist-file "~/.emacs.d/savehist"
      history-length t
      history-delete-duplicates t
      savehist-save-minibuffer-history 1
      savehist-additional-variables '(kill-ring
                                      search-ring
                                      regexp-search-ring))

(savehist-mode 1)

;; ]

;; [ expand region

;; Very handy package. Sets er/try-expand-list on a per mode basis to
;; a list of defuns. Each defun marks part of the
;; buffer. Incrementally largens the part of the buffer a defun
;; operats on. The next larger marked part is then set to the region.
;; To customize add defun to er/try-expand-list in any mode hook.
;;
;;

(use-package expand-region
  :bind
  ("C-v" . er/expand-region)
  ("C-S-v" . er/contract-region) )

;; ]

;; [ yasnippet

(use-package yasnippet
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets/"))
  ;; (dolist (dir yas-snippet-dirs)
  ;;   (when (directory-name-p dir)
  ;;      (add-to-list 'auto-mode-alist (list dir 'snippet-mode) ) ) )
  (yas-global-mode 1) )

;; ]

;; [ auto complete
;;
;; TODO - want keybindings in auto-complete for search and scroll
;; TODO - want to understand how documentation in auto-complete works
;; TODO - want per mode and per file dictionary files
;; TODO - want to understand auto-complete-config and how to extend/customize it

;; see https://github.com/xcwen/ac-php
(use-package ac-php)

(use-package auto-complete
  :config
  ;; this gives default configuration for a couple of modes
  ;; see end of file auto-complete-config.el
  ;; for more custom configuration see http://auto-complete.org/doc/manual.html
  (require 'auto-complete-config) 
  (ac-config-default)
  (define-key ac-mode-map (kbd "C-c C-<SPC>") 'auto-complete) 
  (setq ac-use-menu-map t)
  ;; Default settings
  (require 'ac-php)
  (define-key ac-menu-map "\C-n" 'ac-next)
  (define-key ac-menu-map "\C-p" 'ac-previous)
  (define-key ac-menu-map "\C-s" 'ac-isearch)
  (add-to-list 'ac-modes 'php-mode) 
  (add-to-list 'ac-modes 'web-mode) )

;; ]

;; [ avy-mode

(use-package avy
  :bind ("C-S-j" . avy-goto-word-or-subword-1) )

;; ]

;; [ javascript

(defun mp/js2-mode-hook ()
  (setq indent-tabs-mode nil
        js-indent-level 4)
  ;;  (idle-highlight-mode 1)
  ;; writing longer comments
  (setq-local comment-auto-fill-only-comments t)
  (auto-fill-mode 1)
  (setq-local comment-multi-line t)
  (local-set-key (kbd "RET") 'c-indent-new-comment-line) )

(define-auto-insert '("\\.js\\'" . "Javscript Skeleton")
  [ '(nil
      "/*\n * "
      (file-name-nondirectory (buffer-file-name)) "\n"
      " * Started on " (format-time-string "%A, %e %B %Y.") \n
      " */" \n \n \n ) indent-buffer ] )

(use-package js2-mode
  :mode "\\.js\\'"
  :config
  (add-hook 'js2-mode-hook 'mp/js2-mode-hook))

;; ]

;; [ info browser

;; To read plain info file from the filesystem: "C-u C-h i"

;; TODO: There is a python info page lying around in this directcory,
;; but info does not find it


(add-to-list 'Info-default-directory-list "~/.emacs.d/info")

(defun mp/Info-mode-hook ()
  "Personal info mode hook."
  (define-key Info-mode-map (kbd "C-s") 'isearch-forward-regexp) )

(add-hook 'Info-mode-hook 'mp/Info-mode-hook)

;; ]

;; [ calendar

;; TODO: Want diary view entries be called after moving date marker in calendar
;; TODO: Want tab to jump from one entry to the next (shift-tab to jump back)

(global-set-key (kbd "<f4>") #'(lambda () (interactive)
                                 "Toggle calendar visibility"
                                 (let ((calendar-window
                                        (get-buffer-window "*Calendar*")))
                                   (if calendar-window
                                       (delete-window calendar-window)
                                     (calendar) ) ) ) )

(defun mp/calendar-mode-hook ()
  (interactive)
  (local-set-key (kbd "<RET>") #'diary-view-entries) )

(add-hook 'diary-display-hook 'fancy-diary-display)
(add-hook 'today-visible-calendar-hook 'calendar-mark-today)
(add-hook 'calendar-mode-hook 'mp/calendar-mode-hook)

(setq calendar-longitude 6.116951
      calendar-latitude 50.840401
      calendar-mark-holidays-flag t
      calendar-date-style 'european
      calendar-view-diary-initially-flag t
      calendar-mark-diary-entries-flag t
      number-of-diary-entries 7
      calendar-day-name-array ["Sonntag" "Montag" "Dienstag" "Mittwoch" "Donnerstag" "Freitag" "Samstag"]
      calendar-month-name-array ["Januar" "Februar" "März" "April" "Mai" "Juni" "Juli" "August" "September" "Oktober" "November" "Dezember"]
      solar-n-hemi-seasons '("Frühlingsanfang" "Sommeranfang" "Herbstanfang" "Winteranfang")
      holiday-general-holidays '((holiday-fixed 1 1 "Neujahr")
                                 (holiday-fixed 5 1 "1. Mai")
                                 (holiday-fixed 10 3 "Tag der Deutschen Einheit"))
      holiday-christian-holidays '((holiday-float 12 0 -4 "1. Advent")
                                   (holiday-float 12 0 -3 "2. Advent")
                                   (holiday-float 12 0 -2 "3. Advent")
                                   (holiday-float 12 0 -1 "4. Advent")
                                   (holiday-fixed 12 25 "1. Weihnachtstag")
                                   (holiday-fixed 12 26 "2. Weihnachtstag")
                                   (holiday-fixed 1 6 "Heilige Drei Könige")
                                   (holiday-fixed 8 15 "Mariä Himmelfahrt")
                                   (holiday-fixed 11 1 "Allerheiligen")
                                   (holiday-float 11 3 1 "Buß- und Bettag" 16)
                                   (holiday-float 11 0 1 "Totensonntag" 20)
                                   (holiday-easter-etc -46 "Aschermittwoch")
                                   (holiday-easter-etc -2 "Karfreitag")
                                   (holiday-easter-etc 0 "Ostern")
                                   (holiday-easter-etc 1 "Ostermontag")
                                   (holiday-fixed 12 24 "Heiligabend")
                                   (holiday-fixed 12 25 "1. Weihnachtstag")
                                   (holiday-fixed 12 26 "2. Weihnachtstag"))
      calendar-holidays (append holiday-general-holidays
                                holiday-local-holidays
                                holiday-other-holidays
                                holiday-christian-holidays
                                holiday-solar-holidays))

;; ]

;; [ session management

;; save and restore open buffers
(desktop-save-mode)

;; [ org mode

(global-set-key (kbd "C-c c") #'org-capture)

(defun mp/org-mode-hook ()
  "org mode hook extender [mp]"

  (require 'ob-plantuml)
  (require 'ob-python)

  ;; babel

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((plantuml . t)
     (python . t)
     (C . t)
     (emacs-lisp t)
     (awk . t)
     (dot . t)
     (gnuplot . t)
     (java . t)
     (perl . t)
     (sh . t)))

  (auto-fill-mode)

  (local-set-key (kbd "<return>") 'org-return-indent)

  (setq org-plantuml-jar-path "~/.emacs.d/plantUML/plantuml.jar"
        org-ellipsis "…"
        org-directory "~/org"
        org-default-notes-file "~/org/gtd.org"
        org-confirm-babel-evaluate nil)

  (setq org-capture-templates
        (quote (("t" "todo" entry (file "~/org/gtd.org")
                 "** TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
                ("r" "respond" entry (file "~/org/gtd.org")
                 "** NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
                ("n" "note" entry (file "~/org/gtd.org")
                 "** %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
                ("j" "Journal" entry (file+datetree "~/org/diary.org")
                 "** %?\n%U\n" :clock-in t :clock-resume t)
                ("w" "org-protocol" entry (file "~/org/gtd.org")
                 "** TODO Review %c\n%U\n" :immediate-finish t)
                ("m" "Meeting" entry (file "~/org/gtd.org")
                 "** MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
                ("p" "Phone call" entry (file "~/org/gtd.org")
                 "** PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
                ("h" "Habit" entry (file "~/org/gtd.org")
                 "** NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))))

  (setenv "GRAPHVIZ_DOT" "dot") )

(add-hook 'org-mode-hook 'mp/org-mode-hook)

;; ]

;; [ prodigy service manager

;; Warning: prodigy does not seem to allow stopping services on Windows

(defsubst mp/toggle-prodigy-buffer ()
  (interactive)
  (if (string= (buffer-name) "*prodigy*")
      (quit-window)
    (prodigy)))

(use-package prodigy
  :config

  (defvar mp:prodigy-service-root
    "~/.emacs.d/services/"
    "Root directory for various services bundled with init.el." )

  (defvar mp:prodigy-python-interpreter
    "/usr/bin/python3"
    "Location of python interpreter used by prodigy.  Default just grabs one from PATH.")

  (global-set-key (kbd "<f5>") #'mp/toggle-prodigy-buffer)

  (prodigy-define-service
    :name "Date Server (python)"
    :command mp:prodigy-python-interpreter
    :args '("date.py" "14002")
    :stop-signal 'int
    :cwd (concat mp:prodigy-service-root "date/"))

  (prodigy-define-service
    :name "Network Log-Receiver"
    :command "/usr/bin/python2"
    :args '("logwebmon.py")
    :cwd (concat mp:prodigy-service-root "loghost/"))

  (prodigy-define-service
    :name "Echo Server (python)"
    :command mp:prodigy-python-interpreter
    :args '("echo.py" "14001")
    :stop-signal 'int
    :cwd (concat mp:prodigy-service-root "echo/") ) )

;; ]

;; [ speedbar

(use-package sr-speedbar
  :bind ("<f6>" . sr-speedbar-toggle)
  :config
  (define-key speedbar-file-key-map (kbd "C-j") 'speedbar-expand-line)
  (define-key speedbar-file-key-map (kbd "C-k") 'speedbar-contract-line)
  (defadvice sr-speedbar-toggle (after select-speedbar activate)
    (let ((window (get-buffer-window "*SPEEDBAR*")))
      (when window (select-window window)))))

;; ]

;; [ tags

(setq tags-revert-without-query t)

(defadvice xref-find-definitions (before push-mark-before-find activate)
  (push-mark))

;; ]

;; [ perl mode

(fset 'perl-mode 'cperl-mode)

(add-hook 'cperl-mode-hook '(lambda ()
                              (interactive)
                              (local-set-key (kbd "C-h f") 'cperl-perldoc-at-point)))

;; ]

;; [ hungry delete mode
;;
;; This mode makes backspace erase all consecutive whitespace (instead of just a single one).

(use-package hungry-delete
  :config
  (require 'hungry-delete)
  (global-hungry-delete-mode))

;; ]

;; [ eshell

(defun mp/eshell-mode-hook ()
  "Personal eshell mode hook."
  (interactive)
  (auto-complete-mode t)
  (setq ac-sources '(ac-source-filename ac-source-files-in-current-dir))
  (local-set-key (kbd "C-c C-c") 'mp/eshell)
  )

(add-hook 'eshell-mode-hook 'mp/eshell-mode-hook)

(defconst eshell-window-height -15 "Height of eshell window.")
(defconst eshell-buffer-name "*eshell*")

(defun mp/eshell ()
  "Start eshell open buffer with smart logic."
  (interactive)
  (let ((numWindows (length (window-list))))
    (if (string= (buffer-name) eshell-buffer-name)
        ;; Current buffer is eshell buffer. Lets close it.
        (if (eq 1 numWindows)
            (delete-frame)
          (delete-window))
      (if (eq 1 numWindows)
          ;; Current buffer is not an eshell buffer. Lets start one.
          (let ((newwindow (split-window nil eshell-window-height)))
            (select-window newwindow)
            (eshell)
            )
        (progn
          (select-frame (make-frame))
          (eshell))))))

(global-set-key (kbd "<f7>") #'mp/eshell)

(defun eshell-emergency-exit ()
  "When eshell refuses to close with \"Text is read-only.\" message exit eshell with this function instead."
  (interactive)
  (let ((inhibit-read-only t)) (kill-this-buffer)))

;; ]

;; [ C/C++

(defun mp/c-mode-hook ()
  "Personal c mode hook extender."
  (local-set-key (kbd "C-c C-c") 'compile))


(add-hook 'c-mode-hook 'mp/c-mode-hook)

;; ]

;; [ web mode

(defun mp/web-mode-extension ()
  (setq indent-tabs-mode nil)
  (setq web-mode-markup-indent-offset 4))


(defun toggle-php-flavor-mode ()
  (interactive)
  "Toggle mode between PHP & Web-Mode Helper modes"
  (cond ((string= mode-name "PHP/l")
         (web-mode))
        ((string= mode-name "Web")
         (php-mode))))

(use-package web-mode
  :bind ("C-c 6" . toggle-php-flavor-mode)
  :config
  (add-hook 'web-mode-hook 'mp/web-mode-extension)  )

;; ]

;; [ web development

(defgroup mp nil "All things related to my customization" :group 'Emacs)

(defgroup development nil "All things related to development" :group 'mp)

(defgroup web nil "All things related to web development" :group 'development)

(defcustom web-project-root "~/public_html/" "New web projects are stored in this directory." :group 'web)

(defun mp/html-project-post-processing (name)
  "This method looks for strings %CSSFILE% and %TITLE% and replaces them with some meaningful values ."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "%TITLE%" nil t)
      (replace-match name))
    (goto-char (point-min))
    (when (re-search-forward "%CSSFILE%" nil t)
      (replace-match (replace-regexp-in-string (regexp-quote ".html") ".css" (buffer-name) 'fixedcase 'literal) 'fixedcase))))

(defun mp/start-web-project (name)
  "Create a new web project with NAME.  Create initial html, js, css file."
  (interactive "MProjectname? ")
  (let ((projectroot (concat web-project-root name)))
    (unless (file-exists-p projectroot)
      (mkdir projectroot))
    (select-frame (make-frame))
    (split-window-vertically)
    (find-file (concat projectroot "/" name ".html"))
    (save-buffer)
    (other-window 1)
    (find-file (concat projectroot "/" name ".js"))
    (save-buffer)
    (split-window-horizontally)
    (find-file (concat projectroot "/" name ".css"))
    (save-buffer)
    (other-window -1)
    (copy-file "~/.emacs.d/templates/jquery-3.0.0.js" (concat projectroot "/"))
    (switch-to-buffer (concat name ".html"))
    (mp/html-project-post-processing name)))

(global-set-key (kbd "C-c 4") #'mp/start-web-project)

(defun mp/css-mode-hook ()
  "Personal css mode hook extender."
  (setq ac-sources '(ac-source-css-property)))


(add-hook 'css-mode-hook' mp/css-mode-hook)

;; ]

;; [ frame+window handling

(winner-mode)

(defun mp/detach-window ()
  "Iff current frame hosts at least two windows, close current window
and display corresponding buffer in new frame."
  (interactive)
  (if (not (one-window-p))
      (let ((buffer (current-buffer)))
        (delete-window)
        (display-buffer-pop-up-frame buffer nil))
    (message "Refusing to detach window when one-window-p is true.")))

(defun split-window-below-select ()
  "Just like split-window-below, but select the newly created window."
  (interactive)
  (split-window-below)
  (other-window 1) )

(defun split-window-right-select ()
  "Just like split-window-right, but select the newly created window."
  (interactive)
  (split-window-right)
  (other-window 1) )

(defun swap-windows ()
  "Exchange buffer content between two windows. Current frame must host exactly two windows."
  (interactive)
  (if (eq 2 (length (window-list)))
      (let* ((win-1 (nth 0 (window-list)))
             (win-2 (nth 1 (window-list)))
             (buf-1 (window-buffer win-1))
             (buf-2 (window-buffer win-2)))
        (set-window-buffer win-1 buf-2)
        (set-window-buffer win-2 buf-1) ) ) 
  (message "This function only works when the current frame holds two windows."))

(defun rotate-windows ()
  "Change between horizontal and vertical layout. Current frame must host exactly two windows."
  (interactive)
  (if (eq 2 (length (window-list)))  
      (let* ((win-1 (nth 0 (window-list)))
             (win-2 (nth 1 (window-list)))
             (left-1 (nth 0 (window-edges win-1)))
             (left-2 (nth 1 (window-edges win-2)))
             (buf-1 (window-buffer win-1))
             (buf-2 (window-buffer win-2)))
        (delete-window win-2)
        (if (eq left-1 left-2)
            (split-window-right)
          (split-window-below) ) 
        (switch-to-buffer buf-2)
        (other-window 1)
        (switch-to-buffer buf-1) )
    (message "This function only works when the current frame holds two windows.") ) )

(global-set-key (kbd "<f1>") #'mp/detach-window)
(global-set-key (kbd "<f2>") #'make-frame)
(global-set-key (kbd "<f3>") #'delete-frame)
(global-set-key (kbd "C-x 2") #'split-window-below-select)
(global-set-key (kbd "C-x 3") #'split-window-right-select)
(global-set-key (kbd "C-<f8>") #'swap-windows)
(global-set-key (kbd "<f8>") #'rotate-windows)

;; ]

;; [ ediff

(add-hook 'ediff-after-quit-hook-internal 'winner-undo)

(defun mp/ediff-this ()
  "If current frame hosts exactly two windows. ediff these two buffers."
  (interactive)
  (if (eq 2 (length (window-list)))
      (let* ((win-1 (nth 0 (window-list)))
             (win-2 (nth 1 (window-list)))
             (buf-1 (window-buffer win-1))
             (buf-2 (window-buffer win-2)))
        (ediff-buffers buf-1 buf-2))))

;; ]

;; [ whitespace mode

(global-set-key (kbd "C-c 2") #'whitespace-mode)

;; ]

;; [ Man mode

(defsubst scroll-up-one-line ()
  (interactive)
  (scroll-up 1))

(defsubst scroll-down-one-line ()
  (interactive)
  (scroll-down 1) )

(defun mp/man-mode-hook ()
  (define-key Man-mode-map (kbd "C-n") 'scroll-up-one-line)
  (define-key Man-mode-map (kbd "C-p") 'scroll-down-one-line) )

(add-hook 'Man-mode-hook 'mp/man-mode-hook)

;; ]

;; [ java mode


(use-package jdee
  :disabled
  :config
  (setq jdee-server-dir "~/.emacs.d/jdee-server") )

(defun mp:predict-package-name-for-current-buffer ()
  "Simply take two parent directories and concat with . inbetween."
  (let* ((components (remq ""
                           (reverse
                            (split-string
                             (file-name-directory (buffer-file-name))
                             "\\/")))))
    (concat (nth 0 components) "." (nth 1 components) "." (nth 2 components))))

(defun mp:java-preprocessor()
  (let ((classname (file-name-sans-extension (buffer-name)))
        (packagename (mp:predict-package-name-for-current-buffer)))
    (while (search-forward "CLASSNAME" nil t)
      (replace-match classname t))
    (goto-char (point-min))
    (while (search-forward "PACKAGE" nil t)
      (replace-match packagename) ) ) )

(defun mp:java-mode-hook()
  (setq-local comment-auto-fill-only-comments t)
  (auto-fill-mode 1)
  (setq-local comment-multi-line t) )

(add-hook 'java-mode-hook 'mp:java-mode-hook)

(add-to-list 'auto-insert-alist '(".*\\.java$" . [ "template.java" mp:java-preprocessor] ) ) 

;; ]

;; [ dired

(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map (kbd "<backspace>")
              (lambda () (interactive) (find-alternate-file "..")))))

(put 'dired-find-alternate-file 'disabled nil)

;; ]

;; [ xml mode

;; handy: (nxml-balanced-close-start-tag-inline)

(require 'hideshow)
(require 'sgml-mode)
(require 'nxml-mode)

(add-to-list 'hs-special-modes-alist
             '(nxml-mode
               "<!--\\|<[^/>]*[^/]>"
               "-->\\|</[^/>]*[^/]>"
               "<!--"
               sgml-skip-tag-forward
               nil))

(add-hook 'nxml-mode-hook 'hs-minor-mode)

;; optional key bindings, easier than hs defaults
(define-key nxml-mode-map (kbd "C--") 'hs-toggle-hiding)

(defun mp/nxml-mode-setup ())


(add-hook 'nxml-mode-hook 'mp/nxml-mode-setup)

(add-to-list 'auto-insert-alist '("^pom.xml$" . [ "pom.xml" ]))

(setq xml-modes (list ".*\\.xul\\" ".*\\..rdf\\" ".*\\.xsd\\"))

(add-to-list 'auto-mode-alist '(".*\\.xul\\'" . xml-mode))
(add-to-list 'auto-mode-alist '(".*\\.rdf\\'" . xml-mode))
(add-to-list 'auto-mode-alist '(".*\\.xsd\\'" . xml-mode))

(defun mp/maven-integration ()
  (interactive)
  (when (string= "pom.xml" (buffer-name))
    (progn
      (setq compile-command "mvn clean install")
      (local-set-key (kbd "C-c C-c") 'compile))))

(defun mp/schema-validation-setup ()
  (add-to-list 'rng-schema-locating-files
               "~/.emacs.d/schemas/schemas.xml"))

(add-hook 'nxml-mode-hook 'mp/maven-integration)
(add-hook 'nxml-mode-hook 'mp/schema-validation-setup)

;; ]

;; [ ido & co

(setq ido-enable-flex-matching t
      ido-use-filename-at-point 'guess)

(ido-mode t)
(ido-everywhere)

(use-package smex
  ;; https://github.com/nonsequitur/smex
  :config
  (smex-initialize)
  (global-set-key (kbd "M-x") #'smex))

(use-package ido-vertical-mode
  :config
  (ido-vertical-mode))

(use-package ido-ubiquitous
  :config
  (ido-ubiquitous-mode)
  (setq ido-cr+-max-items 50000) )

;; [ tags

(setq tags-file-name nil
      tags-table-list nil)

;; ]

;; ;; [ Where was I [editing text]?

;; (defun mp/store-lot-position ()
;;   (when (not (string-prefix-p "*" (buffer-name)))
;;     (point-to-register ?z)))

;; (defun mp/goto-lot-position ()
;;   (interactive)
;;   (jump-to-register ?z))

;; (add-hook 'post-self-insert-hook 'mp/store-lot-position)

;; (global-set-key (kbd "C-c 1") 'mp/goto-lot-position)

;; ]

;; [ occur

;; Tip: In the buffer where occur was called use "M-g M-n" and
;;      "M-g M-p" to browse through the occurances
;; Be sure to use "q" to exit *occur* buffer. This way highlighted
;; regular expressions are unhighlighted.

(defun mp/occur-mark-regexp ()
  "If no regexp was marked until now do it now."
  (interactive)
  (when (not mp/occur-marked-regexp)
    (setq mp/occur-origin-buffer (current-buffer))
    (highlight-regexp mp/occur-last-regexp)))

(defun mp/occur-next-line ()
  (interactive)
  (next-line)
  (occur-mode-goto-occurrence)
  (recenter-top-bottom 2)
  (mp/occur-mark-regexp)
  (other-window 1))

(defun mp/occur-prev-line ()
  (interactive)
  (previous-line)
  (occur-mode-goto-occurrence)
  (recenter-top-bottom 2)
  (mp/occur-mark-regexp)
  (other-window 1))

(defun mp/occur-quit-window ()
  (interactive)
  (unhighlight-regexp mp/occur-last-regexp)
  (setq mp/occur-marked-regexp nil
        mp/occur-last-regexp nil
        mp/occur-origin-buffer nil)
  (quit-window) )

(defun mp/occur-goto-occurence ()
  (interactive)
  (occur-mode-goto-occurrence)
  (unhighlight-regexp mp/occur-last-regexp))

(global-set-key (kbd "C-o") #'occur)

(defvar mp/occur-marked-regexp nil)
(defvar mp/occur-last-regexp nil)
(defvar mp/occur-origin-buffer nil)

(defadvice occur (after select-occur-window-after-occur activate)
  "Do make *Occur* buffer current after calling occur."
  (let ((occur-window (get-buffer-window "*Occur*")))
    (when occur-window
      (select-window occur-window)))
  (setq mp/occur-last-regexp (car regexp-history)))

(defun mp/occur-mode-hook ()
  (local-set-key (kbd "C-n") 'mp/occur-next-line)
  (local-set-key (kbd "C-p") 'mp/occur-prev-line)
  (local-set-key (kbd "<return>") 'mp/occur-goto-occurence)
  (local-set-key (kbd "q") 'mp/occur-quit-window))

(add-hook 'occur-mode-hook 'mp/occur-mode-hook)

;; ]


(put 'narrow-to-region 'disabled nil)

;; [ python

(use-package elpy
  :init
  (elpy-enable)
)

(defun mp/python-mode-hook ()
  "Personal python mode hook extension."
  (auto-fill-mode 1)
  (setq-local comment-auto-fill-only-comments t)
  (setq-local comment-multi-line t)
  (local-set-key (kbd "M-;") 'comment-dwim))

(add-hook 'python-mode-hook 'mp/python-mode-hook)

(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.py2\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.py3\\'" . python-mode))

(add-to-list 'auto-insert-alist '(".*\\.py[23]?$" . [ "template.py" ] ) )




;; ]

;; [ restclient mode

(use-package restclient
  :config
  (add-to-list 'auto-mode-alist '("\\.rcm\\'" . restclient-mode)))
;; ]

;; [ ispell mode

;; make sure ispell is in your path or add it here

;; (add-to-list 'exec-path "C:/mp/aspell/bin/")


(setq ispell-program-name "aspell")
(setq ispell-personal-dictionary "~/.emacs.d/ispell.txt")

;; ]

;; [ html editing

(require 'hideshow)
(require 'sgml-mode)
(require 'nxml-mode)

(add-to-list 'hs-special-modes-alist
             '(html-mode
               "<!--\\|<[^/>]*[^/]>"
               "-->\\|</[^/>]*[^/]>"
               "<!--"
               sgml-skip-tag-forward
               nil))

(add-hook 'html-mode-hook 'hs-minor-mode)

;; optional key bindings, easier than hs defaults
(define-key html-mode-map (kbd "C--") 'hs-toggle-hiding)

(defun mp/html-mode-setup ()
  (interactive))

(define-auto-insert '("\\.html\\'" . "HTML5 Skeleton")
  [ '(nil
      "<!DOCTYPE html>\n"
      "<html>\n"
      "<head>\n"
      "<meta charset=\"UTF-8\">\n"
      "<title>%TITLE%</title>\n"
      "<script src=\"jquery-3.0.0.js\"></script>\n"
      "<script src=\"subrx.js\"></script>\n"
      "<link rel=\"stylesheet\" type=\"text.css\" href=\"%CSSFILE%\">\n"
      "</head>\n"
      "<body>\n"
      "</body>\n"
      "</html>\n"
      ) indent-buffer ] )

(add-hook 'html-mode-hook 'mp/html-mode-setup)

;; ]

;; [ php mode

(defun mp/php-mode-extension ()
  (interactive)
  (require 'ac-php)
  (setq indent-tabs-mode nil)
  (setq c-basic-offset 4)
  (setq php-template-compatibility nil) 
  (setq ac-sources '(ac-source-php) ) )

(use-package php-mode
  :config
  (add-hook 'php-mode-hook 'mp/php-mode-extension) )

;; ]

;; [ shell script mode

(defun mp:elisp-post-processor ()
  (interactive)
  (let ((match-found t))
    (progn
      (goto-char (point-min))
      (when (re-search-forward "_" nil t)
        (replace-match "") ) ) ) )

(add-to-list 'auto-insert-alist '(".*\\.sh$" . [ "template.sh" mp:elisp-post-processor] ) )

;; ]

;; [ openssl

(defun mp/show-pem-csr ()
  (interactive)
  (call-process "openssl" nil "openssl.tmp" t "req" "-text" "-noout" "-in" (buffer-file-name)))

(defun mp/show-pem-cert ()
  (interactive)
  (split-window-below)
  (call-process "openssl" nil "openssl.tmp" t "x509" "-text" "-noout" "-in" (buffer-file-name))
  (find-file-other-window "openssl.tmp"))

;; ]

;; [ tramp

;; Note: use /ssh:hostname.domain:/path/to/file to access remote files.
;; For ease of use: Make sure that ssh access is granted via public key

(setq tramp-default-method "ssh")

;; ]

;; [ makefiles

(add-to-list 'auto-insert-alist '("Makefile" . [ "Makefile" ] ))

;; ]

