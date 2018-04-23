;;; init.el --- Emacs initialization file
;; -*- lexical-binding: t -*-
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

;; ]

;; [ custom set variables

(setq gc-cons-threshold (* 512 1024 1024)
      gc-cons-percentage 0.6)

(load "~/.emacs.d/customx.el")

(setq custom-file "~/.emacs.d/custom.el")

(if (file-exists-p custom-file)
    (load custom-file)
  (message "No custom.el file found."))

;; ]

;; [ packages

(require 'package)

(defconst fn-package-guard "~/.emacs.d/.package-guard")
(defconst package-guard-renewal 604800)

(setq package-archives '(("melpa" . "http://melpa.org/packages/"))
      package-enable-at-startup nil
      package-user-dir "~/.emacs.d/packages/")

;; (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

(package-initialize)

;; periodically refresh package contents

(defun timeval-to-seconds (tv)
  "Calculate SEC-HIGH * 2^16 + SEC-LOW for value contained in TV."
  (let* ((sec-high (nth 0 tv))
         (sec-low (nth 1 tv)))
    (+ sec-low (* sec-high (expt 2 16)))))

(defun package-refresh-necessary-p ()
  (if (file-exists-p fn-package-guard)
      (progn
        (let* ((mtime (timeval-to-seconds
                       (nth 5
                            (file-attributes fn-package-guard))))
               (ctime (timeval-to-seconds
                       (current-time))))
          (< (+ mtime package-guard-renewal) ctime )))
    t))

(defun update-package-guard ()
  "Write current time to pacakge-guard file"
  (with-temp-buffer
    (insert (prin1-to-string (current-time)))
    (write-file fn-package-guard)))

(global-set-key (kbd "C-x p") #'package-list-packages)

;; see if this emacs is starting for the first time (with this init.el)
;; and if pacakge refresh is necessary (currently once in a week)

(if (not (file-exists-p fn-package-guard))
    (let* ((emacs-dir (expand-file-name user-emacs-directory))
           (autosave-dir (concat emacs-dir "/auto-save/"))
           (desktop-dir (concat emacs-dir "/desktop"))
           (backup-dir (concat emacs-dir "/backups"))
           (user-information "Will perform first time initialisation! Press enter."))
      (read-from-minibuffer user-information)
      (when (not (file-exists-p autosave-dir))
        (make-directory autosave-dir))
      (when (not (file-exists-p desktop-dir))
        (make-directory desktop-dir))
      (when (not (file-exists-p backup-dir))
        (make-directory backup-dir))
      (update-package-guard)
      (package-refresh-contents)
      (package-install 'use-package))
  (when (package-refresh-necessary-p)
    (let ((user-information "Will refresh package contents! Press enter."))
      (read-from-minibuffer user-information)
      (package-refresh-contents t)
      (update-package-guard))))

(require 'use-package)

(setq use-package-verbose t
      use-package-always-ensure t
      load-prefer-newer t)

;; ]

;; [ calendar

;; TODO: Want diary view entries be called after moving date marker in calendar
;; TODO: Want tab to jump from one entry to the next (shift-tab to jump back)

(global-set-key (kbd "<f4>") #'(lambda ()
                                 (interactive)
                                 "Toggle calendar visibility"
                                 (let ((calendar-window
                                        (get-buffer-window "*Calendar*")))
                                   (if calendar-window
                                       (delete-window calendar-window)
                                     (calendar) ) ) ) )

(defun calendar-mode-setup ()
  (local-set-key (kbd "<RET>") #'diary-view-entries) )

(add-hook 'diary-display-hook 'fancy-diary-display)
(add-hook 'today-visible-calendar-hook 'calendar-mark-today)
(add-hook 'calendar-mode-hook 'calendar-mode-setup)

(setq calendar-longitude 6.116951
      calendar-latitude 50.840401
      calendar-mark-holidays-flag t
      calendar-date-style 'european
      calendar-view-diary-initially-flag t
      calendar-mark-diary-entries-flag t
      number-of-diary-entries 7
      calendar-day-name-array [ "Sonntag"
                                "Montag"
                                "Dienstag"
                                "Mittwoch"
                                "Donnerstag"
                                "Freitag"
                                "Samstag" ]
      calendar-month-name-array [ "Januar"
                                  "Februar"
                                  "März"
                                  "April"
                                  "Mai"
                                  "Juni"
                                  "Juli"
                                  "August"
                                  "September"
                                  "Oktober"
                                  "November"
                                  "Dezember" ]
      solar-n-hemi-seasons '( "Frühlingsanfang"
                              "Sommeranfang"
                              "Herbstanfang"
                              "Winteranfang" )
      holiday-general-holidays '((holiday-fixed 1 1 "Neujahr")
                                 (holiday-fixed 5 1 "1. Mai")
                                 (holiday-fixed 10 3 "Tag der Deutschen Einheit"))
      holiday-christian-holidays '((holiday-float 12 0 -4 "1. Advent")
                                   (holiday-float 12 0 -3 "2. Advent")
                                   (holiday-float 12 0 -2 "3. Advent")
                                   (holiday-float 12 0 -1 "4. Advent")
                                   (holiday-float 11 4 4 "Thanksgiving")
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

;; [ General Emacs Behaviour

(setq auto-window-vscroll nil)

(auto-fill-mode)
(setq fill-column 72)

;; (toggle-debug-on-error)

(setq stack-trace-on-error '(buffer-read-only))

(defsubst notify-available-p ()
  "Return true if notify-send is available in PATH. "
  (exists-in-path "notify-send") )

(defun exists-in-path (file)
  "Search for FILE in PATH. t if file exists. nil otherwise. "
  (let ((available nil)
        (pathelems (split-string (getenv "PATH") ":"))
        (pathelem nil))
    (while (and (not available)
                pathelems)
      (setq pathelem (car pathelems)
            pathelems (cdr pathelems))
      (setq available (file-exists-p (concat pathelem "/" file))) )
    available ) )

(defun notify (msg)
  (if (notify-available-p)
      (start-process "notify-send" nil "notify-send" "-t" "5000" msg)
    (message msg) ) )

;; Show keystrokes immediatly
(setq echo-keystrokes 0.01)

;; do not show startup screen
(setq inhibit-startup-screen t)

;; add system clipboard content to kill ring when copying and yanking
(setq save-interprogram-paste-before-kill t)

;; enable disabled commands
(put 'narrow-to-region 'disabled nil) ;; C-x n n
(put 'narrow-to-page 'disabled nil) ;; C-x n p
(put 'scroll-left 'disabled nil) ;; C-<PgDown>, C-<PgUp>
(put 'erase-buffer 'disabled nil) ;; do not ask when erasing buffer

;; if file starts with #! make set exec bit after saving
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(defun indent-buffer ()
  "A helper function that is called after some templates are auto-inserted."
  (interactive)
  (indent-region (point-min) (point-max)) )

;; Tabs - no.
(setq-default indent-tabs-mode nil)

;; Templates - yes.
(setq auto-insert-directory "~/.emacs.d/templates/"
      auto-insert-query nil)

(auto-insert-mode)

(put 'set-goal-column 'disabled nil)
(put 'downcase-region 'disabled nil) ; C-x C-l

;; yes or no is y or n
(defalias 'yes-or-no-p 'y-or-n-p)

;; handy alias to circumvent the not so intuitive emacs naming
(defalias 'symbol-to-string 'symbol-name)
(defalias 'string-to-symbol 'intern)

(recentf-mode)

;; here goes my personal emacs extension files
(add-to-list 'load-path "~/.emacs.d/elisp/")

(setq scroll-step 1
      scroll-conservatively 10000
      auto-window-vscroll nil
      delete-exited-processes t)

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.")

(global-set-key (kbd "M-Z") #'zap-up-to-char)

;; ]

;; [ marks and navigation

;; use C-u C-SPC to pop mark positions
;; and C-x C-SPC to pop global mark position

(setq set-mark-command-repeat-pop t)

(global-set-key (kbd "C-c k") 'pop-global-mark)

;; ]

;; [ expand region

;; Very handy package. Sets er/try-expand-list on a per mode basis to
;; a list of defuns. Each defun marks part of the
;; buffer. Incrementally largens the part of the buffer a defun
;; operats on. The next larger marked part is then set to the region.
;; To customize add defun to er/try-expand-list in any mode hook.

(use-package expand-region
  :config
  (global-set-key (kbd "C-v") 'er/expand-region)
  (global-set-key (kbd "C-S-v") 'er/contract-region) )

(defun mark-init.el-paragraph ()
  "This function is for the expand region package."
  (interactive)
  (re-search-forward paragraph-separate nil t)
  (set-mark (point))
  (re-search-backward paragraph-start nil t))

(add-hook 'emacs-lisp-mode 
          '(lambda ()
             (add-to-list 'er/try-expand-list 'mark-init.el-paragraph)))

;; ]

;; [ abbreviations

;; (C-u 4 ) C-x a g  to define a global abbrev for word before point

(setq abbrev-file-name "~/.emacs.d/abbrev_defs"
      save-abbrevs t) ;; save abbrevs when file is saved and emacs quits

(if (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file))

;; ]

;; [ server mode edit server

(setq server-use-tcp t
      server-host "localhost"
      server-port 39246)

(server-start)

;; Kill buffers when done (C-x #)
(add-hook 'server-done-hook (lambda nil (kill-buffer nil)))

;; ]

;; [ highlight.el

(use-package highlight)

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
;; 'M-e'     - edit search string
;; C-s C-h b - show all isearch key bindings

;; ]

;; [ s

(use-package s)

;; ]

;; [ encoding systems

;; (modify-coding-system-alist 'file ".*cygwin.*\.sh" 'utf-8-unix)
;; (modify-coding-system-alist 'file ".*cygwin.*\.py[23]?" 'utf-8-unix)

;; ]

;; [ global appearence

(run-with-timer 15 nil '(lambda ()
                          "Set prefered fonts."
                          (progn
                            (dolist (face '(org-level-1
                                            org-level-2
                                            org-level-3
                                            org-level-4
                                            org-level-5))
                              (set-face-attribute face nil
                                                  :family "Hack"
                                                  :height 100
                                                  :weight 'normal
                                                  :width 'normal))

                            (when (eq system-type 'windows-nt)
                              (progn
                                (set-face-attribute 'default nil
                                                    :family "Hack"
                                                    :height 100
                                                    :weight 'normal
                                                    :width 'normal)))

                            (when (eq system-type 'windows-nt)
                              (set-face-attribute 'mode-line nil
                                                  :family "Hack"
                                                  :height 100
                                                  :weight 'normal
                                                  :width 'normal)))))

(use-package stripe-buffer)

(use-package theme-changer
  :config
  (change-theme '(solarized-light) '(solarized-dark)))

(defun add-standard-display-buffer-entry (name)
  "Add an entry to display-buffer-alist for buffers called NAME."
  (add-to-list 'display-buffer-alist
               `(,(regexp-quote name)
                 (display-buffer-at-bottom
                  display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (reusable-frames . visible)
                 (side            . bottom)
                 (window-height   . 0.3))))

(setq frame-title-format '("Emacs://%f "))

(when window-system
  (when (eq system-type 'windows-nt)
    (horizontal-scroll-bar-mode -1))
  (tool-bar-mode -1)
  (menu-bar-mode 1)
  (tooltip-mode -1)
  (scroll-bar-mode -1))

(use-package volatile-highlights
  :disabled
  :init
  (add-hook 'emacs-lisp-mode-hook 'volatile-highlights-mode)
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
  (add-hook 'org-mode-hook 'volatile-highlights-mode))

;; visualize matching paren
(show-paren-mode 1)

;; ]

;; [ the mode line

(let ((line (face-attribute 'mode-line :underline)))
  (set-face-attribute 'mode-line          nil :overline   line)
  (set-face-attribute 'mode-line-inactive nil :overline   line)
  (set-face-attribute 'mode-line-inactive nil :underline  line)
  (set-face-attribute 'mode-line          nil :box        nil)
  (set-face-attribute 'mode-line-inactive nil :box        nil)
  (set-face-attribute 'mode-line-inactive nil :background "#f9f2d9"))

(use-package moody
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(use-package minions
  :config (minions-mode 1))

(which-function-mode)

(setq which-func-unknown "∅")

;; ]

;; [ backup & auto-save

(defconst backup-directory (expand-file-name
                            (concat user-emacs-directory "/backups")))

(defconst auto-save-directory (expand-file-name
                               (concat user-emacs-directory "/auto-save")))

(setq backup-directory-alist `((".*" . ,backup-directory))
      delete-old-versions t
      version-control t
      vc-make-backup-files t
      auto-save-interval 50
      backup-by-copying t
      kept-new-versions 10
      delete-old-versions t
      vc-make-backup-files t
      auto-save-file-name-transforms `((".*" ,auto-save-directory t))
      auto-save-list-file-prefix auto-save-directory
      auto-save-visited-file-name t)

;; ]

;; [ projectile

;; To view key bindings do "C-c p C-h"
;; Also see http://batsov.com/projectile/

(use-package projectile
  :config
  (setq projectile-completion-system 'ivy
        projectile-enable-caching t
        ;; projectile-tags-command "etags --include /home/matthias/opt/jdk/src/TAGS -a TAGS \"%s\"")
        )
  (projectile-mode))

(defun find-file-dispatcher (arg)
  (interactive "P")
  (let ((file-at-point (thing-at-point 'filename)))
    (if (and file-at-point
             (file-exists-p file-at-point))
        (progn
          (message (format "find-file-dispatcher found file at point: %s" file-at-point))
          (find-file file-at-point))
      (call-interactively   (if arg 
                                'projectile-find-file
                              'find-file)))))

(global-set-key (kbd "C-x C-f") 'find-file-dispatcher)

;; ]

;; [ imenu

(setq imenu-auto-rescan t
      imenu-use-popup-menu nil
      imenu-space-replacement "-"
      imenu-sort-function 'imenu--sort-by-name) ;; sorts only mouse menu

(defadvice imenu-recenter-advice (after imenu-center activate)
  (recenter-top-bottom 2))

(global-set-key (kbd "C-'") 'imenu)

;; ]

;; [ ibuffer

(define-ibuffer-sorter alphabetic-by-path
  "Sort the buffers by their filename (including path).
Ordering is lexicographic."
  (:description "buffer name")
  (string-lessp
   (buffer-file-name (car a))
   (buffer-file-name (car b))))


(defadvice ibuffer-center-recenter (around ibuffer-point-to-most-recent activate)
  "Open ibuffer with cursor pointed to most recent (non-minibuffer) buffer name"
  (let ((recent-buffer-name
         (if (minibufferp (buffer-name))
             (buffer-name
              (window-buffer (minibuffer-selected-window)))
           (buffer-name (other-buffer)))))
    ad-do-it
    (ibuffer-jump-to-buffer recent-buffer-name)))

(use-package ibuffer-git
  :disabled)

(defun ibuffer-show-filename ()
  (interactive)
  (let ((buf (ibuffer-current-buffer))
        (lsoutput nil))
    (when (file-exists-p (buffer-file-name buf))
      (with-temp-buffer
        (let* ((filename (buffer-file-name buf))
               (default-directory (file-name-directory filename))
               (just-filename (file-name-nondirectory filename)))
          (call-process "/usr/bin/ls" nil t nil "-l" just-filename)
          (setq lsoutput (buffer-substring-no-properties (point-min) (- (point-max) 1))))))
    (message lsoutput)))

(defun ibuffer-show-file-path ()
  (interactive)
  (let ((buf (ibuffer-current-buffer))
        (lsoutput nil))
    (when (file-exists-p (buffer-file-name buf))
      (with-temp-buffer
        (let* ((filename (buffer-file-name buf))
               (default-directory (file-name-directory filename))
               (just-filename (file-name-nondirectory filename)))
          (call-process "pwd" nil t nil)
          (setq lsoutput (buffer-substring-no-properties (point-min) (- (point-max) 1))))))
    (message lsoutput)))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :init

  (define-ibuffer-column dirname
    (:name "Directory" :inline nil)
    (let ((result (buffer-name))
          (buf-file-name (buffer-file-name buffer)))
      (when (and buf-file-name
                 (file-exists-p buf-file-name))
        (setq result (file-name-directory buf-file-name))
        result)))

  (define-ibuffer-column additional-info
    (:name "Description" :inline nil)
    (let* ((result nil)
           (buffer-mode (symbol-to-string (with-current-buffer buffer
                                            major-mode)))
           (buf-file-name (buffer-file-name buffer))
           (dircomponents nil)
           (dir-name (if (and buf-file-name
                              (file-exists-p buf-file-name))
                         (file-name-directory buf-file-name)
                       nil)))
      (if dir-name
          (let* ((dirparts (reverse (split-string dir-name "/")))
                 (result (if (> (length dirparts) 6)
                             (concat "…/"
                                     (nth 4 dirparts) "/"
                                     (nth 3 dirparts) "/"
                                     (nth 2 dirparts) "/"
                                     (nth 1 dirparts) "/")
                           (file-name-directory buf-file-name))))
            (if result
                result
              buffer-mode))
        buffer-mode)))

  (setq ibuffer-show-empty-filter-groups nil
        ;;      ibuffer-formats '(( mark (git-status-mini) modified read-only "|"
        ibuffer-formats '(( mark modified read-only "|"
                                 (name 36 36 :left :elide)
                                 "|"
                                 (size 9 -1 :left)
                                 "|" additional-info) ;; filename-and-process)
                          (mark " "
                                (name 16 -1)
                                " " filename)))

  (defun ibuffer-mode-hook-extender ()
    (ibuffer-auto-mode 1) ;; auto updates
    (hl-line-mode)
    (define-key ibuffer-mode-map (kbd "p") 'ibuffer-show-file-path))
  
  (add-hook 'ibuffer-mode-hook 'ibuffer-mode-hook-extender))

;; ]

;; [ emacs lisp mode

(defvar elx-debug-ignored-errors nil
  "Keep backup of DEBUG-IGNORED-ERRORS erros before overwriting variable")

(defun elx-debug-on-error (arg)
  "Do debug on error with no ignored errors at all. When prefix ARG is present
restore former values for debug-on-error and debug-ignored-errors."
  (interactive "P")
  (if arg
      (progn
        (setq debug-on-error nil
              debug-ignored-errors elx-debug-ignored-errors
              elx-debug-ignored-errors nil)
        (message "Debug on error disabled"))
    (progn
      (setq elx-debug-ignored-errors (if elx-debug-ignored-errors
                                         elx-debug-ignored-errors
                                       debug-ignored-errors)
            debug-on-error t
            debug-ignored-errors nil)
      (message "Debug on error enabled"))))

(global-set-key (kbd "C-x C-d") 'elx-debug-on-error)

(defun elisp-preprocessor()
  "Process emacs lisp template file and replace place holder."
  (let* ((filename (buffer-name))
         (description (read-from-minibuffer "Description: "))
         (maintainer (format "%s (%s)" user-full-name user-mail-address))
         (author user-full-name)
         (version (read-from-minibuffer "Version: "))
         (url (read-from-minibuffer "Url: "))
         (keywords (read-from-minibuffer "Keywords: "))
         (timestamp (current-time-string)))
    (progn 
      (goto-char (point-min))
      (while (search-forward "FILENAME" nil t)
        (replace-match filename t))
      (goto-char (point-min))
      (while (search-forward "DESCRIPTION" nil t)
        (replace-match description t) )
      (goto-char (point-min))
      (while (search-forward "AUTHOR" nil t)
        (replace-match author t) )
      (goto-char (point-min))
      (while (search-forward "MAINTAINER" nil t)
        (replace-match maintainer t) )
      (goto-char (point-min))
      (while (search-forward "TIMESTAMP" nil t)
        (replace-match timestamp t) )
      (goto-char (point-min))
      (while (search-forward "VERSION" nil t)
        (replace-match version t) )
      (goto-char (point-min))
      (while (search-forward "URL" nil t)
        (replace-match url t) )
      (goto-char (point-min))
      (while (search-forward "KEYWORDS" nil t)
        (replace-match keywords t) ) ) ) )

(add-to-list 'auto-insert-alist '(".*\\.el$" . [ "template.el" elisp-preprocessor] ) )

(defun dotemacs-mode-hook ()
  (local-set-key (kbd "C-S-n") 'forward-paragraph)
  (local-set-key (kbd "C-S-p") 'backward-paragraph)
  (setq imenu-prev-index-position-function nil)
  (setq-local comment-auto-fill-only-comments t)
  (setq-local comment-multi-line t)
  (make-local-variable 'paragraph-start)
  (make-local-variable 'paragraph-separate)
  (setq paragraph-start ";; \\[ "
        paragraph-separate ";; ]")
  (setq imenu-generic-expression 
        (list '(nil "^;; \\[ \\(.+\\)$" 1)))
  (setq-local imenu-create-index-function 'imenu-default-create-index-function) )

(defun byte-compile-current-buffer ()
  (interactive)
  (byte-compile-file (buffer-file-name)))

(defun emacs-lisp-mode-setup ()
  (when (string= (buffer-name) "init.el")
    (dotemacs-mode-hook))
  (local-set-key (kbd "C-/") 'comment-dwim)
  (local-set-key (kbd "C-c C-c") 'byte-compile-current-buffer)
  (electric-pair-mode) )

(add-hook 'emacs-lisp-mode-hook 'emacs-lisp-mode-setup)

(use-package elisp-slime-nav
  :config
  (add-hook 'emacs-lisp-mode-hook 'turn-on-elisp-slime-nav-mode))

;; ]

;; [ save history 
;;
;; Do save minibuffer history

(setq savehist-file "~/.emacs.d/minibuffer"
      history-length t
      history-delete-duplicates t
      savehist-save-minibuffer-history 1
      savehist-additional-variables '(kill-ring
                                      search-ring
                                      regexp-search-ring))

(savehist-mode 1)

;; ]

;; [ the kill ring

(defun elx-browse-kill-ring ()
  (interactive)
  (insert (completing-read "Pick an element: " kill-ring)))

(global-set-key (kbd "C-M-y") 'elx-browse-kill-ring)

;; ]

;; [ yasnippet



(use-package yasnippet

  :config
  
  (require 'yasnippet-snippets)

  (defconst yasnippet-my-snippets-dir "~/.emacs.d/snippets/")
  (add-to-list 'yas-snippet-dirs yasnippet-my-snippets-dir t)

  ;; Add snippet directory to auto-mode-alist
  ;; so that files are opened in snipped-mode
  (dolist (snippet-dir yas-snippet-dirs)
    (add-to-list 'auto-mode-alist (cons (concat ".*" snippet-dir ".*") 'snippet-mode))
    (yas-load-directory snippet-dir))

  (require 'warnings)
  ;; do not complain when snippets change buffer contents
  (add-to-list 'warning-suppress-types '(yasnippet backquote-change))

  ;; enable yasnippet mode in some modes:
  (add-hook 'python-mode-hook 'yas-minor-mode)
  (add-hook 'java-mode-hook 'yas-minor-mode)
  (add-hook 'emacs-lisp-mode-hook 'yas-minor-mode)
  (add-hook 'js2-mode-hook 'yas-minor-mode)
  (add-hook 'xml-mode-hook 'yas-minor-mode)
  (add-hook 'nxml-mode-hook 'yas-minor-mode)
  (add-hook 'maven-mode-hook 'yas-minor-mode)
  (add-hook 'ant-mode-hook 'yas-minor-mode) )

;; ]

;; [ js2 javascript

(defun js2-mode-setup ()
  (setq indent-tabs-mode nil
        js-indent-level 4)
  (setq-local comment-auto-fill-only-comments t)
  (auto-fill-mode 1)
  (setq-local comment-multi-line t)
  (local-set-key (kbd "RET") 'c-indent-new-comment-line))

(define-auto-insert '("\\.js\\'" . "Javscript Skeleton")
  [ '(nil
      "/*\n * "
      (file-name-nondirectory (buffer-file-name)) "\n"
      " * File created on " (format-time-string "%A, %e %B %Y.") \n
      " */" \n \n \n )
    indent-buffer ] )

(defun create-qunit-test-for-current-buffer ()
  (interactive)
  (let ((test-html-file (replace-regexp-in-string (regexp-quote ".js") "-test.html" (buffer-name)))
        (test-js-file (replace-regexp-in-string (regexp-quote ".js") "-test.js" (buffer-name))))
    (progn
      (find-file test-html-file)
      (goto-char (point-min))
      (split-window (selected-window) 15)
      (other-window 1)
      (find-file test-js-file))))

(use-package js2-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-hook 'js2-mode-hook 'js2-mode-setup))

;; ]

;; [ info browser

;; To read plain info file from the filesystem: "C-u C-h i"

;; TODO: There is a python info page lying around in this directcory,
;; but info does not find it


(add-to-list 'Info-default-directory-list "~/.emacs.d/info")

(defun Info-mode-setup ()
  "Personal info mode hook."
  (define-key Info-mode-map (kbd "C-s") 'isearch-forward-regexp) )

(add-hook 'Info-mode-hook 'Info-mode-setup)

;; ]

;; [ session management

;; save and restore open buffers

(desktop-save-mode)

;; ]

;; [ org mode

(add-hook 'org-agenda-mode-hook 'org-agenda-follow-mode)
(add-hook 'org-agenda-mode-hook 'hl-line-mode)

;; TODO: This does not seem to work 

(use-package org-gcal
  :disabled
  :config
  (add-hook 'org-agenda-mode-hook (lambda () (org-gcal-sync) ))
  (add-hook 'org-capture-after-finalize-hook (lambda () (org-gcal-sync)))
  (when (not (require 'org-gcal-secrets nil 'noerror))
    (message "org gcal secrets are not available!")))

;; examples https://github.com/zweifisch/ob-http
(use-package ob-http
  :config
  (require 'ob-http))

(require 'ob-plantuml)

(require 'ob-python)

(global-set-key (kbd "C-x c") #'org-capture)

;; clocking commands:
;;    C-c C-x C-i (org-clock-in)
;;    C-c C-x C-o (org-clock-out)
;;    C-c C-x C-q (org-clock-cancel)
;;    C-c C-x C-d (org-clock-displa)
;;    C-S-<up/down> (org-clock-timestamps-up/down)
;;    S-M-<up/down> (org-timestamp-up-down)

(defun visit-org-files ()
  "Visit any of the files in org-agenda-files or org-default-notes-file."
  (interactive)
  (let ((org-file (completing-read
                   "File to visit: "
                   (cons org-default-notes-file org-agenda-files))))
    (find-file-other-frame org-file)))

;; agenda commands:
;;    C-c C-s    Schedule item for date
;;    C-c C-d    Set deadline for item
;;    Want timestamps? Add manually so that marker string looks
;;    like this: <2016-02-28 Sa 14:00-15:30>
;;    Timestamp magic:
;;    <2016-02-28 Sa 14:00-15:30 +1w>   repeat every week
;;    <2016-02-28 Sa 14:00-15:30 ++1w>  next item will always be in the future
;;    <2016-02-28 Sa 14:00-15:30 .+4w>  timedistance is added to today

(add-to-list 'auto-mode-alist '("organizer\\'" . org-mode))

(defun org-mode-setup ()
  ;;  "Stop the org-level headers from increasing in height relative to the other text."
  
  (org-hide-block-all)
  (flyspell-mode)
  (add-to-list 'org-file-apps '("\\.png\\'" . default))
  (company-mode -1) ;; disabled, since it looks broken
  (setq org-agenda-span 7
        org-agenda-comact-blocks t
        org-agenda-show-all-dates t
        org-babel-python-command "python"
        org-clock-into-drawer t
        org-clock-persist 'history
        org-confirm-babel-evaluate nil
        org-default-notes-file "~/org/organizer"
        org-directory "~/org/"
        org-ellipsis "…"
        org-log-done (quote note)
        org-log-into-drawer t
        org-special-ctrl-a/e t
        org-special-ctrl-k t
        org-todo-keywords '((sequence "TODO" "WAITING" "|" "DONE" "CANCELED" "DELEGATED")))
  (local-set-key (kbd "<return>") #'org-return-indent)
  (local-set-key (kbd "C-'") #'imenu)
  (local-set-key (kbd "C-c t") #'org-set-tags)
  (local-set-key (kbd "C-c i") #'org-clock-in)
  (local-set-key (kbd "C-c o") #'org-clock-out)
  ;;  (setenv "GRAPHVIZ_DOT" "/usr/bin/dot")
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
     (http . t)
     (sh . t) ) ) )

(org-clock-persistence-insinuate)

(defconst org-capture-file "~/org/organizer" "Location of file where to store org capture notes.")

;; capture templates are documented at
;; http://orgmode.org/manual/Capture.html

(setq org-capture-templates
      '(
        ("P" "Project" entry (file+headline org-capture-file "Inbox")
         "* %?\n\n%^T\n\n" :kill-buffer t :clock-in t :clock-resume)
        ("a" "Appointment" entry (file  org-capture-file)
	 "* %?\n\n%^T\n\n:PROPERTIES:\n\n:END:\n\n")
        ("t" "todo" entry (file+headline org-capture-file "Inbox")
         "** TODO %?\n%U\n%a\n" :kill-buffer t :clock-in t :clock-resume t)
        ("n" "note" entry (file+headline org-capture-file "Inbox")
         "** %? :NOTE:\n%U\n%a\n" :kill-buffer t :clock-in t :clock-resume t)
        ("m" "Meeting" entry (file+headline org-capture-file "Inbox")
         "** MEETING with %? :MEETING:\n%U" :kill-buffer t :clock-in t :clock-resume t)
        ("p" "Phone call" entry (file+headline org-capture-file "Inbox")
         "** PHONE %? :PHONE:\n%U" :kill-buffer t :clock-in t :clock-resume t) ) )

(add-hook 'org-mode-hook 'org-mode-setup)

;; ]

;; [ prodigy service manager

;; Warning: prodigy does not seem to allow stopping services on Windows

(use-package prodigy
  :config
  
  (defun prodigy-setup-frame ()
    (interactive)
    (let ((frame-parameters '((name . "Prodigy")
                              (height . 25)
                              (width . 80)
                              (minibuffer . t))))
      (select-frame (make-frame frame-parameters))
      (prodigy)
      (delete-other-windows)))
  
  ;;  (advice-add 'prodigy-start-service :after #'prodigy-display-process)

  (defun prodigy-next-line ()
    (interactive)
    (forward-line)
    (when (looking-at ".*Running.*")
      (progn
        (prodigy-display-process)
        (select-window (get-buffer-window "*prodigy*")))))

  (defun prodigy-previous-line ()
    (interactive)
    (forward-line -1)
    (when (looking-at ".*Running.*")
      (progn
        (prodigy-display-process)
        (select-window (get-buffer-window "*prodigy*")))))

  (defun prodigy-mode-setup ()
    (local-set-key (kbd "C-n") 'prodigy-next-line)
    (local-set-key (kbd "C-p") 'prodigy-previous-line))

  (add-hook 'prodigy-mode-hook 'prodigy-mode-setup)

  (global-set-key (kbd "<f5>") #'prodigy-setup-frame)

  (prodigy-define-service
    :name "Tomcat"
    :command tomcat-start-script
    :args '("run")
    :cwd tomcat-root-dir)

  (prodigy-define-service
    :name "Date Server (14002)"
    :command prodigy-python-interpreter
    :args '("date.py" "14002")
    :stop-signal 'int
    :cwd (concat prodigy-service-root "date/"))

  (prodigy-define-service
    :name "Network Log-Receiver"
    :command prodigy-python-interpreter
    :args '("logwebmon.py")
    :cwd (concat prodigy-service-root "loghost/"))

  (prodigy-define-service
    :name "Echo Server (14001)"
    :command prodigy-python-interpreter
    :args '("echo.py" "14001")
    :stop-signal 'int
    :cwd (concat prodigy-service-root "echo/") ) )

;; ]

;; [ speedbar neotree treemacs

(use-package sr-speedbar
  :bind ("<f6>" . sr-speedbar-toggle)
  :config
  :disabled
  (setq speedbar-fetch-etags-arguments (quote ("--declarations" "-D" "-I" "-o" "-"))
        speedbar-sort-tags t
        speedbar-ag-hierarchy-method (quote (speedbar-sort-tag-hierarchy))
        speedbar-use-imenu-flag nil
        speedbar-verbosity-level 2
        sr-speedbar-right-side nil
        speedbar-show-unknown-files t)
  (define-key speedbar-file-key-map (kbd "C-j") 'speedbar-expand-line)
  (define-key speedbar-file-key-map (kbd "C-k") 'speedbar-contract-line)
  (defadvice sr-speedbar-toggle (after select-speedbar activate)
    (let ((window (get-buffer-window "*SPEEDBAR*")))
      (when window (select-window window)))))

(use-package treemacs-projectile
  :disabled)

(use-package treemacs
  :config
  (setq ;; treemacs-header-function            #'treemacs--create-header-projectile
   treemacs-follow-after-init          t
   treemacs-width                      35
   treemacs-indentation                2
   treemacs-change-root-without-asking nil
   treemacs-sorting                    'alphabetic-desc
   treemacs-show-hidden-files          nil
   treemacs-never-persist              nil
   treemacs-icon-open-png   (propertize "⊖ " 'face 'treemacs-directory-face)
   treemacs-icon-closed-png (propertize "⊕ " 'face 'treemacs-directory-face))
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)

  :bind
  (:map global-map
        ([f6]        . treemacs-toggle)))

;; (modify-frame-parameters nil (list '( name . "Emacs" )

(use-package neotree
  :disabled)

;; ]

;; [ xref and tags

;; TODO Change window handling for xref popups

;;(add-standard-display-buffer-entry "*xref*")

(defun close-window-by-buffer-name (name)
  (interactive "b")
  (let ((buffer-name name)
        (buffer nil)
        (buffer-window nil))
    (setq buffer (get-buffer buffer-name))
    (setq buffer-window (if (bufferp buffer)
                            (get-buffer-window buffer)
                          nil))
    (when (windowp buffer-window)
      (delete-window buffer-window))))

(defun close-xref-buffer ()
  (close-window-by-buffer-name "*xref"))

(setq tags-revert-without-query t)

;; use C-o in xref buffer to display match in other window

(global-set-key (kbd "M-*") #'xref-pop-marker-stack)
(global-set-key (kbd "M-.") #'xref-find-definitions)

;; ]

;; [ perl mode

(fset 'perl-mode 'cperl-mode)

(add-hook 'cperl-mode-hook '(lambda ()
                              (interactive)
                              (local-set-key (kbd "C-h c") 'cperl-perldoc-at-point)))

;; ]

;; [ hungry delete mode
;;
;; This mode makes backspace erase all consecutive whitespace (instead of just a single one).

(use-package hungry-delete
  :config
  (require 'hungry-delete)
  (global-hungry-delete-mode))

;; ]

;; [ ansi term mode

;; C-c C-j  to put terminal to line mode
;; C-c C-k  to put terminal in char mode

(use-package term
  :config
  (add-standard-display-buffer-entry "*terminal*")
  (defun open-terminal (prefix-arg)
    "PREFIX-ARG -> open terminal in new frame
current buffer is terminal buffer -> close buffer
terminal is only buffer in frame -> close frame
current frame has one window -> split window + open terminal
current frame has more windows -> open terminal in new frame"
    (interactive "P")
    (when (and (not (get-buffer "*terminal*"))
               (>= (length (window-list)) 2))
      (setq prefix-arg t))
    (if prefix-arg
        (progn 
          (select-frame (make-frame))
          (let ((term-buffer (get-buffer "*terminal*")))
            (if term-buffer
                (switch-to-buffer "*terminal*")
              (ansi-term "/bin/bash"))))
      (if (string= "*terminal*" (buffer-name))
          (if (eq 1 (length (window-list)))
              (delete-frame)
            (delete-window))
        (progn
          (if (get-buffer "*terminal*")
              (select-window (display-buffer "*terminal*"))
            (ansi-term "/bin/bash"))))))

  (global-set-key (kbd "C-x 0") #'delete-window)
  (global-set-key (kbd "C-x k") #'kill-buffer)
  (global-set-key (kbd "<f7>") #'open-terminal)
  (define-key term-raw-map (kbd "M-o") 'other-buffer))

;; ]

;; [ C/C++

(defun mark-def-undef-block ()
  "Mark block from #define to #undef."
  (interactive)
  (let ((tagname nil))
    (re-search-forward "^#undef \\(.*\\)" nil t)
    (setq tagname (match-string 1))
    (set-mark (point))
    (re-search-backward (concat "^#define " tagname) nil t)))

(defun mark-if-endif-block ()
  "Mark block from #define to #undef."
  (interactive)
  (let ((tagname nil))
    (re-search-forward "^#endif.*" nil t)
    (set-mark (point))
    (re-search-backward (concat "^#ifdef " tagname) nil t)))

(defun openssl-help ()
  (interactive)
  (browse-url-firefox (concat 
                       "https://www.openssl.org/docs/manmaster/man3/"
                       (thing-at-point 'symbol))
                      t))

(defun c++-help ()
  (interactive)
  (browse-url-firefox (concat 
                       "http://www.cplusplus.com/reference/"
                       (thing-at-point 'symbol))
                      t) )

(defun c-mode-setup ()
  "Personal c mode hook extender."
  (let ((add-openssl-dict nil))
    (save-excursion 
      (goto-char (point-min))
      (when (re-search-forward "^#include ?<openssl/.*" nil t)
        (setq add-openssl-dict t)))
    (when add-openssl-dict
      (progn
        (local-set-key (kbd "C-h o") 'openssl-help))))
  (local-set-key (kbd "C-h c") 'c++-help)
  (local-set-key (kbd "C-x a") 'align-regexp)
  (add-to-list 'er/try-expand-list 'mark-def-undef-block)
  (add-to-list 'er/try-expand-list 'mark-if-endif-block)
  (local-set-key (kbd "C-c C-c") 'compile)
  (local-set-key (kbd "C-M-j") 'imenu))

(add-hook 'c++-mode-hook 'c-mode-setup)
(add-hook 'c-mode-hook 'c-mode-setup)

;; ]

;; [ frame+window handling

(use-package windmove
  :config

  (setq windmove-wrap-around t)
  
  (global-set-key (kbd "C-<up>") 'windmove-up)
  (global-set-key (kbd "C-<down>") 'windmove-down)
  (global-set-key (kbd "C-<right>") 'windmove-right)
  (global-set-key (kbd "C-<left>") 'windmove-left))

;; (winner-mode)

(defun detach-window (arg)
  "Iff current frame hosts at least two windows, close current window
and display corresponding buffer in new frame."
  (interactive "P")
  (if (not (one-window-p))
      (let ((buffer (current-buffer)))
        (when (not arg)
          (delete-window) )
        (display-buffer-pop-up-frame buffer nil) ) 
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

(defun swap-buffers ()
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

(global-set-key (kbd "<f1>") #'detach-window)
(global-set-key (kbd "<f2>") #'make-frame)
(global-set-key (kbd "<f3>") #'delete-frame)
(global-set-key (kbd "C-x 2") #'split-window-below)
(global-set-key (kbd "C-x 3") #'split-window-right)

(defvar windows-ops-keymap
  (make-sparse-keymap)
  "Keymap for windmove commands.")

(global-set-key (kbd "C-x w") windows-ops-keymap)
(define-key windows-ops-keymap (kbd "s") #'swap-buffers)
(define-key windows-ops-keymap (kbd "r") #'rotate-windows)


;; ]

;; [ ediff

(add-hook 'ediff-after-quit-hook-internal 'winner-undo)

(defun ediff-this ()
  "If current frame hosts exactly two windows. ediff the two window buffers."
  (interactive)
  (if (eq 2 (length (window-list)))
      (let* ((win-1 (nth 0 (window-list)))
             (win-2 (nth 1 (window-list)))
             (buf-1 (window-buffer win-1))
             (buf-2 (window-buffer win-2)))
        (ediff-buffers buf-1 buf-2))))

;; ]

;; [ whitespace mode

(global-set-key (kbd "C-x TAB") #'whitespace-mode)

;; ]

;; [ man woman help mode

(setq 
 woman-use-own-frame t
 woman-use-topic-at-point-default t)

(defsubst scroll-up-one-line ()
  (interactive)
  (scroll-up 1))

(defsubst scroll-down-one-line ()
  (interactive)
  (scroll-down 1) )

(defun man-mode-setup ()
  (define-key Man-mode-map (kbd "C-n") 'scroll-up-one-line)
  (define-key Man-mode-map (kbd "C-p") 'scroll-down-one-line) )

(add-hook 'Man-mode-hook 'man-mode-setup)

(defun my-help-mode-setup ()
  (local-set-key (kbd "q") 'kill-buffer)  
  (local-set-key (kbd "C-q") 'kill-buffer-and-window))

(add-hook 'help-mode-hook 'my-help-mode-setup)

(require 'man)

(set-face-attribute 'Man-overstrike nil :inherit font-lock-type-face :bold t)
(set-face-attribute 'Man-underline nil :inherit font-lock-keyword-face :underline t)

;; ]

;; [ java mode

(use-package jtags
  :config
  (add-hook 'java-mode-hook 'jtags-mode))

(setq tags-table-list (list (concat jdk-location "/src")))

(setq tags-revert-without-query 't)

(use-package javadoc-lookup
  :config
  (javadoc-add-roots (concat jdk-location "/docs"))
  ;;  (javadoc-add-artifacts [org.springframework spring-core "4.3.2-RELEASE"])
  (setq browser-url-browser-function 'browse-url-chromium))

(defvar-local java-classpath nil "Java classpath. This will be set by .dir-locals.el (hopefully).")
(defvar-local java-project-root nil "Buffer local location of current project root.")
(defvar-local java-classes-cache nil "Cache for the current classpath classes.")

(defun java-read-classes-from-classpath ( classpath-list )
  "Iterate over classpath and gather classes from jar files.
Evaluates into one large list containing all classes."
  (let* ((jarfiles nil)
         (jarfile nil)
         (result '()))
    (progn
      (dolist (file (directory-files (concat jdk-location "/jre/lib") t "\.\*.jar\$"))
        (setq jarfiles (cons file jarfiles)))
      (dolist (file (reverse classpath-list))
        (setq jarfiles (cons file jarfiles))))
    (with-temp-buffer
      (while jarfiles
        (progn
          (setq jarfile (car jarfiles)
                jarfiles (cdr jarfiles))
          (call-process "unzip" nil t nil "-l" (expand-file-name jarfile))
          (goto-char (point-min))
          (let ((end 0)
                (classname ""))
            (while (search-forward ".class" nil t nil)
              (end-of-line)
              (setq end (point))
              (beginning-of-line)
              (goto-char (+ (point) 30))
              (setq classname (substring 
                               (replace-regexp-in-string "/" "."
                                                         (buffer-substring-no-properties (point) end))
                               0 -6))
              (setq result (cons classname result))
              (forward-line 1)
              (beginning-of-line))
            (erase-buffer)))))
    result))

(defun insert-import-statement ()
  (interactive)
  (let* ((default (thing-at-point 'symbol))
         (classname (completing-read "Class: " java-classes-cache)))
    (insert "import " classname ";")))

(defun java-insert-classname-completing-read (prefix)
  "Query the user for a class name.
With prefix argument insert classname with package name. Otherwise omit package name."
  (interactive "P")
  (let* ((default (thing-at-point 'symbol))
         (classname (completing-read "Class: " java-classes-cache)))
    (if prefix
        (insert classname)
      (insert (replace-regexp-in-string ".*\\." "" classname)))))

(defun java-assert-import (name)
  "Insert import statement for class NAME if it does not yet exist. "
  (push-mark)
  (goto-char (point-min))
  (when (not (re-search-forward (format "^import %s;" name) nil t))
    (progn
      (while (re-search-forward "^import.*" nil t))
      (end-of-line)
      (newline-and-indent)
      (insert (format "import %s;" name)))))

(defun java-add-import ()
  (interactive)
  (let* ((classname (completing-read "Class: " java-classes-cache)))
    (java-assert-import classname)))

(defun guess-package-name-for-current-buffer ()
  "See if this is a maven project with standard directory layout.
If so calculate pacakge name from current directory name."
  (let* ((dirname (file-name-directory (buffer-file-name)))
         (indicator "/src/main/java/")
         (package-name "undefined")
         (matched-string nil))
    (progn
      (string-match (concat ".*" indicator "\\(.*\\)") dirname)
      (setq matched-string (match-string 1 dirname))
      (unless matched-string ;; if indicator is not found just take
        (setq matched-string dirname)) ;; whole path for the package-name
      (setq package-name matched-string)
      (when (string-suffix-p "/" package-name)
        (setq package-name (substring package-name 0 -1)))
      (when (string-prefix-p "/" package-name)
        (setq package-name (substring package-name 1)))
      (setq package-name (replace-regexp-in-string "/" "." package-name))
      (setq package-name (replace-regexp-in-string "\\.\\." "." package-name))
      package-name)))

(defun java-preprocessor()
  (let ((classname (file-name-sans-extension (buffer-name)))
        (packagename (guess-package-name-for-current-buffer)))
    (while (search-forward "CLASSNAME" nil t)
      (replace-match classname t))
    (goto-char (point-min))
    (while (search-forward "PACKAGE" nil t)
      (replace-match packagename t) ) ) )

(defun copy-template (filename target-file alist)
  "Copy FILENAME to TARGET-FILE. Then replace keys with values looked up in ALIST"
  (with-temp-buffer
    (insert-file-contents (concat "~/.emacs.d/templates/" filename))
    (dolist (key-value alist)
      (goto-char (point-min))
      (let ((key (symbol-to-string (car key-value)))
            (value (car (cdr key-value)))
            (case-fold-search nil))
        (while (search-forward key nil t)
          (replace-match value t))))
    (write-file target-file nil) 
    (kill-buffer) ) )

(defun start-new-java-project (group-id artifact-id version-number)
  (interactive "MGroup-id: \nMArtifact-id: \nMVersion-number: ")
  (let* ((project-root (concat (expand-file-name java-project-root) artifact-id))
         (target-pom (concat project-root "/pom.xml"))
         (src-dir (concat project-root "/src/main/java/"))
         (main-class (concat src-dir
                             (replace-regexp-in-string "\\." "/" group-id)
                             "/" 
                             artifact-id
                             ".java"))
         (class-dir (file-name-directory main-class))
         (pframe (make-frame))
         (default-directory project-root))

    (when (not (file-exists-p project-root))
      (make-directory project-root t))

    (copy-template "pom.xml" target-pom
                   (list (list 'GROUP-ID group-id)
                         (list 'ARTIFACT-ID artifact-id)
                         (list 'VERSION version-number)))

    (when (not (file-exists-p class-dir))
      (make-directory class-dir t))

    (select-frame pframe)
    (find-file main-class)
    (save-buffer)
    (neotree-dir project-root)) )

;; TODO Something is veery buggy here. Why is C-x c defined in emacs-lisp mode?
(defun java-mode-process-dir-locals ()
  (when (derived-mode-p 'java-mode)
    (progn
      (when (stringp java-project-root)
        ;; sell the stock from emacs-maven-plugin:
        (progn
          (setq-local java-classes-cache (java-read-classes-from-classpath java-classpath) ))
        (local-set-key (kbd "C-x c") 'java-insert-classname-completing-read)))))

(defun java-mode-setup()

  ;; Treat Java 1.5 @-style annotations as comments.
  (setq c-comment-start-regexp "(@|/(/|[*][*]?))")
  (modify-syntax-entry ?@ "< b" java-mode-syntax-table)

  ;;
  (setq-local comment-auto-fill-only-comments t)
  (setq-local comment-multi-line t)

  ;; navigate cammel cased words
  (subword-mode)

  ;; Get help with javadoc (need to have this working for non-jds classes too
  (local-set-key (kbd "C-h j") 'javadoc-lookup)
  (local-set-key (kbd "C-?") 'java-add-import) ;; TODO Have a handy keymap for your java functions like "C-c j"

  ;; Turn on auto-complete mode and set ac-sources
  (auto-complete-mode 1)
  (setq ac-sources '(ac-source-yasnippet
                     ac-source-classpath
                     ac-source-dictionary
                     ac-source-words-in-same-mode-buffers)))

(defun mpj-create-tags ()
  "Create tags file in maven project."
  (let ((default-directory (projectile-project-root)))
    (eshell-command 
     (format (concat "find %s -name \\*.java -type f "
                     "| etags"
                     " -o " (projectile-project-root) "TAGS"
                     " --include /home/matthias/opt/jdk/src/TAGS"
                     " -")
             (projectile-project-root)))))

;; slightly modified for xref-find-definition from emacswiki article
(defadvice xref-find-definitions (around refresh-etags activate)
  "Rerun etags and reload tags if tag not found and redo find-tag.              
   If buffer is modified, ask about save before running etags."
  (let ((extension (file-name-extension (buffer-file-name))))
    (condition-case err
        ad-do-it
      (error (and (buffer-modified-p)
                  (not (ding))
                  (y-or-n-p "Buffer is modified, save it? ")
                  (save-buffer))
             (when (member #'etags--xref-backend xref-backend-functions)
               (let ((tags-revert-without-query t))
                 (mpj-create-tags)
                 (visit-tags-table (projectile-project-root) nil))
               ad-do-it)))))

(add-hook 'hack-local-variables-hook 'java-mode-process-dir-locals)
(add-hook 'java-mode-hook 'java-mode-setup)

;; preprocessor for interactively generating files from templates
(add-to-list 'auto-insert-alist '(".*\\.java$" . [ "template.java" java-preprocessor] ) )

;; ]

;; [ dired

(defun dired-show-only (regexp)
  "Show only files matching REGEXP. To revert buffer to show all files press g."
  (interactive "sFiles to show (regexp): ")
  (dired-mark-files-regexp regexp)
  (dired-toggle-marks)
  (dired-do-kill-lines))

(defun dired-2pane-copy-over ()
  (interactive)
  (when (eq 2 (length (window-list)))
    (let ((other-directory nil)
          (file-to-copy (dired-get-filename)))
      (progn
        (other-window 1)
        (setq other-directory (dired-current-directory))
        (other-window 1)
        (copy-file file-to-copy other-directory)
        (other-window 1)
        (revert-buffer)
        (other-window 1)))))

;; copied from '(or emacs' blog

(defun ora-ediff-files ()
  "Invoke ediff on marked file(s)."
  (interactive)
  (let ((files (dired-get-marked-files))
        (wnd (current-window-configuration)))
    (if (<= (length files) 2)
        (let ((file1 (car files))
              (file2 (if (cdr files)
                         (cadr files)
                       (read-file-name
                        "file: "
                        (dired-dwim-target-directory)))))
          (if (file-newer-than-file-p file1 file2)
              (ediff-files file2 file1)
            (ediff-files file1 file2))
          (add-hook 'ediff-after-quit-hook-internal
                    (lambda ()
                      (setq ediff-after-quit-hook-internal nil)
                      (set-window-configuration wnd))))
      (error "no more than 2 files should be marked"))))

(add-hook 'dired-mode-hook
          (lambda ()
            (set-face-attribute 'dired-header nil
                                :height 1.0
                                :weight 'normal
                                :width 'normal)
            (define-key dired-mode-map "f" 'dired-show-only)
            (define-key dired-mode-map "e" 'ora-ediff-files)
            (define-key dired-mode-map (kbd "<backspace>")
              (lambda () (interactive) (find-alternate-file "..")))
            (define-key dired-mode-map (kbd "c") 'dired-2pane-copy-over)
            (define-key dired-mode-map (kbd "TAB") 'other-window)
            


            (defhydra hydra-dired (:hint nil :color "#268bd2")
              "
_+_ mkdir          _v_iew           _m_ark             _(_ details        _i_nsert-subdir    wdired
_C_opy             _O_ view other   _U_nmark all       _)_ omit-mode      _$_ hide-subdir    C-x C-q : edit
_D_elete           _o_pen other     _u_nmark           _l_ redisplay      _w_ kill-subdir    C-c C-c : commit
_R_ename           _M_ chmod        _t_oggle           _g_ revert buf     _e_ ediff          C-c ESC : abort
_Y_ rel symlink    _G_ chgrp        _E_xtension mark   _s_ort             _=_ pdiff
_S_ymlink          ^ ^              _F_ind marked      _._ toggle hydra   \\ flyspell
_r_sync            ^ ^              ^ ^                ^ ^                _?_ summary
_z_ compress-file  _A_ find regexp
_Z_ compress       _Q_ repl regexp

T - tag prefix
"
              ("\\" dired-do-ispell)
              ("(" dired-hide-details-mode)
              (")" dired-omit-mode)
              ("+" dired-create-directory)
              ("=" diredp-ediff)         ;; smart diff
              ("?" dired-summary)
              ("$" diredp-hide-subdir-nomove)
              ("A" dired-do-find-regexp)
              ("C" dired-do-copy)        ;; Copy all marked files
              ("D" dired-do-delete)
              ("E" dired-mark-extension)
              ("e" dired-ediff-files)
              ("F" dired-do-find-marked-files)
              ("G" dired-do-chgrp)
              ("g" revert-buffer)        ;; read all directories again (refresh)
              ("i" dired-maybe-insert-subdir)
              ("l" dired-do-redisplay)   ;; relist the marked or singel directory
              ("M" dired-do-chmod)
              ("m" dired-mark)
              ("O" dired-display-file)
              ("o" dired-find-file-other-window)
              ("Q" dired-do-find-regexp-and-replace)
              ("R" dired-do-rename)
              ("r" dired-do-rsynch)
              ("S" dired-do-symlink)
              ("s" dired-sort-toggle-or-edit)
              ("t" dired-toggle-marks)
              ("U" dired-unmark-all-marks)
              ("u" dired-unmark)
              ("v" dired-view-file)      ;; q to exit, s to search, = gets line #
              ("w" dired-kill-subdir)
              ("Y" dired-do-relsymlink)
              ("z" diredp-compress-this-file)
              ("Z" dired-do-compress)
              ("q" nil)
              ("." nil :color blue))
            (define-key dired-mode-map (kbd "C-h h") 'hydra-dired/body)
            ))

(put 'dired-find-alternate-file 'disabled nil)

;; ]

;; [ xml mode

;; handy: (nxml-balanced-close-start-tag-inline)

(require 'sgml-mode)

(defun xml-mode-setup ()
  (local-set-key (kbd "C-x <return>") 'sgml-close-tag)
  (setq nxml-slash-auto-complete-flag t))

(add-hook 'xml-mode-hook 'xml-mode-setup)

(add-to-list 'auto-insert-alist '("pom.xml$" . [ "pom.xml" ]))

(setq xml-file-patterns (list ".*\\.wadl'" ".*\\.xul\\'" ".*\\..rdf\\'" ".*\\.xsd\\'" ".*\\.wsdl\\'"))

(dolist (pattern xml-file-patterns)
  (add-to-list 'auto-mode-alist (cons pattern 'xml-mode)))

;; [ ant mode

(define-derived-mode ant-mode xml-mode
  "Ant"
  "Mode for editing ant build.xml files.")

(add-to-list 'auto-mode-alist (cons "build.xml" 'ant-mode))

;; ]

;; [ maven mode

(define-derived-mode maven-mode xml-mode
  "Maven"
  "Mode for editing maven pom.xml files.")

(add-to-list 'auto-mode-alist (cons "pom.xml" 'maven-mode))

(when (getenv "M2_HOME")
  (message
   (concat
    "Overwriting M2_HOME environment variable with custom value \""
    (expand-file-name mvn-home) "\"")))

(setenv "M2_HOME" (expand-file-name mvn-home))
(setenv "PATH"
        (concat (expand-file-name mvn-home) "/bin"
                path-separator
                (getenv "PATH")))

(defconst maven (concat (expand-file-name mvn-home) "/bin/mvn"))

(defun start-new-web-application (group-id artifact-id version-number)
  ;; TODO replace maven archetype with copy-file statements
  ;; include jquery, bootsrap and datatable by default
  (interactive "MGroup-id: \nMArtifact-id: \nMVersion-number: ")
  (let* ((project-path web-application-root)
         (live-buffer-name "*mvn*")
         (live-buffer (get-buffer-create live-buffer-name))
         (target-web-xml (concat project-path "/" artifact-id "/src/main/webapp/WEB-INF/web.xml"))
         (win-edges (window-edges))
         (this-window-x-min (nth 0 win-edges))
         (this-window-x-max (nth 2 win-edges))
         (ww (- this-window-x-max this-window-x-min)))
    (progn
      (when (not (file-exists-p project-path))
        (make-directory project-path t))
      (switch-to-buffer live-buffer)
      (when (string= (buffer-name) live-buffer-name)
        (erase-buffer))
      (cd project-path)
      (with-current-buffer live-buffer
        (term-mode))
      (call-process maven nil live-buffer-name t "archetype:generate"
                    (format "-DgroupId=%s" group-id)
                    (format "-DartifactId=%s" artifact-id)
                    (format "-Dversion=%s" version-number)
                    (format "-DarchetypeArtifactId=%s" "maven-archetype-webapp")
                    (format "-DinteractiveMode=%s" "false") ) 
      (when (file-exists-p artifact-id)
        (cd artifact-id))
      (goto-char (point-min))
      (when (search-forward "BUILD SUCCESS")
        (progn
          (neotree-dir project-path)
          (other-window 1)
          (split-window-below -8)
          (find-file (concat project-path "/" artifact-id "/pom.xml"))
          (copy-template "web-3.0.xml" target-web-xml
                         (list 
                          (list 'DISPLAY-NAME (format "%s %s" artifact-id version-number))))))
      (goto-char (point-max)) ) ) )

;; ]

;; ]

;; [ ivy,avy,ido&co

;; see http://oremacs.com/swiper/ for manual

(use-package ivy
  :config
  (setq ivy-fixed-height-minibuffer t
        ;; add recentf and bookmarks to ivy-switch-buffer completion candidates
        ivy-use-virtual-buffers t
        ivy-count-format "[%d|%d] - ")
  (ivy-mode)
  )

(use-package swiper
  ;; I really don't like it, but next time I change my mind about it
  ;; I just want to have this snippet of emacs lisp code available.
  :disabled
  :bind ("C-M-s" . swiper))

(use-package avy
  :bind ("C-S-j" . avy-goto-word-or-subword-1) )

;; ]

;; [ Where was I [editing text]?

(defun store-lot-position ()
  (when (not (or
              (window-minibuffer-p)
              (string-prefix-p "*" (buffer-name))
              (string-prefix-p " " (buffer-name))))
    (point-to-register ?z)))

(defun goto-lot-position ()
  (interactive)
  (jump-to-register ?z))

(add-hook 'post-self-insert-hook 'store-lot-position)

(global-set-key (kbd "C-c l") 'goto-lot-position)

;; ]

;; [ elpy python jedi

;; do not forget python dependencies: rope, jedi, flake8, importmagic,
;; autopep8, yapf, epc
;; to obtain dependencies use for example: "python -m pip install importmagic"

(use-package jedi)

(defun python-mode-setup ()
  "Personal python mode hook extension."
  (setq-local comment-auto-fill-only-comments t)
  (setq-local comment-multi-line t)
  (setq elpy-rpc-backend "jedi"
        python-indent-offset 4
        elpy-syntax-check-command "flake8")
  (local-set-key (kbd "M-#") 'comment-dwim)
  (jedi:setup)
  (when auto-complete-mode
    (auto-complete-mode -1))
  (setq company-backends '(elpy-company-backend))
  (company-mode))

;;  (pyvenv-activate "~/.emacs.d/.python-environments/default/"))

(use-package elpy
  :config
  (elpy-enable)
  (add-hook 'python-mode-hook 'python-mode-setup)
  (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
  (add-to-list 'auto-mode-alist '("\\.py2\\'" . python-mode))
  (add-to-list 'auto-mode-alist '("\\.py3\\'" . python-mode))
  (add-to-list 'auto-insert-alist '(".*\\.py3?$" . [ "template.py3" ] ) )
  (add-to-list 'auto-insert-alist '(".*\\.py2$" . [ "template.py" ] ) )
  (setq elpy-modules (quote
                      (elpy-module-eldoc
                       elpy-module-flymake
                       elpy-module-highlight-indentation
                       elpy-module-yasnippet
                       elpy-module-sane-defaults))
        elpy-rpc-backend nil
        elpy-rpc-error-timeout 15
        elpy-syntax-check-command "flake8") )

;; ]

;; [ restclient mode

(use-package restclient
  :config
  (add-to-list 'auto-mode-alist '("\\.rst\\'" . restclient-mode))
  (add-to-list 'auto-mode-alist '("\\.rcm\\'" . restclient-mode))
  (add-to-list 'auto-insert-alist '(".*\\.rst?$" . [ "template.rst" ] ) ))

;; ]

;; [ ispell mode

;; make sure ispell is in your path or add it here

;; (defconst ispell-path "")

;; (add-to-list 'exec-path ispill-path)


(setq ispell-program-name "aspell"
      ispell-personal-dictionary "~/.emacs.d/dict")

;; ]

;; [ html editing web mode

(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))


(use-package web-mode
  :config
  (add-hook 'web-mode-hook 'web-mode-setup))

(defun web-mode-setup ()
  (setq indent-tabs-mode nil
        web-mode-markup-indent-offset 4))

(defun html-post-processing ()
  "This method looks for a couple of key-strings and replaces them with some meaningful values."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "%TITLE%" nil t)
      (replace-match (replace-regexp-in-string (regexp-quote ".html") "" (buffer-name) 'fixedcase)))
    (goto-char (point-min))
    (when (re-search-forward "%CSSFILE%" nil t)
      (replace-match (replace-regexp-in-string (regexp-quote ".html") ".css" (buffer-name) 'fixedcase) 'fixedcase))
    (when (re-search-forward "%TESTEE%" nil t)
      (replace-match (replace-regexp-in-string (regexp-quote "-test.html") ".js" (buffer-name) 'fixedcase) 'fixedcase))
    (when (re-search-forward "%UNITTESTS%" nil t)
      (replace-match (replace-regexp-in-string (regexp-quote "-test.html") "-test.js" (buffer-name) 'fixedcase) 'fixedcase))))

(define-auto-insert '("\\.html\\'" . "HTML5 Skeleton")
  [ '(nil
      "<!DOCTYPE html>\n"
      "<html>\n"
      "<head>\n"
      "<meta charset=\"UTF-8\" />\n"
      "<title>%TITLE%</title>\n"
      "<script src=\"jquery-3.1.0.js\"></script>\n"
      "<script src=\"jquery-ui-1.12.0.js\"></script>\n"
      "<script src=\"subrx.js\"></script>\n"
      "<link rel=\"stylesheet\" href=\"%CSSFILE%\" />\n"
      "</head>\n"
      "<body>\n"
      "</body>\n"
      "</html>\n" )
    indent-buffer 
    html-post-processing ] )

;; this auto-insert must be defined *after* the one for *.html files
(define-auto-insert '("-test.html\\'" . "HTML5 Skeleton for QUnit test")
  [ '(nil
      "<!DOCTYPE html>\n"
      "<html>\n"
      "<head>\n"
      "<meta charset=\"UTF-8\" />\n"
      "<title>QUnit Testcase</title>\n"
      "<link rel=\"stylesheet\" href=\"qunit-2.0.1.css\" />\n"
      "</head>\n"
      "<body>\n"
      "<div id=\"qunit\"></div>\n"
      "<div id=\"qunit-fixture\"></div>\n"
      "<script src=\"qunit-2.0.1.js\"></script>\n"
      "<script src=\"%TESTEE%\"></script>\n"
      "<script src=\"%UNITTESTS%\"></script>\n"
      "</body>\n"
      "</html>\n" )
    indent-buffer 
    html-post-processing ] )

(defun html-project-post-processing (name)
  "This method looks for strings %CSSFILE% and %TITLE% and replaces them with some meaningful values ."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "%TITLE%" nil t)
      (replace-match name))
    (goto-char (point-min))
    (when (re-search-forward "%CSSFILE%" nil t)
      (replace-match (replace-regexp-in-string (regexp-quote ".html") ".css" (buffer-name) 'fixedcase) 'fixedcase))))

(defun start-web-project (name)
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
    (copy-file "~/.emacs.d/templates/jquery-3.1.0.js" (concat projectroot "/"))
    (copy-file "~/.emacs.d/templates/jquery-ui-1.12.0.css" (concat projectroot "/"))
    (copy-file "~/.emacs.d/templates/jquery-ui-1.12.0.js" (concat projectroot "/"))
    (copy-file "~/.emacs.d/templates/jquery.mobile-1.4.5.js" (concat projectroot "/"))
    (copy-file "~/.emacs.d/templates/qunit-2.0.1.js" (concat projectroot "/"))
    (copy-file "~/.emacs.d/templates/qunit-2.0.1.css" (concat projectroot "/"))
    (switch-to-buffer (concat name ".html"))
    (html-project-post-processing name)))

(global-set-key (kbd "C-c 4") #'start-web-project)

;; ]

;; [ php mode

(defun mp-php-online-help ()
  (interactive)
  (browse-url-firefox (concat 
                       "http://php.net/manual/de/"
                       (concat "function." (php-prepare-url (thing-at-point 'symbol)) ".php"))
                      t) )

(defun php-prepare-url (args)
  (replace-regexp-in-string "_" "-" args))

(defun php-preprocessor ()
  ;; does nothing yet
  t)

;; (add-to-list 'auto-insert-alist '(".*\\.php$" . [ "template.php" php-preprocessor ] ) )

(defun php-mode-extension ()
  (setq indent-tabs-mode nil
        c-basic-offset 4
        php-template-compatibility nil) )

(use-package php-mode
  :bind ("C-h o" . mp-php-online-help) 
  :config
  (add-hook 'php-mode-hook 'php-mode-extension) )

;; ]

;; [ shell script mode

(defun elisp-post-processor ()
  (interactive)
  (let ((match-found t))
    (progn
      (goto-char (point-min))
      (when (re-search-forward "_" nil t)
        (replace-match "") ) ) ) )

(add-to-list 'auto-insert-alist
             '(".*\\.sh$" . [ "template.sh" elisp-post-processor ] ) )

(defun shell-mode-setup ()
  (interactive) )

(add-hook 'shell-mode-hook 'shell-mode-setup)

;; ]

;; [ tramp

;; Note: use /ssh:hostname.domain:/path/to/file to access remote files.
;; For ease of use: Make sure that ssh access is granted via public key

(setq tramp-default-method "ssh"
      tramp-auto-save-directory "~/.emacs.d/auto-save/")

;; ]

;; [ makefiles

(add-to-list 'auto-insert-alist '("Makefile" . [ "Makefile" ] ))

;; ]

;; [ shared objects elf mode

(when (eq system-type 'gnu/linux)
  (use-package elf-mode
    :config
    (elf-setup-default) ) )

(add-to-list 'auto-mode-alist '("\.dll\\'" . hexl-mode))
(add-to-list 'auto-mode-alist '("\.exe\\'" . hexl-mode))

;; ]

;; [ eldoc

(setq eldoc-echo-area-use-multiline-p t)

(global-eldoc-mode)

;; ]

;; [ magit

(use-package magit
  :disabled
  :config

  (setq magit-completing-read-function 'ivy-completing-read)

  (defun magit-status-wrapper (arg)
    "Start magit. With prefix argument start magit in new frame."
    (interactive "P")
    (when arg
      (select-frame (make-frame '((name . "Magit")))))
    (call-interactively 'magit-status)
    (when arg
      (delete-other-windows) ) )

  (global-set-key (kbd "<f12>") 'magit-status-wrapper))

;; ]

;; [ version control

;; Emacs interface to version control can
;; be found under keymap C-x v (C-x v C-h for help)

;; some eye-candy:

(use-package git-gutter
  :config
  (setq git-gutter:update-interval 5) )

;; ]

;; [ messages mode buffer

(with-current-buffer "*Messages*"
  (visual-line-mode)
  t)

;; ]

;; [ gdb

(setq gdb-many-windows t)

;; ]

;; [ popup edit menu

;; Open edit menu on right-click

(use-package popup-edit-menu
  :config
  (global-set-key [mouse-3] (popup-edit-menu-stub))
  (setq popup-edit-menu-mode-menus-down-flag t)
  (easy-menu-add-item nil '("edit") ["--" nil t])
  (easy-menu-add-item nil '("edit") ["base64-encode" base64-encode-region t])
  (easy-menu-add-item nil '("edit") ["base64-decode" base64-decode-region t])
  (easy-menu-add-item nil '("edit") ["detach window" detach-window t]))

;; ]


;; [ auto-complete

(use-package ac-etags
  :config
  :disabled
  (ac-etags-setup))

(use-package ac-php
  :disabled)

(use-package auto-complete
  :config
  (require 'auto-complete)
  (require 'auto-complete-config)

  (setq
   ac-auto-show-menu 0.2
   ac-auto-start 0.2
   ac-comphist-file "~/.emacs.d/ac-comphist.dat"
   ac-dictionary-directories (quote ("~/.emacs.d/dictionaries/"))   ;; mode specific dictionaries
   ac-dictionary-files (quote ("~/.emacs.d/dictionaries/personal")) ;; personal dictionary
   ac-quick-help-delay 0.2
   ac-use-fuzzy t
   ac-dwim t
   ac-use-menu-map t
   ac-use-quick-help nil
   ac-user-dictionary (quote ("")))

  (global-set-key (kbd "C-c C-<SPC>") 'auto-complete)

  ;; Test for git gutter mode
  
  (define-key ac-menu-map "\C-n" 'ac-next)
  (define-key ac-menu-map "\C-p" 'ac-previous)
  (define-key ac-menu-map "\C-s" 'ac-isearch)
  (define-key ac-mode-map (kbd "C-x /") 'ac-complete-filename)

  (add-to-list 'ac-modes 'web-mode)

  (dolist (mode (list 'xml-mode 'web-mode 'sh-mode
                      'emacs-lisp-mode 'java-mode))
    (add-to-list 'ac-modes mode))

  ;; Just in case linum mode and ac interfer
  (ac-linum-workaround)

  (defun mp-ac-setup-for-emacs-lisp ()
    "Turn on auto-complete mode and set ac-sources for emacs-lisp-mode."
    (setq ac-sources '(ac-source-yasnippet
                       ac-source-filename
                       ac-source-files-in-current-dir))
    (auto-complete-mode t) )

  (add-hook 'emacs-lisp-mode-hook 'mp-ac-setup-for-emacs-lisp)

  (defun mp-ac-setup-for-c-mode ()
    "Turn on auto-complete mode and set ac-sources for c/c++-mode."
    (setq ac-sources  (list
                       'ac-source-yasnippet
                       'ac-source-etags
                       'ac-source-dictionary
                       'ac-source-words-in-same-mode-buffers))
    (auto-complete-mode 1) )

  (add-hook 'c-mode-hook 'mp-ac-setup-for-c-mode)
  (add-hook 'c++-mode-hook 'mp-ac-setup-for-c-mode)

  (defvar ac-source-classpath-cache nil)

  (defun ac-source-classpath-init ()
    (setq ac-source-classpath-cache (java-read-classes-from-classpath)))

  (defvar ac-source-classpath
    '((init . ac-source-classpath-init)
      (candidates . ac-source-classpath-cache)
      (prefix . "^import \\(.*\\)")))

  (defun mp-ac-setup-for-php-mode ()
    "Turn on auto-complete mode and set ac-sources for ac-php."
    (require 'ac-php)    
    (setq ac-sources  (list
                       'ac-source-yasnippet
                       'ac-source-php
                       'ac-source-words-in-same-mode-buffers) )
    (auto-complete-mode 1))

  (add-hook 'php-mode-hook 'mp-ac-setup-for-php-mode)

  (defun mp-ac-setup-for-shell-mode ()
    "Turn on auto-complete mode and set ac-sources for shell-mode."
    (define-key ac-mode-map (kbd "C-c /") 'ac-complete-filename) 
    (auto-complete-mode 1) )

  (add-hook 'shell-mode-hook 'mp-ac-setup-for-shell-mode) ) ;; end of use-package

;; ]

;; [ company

;; (require 'cl-lib)
;; (require 'company)

(use-package company-php
  :disabled
  :config
  (add-hook 'php-mode-hook
            '(lambda ()
               (require 'company-php)
               (setq company-backends '(company-ac-php-backend )))))

;;(use-package company-statistics)

(use-package company
  :config
  ;; see company-backends for company backends
  ;; (make-variable-buffer-local 'company-backends)
  (require 'company-template))

;; ]

;; [ ffip

(use-package find-file-in-project
  :config
  (add-to-list 'ffip-project-file "pom.xml"))

;; ]

;; [ compilation

(add-standard-display-buffer-entry "*compilation*")

(defun compilation-mode-setup ()
  ;; (next-error-follow-minor-mode)
  (local-set-key (kbd "q") 'kill-buffer)  
  (local-set-key (kbd "C-q") 'kill-buffer-and-window))

(add-hook 'compilation-mode-hook 'compilation-mode-setup)

(use-package auto-compile
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

(require 'ansi-color)

(defun colorize-compilation-buffer ()
  (let ((inhibit-read-only nil))
    (ansi-color-apply-on-region (point-min) (point-max))))

(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; ]

;; [ ensime scala

(use-package ensime
  :disabled
  :pin melpa
  :config
  (setq ensime-startup-notification nil))

(use-package scala-mode
  :disabled
  :pin melpa)

(use-package sbt-mode
  :disabled
  :pin melpa
  :interpreter
  ("scala" . scala-mode))

;; ]

;; [ origami

(use-package origami

  ;; https://github.com/gregsexton/origami.el

  :init

  (defvar mp/origami-open-map (make-sparse-keymap))
  (defvar mp/origami-close-map (make-sparse-keymap))
  (defvar mp/origami-map (make-sparse-keymap))

  :config

  (define-key mp/origami-map (kbd "u") 'origami-undo)
  (define-key mp/origami-map (kbd "r") 'origami-redo)
  (define-key mp/origami-map (kbd "f") 'origami-forward-fold-same-level)
  (define-key mp/origami-map (kbd "p") 'origami-backward-fold-same-level)
  (define-key mp/origami-close-map (kbd "a") 'origami-close-all-nodes)
  (define-key mp/origami-close-map (kbd "n") 'origami-close-node)
  (define-key mp/origami-close-map (kbd "s") 'origami-show-only-node)
  (define-key mp/origami-open-map (kbd "a") 'origami-open-all-nodes)
  (define-key mp/origami-open-map (kbd "n") 'origami-open-node)

  (defun mp/enable-origami()
    (local-set-key (kbd "M-+") 'origami-toggle-node)
    (global-unset-key (kbd "M-o"))
    (local-set-key (kbd "M-o") mp/origami-map)
    (define-key mp/origami-map (kbd "c") mp/origami-close-map)
    (define-key mp/origami-map (kbd "o") mp/origami-open-map)
    (origami-mode))

  (add-hook 'emacs-lisp-mode-hook 'mp/enable-origami)
  (add-hook 'xml-mode-hook 'mp/enable-origami)
  (add-hook 'nxml-mode-hook 'mp/enable-origami)
  (add-hook 'python-mode-hook 'mp/enable-origami)
  (add-hook 'web-mode-hook 'mp/enable-origami)
  (add-hook 'java-mode-hook 'mp/enable-origami))

;; ]

;; [ bookmarks

;; C-x r l    show bookmark list
;; C-x p s    save bookmars list

;; ]

;; [ logfile mode

(define-derived-mode logview-mode view-mode
  "logview"
  "Mode for viewing log files files.")

(add-to-list 'auto-mode-alist (cons "\\.log\\'" 'logview-mode))

(add-hook 'logview-mode-hook '(lambda ()
                                (stripe-buffer-mode)
                                (define-key logview-mode-map  (kbd "<") 'beginning-of-buffer)
                                (define-key logview-mode-map (kbd ">") 'end-of-buffer)))

;; ]

;; [ hydra

(defhydra hydra-global-org (:color blue
                                   :hint nil)
  "
Timer^^        ^Clock^         ^Capture^
--------------------------------------------------
s_t_art        _w_ clock in    _c_apture
 _s_top        _o_ clock out   _l_ast capture
_r_eset        _j_ clock goto
_p_rint
"
  ("t" org-timer-start)
  ("s" org-timer-stop)
  ;; Need to be at timer
  ("r" org-timer-set-timer)
  ;; Print timer value to buffer
  ("p" org-timer)
  ("w" (org-clock-in '(4)))
  ("o" org-clock-out)
  ;; Visit the clocked task from any buffer
  ("j" org-clock-goto)
  ("c" org-capture)
  ("l" org-capture-goto-last-stored))

(global-set-key (kbd "C-c h") 'hydra-global-org/body)
;; ]

;; [ Finalizer

(setq gc-cons-threshold (* 1024 1024 32)
      gc-cons-percentage 0.3)


(notify "[Emacs] init.el fully loaded")

;; ]

