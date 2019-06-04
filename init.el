;; -*- lexical-binding: t; -*-
;;
;;
;;; init.el --- Emacs initialization file
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

;; [ Prolog and custom set variables

(setq gc-cons-threshold (* 512 1024 1024)
      gc-cons-percentage 0.6)

(load "~/.emacs.d/customx.el")

(setq custom-file "~/.emacs.d/custom.el")

(if (file-exists-p custom-file)
    (load custom-file)
  (message "No custom.el file found."))

(add-to-list 'load-path "~/.emacs.d/elisp/")

;; load host specific settings

(require (intern (format "init-%s" (downcase 
                                    (getenv (if (eq system-type 'windows-nt)
                                                "COMPUTERNAME"
                                              (if (or
                                                   (eq system-type 'gnu/linux)
                                                   (eq system-type 'cygwin))
                                                  "HOSTNAME"
                                                "default")))))))

;; ]

;; [ packages

(require 'package)

(setq package-enable-at-startup nil
      package-user-dir "~/.emacs.d/packages/")

(setq package-archives (list 
                        '("melpa" . "http://melpa.org/packages/")
                        '("org" . "http://orgmode.org/elpa/")))
(setq package-check-signature nil)
(package-initialize)

(global-set-key (kbd "<f7>") #'package-list-packages)

(require 'use-package)

(setq use-package-verbose t
      use-package-always-ensure t
      load-prefer-newer t)

(global-set-key (kbd "<f4>") 'package-list-packages-no-fetch)

(add-hook 'package-menu-mode-hook 'hl-line-mode)

(defun mpx-get-hostname ()
  "Get hostname in a windows/linux/cygwin agnostic way."
  (interactive)
  (getenv (if (eq system-type 'windows-nt)
              "COMPUTERNAME"
            (if (or
                 (eq system-type 'gnu/linux)
                 (eq system-type 'cygwin))
                "HOSTNAME"
              "default"))))

(defconst prodigy-services-loader
  (format "prodigy-services-%s" (downcase 
                                 (mpx-get-hostname))))

;; ]

;; [ calendar

;; TODO: Want diary view entries be called after moving date marker in calendar
;; TODO: Want tab to jump from one entry to the next (shift-tab to jump back)

(global-set-key (kbd "C-<f4>") #'(lambda ()
                                   (interactive)
                                   "Toggle calendar visibility"
                                   (let ((calendar-window
                                          (get-buffer-window "*Calendar*")))
                                     (if calendar-window
                                         (delete-window calendar-window)
                                       (calendar)))))

(setq diary-display-function #'diary-fancy-display)

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

(setq ring-bell-function 'ignore
      auto-window-vscroll nil
      fill-column 72)

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

(require 'lisp-x)

(setq scroll-step 1
      scroll-conservatively 10000
      auto-window-vscroll nil
      delete-exited-processes t)

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.")

(global-set-key (kbd "M-Z") #'zap-up-to-char)

(setq fill-column 72)

;; (auto-fill-mode) TODO: put this into modes

;; ]

;; [ marks and navigation

;; use C-u C-SPC to pop mark positions
;; and C-x C-SPC to pop global mark position

(setq set-mark-command-repeat-pop t)

(global-set-key (kbd "C-c k") 'pop-global-mark)

;; ]

;; [ hydra

(defhydra hydra-global-org (:color blue
                                   :hint nil)
  "
Timer^^        ^Clock^       
-----------------------------
s_t_art        _w_ clock in  
 _s_top        _o_ clock out 
_r_eset        _j_ clock goto
_p_rint        _m_ clock mru
               _c_ cancel clock
               _d_ display clock"
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
  ("m" org-mru-clock-in)
  ("c" org-clock-cancel)
  ("d" org-clock-display))

;; ]

;; [ expand region

;; Very handy package. Sets er/try-expand-list on a per mode basis to
;; a list of functions. Each defun marks part of the buffer.
;; Incrementally largens the region by checking these functions.
;; To customize add function to er/try-expand-list in any mode hook.

(defun er/mark-table-cell ()
  "Mark a table cell"
  (interactive)
  (search-backward "|")
  (set-mark (+ 1 (point)))
  (search-forward "|" nil nil 2)
  (backward-char)
  (exchange-point-and-mark))

(defun er/mark-table-row ()
  "Mark a table row"
  (interactive)
  (beginning-of-line)
  (search-forward "|")
  (backward-char)
  (set-mark (point))
  (end-of-line)
  (search-backward "|")
  (forward-char)
  (exchange-point-and-mark))

(defun mpx-er-org-extensions ()
  (add-to-list 'er/try-expand-list 'er/mark-table-cell)
  (add-to-list 'er/try-expand-list 'er/mark-table-row)
  (define-key org-mode-map (kbd "C-h o") 'hydra-global-org/body))

(add-hook 'org-mode-hook 'mpx-er-org-extensions)

(use-package expand-region
  :config
  (global-set-key (kbd "C-v") 'er/expand-region)
  (global-set-key (kbd "C-M-v") 'er/contract-region) 
  (eval-after-load 'ng2-mode '(require 'ng2-mode-expansions)))


;; ]

;; [ server mode edit server

(setq server-use-tcp t
      server-host "localhost"
      server-port 39246)

(server-start)

;; Kill buffers when done (C-x #)
;; (add-hook 'server-done-hook (lambda nil (kill-buffer nil)))

;; ]

;; [ highlighting

;; M-s h l or highlight-lines-matching-regexp
;; Highlights all lines matching a regular expression

;; M-s h p or highlight-phrase
;; Highlights everything matching a phrase

;; M-s h r or highlight-regexp
;; Highlights everything matching a regular expression

;; M-s h u or unhighlight-regexp
;; Deletes the highlighter under point

;; M-s h w or hi-lock-write-interactive-patterns
;; Inserts a list of Hi-Lock patterns into the buffer

;; M-s h f or hi-lock-find-patterns

(defhydra highlighting-hydra (:color blue :hint nil)
  "
Highlight                  Unhighlight
------------------------------------------------
_l_ines matching regexp      regular _e_xpression
_p_hrase                 
_r_egular expression
lines containing _E_rror     lines containing E_r_ror

"
  ("l" highlight-lines-matching-regexp)
  ("p" highlight-phrase)
  ("r" highlight-regexp)
  ("e" unhighlight-regexp)
  ("E" (call-interactively '(lambda ()
                              (interactive)
                              (highlight-lines-matching-regexp "ERROR" 'hi-pink))))
  ("r" (call-interactively '(lambda ()
                              (interactive)
                              (unhighlight-regexp "^.*ERROR.*$")))))

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

(global-set-key (kbd "C-s") 'isearch-forward)
(global-set-key (kbd "C-S-s") 'isearch-forward-symbol-at-point)

;; ]

;; [ encoding systems

(set-language-environment "UTF-8")

;; (modify-coding-system-alist 'file ".*cygwin.*\.sh" 'utf-8-unix)
;; (modify-coding-system-alist 'file ".*cygwin.*\.py[23]?" 'utf-8-unix)

;; ]

;; [ global appearence

(use-package fill-column-indicator)

;; how to set fonts:
;; https://www.emacswiki.org/emacs/SetFonts

(defun mpx-set-fonts (family fontsize)
  (interactive)
  "Set prefered fonts."
  (when (eq system-type 'windows-nt)
    (progn
      (dolist (face '(org-level-1
                      org-level-2
                      org-level-3
                      org-level-4
                      org-level-5))
        (set-face-attribute face nil
                            :family family
                            :height fontsize
                            :weight 'normal
                            :width 'normal))
      (set-face-attribute 'default nil
                          :family family
                          :height fontsize
                          :weight 'normal
                          :width 'normal)
      (set-face-attribute 'mode-line nil
                          :family family
                          :height fontsize
                          :weight 'normal
                          :width 'normal))))

(run-with-timer 15 nil #'(lambda ()
                           (mpx-set-fonts prefered-font-family 100)))

(use-package stripe-buffer)

(use-package theme-changer
  :config
  (change-theme '(material-light) '(material))
  (require 'hl-line))

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

(set-default 'frame-title-format '(:eval 
                                   (if (boundp 'mpx-promt-project-root)
                                       (format "Emacs: %%b [%s]"
                                               (car
                                                (cdr
                                                 (reverse
                                                  (split-string mpx-promt-project-root "/")))))
                                     (format "Emacs: %%b"))))

(when window-system
  (progn
    (when (eq system-type 'windows-nt)
      (horizontal-scroll-bar-mode -1))
    (tool-bar-mode -1)
    (menu-bar-mode 1)
    (tooltip-mode -1)
    (scroll-bar-mode -1)))

(use-package volatile-highlights
  :init
  :disabled
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

(use-package moody
  :disabled
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(use-package minions
  :config
  (minions-mode 1))

(which-function-mode)

(setq which-func-unknown "∅")

(defconst mpx-standard-mode-line-format (list
                                         "→ " (propertize
                                               "%b"
                                               'face 'mode-line-buffer-id
                                               'help-echo (when (fboundp 'projectile-project-root)
                                                            (concat "Projectile root: "
                                                                    (let ((pr (projectile-project-root)))
                                                                      (if pr
                                                                          (format "%s" pr)
                                                                        "undefined")))))
                                         " | "
                                         mode-line-position "|";; ⊕
                                         vc-mode " | "
                                         minions-mode-line-modes
                                         " | " mode-line-misc-info
                                         "| "
                                         mode-line-mule-info
                                         mode-line-client
                                         mode-line-modified
                                         " |"))

(setq-default mode-line-format mpx-standard-mode-line-format)

;; ]

;; [ backup & auto-save

(setq auto-save-default nil
      auto-save-file-name-transforms nil
      auto-save-interval 50
      auto-save-list-file-prefix nil
      auto-save-visited-file-name t
      backup-by-copying t
      backup-directory-alist nil
      delete-old-versions t
      delete-old-versions t
      kept-new-versions 10
      make-backup-files nil
      vc-make-backup-files nil
      version-control t)

;; ]

;; [ ibuffer

;; (define-ibuffer-sorter ibuffer-sorter-alphabetic-by-path
;;   "Sort the buffers by their filename (including path).
;; Ordering is lexicographic."
;;   (:description "buffer name")
;;   (string-lessp
;;    (buffer-file-name (car a))
;;    (buffer-file-name (car b))))

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

(defun ibuffer-show-file-path ()
  (interactive)
  (let ((buf (ibuffer-current-buffer))
        (info nil))
    (progn
      (if  buf
          (when (file-exists-p (buffer-file-name buf))
            (with-temp-buffer
              (let* ((filename (buffer-file-name buf))
                     (file-directory (file-name-directory filename))
                     (just-filename (file-name-nondirectory filename)))
                (setq input file-directory))))
        (setq input "Buffer is not backed by a file"))
      (message input))))

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
    (:name "Path" :inline nil)
    (let* ((result nil)
           (buf-file-name (buffer-file-name buffer))
           (dir-name (if (and buf-file-name
                              (file-exists-p buf-file-name))
                         (file-name-directory buf-file-name)
                       nil)))
      (if dir-name
          (let* ((result (if (> (length dir-name) 47)
                             (concat "⋯" (substring dir-name (- (length dir-name) 47)))
                           dir-name)))
            (if result
                result
              buffer-mode))
        "")))

  (setq ibuffer-show-empty-filter-groups nil
        ;;      ibuffer-formats '(( mark (git-status-mini) modified read-only "|"
        ibuffer-formats '(( mark (additional-info  48 48 :right :elide)
                                 "|" (name 24 24 :left :elide)
                                 "|" (mode 16 16 :left :elide)
                                 "|" (size 9 -1 :left)
                                 "[" modified read-only "] " 
                                 ) ;; filename-and-process)
                          (mark " "
                                (name 16 -1)
                                " " filename)))

  (defun ibuffer-mode-hook-extender ()
    (ibuffer-auto-mode 0) ;; auto updates disabled
    (hl-line-mode)
    (define-key ibuffer-mode-map (kbd "M-<RET>") 'ibuffer-visit-buffer-other-frame)
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
         (description (read-from-minibuffer "Description? "))
         (maintainer (format "%s (%s)" user-full-name user-mail-address))
         (author user-full-name)
         (version (read-from-minibuffer "Version? "))
         (url (read-from-minibuffer "Url? "))
         (keywords (read-from-minibuffer "Keywords? "))
         (dependencies (read-from-minibuffer "Dependencies? "))
         (timestamp (current-time-string)))
    (progn 
      (goto-char (point-min))
      (while (search-forward "DESCRIPTION" nil t)
        (replace-match description t) )
      (goto-char (point-min))
      (while (search-forward "AUTHOR" nil t)
        (replace-match author t) )
      (goto-char (point-min))
      (while (search-forward "TIMESTAMP" nil t)
        (replace-match timestamp t) )
      (goto-char (point-min))
      (while (search-forward "VERSION" nil t)
        (replace-match version t) )
      (goto-char (point-min))
      (while (search-forward "KEYWORDS" nil t)
        (replace-match keywords t) )
      (goto-char (point-min))
      (while (search-forward "FILENAME" nil t)
        (replace-match filename t) )
      (goto-char (point-min))
      (while (search-forward "DEPENDENCIES" nil t)
        (replace-match dependencies t) ) ) ) )

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
  (setq-local imenu-create-index-function 'imenu-default-create-index-function))

(defun byte-compile-current-buffer ()
  (interactive)
  (byte-compile-file (buffer-file-name)))

(defun emacs-lisp-mode-setup ()
  (when (string= (buffer-name) "init.el")
    (dotemacs-mode-hook))
  (hl-line-mode)
  (local-set-key (kbd "C-;") 'comment-dwim)
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


(defun mpx-browse-kill-ring (prefix-arg)
  (interactive "P")
  (let ((tmp-kill-ring nil))
    (if prefix-arg
        (setq tmp-kill-ring kill-ring)
      (setq tmp-kill-ring (mapcar 'string-trim kill-ring))
      (insert (completing-read "Pick an element: " (seq-filter 'length tmp-kill-ring))))))

(global-set-key (kbd "C-M-y") 'mpx-browse-kill-ring)

;; ]

;; [ yasnippet

(defun yasx-downcase-initial-char (text)
  "Return copy of text with first character downcased."
  (concat (downcase (substring text 0 1)) (substring text 1)))

(defun yasx-capitalize-initial-char (text)
  "Return copy of text with first character downcased."
  (concat (capitalize (substring text 0 1)) (substring text 1)))

(use-package yasnippet
  :config
  (defconst yasnippet-my-snippets-dir "~/.emacs.d/snippets/")
  ;; Add snippet directory to auto-mode-alist
  ;; so that files are opened in snipped-mode
  (dolist (snippet-dir yas-snippet-dirs)
    (add-to-list 'auto-mode-alist (cons (concat ".*" snippet-dir ".*") 'snippet-mode)))
  (yas-load-directory yasnippet-my-snippets-dir)
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
  (add-hook 'ant-mode-hook 'yas-minor-mode)
  (add-hook 'sh-mode-hook 'yas-minor-mode))
;; [ hydra

(defhydra hydra-yasnippets (:color blue
                                   :hint nil)
  "
Yasnippet - Yasnippet - Yasnippet - Yasnippet - Yasnippet
---------------------------------------------------------
_n_ew snippet for current mode    _e_dit existing snippet
_r_eload snippets                 _i_nsert snippet
"
  ("n" yas-new-snippet)
  ("e" yas-visit-snippet-file)
  ;; Need to be at timer
  ("r" yas-reload-all)
  ("i" yas-insert-snippet))

(define-key yas-minor-mode-map (kbd "C-h y") 'hydra-yasnippets/body)

;; ]

;; [ tide

(use-package tide)

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
  (add-to-list 'auto-mode-alist '("\\.es\\'" . js2-mode))
  (add-hook 'js2-mode-hook 'js2-mode-setup))

;; ]

;; [ session management

;; save and restore open buffers

(desktop-save-mode)

;; ]

;; [ org mode

(add-hook 'org-agenda-mode-hook 'hl-line-mode)

;; (defadvice org-agenda-redo (after fit-window-after-agenda-redo activate)
;;  (fit-window-to-buffer))

(defadvice org-agenda (after fit-agenda-buffer-to-window activate) 
  (fit-window-to-buffer))

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

(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

(use-package pretty-symbols)

(defun org-mode-setup ()
  ;;  "Stop the org-level headers from increasing in height relative to the other text."

  (push '("[ ]" .  "☐") prettify-symbols-alist)
  (push '("[X]" . "☑" ) prettify-symbols-alist)
  (push '("[-]" . "❍" ) prettify-symbols-alist)
  (prettify-symbols-mode)
  (org-hide-block-all)
  (org-clock-persistence-insinuate)
  ;;  (flyspell-mode)
  (add-to-list 'org-file-apps '("\\.png\\'" . default))
  (company-mode -1) ;; disabled, since it looks broken
  ;; (org-bullets-mode)
  (setq org-agenda-span 7
        org-agenda-comact-blocks t
        org-agenda-show-all-dates t
        org-agenda-include-diary t
        org-babel-python-command python-interpreter
        org-clock-into-drawer t
        org-clock-persist 'history
        org-clock-history-length 23
        org-clock-in-resume t
        org-clock-out-remove-zero-time-clocks t
        org-clock-out-when-done t        
        org-clock-persist t
        org-clock-persist-query-resume nil
        org-clock-auto-clock-resolution (quote when-no-clock-is-running)
        org-clock-report-include-clocking-task t
        org-confirm-babel-evaluate nil
        org-log-note-clock-out t
        org-default-notes-file "~/org/organizer"
        org-directory "~/org/"
        org-ellipsis "…"
        org-log-done (quote note)
        org-log-into-drawer t
        org-special-ctrl-a/e t
        org-special-ctrl-k t
        org-src-fontify-natively t
        org-todo-keywords '((sequence "TODO" "DOING" "DONE" "|" "IDEA")))

  (local-set-key (kbd "<return>") #'org-return-indent)
  (local-set-key (kbd "C-'") #'imenu)

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
     (shell . t) ) ) )

(org-clock-persistence-insinuate)

(defconst org-capture-file "~/org/organizer" "Location of file where to store org capture notes.")

;; capture templates are documented at
;; http://orgmode.org/manual/Capture.html

(setq org-capture-templates mpx-host-local-org-capture-templates)

(add-hook 'org-mode-hook 'org-mode-setup)

;; ]

;; [ imenu

(setq imenu-auto-rescan t
      imenu-use-popup-menu nil
      imenu-space-replacement " "
      imenu-sort-function 'imenu--sort-by-name) ;; sorts only mouse menu

(global-set-key (kbd "C-'") 'imenu)

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
    (local-set-key (kbd "n") 'prodigy-next-line)
    (local-set-key (kbd "p") 'prodigy-previous-line))

  (add-hook 'prodigy-mode-hook 'prodigy-mode-setup)
  (global-set-key (kbd "<f5>") #'prodigy-setup-frame)
  (require (string-to-symbol prodigy-services-loader)))

;; ]

;; [

(use-package find-file-in-project)

;; ]

;; [ sr-speedbar neotree

(use-package neotree
  :bind( "<f6>" . neotree-toggle))

(use-package sr-speedbar
  :disabled
  :bind ("<f6>" . sr-speedbar-toggle)
  :config
  (setq speedbar-fetch-etags-arguments (quote
                                        ("--declarations" "-D" "-I" "-o" "-"))
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

;; (modify-frame-parameters nil (list '( name . "Emacs" )

(defun message-no-properties (text)
  (set-text-properties 0 (length text) nil text)
  (message (string-trim text)))

;; ]

;; [ xref and tags

;; TODO Change window handling for xref popups

;; (add-standard-display-buffer-entry "*xref*")

(setq tags-revert-without-query t)

;; use C-o in xref buffer to display match in other window
;; use TAB in xref buffer to go to location and close xref buffer
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
  (add-to-list 'er/try-expand-list 'mark-def-undef-block)
  (add-to-list 'er/try-expand-list 'mark-if-endif-block)
  (local-set-key (kbd "C-c C-c") 'compile)
  (local-set-key (kbd "C-h c") 'c++-help)
  (local-set-key (kbd "C-M-j") 'imenu)
  (local-set-key (kbd "C-x a") 'align-regexp))

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

(defun mpx-delete-frame ()
  "Delete frame. If frame is only frame show friendly message."
  (interactive)
  (condition-case nil
      (delete-frame)
    (error
     (message "This frame is the last remaining frame. It cannot be deleted."))))

(defconst mpx-ui-ops-use-hydra 't)

(if mpx-ui-ops-use-hydra
    (progn
      (defhydra ui-ops-hydra (:color blue :hint nil)
        "
Windows        Frames     Buffers         Moving
----------------------------------------------------------
Split _r_ight    De_t_ach     _s_wap            j← k→
Split _l_eft     _d_elete     r_o_tate          h↑ l↓
_f_it to Buffer  _m_ake       _e_diff
                          _R_evert
                          _M_inions Menu"
        ("d" mpx-delete-frame)
        ("e" ediff-this)
        ("f" fit-window-to-buffer)
        ("g" windmove-right)
        ("l" split-window-left)
        ("m" make-frame)
        ("o" rotate-windows)
        ("r" split-window-right)
        ("R" revert-buffer)
        ("s" swap-buffers)
        ("t" detach-window)
        ("<left>" windmove-left)
        ("<up>" windmove-up)
        ("<down>" windmove-down)
        ("<right>" windmove-right)
        ("j" windmove-left)
        ("h" windmove-up)
        ("l" windmove-down)
        ("k" windmove-right)
        ("M" minions-minor-modes-menu))
      (global-set-key (kbd "C-h C-u") 'ui-ops-hydra/body))
  (progn
    (defvar windows-ops-keymap
      (make-sparse-keymap)
      "Keymap for windmove commands.")
    (global-set-key (kbd "C-x w") windows-ops-keymap)
    (define-key windows-ops-keymap (kbd "s") #'swap-buffers)
    (define-key windows-ops-keymap (kbd "r") #'rotate-windows)
    (define-key windows-ops-keymap (kbd "<f1>") #'detach-window)
    (define-key windows-ops-keymap (kbd "<f2>") #'make-frame)
    (define-key windows-ops-keymap (kbd "<f3>") #'mpx-delete-frame)
    (define-key windows-ops-keymap (kbd "C-x 2") #'split-window-below)
    (define-key windows-ops-keymap (kbd "C-x 3") #'split-window-right)
    (define-key windows-ops-keymap (kbd "C-x =") #'fit-window-to-buffer)))

;; ]

;; [ ediff

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

(global-set-key (kbd "C-c TAB") #'whitespace-mode)

;; ]

;; [ man info help mode

;; To read plain info file from the filesystem: "C-u C-h i"

;; TODO: There is a python info page lying around in this directcory,
;; but info does not find it

(add-to-list 'Info-default-directory-list "~/.emacs.d/info")

(defun Info-mode-setup ()
  "Personal info mode hook."
  (define-key Info-mode-map (kbd "C-M-s") 'isearch-forward-symbol-at-point))

(add-hook 'Info-mode-hook 'Info-mode-setup)

;; Have info open new buffer on each invocation
(global-set-key (kbd "C-h i") #'(lambda ()
                                  (interactive)
                                  (select-frame (make-frame))
                                  (let* ((info-new-buffer-name (generate-new-buffer-name "*info*"))
                                         (info-buffer nil))
                                    (progn
                                      (setq info-buffer (get-buffer-create info-new-buffer-name))
                                      (info nil info-buffer)))))

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

(require 'man)

(defun mpx-help-mode-setup ()
  (local-set-key (kbd "q") 'kill-buffer-and-window))

(add-hook 'help-mode-hook 'mpx-help-mode-setup)

(add-to-list 'display-buffer-alist
             `(,(regexp-quote "*Help*")
               (display-buffer-reuse-window
                display-buffer-at-bottom
                display-buffer-in-side-window)
               (reusable-frames . visible)
               (side            . bottom)
               (window-height   . 0.3)))


;; ]

;; [ java mode

(require 'java-x)

(defun mpx-java-mode-smart-newline ()
  (interactive)
  (cond ((eq font-lock-doc-face (face-at-point))
         (progn
           (newline-and-indent)
           (insert "* ")))
        (t (newline-and-indent))))

(defun mpx-java-mode-smart-newline-finish-javadoc ()
  (interactive)
  (cond ((eq font-lock-doc-face (face-at-point))
         (progn
           (newline-and-indent)
           (insert "*/")
           (newline-and-indent)))
        (t (newline-and-indent))))

(defun pretty-print-list (tlist)
  "Get simple string representation for TLIST."
  (if (null tlist)
      "nil"
    (reduce '(lambda (a b) (format "%s, %s" a b)) tlist)))

(defun pretty-print-list-line-by-line (tlist)
  "Get simple string representation for TLIST."
  (if (null tlist)
      "nil"
    (concat "\n    - " (reduce '(lambda (a b) (format "%s %s %s"  a "\n    - " b)) tlist))))

(use-package jtags
  :config
  (add-hook 'java-mode-hook 'jtags-mode))

;; Add tags files for jdk if it exists

(defun mpx-add-jdk-tags-to-tags-files-list ()
  (when (not (null (concat jdk-location "/src")))
    (let ((jdk-src (concat jdk-location "/src/")))
      (progn
        (message "Loading jdk TAGS for jdk")
        (mapc #'(lambda (file)
                  (when (and
                         (file-directory-p (concat jdk-src file))
                         (file-exists-p (concat jdk-src file "/TAGS")))
                    (progn
                      (mpx-add-tags-file (concat jdk-src file "/TAGS")))))
              (directory-files-recursively jdk-src "^.*TAGS$"))))))

;;(setq tags-revert-without-query 't)

(use-package javadoc-lookup
  :disabled
  :config
  (javadoc-add-roots (concat jdk-location "/docs"))

  (dolist (root host-local-javadoc-roots)
    (javadoc-add-roots root))

  (setq browser-url-browser-function 'browse-url-chromium))

(defun java-mode-setup()
  (local-set-key (kbd "C-h t") 'mpx-help-on-tags)
  ;; Treat Java 1.5 @-style annotations as comments.
  (setq c-comment-start-regexp "(@|/(/|[*][*]?))")
  (modify-syntax-entry ?@ "< b" java-mode-syntax-table)
  (mpx-load-closest-tags-file)
  ;; mark line point is in
  (hl-line-mode)
  (display-line-numbers-mode)
  ;;
  (setq-local comment-auto-fill-only-comments t)
  (setq-local comment-multi-line t)
  ;; navigate camel cased words
  (subword-mode)
  ;; Get help with javadoc (need to have this working for non-jdk classes too
  (local-set-key (kbd "C-h j") 'javadoc-lookup)
  ;; Turn on auto-complete mode and set ac-sources
  (auto-complete-mode 1)
  (setq ac-sources '(ac-source-yasnippet
                     ac-source-classpath
                     ac-source-dictionary
                     ac-source-words-in-same-mode-buffers)))

(add-hook 'java-mode-hook 'java-mode-setup)

;; preprocessor for interactively generating files from templates
(add-to-list 'auto-insert-alist '(".*\\.java$" . [ "template.java" java-preprocessor] ) )

;; ]

;; [ dired

(setq dired-dwim-target t) ;; Make dired guess the target of a copy operation

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
            (hl-line-mode)
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
            (define-key dired-mode-map (kbd "C-h C-h") 'hydra-dired/body)
            ))

(put 'dired-find-alternate-file 'disabled nil)

;; ]

;; [ xml mode

;; handy: (nxml-balanced-close-start-tag-inline)

(require 'sgml-mode)

(defun xml-mode-setup ()
  (local-set-key (kbd "C-x <return>") 'sgml-close-tag)
  (setq nxml-slash-auto-complete-flag t)
  (local-set-key (kbd ">" 'mpx-magic->)))

(add-hook 'xml-mode-hook 'xml-mode-setup)

(add-to-list 'auto-insert-alist '("pom.xml$" . [ "pom.xml" ]))

(setq xml-file-patterns (list ".*\\.wadl'" ".*\\.xul\\'" ".*\\..rdf\\'" ".*\\.xsd\\'" ".*\\.wsdl\\'" ".*\\.grxml\\'"))

(dolist (pattern xml-file-patterns)
  (add-to-list 'auto-mode-alist (cons pattern 'xml-mode)))

(defun xml-pretty-print-buffer()
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "><" nil t)
    (replace-match ">
<"))
  (indent-buffer))

(defun mpx-magic-> ()
  "Add closing tag for lastly entered tag."
  (interactive)
  (let ((beg 0)
        (end 0)
        (cbp (point))
        (tag nil))
    (progn
      (save-excursion
        (search-backward "<")
        (forward-char)
        (when (not (string= (buffer-substring-no-properties (point) (+ 1 (point)))
                            "/"))
          (progn
            (setq beg (point)
                  end (re-search-forward "[^ >]*"cbp t))
            (when (not (and (null beg) (null end)))
              (setq tag (buffer-substring-no-properties beg end))))))
      (if tag
          (progn
            (insert ">")
            (save-excursion
              (insert (format "</%s>" tag))))
        (progn
          (insert ">")
          (message "No tag found"))))))

(setq nxml-child-indent 4
      nxml-attribute-indent 4)

;; [ ant mode

(define-derived-mode ant-mode xml-mode
  "Ant"
  "Mode for editing ant build.xml files.")

(add-to-list 'auto-mode-alist (cons "build.xml" 'ant-mode))

;; ]

;; [ maven mode

(define-derived-mode maven-mode nxml-mode
  "Maven"
  "Mode for editing maven pom.xml files.")

(add-to-list 'auto-mode-alist (cons "pom.xml" 'maven-mode))

(defconst mpx-maven-output-buffer-name "*mvn*")

(defun mpx-maven-output-buffer ()
  (interactive)
  (when (bufferp mpx-maven-output-buffer-name)
    (let ((kill-buffer-query-functions nil)
          (kill-buffer-hook nil))
      (kill-buffer mpx-maven-output-buffer-name)))
  (get-buffer-create mpx-maven-output-buffer-name))

(defun mpx-maven (cmd)
  "Effectifly call \"mvn CMD\""
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (call-process "mvn" nil (mpx-maven-output-buffer) t cmd)
    (display-buffer mpx-maven-output-buffer-name)))

(add-to-list 'display-buffer-alist
             `(,mpx-maven-output-buffer-name
               (display-buffer-at-bottom display-buffer-reuse-window display-buffer-in-side-window)
               (reusable-frames . visible)
               (window-height . 15)))

(defun mpx-maven-clean ()
  (interactive)
  (mpx-maven "clean"))

(defun mpx-maven-validate ()
  (interactive) (mpx-maven "validate"))

(defun mpx-maven-compile ()
  (interactive) (mpx-maven "compile"))

(defun mpx-maven-test ()
  (interactive) (mpx-maven "test"))

(defun mpx-maven-package ()
  (interactive) (mpx-maven "package"))

(defun mpx-maven-verify ()
  (interactive) (mpx-maven "verify"))

(defun mpx-maven-install ()
  (interactive) (mpx-maven "install"))

(defun mpx-maven-deploy ()
  (interactive) (mpx-maven "deploy"))

;; ]

;; [ ivy, avy, ido & co

(use-package ivy
  :config
  (setq ivy-fixed-height-minibuffer t
        ;; add recentf and bookmarks to ivy-switch-buffer completion candidates
        ivy-use-virtual-buffers t
        ivy-count-format "[%d|%d] - ")

  (ivy-mode))

;; see http://oremacs.com/swiper/ for manual
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
  (local-set-key (kbd "M-#") 'comment-dwim)
  (jedi:setup)
  (auto-complete-mode 1)
  (company-mode -1)
  (setq ac-sources (list 'ac-source-jedi-direct 'ac-source-words-in-same-mode-buffers)))

(use-package elpy
  :config
  (elpy-enable)
  (setq python-indent-offset 4
        elpy-rpc-backend "jedi"
        elpy-syntax-check-command "flake8")
  (add-hook 'python-mode-hook 'python-mode-setup)
  (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
  (add-to-list 'auto-mode-alist '("\\.py2\\'" . python-mode))
  (add-to-list 'auto-mode-alist '("\\.py3\\'" . python-mode))
  (add-to-list 'auto-insert-alist '(".*\\.py3?$" . [ "template.py3" ] ) )
  (add-to-list 'auto-insert-alist '(".*\\.py2$" . [ "template.py" ] ) )
  (setq elpy-modules (quote
                      (elpy-module-eldoc
                       ;;                       elpy-module-flymake
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

(defvar mpx-restclient-bookmarks-file "~/.emacs.d/restclient-bookmarks.txt")

(defvar mpx-restclient-bookmarks nil)

(defun mpx-load-restclient-bookmarks ()
  "Retrieve list of restclient bookmarks.
TODO: Have more generic functions read-list-from-file, store-list-to-file
and then do re-implement restclient-bookmarks and project files with this."
  (let ((bookmarks-text nil))
    (with-temp-buffer (insert-file-contents-literally mpx-restclient-bookmarks-file)
                      (setq bookmarks-text (buffer-substring-no-properties (point-min) (point-max))))
    (setq mpx-restclient-bookmarks (split-string bookmarks-text)))
  mpx-restclient-bookmarks)

(defun mpx-add-restclient-bookmark (url)
  (interactive "MAdd url: ")
  "Add ROOT-FOLDER to list of known bookmarks."
  (message (concat "Adding " url " to list of restclient bookmarks"))
  (let ((bookmarks (mpx-load-restclient-bookmarks)))
    (add-to-list 'bookmarks url)
    (mpx-restclient-save-bookmarks bookmarks)))

(defun mpx-restclient-save-bookmarks (bookmarks)
  (interactive (list mpx-restclient-bookmarks))
  (with-temp-buffer
    (progn
      (dolist (bookmark bookmarks)
        (progn
          (insert bookmark)
          (newline)))
      (write-file mpx-restclient-bookmarks-file))))
;; ]

;; [ markdown

(use-package markdown-mode)

;; ]

;; [ angular2 html editing web mode

(require 'webdev-tools)

(use-package web-mode
  :config
  (add-hook 'web-mode-hook 'web-mode-setup)
  (add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode)))

(defun web-mode-setup ()
  (local-set-key (kbd ">") 'mpx-magic->)
  (yas-minor-mode)
  (hl-line-mode)
  (setq indent-tabs-mode nil
        web-mode-markup-indent-offset 4))

;; Make sure that this is loaded after web-mode if you prefer
;; ng2-html-mode over web-mode in angular projects
(use-package ng2-mode
  :config
  ;;  (require 'angular-helpers)

  (add-hook 'ng2-ts-mode-hook #'(lambda ()
                                  (hl-line-mode)
                                  (display-line-numbers-mode)
                                  (auto-complete-mode)
                                  (setq ac-sources '(ac-source-yasnippet
                                                     ac-source-filename
                                                     ac-source-files-in-current-dir))
                                  (setq c-basic-offset 4)))

  (add-hook 'ng2-html-mode-hook #'(lambda ()
                                    (local-set-key (kbd ">") 'mpx-magic->)
                                    (hl-line-mode))))

(add-to-list 'auto-mode-alist '("\\.component.html\\'" . ng2-html-mode))

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
        php-template-compatibility nil)
  (local-set-key (kbd "C-h o") #'mp-php-online-help))

(use-package php-mode
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

(defun re-seq (regexp string idx)
  "Get a list of all REGEXP matches in STRING."
  (save-match-data
    (let ((pos 0)
          matches)
      (while (string-match regexp string pos)
        (push (match-string idx string) matches)
        (setq pos (match-end 0)))
      matches)))

(defun shx-find-variables ()
  "Extract list of variable names from shell script"
  (let ((undeclared (re-seq "^\\([0-9a-zA-Z_]+\\)=.*$" (buffer-substring-no-properties (point-min) (point-max)) 1))
        (declared (re-seq "^declare.*[[:space:]]\\([0-9a-zA-Z_]+\\)\\(=.*\\)?$" (buffer-substring-no-properties (point-min) (point-max)) 1)))
    (sort (append declared undeclared) 'string<)))


(defconst shx-bash-builtin-variables (list "BASH" "BASHOPTS" "BASHPID" "BASH_ALIASES" "BASH_ARGC" "BASH_ARGV" "BASH_CMDS" "BASH_COMMAND" "BASH_EXECUTION_STRING" "BASH_LINENO" "BASH_LOADABLES_PATH" "BASH_REMATCH" "BASH_SOURCE" "BASH_SUBSHELL" "BASH_VERSINFO" "BASH_VERSION" "COMP_CWORD" "COMP_KEY" "COMP_LINE" "COMP_POINT" "COMP_TYPE" "COMP_WORDBREAKS" "COMP_WORDS" "COPROC" "DIRSTACK" "EUID" "FUNCNAME" "GROUPS" "HISTCMD" "HOSTNAME" "HOSTTYPE" "LINENO" "MACHTYPE" "MAPFILE" "OLDPWD" "OPTARG" "OPTIND" "OSTYPE" "PIPESTATUS" "PPID" "PWD" "RANDOM" "READLINE_LINE" "READLINE_POINT" "REPLY" "SECONDS" "SHELLOPTS" "SHLVL" "UID" "BASH_COMPAT" "BASH_ENV" "BASH_XTRACEFD" "CDPATH" "CHILD_MAX" "COLUMNS" "COMPREPLY" "EMACS" "ENV" "EXECIGNORE" "FCEDIT" "FIGNORE" "FUNCNEST" "GLOBIGNORE" "HISTCONTROL" "HISTFILE" "HISTIGNORE" "HISTSIZE" "HISTTIMEFORMAT" "HOME" "HOSTFILE" "IFS" "IGNOREEOF" "INPUTRC" "LANG" "LC_ALL" "LC_COLLATE" "LC_CTYPE" "LC_MESSAGES" "LC_NUMERIC" "LC_TIME" "LINES" "MAIL" "MAILCHECK" "MAILPATH" "OPTERR" "PATH" "POSIXLY_CORRECT" "PROMPT_COMMAND" "PROMPT_DIRTRIM" "PS0" "PS1" "PS2" "PS3" "PS4" "SHELL" "TIMEFORMAT" "TMOUT" "TMPDIR" "auto_resume" "histchars"))

(defun shx-insert-variable ()
  "docstring"
  (interactive)
  (insert "${")
  (insert (completing-read "Variable? " (append 
                                         (shx-find-variables)
                                         shx-bash-builtin-variables)))
  (insert "}"))

(defun sh-mode-setup ()
  (interactive)
  (setq imenu-generic-expression
        (list
         '(nil "^\\(declare.*[[:space:]]\\)?\\([0-9a-zA-Z_]+\\)\\(=.*\\)?$" 2)
         '("functions" "^\\(function \\)?\\([a-z0-9A-Z_]+\\)() {$" 2)))
  (local-set-key (kbd "$") 'shx-insert-variable)
  (fci-mode)
  (display-line-numbers-mode)
  (set-fill-column 120))

(add-hook 'sh-mode-hook 'sh-mode-setup)

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
  (require 'magit-todos)
  (require 'magit-find-file)
  (add-to-list 'display-buffer-alist
               '("magit:.*"
                 (display-buffer-pop-up-frame
                  display-buffer-reuse-window)
                 (reusable-frames . visible)))
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

;; Open edit menu on right-click (TODO Have some usefull functions here)

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
   ac-use-quick-help nil)

  (global-set-key (kbd "C-c C-<SPC>") 'auto-complete)

  (define-key ac-menu-map "\C-n" 'ac-next)
  (define-key ac-menu-map "\C-p" 'ac-previous)
  (define-key ac-menu-map "\C-s" 'ac-isearch)
  (define-key ac-mode-map (kbd "C-x /") 'ac-complete-filename)

  (dolist (mode (list 'xml-mode 'web-mode 'sh-mode
                      'emacs-lisp-mode 'java-mode
                      'js2-mode))
    (add-to-list 'ac-modes mode))

  (defun mp-ac-setup-for-emacs-lisp-mode ()
    "Turn on auto-complete mode and set ac-sources for emacs-lisp-mode."
    (setq ac-sources '(ac-source-yasnippet
                       ac-source-filename
                       ac-source-files-in-current-dir
                       ac-source-features
                       ac-source-functions
                       ac-source-variables
                       ac-source-symbols))
    (auto-complete-mode t) )

  (add-hook 'emacs-lisp-mode-hook 'mp-ac-setup-for-emacs-lisp-mode)

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
  :disabled
  :config
  ;; see company-backends for company backends
  ;; (make-variable-buffer-local 'company-backends)
  (require 'company-template))

;; ]

;; [ ffip

(use-package find-file-in-project
  :config
  (add-to-list 'ffip-project-file "pom.xml")
  (add-to-list 'ffip-project-file "angular.json"))

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

;; [ bookmarks

;; C-x r l    show bookmark list
;; C-x p s    save bookmars list

;; ]

;; [ logfile mode

(define-derived-mode logview-mode view-mode
  "logview"
  "Mode for viewing log files.")

(add-hook 'logview-mode-hook '(lambda ()
                                (stripe-buffer-mode)
                                (hl-line-mode)
                                (font-lock-add-keywords nil '(("\\(INFORMATION\\)" 1 font-lock-warning-face t)))
                                (font-lock-add-keywords nil '(("\\(WARNUNG\\)" 1 font-lock-warning-face t)))
                                (font-lock-add-keywords nil '(("\\(SCHWERWIEGEND\\)" 1 font-lock-warning-face t)))
                                (define-key logview-mode-map  (kbd "<") 'beginning-of-buffer)
                                (define-key logview-mode-map (kbd ">") 'end-of-buffer)
                                (define-key logview-mode-map (kbd "C-h h") 'hydra-highlighting/body)))

(define-derived-mode ndf-logview-mode view-mode
  "ndf-logview"
  "Mode for viewing ndf log files.")

(add-hook 'ndf-logview-mode-hook '(lambda ()
                                    (font-lock-add-keywords
                                     nil '(("\\(\\[Thread-[0-9][0-9]\\] (UTC)\\)" 1 font-lock-warning-face t)))
                                    nil '(("\\(INFO\\)" 1 font-lock-warning-face t))))



(add-hook 'genesys-logview-mode-hook '(lambda ()
                                        (font-lock-add-keywords
                                         nil '(("\\(WARN\\)" 1 font-lock-warning-face t)))
                                        (font-lock-add-keywords
                                         nil '(("\\(Build information\\)" 1 font-lock-warning-face t)))
                                        (font-lock-add-keywords
                                         nil '(("\\(Command line\\)" 1 font-lock-warning-face t)))
                                        (font-lock-add-keywords
                                         nil '(("\\(Host name\\)" 1 font-lock-warning-face t)))
                                        (font-lock-add-keywords
                                         nil '(("\\(DST\\)" 1 font-lock-warning-face t)))
                                        (font-lock-add-keywords
                                         nil '(("\\(Time zone\\)" 1 font-lock-warning-face t)))
                                        (font-lock-add-keywords
                                         nil '(("\\(UTC time\\)" 1 font-lock-warning-face t)))
                                        (font-lock-add-keywords
                                         nil '(("\\(Local time\\)" 1 font-lock-warning-face t)))
                                        (font-lock-add-keywords
                                         nil '(("\\(Start time (UTC)\\)" 1 font-lock-warning-face t)))
                                        (font-lock-add-keywords
                                         nil '(("\\(Running time\\)" 1 font-lock-warning-face t)))
                                        (font-lock-add-keywords
                                         nil '(("\\(Host info\\)" 1 font-lock-warning-face t)))
                                        (font-lock-add-keywords
                                         nil '(("\\(File\\)" 1 font-lock-warning-face t)))
                                        (font-lock-add-keywords
                                         nil '(("\\(Application name\\)" 1 font-lock-warning-face t)))
                                        (font-lock-add-keywords
                                         nil '(("\\(Application type\\)" 1 font-lock-warning-face t)))))


(add-to-list 'auto-mode-alist (cons "\\.log\\'" 'logview-mode))
(add-to-list 'auto-mode-alist (cons "\\-LOG\\'" 'logview-mode))

(add-to-list 'auto-mode-alist (cons "^MCP_standard.*.log\\'" 'genesys-logview-mode))
(add-to-list 'auto-mode-alist (cons "^ndf_.*.log\\'" 'ndf-logview-mode))
;; ]

;; [ macros


;; (require 'macros)

;; C-x (      to start macro recording
;; C-x )      to stop macro recoring
;; C-x C-k r  to apply last macro to region

;; ]

;; [ scratchy

(when (get-buffer "*scratch*")
  (kill-buffer "*scratch*"))

(defvar scratch-map (make-hash-table))

(defun scratch-update-map (map)
  (let ((updated-list (make-hash-table)))
    (progn
      (dolist (mode (map-keys map))
        (let ((buffer (gethash mode map)))
          (when (bufferp buffer)
            (puthash mode buffer map))))
      updated-list)))

(defun scratch-goto-mode (mode)
  (interactive)
  (let ((scratch-buffer (gethash mode scratch-map)))
    (when (or
           (not (buffer-live-p scratch-buffer))
           (null scratch-buffer))
      (progn
        (setq scratch-buffer (generate-new-buffer (format "*scratch-%s*" (symbol-to-string mode))))
        (with-current-buffer scratch-buffer
          (funcall mode))
        (puthash mode scratch-buffer scratch-map)))
    (select-frame (make-frame))
    (switch-to-buffer scratch-buffer)))

(defun scratch-goto-json-mode ()
  (interactive)
  (scratch-goto-mode 'json-mode))

(defun scratch-goto-xml-mode ()
  (interactive)
  (scratch-goto-mode 'xml-mode))

(defun scratch-goto-text-mode ()
  (interactive)
  (scratch-goto-mode 'text-mode))

(defun scratch-goto-emacs-lisp-mode ()
  (interactive)
  (scratch-goto-mode 'emacs-lisp-mode))

(defun scratch-goto-python-mode ()
  (interactive)
  (scratch-goto-mode 'python-mode))

(defun scratch-goto-java-mode ()
  (interactive)
  (scratch-goto-mode 'java-mode))

(defun scratch-goto-org-mode ()
  (interactive)
  (scratch-goto-mode 'org-mode))

(defun scratch-goto-shl-mode ()
  (interactive)
  (scratch-goto-mode 'sh-mode))

(defhydra hydra-global-scratch (:color blue :hint nil)
  "
Mode^^
----------------------------------------------
_t_ext-mode    _p_ython-mode        _o_rg-mode
_j_ava-mode    emacs-_l_isp-mode    _s_h-mode
jso_n_-mode    _x_ml-mode

"
  ("n" scratch-goto-json-mode)
  ("t" scratch-goto-text-mode)
  ("l" scratch-goto-emacs-lisp-mode)
  ("p" scratch-goto-python-mode)
  ("j" scratch-goto-java-mode)
  ("o" scratch-goto-org-mode)
  ("s" scratch-goto-shl-mode)
  ("x" scratch-goto-xml-mode))

(global-set-key (kbd "C-h s") 'hydra-global-scratch/body)


;; ]

;; [ dashboard

;; TODO Unify recentf, bookmarks and projects.
;;      These all print a list of files with a custom heading
;;      - Have alternating colors

(defun fx-file-modified-in-last-24-hours (path)
  "Compare if file at PATH has been modified more recently then TIMESTAMP."
  (message "[dashboard] Checking if cache is current")
  (let* ((attribs (file-attributes path))
         (mtime (nth 5 attribs))
         (now-24 (time-subtract (current-time) '(1 20864 0 0))))
    (if (time-less-p now-24 mtime)
        ;; File was modified within last 24 hours
        ;; will use file-contents instead of calling
        ;; web service
        t
      nil )))

(defun dashboard-pretty-print-qod (qod author)
  ;;  (message "[dashboard] pretty printing quote of the day")
  (let ((width (window-body-width))
        (words (split-string qod))
        (word nil)
        (spacer "    ")
        (line-counter 0))
    (insert spacer)
    (insert "\"")
    (while (and
            words
            (< line-counter 15))
      (progn
        (setq word (car words)
              words (cdr words))
        (if (>
             (+ (current-column) (length word))
             width)
            (progn 
              (newline)
              (insert spacer)))
        (insert word (if (> (length words ) 0) " " "\""))
        (setq line-break nil)))
    (insert (format " (%s)" author))))

(defun dashboard-insert-qod (num-items)
  ;;  (message "[dashboard] Inserting quote of the day")
  (dashboard-insert-heading "Quote of the day: ")
  (condition-case nil
      (let ((dashboard-cache (concat
                              (expand-file-name user-emacs-directory)
                              "quote_of_the_day"))
            (from 0)
            (to 0)
            (qod nil)
            (response-buffer nil)
            (response nil))
        (progn
          (setq response
                (if (and (file-exists-p dashboard-cache)
                         (fx-file-modified-in-last-24-hours dashboard-cache))
                    (with-temp-buffer
                      (insert-file-contents dashboard-cache)
                      (buffer-string))
                  (progn
                    (setq response-buffer (url-retrieve-synchronously "https://quotes.rest/qod" t))
                    (with-current-buffer response-buffer
                      (goto-char (point-min))
                      (search-forward "{")
                      (backward-char 1)
                      (write-region (point) (point-max) dashboard-cache)
                      (buffer-substring-no-properties (point) (point-max))))))
          (let* ((parsed-response (json-read-from-string response))
                 (qod (read-quote-of-the-day parsed-response))
                 (author (read-author parsed-response)))
            (newline)
            (dashboard-pretty-print-qod qod author))))
    (error (insert "No quote of the day is available."))))

(defun read-quote-of-the-day (parsed-response)
  "Extract quote of the day from PARSED-RESPONSE."
  (cdr (nth 0 (aref (cdr (car (cdr (nth 1 parsed-response)))) 0))))

(defun read-author (parsed-response)
  "Extract author from PARSED-RESPONSE."
  (cdr (nth 2 (aref (cdr (car (cdr (nth 1 parsed-response)))) 0))))

(defun dashboard-insert-list (some-list pSpacer)
  "Insert SOME-LIST in a screen-friendly way."
  (when (not (listp some-list))
    (error "Parameter SOME-LIST should be a list, but is not!"))
  (dolist (elem some-list)
    (let ((spacer pSpacer))
      (when (not (stringp elem))
        (error "List element shoudl be a string, but is not!"))
      (insert spacer elem)
      (newline))))

(defun dashboard-insert-mpx-projects (numitems)
  (dashboard-insert-known-projects-list "Known projects:")
  (newline))

(defun dashboard-insert-known-projects-list (list-display-name)
  (dashboard-insert-heading list-display-name)
  (mapc (lambda (el)
          (insert "\n    ")
          (widget-create 'push-button
                         :action `(lambda (&rest ignore) (find-file-existing ,(format "%s" el)))
                         :mouse-face 'highlight
                         :follow-link "\C-m"
                         :button-prefix ""
                         :button-suffix ""
                         :format "%[%t%]"
                         (format "%s" el)))
        projectile-known-projects))


(defun dashboard-system-info-insert-load-path ()
  (dashboard-insert-list load-path "     - "))

(defun dashboard-system-info (numitems)
  (dashboard-insert-heading "Emacs system information:")
  (newline)
  (insert "    Emacs version : " emacs-version)
  (newline)
  (insert "    User init file: " user-init-file)
  (newline)
  (insert "        load-path : ")
  (dashboard-system-info-insert-load-path))

(defun dashboard-insert-recentx-list (list-display-name list)
  "Render LIST-DISPLAY-NAME title and items of LIST."
  (when (car list)
    (dashboard-insert-heading list-display-name)
    (mapc '(lambda (el)
             (insert "\n    ")
             (widget-create 'push-button
                            :action `(lambda (&rest ignore) (find-file-existing ,el))
                            :mouse-face 'highlight
                            :follow-link "\C-m"
                            :button-prefix ""
                            :button-suffix ""
                            :format "%[%t%]"
                            (format "%s" el)))
          list)))

(defun dashboard-insert-recentx (list-size)
  "Pimped version of dashboard-insert-recents. Add the list of LIST-SIZE items from recently edited files."
  (recentf-mode)
  (when (dashboard-insert-recentx-list
	 "Recent Files:"
	 (dashboard-subseq recentf-list
                           0 list-size))
    (dashboard-insert-shortcut "r" "Recent Files:")))

(use-package dashboard
  :config
  (add-to-list 'dashboard-item-generators '(qod . dashboard-insert-qod))
  (add-to-list 'dashboard-item-generators '(mpx-projects . dashboard-insert-mpx-projects))
  (add-to-list 'dashboard-item-generators '(recentx . dashboard-insert-recentx))
  (add-to-list 'dashboard-item-generators '(sysinfo . dashboard-system-info))

  (setq dashboard-items '((qod . nil)
                          ;;                          (agenda . 10)
                          (mpx-projects . 10)
                          (recentx . 15)
                          (bookmarks . 10)
                          (sysinfo . 1)))
  (dashboard-setup-startup-hook))

;; [ profiler report mode ]

(add-hook 'profiler-report-mode-hook 'hl-line-mode)

;; ]

;; [ json

(require 'json)
(use-package json-mode)

;; ]

;; [ openwith

(require 'openwith)
(openwith-mode t)

;; ]

;; [ projectile

;; Also see http://batsov.com/projectile/

(use-package projectile
  :config
  (global-set-key (kbd "C-x C-f") (lambda () (interactive)
                                    (if (featurep 'projectile-project-root)
                                        (projectile-find-file-dwim))
                                    (call-interactively 'find-file)))
  (global-set-key (kbd "C-x b") (lambda () (interactive)
                                  (if (featurep 'projectile-project-root)
                                      (projectile-switch-to-buffer)
                                    (call-interactively 'switch-to-buffer))))
  (setq projectile-completion-system 'ivy
        projectile-indexing-method 'alien
        projectile-enable-caching t
        projectile-switch-project-action 'projectile-dired
        projectile-track-known-projects-automatically nil)
  (projectile-mode))

(defun projectile-available-p ()
  "Determine if we are anywhere where calling projectile functions makes sense."
  (fboundp 'projectile-project-root))

(defhydra hydra-projectile (:color lightblue
                            :hint nil)
  "
     PROJECTILE: %(projectile-project-root)

     Find File          Search/Tags       Buffers              Cache
------------------------------------------------------------------------------------------
  _F_: file            _t_: xref find tag  _i_: Ibuffer           _c_: cache clear
 _ff_: file dwim                         _b_: switch to buffer  _x_: remove known project
 _fd_: file curr dir   _o_: multi-occur    _K_: Kill all buffers  _X_: cleanup non-existing
  _r_: recent file                                            ^^^^_z_: cache current
  _d_: find dir

"
  ("a"   projectile-ag)
  ("b"   projectile-switch-to-buffer)
  ("c"   projectile-invalidate-cache)
  ("d"   projectile-find-dir)
  ("F"   projectile-find-file)
  ("ff"  projectile-find-file-dwim)
  ("fd"  projectile-find-file-in-directory)
  ("t"   xref-find-definitions-other-window)
  ("i"   projectile-ibuffer)
  ("K"   projectile-kill-buffers)
  ("m"   projectile-multi-occur)
  ("o"   projectile-multi-occur)
  ("P"   projectile-switch-project "switch project")
  ("p"   projectile-switch-project)
  ("s"   projectile-switch-project)
  ("r"   projectile-recentf)
  ("x"   projectile-remove-known-project)
  ("X"   projectile-cleanup-known-projects)
  ("z"   projectile-cache-current-file)
  ("`"   hydra-projectile-other-window/body "other window")
  ("q"   nil "cancel" :color blue))

(global-set-key (kbd "C-h C-p") 'hydra-projectile/body)

(defhydra hydra-maven-projectile (:color lightblue :hint nil :exit t)
  "
PROJECTILE: %(projectile-project-root)


lifecycle     ^clean^          ^default^
^^^^^^^^-----------------------------------------------------------------
           _c_: clean       _v_: validate
                          _C_: compile
                          _t_: test
                          _p_: package     
                          _V_: verify
                          _i_: install
                          _d_: deploy        
"
  ("c" mpx-maven-clean)
  ("v" mpx-maven-validate)
  ("C" mpx-maven-compile)
  ("t" mpx-maven-test)
  ("p" mpx-maven-package)
  ("V" mpx-maven-verify)
  ("i" mpx-maven-install)
  ("d" mpx-maven-deploy))

(defun mpx-maven-hydra-wrapper ()
  (interactive)
  (if (projectile-available-p)
      (hydra-maven-projectile/body)
    (message "Projectile is not available. Calling maven hydra makes no sense.")))

(define-key java-mode-map (kbd "C-h C-m") 'mpx-maven-hydra-wrapper)

;; ]

;; [ Finalizer

(require 'convenience)

(setq gc-cons-threshold (* 1024 1024 16)
      gc-cons-percentage 0.1)

(toggle-debug-on-error nil)

(notify "[Emacs] init.el fully loaded")

(setq mode-line-format mpx-standard-mode-line-format)

;; ]
