;;; init.el --- Emacs initialization file
;;
;;
;;; Commentary:
;;  This is the Emacs initialization file.  Emacs reads it when
;;  starting up.  It takes care for loading the users preferences.
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

;; check if "various" and "auto-save" directories are available
;; if not just create them

(let* ((emacs-dir (concat (getenv "HOME") "/.emacs.d/"))
       (various-dir (concat emacs-dir "various/"))
       (autosave-dir (concat emacs-dir "auto-save/")))
  (dolist (dir (list various-dir autosave-dir))
    (when (not (file-exists-p dir))
      (progn
        (message (concat "[emacs boot] Creating directory " dir))
        (make-directory dir)))))

;; ]

;; [ recentf

(recentf-mode 1)

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

;; ]


;; [ compilation

(add-hook 'compilation-mode-hook '(lambda ()
                                    (interactive)
                                    (local-set-key (kbd "q") 'quit-window)))

(use-package auto-compile
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

;; ]

;; [ General Emacs Behaviour

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

(add-to-list 'load-path "~/.emacs.d/lib/")

(setq scroll-step 1
      scroll-conservatively 10000
      auto-window-vscroll nil
      delete-exited-processes t
      gc-cons-threshold 50000000)

;; Kill buffers when done (M-x #)
(add-hook 'server-done-hook (lambda nil (kill-buffer nil)))

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.")

(global-set-key (kbd "M-Z") 'zap-up-to-char)

(add-to-list 'auto-mode-alist '("\.dll\\'" . hexl-mode))
(add-to-list 'auto-mode-alist '("\.exe\\'" . hexl-mode))
(add-to-list 'auto-mode-alist '("\.so\\'" . hexl-mode))

;; ]

;; [ the mark

(setq set-mark-command-repeat-pop t)

;; ]

;; [ server mode

(server-start)

;; ]

;; [ isearch

;; ‘C-w’     - Select the (rest of the) word the TextCursor is on as
;; the search string; 
;; ‘M-s C-e’ - Select the text up to the end of the line as the search
;; string (this was bound to ‘C-y’ up until Emacs 24.1). 
;; ‘M-s h r’ - Highlight regular expression (‘highlight-regexp’)
;; ‘M-s h u’ - Unhighlight regular expression
;; ‘M-s o’   - call ‘occur’ with the current search term

(global-set-key (kbd "C-s") 'isearch-forward-regexp)

(defadvice isearch-forward-regexp (before kill-ring-save-before-search activate)
  "Save region (if active) to \"kill-ring\" before starting isearch.
This way region can be inserted into isearch easily with yank command."
  (when (region-active-p)
    (kill-ring-save (region-beginning) (region-end))))

;; ]

;; [ s

(use-package s)

;; ]

;; [ encoding systems

;;(modify-coding-system-alist 'file ".*cygwin.*\.sh" 'utf-8-unix)
;;(modify-coding-system-alist 'file ".*cygwin.*\.py[23]?" 'utf-8-unix)

;; ]

;; [ appearence

(when window-system
  (horizontal-scroll-bar-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode 1)
  (tooltip-mode -1)
  (scroll-bar-mode -1))

;; do not show startup screen
(setq inhibit-startup-screen t)

;; (require 'hl-line)
;; (global-hl-line-mode nil)

(load-theme 'material)

;; [ backups

(setq backup-directory-alist '(("." . "~/.emacs.d/backups"))
      delete-old-versions t
      version-control t
      vc-make-backup-files t
      auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save/" t)))

;; ]

;; [ imenu

(setq imenu-auto-rescan t
      imenu-use-popup-menu nil
      imenu-space-replacement "-"
      imenu-sort-function 'imenu--sort-by-name) ;; sort only mouse menu

(defadvice imenu-recenter-advice (after mp/imenu-center activate)
  (recenter))

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

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :config
  (require 'ibuffer-git)

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

  (add-hook 'ibuffer-mode-hook
            '(lambda ()
               (ibuffer-auto-mode 1)
               (define-key ibuffer-mode-map (kbd "C-p") 'ibuffer-previous-line)
               (define-key ibuffer-mode-map (kbd "C-n") 'ibuffer-next-line)
               (ibuffer-switch-to-saved-filter-groups "groupmode"))))
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
  (make-local-variable 'paragraph-start)
  (setq paragraph-start "^;; \\[")
  (make-local-variable 'paragraph-separate)
  (setq-local comment-auto-fill-only-comments t)
  (auto-fill-mode 1)
  (setq-local comment-multi-line t)
  (setq paragraph-separate "^;; ]$")
  (setq-local imenu-create-index-function 'imenu-default-create-index-function)
  (setq imenu-generic-expression '((nil "^;; \\[ \\(.*\\)" 1)))
  (ac-emacs-lisp-mode-setup))


(defun byte-compile-current-buffer ()
  (interactive)
  (byte-compile-file (buffer-file-name)))

(defun mp/emacs-lisp-mode-hook ()
  (eldoc-mode 1)
  (when (string= (buffer-name) "init.el")
    (mp/dotemacs-mode-hook))
  (local-set-key (kbd "C-/") 'comment-dwim)
  (local-set-key (kbd "C-c C-c") 'byte-compile-current-buffer)
  (electric-pair-mode)
  (setq fill-column 120)
  (linum-mode))


(add-hook 'emacs-lisp-mode-hook 'mp/emacs-lisp-mode-hook)

(use-package elisp-slime-nav
  :config
  (add-hook 'emacs-lisp-mode-hook 'turn-on-elisp-slime-nav-mode))

;; ]

;; [ save history


(savehist-mode 1)
(setq savehist-file "~/.emacs.d/various/savehist"
      history-length t
      history-delete-duplicates t
      savehist-save-minibuffer-history 1
      savehist-additional-variables '(kill-ring
                                      search-ring
                                      regexp-search-ring))

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
;; TODO - Don't want to trigger auto-complete by key, but don't want
;;        auto-complete to appear in case of possibel snippet expansion
;; TODO - want keybindings in auto-complete for search and scroll
;; TODO - want to understand how documentation in auto-complete works
;; TODO - want to evaluate how well auto-complete is pushed forward or
;;        if its better to switch to "alternative package"
;; TODO - want per mode and per file dictionary files
(use-package auto-complete
  :config
  (require 'auto-complete-config)
  (setq-default ac-sources '(ac-source-abbrev))
  (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
  (setq ac-sources '(ac-source-features
                     ;; collects 'require-able features from the file sytem
                     ac-source-functions
                     ac-source-variables
                     ac-source-symbols)))
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
  (local-set-key (kbd "RET") 'c-indent-new-comment-line))

(use-package js2-mode
  :mode "\\.js\\'"
  :config
  (add-hook 'js2-mode-hook 'mp/j2-mode-hook))

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

(global-set-key (kbd "<f4>") '(lambda () (interactive)
                                "Toggle calendar visibility"
                                (let ((calendar-window
                                       (get-buffer-window "*Calendar*")))
                                  (if calendar-window
                                      (delete-window calendar-window)
                                    (calendar)))))

(add-hook 'diary-display-hook 'fancy-diary-display)
(add-hook 'today-visible-calendar-hook 'calendar-mark-today)

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

(use-package aggressive-fill-paragraph
  :config
  (add-hook 'org-mode-hook aggressive-fill-paragraph-mode) )

(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1) ) ) )

(require 'ob-plantuml)

(setq dot.exe "C:/graphviz/bin/dot.exe"
      org-plantuml-jar-path "~/plantuml/plantuml.jar")

(setq org-ellipsis "…")

(setenv "GRAPHVIZ_DOT" dot.exe)

(add-to-list 'org-babel-load-languages '(plantuml . t))

(require 'ob-python)

(add-to-list 'org-babel-load-languages '(python . t))

(org-babel-do-load-languages 'dont 'care)

(setq org-confirm-babel-evaluate nil)

(defun mp/org-mode-hook-extension ()
  "org mode hook extender [mp]"
  (auto-fill-mode)
  (local-set-key (kbd "<return>") 'org-return-indent))

(add-hook 'org-mode-hook 'mp/org-mode-hook-extension)

;; ]

;; [ prodigy service manager

;; TODO Want python script as services: echo, time, quotd

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
    "C:/mp/Python/Python35-32/python.exe"
    "Location of python interpreter used by prodigy.  Default just grabs one from PATH.")

  (global-set-key (kbd "<f5>") 'mp/toggle-prodigy-buffer)

  (prodigy-define-service
    :name "Date Server (python)"
    :command mp:prodigy-python-interpreter
    :args '("date.py" "14002")
    :stop-signal 'int
    :cwd (concat mp:prodigy-service-root "date/"))

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
                              (linum-mode)
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
  (auto-complete-mode)
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

(global-set-key (kbd "<f7>") 'mp/eshell)

(defun eshell-emergency-exit ()
  "When eshell refuses to close with \"Text is read-only.\" message exit eshell with this function instead."
  (interactive)
  (let ((inhibit-read-only t)) (kill-this-buffer)))

;; ]

;; [ C/C++

(defun mp/c-mode-hook ()
  "Personal c mode hook extender."
  (local-set-key (kbd "C-c C-c") 'compile)
  (linum-mode))

(add-hook 'c-mode-hook 'mp/c-mode-hook)

;; ]

;; [ web development

(defgroup mp nil "All things related to my customization" :group 'Emacs)

(defgroup development nil "All things related to development" :group 'mp)

(defgroup web nil "All things related to web development" :group 'development)

(defcustom web-project-root "~/www/" "New web projects are stored in this directory." :group 'web)

(defun mp/start-web-project (name)
  "Create a new web project with NAME.  Create initial html, js, css file."
  (interactive "MProjectname? ")
  (let ((projectroot (concat web-project-root name)))
    (unless (file-exists-p projectroot)
      (mkdir projectroot))
    (select-frame (make-frame))
    (split-window-vertically)
    (find-file (concat projectroot "/" name ".html"))
    (other-window 1)
    (find-file (concat projectroot "/" name ".js"))
    (split-window-horizontally)
    (find-file (concat projectroot "/" name ".css"))
    (other-window -1)))

(defun mp/css-mode-hook ()
  "Personal css mode hook extender."
  (setq ac-sources '(ac-source-css-property))
  (linum-mode))

(add-hook 'css-mode-hook' mp/css-mode-hook)

(add-hook 'html-mode-hook '(lambda ()
                             "Enable html auto-complete for html-mode."
                             ;; (require 'ac-html)
                             ;; (setq ac-sources '(ac-source-html-attribute-value
                             ;; 'ac-source-html-tag
                             ;; 'ac-source-html-attribute))
                             (linum-mode)
                             (auto-complete-mode)))

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

(global-set-key (kbd "<f1>") 'mp/detach-window)
(global-set-key (kbd "<f2>") 'make-frame)
(global-set-key (kbd "<f3>") 'delete-frame)
(global-set-key (kbd "C-x 2") 'split-window-below-select)
(global-set-key (kbd "C-x 3") 'split-window-right-select)

;; ]

;; [ ediff

(add-hook 'ediff-after-quit-hook-internal 'winner-undo)

;; ]

;; [ whitespace mode

(global-set-key (kbd "C-c 2") 'whitespace-mode)

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

(add-to-list 'auto-insert-alist '(".*\\.java$" . [ "template.java" mp:java-preprocessor] ))

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

(defun mp/nxml-mode-setup ()
  (linum-mode))

(add-hook 'nxml-mode-hook 'mp/nxml-mode-setup)

;; ]

;; [ maven integration

(defun mp/maven-integration ()
  (interactive)
  (when (string= "pom.xml" (buffer-name))
    (progn
      (setq compile-command "mvn clean install")
      (local-set-key (kbd "C-c C-c") 'compile))))

(add-hook 'nxml-mode-hook 'mp/maven-integration)

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
  (global-set-key (kbd "M-x") 'smex))

(use-package ido-vertical-mode
  :config
  (ido-vertical-mode))

(use-package ido-ubiquitous
  :config
  (ido-ubiquitous-mode)
  (setq ido-cr+-max-items 50000)
  (define-key ido-common-completion-map (kbd "C-n") 'ido-next-match)
  (define-key ido-common-completion-map (kbd "C-p") 'ido-prev-match) )

;; [ tags

(setq tags-file-name nil
      tags-table-list nil)

;; ]

;; [ Where was I [editing text]?

(defun mp/store-lot-position ()
  (when (not (string-prefix-p "*" (buffer-name)))
    (point-to-register ?z)))

(defun mp/goto-lot-position ()
  (interactive)
  (jump-to-register ?z))

(add-hook 'post-self-insert-hook 'mp/store-lot-position)

(global-set-key (kbd "C-c 1") 'mp/goto-lot-position)

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
  (quit-window)
  (unhighlight-regexp mp/occur-last-regexp)
  (setq mp/occur-marked-regexp nil
        mp/occur-last-regexp nil
        mp/occur-origin-buffer nil))

(defun mp/occur-goto-occurence ()
  (interactive)
  (occur-mode-goto-occurrence)
  (unhighlight-regexp mp/occur-last-regexp))

(global-set-key (kbd "C-o") 'occur)

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

(setq jedi:complete-on-dot t)

(defun mp/python-mode-hook ()
  "Personal python mode hook extension."
  (setq-local comment-auto-fill-only-comments t)
  (setq-local comment-multi-line t)
  (auto-fill-mode 1)
  (local-set-key (kbd "C-/") 'comment-dwim)
  (jedi:setup)
  (setq jedi:complete-on-dot t)
  (linum-mode))

(add-hook 'python-mode-hook 'mp/python-mode-hook)

(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.py2\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.py3\\'" . python-mode))

(add-to-list 'auto-insert-alist '(".*\\.py[23]?$" . [ "template.py"]))

(defun py-indent-with-region()
  (interactive "")
  (when (use-region-p)
    (let ((beginning (min (region-beginning) (region-end)))
          (end (max (region-beginning )(region-end))))
      (progn
        (goto-char beginning)
        (while (< (point) end)
          (progn
            (indent-for-tab-command)
            (forward-line 1) ) ) ) ) )
  (indent-for-tab-command) )
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

;; [ html

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
  (linum-mode))

(add-hook 'html-mode-hook 'mp/html-mode-setup)

;; ]

;; [ shell script mode

(defun mp:elisp-post-processor ()
  (interactive)
  (let ((match-found t))
    (progn
      (goto-char (point-min))
      (when (re-search-forward "_" nil t)
        (replace-match "") ) ) ) )

(add-to-list 'auto-insert-alist '(".*\\.sh$" . [ "template.sh" mp:elisp-post-processor] ))

;; ]
