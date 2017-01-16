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

;; ]

;; [ packaging

(require 'package)

(defconst mp-fn-package-guard "~/.emacs.d/.package-guard")
(defconst mp-package-guard-renewal 604800) ;; this is one week. use 86400 for one day.

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/"))
      package-enable-at-startup nil
      package-user-dir "~/.emacs.d/packages/")

(package-initialize)

;; periodically refresh package contents

(defun mp-timeval-to-seconds (tv)
  "Calculate SEC-HIGH * 2^16 + SEC-LOW for value contained in TV."
  (let* ((sec-high (nth 0 tv))
         (sec-low (nth 1 tv)))
    (+ sec-low (* sec-high (expt 2 16)))))

(defun mp-package-refresh-necessary-p ()
  (if (file-exists-p mp-fn-package-guard)
      (progn
        (let* ((mtime (mp-timeval-to-seconds (nth 5 (file-attributes mp-fn-package-guard))))
               (ctime (mp-timeval-to-seconds (current-time))))
          (< (+ mtime mp-package-guard-renewal) ctime )))
    t))

(defun mp-update-package-guard ()
  "Write current time to pacakge-guard file"
  (with-temp-buffer
    (insert ";; (prin1-to-string (current-time))\r\n")
    (insert (prin1-to-string (current-time)))
    (write-file mp-fn-package-guard)))

(global-set-key (kbd "C-c 5") #'package-list-packages)

;; see if this emacs is starting for the first time (with this init.el)
;; and if pacakge refresh is necessary (currently once in a week)

(if (not (file-exists-p mp-fn-package-guard))
    (let* ((emacs-dir (expand-file-name "~/.emacs.d"))
           (autosave-dir (concat emacs-dir "/auto-save/"))
           (desktop-dir (concat emacs-dir "/desktop"))
           (user-information "Will perform first time initialisation! Press enter."))
      (read-from-minibuffer user-information)
      (when (not (file-exists-p autosave-dir))
        (make-directory autosave-dir)
        (make-directory desktop-dir))
      (mp-update-package-guard)
      (package-refresh-contents)
      (package-install 'use-package))
  (when (mp-package-refresh-necessary-p)
    (let ((user-information "Will refresh package contents! Press enter."))
      (read-from-minibuffer user-information)
      (package-refresh-contents)
      (mp-update-package-guard))))

(require 'use-package)

(setq use-package-verbose t
      use-package-always-ensure t
      load-prefer-newer t)

;; ]

;; [ compilation

(defun mp-compilation-mode-hook-extender ()
  ;; (next-error-follow-minor-mode)
  (local-set-key (kbd "q") 'quit-window))

(add-hook 'compilation-mode-hook 'mp-compilation-mode-hook-extender)

(use-package auto-compile
  :config
  ;; Watch out! Files are auto-compiled only if compiled file exists
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

;; ]


;; customization

(defgroup mp 
  nil "All things related to my customization"
  :group 'Emacs)

(defgroup ibuffer
  nil "All things related to ibuffer"
  :group 'mp)

(defgroup development
  nil "All things related to development"
 :group 'mp)

(defgroup web
  nil "All things related to web development"
  :group 'development)

(defgroup c++
  nil "All things related to C++ development"
  :group 'development)

(defgroup java
  nil "All things related to web development"
  :group 'development)

(defcustom ibuffer-project-file
  "~/.emacs.d/ibuffer-projects"
  "A file describing a list of project directories for ibuffer."
  :group 'ibuffer)

(defcustom web-project-root
  "~/public_html/"
  "New web projects are stored in this directory."
  :group 'web)

(defcustom java-project-root
  "~/src/"
  "New java projects are stored in this directory."
  :group 'mp-java)

(defcustom jdk-location
  "/home/map/opt/jdk1.8.0_101/"
  "Location of JDK"
  :group 'mp-java)

(defcustom openssl-dictionary-location
  "~/.emacs.d/dictionaries/openssl.txt"
  "Location of a file with openssl function names."
  :group 'c++)

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

(defun mp-calendar-mode-hook ()
  (local-set-key (kbd "<RET>") #'diary-view-entries) )

(add-hook 'diary-display-hook 'fancy-diary-display)
(add-hook 'today-visible-calendar-hook 'calendar-mark-today)
(add-hook 'calendar-mode-hook 'mp-calendar-mode-hook)

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

(defvar mp-general-keymap 
  (make-sparse-keymap)
  "General purpose keymap.")

(defsubst mp-notify-available-p ()
  "Return true if notify-send is available in PATH. "
  (mp-exists-in-path "notify-send") )

(defun mp-exists-in-path (file)
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

(defun mp-notify (msg)
  (if mp-notify-available-p
      (start-process "notify-send" nil "notify-send" "-t" "3000" msg)
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

;; do not ask when erasing buffer
(put 'erase-buffer 'disabled nil)

;; if file starts with #! make set exec bit after saving
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(defun indent-buffer ()
  "A helper function that is called after some templates are auto-inserted."
  (interactive)
  (indent-region (point-min) (point-max)) )

;; Tabs - no.
(setq-default indent-tabs-mode nil)

;; Tempaltes - yes.
(setq auto-insert-directory "~/.emacs.d/templates/"
      auto-insert-query nil)

(auto-insert-mode)

(put 'set-goal-column 'disabled nil)
(put 'downcase-region 'disabled nil)

;; yes or no is y or n
(defalias 'yes-or-no-p 'y-or-n-p)

;; handy alias to circumvent the not so intuitive emacs naming
(defalias 'symbol-to-string 'symbol-name)
(defalias 'string-to-symbol 'intern)

;; 
(recentf-mode 1)

;; here goes my personal emacs extension files
(add-to-list 'load-path "~/.emacs.d/elisp/")

;; 
(setq scroll-step 1
      scroll-conservatively 10000
      auto-window-vscroll nil
      delete-exited-processes t)

;; Kill buffers when done (M-x #)
(add-hook 'server-done-hook (lambda nil (kill-buffer nil)))

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.")

(global-set-key (kbd "M-Z") #'zap-up-to-char)

;; use C-u C-SPC to pop mark positions
;; and C-x C-SPC to pop global mark position

(setq set-mark-command-repeat-pop t)

;; ]

;; [ hideshow minor mode

;; Restrict visible portion of buffer to certain blocks
;; Contract anything between start/end to '...'

(use-package hideshow
  :config
  (add-hook 'c-mode-common-hook 'hs-minor-mode)
  (add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
  (add-hook 'python-mode-hook 'hs-minor-mode)

  (global-set-key (kbd "C--") 'hs-hide-block)
  (global-set-key (kbd "C-+") 'hs-show-block)
  (global-set-key (kbd "C-M--") 'hs-hide-all)
  (global-set-key (kbd "C-M-+") 'hs-show-all) )

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

;; ]

;; [ abbreviations

;; (C-u 4 ) C-x a g  to define a global abbrev for word before point

(setq abbrev-file-name "~/.emacs.d/abbrev_defs"
      save-abbrevs t) ;; save abbrevs when file is saved and emacs quits

(if (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file))

(add-hook 'post-self-insert-hook 'expand-abbrev)

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
;; 'M-e'     - edit search string
;; C-s C-h b - show all isearch key bindings

;; shamelessly stolen from http://goo.gl/JFRl1k to keep found string centered
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

(setq frame-title-format "%b")

(global-hl-line-mode)

(when window-system
  (when (eq system-type 'windows-nt)
    (horizontal-scroll-bar-mode -1))
  (tool-bar-mode -1)
  (menu-bar-mode 1)
  (tooltip-mode -1)
  (scroll-bar-mode -1))

(require 'solar)
(require 'calendar)

(defun mp-sunrise-sunset-for-modeline ()
  (let ((calendar-time-display-form '(24-hours ":" minutes))
        (l (solar-sunrise-sunset (calendar-current-date))))
    (format "[↑%s, ↓%s]"
            (apply 'solar-time-string (car l))
            (apply 'solar-time-string (cadr l)))))

;; nice dark theme with a light variante

(use-package material-theme)

;; (load-theme 'material-light)
(load-theme 'material)

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
  (add-hook 'shell-mode-hook 'volatile-highlights-mode) )

;; visualize matching paren
(show-paren-mode)

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

(defadvice imenu-recenter-advice (after mp-imenu-center activate)
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
  :init

  (define-ibuffer-column dirname
    (:name "Directory" :inline nil)
    (let ((result (buffer-name))
          (buf-file-name (buffer-file-name buffer)))
      (and buf-file-name
           (file-exists-p buf-file-name)
           (setq result (file-name-directory buf-file-name)))
      result))

  (setq ibuffer-show-empty-filter-groups nil
        ibuffer-formats '(( mark (git-status-mini) modified read-only "|"
                                 (name 26 26 :left :elide)
                                 "|"
                                 (size 9 -1 :left)
                                 "|" dirname) ;; filename-and-process)
                          (mark " "
                                (name 16 -1)
                                " " filename)))

  (defun mp-ibuffer-add-project (groupname projectname directory)
    (let* ((group (assoc groupname ibuffer-saved-filter-groups))
           (project (assoc projectname (cdr group))))
      (if project
          (setcdr project (list (cons 'filename directory)))
        (setcdr group (cons (list projectname (cons 'filename directory)) (cdr group))))))

  ;; use M-n, M-p to navigate between groups
  (setq ibuffer-saved-filter-groups
        (quote (("Projects"
                 ("Dired" (mode . dired-mode))
                 ("Emacs" (or
                           (name . "^\\*scratch\\*$")
                           (name . "^\\*Messages\\*$")
                           (filename . "^.*/.emacs.d/.*$")))
                 ("Emacs Lisp" (mode . emacs-lisp-mode))
                 ("Customization" (name . "^\\*Customize.*"))
                 ("Organizer" (mode . org-mode))))))

  ;; Load list of project directories from ibuffer-project-file into
  ;; saved filter-group named "Projects". Note that a buffer goes to 
  ;; first matching group.
  (when (file-exists-p ibuffer-project-file)
    (with-temp-buffer
      (insert-file-contents ibuffer-project-file)
      (dolist (line (split-string (buffer-string) "\n" t " "))
        (let ((project (split-string line "," t " "))
              (projectname nil)
              (projectdir nil) )
          (when (eq (length project) 2)
            (progn
              (setq projectname (car project)
                    projectdir (car (cdr project)))
              (mp-ibuffer-add-project "Projects" projectname projectdir)))))))

  (defun mp-ibuffer-mode-hook-extender ()
    (ibuffer-auto-mode 1) ;; auto updates
    (hl-line-mode)
    (define-key ibuffer-mode-map (kbd "C-p") 'ibuffer-previous-line)
    (define-key ibuffer-mode-map (kbd "C-n") 'ibuffer-next-line)
    (ibuffer-switch-to-saved-filter-groups "Projects"))
  
  (add-hook 'ibuffer-mode-hook 'mp-ibuffer-mode-hook-extender))

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

(defun mp-mark-init.el-paragraph ()
  "Mark the entire paragraph around point."
  (interactive)
  (re-search-forward paragraph-separate nil t)
  (set-mark (point))
  (re-search-backward paragraph-start nil t))

(defun mp-dotemacs-mode-hook ()
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
  (add-to-list 'er/try-expand-list 'mp-mark-init.el-paragraph)
  (setq-local imenu-create-index-function 'imenu-default-create-index-function) )

(defun byte-compile-current-buffer ()
  (interactive)
  (byte-compile-file (buffer-file-name)))

(defun mp-emacs-lisp-mode-hook ()

  (when (string= (buffer-name) "init.el")
    (mp-dotemacs-mode-hook))

  (local-set-key (kbd "C-/") 'comment-dwim)
  (local-set-key (kbd "C-c C-c") 'byte-compile-current-buffer)

  (electric-pair-mode)

  (setq ac-sources '(ac-source-yasnippet
                     ac-source-words-in-same-mode-buffers
                     ac-source-features
                     ac-source-functions
                     ac-source-variables
                     ac-source-symbols))
  (auto-complete-mode 1) )

(add-hook 'emacs-lisp-mode-hook 'mp-emacs-lisp-mode-hook)

(use-package elisp-slime-nav
  :config
  (add-hook 'emacs-lisp-mode-hook 'turn-on-elisp-slime-nav-mode))

;; ]

;; [ save history

(setq savehist-file "~/.emacs.d/minibuffer"
      history-length t
      history-delete-duplicates t
      savehist-save-minibuffer-history 1
      savehist-additional-variables '(kill-ring
                                      search-ring
                                      regexp-search-ring))

(savehist-mode 1)

;; ]

;; [ yasnippet

(use-package yasnippet

  :config

  (defconst mp-snippet-dir "~/.emacs.d/snippets/")

  (setq yas-snippet-dirs (list mp-snippet-dir))

  (dolist (snippet-dir yas-snippet-dirs)
    (add-to-list 'auto-mode-alist (cons (concat ".*" snippet-dir ".*") 'snippet-mode))
    (yas-load-directory snippet-dir))

  (require 'warnings)

  ;; do not complain when snippets change buffer contents
  (add-to-list 'warning-suppress-types '(yasnippet backquote-change))

  (yas-global-mode 1) )

;; ]

;; [ auto complete
;;
;; TODO - want per mode and per file dictionary files
;; TODO - want to understand auto-complete-config and how to extend/customize it

(use-package auto-complete
  :defer 1
  :config

  (require 'auto-complete)
  (require 'auto-complete-config)

  (setq
   ac-auto-show-menu 2
   ac-auto-start 0.1
   ac-comphist-file "~/.emacs.d/ac-comphist.dat"
   ac-dictionary-directories (quote ("~/.emacs.d/dictionaries/")) ;; mode specific dictionaries
   ac-dictionary-files (quote ("~/.dict")) ;; personal dictionary
   ac-quick-help-delay 0.2
   ac-use-fuzzy t
   ac-dwim t
   ac-use-menu-map t
   ac-use-quick-help nil
   ac-user-dictionary (quote ("")))

  (global-set-key (kbd "C-c C-<SPC>") 'auto-complete)

  (define-key ac-menu-map "\C-n" 'ac-next)
  (define-key ac-menu-map "\C-p" 'ac-previous)
  (define-key ac-menu-map "\C-s" 'ac-isearch)
  (define-key ac-mode-map (kbd "C-c /") 'ac-complete-filename)

  (add-to-list 'ac-modes 'web-mode)

  (dolist (mode (list 'xml-mode 'web-mode 'sh-mode
                      'emacs-lisp-mode 'java-mode))
    (add-to-list 'ac-modes mode))

  ;; for major mode specific setup of auto-complete see
  ;; the according sections. eg for setting up auto-completion
  ;; for java-mode have a look at java-mode.

  ) ;; end of use-package

;; ]

;; [ avy-mode

(use-package avy
  :bind ("C-S-j" . avy-goto-word-or-subword-1) )

;; ]

;; [ javascript

(defun mp-js2-mode-hook ()
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
      " */" \n \n \n )
    indent-buffer ] )

(defun mp-qunit-test-for-current-buffer ()
  (interactive)
  (let ((test-html-file (replace-regexp-in-string (regexp-quote ".js") "-test.html" (buffer-name)))
        (test-js-file (replace-regexp-in-string (regexp-quote ".js") "-test.js" (buffer-name))))
    (when (eq 1 (length (window-list)))
      (progn
        (find-file test-html-file)
        (goto-char (point-min))
        (split-window (selected-window) 15)
        (other-window 1)
        (find-file test-js-file)))))

(use-package js2-mode
  :mode "\\.js\\'"
  :config
  (add-hook 'js2-mode-hook 'mp-js2-mode-hook))

;; ]

;; [ info browser

;; To read plain info file from the filesystem: "C-u C-h i"

;; TODO: There is a python info page lying around in this directcory,
;; but info does not find it


(add-to-list 'Info-default-directory-list "~/.emacs.d/info")

(defun mp-Info-mode-hook ()
  "Personal info mode hook."
  (define-key Info-mode-map (kbd "C-s") 'isearch-forward-regexp) )

(add-hook 'Info-mode-hook 'mp-Info-mode-hook)

;; ]

;; [ session management

;; save and restore open buffers

(desktop-save-mode)

;; [ org mode

(global-set-key (kbd "C-c c") #'org-capture)

(require 'ob-plantuml)
(require 'ob-python)

;; useful clocking commands
;;    C-c C-x C-i (org-clock-in)
;;    C-c C-x C-o (org-clock-out)
;;    C-c C-x C-q (org-clock-cancel)
;;    C-c C-x C-d (org-clock-displa)
;;    C-S-<up/down> (org-clock-timestamps-up/down)
;;    S-M-<up/down> (org-timestamp-up-down)

(defun mp-org-clone-and-narrow-to-block ()
  (interactive)
  (if (one-window-p)
      (progn
        (clone-indirect-buffer-other-window nil t)
        (org-narrow-to-block) )
    (message "This function is only applicable for frames that show a single window.") ) )

(defun mp-org-mode-hook ()
  (flyspell-mode)
  (add-to-list 'auto-mode-alist '("organizer\\'" . org-mode))
  (setq org-agenda-span 7
        org-agenda-comact-blocks t
        org-agenda-show-all-dates t
        org-agenda-files '("~/org/organizer" "~/org/family.org")
        org-babel-python-command "python"
        org-clock-into-drawer t
        org-clock-persist 'history
        org-confirm-babel-evaluate nil
        org-default-notes-file "~/org/organizer"
        org-directory "~/org/"
        org-ellipsis "…"
        org-log-done (quote note)
        org-log-into-drawer t
        org-plantuml-jar-path "~/.emacs.d/plantUML/plantuml.jar"
        org-special-ctrl-a/e t
        org-special-ctrl-k t
        org-todo-keywords (quote ((sequence "TODO(t)" "WAIT(w)" "DONE(d)" "CANCEL(c)"))))

  (local-set-key (kbd "<return>") 'org-return-indent)
  (local-set-key (kbd "C-x n c") 'mp-org-clone-and-narrow-to-block)

  (setenv "GRAPHVIZ_DOT" "/usr/bin/dot")

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
     (shell . t) ) ) )

(org-clock-persistence-insinuate)

(setq org-capture-templates
      (quote (("s" "source code location" entry (file "~/org/bookmarks.org")
               "* New Entry\n  - %U\n  - %A%?\n" :clock-in nil :clock-resume nil)
              ("t" "todo" entry (file "~/org/gtd.org")
               "** TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
              ("r" "respond" entry (file "~/org/gtd.org")
               "** NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
              ("n" "note" entry (file "~/org/gtd.org")
               "** %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
              ("m" "Meeting" entry (file "~/org/gtd.org")
               "** MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
              ("p" "Phone call" entry (file "~/org/gtd.org")
               "** PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t) ) ) )

(add-hook 'org-mode-hook 'mp-org-mode-hook)

;; ]

;; [ prodigy service manager

;; Warning: prodigy does not seem to allow stopping services on Windows

(use-package prodigy
  :config

  (defvar mp-prodigy-service-root
    "~/.emacs.d/services/"
    "Root directory for various services bundled with init.el." )

  (defvar mp-prodigy-python-interpreter
    "/usr/bin/python3"
    "Location of python interpreter used by prodigy.  Default just grabs one from PATH.")

  (defvar mp-prodigy-tomcat-root-dir
    "~/opt/apache-tomcat-8.5.4/"
    "Root directory of tomcat installation")

  (defvar mp-prodigy-tomcat-start-script
    (concat mp-prodigy-tomcat-root-dir "bin/catalina.sh")
    "Path to script that starts Tomcat.")

  (defvar mp-prodigy-wildfly-root-dir
    "~/opt/wildfly-10.1.0.Final/"
    "Root directory of wildfly installation")

  (defvar mp-prodigy-wildfly-start-script
    (concat mp-prodigy-tomcat-root-dir "bin/standalone.sh")
    "Path to script that starts Wildfly. Path is relative to mp-prodigy-wildfly-root-dir.")

  (defun mp-prodigy-setup-frame ()
    (interactive)
    (let ((frame-parameters '((name . "Prodigy")
                              (height . 25)
                              (width . 80)
                              (minibuffer . t))))
      (select-frame (make-frame frame-parameters))
      (prodigy)
      (delete-other-windows)))
  
  ;;  (advice-add 'prodigy-start-service :after #'prodigy-display-process)

  (defun mp-prodigy-next-line ()
    (interactive)
    (next-line)
    (when (looking-at ".*Running.*")
      (progn
        (prodigy-display-process)
        (select-window (get-buffer-window "*prodigy*")))))


  (defun mp-prodigy-previous-line ()
    (interactive)
    (previous-line)
    (when (looking-at ".*Running.*")
      (progn
        (prodigy-display-process)
        (select-window (get-buffer-window "*prodigy*")))))

  (defun mp-prodigy-mode-extender ()
    (local-set-key (kbd "C-n") 'mp-prodigy-next-line)
    (local-set-key (kbd "C-p") 'mp-prodigy-previous-line))

  (add-hook 'prodigy-mode-hook 'mp-prodigy-mode-extender)

  (global-set-key (kbd "<f5>") #'mp-prodigy-setup-frame)

  (prodigy-define-service
    :name "Tomcat 8.5.4"
    :command mp-prodigy-tomcat-start-script
    :args '("run")
    :cwd mp-prodigy-tomcat-root-dir)

  (prodigy-define-service
    :name "Wildfly 10.1.0"
    :command mp-prodigy-wildfly-start-script
    :args '("run")
    :cwd mp-prodigy-wildfly-root-dir)

  (prodigy-define-service
    :name "Date Server (14002)"
    :command mp-prodigy-python-interpreter
    :args '("date.py" "14002")
    :stop-signal 'int
    :cwd (concat mp-prodigy-service-root "date/"))

  (prodigy-define-service
    :name "Network Log-Receiver"
    :command "/usr/bin/python2"
    :args '("logwebmon.py")
    :cwd (concat mp-prodigy-service-root "loghost/"))

  (prodigy-define-service
    :name "Echo Server (14001)"
    :command mp-prodigy-python-interpreter
    :args '("echo.py" "14001")
    :stop-signal 'int
    :cwd (concat mp-prodigy-service-root "echo/") ) )

;; ]

;; [ speedbar

(use-package sr-speedbar
  :bind ("<f6>" . sr-speedbar-toggle)
  :config
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

;; (modify-frame-parameters nil (list '( name . "Emacs" )

;; ]

;; [ xref and tags

(setq tags-file-name nil
      tags-table-list nil
      tags-revert-without-query t)

(global-set-key (kbd "M-*") #'xref-pop-marker-stack)

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

(defun mp-eshell-mode-hook ()
  "Personal eshell mode hook."
  (interactive)
  (setq ac-sources '(ac-source-yasnippet ac-source-filename ac-source-files-in-current-dir))
  (auto-complete-mode t)
  (local-set-key (kbd "C-c C-c") 'mp-eshell) )

(add-hook 'eshell-mode-hook 'mp-eshell-mode-hook)

(defconst eshell-window-height -15 "Height of eshell window.")
(defconst eshell-buffer-name "*eshell*")

(defun mp-eshell ()
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

(global-set-key (kbd "<f7>") #'mp-eshell)

(defun eshell-emergency-exit ()
  "When eshell refuses to close with \"Text is read-only.\" message exit eshell with this function instead."
  (interactive)
  (let ((inhibit-read-only t)) (kill-this-buffer)))

;; ]

;; [ C/C++

(use-package ac-etags
  :config
  (ac-etags-setup) )

(defun mp-mark-def-undef-block ()
  "Mark block from #define to #undef."
  (interactive)
  (let ((tagname nil))
    (re-search-forward "^#undef \\(.*\\)" nil t)
    (setq tagname (match-string 1))
    (set-mark (point))
    (re-search-backward (concat "^#define " tagname) nil t)))

(defun mp-mark-if-endif-block ()
  "Mark block from #define to #undef."
  (interactive)
  (let ((tagname nil))
    (re-search-forward "^#endif.*" nil t)
    (set-mark (point))
    (re-search-backward (concat "^#ifdef " tagname) nil t)))

(defun mp-openssl-help ()
  (interactive)
  (browse-url-firefox (concat 
                       "https://www.openssl.org/docs/manmaster/man3/"
                       (thing-at-point 'symbol))
                      t))

(defun mp-c-mode-hook ()
  "Personal c mode hook extender."
  (auto-complete-mode 1)
  (linum-mode)
  (setq ac-sources  (list
                     'ac-source-yasnippet
                     'ac-source-etags
                     'ac-source-dictionary
                     'ac-source-words-in-same-mode-buffers))
  (let ((add-openssl-dict nil))
    (save-excursion 
      (goto-char (point-min))
      (when (re-search-forward "^#include ?<openssl/.*" nil t)
        (setq add-openssl-dict t)))
    (when add-openssl-dict
      (progn
        (local-set-key (kbd "C-h o") 'mp-openssl-help)
        (add-to-list 'ac-user-dictionary 'openssl-dictionary-location))))
  (add-to-list 'er/try-expand-list 'mp-mark-def-undef-block)
  (add-to-list 'er/try-expand-list 'mp-mark-if-endif-block)
  (local-set-key (kbd "C-c C-c") 'compile) )

(add-hook 'c++-mode-hook 'mp-c-mode-hook)
(add-hook 'c-mode-hook 'mp-c-mode-hook)

;; ]

;; [ frame+window handling

(use-package windmove
  :config

  (setq windmove-wrap-around t) 

  (defvar mp-windmove-keymap 
    (make-sparse-keymap)
    "Keymap for windmove commands.")

  (define-key mp-windmove-keymap (kbd "p") 'windmove-up)
  (define-key mp-windmove-keymap (kbd "n") 'windmove-down)
  (define-key mp-windmove-keymap (kbd "f") 'windmove-right)
  (define-key mp-windmove-keymap (kbd "b") 'windmove-left)

  (global-set-key (kbd "C-x w") mp-windmove-keymap) )

(winner-mode)

(defun mp-detach-window (arg)
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

(global-set-key (kbd "<f1>") #'mp-detach-window)
(global-set-key (kbd "<f2>") #'make-frame)
(global-set-key (kbd "<f3>") #'delete-frame)
(global-set-key (kbd "C-x 2") #'split-window-below)
(global-set-key (kbd "C-x 3") #'split-window-right)
(global-set-key (kbd "<f8>") #'rotate-windows)
(global-set-key (kbd "<f9>") #'swap-buffers)

;; ]

;; [ ediff

(add-hook 'ediff-after-quit-hook-internal 'winner-undo)

(defun mp-ediff-this ()
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

(global-set-key (kbd "C-c 2") #'whitespace-mode)

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

(defun mp-man-mode-hook ()
  (define-key Man-mode-map (kbd "C-n") 'scroll-up-one-line)
  (define-key Man-mode-map (kbd "C-p") 'scroll-down-one-line) )

(add-hook 'Man-mode-hook 'mp-man-mode-hook)

(defun mp-help-mode-setup ()
  (local-set-key (kbd "q") 'delete-window))

(add-hook 'help-mode-hook 'mp-help-mode-setup)

;; ]

;; [ java mode

(defvar mp-ac-classpath-cache nil)

(use-package javadoc-lookup)

(defun mp-ac-classpath-init ()
  (setq mp-ac-classpath-cache (mp-read-classes-from-jar)))

(defvar ac-source-classpath
  '((prefix . "^import \\(.*\\)")
    (init . mp-ac-classpath-init)
    (candidates . mp-ac-classpath-cache)))

(defun mp-read-classes-from-jar ()
  (with-temp-buffer
    (call-process "/usr/bin/unzip" nil t nil "-l" (concat jdk-location "jre/lib/rt.jar"))
    (goto-char (point-min))
    (let ((end 0)
          (result '())
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
      result)))

(defun mp-assert-import (name)
  "Insert import statement for class NAME if it does not yet exist. "
  (save-excursion
    (goto-char (point-min))
    (when (not (re-search-forward (format "^import %s;" name) nil t))
      (progn
        (while (re-search-forward "^import.*" nil t))
        (end-of-line)
        (newline-and-indent)
        (newline-and-indent)
        (insert (format "import %s;" name))))))

(defun mp-start-new-web-application (group-id artifact-id version-number)
  (interactive "MGroup-id: \nMArtifact-id: \nMVersion-number: ")
  (let* ((project-path java-project-root)
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
      (call-process "mvn" nil live-buffer-name t "archetype:generate"
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
          (mp-copy-template "web-3.0.xml" target-web-xml
                            (list 
                             (list 'DISPLAY-NAME (format "%s %s" artifact-id version-number))))))
      (goto-char (point-max)) ) ) )


(defun mp-guess-package-name-for-current-buffer ()
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

(defun mp-java-preprocessor()
  (let ((classname (file-name-sans-extension (buffer-name)))
        (packagename (mp-guess-package-name-for-current-buffer)))
    (while (search-forward "CLASSNAME" nil t)
      (replace-match classname t))
    (goto-char (point-min))
    (while (search-forward "PACKAGE" nil t)
      (replace-match packagename t) ) ) )

(defun mp-copy-template (filename target-file alist)
  "Copy FILENAME to TARGET-FILE. Then replace keys with values looked up in ALIST"
  (with-temp-buffer
    (insert-file (concat "~/.emacs.d/templates/" filename))
    (dolist (key-value alist)
      (goto-char (point-min))
      (let ((key (symbol-to-string (car key-value)))
            (value (car (cdr key-value)))
            (case-fold-search nil))
        (while (search-forward key nil t)
          (replace-match value t))))
    (write-file target-file nil) 
    (kill-buffer) ) )

(defun mp-start-new-java-project (group-id artifact-id version-number)
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

    (mp-copy-template "pom.xml" target-pom
                      (list (list 'GROUP-ID group-id)
                            (list 'ARTIFACT-ID artifact-id)
                            (list 'VERSION version-number)))

    (when (not (file-exists-p class-dir))
      (make-directory class-dir t))

    (select-frame pframe)
    (find-file main-class)
    (save-buffer)
    (neotree-dir project-root)) )

(defun mp-java-mode-hook()
  (setq-local comment-auto-fill-only-comments t)
  (auto-complete-mode)
  (setq ac-sources '(ac-source-yasnippet ac-source-classpath ac-source-dictionary ac-source-words-in-same-mode-buffers))
  (subword-mode)
  (linum-mode)
  (local-set-key (kbd "C-h j") 'javadoc-lookup)
  (setq-local comment-multi-line t) )

(add-hook 'java-mode-hook 'mp-java-mode-hook)

;; preprocessor for interactively generating files from templates
(add-to-list 'auto-insert-alist '(".*\\.java$" . [ "template.java" mp-java-preprocessor] ) )

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

(defun mp-nxml-mode-setup ())

(add-hook 'nxml-mode-hook 'mp-nxml-mode-setup)

(add-to-list 'auto-insert-alist '("pom.xml$" . [ "pom.xml" ]))

(setq xml-modes (list ".*\\.xul\\'" ".*\\..rdf\\'" ".*\\.xsd\\'" ".*\\.wsdl\\'"))

(dolist (mode xml-modes)
  (add-to-list 'auto-mode-alist (cons mode 'xml-mode)))

(defun mp-maven-integration ()
  (interactive)
  (when (string= "pom.xml" (buffer-name))
    (progn
      (setq compile-command "mvn clean install")
      (local-set-key (kbd "C-c C-c") 'compile))))

(defun mp-schema-validation-setup ()
  (add-to-list 'rng-schema-locating-files
               "~/.emacs.d/schemas/schemas.xml"))

(add-hook 'nxml-mode-hook 'mp-maven-integration)
(add-hook 'nxml-mode-hook 'mp-schema-validation-setup)

;; ]

;; [ ido & co

(setq ido-enable-flex-matching t
      ido-use-filename-at-point 'guess)

(ido-mode t)
(ido-everywhere) ;; ido for all buffer/file reading

(define-key ido-common-completion-map (kbd "C-n") 'ido-next-match)
(define-key ido-common-completion-map (kbd "C-p") 'ido-prev-match)

(use-package smex
  ;; https://github.com/nonsequitur/smex
  :config
  (smex-initialize)
  (global-set-key (kbd "M-x") #'smex))

(use-package ido-vertical-mode
  :disabled
  :config
  (ido-vertical-mode))

(use-package ido-grid-mode
  :config
  (ido-grid-mode) )

(use-package ido-ubiquitous ;; have ido not only for buffer/file reading
  :config
  (ido-ubiquitous-mode) )



;; [ Where was I [editing text]?

(defun mp-store-lot-position ()
  (when (not (or 
              (string-prefix-p "*" (buffer-name))
              (string-prefix-p " " (buffer-name))))
    (point-to-register ?z)))

(defun mp-goto-lot-position ()
  (interactive)
  (jump-to-register ?z))

(add-hook 'post-self-insert-hook 'mp-store-lot-position)

(global-set-key (kbd "C-c l") 'mp-goto-lot-position)

;; ]

;; [ occur

;; Tip: In the buffer where occur was called use "M-g M-n" and
;;      "M-g M-p" to browse through the occurances
;; Be sure to use "q" to exit *occur* buffer. Otherwise highlighted
;; regular expressions are not unhighlighted.

(defun mp-occur-mark-regexp ()
  "If no regexp was marked until now do it now."
  (interactive)
  (when (not mp-occur-marked-regexp)
    (setq mp-occur-origin-buffer (current-buffer))
    (highlight-regexp mp-occur-last-regexp)))

(defun mp-occur-next-line ()
  (interactive)
  (next-line)
  (occur-mode-goto-occurrence)
  (recenter-top-bottom 2)
  (mp-occur-mark-regexp)
  (other-window 1))

(defun mp-occur-prev-line ()
  (interactive)
  (previous-line)
  (occur-mode-goto-occurrence)
  (recenter-top-bottom 2)
  (mp-occur-mark-regexp)
  (other-window 1))

(defun mp-occur-quit-window ()
  (interactive)
  (other-window 1)
  (unhighlight-regexp mp-occur-last-regexp)
  (other-window 1)
  (setq mp-occur-marked-regexp nil
        mp-occur-last-regexp nil
        mp-occur-origin-buffer nil)
  (quit-window) )

(defun mp-occur-goto-occurence ()
  (interactive)
  (occur-mode-goto-occurrence)
  (unhighlight-regexp mp-occur-last-regexp))

(global-set-key (kbd "C-o") #'occur)

(defvar mp-occur-marked-regexp nil)
(defvar mp-occur-last-regexp nil)
(defvar mp-occur-origin-buffer nil)

(defadvice occur (after select-occur-window-after-occur activate)
  "Do make *Occur* buffer current after calling occur."
  (let ((occur-window (get-buffer-window "*Occur*")))
    (when occur-window
      (select-window occur-window)))
  (setq mp-occur-last-regexp (car regexp-history)))

(defun mp-occur-mode-hook ()
  (local-set-key (kbd "C-n") 'mp-occur-next-line)
  (local-set-key (kbd "C-p") 'mp-occur-prev-line)
  (local-set-key (kbd "<return>") 'mp-occur-goto-occurence)
  (local-set-key (kbd "q") 'mp-occur-quit-window))

(add-hook 'occur-mode-hook 'mp-occur-mode-hook)

;; ]

;; [ smarter comments

(defun mp-smart-enter ()
  (interactive)
  (let ((in-comment nil))
    (save-excursion 
      (backward-char)
      (when (eq font-lock-comment-face (face-at-point t))
        (setq in-comment t)))
    (newline)
    (when in-comment
      (progn
        (insert comment-start)
        (indent-for-comment)))))

(global-set-key (kbd "<RET>") 'mp-smart-enter)

;; ]

;; [ python

(use-package elpy)

(defun mp-electric-= ()
  (interactive)
  (when (not (eq font-lock-string-face (face-at-point t)))
    (when (not (looking-back " "))
      (insert " ") )
    (insert "= ") ) )

(defun mp-python-mode-hook ()
  "Personal python mode hook extension."
  (auto-fill-mode 1)
  (elpy-mode)
  (setq-local comment-auto-fill-only-comments t)
  (setq-local comment-multi-line t)
  (setq python-indent-offset 4)
  (local-set-key (kbd "M-;") 'comment-dwim)
  (local-set-key (kbd "=") 'mp-electric-=) )

(add-hook 'python-mode-hook 'mp-python-mode-hook)

(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.py2\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.py3\\'" . python-mode))

(add-to-list 'auto-insert-alist '(".*\\.py3?$" . [ "template.py3" ] ) )
(add-to-list 'auto-insert-alist '(".*\\.py2$" . [ "template.py" ] ) )

;; ]

;; [ restclient mode

(use-package restclient
  :config
  (add-to-list 'auto-mode-alist '("\\.rcm\\'" . restclient-mode)))

;; ]

;; [ ispell mode

;; make sure ispell is in your path or add it here

;; (add-to-list 'exec-path "C:/mp-aspell/bin/")


(setq ispell-program-name "aspell"
      ispell-personal-dictionary "~/.emacs.d/dict")

;; ]

;; [ html editing web mode

(use-package web-mode
  :config
  (add-hook 'web-mode-hook 'mp-web-mode-extension)
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))

(use-package ac-html
  :after web-mode)

(defun mp-web-mode-extension ()
  (interactive)
  (make-local-variable 'ac-use-quick-help)  
  (setq indent-tabs-mode nil
        web-mode-markup-indent-offset 4
        ac-use-quick-help t
        ac-sources '(ac-source-yasnippet ac-source-html))
  (hs-minor-mode))

(require 'hideshow)
(require 'sgml-mode)

(add-to-list 'hs-special-modes-alist
             '(web-mode
               "<!--\\|<[^/>]*[^/]>"
               "-->\\|</[^/>]*[^/]>"
               "<!--"
               sgml-skip-tag-forward
               nil))

(define-key web-mode-map (kbd "C--") 'hs-toggle-hiding)

(defun mp-html-post-processing ()
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
    mp-html-post-processing ] )

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
    mp-html-post-processing ] )

(defun mp-html-project-post-processing (name)
  "This method looks for strings %CSSFILE% and %TITLE% and replaces them with some meaningful values ."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "%TITLE%" nil t)
      (replace-match name))
    (goto-char (point-min))
    (when (re-search-forward "%CSSFILE%" nil t)
      (replace-match (replace-regexp-in-string (regexp-quote ".html") ".css" (buffer-name) 'fixedcase) 'fixedcase))))

(defun mp-start-web-project (name)
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
    (mp-html-project-post-processing name)))

(global-set-key (kbd "C-c 4") #'mp-start-web-project)

(defun mp-css-mode-hook ()
  "Personal css mode hook extender."
  (setq ac-sources '(ac-source-yasnippet ac-source-css-property ac-source-words-in-same-mode-buffers)))

(add-hook 'css-mode-hook' mp-css-mode-hook)

;; ]

;; [ php mode

(defun mp-php-mode-extension ()
  (setq indent-tabs-mode nil
        c-basic-offset 4
        php-template-compatibility nil) )

;; see https://github.com/xcwen/ac-php
(use-package ac-php)

(defun mp-setup-ac-php ()
  "Turn on auto-complete mode and set ac-sources for ac-php."
  (auto-complete-mode)
  (setq ac-sources  '(ac-source-yasnippet ac-source-php) )
  (require 'ac-php) )

(add-hook 'php-mode-hook 'mp-setup-ac-php)

(use-package php-mode
  :config
  (add-hook 'php-mode-hook 'mp-php-mode-extension) )

;; ]

;; [ shell script mode

(defun mp-elisp-post-processor ()
  (interactive)
  (let ((match-found t))
    (progn
      (goto-char (point-min))
      (when (re-search-forward "_" nil t)
        (replace-match "") ) ) ) )

(add-to-list 'auto-insert-alist
             '(".*\\.sh$" . [ "template.sh" mp-elisp-post-processor] ) )

(defun mp-shell-mode-extender ()
  (interactive)
  (define-key ac-mode-map (kbd "C-c /") 'ac-complete-filename) )

(add-hook 'shell-mode-hook 'mp-shell-mode-extender)
;; ]

;; [ tramp

;; Note: use /ssh:hostname.domain:/path/to/file to access remote files.
;; For ease of use: Make sure that ssh access is granted via public key

(setq tramp-default-method "ssh"
      tramp-auto-save-directory "~/.emacs.d/tramp-auto-save/")

;; ]

;; [ makefiles

(add-to-list 'auto-insert-alist '("Makefile" . [ "Makefile" ] ))

;; ]

;; [ shared objects elf mode

(when (eq system-type 'gnu/linux)
  (use-package elf-mode
    :init
    (add-to-list 'auto-mode-alist '("\\(\\.\\(?:o\\|so\\.\\(?:[0-9]\\.[0-9]\\.[0-9]\\|[0-9]\\.[0-9]\\|[0-9]\\)\\)\\)\\'" . elf-mode))))

(add-to-list 'auto-mode-alist '("\.dll\\'" . hexl-mode))
(add-to-list 'auto-mode-alist '("\.exe\\'" . hexl-mode))

;; ]


;; [ which function mode

;; this is a global minor mode and displays the name
;; of the function that surrounds point. To look into
;; how it works look at which-func-* variables.
;; TODO: Want to do this:
;; http://emacs.stackexchange.com/questions/28104/customize-in-which-func-mode

(which-function-mode)

(setq which-func-unknown "∅")

;; ]

;; [ eldoc

(setq eldoc-echo-area-use-multiline-p t)

(global-eldoc-mode)

;; ]

;; [ magit

(use-package magit

  :config

  (defun mp-magit-status (arg)
    "Start magit. With prefix argument start magit in new frame."
    (interactive "P")
    (when arg
      (select-frame (make-frame '((name . "Magit")))))
    (magit-status)
    (when arg
      (delete-other-windows) ) )

  (global-set-key (kbd "<f12>") 'mp-magit-status) )

;; ]

;; [ all things mode-line

(use-package powerline
  :disabled)

(setq-default mode-line-format (list
                                "%e" "[" mode-line-mule-info mode-line-client mode-line-modified "] "
                                '(:eval
                                  (propertize "[%b] " 'help-echo (buffer-file-name)))
                                ;; line and column
                                "[" ;; '%02' to set to 2 chars at least; prevents flickering
                                (propertize "%03l") ","
                                (propertize "%03c")
                                "] "
                                (mp-sunrise-sunset-for-modeline)
                                " [" '(vc-mode vc-mode) " ] " mode-line-misc-info))

;; ]
