;; empty
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Info-additional-directory-list (quote ("~/.emacs.d/info/")))
 '(auto-save-list-file-prefix "~/.emacs.d/auto-save/saved-")
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(c-basic-offset 4)
 '(calendar-mark-diary-entries-flag t)
 '(calendar-view-diary-initially-flag t)
 '(comment-auto-fill-only-comments t)
 '(compilation-window-height 14)
 '(custom-safe-themes
   (quote
    ("98cc377af705c0f2133bb6d340bf0becd08944a588804ee655809da5d8140de6" "5dc0ae2d193460de979a463b907b4b2c6d2c9c4657b2e9e66b8898d2592e3de5" "2a3997ddecc3b3196649f72855390b346cc27f5c13d0a420b26d23b739942046" "72c7c8b431179cbcfcea4193234be6a0e6916d04c44405fc87905ae16bed422a" "b0ab5c9172ea02fba36b974bbd93bc26e9d26f379c9a29b84903c666a5fde837" "e56ee322c8907feab796a1fb808ceadaab5caba5494a50ee83a13091d5b1a10c" "e97dbbb2b1c42b8588e16523824bc0cb3a21b91eefd6502879cf5baa1fa32e10" "2305decca2d6ea63a408edd4701edf5f4f5e19312114c9d1e1d5ffe3112cde58" "d4e9f95acd51433b776f1127143bbc7d0f1d41112d547e5b7a9a506be369dc39" "5b29f90eb304b440c908de31caf7d730db451b5909e8a84a2e7cd8d60f6d5c1f" "b869a1353d39ab81b19eb79de40ff3e7bb6eaad705e61f7e4dbdcb183f08c5a6" "6998bd3671091820a6930b52aab30b776faea41449b4246fdce14079b3e7d125" "7e376fb329a0e46a04e8285b0e45199a083f98c69b0e1039ec1cb1d366e66e9c" "beeb5ac6b65fcccfe434071d4624ff0308b5968bf2f0c01b567d212bcaf66054" "12b4427ae6e0eef8b870b450e59e75122d5080016a9061c9696959e50d578057" "e87a2bd5abc8448f8676365692e908b709b93f2d3869c42a4371223aab7d9cf8" "d606ac41cdd7054841941455c0151c54f8bff7e4e050255dbd4ae4d60ab640c1" "fbcdb6b7890d0ec1708fa21ab08eb0cc16a8b7611bb6517b722eba3891dfc9dd" "8e7ca85479dab486e15e0119f2948ba7ffcaa0ef161b3facb8103fb06f93b428" "28ec8ccf6190f6a73812df9bc91df54ce1d6132f18b4c8fcc85d45298569eb53" "1db337246ebc9c083be0d728f8d20913a0f46edc0a00277746ba411c149d7fe5" "ad950f1b1bf65682e390f3547d479fd35d8c66cafa2b8aa28179d78122faa947" "8577da1641ed4bdf255341ca92e3d0e49c9f4d574458f09ce78159690442cade" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(desktop-modes-not-to-save (quote (tags-table-mode prodigy-mode)))
 '(desktop-path (quote ("~/.emacs.d/desktop")))
 '(diary-file "~/.emacs.d/diary")
 '(elpy-modules
   (quote
    (elpy-module-company elpy-module-eldoc elpy-module-pyvenv elpy-module-highlight-indentation elpy-module-yasnippet elpy-module-sane-defaults)))
 '(elpy-project-root-finder-functions
   (quote
    (elpy-project-find-projectile-root elpy-project-find-python-root elpy-project-find-git-root elpy-project-find-svn-root)))
 '(elpy-rpc-error-timeout 30)
 '(elpy-rpc-timeout 2)
 '(eshell-directory-name "~/.emacs.d/")
 '(fit-window-to-buffer-horizontally t)
 '(global-linum-mode nil)
 '(help-window-select t)
 '(ibuffer-default-sorting-mode (quote alphabetic))
 '(ibuffer-saved-filter-groups
   (quote
    (("groupmode"
      ("emacs-lisp-mode"
       (mode . emacs-lisp-mode))
      ("dired-mode"
       (mode . dired-mode))
      ("minibuffer-inactive-mode"
       (mode . minibuffer-inactive-mode))
      ("fundamental-mode"
       (mode . fundamental-mode))
      ("org-mode"
       (mode . org-mode))
      ("nxml-mode"
       (mode . nxml-mode))
      ("conf-javaprop-mode"
       (mode . conf-javaprop-mode))
      ("cperl-mode"
       (mode . cperl-mode))
      ("occur-mode"
       (mode . occur-mode))
      ("lisp-interaction-mode"
       (mode . lisp-interaction-mode))
      ("messages-buffer-mode"
       (mode . messages-buffer-mode))
      ("compilation-mode"
       (mode . compilation-mode))
      ("Info-mode"
       (mode . Info-mode))
      ("diary-mode"
       (mode . diary-mode))
      ("calendar-mode"
       (mode . calendar-mode))
      ("help-mode"
       (mode . help-mode))
      ("elisp-byte-code-mode"
       (mode . elisp-byte-code-mode))))))
 '(ibuffer-saved-filters
   (quote
    (("gnus"
      ((or
        (mode . message-mode)
        (mode . mail-mode)
        (mode . gnus-group-mode)
        (mode . gnus-summary-mode)
        (mode . gnus-article-mode))))
     ("programming"
      ((or
        (mode . emacs-lisp-mode)
        (mode . cperl-mode)
        (mode . c-mode)
        (mode . java-mode)
        (mode . idl-mode)
        (mode . lisp-mode)))))))
 '(ido-cr+-max-items nil)
 '(ido-cr+-replace-completely t)
 '(ido-create-new-buffer (quote always))
 '(ido-default-buffer-method (quote selected-window))
 '(ido-grid-mode t)
 '(ido-grid-mode-padding "|")
 '(ido-ubiquitous-function-overrides
   (quote
    ((disable exact "read-file-name")
     (disable exact "read-file-name-internal")
     (disable exact "read-buffer")
     (disable exact "gnus-emacs-completing-read")
     (disable exact "gnus-iswitchb-completing-read")
     (disable exact "grep-read-files")
     (disable exact "magit-builtin-completing-read")
     (enable exact "bookmark-completing-read")
     (enable-old exact "webjump-read-choice")
     (enable-old exact "webjump-read-url-choice")
     (disable exact "isearchp-read-unicode-char")
     (enable exact "read-char-by-name")
     (disable exact "Info-read-node-name")
     (disable exact "tmm-menubar")
     (enable exact "imenu--completion-buffer")
     (enable-old exact "auto-insert")
     (enable exact "completing-read"))))
 '(ido-use-url-at-point t)
 '(ido-use-virtual-buffers (quote auto))
 '(ido-vertical-pad-list nil)
 '(ido-vertical-show-count t)
 '(ispell-complete-word-dict "~/.emacs.d/ispell-completion.dict")
 '(ispell-personal-dictionary "~/.emacs.d/ispell.dict")
 '(ispell-silently-savep t)
 '(java-project-root "~/java/")
 '(linum-format "%5d")
 '(minimap-dedicated-window nil)
 '(neo-force-change-root t)
 '(neo-smart-open t)
 '(neo-theme (quote classic))
 '(neo-window-width 30)
 '(next-error-recenter (quote (4)))
 '(nxml-slash-auto-complete-flag t)
 '(org-clock-mode-line-total (quote today))
 '(package-selected-packages
   (quote
    (hs-minor-mode ido-grid-mode wndmove bookmark+ adoc-mode restclient ac-html fuzzy javadoc-lookup helm help material ac-etags theme-changer gnuplot-mode zerodark-theme org-mobile-sync ldap-mode magit sunny-day-theme sunshine elf-mode volatile-highlights ac-slime ac-php web-mode auto-complete yatemplate aggressive-indent ac-ispell yaml-mode org-bullets ibuffer-git ibuffer-projectile projectile projectile-speedbar htmlize paredit edit-server auto-compile neotree elpy material-theme log4e company-jedi autumn-light-theme paper-theme soft-morning-theme org-ac foggy-night-theme yasnippet use-package sr-speedbar solarized-theme smex prodigy php-mode js2-mode jedi-direx ido-vertical-mode ido-ubiquitous hungry-delete expand-region elisp-slime-nav dracula-theme color-theme-solarized avy aurora-theme anti-zenburn-theme ample-zen-theme ample-theme airline-themes ahungry-theme afternoon-theme)))
 '(projectile-cache-file "~/.emacs.d/projectile.cache")
 '(projectile-known-projects-file "~/.emacs.d/projectile-bookmarks.eld")
 '(python-environment-directory "~/.emacs.d/python-environments")
 '(python-environment-virtualenv
   (quote
    ("virtualenv.exe" "--system-site-packages" "--quiet")))
 '(python-shell-interpreter "python3")
 '(recentf-save-file "~/.emacs.d/recentf")
 '(ring-bell-function (quote ignore))
 '(smex-save-file "~/.emacs.d/smex-items")
 '(speedbar-fetch-etags-arguments (quote ("--declarations" "-D" "-I" "-o" "-")))
 '(speedbar-obj-do-check nil)
 '(speedbar-sort-tags t)
 '(speedbar-tag-hierarchy-method (quote (speedbar-sort-tag-hierarchy)))
 '(speedbar-use-imenu-flag nil)
 '(speedbar-verbosity-level 2)
 '(sr-speedbar-right-side t)
 '(temp-buffer-max-height (lambda (buffer) (min 12 (- (frame-height) 10))))
 '(temp-buffer-max-width (lambda (buffer) (min 75 (- (frame-width) 10))))
 '(temp-buffer-resize-mode t)
 '(truncate-lines t)
 '(url-cookie-file "~/.emacs.d/cookies")
 '(visible-bell nil)
 '(woman-use-own-frame t)
 '(woman-use-topic-at-point-default t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diary ((t (:background "cornflower blue" :foreground "yellow1")))))

