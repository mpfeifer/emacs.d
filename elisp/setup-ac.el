;;; setup-ac.el --- 
;; 
;; Description: 
;; Author: Matthias
;; Keywords: 
;; Dependencies: 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Changelog: 
;; 
;;  (Mon Jul  8 13:30:36 2019): Initial release
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:


;; [ auto-complete / company

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




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; setup-ac.el ends here
