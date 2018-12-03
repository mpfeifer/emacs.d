;;; java-x.el --- 
;; 
;; Description: Extensions to emacs lisp for working with Java source files
;; Author: Matthias
;; Keywords: java
;; Dependencies: none
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Changelog: 
;; 
;; 1.0 (Wed Aug 22 10:52:34 2018): Initial release
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

(defun mpx-java-mode-set-buffer-locals ()
  "Called from java-mode-hook sets buffer local variables for java-mode."
  (defvar-local java-classpath nil "Java classpath. This will be set by .dir-locals.el (hopefully).")
  (defvar-local java-local-project-root nil "Buffer local location of current project root.")
  (defvar-local java-classes-cache nil "Cache for the current classpath classes."))

(add-hook 'java-mode-hook 'mpx-java-mode-set-buffer-locals)

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

(defun java-mode-process-dir-locals ()
  (when (derived-mode-p 'java-mode)
    (progn
      (when (stringp java-local-project-root)
        ;; sell the stock from emacs-maven-plugin:
        (progn
          (setq-local java-classes-cache (java-read-classes-from-classpath java-classpath) ))
        (local-set-key (kbd "C-x c") 'java-insert-classname-completing-read)))))

(add-hook 'hack-local-variables-hook 'java-mode-process-dir-locals)

(provide 'java-x)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; java-x.el ends here
