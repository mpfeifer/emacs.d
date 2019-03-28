;;; angulare-helpers.el --- 
;; 
;; Description: Set of functions to assist in angular2 editing
;; Author: Matthias
;; Keywords: angular2
;; Dependencies: 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Changelog: 
;; 
;; 1.0 (Mon Dec 17 10:59:18 2018): Initial release
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


(require 'promt)

(defun mpx-find-component-files ()
  "Get all a list of al file in current project that match the pattern .*.component.ts"
  (directory-files-recursively
   (concat mpx-promt-project-root "/src/") ".*\\.component\\.ts" ))

(defun mpx-find-component-files-for-tag (tag)
  (let* ((component-files (mpx-find-component-files))
         (component-file (seq-filter #'(lambda (file)
                                         (with-temp-buffer
                                           (insert-file file)
                                           (goto-char (point-min))
                                           (if (search-forward (format "selector: '%s'" tag) nil t)
                                               t
                                             nil)))
                                     component-files)))
    component-file))

(defun mpx-angular-symbol-at-point ()
  "Find symbol at point. Everything from last seperator until next.
Where separators are <, >, <whitespace> or / "
  (save-excursion
    (let ((beos nil)
          (eos nil))
      (re-search-backward "[></[:space:]]" nil t)
      (forward-char 1)
      (setq beos (point))
      (re-search-forward "[></[:space:]]" nil t)
      (backward-char 1)
      (setq eos (point))
      (buffer-substring-no-properties beos eos))))

(defun mpx-find-component-for-tag (tag)
  (interactive (list (mpx-angular-symbol-at-point)))
  (let ((component-file (mpx-find-component-files-for-tag tag)))
    (cond 
     ((eq 0 (length component-file))
      (message (concat "No file found that provides the tag " tag)))
     ((eq 1 (length component-file))
      (find-file (car component-file)))
     ((< 1 (length component-file))
      (find-file (completing-read "Which file do you want to open?" component-files ))))))

(defun mpx-open-component-for-tag-in-frame (tag)
  "Look for component that defines TAG. Assume a component consists of three files (a css file, a ts file and a html file)
and open all of these in a new frame like this:

+------------------------+---------------+
| .ts-file               |.html-file     |
|                        |               |
|                        |               |
|                        |               |
|                        |               |
|                        +---------------+
|                        |.css-file      |
|                        |               |
|                        |               |
|                        |               |
|                        |               |
+------------------------+---------------+

TODO: find files in the file system"
  (interactive (list (mpx-angular-symbol-at-point)))
  (let* ((component (mpx-find-component-files-for-tag tag))
         (tsfile (cond
                  ((eq 1 (length component))
                   (car component))
                  ((< 1 (length component))
                   (completing-read
                    "There are multiple files available. Which one are you lookingg for?"
                    component))
                  (t nil))))
    (if tsfile
        (let ((css-file (replace-regexp-in-string "\\.ts$" ".css" tsfile))
              (ts-file tsfile)
              (html-file (replace-regexp-in-string "\\.ts$" ".html" tsfile)))
          (select-frame (make-frame))
          (find-file ts-file)
          (split-window-right-select (find-file html-file))
          (split-window-below-select (find-file css-file)))
      (message "No file found matching component"))))

(defun mpx-open-component-in-frame (component-file)
  "Assume a component consists of three files (a css file, a ts file and a html file)
and open all of these in a new frame like this:

+------------------------+---------------+
| .ts-file               |.html-file     |
|                        |               |
|                        |               |
|                        |               |
|                        |               |
|                        +---------------+
|                        |.css-file      |
|                        |               |
|                        |               |
|                        |               |
|                        |               |
+------------------------+---------------+

TODO: find files in the file system"
  (interactive (list
                (completing-read
                 "Which componennt do you want to edit?"
                 (mpx-find-component-files))))
  (let ((css-file (replace-regexp-in-string "\\.ts$" ".css" component-file))
        (ts-file component-file)
        (html-file (replace-regexp-in-string "\\.ts$" ".html" component-file)))
    (select-frame (make-frame))
    (find-file component-file)
    (split-window-right-select)
    (find-file html-file)
    (split-window-below-select)
    (find-file css-file)))

(define-key ng2-html-map (kbd "C-c C-.") 'mpx-find-component-for-tag)    

(provide 'angular-helpers)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; angulare-helpers.el ends here
