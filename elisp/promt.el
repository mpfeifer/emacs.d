;;; promt.el  --- 
;; 
;; Description: A set of project management tools
;; Author: Matthias
;; Keywords: Project management tools
;; Dependencies: 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Changelog: 
;; 
;; VERSION (24.12.2018): Initial release
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
    (write-file target-file nil)))

(defun start-website (name)
  "Create a new web site with initial html, js and css file. NAME will be the project folder."
  (interactive "MSite name? ")
  (let ((projectroot (concat mpx-src-dir "/" name)))
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
    (copy-file "~/.emacs.d/templates/qunit-2.0.1.js" (concat projectroot "/"))
    (copy-file "~/.emacs.d/templates/qunit-2.0.1.css" (concat projectroot "/"))
    (switch-to-buffer (concat name ".html"))
    (html-project-post-processing name)))

(defun start-new-web-application (group-id artifact-id version-number)
  "Creates a new maven war project by calling maven archetype for web applications."
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
      ;; Note: Maven should be in the PATH
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
          (copy-template "web-3.0.xml" target-web-xml
                         (list 
                          (list 'DISPLAY-NAME (format "%s %s" artifact-id version-number))))))
      (goto-char (point-max)) ) ) )

(defun start-new-maven-project (group-id artifact-id version-number)
  "Create a simple maven project using jar packaging.
Last tested: 15.07.19."
  (interactive "MGroup-id: \nMArtifact-id: \nMVersion-number: ")
  (let* ((project-root (concat (expand-file-name java-project-root) "/" artifact-id))
         (target-pom (concat project-root "/pom.xml"))
         (src-dir (concat project-root "/src/main/java/"))
         (test-dir (concat project-root "/src/test/java/"))
         (premature-exit-f nil)
         (main-class (concat src-dir
                             (replace-regexp-in-string "\\." "/" group-id)
                             "/" 
                             artifact-id
                             ".java"))
         (main-class-dir (file-name-directory main-class))
         (test-main-class (concat test-dir
                             (replace-regexp-in-string "\\." "/" group-id)
                             "/Test" 
                             artifact-id
                             ".java"))
         (test-main-class-dir (file-name-directory test-main-class))
         (pframe (make-frame))
         (default-directory project-root))
    (if (not (file-exists-p project-root))
        (make-directory project-root t)
      (when (yes-or-no-p "Directory already exists. Continue?" )
        (setq premature-exit-f t)))
    (when (not premature-exit-f)
      (progn
        (copy-template "pom.xml" target-pom
                       (list (list 'GROUP-ID group-id)
                             (list 'ARTIFACT-ID artifact-id)
                             (list 'VERSION version-number)))
        (when (not (file-exists-p main-class-dir))
          (make-directory main-class-dir t))
        (when (not (file-exists-p test-main-class-dir))
          (make-directory test-main-class-dir t))))
    (select-frame pframe)
    (find-file test-main-class)
    (save-buffer)
    (split-window-horizontally)
    (find-file main-class)
    (save-buffer)))

(provide 'promt)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; promt.el ends here
