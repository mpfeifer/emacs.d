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

(defvar java-project-root (concat (getenv "HOME") "/src"))

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

(global-set-key (kbd "C-c 4") 'start-website)


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
  "Create a simple maven project using jar packaging."
  (interactive "MGroup-id: \nMArtifact-id: \nMVersion-number: ")
  (let* ((project-root (concat (expand-file-name java-project-root) artifact-id))
         (target-pom (concat project-root "/pom.xml"))
         (src-dir (concat project-root "/src/main/java/"))
         (premature-exit nil)
         (main-class (concat src-dir
                             (replace-regexp-in-string "\\." "/" group-id)
                             "/" 
                             artifact-id
                             ".java"))
         (class-dir (file-name-directory main-class))
         (pframe (make-frame))
         (default-directory project-root))
    (if (not (file-exists-p project-root))
        (make-directory project-root t)
      (when (yes-or-no-p "Directory already exists. Continue?" )
        (setq premature-exit t))
      (when (not premature-exit)
        (progn
          (copy-template "pom.xml" target-pom
                         (list (list 'GROUP-ID group-id)
                               (list 'ARTIFACT-ID artifact-id)
                               (list 'VERSION version-number)))
          (when (not (file-exists-p class-dir))
            (make-directory class-dir t)))))
    (select-frame pframe)
    (find-file main-class)
    (save-buffer)))

(defvar mpx-promt-project-root-indicator-files (list "pom.xml" "angular.json" ".git"))

(defun mpx-promt-project-indicator-in-cwd-p ()
  "Set if a project root can be found. If so store the root directory and set a buffer local variable"  
  (let ((known-project-files mpx-promt-project-root-indicator-files))
    (if (member t (mapcar 'file-exists-p known-project-files))
        (progn
          (when (not (member default-directory mpx-promt-projects)) ;; side effect: add directory to list of known directories
            (mpx-promt-add-project default-directory))
          t)
      nil)))

(defun mpx-get-working-directory ()
  "Calculate current working directory (obsolete)."
  (if (and (fboundp 'dired-mode)
           (eq major-mode 'dired-mode))
      dired-directory
    (file-name-directory (buffer-file-name))))

(defun mpx-find-closest-project-indicator ()
  "Traverse the directory tree upwards and look for the next project indicator file."
  (interactive)
  (let ((cwd default-directory)
        (cwd-bak default-directory)
        (cwd-last nil)
        (keep-going t)
        (result nil))
    (progn
      (while keep-going
        (progn
          (if (mpx-promt-project-indicator-in-cwd-p)
              (progn
                (setq result cwd)
                (setq keep-going nil))
            (progn
              (cd "..")
              (setq cwd-last cwd
                    cwd default-directory)
              (when (string= cwd-last cwd)
                (setq keep-going nil))))))
      (setq default-directory cwd-bak)      
      result)))

(defun mpx-promt-standard-hook-extender ()
  (let ((project-root (mpx-find-closest-project-indicator)))
    (when project-root
      (progn
        (message (format "Found project root %s for directory %s" project-root default-directory))
        (setq-local mpx-promt-project-root project-root)
        (when (not (member project-root mpx-promt-projects))
          (mpx-promt-add-project project-root))))))

(add-hook 'find-file-hook 'mpx-promt-standard-hook-extender)
(add-hook 'dired-mode-hook 'mpx-promt-standard-hook-extender)

(defun mpx-promt-find-project-for-path (file-name)
  (interactive)
  (if file-name
      (let ((result nil)
            (stop nil)
            (project nil)
            (projects mpx-promt-projects)
            (directory (expand-file-name (file-name-directory file-name))))
	(while (and (not stop) projects)
	  (progn
            (setq project (car projects)
		  projects (cdr projects))
            (when (string-prefix-p project directory)
              (setq stop t
                    result project))))
	result)
    nil))

(defun mpx-find-file-in-project ()
  "Start find-file-in-project at project-root directory"
  (interactive)
  (let ((project-root (mpx-promt-find-project-for-path buffer-file-name)))
    (if project-root
        (let ((default-directory project-root))
          (find-file-in-project))
      (if (and (fboundp 'mpx-promt-project-root)
               (not (null mpx-promt-project-root)))
          (let ((default-directory mpx-promt-project-root))
            (find-file-in-project))
        (find-file-in-project)))))

;; Save list of encountedred projects into file MPX-PROMT-PROJECTS-FILE

(defvar mpx-promt-projects-file (concat (file-name-directory user-init-file) "promt.txt"))
(defvar mpx-promt-projects (list nil))

(defun mpx-neotree-show-project ()
  "Show project directory in neotree"
  (let ((project-dir (completing-read "Which project? " mpx-promt-projects)))
    (neotree-dir project-dir)))

(defun mpx-promt-load-current-projects ()
  "Retrieve list of currently encountered projects."
  (let ((projects nil))
    (with-temp-buffer (insert-file-contents-literally mpx-promt-projects-file)
                      (setq projects (buffer-substring-no-properties (point-min) (point-max))))
    (setq mpx-promt-projects (split-string projects))
    mpx-promt-projects))

(defun mpx-prompt-remove-not-existing-projects ()
  "Load list of known projects. Check if each exists. If not remove it from list. Store remaining existing projects."
  (let ((updated-projects (seq-filter 'file-exists-p (mpx-promt-load-current-projects))))
    (mpx-promt-store-current-projects updated-projects)
    updated-projects))

(mpx-promt-load-current-projects)

(defun mpx-promt-store-current-projects (projects)
  "Store PROJECTS as list of current projects."
  (with-temp-buffer
    (progn
      (dolist (project projects)
        (progn
          (insert project)
          (newline)))
      (write-file mpx-promt-projects-file))))

(defun mpx-promt-add-project (root-folder)
  "Add ROOT-FOLDER to list of known projects."
  (message (concat "Adding " root-folder " to list of known projects"))
  (let ((projects (mpx-promt-load-current-projects)))
    (add-to-list 'projects root-folder)
    (mpx-promt-store-current-projects projects)))
  
(defun find-file-dispatcher (arg)
  "If on an org-link open it.
If point is on any file open it.
If argument is given use mpx-find-file-in-project
Otherweise plain find-file."
  (interactive "P")
  (if (and
       (eq major-mode 'org-mode)
       (eq (get-char-property (point) 'face) 'org-link))
      (org-open-at-point)
    (let ((file-at-point (thing-at-point 'filename)))
      (if (eq major-mode 'dired-mode)
          (call-interactively #'mpx-find-file-in-project)
        (if (and file-at-point
                 (file-exists-p file-at-point))
            (progn
              (message (format "find-file-dispatcher found file at point: %s" file-at-point))
              (find-file file-at-point))
          (call-interactively   (if arg
                                    'mpx-find-file-in-project
                                  'find-file)))))))

(global-set-key (kbd "C-x C-f") 'find-file-dispatcher)

(defun mpx-add-tags-file (tags-file)
  "Conveniance helper to add TAGS-FILE to tags-table-list."
  (interactive "fWhich TAGS file? ")
  (if (member tags-file tags-table-list)
      (message (format "Tags file %s is already in tags-table-list" tags-file))
    (progn
      (message (format "Tags file %s added to tags-table-list" tags-file))
      (add-to-list 'tags-table-list tags-file  t nil))))

(defvar mpx-help-on-tags-buffer nil)

(defun mpx-help-on-tags ()
  (interactive)
  (setq mpx-help-on-tags-buffer (get-buffer-create "*tags-help*"))
  (with-current-buffer mpx-help-on-tags-buffer
    (let ((tmp-string ""))
      (erase-buffer)
      (insert (format "%-24s: %s" "tags-table-files" (pretty-print-list tags-table-files)))
      (newline)
      (insert (format "%-24s: %s" "tags-file-name" (if (null tags-file-name) "nil" tags-file-name)))
      (newline)
      (insert (format "%-24s: %s" "tags-table-list" (pretty-print-list-line-by-line tags-table-list)))
      (newline)))
  (display-buffer-pop-up-window mpx-help-on-tags-buffer '((inhibit-switch-frame t))))

(defun mpx-load-closest-tags-file ()
  "Traverse the directory tree upwards and look for the next pom.xml and TAGS file"
  (interactive)
  (let ((cwd-bak default-directory)
        (cwd-last nil)
        (cwd default-directory)
        (keep-going t)
        (tags-file-found nil))
    (progn
      (while keep-going
        (progn
          ;;          (message "Checking directory " cwd)
          (if (file-exists-p "TAGS")
              (progn
                (mpx-add-tags-file (concat cwd "TAGS"))
                (setq keep-going nil
                      tags-file-found t))
            (progn
              (cd "..")
              (setq cwd-last cwd
                    cwd default-directory)
              (when (string= cwd-last cwd)
                (setq keep-going nil))))))
      (setq default-directory cwd-bak)
      (message (if tags-file-found
                   "TAGS file was found and added to tags-table-list"
                 "No TAGS file was found"))
      t)))

(defun mpx-open-project-directory-in-dired (project-dir)
  "Open PROJECT-DIR in new frame using dired. If called interactively
query user for directory."
  (interactive (list (completing-read "Which project? " mpx-promt-projects)))
  (dired-other-frame project-dir))

(defun mpx-find-component ()
  "Do completing read on all filenames indicating component files.
ie. Files that match the regexp .*.component.ts. Only files below
mpx-prompt-project-root/src are considered."
  (interactive)
  (completing-read "Welche Komponente suchst du? "
                   (directory-files-recursively
                    (concat mpx-promt-project-root "/src/") ".*\\.component\\.ts" )))

(provide 'promt)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; promt.el ends here
