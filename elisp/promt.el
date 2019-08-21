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
;; 1.0 (24.12.2018): Initial release
;; 1.1 (15.07.2019): Redesign
;;
;;
;;
;;
;;
;;
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Description:
;;
;; Some lines of code to bootstrap some small projects for.
;;
;; TODO
;;
;; - which project types are there
;; - offer a nice hydra that allows project creation
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

(defun html-project-post-processing (name)
  "This method looks for strings %CSSFILE% and %TITLE% and replaces them with some meaningful values ."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "%TITLE%" nil t)
      (replace-match name))
    (goto-char (point-min))
    (when (re-search-forward "%CSSFILE%" nil t)
      (replace-match (replace-regexp-in-string (regexp-quote ".html") ".css" (buffer-name) 'fixedcase) 'fixedcase))))

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

(defun html-post-processing ()
  "This method looks for a couple of key-strings and replaces them with some meaningful values. See copy-template for more generic approach."
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
      "<script src=\"https://code.jquery.com/jquery-3.4.1.js\" integrity=\"sha256-WpOohJOqMqqyKL9FccASB9O0KwACQJpFTUBLTYOVvVU=\" crossorigin=\"anonymous\"></script>"
      "<script src=\"https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/js/bootstrap.min.js\" integrity=\"sha384-ChfqqxuZUCnJSK3+MXmPNIyE6ZbWh2IMqE241rYiqJxyMiZ6OW/JmZQ5stwEULTy\" crossorigin=\"anonymous\"></script>"
      "<script src=\"subrx.js\"></script>\n"
      "<link rel=\"stylesheet\" href=\"https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/css/bootstrap.min.css\" integrity=\"sha384-MCw98/SFnGE8fJT3GXwEOngsV7Zt27NXFoaoApmYm81iuXoPkFOJwJ8ERdknLPMO\" crossorigin=\"anonymous\">"
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
      "<link rel=\"stylesheet\" href=\"https://code.jquery.com/qunit/qunit-2.9.2.css\">"
      "</head>\n"
      "<body>\n"
      "<div id=\"qunit\"></div>\n"
      "<div id=\"qunit-fixture\"></div>\n"
      "<script src=\"https://code.jquery.com/qunit/qunit-2.9.2.js\"></script>"
      "<script src=\"%TESTEE%\"></script>\n"
      "<script src=\"%UNITTESTS%\"></script>\n"
      "</body>\n"
      "</html>\n" )
    indent-buffer 
    html-post-processing ] )

(provide 'promt)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; promt.el ends here
