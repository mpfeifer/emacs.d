;;; lispx.el --- 
;; 
;; Filename: lispx.el
;; Description: Extensions to the Emacs Lisp library
;; Author: Matthias
;; Maintainer: Matthias (mpfeifer77@gmail.com)

;; This function depends on solar.el and calendar.el
(defun sunrise-sunset-for-modeline ()
  (let ((calendar-time-display-form '(24-hours ":" minutes))
        (l (solar-sunrise-sunset (calendar-current-date))))
    (format "[↑%s, ↓%s]"
            (apply 'solar-time-string (car l))
            (apply 'solar-time-string (cadr l)))))

(defcustom ibuffer-project-file
  "/home/matthias/.emacs.d/ibuffer-projects"
  "A file describing a list of project directories for ibuffer. Format
of the file is like this:
 projectname,projectdir
 projectname,projectdir
 …"
  :group 'ibuffer)

(defun ibuffer-previous-line ()
  "Move point to last buffer when going before first buffer."
  (interactive)
  (forward-line -1)
  (if (<= (line-number-at-pos) 2)
      (goto-line (- (count-lines (point-min) (point-max)) 2))))

(defun ibuffer-next-line ()
  "Wrap point to first buffer when going after last buffer."
  (interactive)
  (forward-line)
  (if (>= (line-number-at-pos)
          (- (count-lines (point-min) (point-max)) 1))
      (goto-line 3)))

(defun ibuffer-add-project (groupname projectname directory)
    (let* ((group (assoc groupname ibuffer-saved-filter-groups))
           (project (assoc projectname (cdr group))))
      (if project
          (setcdr project (list (cons 'filename directory)))
        (setcdr group (cons (list
                             projectname ;; might as well be directory
                             (cons 'filename directory))
                            (cdr group))))))

(defun ibuffer-load-groups-from-project-file ()
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
              (ibuffer-add-project "Projects" projectname projectdir))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; lispx.el ends here
