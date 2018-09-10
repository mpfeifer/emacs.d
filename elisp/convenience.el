;;; convenience.el --- 
;; 
;; Description: Convenience functions, keybindings, etc
;; Author: Matthias
;; Keywords: emacs lisp
;; Dependencies: init.el
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Changelog: 
;; 
;; 1.0 (Mon Aug 27 08:18:09 2018): Initial release
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

(defun find-file-dispatcher (arg)
  "If on an org-link open it.
If point is on any file open it.
If projectile-find-file is available use it.
Otherweise plain find-file."
  (interactive "P")
  (if (and
       (eq major-mode 'org-mode)
       (eq (get-char-property (point) 'face) 'org-link))
      (org-open-at-point)
    (let ((file-at-point (thing-at-point 'filename)))
      (if (and file-at-point
               (file-exists-p file-at-point))
          (progn
            (message (format "find-file-dispatcher found file at point: %s" file-at-point))
            (find-file file-at-point))
        (call-interactively   (if arg
                                  'find-file
                                'find-file-in-project))))))

(global-set-key (kbd "C-x C-f") 'find-file-dispatcher)

(defun mp-open-pdf-file (arg)
  "Open pdf file ARG in external viewer. (Its not working like this...)"
  (start-process-shell-command "Adobe_Reader" nil pdf-reader))

(provide 'convenience)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; convenience.el ends here
