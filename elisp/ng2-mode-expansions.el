;;; ng2-mode-expansions.el --- 
;; 
;; Description: Expand region for angular2 decorators
;; Author: Matthias
;; Keywords: angular2, expand-region, er
;; Dependencies: expand-region
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Changelog: 
;; 
;; 1.0 (Tue Dec 18 10:20:40 2018): Initial release
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

;;; ng2-mode-expansions.el --- Additional expansions for js2-mode

(require 'expand-region-core)

(defun ng2-mark-decorator ()
  (interactive)
  (let* ((decoration-start nil)
         (decoration-end nil))
    (progn
      (save-excursion
        (search-backward "@")
        (setq decoration-start (point))
        (search-forward "})")
        (setq decoration-end (point)))
      (goto-char (decoration-start))
      (set-mark decoration-end))))


(defun er/add-ng2-mark-decorator ()
  "Add decorator expansion in ng2-mode buffers"
  (set (make-local-variable 'er/try-expand-list) (append
                                                  er/try-expand-list
                                                  '(ng2-mark-decorator))))

(er/enable-mode-expansions 'ng2-mode 'er/add-ng2-mark-decorator)

(provide 'ng2-mode-expansions)

;; ng2-mode-expansions.el ends here
