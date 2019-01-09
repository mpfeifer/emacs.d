;;; package-guard.el --- 
;; 
;; Description: Some functions to track a timestamp using the file-system
;; Author: Matthias
;; Keywords: 
;; Dependencies: 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Changelog: 
;; 
;; 1.0 (Tue Jan  8 14:56:46 2019): Initial release
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

(defconst package-guard-renewal 604800)
(defconst fn-package-guard "~/.emacs.d/.package-guard")
;; periodically refresh package contents

(defun timeval-to-seconds (tv)
  "Calculate SEC-HIGH * 2^16 + SEC-LOW for value contained in TV."
  (let* ((sec-high (nth 0 tv))
         (sec-low (nth 1 tv)))
    (+ sec-low (* sec-high (expt 2 16)))))

(defun package-refresh-necessary-p ()
  (if (file-exists-p fn-package-guard)
      (progn
        (let* ((mtime (timeval-to-seconds
                       (nth 5
                            (file-attributes fn-package-guard))))
               (ctime (timeval-to-seconds
                       (current-time))))
          (< (+ mtime package-guard-renewal) ctime )))
    t))

(defun update-package-guard ()
  "Write current time to pacakge-guard file"
  (with-temp-buffer
    (insert (prin1-to-string (current-time)))
    (write-file fn-package-guard)))


;; see if this emacs is starting for the first time (with this init.el)
;; and if pacakge refresh is necessary (currently once in a week)

(if (not (file-exists-p fn-package-guard))
    (let* ((emacs-dir( expand-file-name user-emacs-directory))
           (desktop-dir (concat emacs-dir "/desktop"))
           (user-information "Will perform first time initialisation! Press enter."))
      (read-from-minibuffer user-information)
      (when (not (file-exists-p desktop-dir))
        (make-directory desktop-dir))
      (update-package-guard)
      (package-refresh-contents)
      (package-install 'use-package))
  (when (package-refresh-necessary-p)
    (let ((user-information "Will refresh package contents! Press enter."))
      (read-from-minibuffer user-information)
      (package-refresh-contents t)
      (update-package-guard))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; package-guard.el ends here
