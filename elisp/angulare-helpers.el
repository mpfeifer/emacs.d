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


(defun mpx-open-component-in-frame ()
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
  (interactive)
  (let ((css-file nil)
        (ts-file nil)
        (html-file nil))
    (select-frame (make-frame))
    (find-file ts-file)
    (split-window-right-select (find-file html-file))
    (split-window-below-select (find-file css-file))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; angulare-helpers.el ends here
