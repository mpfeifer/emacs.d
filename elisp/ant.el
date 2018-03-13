;;; ant.el --- 
;; 
;; Filename: ant.el
;; Description: 
;; Author: Matthias
;; Maintainer: Matthias (mpfeifer77@gmail.com)
;; Copyright (C) 2017, Matthias, all rights reserved.
;; Created: Mon Mar 12 23:38:52 2018
;; Version: 
;; Last-Updated: 
;;           By: 
;;     Update #: 0
;; : 
;; Keywords: 
;; Compatibility: 
;; 
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;; 
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

(defun re-seq (regexp index string)
  "Get a list of all regexp matches in a string"
  (interactive)
  (save-match-data
    (let ((pos 0)
          matches)
      (while (string-match regexp string pos)
        (push (match-string index string) matches)
        (setq pos (match-end index)))
      matches)))

(easy-menu-define
  ant-menu
  global-map
  "Ant"
  '("Targets"))

(defun ant/get-menu ()
  (easy-menu-create-menu
   "Ant"
   (mapcar (lambda (target)
             (vector target
                     `(lambda ()
                        (interactive)
                        (message ,target) t)))
                 (with-current-buffer "build.xml"
                   (re-seq "target name=\"\\([^\"]*\\)"
                           1
                           (buffer-substring-no-properties (point-min) (point-max)))))))

;; (easy-menu-add-item ant-menu '() (ant/get-menu))

(defun ant/update-menu ()
  (easy-menu-add-item ant-menu '() (ant/get-menu)))

(add-hook 'menu-bar-update-hook 'ant/update-menu)



(setq xml-parse-tree
      (with-current-buffer "build.xml"
        (xml-parse-file (buffer-file-name)
         (point-min) (point-max))))
 




;; This one will show the tree-widget in an emp



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ant.el ends here
