;;; ndf-mode.el --- 
;; 
;; Description: Major mode for editing ndf xml files
;; Author: Matthias
;; Keywords: ndf xml
;; Dependencies: 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Changelog: 
;; 
;; 0.2 (Thu Jul 11 14:09:54 2019): Initial release
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

(define-derived-mode ndf-xml-mode xml-mode
  "NDF"
  "Mode for editing ndf xml files.")

(defun ndf-xml-mode-setup ()
  "Initialize ndf mode"
  (setq imenu-generic-expression (list '("All" "^.* id=\"\\([^\"]+\\)\".*$" 1)
                                       '("Sentences" ".*<sentence id=\"\\([^\"]+\\)\".*$" 1)
                                       '("Decision States" ".*<decision-state id=\"\\([^\"]+\\)\".*$" 1)
                                       '("Dialog Modules" ".*<dm-state id=\"\\([^\"]+\\)\".*$" 1)
                                       '("Data Access States" ".*<data-access-state id=\"\\([^\"]+\\)\".*$" 1)
                                       '("Dialogs" ".*<dialog id=\"\\([^\"]+\\)\".*$" 1)
                                       '("Custom States" ".*<custom-state id=\"\\([^\"]+\\)\".*$" 1)
                                       '("Play States" ".*<play-state id=\"\\([^\"]+\\)\".*$" 1)))
  ;; '("Dataaccess States" "^.*<data-access-state ^.* id=\"\\([^\"]+\\)\".*$" 1)
  ;; '("Custom States" "^.*<custom-state ^.* id=\"\\([^\"]+\\)\".*$" 1)
  ;; '("Play States" "^.*<play-state ^.* id=\"\\([^\"]+\\)\".*$" 1)))
  (local-set-key (kbd "C-'") 'imenu))

(add-hook 'ndf-xml-mode-hook 'ndf-xml-mode-setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ndf-mode.el ends here
