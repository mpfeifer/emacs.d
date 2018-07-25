;;; FILENAME --- 
;; 
;; Description: 
;; Author: Matthias
;; Keywords: treemacs
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Changelog: 
;; 
;;  (Wed Jul 18 10:29:28 2018): Initial release
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

(use-package treemacs-projectile
  :disabled)

(use-package treemacs
  :config
  (setq ;; treemacs-header-function            #'treemacs--create-header-projectile
   treemacs-follow-after-init          t
   treemacs-width                      35
   treemacs-indentation                2
   treemacs-change-root-without-asking nil
   treemacs-sorting                    'alphabetic-desc
   treemacs-show-hidden-files          nil
   treemacs-never-persist              nil
   treemacs-icon-open-png   (propertize "⊖ " 'face 'treemacs-directory-face)
   treemacs-icon-closed-png (propertize "⊕ " 'face 'treemacs-directory-face))
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)

  :bind
  (:map global-map
        ([f6]        . treemacs-toggle)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FILENAME ends here
