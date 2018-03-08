;;; images.el --- 
;; 
;; Filename: images.el
;; Description: I want to get image support and its not working
;; Author: Matthias
;; Maintainer: Matthias (mpfeifer77@gmail.com)
;; Copyright (C) 2017, Matthias, all rights reserved.
;; Created: Mon Feb 26 13:18:11 2018
;; Version: 1.0
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


(when nil
  (progn
    (defvar debug-image (create-image "C:\\Users\\matthias.pfeifer\\Pictures\\IMG_3706.JPG" 'png nil :ascent 'center))
    (insert "\nInserting created image directly: ")
    (insert-image debug-image)
    (defvar debug-image-string (propertize " " 'display debug-image))
    (insert "\nInsert created image string: " debug-image-string)))

(cdr (assq 'png dynamic-library-alist))

(image-type-available-p 'png)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; images.el ends here
