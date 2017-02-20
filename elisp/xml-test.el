;;; xml-test.el --- 
;; 
;; Filename: xml-test.el
;; Description: Test for xml library
;; Author: Matthias (mpfeifer77@gmail.com)
;; Maintainer: Matthias (mpfeifer77@gmail.com)
;; Copyright (C) 2017, Matthias (mpfeifer77@gmail.com), all rights reserved.
;; Created: Mon Feb 20 14:27:36 2017
;; Version: 1.0
;; Last-Updated: 
;;           By: 
;;     Update #: 0
;; https://www.emacswiki.org/emacs/XmlParserExamples: https://www.emacswiki.org/emacs/XmlParserExamples
;; Keywords: xml
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

(setq xml "<post time=\"20050716234509\" id=\"010101\"><login><id>123</id></login><msg>Here is the message</msg><info>My UA</info></post>")

(setq root (with-temp-buffer
             (insert xml)
             (xml-parse-region (point-min) (point-max))))


(cdr (car (car (nthcdr 1 (car root)))))

  ;;      (post (car root))
  ;;      (attrs (xml-node-attributes post))
  ;;      (time (cdr (assq 'time attrs)))
  ;;      (msg (car (xml-get-children post 'msg)))
  ;;      (text (car (xml-node-children msg))))
  ;; (message "time: %s, message '%s'" time text))

(require 'tree-widget)



(defun widgets (xml)
  (interactive)
  (if (listp xml)
      (let ((properties (car (nthcdr 1 xml)))
            (propwidgets nil))
        (when properties
          (setq propwidgets (mapcar (lambda (prop)
                                      (widget-convert 'item
                                                      :tag (format "%s=%s" (symbol-to-string (car prop)) (cdr prop))))
                                    properties)))
        (setq node_children (xml-node-children xml))
        (when node_children
          (widget-convert 'tree-widget 
                          :tag (symbol-to-string (car xml)) 
                          :args (add-to-list propwidgets (mapcar (lambda (node)
                                                                   (widgets node))
                                                                 node_children)))))
    (widget-convert 'item :tag xml)))

(widgets (car root))

(defun test-tree-widget ()
  (interactive)
  (with-current-buffer (get-buffer-create "*tree-widget-test*")
    (erase-buffer)
    (setq-local my-tree-widget
                (widget-create
                 'tree-widget
                 :open t
                 :tag "Root"
                 :args (widgets nil)))
    (switch-to-buffer (current-buffer))))

(test-tree-widget)

(defun my-create-tree-widget ()
  (interactive)
  (with-current-buffer (get-buffer-create "*tree-widget-test*")
    (setq-local my-tree-widget
                (widget-create
                 'tree-widget
                 :open t
                 :tag "Root"
                 :args (list (widget-convert
                              'tree-widget
                              :tag "two")
                             (widget-convert
                              'item
                              :tag "three")
                             (widget-convert
                              'tree-widget
                              :tag "four"
                              :args (mapcar (apply-partially #'widget-convert 'item)
                                     '("five" "six"))))))
    (switch-to-buffer (current-buffer))))

(my-create-tree-widget)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; xml-test.el ends here