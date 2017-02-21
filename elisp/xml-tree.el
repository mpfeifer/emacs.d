;;; xml-tree.el --- 
;; 
;; Filename: xml-tree.el
;; Description: XML Treeview
;; Author: Matthias
;; Maintainer: Matthias (mpfeifer77@gmail.com)
;; Copyright (C) 2017, Matthias, all rights reserved.
;; Created: Tue Feb 21 15:36:29 2017
;; Version: 0.1
;; Last-Updated: 
;;           By: 
;;     Update #: 0
;; http://mp.vv.si/blog: http://mp.vv.si/blog
;; Keywords: xml, treeview
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

(provide 'xml-tree)

(eval-when-compile
  (require 'cl))

(require 'tree-mode)
(require 'windata)

(defgroup xml-tree nil
  "Display xml structure using tree-widget"
  :group 'convenience
  :group 'pde)

(defcustom xml-tree-create-buffer-function nil
  "*A function to create buffer for insert xml tree"
  :group 'xml-tree
  :type 'function)

(defcustom xml-tree-name `(concat mode-name ": " (or (buffer-name) "<NIL>"))
  "*Tree xml root name. "
  :group 'xml-tree
  :type 'sexp)

;;;###autoload
(defcustom xml-tree-icons
  '(("Types" . "function")
    ("Variables" . "variable"))
  "*A list to search icon for the button in the tree.
The key is a regexp match the tree node name. The value is the icon
name for the children of the tree node."
  :group 'xml-tree
  :type '(alist :keytype regexp :value-type string))

(defcustom xml-tree-windata
  '(frame left 0.3 delete)
  "*Arguments to set the window buffer display.
See `windata-display-buffer' for setup the arguments."
  :group 'xml-tree
  :type 'sexp)

(defcustom xml-tree-auto-update nil
  "*Non-nil means auto update xml-tree."
  :group 'xml-tree
  :type 'boolean)

(defcustom xml-tree-update-interval 2
  "*Seconds between update xml tree."
  :type 'integer
  :group 'xml-tree)

(defvar xml-tree-need-update nil)
(defvar xml-tree-update-timer nil)
(defvar xml-tree-buffer nil)
(defvar xml-tree nil)

(define-derived-mode xml-tree-mode tree-mode "Xml-Tree"
  "A mode to display tree of xml"
  (tree-widget-set-theme "imenu")
  (add-hook 'tree-mode-delete-tree-hook 'tree-mode-kill-buffer))

;;;###autoload 
(defun xml-tree (arg)
  "Display tree view of xml.
With prefix argument, select xml tree buffer window."
  (interactive "P")
  (let ((old-tree (and (local-variable-p 'xml-tree) xml-tree))
        (buf (current-buffer))
        tree)
    (if (and (local-variable-p 'xml-tree-buffer)
             (buffer-live-p xml-tree-buffer))
        (with-current-buffer xml-tree-buffer
          (if (and old-tree (memq old-tree tree-mode-list))
              (setq tree old-tree)
            (setq tree (tree-mode-insert (xml-tree-widget buf)))))
      (let ((buffer (if (functionp xml-tree-create-buffer-function)
                        (funcall xml-tree-create-buffer-function buf)
                      (get-buffer-create "*xml-tree*"))))
        (set (make-local-variable 'xml-tree-buffer) buffer)
        (when xml-tree-auto-update
          (or xml-tree-update-timer
              (xml-tree-toggle-auto-update t))
          (set (make-local-variable 'xml-tree-need-update) nil)
          (add-hook 'after-change-functions 'xml-tree-after-change nil t))
        (add-hook 'kill-buffer-hook 'xml-tree-kill nil t)
        (with-current-buffer buffer
          (unless (eq major-mode 'xml-tree-mode)
            (xml-tree-mode))
          (setq tree (tree-mode-insert (xml-tree-widget buf))))))
    (set (make-local-variable 'xml-tree) tree)
    (let ((win (get-buffer-window xml-tree-buffer)))
      ;; if xml-tree-buffer is visible, do nothing
      (unless win
        (setq win (apply 'windata-display-buffer
                         xml-tree-buffer
                         xml-tree-windata))
        (select-window win))
      (with-selected-window win
        (unless (widget-get tree :open)
          (widget-apply-action tree))
        (goto-char (widget-get tree :from))
        (recenter 1))
      (if arg
          (select-window win)))))

(defun xml-tree-kill ()
  (let ((tree xml-tree))
    (when (and tree
               xml-tree-buffer
               (buffer-live-p xml-tree-buffer))
      (with-current-buffer xml-tree-buffer
        (ignore-errors
          (tree-mode-delete tree))))))

(defun xml-tree-show ()
  "If the `xml-tree' of current buffer is not visible, show the tree."
  (interactive)
  (let (win)
    (when (and xml-tree
               (setq win (get-buffer-window xml-tree-buffer)))
      (let ((pos (window-point win)))
        (if (not (and (>= pos (widget-get xml-tree :from))
                      (<= pos (widget-get xml-tree :to))))
            (set-window-start win (widget-get xml-tree :from)))))))

(defun xml-tree-toggle-auto-update (arg)
  "Toggle xml-tree auto update.
With prefix argument, turn on auto update."
  (interactive "P")
  (setq xml-tree-auto-update
        (if (null arg)
            (not xml-tree-auto-update)
          (> (prefix-numeric-value arg) 0)))
  (and xml-tree-update-timer
       (cancel-timer xml-tree-update-timer))
  (when xml-tree-auto-update
    (setq xml-tree-update-timer
          (run-with-timer nil xml-tree-update-interval
                          'xml-tree-update-timer))
    (mapc (lambda (buf)
            (when (local-variable-if-set-p 'xml-tree)
              (set (make-local-variable 'xml-tree-need-update) t)
              (add-hook 'after-change-functions 'xml-tree-after-change nil t)))
          (buffer-list))))

(defun xml-tree-update-timer ()
  "Update and show the tree if needed."
  (xml-tree-show)
  (when (and xml-tree
             ;; the tree is visible
             (get-buffer-window xml-tree-buffer) 
             xml-tree-need-update
             ;; the buffer is not too large
             (not (> (buffer-size) xml-auto-rescan-maxout)))
    (let ((tree xml-tree))
      (with-current-buffer xml-tree-buffer
        (goto-char (widget-get tree :from))
        (xml-mode-reflesh)))
    (setq xml-tree-need-update nil)))

(defun xml-tree-after-change (&rest ignore)
  "Mark `xml-tree-need-update' if make change in buffer"
  (setq xml-tree-need-update t))

(defun xml-tree-widget (buf)
  `(tree-widget
    :node (push-button
           :tag ,(with-current-buffer buf
                   (eval xml-tree-name))
           :format "%[%t%]\n"
           :notify tree-mode-reflesh-parent)
    :dynargs xml-tree-expand
    :has-children t
    :buffer ,buf
    :open t))

(defun xml-tree-item (item buf icon)
  (if (listp (cdr item))
      `(tree-widget
        :node (push-button
               :tag ,(car item)
               :button-icon "bucket"
               :notify tree-mode-reflesh-parent
               :format "%[%t%]\n")
        :dynargs xml-tree-expand-bucket
        :has-children t)
    `(push-button
      :tag ,(car item)
      :xml-marker ,(let ((pos (cdr item)))
                       (cond ((markerp pos) pos)
                             ((numberp pos)
                              (set-marker (make-marker) pos buf))
                             ((overlayp pos)
                              (set-marker (make-marker) (overlay-start pos) buf))
                             (t (error "Unknown position type: %S" pos))))
      :button-icon ,icon
      :format "%[%t%]\n"
      :notify xml-tree-select)))

(defun xml-tree-select (node &rest ignore)
  (let ((marker (widget-get node :xml-marker)))
    (select-window (display-buffer (marker-buffer marker)))
    (goto-char marker)))

(defun xml-tree-expand-bucket (bucket)
  (let ((tree bucket) path buf index name)
    (while (and (tree-widget-p tree)
                (widget-get tree :parent))
      (push (widget-get (widget-get tree :node) :tag) path)
      (setq tree (widget-get tree :parent)))
    (setq buf (widget-get tree :buffer)
          name (car (last path)))
    (while path
      (setq index (cdr (assoc (car path) index)))
      (if (null index)
          (error "Type g to update imenu index"))
      (setq path (cdr path)))
    (mapcar (lambda (item)
              (xml-tree-item item buf
                             (or (assoc-default name imenu-tree-icons
                                                'string-match)
                                 "function")))
            index)))

(defun xml-tree-expand (tree)
  (or (widget-get tree :args)
      (let ((buf (widget-get tree :buffer))
            index)
        (setq index (with-current-buffer buf 0)) ;; just set to zero... whoaw...
        (mapcar (lambda (item)
                  (xml-tree-item item buf "function"))
                index))))

(defun xml-tree-display ()
  (interactive)
  (let ((widget (widget-at (1- (line-end-position))))
        marker)
    (if (setq marker (widget-get widget :xml-marker))
        (with-selected-window (display-buffer (marker-buffer marker))
          (goto-char marker)))))

(define-key xml-tree-mode-map "\C-o" 'xml-tree-display)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; xml-tree.el ends here
