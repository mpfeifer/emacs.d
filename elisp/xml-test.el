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
(setq xml-2 "<post topic=\"blog\"><login><id>123</id></login><msg>Here is the message</msg><info>My UA</info></post>")
(setq xml-3 "<id>123</id>")
(setq xml-5 "<sect2 id=\"GUI.General.New\"><title lang=\"en\">Create new</title><title lang=\"de\">Neu erstellen</title><para lang=\"en\">Here you can create a new E-mail CA. First you have to select thePKI type you want to create. If you click then the button <guibutton>Create new</guibutton>,the browser opens an <guimenu>Edit</guimenu> page for a new<guibutton><link linkend=\"GUI_Email_CA.SMIME.PKI_types.Internal_PKI\"xrefstyle =\"select:nopage\">Internal PKI</link></guibutton>,<guibutton><link linkend=\"GUI_Email_CA.SMIME.PKI_types.TC_TrustCenter\"xrefstyle =\"select:nopage\">TC TrustCenter PKI</link></guibutton>,<guibutton><link linkend=\"GUI_Email_CA.SMIME.PKI_types.Comodo\"xrefstyle =\"select:nopage\">Comodo PKI</link></guibutton> or<guibutton><link linkend=\"GUI_Email_CA.SMIME.PKI_types.Verisign\"xrefstyle =\"select:nopage\">Verisign PKI</link></guibutton> or<guibutton><link linkend=\"GUI_Email_CA.SMIME.PKI_types.SwissSign\"xrefstyle =\"select:nopage\">SwissSign PKI</link></guibutton>.</para><para lang=\"de\">Hier können Sie eine neue E-Mail-CA erstellen, wobei Sie dafür zuerstden PKI-Typ auswählen müssen. Wenn Sie anschließend auf den Button <guibutton>Neu erstellen</guibutton>klicken, öffnet sich die <guimenu>Bearbeiten</guimenu>-Seite für eine neue E-Mail-CA vom Typ<guibutton><link linkend=\"GUI_Email_CA.SMIME.PKI_types.Internal_PKI\"xrefstyle =\"select:nopage\">Interne PKI</link></guibutton>,<guibutton><link linkend=\"GUI_Email_CA.SMIME.PKI_types.TC_TrustCenter\"xrefstyle =\"select:nopage\">TC TrustCenter PKI</link></guibutton>,<guibutton><link linkend=\"GUI_Email_CA.SMIME.PKI_types.Comodo\"xrefstyle =\"select:nopage\">Comodo PKI</link></guibutton> oder<guibutton><link linkend=\"GUI_Email_CA.SMIME.PKI_types.Verisign\"xrefstyle =\"select:nopage\">Verisign PKI</link></guibutton><guibutton><link linkend=\"GUI_Email_CA.SMIME.PKI_types.SwissSign\"xrefstyle =\"select:nopage\">SwissSign PKI</link></guibutton>.</para></sect2>")

(setq root (with-temp-buffer
             (insert xml)
             (xml-parse-region (point-min) (point-max))))

(setq root-2 (with-temp-buffer
               (insert xml-2)
               (xml-parse-region (point-min) (point-max))))

(setq root-3 (with-temp-buffer
               (insert xml-3)
               (xml-parse-region (point-min) (point-max))))

(setq root-4 (with-current-buffer "pom.xml"
               (xml-parse-region (point-min) (point-max))))

(setq root-5 (with-temp-buffer
               (insert xml-5)
               (xml-parse-region (point-min) (point-max))))

(setq root root-2)

(xml-node-children (car root-3))

(cdr (car (car (nthcdr 1 (car root)))))


(let* (
       (post (car root))
       (attrs (xml-node-attributes post)))
  attrs)


;;      (time (cdr (assq 'time attrs)))
;;      (msg (car (xml-get-children post 'msg)))
;;      (text (car (xml-node-children msg))))
;; (message "time: %s, message '%s'" time text))

(require 'tree-widget)

(defconst testlist (list (list 1 2) 3 4))

(defun any-to-tree-widget (any)
  (interactive)
  (cond 
   ((stringp any)
    (widget-convert 'item :tag any))
   ((listp any)
    (widget-convert 'tree-widget 
                    :tag "List"
                    :args (mapcar (lambda (node)
                                    (any-to-tree-widget node)) 
                                  any) ) ) ) )

(defun test-any-to-tree-widget ()
  (interactive)
  (with-current-buffer (get-buffer-create "*tree-widget-test*")
    (erase-buffer)
    (setq-local my-tree-widget (widget-create (any-to-tree-widget testlist)))
    (switch-to-buffer (current-buffer))))

(test-any-to-tree-widget)



(defun xml-to-tree-widget (xml)
  (interactive)
  (cond 
   ((stringp xml)
    (widget-convert 'item :tag xml))
   ((listp xml)
    (let ((attributes (xml-node-attributes xml))
          (attrib-widgets nil)
          (children (xml-node-children xml))
          (current-node))
      (progn
        (when attributes
          (setq attrib-widgets (mapcar (lambda (prop)
                                         (widget-convert 'item
                                                         :tag (format "%s=%s" (symbol-to-string (car prop)) (cdr prop))))
                                       attributes)))
        (setq current-node (widget-convert 'tree-widget 
                                           :tag (symbol-to-string (car xml))
                                           :args (append (if children 
                                                     (mapcar (lambda (node)
                                                               (xml-to-tree-widget node))
                                                             children)
                                                   nil)
                                                         attrib-widgets)))
        current-node ) ) ) ) )



       (xml-to-tree-widget (car root))




;; (tree-widget :args
;;              (
;;               (tree-widget :args (...) :expander nil :tag "login")
;;               (tree-widget :args (...) :expander nil :tag "msg")
;;               (tree-widget :args (...) :expander nil :tag "info")
;;               (item :tag "time=20050716234509")
;;               (item :tag "id=010101"))
;;              :expander nil
;;              :tag "post")

       (xml-to-tree-widget (car root-3))

       (xml-to-tree-widget "text")

       (let ((ml (list 1 2 3)))
         (setq ml (cons 4 ml ))
         (setq ml (cons 5 ml ))
         (setq ml (cons 6 ml ))
         ml )


       (defun test-tree-widget ()
         (interactive)
         (with-current-buffer (get-buffer-create "*tree-widget-test*")
           (erase-buffer)
           (setq-local my-tree-widget (widget-create (xml-to-tree-widget (car root-4))))
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
