;;; echo-server.el --- 
;; 
;; Description: Echo server in emacs lisp
;; Author: Matthias
;; Keywords: emacs, lisp, echo, server
;; Dependencies: Emacs version >= 22
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Changelog: 
;; 
;; 1.0 (Wed Oct  3 22:36:34 2018): Initial release
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

(defvar echo-server-port 10000
    "port of the echo server")
  
(defvar echo-server-clients '() 
    "alist where KEY is a client process and VALUE is the string")


(defun echo-server-start nil
    "starts an emacs echo server"
    (interactive)
    (unless (process-status "echo-server")
      (make-network-process :name "echo-server" :buffer "*echo-server*" :family 'ipv4 :service echo-server-port :sentinel 'echo-server-sentinel :filter 'echo-server-filter :server 't) 
      (setq echo-server-clients '())
      )
    )
  
  (defun echo-server-stop nil
    "stop an emacs echo server"
    (interactive)
    (while  echo-server-clients
      (delete-process (car (car echo-server-clients)))
      (setq echo-server-clients (cdr echo-server-clients)))
    (delete-process "echo-server")
    )
  
  (defun echo-server-filter (proc string)   
    (let ((pending (assoc proc echo-server-clients))
        message
        index)
      ;;create entry if required
      (unless pending
        (setq echo-server-clients (cons (cons proc "") echo-server-clients))
        (setq pending  (assoc proc echo-server-clients)))
      (setq message (concat (cdr pending) string))
      (while (setq index (string-match "\n" message))
        (setq index (1+ index))
        (process-send-string proc (substring message 0 index))
        (echo-server-log  (substring message 0 index) proc)
        (setq message (substring message index)))
      (setcdr pending message))
    )
  
  (defun echo-server-sentinel (proc msg)
   (when (string= msg "connection broken by remote peer\n")
    (setq echo-server-clients (assq-delete-all proc echo-server-clients))
    (echo-server-log (format "client %s has quit" proc))))
  
  ;;from server.el
  (defun echo-server-log (string &optional client)
      "If a *echo-server* buffer exists, write STRING to it for logging purposes."
    (if (get-buffer "*echo-server*")
        (with-current-buffer "*echo-server*"
          (goto-char (point-max))
          (insert (current-time-string)
                  (if client (format " %s:" client) " ")
                  string)
          (or (bolp) (newline)))))

(provide 'echo-server)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; echo-server.el ends here
