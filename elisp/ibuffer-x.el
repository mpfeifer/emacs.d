;;; ibuffer-x.el --- 
;; 
;; Filename: ibuffer-x.el
;; Description: Extensions to the ibuffer package
;; Author: Matthias
;; Maintainer: Matthias (mpfeifer77@gmail.com)
;; Copyright (C) 2017, Matthias, all rights reserved.
;; Created: Mon May 14 23:58:32 2018
;; Version: 1.0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(defcustom ibuffer-do-view-keep-ibuffer t
  "If t keep ibuffer window when viewing buffers from ibuffer.")

(defun ibuffer-do-view-1 (type)
  (let ((marked-bufs (ibuffer-get-marked-buffers)))
    (when (null marked-bufs)
      (setq marked-bufs (list (ibuffer-current-buffer t))))
    (unless (and (eq type 'other-frame)
		 (not ibuffer-expert)
		 (> (length marked-bufs) 3)
		 (not (y-or-n-p (format "Really create a new frame for %s buffers? "
					(length marked-bufs)))))
      (set-buffer-modified-p nil)
      (when (not ibuffer-do-view-keep-ibuffer)
        (progn
          (delete-other-windows)
          (switch-to-buffer (pop marked-bufs))))
      (let ((height (/ (1- (if (eq type 'horizontally) (frame-width)
			     (frame-height)))
		       (1+ (length marked-bufs)))))
	(mapcar (if (eq type 'other-frame)
		    (lambda (buf)
		      (let ((curframe (selected-frame)))
			(select-frame (make-frame))
			(switch-to-buffer buf)
			(select-frame curframe)))
		  (lambda (buf)
		    (split-window nil height (eq type 'horizontally))
		    (other-window 1)
		    (switch-to-buffer buf)))
		marked-bufs)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ibuffer-x.el ends here
