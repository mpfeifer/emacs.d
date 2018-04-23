;;; experimental.el --- Lines of code not yet tested  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Matthias

;; Author: Matthias <mpfeifer77@gmail.com>
;; Keywords: lisp, extensions
                                        ;
  (defun mp:neotree-updater ()
    (when (eq 2 (length (window-list)))
      (let* ((wnd-0 (nth 0 (window-list)))
             (wnd-1 (nth 1 (window-list)))
             (buf-0 (window-buffer wnd-0))
             (buf-1 (window-buffer wnd-1))
             (neo-buf nil)
             (other-buf nil)
             (neo-wnd nil)
             (other-wnd nil)
             (filename nil)
             (neo-buffer (get-buffer " *NeoTree*")))
        (when (and neo-buffer
                   (or (eq buf-0 neo-buffer)
                       (eq buf-1 neo-buffer)))
          (progn
            (if (eq buf-0 neo-buffer)
                (setq neo-buf buf-0
                      other-buf buf-1
                      neo-wnd wnd-0
                      other-wnd wnd-1)
              (setq neo-buf buf-1
                    other-buf buf-0
                    neo-wnd wnd-1
                    other-wnd wnd-0))
            (when (not (eq wnd-0 neo-wnd))
              (progn
                (setq filename (buffer-file-name other-buf))
                (when (and filename
                           (file-exists-p filename))
                  (progn
                    (let ((buffer-list-update-hook nil))
                      (neotree-find filename)
                      (select-window other-wnd)))))))))))

  ;; (add-hook 'buffer-list-update-hook 'mp:neotree-updater)

  ;; (remove-hook 'buffer-list-update-hook 'mp:neotree-updater)
  
;; [ openssl


;; define several category of keywords (note: order matters - sort by length)
;; (setq openssl-keywords '(;;"-----BEGIN CERTIFICATE-----" "-----END CERTIFICATE-----"
;;                          "X509v3 Authority Key Identifier" "X509v3 CRL Distribution Points"
;;                          "X509v3 Subject Key Identifier" "Authority Information Access"
;;                          "X509v3 Certificate Policies" "X509v3 Basic Constraints"
;;                          "Subject Public Key Info" "Public Key Algorithm" "Signature Algorithm"
;;                          "X509v3 extensions" "X509v3 Key Usage" "Serial Number" "\\^Certificate"
;;                          "Not Before" "Public-Key" "Full Name" "Not After" "Validity" "Exponent"
;;                          "Subject" "Version" "Modulus" "Policy" "Issuer" "keyid" "Data" "CPS" ) )

;; (setq openssl-keywords-regexp (regexp-opt openssl-keywords 'words))

;; ;; note: order matters for openssl-font-lock-keywords,
;; ;; because once colored, that part won't change.
;; ;; in general, longer words first
;; (setq openssl-font-lock-keywords`((,openssl-keywords-regexp . font-lock-keyword-face)))

;; (define-derived-mode openssl-mode view-mode "ossl"
;;   "Major mode for viewing pem files"
;;   ;; code for syntax highlighting
;;   (setq font-lock-defaults '((openssl-font-lock-keywords)))
;;   (font-lock-mode) )
;;
;; ;; clear memory. no longer needed
;; (setq openssl-keywords-regexp nil)
;;
;; (add-to-list 'auto-mode-alist '("\.pem\\'" . openssl-mode))
;; (add-to-list 'auto-mode-alist '("\.cer\\'" . hexl-mode))
;;
;; other extensios: key, der, csr
;; other openssl commands:
;;    openssl x509 -in some.file -text [ -inform der]
;;    openssl req -in some.file -text
;; pem file header
;;    ----- BEGIN CERTIFICATE REQUEST -----
;;    ----- BEGIN RSA PRIVATE KEY -----
;;    ----- BEGIN CERTIFICATE -----
;; (defun mp:show-x509 ()
;;   (interactive)
;;   (let ((cert-file (buffer-file-name))
;;         (right-window (split-window-right))
;;         (openssl-buffer (generate-new-buffer
;;                          (generate-new-buffer-name "*openssl*"))))
;;     (select-window right-window)
;;     (set-window-buffer nil openssl-buffer)
;;     (call-process "openssl" nil (list openssl-buffer t)
;;                   t "rsa" "-text" "-noout" "-in" cert-file)))

;; ]

