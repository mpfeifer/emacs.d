;;; n.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NSS log files

(define-derived-mode nss-config-file-mode view-mode
  "NSS"
  "Mode for reading speech server config files")

(defun nss/clean-out-comments ()
  "Remove empty lines and comment lines from current buffer"
  (when nss-config-file-mode
    (progn
      (goto-char (point-min))
      (set-mark (point))
      (goto-char (point-max))
      (exchange-point-and-mark)
      (flush-lines "^$\\|^#.*")
      (sort-lines nil (point-min) (point-max)))))

;; [ ndf mode

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

;; ]

(define-derived-mode nar-log-mode view-mode
  "Nar"
  "Mode for reading nar log files")

(add-hook 'nar-log-mode-hook '(lambda ()
                           (local-set-key (kbd "n") 'next-line)
                           (local-set-key (kbd "p") 'previous-line)
                           (local-set-key (kbd "f") 'forward-word)
                           (hl-line-mode)))

(provide 'n)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; n.el ends here
