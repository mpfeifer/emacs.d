;;; n.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun open-test-env ()
  (interactive)
  (let ((postPaidCustomer-folder "\\\\ac-ps-nss04\\Servers\\SoapUI\\mockResponses\\customer\\brands\\1\\subscriptions\\mobile")
        (inlife-folder "\\\\ac-ps-nss04\\Servers\\SoapUI\\mockResponses\\inlife"))
  (select-frame (new-frame))
  (find-file postPaidCustomer-folder)
  (split-window)
  (find-file inlife-folder)))

(defun build-path (&rest path-elements)
  (interactive)
  (let (result)
    (dolist (path-element path-elements)
      (setq result (concat result (when result "/") path-element)))
    result))

(defun work-on-sv22140 ()
  (interactive)
  (let* ((project-root "C:/Projects/Telefonica_Germany_VP/Telefonica_VP_DEV_VPNG-1801")
         (module "3-Implementation/telefonicaSimCard")
         (sentences_xml (build-path project-root module "WebContent/configuration/application/audio/SimCardVSS_Sentences.xml"))
         (sv22140_java (build-path project-root module "src/com/nuance/ps/telefonica/simcard/dataaccess/sv22140_SimActivated.java")))
    (select-frame (new-frame))
    (find-file sentences_xml)
    (split-window-horizontally)
    (other-window 1)
    (find-file sv22140_java)))

(add-hook 'nar-log-mode '(lambda ()
                           (local-set-key (kbd "n") 'next-line)
                           (local-set-key (kbd "p") 'previous-line)
                           (local-set-key (kbd "f") 'forward-word)
                           (hl-line-mode)))

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

(provide 'n)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; n.el ends here
