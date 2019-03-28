;;; init-ac-mpfeiffer01.el --- 
;; 
;; Description: System specific settings for host 
;; Author: Matthias
;; Keywords: emacs, init.el
;; Dependencies: none
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

;; see also customx.el

(setq python-interpreter "")
(setq tomcat-home (getenv ""))
(setq tomcat-start-script (format "%s/bin/startup.bat" tomcat-home))
(setq tomcat-stop-script (format "%s/bin/shutdown.bat" tomcat-home))
(setq jdk-location "")
(setq diary-file "")
(setq mvn-home (getenv "M2_HOME"))
(setq java-home (getenv "JAVA_HOME"))
(setq host-local-javadoc-roots nil)
(setq prefered-font-family "")
(defconst pdf-reader "")

(setq mpx-host-local-org-capture-templates
      '( ;; See https://orgmode.org/manual/Template-elements.html#Template-elements
        ("i" "Put item in the global inbox" entry (file+headline "~/org/organizer" "INBOX") "** %?\n" :clock-in t :clock-resume t) ) )

(require 'n)
(provide 'init-fill-in-hostname-here)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
