
;;; init-template.el  --- 
;; 
;; Description: System specific settings for this host
;; Author: Matthias
;; Keywords: emacs, init.el
;; Dependencies: none
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:



(setq python-interpreter "")
(setq tomcat-home (getenv "CATALINA_HOME"))
(setq mpx-jdk "C:/Java/jdk1.8.0_152")
(setq diary-file nil)
(setq tomcat-start-script (format "%s/bin/startup.bat" tomcat-home))
(setq tomcat-stop-script (format "%s/bin/shutdown.bat" tomcat-home))
(setq mvn-home (getenv "M2_HOME"))
(setq java-home (getenv "JAVA_HOME"))

(setq mpx-host-local-org-capture-templates
      '( ;; See https://orgmode.org/manual/Template-elements.html#Template-elements
         ) )

;; TOD: Add hostname
(provide 'init-ac-)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ac-mpfeiffer01.el ends here
