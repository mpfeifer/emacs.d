;;; FILENAME --- 
;; 
;; Description: System specific settings for host ac-mpfeiffer01
;; Author: Matthias
;; Keywords: emacs, init.el
;; Dependencies: none
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:



(setq python-interpreter "C:/Python/Python37/python.exe")
(setq tomcat-8-home "C:/Java/tomcat/8.0.48")
(setq tomcat-9-home "C:/Java/tomcat/9.0.2")
(setq tomcat-home (let ((catalina-home (getenv "CATALINA_HOME")))
                    (if catalina-home
                        catalina-home
                      tomcat-8-home)))
(setq mp-jdk-8 "C:/Java/jdk1.8.0_152")
(setq diary-file "c:/users/matthias.pfeifer/emacs-diary")
(setq tomcat-start-script (format "%s/bin/startup.bat" tomcat-8-home))
(setq tomcat-stop-script (format "%s/bin/shutdown.bat" tomcat-8-home))
(setq mvn-home (getenv "M2_HOME"))
(setq java-home (getenv "JAVA_HOME"))
(provide 'init-ac-mpfeiffer01)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ac-mpfeiffer01.el ends here
