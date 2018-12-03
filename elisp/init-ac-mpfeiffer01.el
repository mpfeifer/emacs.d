;;; init-ac-mpfeiffer01.el --- 
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
(setenv "CATALINA_HOME" tomcat-8-home)
(setq tomcat-8-start-script (format "%s/bin/catalina.bat" tomcat-8-home))
(setq tomcat-8-stop-script (format "%s/bin/catalina.bat" tomcat-8-home))
(setq tomcat-9-home "C:/Java/tomcat/9.0.2")
(setq tomcat-9-start-script (format "%s/bin/startup.bat" tomcat-9-home))
(setq tomcat-9-stop-script (format "%s/bin/shutdown.bat" tomcat-9-home))
(setq mp-jdk-8 "C:/Java/jdk/1.8.191/")
(setq diary-file "c:/users/matthias.pfeifer/emacs-diary")
(setq mvn-home (getenv "M2_HOME"))
(setq java-home (getenv "JAVA_HOME"))
(setq host-local-javadoc-roots (list
                                "c:/Java/apache-log4j-2.11.1-bin/apache-log4j-2.11.1-bin/"))
(defconst pdf-reader "C:/Program Files (x86)/Adobe/Acrobat Reader DC/Reader/AcroRd32.exe")
(defconst prodigy-services-loader (format "prodigy-services-%s" (downcase 
                                                             (getenv (if (eq system-type 'windows-nt)
                                                                         "COMPUTERNAME"
                                                                       (if (or
                                                                            (eq system-type 'gnu/linux)
                                                                            (eq system-type 'cygwin))
                                                                           "HOSTNAME"
                                                                         "default"))))))
(setq mpx-host-local-org-capture-templates
      '( ;; See https://orgmode.org/manual/Template-elements.html#Template-elements
        ("I" "INBOX Item for telefonica's current Sprint" entry (file+olp "~/org/telefonica.org" "Agile Team" "Sprint 18.16" "INBOX")
         "***** %?\n      %T\n" :clock-in t :clock-resume t)
        ("N" "A note for the current task" entry (clock) note-template) ) )

(require 'n)
(provide 'init-ac-mpfeiffer01)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ac-mpfeiffer01.el ends here
