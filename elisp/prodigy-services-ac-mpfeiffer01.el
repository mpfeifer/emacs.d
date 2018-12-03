;;; prodigy-services-ac-mpfeiffer01.el --- 
;; 
;; Description: Define prodigy services for host ac-mpfeiffer01
;; Author: Matthias
;; Keywords: prodigy, services
;; Dependencies: 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Changelog: 
;; 
;; 1.0 (Wed Oct 17 08:48:36 2018): Initial release
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(prodigy-define-service
  :name "Tomcat 8.0.48"
  :command tomcat-8-start-script
  :args '("run")
  :env '(("CATALINA_HOME" tomcat-8-home))
  :cwd tomcat-8-home)

(prodigy-define-service
  :name "Tomcat 9.0.2"
  :command tomcat-9-start-script
  :env '(("CATALINA_HOME" tomcat-9-home))
  :args '("run")
  :cwd tomcat-9-home)

(prodigy-define-service
  :name "Date Server (14002)"
  :command prodigy-python-interpreter
  :args '("date.py" "14002")
  :stop-signal 'int
  :cwd (concat prodigy-service-root "date/"))

(prodigy-define-service
  :name "Network Log-Receiver"
  :command prodigy-python-interpreter
  :args '("logwebmon.py")
  :cwd (concat prodigy-service-root "loghost/"))

(prodigy-define-service
  :name "Echo Server (14001)"
  :command prodigy-python-interpreter
  :args '("echo.py" "14001")
  :stop-signal 'int
  :cwd (concat prodigy-service-root "echo/"))

(provide 'prodigy-services-ac-mpfeiffer01)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; prodigy-services-ac-mpfeiffer01.el ends here
