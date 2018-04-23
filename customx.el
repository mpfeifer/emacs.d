;; Extensions to the custom system for init.el

(defgroup Custom-X
  nil "All things related to my customization"
  :group 'Emacs)

(defgroup development
  nil "All things related to development"
  :group 'Custom-X)

(defgroup ibuffer
  nil "All things related to ibuffer"
  :group 'Custom-X)

(defgroup c++
  nil "All things related to C++ development"
  :group 'Development-X)

(defcustom openssl-dictionary-location
  (concat (expand-file-name "~") "/.emacs.d/dictionaries/openssl.txt")
  "Location of a file with openssl function names."
  :group 'C++-X)

(defgroup java
  nil "All things related to java development"
  :group 'Development-X)

(defcustom java-project-root
  (concat (expand-file-name "~") "/src")
  "New java projects are stored in this directory."
  :group 'Java-X)

(defcustom jdk-location
  (getenv "JAVA_HOME")
  "Location of JDK"
  :group 'Java-X)

(defcustom mvn-home
  (getenv "M2_HOME")
  "Value for M2_HOME"
  :group 'Java-X)

(defcustom tomcat-root-dir
  (getenv "CATALINA_HOME")
  "Root directory of tomcat installation."
  :group 'Java-X)

(defcustom tomcat-start-script
  (if tomcat-root-dir 
      (concat tomcat-root-dir "/bin/startup"
              (if (eq system-type 'windows-nt)
                  ".bat"
                ".sh"))
    "startup.sh")
  "Path to script that starts Tomcat.")

(defgroup web
  nil "All things related to web development"
  :group 'Development-X)

(defcustom web-project-root
  (concat (expand-file-name "~") "/public_html/")
  "New web projects are stored in this directory."
  :group 'Web-X)

(defcustom web-application-root
  java-project-root
  "New web projects are stored in this directory."
  :group 'Web-X)

(defcustom
  prodigy-service-root
  "~/.emacs.d/services/"
  "Root directory for various services bundled with init.el."
  :group 'Prodigy-X)

(defcustom
  prodigy-python-interpreter
  "python3"
  "Location of python interpreter used by prodigy. Default just grabs one from PATH."
  :group 'Prodigy-X)
