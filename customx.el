;; Personal extensions to the Emacs customization system


(defgroup Custom-X
  nil "All things related to my customization"
  :group 'Emacs)

(defgroup External-Utilities
  nil "List of extern tools"
  :group 'Custom-X)

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

(defcustom host-local-javadoc-roots '()
  "A list of directories that will be indexed by javadoc-lookup package."
  :group 'Java-X)

(defcustom java-project-root
  (concat (expand-file-name "~") "/src")
  "New java projects are stored in this directory."
  :group 'Java-X)

(defcustom jdk-location nil  
  "Location of JDK"
  :group 'Java-X)

(defcustom mvn-home nil
  "Value for M2_HOME"
  :group 'Java-X)

(defcustom tomcat-home nil
  "Root directory of tomcat installation."
  :group 'Java-X)

(defcustom tomcat-start-script nil
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
  "python"
  "Location of python interpreter used by prodigy. Default just grabs one from PATH."
  :group 'Prodigy-X)

(defcustom
  pdf-reader
  nil
  "Location of a pdf reader application"
  :group 'External-Utilities)
