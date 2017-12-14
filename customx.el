;; Extensions to the custom system for init.el

(defcustom ibuffer-project-file
  "/home/matthias/.emacs.d/ibuffer-projects"
  "A file describing a list of project directories for ibuffer. Format
of the file is like this:
 projectname,projectdir
 projectname,projectdir
 …"
  :group 'ibuffer)

(defgroup c++
  nil "All things related to C++ development"
  :group 'development)

(defcustom openssl-dictionary-location
  "/home/matthias/.emacs.d/dictionaries/openssl.txt"
  "Location of a file with openssl function names."
  :group 'c++)

(defgroup java
  nil "All things related to java development"
  :group 'development)

(defcustom java-project-root
  "/home/matthias/src/"
  "New java projects are stored in this directory."
  :group 'java)

(defcustom jdk-location
  ""
  "Location of JDK"
  :group 'java)

(defcustom mvn-home
  "/home/matthias/opt/apache-maven"
  "Value for M2_HOME"
  :group 'java)

(defcustom tomcat-root-dir
  "/home/matthias/opt/apache-tomcat"
  "Root directory of tomcat installation.")

(defcustom tomcat-start-script
  (concat tomcat-root-dir "/bin/catalina.sh")
  "Path to script that starts Tomcat.")

(defgroup web
  nil "All things related to web development"
  :group 'development)

(defcustom web-project-root
  "/home/matthias/public_html/"
  "New web projects are stored in this directory."
  :group 'web)

(defcustom web-application-root
  java-project-root
  "New web projects are stored in this directory."
  :group 'web)

