(defsystem #:migraine-tracker
  :description "A web application for tracking migraine medication and weather."
  :author "Your Name <your.email@example.com>"
  :license "Specify license here"
  :version "1.0.0"
  :depends-on (#:hunchentoot
               #:sqlite
               #:dexador
               #:jonathan
               #:spinneret)
  :components ((:file "src/packages")
               (:file "src/db" :depends-on ("src/packages"))
               (:file "src/weather" :depends-on ("src/packages"))
               (:file "src/web" :depends-on ("src/packages" "src/db" "src/weather"))
               (:file "src/main" :depends-on ("src/packages" "src/web" "src/db")))
  :build-operation "program-op"
  :build-pathname "migraine-tracker"
  :entry-point "migraine-tracker/main:main")
