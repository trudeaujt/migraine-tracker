(in-package #:migraine-tracker.web)

(defvar *server* nil "The Hunchentoot server instance.")
(defvar *medicines* '("ibuprofen" "bc-powder" "rizatriptan") "List of tracked medicines.")

;; --- Configuration (can be moved to a separate config file if needed) ---
(defpackage #:migraine-tracker.config
  (:use #:cl)
  (:export #:*default-port* #:*server-lat* #:*server-lon*))

(in-package #:migraine-tracker.config)

(defvar *default-port* 5000)
(defvar *server-lat* 36.6852)
(defvar *server-lon* 139.7528)

;; --- Web Application Code ---
(in-package #:migraine-tracker.web)

(defmacro with-page ((&key (title "Migraine Tracker")) &body body)
  "A macro to generate a common HTML page structure with Spinneret."
  `(spinneret:with-html-string
     (:doctype)
     (:html
      (:head
       (:meta :charset "utf-8")
       (:meta :name "viewport" :content "width=device-width, initial-scale=1")
       (:title ,title)
       (:style (:raw "
          body { font-family: sans-serif; line-height: 1.6; margin: 2em; background-color: #f4f4f4; color: #333; }
         .container { max-width: 800px; margin: auto; background: #fff; padding: 2em; border-radius: 8px; box-shadow: 0 0 10px rgba(0,0,0,0.1); }
          h1, h2 { color: #555; }
         .med-buttons,.report,.snapshot { margin-bottom: 2em; }
         .btn { display: inline-block; padding: 10px 20px; font-size: 1em; cursor: pointer; text-align: center; text-decoration: none;
                 outline: none; color: #fff; background-color: #007bff; border: none; border-radius: 5px; margin: 5px; }
         .btn:hover { background-color: #0056b3; }
         .btn-snapshot { background-color: #28a745; }
         .btn-snapshot:hover { background-color: #218838; }
          table { width: 100%; border-collapse: collapse; margin-top: 1em; }
          th, td { padding: 12px; border: 1px solid #ddd; text-align: left; }
          th { background-color: #f8f8f8; }
          footer { text-align: center; margin-top: 2em; font-size: 0.8em; color: #777; }
          footer a { color: #007bff; }
       ")))
      (:body
       (:div :class "container"
             ,@body
             (:footer
              (:p "Migraine Tracker Application")
              (:p (:a :href "https://open-meteo.com/" "Weather data by Open-Meteo.com"))))))))

(hunchentoot:define-easy-handler (main-page :uri "/") ()
  (let ((report-data (migraine-tracker.db:get-report-data))
        (last-snapshot-time (migraine-tracker.db:get-last-snapshot-time)))
    (with-page (:title "Migraine Tracker")
      (:h1 "Migraine Medication Tracker")
      (:div :class "med-buttons"
            (:h2 "Log Medication Use")
            (dolist (med *medicines*)
              (:a :class "btn" :href (format nil "/log?medicine=~A" med)
                  (string-capitalize med))))
      (:div :class "report"
            (:h2 "Usage Since Last Visit")
            (if (> last-snapshot-time 0)
                (:p "Reporting period started on: "
                    (multiple-value-bind (s m h d mo y)
                        (decode-universal-time last-snapshot-time)
                      (declare (ignore s m h))
                      (format nil "~4,'0d-~2,'0d-~2,'0d" y mo d)))
                (:p "This is the first reporting period."))
            (if report-data
                (:table
                 (:thead (:tr (:th "Medicine") (:th "Count")))
                 (:tbody
                  (dolist (row report-data)
                    (:tr (:td (string-capitalize (first row)))
                         (:td (second row))))))
                (:p "No medications logged in this period.")))
      (:div :class "snapshot"
            (:h2 "End of Reporting Period")
            (:p "Press this button when you visit the doctor to start a new reporting period.")
            (:a :class "btn btn-snapshot" :href "/snapshot" "Snapshot for Doctor")))))

(hunchentoot:define-easy-handler (log-usage :uri "/log") (medicine)
  (unless (member medicine *medicines* :test #'string-equal)
    (setf (hunchentoot:return-code*) hunchentoot:+http-bad-request+)
    (return-from log-usage "Invalid medicine specified."))

  (multiple-value-bind (pressure forecast-json)
      (migraine-tracker.weather:fetch-weather-data migraine-tracker.config:*server-lat*
                                                    migraine-tracker.config:*server-lon*)
    (migraine-tracker.db:log-medication-use medicine pressure forecast-json)
    (hunchentoot:redirect "/")))

(hunchentoot:define-easy-handler (snapshot-report :uri "/snapshot") ()
  (migraine-tracker.db:snapshot-reporting-period)
  (hunchentoot:redirect "/"))

(defun start-app (port)
  "Starts the Hunchentoot web server."
  (when *server*
    (stop-app))
  (setf *server* (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port port)))
  (format t "~&[INFO] Migraine Tracker server started on port ~A.~%" port))

(defun stop-app ()
  "Stops the Hunchentoot web server."
  (when *server*
    (hunchentoot:stop *server*)
    (setf *server* nil)
    (format t "~&[INFO] Migraine Tracker server stopped.~%")))
