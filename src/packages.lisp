(defpackage #:migraine-tracker.config
  (:use #:cl)
  (:export #:*default-port*
           #:*server-lat*
           #:*server-lon*))

(defpackage #:migraine-tracker.db
  (:use #:cl)
  (:export #:initialize-database
           #:log-medication-use
           #:get-report-data
           #:snapshot-reporting-period
           #:get-last-snapshot-time))

(defpackage #:migraine-tracker.weather
  (:use #:cl)
  (:export #:fetch-weather-data))

(defpackage #:migraine-tracker.web
  (:use #:cl)
  (:export #:start-app
           #:stop-app))

(defpackage #:migraine-tracker/main
  (:use #:cl)
  (:export #:main))

(defpackage #:migraine-tracker
  (:use #:cl #:spinneret)
  (:import-from #:migraine-tracker.config
                #:*default-port*
                #:*server-lat*
                #:*server-lon*)
  (:import-from #:migraine-tracker.db
                #:get-report-data
                #:get-last-snapshot-time)
  (:import-from #:migraine-tracker.weather
                #:fetch-weather-data)
  (:shadowing-import-from #:spinneret #:get))
