(in-package #:migraine-tracker.weather)

(defun fetch-weather-data (lat lon)
  "Fetches current pressure and daily forecast from the Open-Meteo API.
   Returns two values: current pressure in hPa and the daily forecast as a JSON string.
   Returns (values nil nil) on failure."
  (handler-case
      (let* ((url (format nil "https://api.open-meteo.com/v1/forecast?latitude=~A&longitude=~A&current=pressure_msl&daily=weathercode&timezone=UTC"
                          lat lon))
             (response-body (dex:get url))
             (json-data (jonathan:parse response-body)))
        (let ((pressure (getf (getf json-data :|current|) :|pressure_msl|))
              (daily-forecast-json (jonathan:to-json (getf json-data :|daily|))))
          (values pressure daily-forecast-json)))
    (error (c)
      (format *error-output* "~& Failed to fetch weather data: ~A~%" c)
      (values nil nil))))
