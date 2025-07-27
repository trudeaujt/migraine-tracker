(in-package #:migraine-tracker.db)

(defun get-db-path ()
  "Returns the XDG-compliant path to the SQLite database file."
  (let ((path (uiop:xdg-data-home "migraine-tracker/database.sqlite")))
    (ensure-directories-exist path)
    path))

(defun initialize-database ()
  "Ensures the database and its tables exist."
  (let ((db-path (get-db-path)))
    (sqlite:with-open-database (db db-path)
      (sqlite:execute-non-query
       db
       "CREATE TABLE IF NOT EXISTS logs (
          id INTEGER PRIMARY KEY,
          timestamp INTEGER NOT NULL,
          medicine_name TEXT NOT NULL,
          pressure_hpa REAL,
          forecast_json TEXT
        );")
      (sqlite:execute-non-query
       db
       "CREATE INDEX IF NOT EXISTS idx_logs_timestamp ON logs(timestamp);")
      (sqlite:execute-non-query
       db
       "CREATE TABLE IF NOT EXISTS reporting_periods (
          id INTEGER PRIMARY KEY,
          end_timestamp INTEGER NOT NULL UNIQUE
        );"))))

(defun log-medication-use (medicine-name pressure forecast-json)
  "Logs a medication use event to the database."
  (sqlite:with-open-database (db (get-db-path))
    (sqlite:with-transaction db
      (sqlite:execute-non-query
       db
       "INSERT INTO logs (timestamp, medicine_name, pressure_hpa, forecast_json)
        VALUES (?,?,?,?);"
       (get-universal-time)
       medicine-name
       pressure
       forecast-json))))

(defun get-last-snapshot-time ()
  "Returns the timestamp of the last reporting period snapshot, or 0 if none."
  (sqlite:with-open-database (db (get-db-path))
    (or (sqlite:execute-single db "SELECT MAX(end_timestamp) FROM reporting_periods;")
        0)))

(defun get-report-data ()
  "Retrieves the medication usage counts for the current reporting period."
  (let ((last-snapshot (get-last-snapshot-time)))
    (sqlite:with-open-database (db (get-db-path))
      (sqlite:execute-to-list
       db
       "SELECT medicine_name, COUNT(*)
        FROM logs
        WHERE timestamp >?
        GROUP BY medicine_name
        ORDER BY medicine_name;"
       last-snapshot))))

(defun snapshot-reporting-period ()
  "Creates a new reporting period snapshot."
  (sqlite:with-open-database (db (get-db-path))
    (sqlite:with-transaction db
      (sqlite:execute-non-query
       db
       "INSERT INTO reporting_periods (end_timestamp) VALUES (?);"
       (get-universal-time)))))
