(in-package #:migraine-tracker/main)

(defun find-port-in-args (args)
  "Parses command line arguments to find a --port flag."
  (let ((port-arg (find "--port" args :test #'string=)))
    (when port-arg
      (let ((port-val (second (member port-arg args :test #'string=))))
        (when port-val
          (parse-integer port-val :junk-allowed nil))))))

(defun main ()
  "The main entry point for the executable."
  (let* ((args (uiop:command-line-arguments))
         (port (or (find-port-in-args args)
                   migraine-tracker.config:*default-port*)))

    ;; Initialize the application components
    (migraine-tracker.db:initialize-database)
    (migraine-tracker.web:start-app port)

    ;; Keep the main thread alive, and handle Ctrl-C for clean shutdown.
    (handler-case
        (loop (sleep 3600))
      (sb-sys:interactive-interrupt ()
        (format t "~&[INFO] Ctrl-C detected. Shutting down server.~%")
        (migraine-tracker.web:stop-app)
        (uiop:quit 0)))))
