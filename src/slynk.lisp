(defpackage #:ngrok/slynk
  (:use #:cl)
  (:import-from #:slynk)
  (:import-from #:log4cl)
  (:import-from #:ngrok/setup)
  (:export #:start))
(in-package ngrok/slynk)

(defvar slynk:*use-dedicated-output-stream*)


(defun start (port &key auth-token region)
  "Starts SLYNK on the port and creates Ngrok tunnel to access it remotely"

  ;; To make it possible to connect to a remote SLYNK server where ports are closed
  ;; with firewall.
  (setf slynk:*use-dedicated-output-stream* nil)
  
  (log:info "Starting SLYNK server on localhost:~A"
            port)
  
  (slynk:create-server :dont-close t
                       :port port
                       :interface "localhost")

  (apply #'ngrok/setup:start
         port
         (append
          (when auth-token
            (list :auth-token auth-token))
          (when region
            (list :region region)))))
