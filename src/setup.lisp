(defpackage #:ngrok
  (:use #:cl)
  (:nicknames #:ngrok/setup)
  (:import-from #:ngrok/utils
                #:run
                #:terminate-running-command)
  (:import-from #:log4cl)
  (:import-from #:bordeaux-threads)
  (:import-from #:jonathan)
  (:export #:start
           #:stop))
(in-package ngrok)


(defvar *thread* nil)
(defvar *port* nil)
(defparameter *default-timeout* 15)


(defun os ()
  "Returns keywords like :linux :macos :windows"
  #+darwin
  :macos
  
  #+linux
  :linux
  
  #+windows
  :windows)


(defun archive-name ()
  (ecase (os)
    (:linux "ngrok-stable-linux-386.zip")
    (:macos "ngrok-stable-darwin-amd64.zip")
    (:windows "ngrok-stable-windows-amd64.zip")))


(defun install-ngrok (token)
  (cond
    ((probe-file "./ngrok")
     (log:info "Ngrok already installed, changing authtoken")
     (run
      (format nil "./ngrok authtoken ~A" token)))
    (t
     (log:info "Installing ngrok")
     (run
      (format nil "curl https://bin.equinox.io/c/4VmDzA7iaHb/~A --output ngrok.zip"
              (archive-name)))
     (run "unzip ngrok.zip")
     (run "chmod +x ./ngrok")
     (run
      (format nil "./ngrok authtoken ~A" token)))))


(defun find-tunnel-url-in-log (log-filename)
  (when (probe-file log-filename)
    (loop for line in (uiop:read-file-lines log-filename)
          for parsed = (jonathan:parse line)
          for msg = (getf parsed :|msg|)
          when (and msg
                    (string-equal msg
                                  "started tunnel"))
            do (return (getf parsed :|url|)))))


(defun ngrok-webserver-port (log)
  "Returns the port number Ngrok Webserver is currently listening to, or NIL"
  (let ((url (find-ngrok-webserver-url-in-log log)))
    (when url
      (parse-integer (subseq url (1+ (position #\: url :from-end t)))))))


(defun find-ngrok-webserver-url-in-log (log-filename)
  (when (probe-file log-filename)
    (loop for line in (uiop:read-file-lines log-filename)
          for parsed = (jonathan:parse line)
          for msg = (getf parsed :|msg|)
          when (and msg
                    (string-equal msg
                                  "starting web service"))
            do (return (getf parsed :|addr|)))))

(defun wait-for-connection (log-filename &key (timeout *default-timeout*))
  (loop with started-at = (get-universal-time)
        for url = (find-tunnel-url-in-log log-filename)
        do (cond
             (url
              (return-from wait-for-connection url))
             ((< (- (get-universal-time)
                    started-at)
                 timeout)
              (sleep 0.1))
             (t (return-from wait-for-connection
                  nil)))))


(defun start (port &key
                     (auth-token (uiop:getenv "NGROK_AUTH_TOKEN"))
                     (region (or (uiop:getenv "NGROK_REGION")
                                 "eu"))
                     (log (or (uiop:getenv "NGROK_LOG")
                              ".ngrok.log"))
                     (timeout *default-timeout*))
  "Returns Ngrok tunnel's URL or NIL.

  On success, the PORT number the Ngrok's Web server wound up listening too is
  returned as well, as a second value."
  
  (unless auth-token
    (error "Auth token should be provided as a parameter or as NGROK_AUTH_TOKEN env variable."))

  (install-ngrok auth-token)
  
  (cond
    ((and *thread* *port*)
     (log:error "Ngrok already running on port ~A"
                *port*)
     (values nil))
    (t
     (log:info "Starting Ngrok on TCP port ~A" port)
     
     (uiop:delete-file-if-exists log)
     
     (setf *thread*
           (bt:make-thread (lambda ()
                             ;; RUN delegates to UIOP:LAUNCH-PROGRAM.
                             ;;
                             ;; UIOP:LAUNCH-PROGRAM, as it turns out, behaves
                             ;; differently if the specified command is a string
                             ;; or a list of arguments: if a string, it would
                             ;; run the command via `sh -c $command`,
                             ;; effectively running the specified command in a
                             ;; child process; in case of a list instead, the
                             ;; command will be executed as-is, i.e. without
                             ;; creating a child process.
                             ;;
                             ;; Why does this matter? Because we want to be
                             ;; able to interrupt this long-running process,
                             ;; and in order for NGROK:STOP first, and then
                             ;; UIOP:TERMINATE-PROCESS, to do that (i.e.
                             ;; interrupt the running process) they need to get
                             ;; ahold of the actual Ngrok process, and not the
                             ;; parent shell that spawend it.
                             ;;
                             ;; Hence, we specify the Ngrok command to run as
                             ;; a list.
                             (run (list "./ngrok" "tcp" (write-to-string port)
                                        "--log" log "--log-format" "json"
                                        "--region" region)))
                           :name (format nil "Ngrok on port ~A"
                                         port))
           *port* port)

     (let ((url (wait-for-connection log :timeout timeout)))
       (cond
         (url
          (log:info "Tunnnel established! Connect to the ~A" url)
          (values url (ngrok-webserver-port log)))
         (t
          (log:error "Unable to establish tunnel in ~A seconds. Closing connection."
                     timeout)
          (stop)
          (values nil)))))))


(defun stop ()
  (when *thread*
    (when (bt:thread-alive-p *thread*)
      (bt:interrupt-thread *thread* #'terminate-running-command))
    (setf *thread* nil
          *port* nil)))
