(defpackage #:ngrok/utils
  (:use #:cl)
  (:export #:subprocess-error-with-output
           #:run
           #:terminate-running-command))
(in-package ngrok/utils)


(define-condition unable-to-proceed (simple-error)
  ((message :initarg :message
            :reader get-message))
  (:report (lambda (condition stream)
             (format stream (get-message condition)))))


(define-condition subprocess-error-with-output (uiop::subprocess-error)
  ((stdout :initarg :stdout :reader subprocess-error-stdout)
   (stderr :initarg :stderr :reader subprocess-error-stderr))
  (:report (lambda (condition stream)
             (format stream "Subprocess ~@[~S~% ~]~@[with command ~S~% ~]exited with error~@[ code ~D ~]~@[~%STDOUT:~% ~S~]~@[~%STDERR:~% ~S~]"
                     (uiop:subprocess-error-process condition)
                     (uiop:subprocess-error-command condition)
                     (uiop:subprocess-error-code condition)
                     (subprocess-error-stdout condition)
                     (subprocess-error-stderr condition)))))


(defun run (command &key (raise t))
  "Runs command and returns it's stdout stderr and code.

If there was an error, raises subprocess-error-with-output, but this
behaviour can be overriden by passing NIL to the keyword argument ``:raise``.

Establish a terminate-process restart which, once invoked, will try and
terminate the running command."
  (let (proc-info)
    (labels ((read-all-from-stream (stream)
               (loop :for line := (read-line stream nil nil)
                     :while line :collect line :into lines
                     :finally (return (format nil "~{~A~^~&~}" lines))))
             (run-command ()
               (setf proc-info (uiop:launch-program command
                                                    :output :stream
                                                    :error-output :stream
                                                    :ignore-error-status t))
               (let ((code (uiop:wait-process proc-info))
                     (stdout (read-all-from-stream (uiop:process-info-output proc-info)))
                     (stderr (read-all-from-stream (uiop:process-info-error-output proc-info))))
                 (when (and raise
                            (not (eql code 0)))
                   (error 'subprocess-error-with-output
                          :stdout stdout
                          :stderr stderr
                          :code code
                          :command command))
                 (values stdout stderr code))))
      (restart-case (run-command)
        (terminate-process ()
          :report "Terminate process"
          (when proc-info
            (uiop:terminate-process proc-info)))))))

(defun terminate-running-command ()
  "Terminates whichever command is found running in the current execution context.

  If no command is found running, an error is signaled."
  (let ((restart (find 'terminate-process (compute-restarts)
                       :key #'restart-name)))
    (if restart
        (invoke-restart restart)
        (error "Cannot find TERMINATE-PROCESS restart to invoke"))))
