(defpackage #:ngrok/utils
  (:use #:cl)
  (:export #:run))
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
behaviour could be overriden by keyword argument ``:raise t``."
  
  (multiple-value-bind (stdout stderr code)
      (uiop:run-program command
                        :output '(:string :stripped t)
                        :error-output '(:string :stripped t)
                        :ignore-error-status t)
    
    (when (and raise
               (not (eql code 0)))
      (error 'subprocess-error-with-output
             :stdout stdout
             :stderr stderr
             :code code
             :command command))
    (values stdout stderr code)))
