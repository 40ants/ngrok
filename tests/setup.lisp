(defpackage #:ngrok-tests/setup
  (:use #:cl
        #:rove)
  (:shadowing-import-from #:rove
                          #:*debug-on-error*)
  (:import-from #:ngrok)
  (:import-from #:find-port))
(in-package #:ngrok-tests/setup)

(deftest start
  (testing "happy-path"
    (unwind-protect (multiple-value-bind (tunnel-url webserver-port)
                        (ngrok:start (find-port:find-port))
                      (ok tunnel-url "Returns Ngrok public URL")
                      (ok webserver-port "Returns Ngrok webserver port number")

                      (ng (find-port:port-open-p webserver-port)
                          "Ngrok Web server listens to one port"))
      (ngrok:stop))))

(deftest stop
  (testing "happy-path"
    (let ((webserver-port (nth-value 1 (ngrok:start (find-port:find-port)))))
      (when (find-port:port-open-p webserver-port)
        (error "Ngrok expected to be running, but it's not"))

      (ngrok:stop)
      (sleep 0.1) ;; give some time for Ngrok to shut itself down

      (ok (find-port:port-open-p webserver-port)
          "Ngrok Web server is shut down"))))
