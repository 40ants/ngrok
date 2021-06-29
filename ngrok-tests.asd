(defsystem "ngrok-tests"
  :class :package-inferred-system
  :pathname "tests"
  :depends-on ("rove"
               "ngrok-tests/setup")
  :perform (test-op (o c) (symbol-call :rove '#:run c)))
