(defsystem "ngrok"
  :class :package-inferred-system
  :author "Alexander Artemenko"
  :license "Unlicense"
  :pathname "src"
  :description "A wrapper around ngrok to help debug remote Lisp servers."
  :depends-on ("trivial-features"
               "ngrok/setup")
  :in-order-to ((test-op (test-op "ngrok-tests"))))
