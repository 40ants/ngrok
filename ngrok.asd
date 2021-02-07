(defsystem "ngrok" 
  :class :package-inferred-system
  :author "Alexander Artemenko"
  :license "Unlicense"
  :pathname "src"
  :description "A wrapper around ngrok to help debug remote Lisp servers."
  :depends-on ("mgl-pax-minimal"
               "trivial-features"
               "ngrok/setup"))
