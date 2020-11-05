(defpackage #:example/app
  (:use #:cl)
  (:import-from #:example/utils)
  (:documentation "This is docstring for the package.

                   The package contains a function which does it's job by
                   applying transformation to the first and second arguments.

                   CLDomain is not support these docstrings yet.")
  (:export #:foo))
(in-package example/app)


(defun foo (first &key (other 100500))
  "This is example function.

   Internally it calls :function:`example/utils:do-the-job`
   to do the real job.

   Note, that the link above is broken, but Coo does not warn us when building the docs.
   Sphinx issues a warning inn such case."
  (example/utils:do-the-job first other))
