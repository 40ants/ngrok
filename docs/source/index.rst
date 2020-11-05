======================================================
Example of Coo documentation for a Common Lisp library
======================================================

This is a small library includes a few functions with docstrings and a documentation
for the system and all included packages.

The purpose is to demonstrate core features of the
`coo <https://github.com/fisxoj/coo>`_ library.

The repository can be used as a template for new libraries if you've choosen ``Coo``
for documenting your library.

Let's review features, provided by ``Coo``.

==================
Pros & Cons of Coo
==================

.. note:: Because of some bug in ``cl-docutils``, I have to decorate this header above
          as a document's title :(

Pros
----

* reStructured text format has flexible abstraction to extend its functionality.
  You can add new "roles" to support different types of documentation blocks.
* reStructured text is widely used. It is like markdown, but is more suitable
  for writing large interlinked documents.
* extensions can be written in Common Lisp.
* cross-referencing works almost the same way like with cldomain_.


Cons
----

* reStructured implementation is incomplete and has bugs. For example, ``include``
  tag duplicates preceding lines. Roles ``toctree`` and ``code-block`` aren't supported.
* Syntax is more complex than Markdown and requires some time to get familiar with.
* Syntax relies on indentation and sometimes it leads to the problems.
* It is hard to debug error messages from ``cl-docutils``. But the Coo's author
  is `thinking about the alternatives <https://github.com/fisxoj/coo/issues/19>`_.
  For example, cl-docutils' header level processing is very buggy which makes
  hard to write large docs like this one. It frequently signals the
  ``Title Level Inconsistent`` error.
* You can fully control documentation layout. It is always file with system's description
  which includes one or more files for each system's packages.
* It inconvenient to write large blocks of code as docstring and you have to
  read them from files at compile-time.
* HTML themes aren't supported `yet <https://github.com/fisxoj/coo/issues/14>`_.


How to build the documentation
==============================

Comparing to cldomain_, Coo usage is much more easier for a Common Lisper.

You just install it from Ultralisp.org and run from the REPL like that:

::

   (coo:document-system "example"
                        :base-path #P"docs/build/")


Handwritten documentation
=========================

I think the ability to write a large pieces of documentation which aren't bound to
a function, class or module is an important feature. This way you can tell the user
about some toplevel abstractions and give a bird eye view on the library or system.

For example, handwritten parts of the documentation can provide some code snippets
to demonstrate the ways, how to use the library:

::

   (loop repeat 42
         collect (foo "bar" 100500))

And when you are talking about some function or class, you can reference it.
For example, if I'm talking about ``foo`` function, I can reference it like this
``:function:`example/app:foo``` and it will appear in the code as
the link :function:`example/app:foo`.

.. note:: However, Coo uses only docstrings for the system, packages and functions/classes/methods.

          You can't just write an chapter of the documentation on some abstract theme.

          That is why I consider that "handwritten docs" feature is missing from Coo :(


Extending Coo
=============

Coo can be extended with new roles and directives using
:macro:`docutils.parser.rst:def-role` and :macro:`docutils.parser.rst:def-directive`.

For example there here is a role to eval any lisp code and insert the result into the text::

  (def-role eval(text)
    (handler-case
        (with-input-from-string(is text)
          (let ((*package* (find-package :common-lisp-user)))
            (let ((arg1 (read is t nil))
                  (arg2 (read is nil nil)))
              (make-instance
               'docutils.nodes:inline-evaluation
               :expression (or arg2 arg1)
               :format (when arg2 arg1)))))
      (error(e)
        (make-node 'problematic
                   (write-to-string e :escape nil :readably nil )))))

To use it in the code, run:

* ``:eval:`(package-name *package*)``` ➞ :eval:`(package-name *package*)`
* ``:eval:`(format nil "~A ~A" (lisp-implementation-type) (lisp-implementation-version))``` ➞ :eval:`(format nil "~A ~A" (lisp-implementation-type) (lisp-implementation-version))`

However, in reStructured_ format role is an inline entity.
If you want to define a custom multiline block, then use
:macro:`docutils.parser.rst:def-directive`.

cl-docutils includes a directive::

  (def-directive evaluation
      (parent language
              &option
              (format symbol nil)
              (package symbol nil)
              &content content)
    (let ((language (intern (string-upcase language) :keyword)))
      (if content
          (let ((content
                 (with-output-to-string(os)
                   (loop :for line :across content
                      :do (write-line line os)))))
            (add-child
             parent
             (make-instance
              'docutils.nodes:block-evaluation
              :format (or format (setting :default-evaluation-format parent))
              :expression (ecase language
                            (:lisp (let ((*package*
                                          (or (and package (find-package package))
                                              *package*)))
                                     (read-from-string content)))))))
        (report :error "Evaluation directive is empty; content required."))))


Here is an example of calling such block and interpreting the returned result as
an HTML fragment::

  .. evaluation:: lisp
     :format: HTML
  
     (format nil "<b>~A</b> ~A:~%~%<pre>~A</pre>"
       (lisp-implementation-type)
       (lisp-implementation-version)
       (with-output-to-string (*standard-output*)
         (room nil)))

Result:

.. evaluation:: lisp
   :format: HTML

   (format nil "<b>~A</b> ~A:~%~%<pre>~A</pre>"
     (lisp-implementation-type)
     (lisp-implementation-version)
     (with-output-to-string (*standard-output*)
       (room nil)))


This is a very cool feature, because this way you can create
a custom blocks for your documentation.

For example, I always wanted to be able to show Weblocks_ widgets examples
along with their rendered pictures. With Coo it is possible to create
a directive, which will run the example's code, make a screeshot and save
it into the static folder!

Autogenerated API
=================

``Coo`` provides a completely automated generation of the API reference.

At the end of the page with the system's documentation it includes links to all
packages, provided by the system.


.. _cldomain: https://cl-doc-systems.github.io/sphinxcontrib-cldomain/
.. _reStructured: https://www.sphinx-doc.org/en/master/usage/restructuredtext/
.. _Weblocks: http://40ants.com/weblocks/
