cl-spasm
========

A Common Lisp port of Hiccup, Clojure's HTML-building, vector- and map-based
library.

"Port" may be too strong a word, though... perhaps "heavily inspired by".
Additional inspiration was taken from:

* cl-who (`darcs repo`_, `project page`_)

* `Twisted's stan syntax`_ (template tags) for representing HTML as
  S-expressions. Additionally, spasm includes some of the functionality
  provided by hickory (Clojure).

.. Links:
.. _Hiccup:
.. _Twisted's stan syntax: http://twistedmatrix.com/trac/browser/trunk/twisted/web/template.py#L518
.. _hickory:
.. _darcs repo: http://common-lisp.net/~loliveira/ediware/cl-who/
.. _project page: http://weitz.de/cl-who/

Install
-------

Add the following in your project's ``(defpackage`` call:

.. code:: lisp

   (:use :cl :spasm)

If you would like to run the test suite, you may do the following:

.. code:: lisp

   * (ql:quickload 'spasm)
   * (ql:quickload 'spasm-tests)
   * (spasm-tests:run-spasm-tests)

Documentation
-------------

* `Syntax`_

.. Links:
.. _Syntax: 


Quickstart
----------

Here is a basic example of using spasm:

.. code:: lisp

  * (use-package :spasm)

  (SPASM)

Or, if you are running from the git checkout:

.. code:: lisp

  * (ql:quickload 'spasm)

  (SPASM)

  * (in-package :spasm)

  #<PACKAGE "SPASM">

  * (html (:span :class "foo" "bar"))
  "<span class='foo'>bar</span>"

If you are familiar with CL-WHO, then this will look quite familiar; it's much
closer to CL-WHO's usage than Clojure's Hiccup. Each list beginning with a
keyword is transformed into an XHTML tag of the same name.

Differences with CL-WHO:

* No HTML support; only XHTML.

* No streams support; you figure out what you want to do with the string, once
  you have it.

* Supports both Lisp -> XHTML and XHTML -> Lisp.

Like Clojure's Hiccup, Spasm provides a CSS-like shortcut for denoting `id` and
`class` attributes:

.. code:: lisp

  (html (:div#foo.bar.baz "bang"))
  "<div id='foo' class='bar baz'>bang</div>"
