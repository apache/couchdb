.. Licensed under the Apache License, Version 2.0 (the "License"); you may not
.. use this file except in compliance with the License. You may obtain a copy of
.. the License at
..
..   http://www.apache.org/licenses/LICENSE-2.0
..
.. Unless required by applicable law or agreed to in writing, software
.. distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
.. WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
.. License for the specific language governing permissions and limitations under
.. the License.

.. _best-practices/jsdevel:

===========================
JavaScript development tips
===========================

Working with Apache CouchDB's JavaScript environment is a lot different than
working with traditional JavaScript development environments. Here are some
tips and tricks that will ease the difficulty.

.. rst-class:: open

- Check the JavaScript version being used by your CouchDB. As of version 3.2.0,
  this is reported in the output of ``GET /_node/_local/_versions``. Prior to
  version 3.2.0, you will   need to see which JavaScript library is installed by
  your CouchDB binary   distribution, provided by your operating system, or
  linked by your compilation process.

  If the version is 1.8.5, this is an **old** version of JavaScript, only
  supporting the ECMA-262 5th edition ("ES5") of the language. ES6/2015 and
  newer constructs **cannot** be used.

  Fortunately, there are many tools available for transpiling modern JavaScript
  into code compatible with older JS engines. The `Babel Project website
  <http://babeljs.io/repl>`_, for example, offers an in-browser text editor
  which transpiles JavaScript in real-time. Configuring CouchDB-compatibility
  is as easy as enabling the ``ENV PRESET`` option, and typing "firefox 4.0"
  into the *TARGETS* field.

- The ``log()`` function will log output to the CouchDB log file or stream.
  You can log strings, objects, and arrays directly, without first converting
  to JSON.  Use this in conjunction with a local CouchDB instance for best
  results.

- Be sure to guard all document accesses to avoid exceptions when fields
  or subfields are missing: ``if (doc && doc.myarray && doc.myarray.length)...``
