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

===========================
JavaScript engine versions
===========================

Until version 3.4 Apache CouchDB used only SpiderMonkey as its underlying
JavaScript engine. With version 3.4, it's possible to configure CouchDB to use
QuickJS.

Recent versions of CouchDB may use the node-local ``_versions`` API endpoint to
get the current engine type and version:

.. code-block:: bash

   % http http://adm:pass@localhost:5984/_node/_local/_versions | jq '.javascript_engine'
   {
     "version": "1.8.5",
     "name": "spidermonkey"
   }

SpiderMonkey version compatibility
==================================

Depending on the CouchDB version and what's available on supported operating
systems, the SpiderMonkey version may be any one of these: 1.8.5, 60, 68, 78,
86 or 91. Sometimes there are differences in supported features between
versions. Usually later versions only add features, so views will work on
version upgrades. However, there are a few exceptions to this. These are a few
known regression or discrepancies between versions:

1. ``for each (var x in ...)``

Version ``1.8.5`` supports the ``for each (var x in ...)`` looping
expression. That's not a standard JavaScript syntax and is not supported in
later versions:

.. code-block:: bash

   % js
   js>  for each (var x in [1,2]) {print(x)}
   1
   2

   % js91
   js> for each (var x in [1,2]) {print(x)}
   typein:1:4 SyntaxError: missing ( after for:
   typein:1:4 for each (var x in [1,2]) {print(x)}
   typein:1:4 ....^

2. E4X (ECMAScript for XML)

This is not supported in versions greater than ``1.8.5``. This feature may be
inadvertently triggered when inserting a ``.`` character between a variable and
``(``. That would compile on ``1.8.5`` and throw a ``SyntaxError`` on other
versions:

.. code-block:: bash

   % js
   js> var xml = <root><x></x></root>
   js> xml.(x)
   <root>
     <x/>
   </root>

   % js91
   js>  var xml = <root><x></x></root>
   typein:1:11 SyntaxError: expected expression, got '<':
   typein:1:11  var xml = <root><x></x></root>
   typein:1:11 ...........^

3. ``toLocaleFormat(...)`` function.

This ``Date`` function is not present in versions greater than ``1.8.5``:

.. code-block:: bash

   % js
   js> d = new Date("Dec 1, 2015 3:22:46 PM")
   (new Date(1449001366000))
   js> d.toLocaleFormat("%Y-%m-%d")
   "2015-12-01"

   % js91
   js> d = new Date("Dec 1, 2015 3:22:46 PM")
   (new Date(1449001366000))
   js> d.toLocaleFormat("%Y-%m-%d")
   typein:2:3 TypeError: d.toLocaleFormat is not a function

4. ``toLocaleString(...)`` function.

SpiderMonkey 1.8.5 ignored locale strings. Later versions started to return the
correct format:

.. code-block:: bash

   % js
   js > (new Date("2019-01-15T19:32:52.915Z")).toLocaleString('en-US')
   "Tue Jan 15 14:32:52 2019"

   % js91
   js > (new Date("2019-01-15T19:32:52.915Z")).toLocaleString('en-US')
   "01/15/2019, 02:32:52 PM"

Spidermonkey 91 output also match QuickJS and v8.

5. Invalid expressions following ``function(){...}`` are not ignored any longer
   and will throw an error.

Previously, in versions less than or equal to ``1.8.5`` it was possible add any
expression following the main function definition and they were mostly ignored:

.. code-block:: bash

   $ http put $DB/db/_design/d4 views:='{"v1":{"map":"function(doc){emit(1,2);} if(x) a"}}'
   HTTP/1.1 201 Created
   {
       "id": "_design/d4",
       "ok": true,
       "rev": "1-08a7d8b139e52f5f3df5bc27e20eeff1"
   }

   % http $DB/db/_design/d4/_view/v1
   HTTP/1.1 200 OK
   {
       "offset": 0,
       "rows": [
           {
               "id": "doc1",
               "key": 1,
               "value": 2
           }
       ],
       "total_rows": 1
   }

With higher versions of SpiderMonkey, that would throw a compilation error:

.. code-block:: bash

    $ http put $DB/db/_design/d4 views:='{"v1":{"map":"function(doc){emit(1,2);} if(x) a"}}'
    HTTP/1.1 400 Bad Request
    {
        "error": "compilation_error",
        "reason": "Compilation of the map function in the 'v1' view failed: ..."
    }

6. Object key order.

Object key order may change between versions, so any views which rely on that
order may emit different results depending on the engine version:

.. code-block:: bash

   % js
   js> r={}; ["Xyz", "abc", 1].forEach(function(v) {r[v]=v;}); Object.keys(r)
   ["Xyz", "abc", "1"]

   % js91
   js> r={}; ["Xyz", "abc", 1].forEach(function(v) {r[v]=v;}); Object.keys(r)
   ["1", "Xyz", "abc"]

7. String ``match(undefined)``

Spidermonkey 1.8.5 returns ``null`` for ``match(undefined)`` while versions
starting with at least ``78`` return ``[""]``.

.. code-block:: bash

   % js
   js> "abc".match(undefined)
   null

   % js91
   js> "abc".match(undefined)
   [""]

8. String ``substring(val, start, end)``

Spidermonkey ``1.8.5`` has a ``String.substring(val, start, end)`` function. That
function is not present in at least Spidermonkey ``91`` and higher:

.. code-block:: bash

    % js
    js> String.substring("abcd", 1, 2)
    "b"

    % js91
    js> String.substring("abcd", 1, 2)
    typein:1:8 TypeError: String.substring is not a function
    Stack:
        @typein:1:

Use ``String.prototype.substring(start, end)`` instead:

.. code-block:: bash

    % js91
    js> "abcd".substring(1, 2)
    "b"

9. The ``toISOString()`` throws an error on invalid ``Date`` objects.

SpiderMonkey version ``1.8.5`` does not throw an error when calling
``toISOString()`` on invalid ``Date`` objects, but SpiderMonkey versions at
least ``78+`` do:

.. code-block:: bash

  % js
  js>  (new Date(undefined)).toISOString()
  "Invalid Date"

  % js91
  js> (new Date(undefined)).toISOString()
  typein:1:23 RangeError: invalid date
  Stack:
    @typein:1:23

This can affect views emitting an invalid date object. Previousy, the view
might have emitted the "Invalid Date" string, while in later SpiderMonkey
engines all the emit results from that document will be skipped, since view
functions skip view results if an exception is thrown.

10. Invalid JavaScript before function definition

SpiderMoneky version ``1.8.5`` allowed the invalid ``term : function(...)``
syntax. So a view function like the following worked and produced successfull
view results. In later version, at least as of ``78+``, that function will fail
with a compilation error:

.. code-block:: javascript

   "views": {
            "v1": {
                 "map": "foo : function(doc){emit(doc._id, 1);}"
        }
    }

11. Constant values leak out of nested scopes

In Spidermonkey 1.8.5 ``const`` values leak from nested expression scopes.
Referencing them in Spidermonkey 1.8.5 produces ``undefined``, while in
Spidermonkey 91, QuickJS and V8 engines raises a ``ReferenceError``.

.. code-block::

  % js
  js> f = function(doc){if(doc.x === 'x') { const value='inside_if'}; print(value)};
  js> f({'x':'y'})
  undefined

  % js91
  js> f = function(doc){if(doc.x === 'x') {const value='inside_if';}; print(value)};
  js> f({'x':'y'})
  typein:1:23 TypeError: can't access property "x", doc is undefined

12. Zero-prefixed input with ``parseInt()``

The ``parseInt()`` function in Spidermonkey 1.8.5 treats a leading ``0`` as
octal (base 8) prefix. It then parses the following input as an octal number.
Spidermonkey 91, and other modern JS engine, assume a base 10 as a default even
when parsing numbers with leading zeros. This can be a stumbling block
especially when parsing months and days in a date string. One way to mitigate
this discrepancy is to use an explicit base.

.. code-block::

  % js
  js> parseInt("08")
  0
  js> parseInt("09")
  0
  js> parseInt("010")
  8
  js> parseInt("08", 10)
  8

  % js91
  js> parseInt("08")
  8
  js> parseInt("09")
  9
  js> parseInt("010")
  10
  js> parseInt("08", 10)
  8

13. Callable regular expressions

Spidermonkey 1.8.5 allowed calling regular expression as a function. The call
worked the same as calling the ``.exec()`` method.

.. code-block::

  % js
  js> /.*abc$/("abc")
  ["abc"]

  % js91
  js> /.*abc$/("abc")
  typein:1:9 TypeError: /.*abc$/ is not a function
  Stack:
    @typein:1:9
  js> /.*abc$/.exec("abc")
  ["abc"]

Using QuickJS
=============

The QuickJS-based JavaScript engine is available as of CouchDB version 3.4. It
has to be explicitly enabled via ``[couchdb] js_engine = quickjs`` and
restarting the service.

Generally, QuickJS engine is a bit faster, consumes less memory, and provides
slightly better isolation between contexts by re-creating the whole javascript
engine runtime on every ``reset`` command.

To try building invidual views using QuickJS, even when the default engine is
SpiderMonkey, can use the ``"javascript_quickjs"`` as the view language,
instead of ``"javascript"``. Just that view will be rebuilt using the QuickJS
engine. However, when switching back to ``"javascript"`` the view will have to
be re-built again.

QuickJS vs SpiderMonkey incompatibilities
============================================

The QuickJS engine is quite compatible with SpiderMonkey version 91. The same
incompatibilities between 1.8.5 and 91 are also present between 1.8.5 and
QuickJS. So, when switching from 1.8.5 to QuickJS see the ``SpiderMonkey version
compatibility`` section above.

These are a few incompatibilties between SpiderMonkey 91 and QuickJS engine:

1. ``RegExp.$1``, ..., ``RegExp.$9``

This is a deprecated JavaScript feature that's not available in QuickJS.
https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/RegExp/n

2. ``Date.toString()`` doesn't include the timezone name, just the offset.

.. code-block:: bash

   % qjs > (new Date()).toString();
   "Thu Sep 05 2024 17:03:23 GMT-0400"

   % js91
   js>  (new Date()).toString();
   "Thu Sep 05 2024 17:04:03 GMT-0400 (EDT)"

Scanning for QuickJS incompatibilities
======================================

CouchDB version 3.4 and higher include a background scanner which can be used
traverse all the databases and design documents and run them agaiinst
SpiderMonkey and the QuickJS engine and report any discrepancies in the logs.
That could be a useful run before deciding to switch to QuickJS as the default
JavaScript engine.

The scanner can be enabled with:

.. code-block:: ini

   [couch_scanner_plugins]
   couch_quickjs_scanner_plugin = true

And configured to run at a predetermined time or on a periodic schedule. For
instance:

.. code-block:: ini

   [couch_quickjs_scanner_plugin]
   after = 2024-09-05T18:10:00
   repeat = 1_day

It will not start until after the specified time and then it will run about once
every 24 hours.

The logs will indicate when the scan starts and finishes:

.. code-block:: text

   couch_quickjs_scanner_plugin s:1725559802-c615220453e6 starting
   ...
   couch_quickjs_scanner_plugin s:1725559802-c615220453e6 completed

During scanning discrepancies are reported in the log. They may look like:

.. code-block:: text

   couch_quickjs_scanner_plugin s:1725559802-c615220453e6
   db:mydb/40000000-5fffffff
   ddoc:_design/mydesign
   view validation failed
   {map_doc,<<"doc1">>, $quickjs_res, $sm_res}

The ``s:...`` field indicates which scan session it belongs to, which db and
shard range it found the issue on, followed by the design document, and the
document ID. Then, the ``{map_doc, ..., ...}`` tuple indicates which operation
failed (mapping a document) where the 2nd element is the result from the
QuickJS engine, and the 3rd is the result from the SpiderMonkey engine.

Sometimes it maybe needed to ignore some databases or design documents. That
can be done with a number of regular expression patterns in the
``[couch_quickjs_scanner_plugin.skip_dbs]`` config section:

.. code-block:: ini

   [couch_quickjs_scanner_plugin.skip_dbs]
   pattern1 = bar.*
   pattern2 = .*foo
