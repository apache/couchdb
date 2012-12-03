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

.. _commonjs:

CommonJS support for map functions
==================================

CommonJS support allows you to use `CommonJS notation <http://commonjs.org/specs>`_
inside map and reduce functions, but only of libraries that are stored
inside the views part of the design doc.

So you could continue to access CommonJS code in design\_doc.foo, from
your list functions etc, but we'd add the ability to require CommonJS
modules within map and reduce, but only from ``design_doc.views.lib``.

There's no worry here about namespace collisions, as Couch just plucks
``views.*.map`` and ``views.*.reduce`` out of the design doc. So you
could have a view called ``lib`` if you wanted, and still have CommonJS
stored in ``views.lib.sha1`` and ``views.lib.stemmer`` if you wanted.

The implementation is simplified by enforcing that CommonJS modules to
be used in map functions be stored in views.lib.

A sample design doc (taken from the test suite in Futon) is below:

.. code-block:: javascript

    {
       "views" : {
          "lib" : {
             "baz" : "exports.baz = 'bam';",
             "foo" : {
                "zoom" : "exports.zoom = 'yeah';",
                "boom" : "exports.boom = 'ok';",
                "foo" : "exports.foo = 'bar';"
             }
          },
          "commonjs" : {
             "map" : "function(doc) { emit(null, require('views/lib/foo/boom').boom)}"
          }
       },
       "_id" : "_design/test"
    }

The ``require()`` statement is relative to the design document, but
anything loaded form outside of ``views/lib`` will fail.
