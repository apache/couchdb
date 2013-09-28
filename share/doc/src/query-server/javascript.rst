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

.. default-domain:: js

.. _query-server/js:

JavaScript
==========

.. note:: While every design function has access to all JavaScript objects,
   the table below describes appropriate usage cases. For example,
   you may use :func:`emit` in :ref:`listfun`, but :func:`getRow` is not permitted during :ref:`mapfun`.

+--------------------------------+---------------------------------------------+
| JS Function                    | Reasonable to use in design doc functions   |
+================================+=============================================+
| :func:`emit`                   | :ref:`mapfun`                               |
+--------------------------------+---------------------------------------------+
| :func:`getRow`                 | :ref:`listfun`                              |
+--------------------------------+---------------------------------------------+
| :data:`JSON`                   | any                                         |
+--------------------------------+---------------------------------------------+
| :func:`isArray`                | any                                         |
+--------------------------------+---------------------------------------------+
| :func:`log`                    | any                                         |
+--------------------------------+---------------------------------------------+
| :func:`provides`               | :ref:`showfun`, :ref:`listfun`              |
+--------------------------------+---------------------------------------------+
| :func:`registerType`           | :ref:`showfun`, :ref:`listfun`              |
+--------------------------------+---------------------------------------------+
| :func:`require`                | any, except :ref:`reducefun`                |
+--------------------------------+---------------------------------------------+
| :func:`send`                   | :ref:`listfun`                              |
+--------------------------------+---------------------------------------------+
| :func:`start`                  | :ref:`listfun`                              |
+--------------------------------+---------------------------------------------+
| :func:`sum`                    | any                                         |
+--------------------------------+---------------------------------------------+
| :func:`toJSON`                 | any                                         |
+--------------------------------+---------------------------------------------+

Design functions context
------------------------

Each design function executes in a special context of predefined objects,
modules and functions:


.. function:: emit(key, value)

   Emits a `key`-`value` pair for further processing by CouchDB after the map
   function is done.

   :param key: The view key
   :param value: The `key`'s associated value

   .. code-block:: javascript

      function(doc){
        emit(doc._id, doc._rev);
      }


.. function:: getRow()

   Extracts the next row from a related view result.

   :return: View result row
   :rtype: object

   .. code-block:: javascript

      function(head, req){
        send('[');
        row = getRow();
        if (row){
          send(toJSON(row));
          while(row = getRow()){
            send(',');
            send(toJSON(row));
          }
        }
        return ']';
      }


.. data:: JSON

   `JSON2 <https://git-wip-us.apache.org/repos/asf?p=couchdb.git;a=blob;f=share/server/json2.js>`_
   object.


.. function:: isArray(obj)

   A helper function to check if the provided value is an `Array`.

   :param obj: Any Javascript value
   :return: ``true`` if `obj` is `Array`-typed, ``false`` otherwise
   :rtype: boolean


.. function:: log(message)

   Log a message to the CouchDB log (at the `INFO` level).

   :param message: Message to be logged

   .. code-block:: javascript

      function(doc){
        log('Procesing doc ' + doc['_id']);
        emit(doc['_id'], null);
      }

   After the map function has run, the following line can be found in CouchDB
   logs (e.g. at `/var/log/couchdb/couch.log`):

   .. code-block:: text

      [Sat, 03 Nov 2012 17:38:02 GMT] [info] [<0.7543.0>] OS Process #Port<0.3289> Log :: Processing doc 8d300b86622d67953d102165dbe99467


.. function:: provides(key, func)

   Registers callable handler for specified MIME key.

   :param key: MIME key previously defined by :func:`registerType`
   :param func: MIME type handler


.. function:: registerType(key, *mimes)

   Registers list of MIME types by associated `key`.

   :param key: MIME types
   :param mimes: MIME types enumeration

   Predefined mappings (`key`-`array`):

   - **all**: ``*/*``
   - **text**: ``text/plain; charset=utf-8``, ``txt``
   - **html**: ``text/html; charset=utf-8``
   - **xhtml**: ``application/xhtml+xml``, ``xhtml``
   - **xml**: ``application/xml``, ``text/xml``, ``application/x-xml``
   - **js**: ``text/javascript``, ``application/javascript``,
     ``application/x-javascript``
   - **css**: ``text/css``
   - **ics**: ``text/calendar``
   - **csv**: ``text/csv``
   - **rss**: ``application/rss+xml``
   - **atom**: ``application/atom+xml``
   - **yaml**: ``application/x-yaml``, ``text/yaml``
   - **multipart_form**: ``multipart/form-data``
   - **url_encoded_form**: ``application/x-www-form-urlencoded``
   - **json**: ``application/json``, ``text/x-json``


.. function:: require(path)

   Loads CommonJS module by a specified `path`. The path should not start with
   a slash.

   :param path: A CommonJS module path started from design document root
   :return: Exported statements


.. function:: send(chunk)

   Sends a single string `chunk` in response.

   :param chunk: Text chunk

   .. code-block:: javascript

      function(head, req){
        send('Hello,');
        send(' ');
        send('Couch');
        return !
      }


.. function:: start(init_resp)

   Initiates chunked response. As an option, a custom
   :ref:`response <response_object>` object may be sent at this point.
   For `list`-functions only!

   .. note::

      list functions may set the `HTTP response code` and `headers` by calling
      this function. This function must be called before :func:`send`,
      :func:`getRow` or a `return` statement; otherwise, the query server will
      implicitly call this function with the empty object (``{}``).

   .. code-block:: javascript

      function(head, req){
        start({
          "code": 302,
          "headers": {
            "Location": "http://couchdb.apache.org"
          }
        });
        return "Relax!";
      }


.. function:: sum(arr)

   Sum `arr`'s items.

   :param arr: Array of numbers
   :rtype: number


.. function:: toJSON(obj)

   Encodes `obj` to JSON string. This is an alias for the ``JSON.stringify``
   method.

   :param obj: JSON encodable object
   :return: JSON string

.. _commonjs:

CommonJS Modules
----------------

Support for `CommonJS Modules <http://wiki.commonjs.org/wiki/Modules/1.1.1>`_
(introduced in CouchDB 0.11.0) allows you to create modular design functions
without the need for duplication of functionality.

Here's a CommonJS module that checks user permissions:

.. code-block:: javascript

    function user_context(userctx, secobj) {
      var is_admin = function() {
        return userctx.indexOf('_admin') != -1;
      }
      return {'is_admin': is_admin}
    }

    exports['user'] = user_context

Each module has access to additional global variables:

- **module** (`object`): Contains information about the stored module

  - **id** (`string`): The module id; a JSON path in ddoc context
  - **current** (`code`): Compiled module code object
  - **parent** (`object`): Parent frame
  - **exports** (`object`): Export statements

- **exports** (`object`): Shortcut to the ``module.exports`` object

The CommonJS module can be added to a design document, like so:

.. code-block:: javascript

    {
       "views": {
          "lib": {
             "security": "function user_context(userctx, secobj) { ... }"
          },
          "validate_doc_update": "function(newdoc, olddoc, userctx, secobj) {
            user = require('lib/security').user(userctx, secobj);
            return user.is_admin();
          }"
       },
       "_id": "_design/test"
    }

Modules paths are relative to the design document's ``views`` object, but
modules can only be loaded from the object referenced via ``lib``. The
``lib`` structure can still be used for view functions as well, by simply
storing view functions at e.g. ``views.lib.map``, ``views.lib.reduce``, etc.
