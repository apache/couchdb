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

.. _ddocs:

================
Design Functions
================

In this section we'll show how to write design documents, using the built-in
:ref:`JavaScript Query Server <query-server/js>`.

But before we start to write our first function, let's take a look at the list
of common objects that will be used during our code journey - we'll be using
them extensively within each function:

- :ref:`Database information object <dbinfo_object>`
- :ref:`Request object <request_object>`
- :ref:`Response object <response_object>`
- :ref:`UserCtx object <userctx_object>`
- :ref:`Database Security object <security_object>`
- :ref:`Guide to JavaScript Query Server <query-server/js>`

.. _viewfun:

View functions
==============

Views are the primary tool used for querying and reporting on CouchDB databases.

.. _mapfun:

Map functions
-------------

.. function:: mapfun(doc)

   :param doc: Processed document object.

Map functions accept a single document as the argument and (optionally) :func:`emit`
key/value pairs that are stored in a view.

.. code-block:: javascript

    function (doc) {
      if (doc.type === 'post' && doc.tags && Array.isArray(doc.tags)) {
        doc.tags.forEach(function (tag) {
          emit(tag.toLowerCase(), 1);
        });
      }
    }

In this example a key/value pair is emitted for each value in the `tags` array
of a document with a `type` of "post". Note that :func:`emit` may be called many
times for a single document, so the same document may be available by several
different keys.

Also keep in mind that each document is *sealed* to prevent situation when one
map function changes document state and the other one received a modified
version.

For efficiency reasons, documents are passed to a group of map functions -
each document is processed by group of map functions from all views of
related design document. This means that if you trigger index update for one
view in ddoc, all others will get updated too.

Since `1.1.0` release `map` function supports
:ref:`CommonJS <commonjs>` modules and access to :func:`require` function.

.. _reducefun:

Reduce and rereduce functions
-----------------------------

.. function:: redfun(keys, values[, rereduce])

   :param keys: Array of pairs docid-key for related map function result.
                Always ``null`` if rereduce is running (has ``true`` value).
   :param values: Array of map function result values.
   :param rereduce: Boolean sign of rereduce run.

   :return: Reduces `values`

Reduce functions takes two required arguments of keys and values lists - the
result of the related map function - and optional third one which indicates if
`rereduce` mode is active or not. `Rereduce` is using for additional reduce
values list, so when it is ``true`` there is no information about related `keys`
(first argument is ``null``).

Note, that if produced result by `reduce` function is longer than initial
values list then a Query Server error will be raised. However, this behavior
could be disabled by setting ``reduce_limit`` config option to ``false``:

.. code-block:: ini

   [query_server_config]
   reduce_limit = false

While disabling ``reduce_limit`` might be useful for debug proposes, remember,
that main task of reduce functions is to *reduce* mapped result, not to make it
even bigger. Generally, your reduce function should converge rapidly to a single
value - which could be an array or similar object.


.. _reducefun/builtin:

Builtin reduce functions
^^^^^^^^^^^^^^^^^^^^^^^^

Additionally, CouchDB has three built-in reduce functions. These are implemented
in Erlang and runs inside CouchDB, so they are much faster than the equivalent
JavaScript functions: ``_sum``, ``_count`` and ``_stats``. Their equivalents in
JavaScript below:

.. code-block:: javascript

    // could be replaced by _sum
    function(keys, values) {
      return sum(values);
    }

    // could be replaced by _count
    function(keys, values, rereduce) {
      if (rereduce) {
        return sum(values);
      } else {
        return values.length;
      }
    }

    // could be replaced by _stats
    function(keys, values, rereduce) {
      if (rereduce) {
        return {
          'sum': values.reduce(function(a, b) { return a + b.sum }, 0),
          'min': values.reduce(function(a, b) { return Math.min(a, b.min) }, Infinity),
          'max': values.reduce(function(a, b) { return Math.max(a, b.max) }, -Infinity),
          'count': values.reduce(function(a, b) { return a + b.count }, 0),
          'sumsqr': values.reduce(function(a, b) { return a + b.sumsqr }, 0)
        }
      } else {
        return {
          'sum': sum(values),
          'min': Math.min.apply(null, values),
          'max': Math.max.apply(null, values),
          'count': values.length,
          'sumsqr': (function() {
            var sumsqr = 0;

            values.forEach(function (value) {
              sumsqr += value * value;
            });

            return sumsqr;
          })(),
        }
      }
    }

.. note:: **Why don't reduce functions support CommonJS modules?**

   While `map` functions have limited access to stored modules through
   :func:`require` function there is no such feature for `reduce` functions.
   The reason lies deep inside in mechanism how `map` and `reduce` functions
   are processed by Query Server. Let's take a look on `map` functions first:

   #. CouchDB sends all `map` functions for processed design document to
      Query Server.
   #. Query Server handles them one by one, compiles and puts them onto an
      internal stack.
   #. After all `map` functions had been processed, CouchDB will send the
      remaining documents to index one by one.
   #. The Query Server receives the document object and applies it to every function
      from the stack. The emitted results are then joined into a single array and sent
      back to CouchDB.

   Now let's see how `reduce` functions are handled:

   #. CouchDB sends *as single command* list of available `reduce` functions
      with result list of key-value pairs that was previously received as
      result of `map` functions work.
   #. Query Server compiles reduce functions and applies them to key-value
      lists. Reduced result sends back to CouchDB.

   As you may note, `reduce` functions been applied in single shot while
   `map` ones are applied in an iterative way per each document. This means that
   it's possible for `map` functions to precompile CommonJS libraries and use them
   during the entire view processing, but for `reduce` functions it will be
   compiled again and again for each view result reduction, which will lead to
   performance degradation (`reduce` function are already does hard work to make
   large result smaller).


.. _showfun:

Show functions
==============

.. function:: showfun(doc, req)

   :param doc: Processed document, may be omitted.
   :param req: :ref:`Request object <request_object>`.

   :return: :ref:`Response object <response_object>`
   :rtype: object or string

Show functions are used to represent documents in various formats, commonly as
HTML page with nicer formatting. They can also be used to run server-side functions
without requiring a pre-existing document.

Basic example of show function could be:

.. code-block:: javascript

    function(doc, req){
      if (doc) {
        return "Hello from " + doc._id + "!";
      } else {
        return "Hello, world!";
      }
    }

Also, there is more simple way to return json encoded data:

.. code-block:: javascript

    function(doc, req){
      return {
        'json': {
          'id': doc['_id'],
          'rev': doc['_rev']
        }
      }
    }


and even files (this one is CouchDB logo):

.. code-block:: javascript

    function(doc, req){
      return {
        'headers': {
          'Content-Type' : 'image/png',
        },
        'base64': ''.concat(
          'iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAMAAAAoLQ9TAAAAsV',
          'BMVEUAAAD////////////////////////5ur3rEBn////////////////wDBL/',
          'AADuBAe9EB3IEBz/7+//X1/qBQn2AgP/f3/ilpzsDxfpChDtDhXeCA76AQH/v7',
          '/84eLyWV/uc3bJPEf/Dw/uw8bRWmP1h4zxSlD6YGHuQ0f6g4XyQkXvCA36MDH6',
          'wMH/z8/yAwX64ODeh47BHiv/Ly/20dLQLTj98PDXWmP/Pz//39/wGyJ7Iy9JAA',
          'AADHRSTlMAbw8vf08/bz+Pv19jK/W3AAAAg0lEQVR4Xp3LRQ4DQRBD0QqTm4Y5',
          'zMxw/4OleiJlHeUtv2X6RbNO1Uqj9g0RMCuQO0vBIg4vMFeOpCWIWmDOw82fZx',
          'vaND1c8OG4vrdOqD8YwgpDYDxRgkSm5rwu0nQVBJuMg++pLXZyr5jnc1BaH4GT',
          'LvEliY253nA3pVhQqdPt0f/erJkMGMB8xucAAAAASUVORK5CYII=')
      }
    }

But what if you need to represent data in different formats via a single function?
Functions :func:`registerType` and :func:`provides` are your the best friends in
that question:

.. code-block:: javascript

    function(doc, req){
      provides('json', function(){
        return {'json': doc}
      });
      provides('html', function(){
        return '<pre>' + toJSON(doc) + '</pre>'
      })
      provides('xml', function(){
        return {
          'headers': {'Content-Type': 'application/xml'},
          'body' : ''.concat(
            '<?xml version="1.0" encoding="utf-8"?>\n',
            '<doc>',
            (function(){
              escape = function(s){
                return s.replace(/&quot;/g, '"')
                        .replace(/&gt;/g, '>')
                        .replace(/&lt;/g, '<')
                        .replace(/&amp;/g, '&');
              };
              var content = '';
              for(var key in doc){
                if(!doc.hasOwnProperty(key)) continue;
                var value = escape(toJSON(doc[key]));
                var key = escape(key);
                content += ''.concat(
                  '<' + key + '>',
                  value
                  '</' + key + '>'
                )
              }
              return content;
            })(),
            '</doc>'
          )
        }
      })
      registerType('text-json', 'text/json')
      provides('text-json', function(){
        return toJSON(doc);
      })
    }

This function may return `html`, `json` , `xml` or our custom `text json` format
representation of same document object with same processing rules. Probably,
the `xml` provider in our function needs more care to handle nested objects
correctly, and keys with invalid characters, but you've got the idea!

.. seealso::

   CouchDB Wiki:
    - `Showing Documents <http://wiki.apache.org/couchdb/Formatting_with_Show_and_List#Showing_Documents>`_

   CouchDB Guide:
     - `Show Functions <http://guide.couchdb.org/editions/1/en/show.html>`_


.. _listfun:

List functions
==============

.. function:: listfun(head, req)

   :param head: :ref:`view_head_info_object`
   :param req: :ref:`Request object <request_object>`.

   :return: Last chunk.
   :rtype: string

While :ref:`showfun` are used to customize document presentation, :ref:`listfun`
are used for same purpose, but against :ref:`viewfun` results.

The next list function formats view and represents it as a very simple HTML page:

.. code-block:: javascript

    function(head, req){
      start({
        'headers': {
          'Content-Type': 'text/html'
        }
      });
      send('<html><body><table>');
      send('<tr><th>ID</th><th>Key</th><th>Value</th></tr>')
      while(row = getRow()){
        send(''.concat(
          '<tr>',
          '<td>' + toJSON(row.id) + '</td>',
          '<td>' + toJSON(row.key) + '</td>',
          '<td>' + toJSON(row.value) + '</td>',
          '</tr>'
        ));
      }
      send('</table></body></html>');
    }

Templates and styles could obviously be used to present data in a nicer
fashion, but this is an excellent starting point. Note that you may also
use :func:`registerType` and :func:`provides` functions in the same
way as for :ref:`showfun`!

.. seealso::

   CouchDB Wiki:
    - `Listing Views with CouchDB 0.10 and later <http://wiki.apache.org/couchdb/Formatting_with_Show_and_List#Listing_Views_with_CouchDB_0.10_and_later>`_

   CouchDB Guide:
    - `Transforming Views with List Functions <http://guide.couchdb.org/draft/transforming.html>`_


.. _updatefun:

Update functions
================

.. function:: updatefun(doc, req)

   :param doc: Update function target document.
   :param req: :ref:`request_object`

   :returns: Two-element array: the first element is the (updated or new)
             document, which is committed to the database. If the first element
             is ``null`` no document will be committed to the database.
             If you are updating an existing, it should already have an ``_id``
             set, and if you are creating a new document, make sure to set its
             ``_id`` to something, either generated based on the input or the
             ``req.uuid`` provided. The second element is the response that will
             be sent back to the caller.

Update handlers are functions that clients can request to invoke server-side
logic that will create or update a document. This feature allows a range of use
cases such as providing a server-side last modified timestamp, updating
individual fields in a document without first getting the latest revision, etc.

When the request to an update handler includes a document ID in the URL, the
server will provide the function with the most recent version of that document.
You can provide any other values needed by the update handler function via the
``POST``/``PUT`` entity body or query string parameters of the request.

The basic example that demonstrates all use-cases of update handlers below:

.. code-block:: javascript

    function(doc, req){
        if (!doc){
            if ('id' in req){
                // create new document
                return [{'_id': req['id']}, 'New World']
            }
            // change nothing in database
            return [null, 'Empty World']
        }
        doc['world'] = 'hello';
        doc['edited_by'] = req['userCtx']['name']
        return [doc, 'Edited World!']
    }

.. seealso::

   CouchDB Wiki:
    - `Document Update Handlers <http://wiki.apache.org/couchdb/Document_Update_Handlers>`_


.. _filterfun:

Filter functions
================

.. function:: filterfun(doc, req)

   :param doc: Processed document object.
   :param req: :ref:`request_object`
   :return: Boolean value: ``true`` means that `doc` passes the filter rules,
            ``false`` means that it does not.

Filter functions mostly act like :ref:`showfun` and :ref:`listfun`: they
format, or *filter* the :ref:`changes feed<changes>`.

Classic filters
---------------

By default the changes feed emits all database documents changes. But if you're
waiting for some special changes, processing all documents is inefficient.

Filters are special design document functions that allow the changes feed to emit
only specific documents that pass filter rules.

Let's assume that our database is a mailbox and we need to handle only new mail
events (documents with status `new`). Our filter function will look like this:

.. code-block:: javascript

  function(doc, req){
    // we need only `mail` documents
    if (doc.type != 'mail'){
      return false;
    }
    // we're interested only in `new` ones
    if (doc.status != 'new'){
      return false;
    }
    return true; // passed!
  }
 
Filter functions must return ``true`` if a document passed all defined
rules. Now, if you apply this function to the changes feed it will emit only changes
about "new mails"::

    GET /somedatabase/_changes?filter=mailbox/new_mail HTTP/1.1

.. code-block:: javascript

    {"results":[
    {"seq":1,"id":"df8eca9da37dade42ee4d7aa3401f1dd","changes":[{"rev":"1-c2e0085a21d34fa1cecb6dc26a4ae657"}]},
    {"seq":7,"id":"df8eca9da37dade42ee4d7aa34024714","changes":[{"rev":"1-29d748a6e87b43db967fe338bcb08d74"}]},
    ],
    "last_seq":27}

Note that the value of ``last_seq`` is 27, but we'd received only two records.
Seems like any other changes were for documents that haven't passed our filter.

We probably need to filter the changes feed of our mailbox by more than a single
status value. We're also interested in statuses like "spam" to update
spam-filter heuristic rules, "outgoing" to let a mail daemon actually send mails,
and so on. Creating a lot of similar functions that actually do similar work
isn't good idea - so we need a dynamic filter.

You may have noticed that filter functions take a second argument named
:ref:`request <request_object>` - it allows creating dynamic filters
based on query parameters, :ref:`user context <userctx_object>` and more.

The dynamic version of our filter looks like this:

.. code-block:: javascript

  function(doc, req){
    // we need only `mail` documents
    if (doc.type != 'mail'){
      return false;
    }
    // we're interested only in requested status
    if (doc.status != req.query.status){
      return false;
    }
    return true; // passed!
  }

and now we have passed the `status` query parameter in request to let our filter match
only required documents::

    GET /somedatabase/_changes?filter=mailbox/by_status&status=new HTTP/1.1

.. code-block:: javascript

    {"results":[
    {"seq":1,"id":"df8eca9da37dade42ee4d7aa3401f1dd","changes":[{"rev":"1-c2e0085a21d34fa1cecb6dc26a4ae657"}]},
    {"seq":7,"id":"df8eca9da37dade42ee4d7aa34024714","changes":[{"rev":"1-29d748a6e87b43db967fe338bcb08d74"}]},
    ],
    "last_seq":27}

and we can easily change filter behavior with::

    GET /somedatabase/_changes?filter=mailbox/by_status&status=spam HTTP/1.1

.. code-block:: javascript

    {"results":[
    {"seq":11,"id":"8960e91220798fc9f9d29d24ed612e0d","changes":[{"rev":"3-cc6ff71af716ddc2ba114967025c0ee0"}]},
    ],
    "last_seq":27}


Combining filters with a `continuous` feed allows creating powerful event-driven
systems.

.. _viewfilter:

View filters
------------

View filters are the same as above, with one small difference: they use
views `map` function instead to `filter` one to process the changes feed. Each
time when a key-value pair could be emitted, a change is returned. This allows
to avoid creating filter functions that are mostly does same works as views.

To use them just specify `_view` value for ``filter`` parameter and
`designdoc/viewname` for ``view`` one::

    GET /somedatabase/_changes?filter=_view&view=dname/viewname  HTTP/1.1

.. note::

   Since view filters uses `map` functions as filters, they can't show any
   dynamic behavior since :ref:`request object<request_object>` is not
   available.

.. seealso::

   CouchDB Guide:
    - `Guide to filter change notification <http://guide.couchdb.org/draft/notifications.html#filters>`_

   CouchDB Wiki:
    - `Filtered replication <http://wiki.apache.org/couchdb/Replication#Filtered_Replication>`_


.. _vdufun:

Validate document update functions
==================================

.. function:: validatefun(newDoc, oldDoc, userCtx, secObj)

   :param newDoc: New version of document that will be stored.
   :param oldDoc: Previous version of document that is already stored.
   :param userCtx: :ref:`userctx_object`
   :param secObj: :ref:`security_object`

   :throws: ``forbidden`` error to gracefully prevent document storing.
   :throws: ``unauthorized`` error to prevent storage and allow the user to
            re-auth.

A design document may contain a function named `validate_doc_update`
which can be used to prevent invalid or unauthorized document update requests
from being stored.  The function is passed the new document from the update
request, the current document stored in the database, a :ref:`userctx_object`
containing information about the user writing the document (if present), and
a :ref:`security_object` with lists of database security roles.

Validation functions typically examine the structure of the new document to
ensure that required fields are present and to verify that the requesting user
should be allowed to make changes to the document properties.  For example,
an application may require that a user must be authenticated in order to create
a new document or that specific document fields be present when a document
is updated. The validation function can abort the pending document write
by throwing one of two error objects:

.. code-block:: javascript

  // user is not authorized to make the change but may re-authenticate
  throw({ unauthorized: 'Error message here.' });
  
  // change is not allowed
  throw({ forbidden: 'Error message here.' });

Document validation is optional, and each design document in the database may
have at most one validation function.  When a write request is received for
a given database, the validation function in each design document in that
database is called in an unspecified order.  If any of the validation functions
throw an error, the write will not succeed.

**Example**: The ``_design/_auth`` ddoc from `_users` database uses a validation
function to ensure that documents contain some required fields and are only
modified by a user with the ``_admin`` role:

.. code-block:: javascript

    function(newDoc, oldDoc, userCtx, secObj) {
        if (newDoc._deleted === true) {
            // allow deletes by admins and matching users
            // without checking the other fields
            if ((userCtx.roles.indexOf('_admin') !== -1) ||
                (userCtx.name == oldDoc.name)) {
                return;
            } else {
                throw({forbidden: 'Only admins may delete other user docs.'});
            }
        }

        if ((oldDoc && oldDoc.type !== 'user') || newDoc.type !== 'user') {
            throw({forbidden : 'doc.type must be user'});
        } // we only allow user docs for now

        if (!newDoc.name) {
            throw({forbidden: 'doc.name is required'});
        }

        if (!newDoc.roles) {
            throw({forbidden: 'doc.roles must exist'});
        }

        if (!isArray(newDoc.roles)) {
            throw({forbidden: 'doc.roles must be an array'});
        }

        if (newDoc._id !== ('org.couchdb.user:' + newDoc.name)) {
            throw({
                forbidden: 'Doc ID must be of the form org.couchdb.user:name'
            });
        }

        if (oldDoc) { // validate all updates
            if (oldDoc.name !== newDoc.name) {
                throw({forbidden: 'Usernames can not be changed.'});
            }
        }

        if (newDoc.password_sha && !newDoc.salt) {
            throw({
                forbidden: 'Users with password_sha must have a salt.' +
                    'See /_utils/script/couch.js for example code.'
            });
        }

        var is_server_or_database_admin = function(userCtx, secObj) {
            // see if the user is a server admin
            if(userCtx.roles.indexOf('_admin') !== -1) {
                return true; // a server admin
            }

            // see if the user a database admin specified by name
            if(secObj && secObj.admins && secObj.admins.names) {
                if(secObj.admins.names.indexOf(userCtx.name) !== -1) {
                    return true; // database admin
                }
            }

            // see if the user a database admin specified by role
            if(secObj && secObj.admins && secObj.admins.roles) {
                var db_roles = secObj.admins.roles;
                for(var idx = 0; idx < userCtx.roles.length; idx++) {
                    var user_role = userCtx.roles[idx];
                    if(db_roles.indexOf(user_role) !== -1) {
                        return true; // role matches!
                    }
                }
            }

            return false; // default to no admin
        }

        if (!is_server_or_database_admin(userCtx, secObj)) {
            if (oldDoc) { // validate non-admin updates
                if (userCtx.name !== newDoc.name) {
                    throw({
                        forbidden: 'You may only update your own user document.'
                    });
                }
                // validate role updates
                var oldRoles = oldDoc.roles.sort();
                var newRoles = newDoc.roles.sort();

                if (oldRoles.length !== newRoles.length) {
                    throw({forbidden: 'Only _admin may edit roles'});
                }

                for (var i = 0; i < oldRoles.length; i++) {
                    if (oldRoles[i] !== newRoles[i]) {
                        throw({forbidden: 'Only _admin may edit roles'});
                    }
                }
            } else if (newDoc.roles.length > 0) {
                throw({forbidden: 'Only _admin may set roles'});
            }
        }

        // no system roles in users db
        for (var i = 0; i < newDoc.roles.length; i++) {
            if (newDoc.roles[i][0] === '_') {
                throw({
                    forbidden:
                    'No system roles (starting with underscore) in users db.'
                });
            }
        }

        // no system names as names
        if (newDoc.name[0] === '_') {
            throw({forbidden: 'Username may not start with underscore.'});
        }

        var badUserNameChars = [':'];

        for (var i = 0; i < badUserNameChars.length; i++) {
            if (newDoc.name.indexOf(badUserNameChars[i]) >= 0) {
                throw({forbidden: 'Character `' + badUserNameChars[i] +
                        '` is not allowed in usernames.'});
            }
        }
    }

.. note::

   The ``return`` statement used only for function, it has no impact on
   the validation process.

.. seealso::

   CouchDB Guide:
    - `Validation Functions <http://guide.couchdb.org/editions/1/en/validation.html>`_

   CouchDB Wiki:
    - `Document Update Validation <http://wiki.apache.org/couchdb/Document_Update_Validation>`_
