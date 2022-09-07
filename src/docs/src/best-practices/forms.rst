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

.. _best-practices/forms:

====================================
Document submission using HTML Forms
====================================

It is possible to write to a CouchDB document directly from an HTML form by
using a document :ref:`update function <updatefun>`. Here's how:

The HTML form
=============

First, write an HTML form. Here's a simple "Contact Us" form excerpt:

.. code-block:: html

    <form action="/dbname/_design/ddocname/_update/contactform" method="post">
        <div>
            <label for="name">Name:</label>
            <input type="text" id="name" name="name" />
        </div>
        <div>
            <label for="mail">Email:</label>
            <input type="text" id="mail" name="email" />
        </div>
        <div>
            <label for="msg">Message:</label>
            <textarea id="msg" name="message"></textarea>
        </div>
    </form>

Customize the ``/dbname/_design/ddocname/_update/contactform`` portion of the
form action URL to reflect the exact path to your database, design document
and update function (see below).

As CouchDB
:ref:`no longer recommends the use of CouchDB-hosted web applications <indexes>`
, you may want to use a reverse proxy to expose CouchDB as a subdirectory of
your web application.  If so, add that prefix to the ``action`` destination in
the form.

Another option is to alter CouchDB's :ref:`CORS <cors>` settings and use a
cross-domain POST. *Be sure you understand all security implications before
doing this!*

The update function
===================

Then, write an update function. This is the server-side JavaScript function
that will receive the ``POST``-ed data.

The first argument to the function will be the document that is being processed
(if it exists). Because we are using ``POST`` and not ``PUT``, this should be
empty in our scenario - but we should check to be sure. The ``POST``-ed data
will be passed as the second parameter to the function, along with any query
parameters and the full request headers.

Here's a sample handler that extracts the form data, generates a document _id
based on the email address and timestamp, and saves the document. It then
returns a JSON success response back to the browser.

.. code-block:: javascript

    function(doc, req) {

        if (doc) {
            return [doc, toJSON({"error": "request already filed"})]
        }

        if !(req.form && req.form.email) {
            return [null, toJSON({"error": "incomplete form"})]
        }

        var date = new Date()
        var newdoc = req.form
        newdoc._id = req.form.email + "_" + date.toISOString()

        return [newdoc, toJSON({"success":"ok"})]
    }

Place the above function in your design document under the ``updates`` key.

Note that this function does not attempt any sort of input validation or
sanitization. That is best handled by a
:ref:`validate document update function <vdufun>` instead.  (A "VDU" will
validate any document written to the database, not just those that use your
update function.)

If the first element passed to ``return`` is a document, the HTTP response
headers will include ``X-Couch-Id``, the ``_id`` value for the newly created
document, and ``X-Couch-Update-NewRev``, the ``_rev`` value for the newly
created document. This is handy if your client-side code wants to access or
update the document in a future call.

Example output
==============

Here's the worked sample above, using ``curl`` to simulate the form POST.

.. code-block:: bash

    $ curl -X PUT localhost:5984/testdb/_design/myddoc -d '{ "updates": { "contactform": "function(doc, req) { ... }" } }'
    {"ok":true,"id":"_design/myddoc","rev":"1-2a2b0951fcaf7287817573b03bba02ed"}

    $ curl --data "name=Lin&email=lin@example.com&message=I Love CouchDB" http://localhost:5984/testdb/_design/myddoc/_update/contactform
    *   Trying 127.0.0.1...
    * TCP_NODELAY set
    * Connected to localhost (127.0.0.1) port 5984 (#1)
    > POST /testdb/_design/myddoc/_update/contactform HTTP/1.1
    > Host: localhost:5984
    > User-Agent: curl/7.59.0
    > Accept: */*
    > Content-Length: 53
    > Content-Type: application/x-www-form-urlencoded
    >
    * upload completely sent off: 53 out of 53 bytes
    < HTTP/1.1 201 Created
    < Content-Length: 16
    < Content-Type: text/html; charset=utf-8
    < Date: Thu, 05 Apr 2018 19:56:42 GMT
    < Server: CouchDB/2.2.0-948a1311c (Erlang OTP/19)
    < X-Couch-Id: lin%40example.com_2018-04-05T19:51:22.278Z
    < X-Couch-Request-ID: 03a5f4fbe0
    < X-Couch-Update-NewRev: 1-34483732407fcc6cfc5b60ace48b9da9
    < X-CouchDB-Body-Time: 0
    <
    * Connection #1 to host localhost left intact
    {"success":"ok"}

    $ curl http://localhost:5984/testdb/lin\@example.com_2018-04-05T19:51:22.278Z
    {"_id":"lin@example.com_2018-04-05T19:51:22.278Z","_rev":"1-34483732407fcc6cfc5b60ace48b9da9","name":"Lin","email":"lin@example.com","message":"I Love CouchDB"}
