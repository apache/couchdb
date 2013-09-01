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

.. _intro/curl:

==============================
cURL: Your Command Line Friend
==============================

The ``curl`` utility is a command line tool available on Unix, Linux,
Mac OS X and Windows and many other platforms. ``curl`` provides easy
access to the HTTP protocol (among others) directly from the
command-line and is therefore an ideal way of interacting with CouchDB
over the HTTP REST API.

For simple ``GET`` requests you can supply the URL of the request. For
example, to get the database information:

.. code-block:: bash

    shell> curl http://127.0.0.1:5984

This returns the database information (formatted in the output below for
clarity):

.. code-block:: json

  {
      "couchdb": "Welcome",
      "uuid": "85fb71bf700c17267fef77535820e371",
      "vendor": {
          "name": "The Apache Software Foundation",
          "version": "1.4.0"
      },
      "version": "1.4.0"
  }


.. note:: For some URLs, especially those that include special characters such
   as ampersand, exclamation mark, or question mark, you should quote
   the URL you are specifying on the command line. For example:

   .. code-block:: bash

      shell> curl 'http://couchdb:5984/_uuids?count=5'

You can explicitly set the HTTP command using the ``-X`` command line
option. For example, when creating a database, you set the name of the
database in the URL you send using a PUT request:

.. code-block:: bash

    shell> curl -X PUT http://127.0.0.1:5984/demo
    {"ok":true}

But to obtain the database information you use a ``GET`` request (with
the return information formatted for clarity):

.. code-block:: bash

    shell> curl -X GET http://127.0.0.1:5984/demo
    {
       "compact_running" : false,
       "doc_count" : 0,
       "db_name" : "demo",
       "purge_seq" : 0,
       "committed_update_seq" : 0,
       "doc_del_count" : 0,
       "disk_format_version" : 5,
       "update_seq" : 0,
       "instance_start_time" : "1306421773496000",
       "disk_size" : 79
    }

For certain operations, you must specify the content type of request,
which you do by specifying the ``Content-Type`` header using the ``-H``
command-line option:

.. code-block:: bash

    shell> curl -H 'Content-Type: application/json' http://127.0.0.1:5984/_uuids

You can also submit 'payload' data, that is, data in the body of the
HTTP request using the ``-d`` option. This is useful if you need to
submit JSON structures, for example document data, as part of the
request. For example, to submit a simple document to the ``demo``
database:

.. code-block:: bash

    shell> curl -H 'Content-Type: application/json' \
                -X POST http://127.0.0.1:5984/demo \
                -d '{"company": "Example, Inc."}'
    {"ok":true,"id":"8843faaf0b831d364278331bc3001bd8",
     "rev":"1-33b9fbce46930280dab37d672bbc8bb9"}

In the above example, the argument after the ``-d`` option is the JSON
of the document we want to submit.

The document can be accessed by using the automatically generated
document ID that was returned:

.. code-block:: bash

    shell> curl -X GET http://127.0.0.1:5984/demo/8843faaf0b831d364278331bc3001bd8
    {"_id":"8843faaf0b831d364278331bc3001bd8",
     "_rev":"1-33b9fbce46930280dab37d672bbc8bb9",
     "company":"Example, Inc."}

The API samples in the :ref:`api/basics` show the HTTP command, URL and any
payload information that needs to be submitted (and the expected return
value). All of these examples can be reproduced using ``curl`` with the
command-line examples shown above.
