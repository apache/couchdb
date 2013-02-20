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

========================
JSON Structure Reference
========================

The following appendix provides a quick reference to all the JSON structures
that you can supply to CouchDB, or get in return to requests.

All Database Documents
======================

+--------------------------------+---------------------------------------------+
| Field                          | Description                                 |
+================================+=============================================+
| total_rows                     | Number of documents in the database/view    |
+--------------------------------+---------------------------------------------+
| offset                         | Offset where the document list started      |
+--------------------------------+---------------------------------------------+
| update_seq (optional)          | Current update sequence for the database    |
+--------------------------------+---------------------------------------------+
| rows [array]                   | Array of document object                    |
+--------------------------------+---------------------------------------------+

Bulk Document Response
======================

+--------------------------------+---------------------------------------------+
| Field                          | Description                                 |
+================================+=============================================+
| docs [array]                   | Bulk Docs Returned Documents                |
+--------------------------------+---------------------------------------------+
|         id                     | Document ID                                 |
+--------------------------------+---------------------------------------------+
|         error                  | Error type                                  |
+--------------------------------+---------------------------------------------+
|         reason                 | Error string with extended reason           |
+--------------------------------+---------------------------------------------+

Bulk Documents
==============

+--------------------------------+---------------------------------------------+
| Field                          | Description                                 |
+================================+=============================================+
| all_or_nothing (optional)      | Sets the database commit mode to use        |
|                                | all-or-nothing semantics                    |
+--------------------------------+---------------------------------------------+
| docs [array]                   | Bulk Documents Document                     |
+--------------------------------+---------------------------------------------+
|         _id (optional)         | Document ID                                 |
+--------------------------------+---------------------------------------------+
|         _rev (optional)        | Revision ID (when updating an existing      |
|                                | document)                                   |
+--------------------------------+---------------------------------------------+
|         _deleted (optional)    | Whether the document should be deleted      |
+--------------------------------+---------------------------------------------+

Changes information for a database
==================================

+--------------------------------+---------------------------------------------+
| Field                          | Description                                 |
+================================+=============================================+
| last_seq                       | Last change sequence number                 |
+--------------------------------+---------------------------------------------+
| results [array]                | Changes made to a database                  |
+--------------------------------+---------------------------------------------+
|         seq                    | Update sequence number                      |
+--------------------------------+---------------------------------------------+
|         id                     | Document ID                                 |
+--------------------------------+---------------------------------------------+
|         changes [array]        | List of changes, field-by-field, for this   |
|                                | document                                    |
+--------------------------------+---------------------------------------------+

CouchDB Document
================

+--------------------------------+---------------------------------------------+
| Field                          | Description                                 |
+================================+=============================================+
| _id (optional)                 | Document ID                                 |
+--------------------------------+---------------------------------------------+
| _rev (optional)                | Revision ID (when updating an existing      |
|                                | document)                                   |
+--------------------------------+---------------------------------------------+

CouchDB Error Status
====================

+--------------------------------+---------------------------------------------+
| Field                          | Description                                 |
+================================+=============================================+
| id                             | Document ID                                 |
+--------------------------------+---------------------------------------------+
| error                          | Error type                                  |
+--------------------------------+---------------------------------------------+
| reason                         | Error string with extended reason           |
+--------------------------------+---------------------------------------------+

.. _dbinfo_object:

CouchDB database information object
===================================

+--------------------------------+---------------------------------------------+
| Field                          | Description                                 |
+================================+=============================================+
| db_name                        | The name of the database.                   |
+--------------------------------+---------------------------------------------+
| committed_update_seq           | The number of committed update.             |
+--------------------------------+---------------------------------------------+
| doc_count                      | A count of the documents in the specified   |
|                                | database.                                   |
+--------------------------------+---------------------------------------------+
| doc_del_count                  | Number of deleted documents                 |
+--------------------------------+---------------------------------------------+
| compact_running                | Set to true if the database compaction      |
|                                | routine is operating on this database.      |
+--------------------------------+---------------------------------------------+
| disk_format_version            | The version of the physical format used for |
|                                | the data when it is stored on disk.         |
+--------------------------------+---------------------------------------------+
| disk_size                      | Size in bytes of the data as stored on the  |
|                                | disk. Views indexes are not included in the |
|                                | calculation.                                |
+--------------------------------+---------------------------------------------+
| instance_start_time            | Timestamp of when the database was created, |
|                                | expressed in milliseconds since the epoch.  |
+--------------------------------+---------------------------------------------+
| purge_seq                      | The number of purge operations on the       |
|                                | database.                                   |
+--------------------------------+---------------------------------------------+
| update_seq                     | The current number of updates to the        |
|                                | database.                                   |
+--------------------------------+---------------------------------------------+

Design Document
===============

+--------------------------------+---------------------------------------------+
| Field                          | Description                                 |
+================================+=============================================+
| _id                            | Design Document ID                          |
+--------------------------------+---------------------------------------------+
| _rev                           | Design Document Revision                    |
+--------------------------------+---------------------------------------------+
| views                          | View                                        |
+--------------------------------+---------------------------------------------+
|     viewname                   | View Definition                             |
+--------------------------------+---------------------------------------------+
|         map                    | Map Function for View                       |
+--------------------------------+---------------------------------------------+
|         reduce (optional)      | Reduce Function for View                    |
+--------------------------------+---------------------------------------------+

Design Document Information
===========================

+--------------------------------+---------------------------------------------+
| Field                          | Description                                 |
+================================+=============================================+
| name                           | Name/ID of Design Document                  |
+--------------------------------+---------------------------------------------+
| view_index                     | View Index                                  |
+--------------------------------+---------------------------------------------+
|     compact_running            | Indicates whether a compaction routine is   |
|                                | currently running on the view               |
+--------------------------------+---------------------------------------------+
|     disk_size                  | Size in bytes of the view as stored on disk |
+--------------------------------+---------------------------------------------+
|     language                   | Language for the defined views              |
+--------------------------------+---------------------------------------------+
|     purge_seq                  | The purge sequence that has been processed  |
+--------------------------------+---------------------------------------------+
|     signature                  | MD5 signature of the views for the design   |
|                                | document                                    |
+--------------------------------+---------------------------------------------+
|     update_seq                 | The update sequence of the corresponding    |
|                                | database that has been indexed              |
+--------------------------------+---------------------------------------------+
|     updater_running            | Indicates if the view is currently being    |
|                                | updated                                     |
+--------------------------------+---------------------------------------------+
|     waiting_clients            | Number of clients waiting on views from this|
|                                | design document                             |
+--------------------------------+---------------------------------------------+
|     waiting_commit             | Indicates if there are outstanding commits  |
|                                | to the underlying database that need to     |
|                                | processed                                   |
+--------------------------------+---------------------------------------------+

Design Document spatial index Information
=========================================

+--------------------------------+---------------------------------------------+
| Field                          | Description                                 |
+================================+=============================================+
| name                           | Name/ID of Design Document                  |
+--------------------------------+---------------------------------------------+
| spatial_index                  | View Index                                  |
+--------------------------------+---------------------------------------------+
|     compact_running            | Indicates whether a compaction routine is   |
|                                | currently running on the view               |
+--------------------------------+---------------------------------------------+
|     disk_size                  | Size in bytes of the view as stored on disk |
+--------------------------------+---------------------------------------------+
|     language                   | Language for the defined views              |
+--------------------------------+---------------------------------------------+
|     purge_seq                  | The purge sequence that has been processed  |
+--------------------------------+---------------------------------------------+
|     signature                  | MD5 signature of the views for the design   |
|                                | document                                    |
+--------------------------------+---------------------------------------------+
|     update_seq                 | The update sequence of the corresponding    |
|                                | database that has been indexed              |
+--------------------------------+---------------------------------------------+
|     updater_running            | Indicates if the view is currently being    |
|                                | updated                                     |
+--------------------------------+---------------------------------------------+
|     waiting_clients            | Number of clients waiting on views from this|
|                                | design document                             |
+--------------------------------+---------------------------------------------+
|     waiting_commit             | Indicates if there are outstanding commits  |
|                                | to the underlying database that need to     |
|                                | processed                                   |
+--------------------------------+---------------------------------------------+

Document with Attachments
=========================

+--------------------------------+---------------------------------------------+
| Field                          | Description                                 |
+================================+=============================================+
| _id (optional)                 | Document ID                                 |
+--------------------------------+---------------------------------------------+
| _rev (optional)                | Revision ID (when updating an existing      |
|                                | document)                                   |
+--------------------------------+---------------------------------------------+
| _attachments (optional)        | Document Attachment                         |
+--------------------------------+---------------------------------------------+
|     filename                   | Attachment information                      |
+--------------------------------+---------------------------------------------+
|         content_type           | MIME Content type string                    |
+--------------------------------+---------------------------------------------+
|         data                   | File attachment content, Base64 encoded     |
+--------------------------------+---------------------------------------------+

List of Active Tasks
====================

+--------------------------------+---------------------------------------------+
| Field                          | Description                                 |
+================================+=============================================+
| tasks [array]                  | Active Task                                 |
+--------------------------------+---------------------------------------------+
|     pid                        | Process ID                                  |
+--------------------------------+---------------------------------------------+
|     status                     | Task status message                         |
+--------------------------------+---------------------------------------------+
|     task                       | Task name                                   |
+--------------------------------+---------------------------------------------+
|     type                       | Operation Type                              |
+--------------------------------+---------------------------------------------+

Replication Settings
====================

+--------------------------------+---------------------------------------------+
| Field                          | Description                                 |
+================================+=============================================+
| source                         | Source database name or URL                 |
+--------------------------------+---------------------------------------------+
| target                         | Target database name or URL                 |
+--------------------------------+---------------------------------------------+
| create_target (optional)       | Creates the target database                 |
+--------------------------------+---------------------------------------------+
| continuous (optional)          | Configure the replication to be continuous  |
+--------------------------------+---------------------------------------------+
| cancel (optional)              | Cancels the replication                     |
+--------------------------------+---------------------------------------------+
| doc_ids (optional)             | Array of document IDs to be synchronized    |
+--------------------------------+---------------------------------------------+
| proxy (optional)               | Address of a proxy server through which     |
|                                | replication should occur                    |
+--------------------------------+---------------------------------------------+

Replication Status
==================

+--------------------------------+---------------------------------------------+
| Field                          | Description                                 |
+================================+=============================================+
| ok                             | Replication status                          |
+--------------------------------+---------------------------------------------+
| session_id                     | Unique session ID                           |
+--------------------------------+---------------------------------------------+
| source_last_seq                | Last sequence number read from source       |
|                                | database                                    |
+--------------------------------+---------------------------------------------+
| history [array]                | Replication History                         |
+--------------------------------+---------------------------------------------+
|     session_id                 | Session ID for this replication operation   |
+--------------------------------+---------------------------------------------+
|     recorded_seq               | Last recorded sequence number               |
+--------------------------------+---------------------------------------------+
|     docs_read                  | Number of documents read                    |
+--------------------------------+---------------------------------------------+
|     docs_written               | Number of documents written to target       |
+--------------------------------+---------------------------------------------+
|     doc_write_failures         | Number of document write failures           |
+--------------------------------+---------------------------------------------+
|     start_time                 | Date/Time replication operation started     |
+--------------------------------+---------------------------------------------+
|     start_last_seq             | First sequence number in changes stream     |
+--------------------------------+---------------------------------------------+
|     end_time                   | Date/Time replication operation completed   |
+--------------------------------+---------------------------------------------+
|     end_last_seq               | Last sequence number in changes stream      |
+--------------------------------+---------------------------------------------+
|     missing_checked            | Number of missing documents checked         |
+--------------------------------+---------------------------------------------+
|     missing_found              | Number of missing documents found           |
+--------------------------------+---------------------------------------------+

.. _request_object:

Request object
==============

+--------------------------------+---------------------------------------------+
| Field                          | Description                                 |
+================================+=============================================+
| body                           | Request body data as `string`.              |
|                                | If request method is `GET` method contains  |
|                                | this field contains ``"undefined"`` value,  |
|                                | while if `DELETE` or `HEAD` value is ``""`` |
|                                | (empty string)                              |
+--------------------------------+---------------------------------------------+
| cookie                         | Cookies `object`.                           |
+--------------------------------+---------------------------------------------+
| form                           | Form data `object`.                         |
|                                | Contains decoded body as key-value pairs if |
|                                | `Content-Type` header was                   |
|                                | ``application/x-www-form-urlencoded``.      |
+--------------------------------+---------------------------------------------+
| headers                        | Request headers `object`.                   |
+--------------------------------+---------------------------------------------+
| id                             | Requested document id `string` if it was    |
|                                | specified or ``null`` otherwise.            |
+--------------------------------+---------------------------------------------+
| info                           | :ref:`Database information <dbinfo_object>` |
+--------------------------------+---------------------------------------------+
| method                         | Request method as `string` or `array`.      |
|                                | String value is method is one of: `HEAD`,   |
|                                | `GET`, `POST`, `PUT`, `DELETE`, `OPTIONS`,  |
|                                | and `TRACE`, otherwise it will be           |
|                                | represented as array of char codes.         |
+--------------------------------+---------------------------------------------+
| path                           | List of requested path sections.            |
+--------------------------------+---------------------------------------------+
| peer                           | Request source IP address.                  |
+--------------------------------+---------------------------------------------+
| query                          | URL query parameters `object`.              |
|                                | Note that multiple keys not supported and   |
|                                | last key value suppress others.             |
+--------------------------------+---------------------------------------------+
| requested_path                 | List of actual requested path section.      |
+--------------------------------+---------------------------------------------+
| raw_path                       | Raw requested path `string`.                |
+--------------------------------+---------------------------------------------+
| secObj                         | :ref:`security_object`.                     |
+--------------------------------+---------------------------------------------+
| userCtx                        | :ref:`userctx_object`.                      |
+--------------------------------+---------------------------------------------+
| uuid                           | Generated UUID by specified algorithm in    |
|                                | config file.                                |
+--------------------------------+---------------------------------------------+

.. code-block:: javascript

  {
      "body": "undefined",
      "cookie": {
          "AuthSession": "cm9vdDo1MDZBRjQzRjrfcuikzPRfAn-EA37FmjyfM8G8Lw",
          "m": "3234"
      },
      "form": {},
      "headers": {
          "Accept": "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
          "Accept-Charset": "ISO-8859-1,utf-8;q=0.7,*;q=0.3",
          "Accept-Encoding": "gzip,deflate,sdch",
          "Accept-Language": "en-US,en;q=0.8",
          "Connection": "keep-alive",
          "Cookie": "m=3234:t|3247:t|6493:t|6967:t|34e2:|18c3:t|2c69:t|5acb:t|ca3:t|c01:t|5e55:t|77cb:t|2a03:t|1d98:t|47ba:t|64b8:t|4a01:t; AuthSession=cm9vdDo1MDZBRjQzRjrfcuikzPRfAn-EA37FmjyfM8G8Lw",
          "Host": "127.0.0.1:5984",
          "User-Agent": "Mozilla/5.0 (Windows NT 5.2) AppleWebKit/535.7 (KHTML, like Gecko) Chrome/16.0.912.75 Safari/535.7"
      },
      "id": "foo",
      "info": {
          "committed_update_seq": 2701412,
          "compact_running": false,
          "data_size": 7580843252,
          "db_name": "mailbox",
          "disk_format_version": 6,
          "disk_size": 14325313673,
          "doc_count": 2262757,
          "doc_del_count": 560,
          "instance_start_time": "1347601025628957",
          "purge_seq": 0,
          "update_seq": 2701412
      },
      "method": "GET",
      "path": [
          "mailbox",
          "_design",
          "request",
          "_show",
          "dump",
          "foo"
      ],
      "peer": "127.0.0.1",
      "query": {},
      "raw_path": "/mailbox/_design/request/_show/dump/foo",
      "requested_path": [
          "mailbox",
          "_design",
          "request",
          "_show",
          "dump",
          "foo"
      ],
      "secObj": {
          "admins": {
              "names": [
                  "Bob"
              ],
              "roles": []
          },
          "members": {
              "names": [
                  "Mike",
                  "Alice"
              ],
              "roles": []
          }
      },
      "userCtx": {
          "db": "mailbox",
          "name": "Mike",
          "roles": [
              "user"
          ]
      },
      "uuid": "3184f9d1ea934e1f81a24c71bde5c168"
  }


.. _response_object:

Response object
===============

+--------------------------------+---------------------------------------------+
| Field                          | Description                                 |
+================================+=============================================+
| code                           | HTTP status code `number`.                  |
+--------------------------------+---------------------------------------------+
| json                           | JSON encodable `object`.                    |
|                                | Implicitly sets `Content-Type` header as    |
|                                | ``application/json``.                       |
+--------------------------------+---------------------------------------------+
| body                           | Raw response text `string`.                 |
|                                | Implicitly sets `Content-Type` header as    |
|                                | ``text/html; charset=utf-8``.               |
+--------------------------------+---------------------------------------------+
| base64                         | Base64 encoded `string`.                    |
|                                | Implicitly sets `Content-Type` header as    |
|                                | ``application/binary``.                     |
+--------------------------------+---------------------------------------------+
| headers                        | Response headers `object`.                  |
|                                | `Content-Type` header from this object      |
|                                | overrides any implicitly assigned one.      |
+--------------------------------+---------------------------------------------+
| stop                           | `boolean` signal to stop iteration over     |
|                                | view result rows (for list functions only)  |
+--------------------------------+---------------------------------------------+

.. warning::
   ``body``, ``base64`` and ``json`` object keys are overlaps each other and
   the last wins. Since most realizations of key-value objects doesn't preserve
   key order mixing them may create confusing situation. Try to use only one of
   them.

.. note::
   Any custom property makes CouchDB raise internal exception.
   Also `Response object` could be a simple string value which would be
   implicitly wrapped into ``{"body": ...}`` object.


Returned CouchDB Document with Detailed Revision Info
=====================================================

+--------------------------------+---------------------------------------------+
| Field                          | Description                                 |
+================================+=============================================+
| _id (optional)                 | Document ID                                 |
+--------------------------------+---------------------------------------------+
| _rev (optional)                | Revision ID (when updating an existing      |
|                                | document)                                   |
+--------------------------------+---------------------------------------------+
| _revs_info [array]             | CouchDB Document Extended Revision Info     |
+--------------------------------+---------------------------------------------+
|         rev                    | Full revision string                        |
+--------------------------------+---------------------------------------------+
|         status                 | Status of the revision                      |
+--------------------------------+---------------------------------------------+

Returned CouchDB Document with Revision Info
============================================

+--------------------------------+---------------------------------------------+
| Field                          | Description                                 |
+================================+=============================================+
| _id (optional)                 | Document ID                                 |
+--------------------------------+---------------------------------------------+
| _rev (optional)                | Revision ID (when updating an existing      |
|                                | document)                                   |
+--------------------------------+---------------------------------------------+
| _revisions                     | CouchDB Document Revisions                  |
+--------------------------------+---------------------------------------------+
|     ids [array]                | Array of valid revision IDs, in reverse     |
|                                | order (latest first)                        |
+--------------------------------+---------------------------------------------+
|     start                      | Prefix number for the latest revision       |
+--------------------------------+---------------------------------------------+

Returned Document with Attachments
==================================

+--------------------------------+---------------------------------------------+
| Field                          | Description                                 |
+================================+=============================================+
| _id (optional)                 | Document ID                                 |
+--------------------------------+---------------------------------------------+
| _rev (optional)                | Revision ID (when updating an existing      |
|                                | document)                                   |
+--------------------------------+---------------------------------------------+
| _attachments (optional)        | Document Attachment                         |
+--------------------------------+---------------------------------------------+
|     filename                   | Attachment                                  |
+--------------------------------+---------------------------------------------+
|         stub                   | Indicates whether the attachment is a stub  |
+--------------------------------+---------------------------------------------+
|         content_type           | MIME Content type string                    |
+--------------------------------+---------------------------------------------+
|         length                 | Length (bytes) of the attachment data       |
+--------------------------------+---------------------------------------------+
|         revpos                 | Revision where this attachment exists       |
+--------------------------------+---------------------------------------------+

.. _security_object:

Security Object
===============

+--------------------------------+---------------------------------------------+
| Field                          | Description                                 |
+================================+=============================================+
| admins                         | Roles/Users with admin privileges           |
+--------------------------------+---------------------------------------------+
|         roles [array]          | List of roles with parent privilege         |
+--------------------------------+---------------------------------------------+
|         users [array]          | List of users with parent privilege         |
+--------------------------------+---------------------------------------------+
| readers                        | Roles/Users with reader privileges          |
+--------------------------------+---------------------------------------------+
|         roles [array]          | List of roles with parent privilege         |
+--------------------------------+---------------------------------------------+
|         users [array]          | List of users with parent privilege         |
+--------------------------------+---------------------------------------------+

.. code-block:: javascript

  {
      "admins": {
          "names": [
              "Bob"
          ],
          "roles": []
      },
      "members": {
          "names": [
              "Mike",
              "Alice"
          ],
          "roles": []
      }
    }


.. _userctx_object:

User Context Object
===================

+--------------------------------+---------------------------------------------+
| Field                          | Description                                 |
+================================+=============================================+
| db                             | Database name in context of provided        |
|                                | operation.                                  |
+--------------------------------+---------------------------------------------+
| name                           | User name.                                  |
+--------------------------------+---------------------------------------------+
| roles                          | List of user roles.                         |
+--------------------------------+---------------------------------------------+

.. code-block:: javascript

    {
        "db": "mailbox",
        "name": null,
        "roles": [
            "_admin"
        ]
    }


.. _view_head_info_object:

View Head Information
=====================

+--------------------------------+---------------------------------------------+
| Field                          | Description                                 |
+================================+=============================================+
| total_rows                     | Number of documents in the view             |
+--------------------------------+---------------------------------------------+
| offset                         | Offset where the document list started      |
+--------------------------------+---------------------------------------------+

.. code-block:: javascript

    {
        "total_rows": 42,
        "offset": 3
    }

Number Handling
===============

Any numbers defined in JSON that contain a decimal point or exponent
will be passed through the Erlang VM's idea of the "double" data type.
Any numbers that are used in views will pass through the views idea of
a number (the common JavaScript case means even integers pass through
a double due to JavaScript's definition of a number).

Consider this document that we write to CouchDB:

.. code-block:: javascript

    {
      "_id":"30b3b38cdbd9e3a587de9b8122000cff",
      "number": 1.1
    }

Now let’s read that document back from CouchDB:

.. code-block:: javascript

    {
      "_id":"30b3b38cdbd9e3a587de9b8122000cff",
      "_rev":"1-f065cee7c3fd93aa50f6c97acde93030",
      "number":1.1000000000000000888
    }


What happens is CouchDB is changing the textual representation of the
result of decoding what it was given into some numerical format. In most
cases this is an `IEEE 754`_ double precision floating point number which
is exactly what almost all other languages use as well.

.. _IEEE 754: https://en.wikipedia.org/wiki/IEEE_754-2008

What CouchDB does a bit differently than other languages is that it
does not attempt to pretty print the resulting output to use the
shortest number of characters. For instance, this is why we have this
relationship:

.. code-block:: erlang

    ejson:encode(ejson:decode(<<"1.1">>)).
    <<"1.1000000000000000888">>

What can be confusing here is that internally those two formats
decode into the same IEEE-754 representation. And more importantly, it
will decode into a fairly close representation when passed through all
major parsers that I know about.

While we've only been discussing cases where the textual
representation changes, another important case is when an input value
is contains more precision than can actually represented in a double.
(You could argue that this case is actually "losing" data if you don't
accept that numbers are stored in doubles).

Here's a log for a couple of the more common JSON libraries I happen
to have on my machine:

Spidermonkey::

    $ js -h 2>&1 | head -n 1
    JavaScript-C 1.8.5 2011-03-31
    $ js
    js> JSON.stringify(JSON.parse("1.01234567890123456789012345678901234567890"))
    "1.0123456789012346"
    js> var f = JSON.stringify(JSON.parse("1.01234567890123456789012345678901234567890"))
    js> JSON.stringify(JSON.parse(f))
    "1.0123456789012346"

Node::

    $ node -v
    v0.6.15
    $ node
    JSON.stringify(JSON.parse("1.01234567890123456789012345678901234567890"))
    '1.0123456789012346'
    var f = JSON.stringify(JSON.parse("1.01234567890123456789012345678901234567890"))
    undefined
    JSON.stringify(JSON.parse(f))
    '1.0123456789012346'

Python::

    $ python
    Python 2.7.2 (default, Jun 20 2012, 16:23:33)
    [GCC 4.2.1 Compatible Apple Clang 4.0 (tags/Apple/clang-418.0.60)] on darwin
    Type "help", "copyright", "credits" or "license" for more information.
    import json
    json.dumps(json.loads("1.01234567890123456789012345678901234567890"))
    '1.0123456789012346'
    f = json.dumps(json.loads("1.01234567890123456789012345678901234567890"))
    json.dumps(json.loads(f))
    '1.0123456789012346'

Ruby::

    $ irb --version
    irb 0.9.5(05/04/13)
    require 'JSON'
    => true
    JSON.dump(JSON.load("[1.01234567890123456789012345678901234567890]"))
    => "[1.01234567890123]"
    f = JSON.dump(JSON.load("[1.01234567890123456789012345678901234567890]"))
    => "[1.01234567890123]"
    JSON.dump(JSON.load(f))
    => "[1.01234567890123]"


.. note:: A small aside on Ruby, it requires a top level object or array, so I just
         wrapped the value. Should be obvious it doesn't affect the result of
         parsing the number though.


Ejson (CouchDB's current parser) at CouchDB sha 168a663b::

    $ ./utils/run -i
    Erlang R14B04 (erts-5.8.5) [source] [64-bit] [smp:2:2] [rq:2]
    [async-threads:4] [hipe] [kernel-poll:true]

    Eshell V5.8.5  (abort with ^G)
    1> ejson:encode(ejson:decode(<<"1.01234567890123456789012345678901234567890">>)).
    <<"1.0123456789012346135">>
    2> F = ejson:encode(ejson:decode(<<"1.01234567890123456789012345678901234567890">>)).
    <<"1.0123456789012346135">>
    3> ejson:encode(ejson:decode(F)).
    <<"1.0123456789012346135">>


As you can see they all pretty much behave the same except for Ruby
actually does appear to be losing some precision over the other
libraries.

The astute observer will notice that ejson (the CouchDB JSON library)
reported an extra three digits. While its tempting to think that this
is due to some internal difference, its just a more specific case of
the 1.1 input as described above.

The important point to realize here is that a double can only hold a
finite number of values. What we're doing here is generating a string
that when passed through the "standard" floating point parsing
algorithms (ie, strtod) will result in the same bit pattern in memory
as we started with. Or, slightly different, the bytes in a JSON
serialized number are chosen such that they refer to a single specific
value that a double can represent.

The important point to understand is that we're mapping from one
infinite set onto a finite set. An easy way to see this is by
reflecting on this::

    1.0 == 1.00 == 1.000 = 1.(infinite zeroes)

Obviously a computer can't hold infinite bytes so we have to
decimate our infinitely sized set to a finite set that can be
represented concisely.

The game that other JSON libraries are playing is merely:

"How few characters do I have to use to select this specific value for a double"

And that game has lots and lots of subtle details that are difficult
to duplicate in C without a significant amount of effort (it took
Python over a year to get it sorted with their fancy build systems
that automatically run on a number of different architectures).

Hopefully we've shown that CouchDB is not doing anything "funky" by
changing input. Its behaving the same as any other common JSON library
does, its just not pretty printing its output.

On the other hand, if you actually are in a position where an IEEE-754
double is not a satisfactory datatype for your numbers, then the
answer as has been stated is to not pass your numbers through this
representation. In JSON this is accomplished by encoding them as a
string or by using integer types (although integer types can still
bite you if you use a platform that has a different integer
representation than normal, ie, JavaScript).

Also, if anyone is really interested in changing this behavior, I'm
all ears for contributions to `jiffy`_ (which is theoretically going to
replace ejson when I get around to updating the build system). The
places I've looked for inspiration are TCL and Python. If you know a
decent implementation of this float printing algorithm give me a
holler.

.. _jiffy: https://github.com/davisp/jiffy
