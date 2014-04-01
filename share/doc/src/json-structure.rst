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
| committed_update_seq           | The number of committed updates.            |
+--------------------------------+---------------------------------------------+
| doc_count                      | The number of documents in the database.    |
+--------------------------------+---------------------------------------------+
| doc_del_count                  | The number of deleted documents.            |
+--------------------------------+---------------------------------------------+
| compact_running                | Set to true if the database compaction      |
|                                | routine is operating on this database.      |
+--------------------------------+---------------------------------------------+
| disk_format_version            | The version of the physical format used for |
|                                | the data when it is stored on hard disk.    |
+--------------------------------+---------------------------------------------+
| disk_size                      | Size in bytes of the data as stored on disk.|
|                                | View indexes are not included in the        |
|                                | calculation.                                |
+--------------------------------+---------------------------------------------+
| instance_start_time            | Timestamp indicating when the database was  |
|                                | opened, expressed in microseconds since the |
|                                | epoch.                                      |
+--------------------------------+---------------------------------------------+
| purge_seq                      | The number of purge operations on the       |
|                                | database.                                   |
+--------------------------------+---------------------------------------------+
| update_seq                     | The current number of updates made in the   |
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
| tasks [array]                  | Active Tasks                                |
+--------------------------------+---------------------------------------------+
|     pid                        | Process ID                                  |
+--------------------------------+---------------------------------------------+
|     status                     | Task status message                         |
+--------------------------------+---------------------------------------------+
|     task                       | Task name                                   |
+--------------------------------+---------------------------------------------+
|     type                       | Operation Type                              |
+--------------------------------+---------------------------------------------+

.. _replication-settings:

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
| since_seq (optional)           | Sequence from which the replication should  |
|                                | start                                       |
+--------------------------------+---------------------------------------------+
| filter (optional)              | name of the filter function in the form of  |
|                                | ``ddoc/myfilter``                           |
+--------------------------------+---------------------------------------------+
| query_params (optional)        | Query parameter that are passed to the      |
|                                | filter function; the value should be a      |
|                                | document containing parameters as members   |
+--------------------------------+---------------------------------------------+
| use_checkpoints (optional)     | Whether to use replication checkpoints      |
|                                | or not                                      |
+--------------------------------+---------------------------------------------+
| checkpoint_interval (optional) | Specifies the checkpoint interval in ms.    |
+--------------------------------+---------------------------------------------+

.. _replication-status:

Replication Status
==================

+--------------------------------+---------------------------------------------+
| Field                          | Description                                 |
+================================+=============================================+
| ok                             | Replication status                          |
+--------------------------------+---------------------------------------------+
| session_id                     | Unique session ID                           |
+--------------------------------+---------------------------------------------+
| source_last_seq                | Last sequence number read from the source   |
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
|                                | If the request method is `GET` this field   |
|                                | contains the value ``"undefined"``. If the  |
|                                | method is `DELETE` or `HEAD` the value is   |
|                                | ``""`` (empty string).                      |
+--------------------------------+---------------------------------------------+
| cookie                         | Cookies `object`.                           |
+--------------------------------+---------------------------------------------+
| form                           | Form data `object`.                         |
|                                | Contains the decoded body as key-value      |
|                                | pairs if the `Content-Type` header was      |
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
|                                | String value is a method as one of: `HEAD`, |
|                                | `GET`, `POST`, `PUT`, `DELETE`, `OPTIONS`,  |
|                                | and `TRACE`. Otherwise it will be           |
|                                | represented as an array of char codes.      |
+--------------------------------+---------------------------------------------+
| path                           | List of requested path sections.            |
+--------------------------------+---------------------------------------------+
| peer                           | Request source IP address.                  |
+--------------------------------+---------------------------------------------+
| query                          | URL query parameters `object`.              |
|                                | Note that multiple keys are not supported   |
|                                | and the last key value suppresses others.   |
+--------------------------------+---------------------------------------------+
| requested_path                 | List of actual requested path section.      |
+--------------------------------+---------------------------------------------+
| raw_path                       | Raw requested path `string`.                |
+--------------------------------+---------------------------------------------+
| secObj                         | :ref:`security_object`.                     |
+--------------------------------+---------------------------------------------+
| userCtx                        | :ref:`userctx_object`.                      |
+--------------------------------+---------------------------------------------+
| uuid                           | Generated UUID by a specified algorithm in  |
|                                | the config file.                            |
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
   The ``body``, ``base64`` and ``json`` object keys are overlapping each other
   where the last one wins. Since most realizations of key-value objects do
   not preserve the key order or if they are mixed, confusing situations can
   occure. Try to use only one of them.

.. note::
   Any custom property makes CouchDB raise an internal exception.
   Furthermore, the `Response object` could be a simple string value which would
   be implicitly wrapped into a ``{"body": ...}`` object.


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
| _revs_info [array]             | CouchDB document extended revision info     |
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
| _revisions                     | CouchDB document revisions                  |
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
| _attachments (optional)        | Document attachment                         |
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
| db                             | Database name in the context of the         |
|                                | provided operation.                         |
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
