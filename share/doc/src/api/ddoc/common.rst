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


.. _api/ddoc:

``/db/_design/design-doc``
==========================

.. http:head:: /{db}/_design/{ddocname}

  Returns the HTTP Headers containing a minimal amount of information
  about the specified design document.

  .. seealso::

    :http:head:`/{db}/{docid}`


.. http:get:: /{db}/_design/{ddocname}

  Returns design document with the specified design document` from the specified
  database. Unless you request a specific revision, the latest revision of the
  document will always be returned.

  .. seealso::

    :http:get:`/{db}/{docid}`


.. http:put:: /{db}/_design/{ddocname}

  The :http:method:`PUT` method creates a new named design document, or creates
  a new revision of the existing design document.

  The design documents have some agreement upon their fields and structure.
  Currently it is the next:

  * **language** (*string*): Defines :ref:`Query Server <query-server>`
    :ref:`key <config/query_servers>` to process design document functions
  * **options** (*object*): View's default options
  * **filters** (*object*): :ref:`Filter functions <filterfun>` definition
  * **lists** (*object*): :ref:`List functions <listfun>` definition
  * **rewrites** (*array*): Rewrite rules definition
  * **shows** (*object*): :ref:`Show functions <showfun>` definition
  * **updates** (*object*): :ref:`Update functions <updatefun>` definition
  * **validate_doc_update** (*string*): :ref:`Validate document update <vdufun>`
    function source
  * **views** (*object*): :ref:`View functions <viewfun>` definition.

  Note, that for ``filters``, ``lists``, ``shows`` and ``updates`` fields
  objects are mapping of function name to string function source code.
  For ``views`` mapping is the same except that values are objects with ``map``
  and ``reduce`` (optional) keys which also contains functions source code.

  .. seealso::

    :http:put:`/{db}/{docid}`


.. http:delete:: /{db}/_design/{ddocname}

  Deletes the specified document from the database. You must supply the
  current (latest) revision, either by using the ``rev`` parameter to
  specify the revision.

  .. seealso::

    :http:delete:`/{db}/{docid}`

.. http:copy:: /{db}/_design/{ddocname}

  The :http:method:`COPY` (which is non-standard HTTP) copies an existing
  design document to a new or existing one.

  .. note::
     Copying a design document does automatically reconstruct the view
     indexes. These will be recreated, as with other views, the first
     time the new view is accessed.

  .. seealso::

    :http:copy:`/{db}/{docid}`


.. _api/ddoc/attachment:

``/db/_design/design-doc/attachment``
=====================================

.. http:head:: /{db}/_design/{ddocname}/{attname}

  Returns the HTTP headers containing a minimal amount of information
  about the specified attachment.

  .. seealso::

    :http:head:`/{db}/{docid}/{attname}`

.. http:get:: /{db}/_design/{ddocname}/{attname}

  Returns the file attachment associated with the design document.
  The raw data of the associated attachment is returned (just as if you were
  accessing a static file.

  .. seealso::

    :http:get:`/{db}/{docid}/{attname}`

.. http:put:: /{db}/_design/{ddocname}/{attname}

  Uploads the supplied content as an attachment to the specified design
  document. The attachment name provided must be a URL encoded string.

  .. seealso::

    :http:put:`/{db}/{docid}/{attname}`

.. http:delete:: /{db}/_design/{ddocname}/{attname}

  Deletes the attachment of the specified design document.

  .. seealso::

    :http:delete:`/{db}/{docid}/{attname}`


.. _api/ddoc/info:

``/db/_design/design-doc/_info``
================================

.. http:get:: /{db}/_design/{ddocname}/_info

  Obtains information about the specified design document, including the index,
  index size and current status of the design document and associated
  index information.

  :param db: Database name
  :param ddocname: Design document name
  :<header Accept: - :mimetype:`application/json`
                   - :mimetype:`text/plain`
  :>header Content-Type: - :mimetype:`application/json`
                         - :mimetype:`text/plain; charset=utf-8`
  :>json string name: Design document name
  :>json object view_index: :ref:`api/ddoc/view_index_info`
  :code 200: Request completed successfully

  **Request**:

  .. code-block:: http

    GET /recipes/_design/recipe/_info HTTP/1.1
    Accept: application/json
    Host: localhost:5984

  **Response**:

  .. code-block:: http

    HTTP/1.1 200 OK
    Cache-Control: must-revalidate
    Content-Length: 263
    Content-Type: application/json
    Date: Sat, 17 Aug 2013 12:54:17 GMT
    Server: CouchDB/1.4.0 (Erlang OTP/R16B)

    {
        "name": "recipe",
        "view_index": {
            "compact_running": false,
            "data_size": 926691,
            "disk_size": 1982704,
            "language": "python",
            "purge_seq": 0,
            "signature": "a59a1bb13fdf8a8a584bc477919c97ac",
            "update_seq": 12397,
            "updater_running": false,
            "waiting_clients": 0,
            "waiting_commit": false
        }
    }


.. _api/ddoc/view_index_info:

View Index Information
----------------------

The response from :http:get:`/{db}/_design/{ddocname}/_info` contains
``view_index`` (*object*) field with the next structure:

* **compact_running** (*boolean*):  Indicates whether a compaction routine
  is currently running on the view
* **data_size** (*number*): Actual size in bytes of the view
* **disk_size** (*number*): Size in bytes of the view as stored on disk
* **language** (*string*): Language for the defined views
* **purge_seq** (*number*): The purge sequence that has been processed
* **signature** (*string*): MD5 signature of the views for the design document
* **update_seq** (*number*): The update sequence of the corresponding database
  that has been indexed
* **updater_running** (*boolean*): Indicates if the view is currently
  being updated
* **waiting_clients** (*number*): Number of clients waiting on views from
  this design document
* **waiting_commit** (*boolean*): Indicates if there are outstanding commits
  to the underlying database that need to processed
