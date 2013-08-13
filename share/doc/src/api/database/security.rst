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


.. _api/db/security:

``/db/_security``
=================

.. http:get:: /{db}/_security

  Returns the current security object from the specified database.

  The security object consists of two compulsory elements, ``admins``
  and ``members``, which are used to specify the list of users and/or roles
  that have admin and members rights to the database respectively:

  - ``members``: they can read all types of documents from the DB, and they can
    write (and edit) documents to the DB except for design documents.

  - ``admins``: they have all the privileges of ``members`` plus the privileges:
    write (and edit) design documents, add/remove database admins and members,
    set the :ref:`database revisions limit <api/db/revs_limit>` and execute
    :ref:`temporary views <api/db/temp_view>` against the database.
    They can not create a database and neither delete a database.

  Both ``members`` and ``admins`` objects are contains two array-typed fields:

  - ``users``: List of CouchDB user names
  - ``roles``: List of users roles

  Any other additional fields in the security object are optional.
  The entire security object is made available to validation and other
  internal functions so that the database can control and limit functionality.

  If both the names and roles fields of either the admins or members properties
  are empty arrays, it means the database has no admins or members.

  Having no admins, only server admins (with the reserved ``_admin`` role)
  are able to update design document and make other admin level changes.

  Having no members, any user can write regular documents (any non-design
  document) and read documents from the database.

  If there are any member names or roles defined for a database, then only
  authenticated users having a matching name or role are allowed to
  read documents from the database (or do a :http:get:`/{db}` call).

  .. note::
     If the security object for a database has never been set, then the
     value returned will be empty.

     Also note, that security objects are not regular versioned documents
     (that is, they are not under MVCC rules). This is a design choice to
     speedup authorization checks (avoids traversing a database`s documents
     B-Tree).

  :param db: Database name
  :<header Accept: - :mimetype:`application/json`
                   - :mimetype:`text/plain`
  :>header Content-Type: - :mimetype:`application/json`
                         - :mimetype:`text/plain; charset=utf-8`
  :>json object admins: Object with two fields as ``names`` and ``roles``.
    See description above for more info.
  :>json object members: Object with two fields as ``names`` and ``roles``.
    See description above for more info.
  :code 200: Request completed successfully

  **Request**:

  .. code-block:: http

    GET /db/_security HTTP/1.1
    Accept: application/json
    Host: localhost:5984

  **Response**:

  .. code-block:: http

    HTTP/1.1 200 OK
    Cache-Control: must-revalidate
    Content-Length: 109
    Content-Type: application/json
    Date: Mon, 12 Aug 2013 19:05:29 GMT
    Server: CouchDB/1.4.0 (Erlang OTP/R16B)

    {
        "admins": {
            "names": [
                "superuser"
            ],
            "roles": [
                "admins"
            ]
        },
        "members": {
            "names": [
                "user1",
                "user2"
            ],
            "roles": [
                "developers"
            ]
        }
    }


.. http:put:: /{db}/_security

  Sets the security object for the given database.

  :param db: Database name
  :<header Accept: - :mimetype:`application/json`
                   - :mimetype:`text/plain`
  :>header Content-Type: - :mimetype:`application/json`
                         - :mimetype:`text/plain; charset=utf-8`
  :<json object admins: Object with two fields as ``names`` and ``roles``.
    :ref:`See description above for more info <api/db/security>`.
  :<json object members: Object with two fields as ``names`` and ``roles``.
    :ref:`See description above for more info <api/db/security>`.
  :>json boolean ok: Operation status
  :code 200: Request completed successfully
  :code 401: Administrator's privileges required

  **Request**:

  .. code-block:: http

    PUT /db/_security HTTP/1.1
    Accept: application/json
    Content-Length: 121
    Content-Type: application/json
    Host: localhost:5984

    {
        "admins": {
            "names": [
                "superuser"
            ],
            "roles": [
                "admins"
            ]
        },
        "members": {
            "names": [
                "user1",
                "user2"
            ],
            "roles": [
                "developers"
            ]
        }
    }

  **Response**:

  .. code-block:: http

    HTTP/1.1 200 OK
    Cache-Control: must-revalidate
    Content-Length: 12
    Content-Type: application/json
    Date: Tue, 13 Aug 2013 11:26:28 GMT
    Server: CouchDB/1.4.0 (Erlang OTP/R16B)

    {
        "ok": true
    }
