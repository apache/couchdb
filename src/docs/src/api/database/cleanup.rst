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

.. _api/db/cleanup:

=========================
``/{db}/_search_cleanup``
=========================

.. http:post:: /{db}/_search_cleanup
    :synopsis: Deletes unreachable search indexes for the database

    Requests deletion of unreachable search (Clouseau) indexes of the specified
    database. The signatures for all current design documents is retrieved and
    any index found on disk with a signature that is not in that list is deleted.

    :param db: Database name
    :<header Accept: - :mimetype:`application/json`
                     - :mimetype:`text/plain`
    :<header Content-Type: :mimetype:`application/json`
    :>header Content-Type: - :mimetype:`application/json`
                           - :mimetype:`text/plain; charset=utf-8`
    :>json boolean ok: Operation status
    :code 202: Cleanup request has been accepted
    :code 400: Invalid database name
    :code 401: CouchDB Server Administrator privileges required

    **Request**:

    .. code-block:: http

        POST /db/_search_cleanup HTTP/1.1
        Accept: application/json
        Content-Type: application/json
        Host: localhost:5984

    **Response**:

    .. code-block:: http

        HTTP/1.1 202 Accepted
        Cache-Control: must-revalidate
        Content-Length: 12
        Content-Type: application/json
        Server: CouchDB (Erlang/OTP)

        {
            "ok": true
        }

==========================
``/{db}/_nouveau_cleanup``
==========================

.. http:post:: /{db}/_nouveau_cleanup
    :synopsis: Deletes unreachable nouveau search indexes for the database

    Requests deletion of unreachable search (Nouveau) indexes of the specified
    database. The signatures for all current design documents is retrieved and
    any index found on disk with a signature that is not in that list is deleted.

    :param db: Database name
    :<header Accept: - :mimetype:`application/json`
                     - :mimetype:`text/plain`
    :<header Content-Type: :mimetype:`application/json`
    :>header Content-Type: - :mimetype:`application/json`
                           - :mimetype:`text/plain; charset=utf-8`
    :>json boolean ok: Operation status
    :code 202: Cleanup request has been accepted
    :code 400: Invalid database name
    :code 401: CouchDB Server Administrator privileges required

    **Request**:

    .. code-block:: http

        POST /db/_nouveau_cleanup HTTP/1.1
        Accept: application/json
        Content-Type: application/json
        Host: localhost:5984

    **Response**:

    .. code-block:: http

        HTTP/1.1 202 Accepted
        Cache-Control: must-revalidate
        Content-Length: 12
        Content-Type: application/json
        Server: CouchDB (Erlang/OTP)

        {
            "ok": true
        }
