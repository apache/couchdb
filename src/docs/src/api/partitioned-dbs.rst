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

.. _api/partioned-dbs:

=====================
Partitioned Databases
=====================

Partitioned databases allow for data colocation in a cluster, which provides
significant performance improvements for queries constrained to a single
partition.

See the guide for
:ref:`getting started with partitioned databases <partitioned-dbs>`

``/db/_partition/partition``
============================

.. http:get:: /{db}/_partition/{partition}
    :synopsis: Returns document and size info for the given partition

    This endpoint returns information describing the provided partition.
    It includes document and deleted document counts along with external
    and active data sizes.

    :code 200: Request completed successfully

    **Request**:

    .. code-block:: http

        GET /db/_partition/sensor-260 HTTP/1.1
        Accept: application/json
        Host: localhost:5984

    **Response**:

    .. code-block:: http

        HTTP/1.1 200 OK
        Cache-Control: must-revalidate
        Content-Length: 119
        Content-Type: application/json
        Date: Thu, 24 Jan 2019 17:19:59 GMT
        Server: CouchDB/2.3.0-a1e11cea9 (Erlang OTP/21)

        {
          "db_name": "my_new_db",
          "doc_count": 1,
          "doc_del_count": 0,
          "partition": "sensor-260",
          "sizes": {
            "active": 244,
            "external": 347
          }
        }

``/db/_partition/partition/_all_docs``
======================================

.. http:get:: /{db}/_partition/{partition}/_all_docs
    :synopsis: Return all docs in the specified partition

    :param db: Database name
    :param partition: Partition name

    This endpoint is a convenience endpoint for automatically setting
    bounds on the provided partition range. Similar results can be had
    by using the global ``/db/_all_docs`` endpoint with appropriately
    configured values for ``start_key`` and ``end_key``.

    Refer to the :ref:`view endpoint <api/ddoc/view>` documentation for
    a complete description of the available query parameters and the format
    of the returned data.

    **Request**:

    .. code-block:: http

        GET /db/_partition/sensor-260/_all_docs HTTP/1.1
        Accept: application/json
        Host: localhost:5984

    **Response**:

    .. code-block:: http

        HTTP/1.1 200 OK
        Cache-Control: must-revalidate
        Content-Type: application/json
        Date: Sat, 10 Aug 2013 16:22:56 GMT
        ETag: "1W2DJUZFZSZD9K78UFA3GZWB4"
        Server: CouchDB (Erlang/OTP)
        Transfer-Encoding: chunked

        {
          "offset": 0,
          "rows": [
            {
              "id": "sensor-260:sensor-reading-ca33c748-2d2c-4ed1-8abf-1bca4d9d03cf",
              "key": "sensor-260:sensor-reading-ca33c748-2d2c-4ed1-8abf-1bca4d9d03cf",
              "value": {
                "rev": "1-05ed6f7abf84250e213fcb847387f6f5"
              }
            }
          ],
          "total_rows": 1
        }

.. _api/partitioned/views:

``/db/_partition/partition/_design/design-doc/_view/view-name``
===============================================================

.. http:get:: /{db}/_partition/{partition}/_design/{ddoc}/_view/{view}
    :synopsis: Execute a partitioned query

    :param db: Database name
    :param partition: Partition name
    :param ddoc: Design document id
    :param view: View name

    This endpoint is responsible for executing a partitioned query. The
    returned view result will only contain rows with the specified
    partition name.

    Refer to the :ref:`view endpoint <api/ddoc/view>` documentation for
    a complete description of the available query parameters and the format
    of the returned data.

    .. code-block:: http

        GET /db/_partition/sensor-260/_design/sensor-readings/_view/by_sensor HTTP/1.1
        Accept: application/json
        Host: localhost:5984

    **Response**:

    .. code-block:: http

        HTTP/1.1 200 OK
        Cache-Control: must-revalidate
        Content-Type: application/json
        Date: Wed, 21 Aug 2013 09:12:06 GMT
        ETag: "2FOLSBSW4O6WB798XU4AQYA9B"
        Server: CouchDB (Erlang/OTP)
        Transfer-Encoding: chunked

        {
          "offset": 0,
          "rows": [
            {
              "id": "sensor-260:sensor-reading-ca33c748-2d2c-4ed1-8abf-1bca4d9d03cf",
              "key": [
                "sensor-260",
                "0"
              ],
              "value": null
            },
            {
              "id": "sensor-260:sensor-reading-ca33c748-2d2c-4ed1-8abf-1bca4d9d03cf",
              "key": [
                "sensor-260",
                "1"
              ],
              "value": null
            },
            {
              "id": "sensor-260:sensor-reading-ca33c748-2d2c-4ed1-8abf-1bca4d9d03cf",
              "key": [
                "sensor-260",
                "2"
              ],
              "value": null
            },
            {
              "id": "sensor-260:sensor-reading-ca33c748-2d2c-4ed1-8abf-1bca4d9d03cf",
              "key": [
                "sensor-260",
                "3"
              ],
              "value": null
            }
          ],
          "total_rows": 4
        }
.. _api/partitioned/find:

``/db/_partition/partition_id/_find``
=====================================

.. http:post:: /{db}/_partition/{partition_id}/_find
    :synopsis: Query the partition specified by ``partition_id``

    :param db: Database name
    :param partition id: Name of the partition to query

    This endpoint is responsible for finding a partition query by its ID.
    The returned view result will only contain rows with the
    specified partition id.

    Refer to the :ref:`find endpoint <api/db/_find>`
    documentation for a complete description of the
    available parameters and the format
    of the returned data.
.. _api/partitioned/explain:

``/db/_partition/partition_id/_explain``
========================================

.. http:post:: /{db}/_partition/{partition_id}/_explain
    :synopsis: Find index that is used with a query

    :param db: Database name
    :partition id: Name of the partition to query

    This endpoint shows which index is being used by the query.

    Refer to the :ref:`explain endpoint <api/db/find/explain>`
    documentation for a complete description of the available
    parameters and the format of the returned data.
