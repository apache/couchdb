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

.. _api/server/csrt:

============================================
``/_active_resources/_match/{matcher-name}``
============================================

.. versionadded:: 3.5.1

Find active processes (being tracked in CSRT) using a declarative JSON querying syntax. You can learn more about Couch Stats Resource Tracker (CSRT) :doc:`here </csrt/index>`.
The query passed to the endpoint can be in following forms

* :ref:`group_by <api/server/csrt/group_by>` - The value of ``counter_key`` would be extracted and aggregated in the order of provided ``aggregate_keys``.
* :ref:`count_by <api/server/csrt/count_by>` - The value of ``counter_key`` would be extracted and sorted by given ``aggregate_keys``.
* :ref:`sort_by <api/server/csrt/sort_by>` - Count number of unique combinations of values by given ``aggregate_keys``.

.. http:post:: /_active_resources/_match/{matcher-name}
    :synopsis: Return snapshot of active processes (being tracked in CSRT)

    Find active processes (being tracked in CSRT) using a declarative JSON querying syntax.

    :param matcher-name: The name of the matcher to use for filtering active processes.

    :<header Content-Type: - :mimetype:`application/json`

    :<json object group_by: JSON object describing the :ref:`group_by <api/server/csrt/group_by>` query.
    :<json string group_by.counter_key: The key of the counter to extract and aggregate.
    :<json array|string group_by.aggregate_keys: The keys to aggregate the counter values by.

    :<json object count_by: JSON object describing the :ref:`count_by <api/server/csrt/count_by>` query.
    :<json array|string count_by.aggregate_keys: The keys to aggregate the counter values by.

    :<json object sort_by: JSON object describing the :ref:`sort_by <api/server/csrt/sort_by>` query.
    :<json string sort_by.counter_key: The key of the counter to extract and aggregate.
    :<json array|string sort_by.aggregate_keys: The keys to aggregate the counter values by.

    :>header Content-Type: - :mimetype:`application/json`

    :>json array: Array of objects containing the aggregated counter values for each node.
    :>jsonarr object row: Object containing the results received from a node.
    :>json array row.result: Array of objects containing the aggregated counter values.
    :>jsonarr object result[_].key: Object containing the aggregate keys and their values.
    :>json string|none result[_].key.pid_ref: Opaque string representing identity of the resource.
    :>json string|none result[_].key.dbname: The database name used by the resource.
    :>json string|none result[_].key.username: The username used to access the resource.
    :>json string|none result[_].key.dbname: The database name.
    :>json string|none result[_].key.type: The string representing a type of the resource.
    :>jsonarr int result[_].value: The aggregated value of ``counter_key``
    :>json string row.node: The node that the aggregated counter values belong to.
    :>json array row.errors: Array of error messages.

    :code 200: Request completed successfully
    :code 400: Invalid request (a JSON object of following structure):

        .. code-block:: json

            {
                "error": "bad_request",
                "reason": "Unknown field name 'unknown_field'"
            }


        The following errors may be returned:

            +-------------+-------------------------------------------+
            | Error       | Reason                                    |
            +=============+===========================================+
            | bad_request | Unknown field name '...'                  |
            +-------------+-------------------------------------------+
            | bad_request | Unknown matcher '...'                     |
            +-------------+-------------------------------------------+
            | bad_request | "Multiple aggregations are not supported" |
            +-------------+-------------------------------------------+
            | bad_request | "Multiple keys in 'counter_key'"          |
            +-------------+-------------------------------------------+

.. _api/server/csrt/group_by:

``group_by`` Syntax
-------------------

The ``group_by`` syntax is used to find active processes (being tracked in CSRT) which are matched by given ``matcher-name``.
For all active processes matching the matcher the value of ``counter_key`` would be extracted and aggregated in the order of
provided ``aggregate_keys``.

    **Request**:

    Example request body to return ``ioq_calls`` grouped by ``["username", "dbname"]`` from ``docs_read`` matcher.

    .. code-block:: http

        POST /_active_resources/_match/docs_read HTTP/1.1
        Accept: application/json
        Content-Type: application/json
        Content-Length: 80
        Host: localhost:5984

        {
            "group_by": {
                "counter_key": "ioq_calls",
                "aggregate_keys": [
                    "username",
                    "dbname"
                ]
            }
        }

    **Response**:

    .. code-block:: http

        HTTP/1.1 200 OK
        Cache-Control: must-revalidate
        Content-Length: 783
        Content-Type: application/json
        Date: Thu, 29 Jul 2025 14:05:59 GMT
        Server: CouchDB (Erlang OTP/26)

        [
            {
                "result": [
                    {
                        "value": 90817,
                        "key": {
                            "username": "user_foo",
                            "dbname": "db2"
                        }
                    },
                    {
                        "value": 42434,
                        "key" : {
                            "username" : "user_foo",
                            "dbname" : "db1"
                        }
                    },
                    {
                        "value" : 84828,
                        "key" : {
                            "username" : "user_bar",
                            "dbname" : "db2"
                        }
                    },
                    {
                        "value" : 6278,
                        "key" : {
                            "username" : "user_bar",
                            "dbname" : "db1"
                        }
                    }
                ],
                "node" : "node1@127.0.0.1",
                "errors" : [
                ]
            }
        ]

.. _api/server/csrt/sort_by:

``sort_by`` Syntax
------------------

The ``sort_by`` syntax is used to find active processes (being tracked in CSRT) which are matched by given ``matcher-name``.
For all active processes matching the matcher the value of ``counter_key`` would be extracted and sorted by given ``aggregate_keys``.

    **Request**:

    Example request body to return ``ioq_calls`` sorted by ``["username", "dbname"]`` from ``docs_read`` matcher.

    .. code-block:: http

        POST /_active_resources/_match/docs_read HTTP/1.1
        Accept: application/json
        Content-Type: application/json
        Content-Length: 80
        Host: localhost:5984

        {
            "sort_by" : {
                "counter_key" : "ioq_calls",
                "aggregate_keys" : [
                    "username",
                    "dbname"
                ]
            }
        }

    **Response**:

    .. code-block:: http

        HTTP/1.1 200 OK
        Cache-Control: must-revalidate
        Content-Length: 783
        Content-Type: application/json
        Date: Thu, 29 Jul 2025 14:05:59 GMT
        Server: CouchDB (Erlang OTP/26)

        [
            {
                "result": [
                    {
                        "value": 90817,
                        "key": {
                            "username": "user_foo",
                            "dbname": "db2"
                        }
                    },
                    {
                        "value": 42434,
                        "key" : {
                            "username" : "user_foo",
                            "dbname" : "db1"
                        }
                    },
                    {
                        "value" : 84828,
                        "key" : {
                            "username" : "user_bar",
                            "dbname" : "db2"
                        }
                    },
                    {
                        "value" : 6278,
                        "key" : {
                            "username" : "user_bar",
                            "dbname" : "db1"
                        }
                    }
                ],
                "node" : "node1@127.0.0.1",
                "errors" : [
                ]
            }
        ]

.. _api/server/csrt/count_by:

``count_by`` Syntax
-------------------

The ``count_by`` syntax is used to find active processes (being tracked in CSRT) which are matched by given ``matcher-name``.
For all active processes matching the matcher we count number of unique combinations of values by given ``aggregate_keys``.

    **Request**:

    Example request body to return number of unique combinations of values ``["username", "dbname"]`` from ``docs_read`` matcher.

    .. code-block:: http

        POST /_active_resources/_match/docs_read HTTP/1.1
        Accept: application/json
        Content-Type: application/json
        Content-Length: 72
        Host: localhost:5984

        {
            "count_by" : {
                "aggregate_keys" : [
                    "username",
                    "dbname"
                ]
            }
        }

    **Response**:

    .. code-block:: http

        HTTP/1.1 200 OK
        Cache-Control: must-revalidate
        Content-Length: 715
        Content-Type: application/json
        Date: Thu, 29 Jul 2025 14:12:32 GMT
        Server: CouchDB (Erlang OTP/26)

        [
            {
                "result": [
                    {
                        "value": 7,
                        "key": {
                            "username": "user_foo",
                            "dbname": "db2"
                        }
                    },
                    {
                        "value": 42,
                        "key" : {
                            "username" : "user_foo",
                            "dbname" : "db1"
                        }
                    },
                    {
                        "value" : 28,
                        "key" : {
                            "username" : "user_bar",
                            "dbname" : "db2"
                        }
                    },
                    {
                        "value" : 627,
                        "key" : {
                            "username" : "user_bar",
                            "dbname" : "db1"
                        }
                    }
                ],
                "node" : "node1@127.0.0.1",
                "errors" : [
                ]
            }
        ]
