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


.. _api/db/temp_view:

``/db/_temp_view``
==================

.. http:post:: /{db}/_temp_view

  Creates (and executes) a temporary view based on the view function
  supplied in the JSON request.

  The arguments also available to standard view requests also apply to
  temporary views, but the execution of the view may take some time as it
  relies on being executed at the time of the request. In addition to the
  time taken, they are also computationally very expensive to produce. You
  should use a defined view if you want to achieve the best performance.

  See :ref:`api/ddoc/view` for more info.

  **Request**:

  .. code-block:: http

    POST /db/_temp_view?group=true HTTP/1.1
    Accept: application/json
    Content-Length: 92
    Content-Type: application/json
    Host: localhost:5984

    {
        "map": "function(doc) { if (doc.value) { emit(doc.value, null); } }",
        "reduce": "_count"
    }

  **Response**:

  .. code-block:: http

    HTTP/1.1 200 OK
    Cache-Control: must-revalidate
    Content-Type: application/json
    Date: Tue, 13 Aug 2013 12:28:12 GMT
    ETag: "AU33B3N7S9K4SAZSFA048HVB4"
    Server: CouchDB (Erlang/OTP)
    Transfer-Encoding: chunked

    {
        "rows": [
            {
                "key": -10,
                "value": 1
            },
            {
                "key": 10,
                "value": 2
            },
            {
                "key": 15,
                "value": 1
            }
        ]
    }
