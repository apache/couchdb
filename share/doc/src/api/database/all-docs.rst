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


.. _api/db/all_docs:

``/db/_all_docs``
=================

.. http:get:: /{db}/_all_docs

  Returns a JSON structure of all of the documents in a given database.
  The information is returned as a JSON structure containing meta
  information about the return structure, and the list documents and basic
  contents, consisting the ID, revision and key. The key is generated from
  the document ID.

  :resheader ETag: Response signature
  :query boolean conflicts: Includes `conflicts` information in response.
    Ignored if `include_docs` isn't ``true``. Default is ``false``.
  :query boolean descending: Return the documents in descending by key order.
    Default is ``false``.
  :query string endkey: Stop returning records when the specified key is
    reached. *Optional*.
  :query string end_key: Alias for `endkey` param.
  :query string endkey_docid: Stop returning records when the specified
    document ID is reached. *Optional*.
  :query string end_key_doc_id: Alias for `endkey_docid` param.
  :query boolean group: Group the results using the reduce function to a group
    or single row. Default is ``false``.
  :query number group_level: Specify the group level to be used. *Optional*.
  :query boolean include_docs: Include the full content of the documents in
    the return. Default is ``false``.
  :query boolean inclusive_end: Specifies whether the specified end key should
    be included in the result. Default is ``true``.
  :query string key: Return only documents that match the specified key.
    *Optional*.
  :query number limit: Limit the number of the returned documents to the
    specified number. *Optional*.
  :query boolean reduce: Use the reduction function. Default is ``true``.
  :query number skip: Skip this number of records before starting to return
    the results. Default is ``0``.
  :query string stale: Allow the results from a stale view to be used.
    Supported values: ``ok`` and ``update_after``. *Optional*.
  :query string startkey: Return records starting with the specified key.
    *Optional*.
  :query string start_key: Alias for `startkey` param.
  :query string startkey_docid: Return records starting with the specified
    document ID. *Optional*.
  :query string start_key_doc_id: Alias for `startkey_docid` param.
  :query boolean update_seq: Response includes an ``update_seq`` value
    indicating which sequence id of the database the view reflects.
    Default is ``false``.

  The response JSON object contains next fields:

  :json number offset: Offset where the document list started
  :json array rows: Array of view row objects. By default the information
    returned contains only the document ID and revision.
  :json number total_rows: Number of documents in the database/view
  :json number update_seq: Current update sequence for the database


  **Request**:

  .. code-block:: http

    GET /db/_all_docs HTTP/1.1
    Accept: application/json
    Host: localhost:5984

  **Response**:

  .. code-block:: http

    HTTP/1.1 200 OK
    Cache-Control: must-revalidate
    Content-Type: application/json
    Date: Sat, 10 Aug 2013 16:22:56 GMT
    ETag: "1W2DJUZFZSZD9K78UFA3GZWB4"
    Server: CouchDB/1.4.0 (Erlang OTP/R16B)
    Transfer-Encoding: chunked

    {
        "offset": 0,
        "rows": [
            {
                "id": "16e458537602f5ef2a710089dffd9453",
                "key": "16e458537602f5ef2a710089dffd9453",
                "value": {
                    "rev": "1-967a00dff5e02add41819138abb3284d"
                }
            },
            {
                "id": "a4c51cdfa2069f3e905c431114001aff",
                "key": "a4c51cdfa2069f3e905c431114001aff",
                "value": {
                    "rev": "1-967a00dff5e02add41819138abb3284d"
                }
            },
            {
                "id": "a4c51cdfa2069f3e905c4311140034aa",
                "key": "a4c51cdfa2069f3e905c4311140034aa",
                "value": {
                    "rev": "5-6182c9c954200ab5e3c6bd5e76a1549f"
                }
            },
            {
                "id": "a4c51cdfa2069f3e905c431114003597",
                "key": "a4c51cdfa2069f3e905c431114003597",
                "value": {
                    "rev": "2-7051cbe5c8faecd085a3fa619e6e6337"
                }
            },
            {
                "id": "f4ca7773ddea715afebc4b4b15d4f0b3",
                "key": "f4ca7773ddea715afebc4b4b15d4f0b3",
                "value": {
                    "rev": "2-7051cbe5c8faecd085a3fa619e6e6337"
                }
            }
        ],
        "total_rows": 5
    }


.. http:post:: /{db}/_all_docs

  The ``POST`` to ``_all_docs`` allows to specify multiple keys to be
  selected from the database. This enables you to request multiple
  documents in a single request, in place of multiple :ref:`api/doc.get`
  requests.

  The request body should contain a list of the keys to be returned as an
  array to a ``keys`` object. For example:

  .. code-block:: http

    POST /db/_all_docs HTTP/1.1
    Accept: application/json
    Content-Length: 70
    Content-Type: application/json
    Host: localhost:5984

    {
       "keys" : [
          "Zingylemontart",
          "Yogurtraita"
       ]
    }

  The return JSON is the all documents structure, but with only the
  selected keys in the output:

  .. code-block:: javascript

      {
         "total_rows" : 2666,
         "rows" : [
            {
               "value" : {
                  "rev" : "1-a3544d296de19e6f5b932ea77d886942"
               },
               "id" : "Zingylemontart",
               "key" : "Zingylemontart"
            },
            {
               "value" : {
                  "rev" : "1-91635098bfe7d40197a1b98d7ee085fc"
               },
               "id" : "Yogurtraita",
               "key" : "Yogurtraita"
            }
         ],
         "offset" : 0
      }
