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


.. _api/ddoc/show:

``/db/_design/design-doc/_show/show-name``
==========================================

.. http:get:: /{db}/_design/{ddoc}/_show/{func}
  :synopsis: Executes a show function against null document

.. http:post:: /{db}/_design/{ddoc}/_show/{func}
  :synopsis: Same as GET method for the related endpoint

  Applies :ref:`show function <showfun>` for `null` document.

  The request and response parameters are depended upon function implementation.

  :param db: Database name
  :param ddoc: Design document name
  :param func: Show function name
  :>header ETag: Response signature
  :query string format: Format of the returned response.
    Used by :js:func:`provides` function
  :code 200: Request completed successfully
  :code 500: Query server error

  **Function**:

  .. code-block:: javascript

    function(doc, req) {
      if (!doc) {
        return {body: "no doc"}
      } else {
        return {body: doc.description}
      }
    }

  **Request**:

  .. code-block:: http

    GET /recipes/_design/recipe/_show/description HTTP/1.1
    Accept: application/json
    Host: localhost:5984

  **Response**:

  .. code-block:: http

    HTTP/1.1 200 OK
    Content-Length: 6
    Content-Type: text/html; charset=utf-8
    Date: Wed, 21 Aug 2013 12:34:07 GMT
    Etag: "7Z2TO7FPEMZ0F4GH0RJCRIOAU"
    Server: CouchDB (Erlang/OTP)
    Vary: Accept

    no doc


.. _api/ddoc/show/id:

``/db/_design/design-doc/_show/show-name/doc-id``
=================================================

.. http:get:: /{db}/_design/{ddoc}/_show/{func}/{docid}
  :synopsis: Executes a show function against the specified document
.. http:post:: /{db}/_design/{ddoc}/_show/{func}/{docid}
  :synopsis: Same as GET method for the related endpoint

  Applies :ref:`show function <showfun>` for the specified document.

  The request and response parameters are depended upon function implementation.

  :param db: Database name
  :param ddoc: Design document name
  :param func: Show function name
  :param docid: Document ID
  :>header ETag: Response signature
  :query string format: Format of the returned response.
    Used by :js:func:`provides` function
  :code 200: Request completed successfully
  :code 500: Query server error

  **Function**:

  .. code-block:: javascript

    function(doc, req) {
      if (!doc) {
        return {body: "no doc"}
      } else {
        return {body: doc.description}
      }
    }

  **Request**:

  .. code-block:: http

    GET /recipes/_design/recipe/_show/description/SpaghettiWithMeatballs HTTP/1.1
    Accept: application/json
    Host: localhost:5984

  **Response**:

  .. code-block:: http

    HTTP/1.1 200 OK
    Content-Length: 88
    Content-Type: text/html; charset=utf-8
    Date: Wed, 21 Aug 2013 12:38:08 GMT
    Etag: "8IEBO8103EI98HDZL5Z4I1T0C"
    Server: CouchDB (Erlang/OTP)
    Vary: Accept

    An Italian-American dish that usually consists of spaghetti, tomato sauce and meatballs.


.. _api/ddoc/list:

``/db/_design/design-doc/_list/list-name/view-name``
====================================================

.. http:get:: /{db}/_design/{ddoc}/_list/{func}/{view}
  :synopsis: Executes a list function against the view from the same design document
.. http:post:: /{db}/_design/{ddoc}/_list/{func}/{view}
  :synopsis: Same as GET method for the related endpoint

  Applies :ref:`list function <listfun>` for the :ref:`view function <viewfun>`
  from the same design document.

  The request and response parameters are depended upon function implementation.

  :param db: Database name
  :param ddoc: Design document name
  :param func: List function name
  :param view: View function name
  :>header ETag: Response signature
  :>header Transfer-Encoding: ``chunked``
  :query string format: Format of the returned response.
    Used by :js:func:`provides` function
  :code 200: Request completed successfully
  :code 500: Query server error

  **Function**:

  .. code-block:: javascript

    function(head, req) {
      var row = getRow();
      if (!row){
        return 'no ingredients'
      }
      send(row.key);
      while(row=getRow()){
        send(', ' + row.key);
      }
    }

  **Request**:

  .. code-block:: http

    GET /recipes/_design/recipe/_list/ingredients/by_name HTTP/1.1
    Accept: text/plain
    Host: localhost:5984

  **Response**:

  .. code-block:: http

    HTTP/1.1 200 OK
    Content-Type: text/plain; charset=utf-8
    Date: Wed, 21 Aug 2013 12:49:15 GMT
    Etag: "D52L2M1TKQYDD1Y8MEYJR8C84"
    Server: CouchDB (Erlang/OTP)
    Transfer-Encoding: chunked
    Vary: Accept

    meatballs, spaghetti, tomato sauce


.. _api/ddoc/list/ddoc:

``/db/_design/design-doc/_list/list-name/other-ddoc/view-name``
===============================================================

.. http:get:: /{db}/_design/{ddoc}/_list/{func}/{other-ddoc}/{view}
  :synopsis: Executes a list function against the view from other design document
.. http:post:: /{db}/_design/{ddoc}/_list/{func}/{other-ddoc}/{view}
  :synopsis: Same as GET method for the related endpoint

  Applies :ref:`list function <listfun>` for the :ref:`view function <viewfun>`
  from the other design document.

  The request and response parameters are depended upon function implementation.

  :param db: Database name
  :param ddoc: Design document name
  :param func: List function name
  :param other-ddoc: Other design document name that holds view function
  :param view: View function name
  :>header ETag: Response signature
  :>header Transfer-Encoding: ``chunked``
  :query string format: Format of the returned response.
    Used by :js:func:`provides` function
  :code 200: Request completed successfully
  :code 500: Query server error

  **Function**:

  .. code-block:: javascript

    function(head, req) {
      var row = getRow();
      if (!row){
        return 'no ingredients'
      }
      send(row.key);
      while(row=getRow()){
        send(', ' + row.key);
      }
    }

  **Request**:

  .. code-block:: http

    GET /recipes/_design/ingredient/_list/ingredients/recipe/by_ingredient?key="spaghetti" HTTP/1.1
    Accept: text/plain
    Host: localhost:5984

  **Response**:

  .. code-block:: http

    HTTP/1.1 200 OK
    Content-Type: text/plain; charset=utf-8
    Date: Wed, 21 Aug 2013 12:49:15 GMT
    Etag: "5L0975X493R0FB5Z3043POZHD"
    Server: CouchDB (Erlang/OTP)
    Transfer-Encoding: chunked
    Vary: Accept

    spaghetti


.. _api/ddoc/update:

``/db/_design/design-doc/_update/update-name``
==============================================

.. http:post:: /{db}/_design/{ddoc}/_update/{func}
  :synopsis: Executes an update function against the null document

  Executes :ref:`update function <updatefun>` on server side for ``null``
  document.

  :param db: Database name
  :param ddoc: Design document name
  :param func: Update function name
  :>header X-Couch-Id: Created/updated document's ID
  :>header X-Couch-Update-NewRev: Created/updated document's revision
  :code 200: No document was created or updated
  :code 201: Document was created or updated
  :code 500: Query server error

  **Function**:

  .. code-block:: javascript

    function(doc, req) {
      if (!doc){
        return [null, {'code': 400,
                       'json': {'error': 'missed',
                                'reason': 'no document to update'}}]
      } else {
        doc.ingredients.push(req.body);
        return [doc, {'json': {'status': 'ok'}}];
      }
    }

  **Request**:

  .. code-block:: http

    POST /recipes/_design/recipe/_update/ingredients HTTP/1.1
    Accept: application/json
    Content-Length: 10
    Content-Type: application/json
    Host: localhost:5984

    something

  **Response**:

  .. code-block:: http

    HTTP/1.1 404 Object Not Found
    Cache-Control: must-revalidate
    Content-Length: 52
    Content-Type: application/json
    Date: Wed, 21 Aug 2013 14:00:58 GMT
    Server: CouchDB (Erlang/OTP)

    {
        "error": "missed",
        "reason": "no document to update"
    }


.. _api/ddoc/update/id:

``/db/_design/design-doc/_update/update-name/doc-id``
=====================================================

.. http:put:: /{db}/_design/{ddoc}/_update/{func}/{docid}
  :synopsis: Executes an update function against the specified document

  Executes :ref:`update function <updatefun>` on server side for the specified
  document.

  :param db: Database name
  :param ddoc: Design document name
  :param func: Update function name
  :param docid: Document ID
  :>header X-Couch-Id: Created/updated document's ID
  :>header X-Couch-Update-NewRev: Created/updated document's revision
  :code 200: No document was created or updated
  :code 201: Document was created or updated
  :code 500: Query server error

  **Function**:

  .. code-block:: javascript

    function(doc, req) {
      if (!doc){
        return [null, {'code': 400,
                       'json': {'error': 'missed',
                                'reason': 'no document to update'}}]
      } else {
        doc.ingredients.push(req.body);
        return [doc, {'json': {'status': 'ok'}}];
      }
    }

  **Request**:

  .. code-block:: http

    POST /recipes/_design/recipe/_update/ingredients/SpaghettiWithMeatballs HTTP/1.1
    Accept: application/json
    Content-Length: 5
    Content-Type: application/json
    Host: localhost:5984

    love

  **Response**:

  .. code-block:: http

    HTTP/1.1 201 Created
    Cache-Control: must-revalidate
    Content-Length: 16
    Content-Type: application/json
    Date: Wed, 21 Aug 2013 14:11:34 GMT
    Server: CouchDB (Erlang/OTP)
    X-Couch-Id: SpaghettiWithMeatballs
    X-Couch-Update-NewRev: 12-a5e099df5720988dae90c8b664496baf

    {
        "status": "ok"
    }
