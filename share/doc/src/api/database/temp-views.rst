.. _api/db/temp_view:
.. _api/db/temp_view.post:

``POST /db/_temp_view``
=======================

* **Method**: ``POST /db/_temp_view``
* **Request**: JSON with the temporary view definition
* **Response**: Temporary view result set
* **Admin Privileges Required**: yes

Creates (and executes) a temporary view based on the view function
supplied in the JSON request. For example:

.. code-block:: http

    POST http://couchdb:5984/recipes/_temp_view
    Content-Type: application/json

    {
       "map" : "function(doc) { if (doc.value > 9995) { emit(null, doc.value); } }"
    }

The resulting JSON response is the result from the execution of the
temporary view:

.. code-block:: javascript

    {
       "total_rows" : 3,
       "rows" : [
          {
             "value" : 9998.41913029012,
             "id" : "05361cc6aa42033878acc1bacb1f39c2",
             "key" : null
          },
          {
             "value" : 9998.94149934853,
             "id" : "1f443f471e5929dd7b252417625ed170",
             "key" : null
          },
          {
             "value" : 9998.01511339154,
             "id" : "1f443f471e5929dd7b252417629c102b",
             "key" : null
          }
       ],
       "offset" : 0
    }

The arguments also available to standard view requests also apply to
temporary views, but the execution of the view may take some time as it
relies on being executed at the time of the request. In addition to the
time taken, they are also computationally very expensive to produce. You
should use a defined view if you want to achieve the best performance.
