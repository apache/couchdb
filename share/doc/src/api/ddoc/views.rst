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


.. _api/ddoc/view:
.. _api/ddoc/view.get:

``GET /db/_design/design-doc/_view/view-name``
==============================================

* **Method**: ``GET /db/_design/design-doc/_view/view-name``
* **Request**: None
* **Response**: JSON of the documents returned by the view
* **Admin Privileges Required**: no
* **Query Arguments**:

  * **Argument**: descending

    * **Description**:  Return the documents in descending by key order
    * **Optional**: yes
    * **Type**: boolean
    * **Default**: false

  * **Argument**: endkey

    * **Description**:  Stop returning records when the specified key is reached
    * **Optional**: yes
    * **Type**: string

  * **Argument**: endkey_docid

    * **Description**:  Stop returning records when the specified document
      ID is reached
    * **Optional**: yes
    * **Type**: string

  * **Argument**: group

    * **Description**:  Group the results using the reduce function to a
      group or single row
    * **Optional**: yes
    * **Type**: boolean
    * **Default**: false

  * **Argument**: group_level

    * **Description**:  Specify the group level to be used
    * **Optional**: yes
    * **Type**: numeric

  * **Argument**: include_docs

    * **Description**:  Include the full content of the documents in the return
    * **Optional**: yes
    * **Type**: boolean
    * **Default**: false

  * **Argument**: inclusive_end

    * **Description**:  Specifies whether the specified end key should be
      included in the result
    * **Optional**: yes
    * **Type**: boolean
    * **Default**: true

  * **Argument**: key

    * **Description**:  Return only documents that match the specified key
    * **Optional**: yes
    * **Type**: string

  * **Argument**: limit

    * **Description**:  Limit the number of the returned documents to the
      specified number
    * **Optional**: yes
    * **Type**: numeric

  * **Argument**: reduce

    * **Description**:  Use the reduction function
    * **Optional**: yes
    * **Type**: boolean
    * **Default**: true

  * **Argument**: skip

    * **Description**:  Skip this number of records before starting to return
      the results
    * **Optional**: yes
    * **Type**: numeric
    * **Default**: 0

  * **Argument**: stale

    * **Description**:  Allow the results from a stale view to be used
    * **Optional**: yes
    * **Type**: string
    * **Default**:
    * **Supported Values**

      * **ok**: Allow stale views

  * **Argument**: startkey

    * **Description**:  Return records starting with the specified key
    * **Optional**: yes
    * **Type**: string

  * **Argument**: startkey_docid

    * **Description**:  Return records starting with the specified document ID
    * **Optional**: yes
    * **Type**: string

  * **Argument**: update_seq

    * **Description**:  Include the update sequence in the generated results
    * **Optional**: yes
    * **Type**: boolean
    * **Default**: false

Executes the specified ``view-name`` from the specified ``design-doc``
design document.

Querying Views and Indexes
--------------------------

The definition of a view within a design document also creates an index
based on the key information defined within each view. The production
and use of the index significantly increases the speed of access and
searching or selecting documents from the view.

However, the index is not updated when new documents are added or
modified in the database. Instead, the index is generated or updated,
either when the view is first accessed, or when the view is accessed
after a document has been updated. In each case, the index is updated
before the view query is executed against the database.

View indexes are updated incrementally in the following situations:

-  A new document has been added to the database.

-  A document has been deleted from the database.

-  A document in the database has been updated.

View indexes are rebuilt entirely when the view definition changes. To
achieve this, a 'fingerprint' of the view definition is created when the
design document is updated. If the fingerprint changes, then the view
indexes are entirely rebuilt. This ensures that changes to the view
definitions are reflected in the view indexes.

.. note::
   View index rebuilds occur when one view from the same the view group
   (i.e. all the views defined within a single a design document) has
   been determined as needing a rebuild. For example, if if you have a
   design document with different views, and you update the database,
   all three view indexes within the design document will be updated.

Because the view is updated when it has been queried, it can result in a
delay in returned information when the view is accessed, especially if
there are a large number of documents in the database and the view index
does not exist. There are a number of ways to mitigate, but not
completely eliminate, these issues. These include:

-  Create the view definition (and associated design documents) on your
   database before allowing insertion or updates to the documents. If
   this is allowed while the view is being accessed, the index can be
   updated incrementally.

-  Manually force a view request from the database. You can do this
   either before users are allowed to use the view, or you can access
   the view manually after documents are added or updated.

-  Use the ``/db/_changes`` method to monitor for changes to the
   database and then access the view to force the corresponding view
   index to be updated. See :ref:`api/db/changes` for more information.

-  Use a monitor with the ``update_notification`` section of the CouchDB
   configuration file to monitor for changes to your database, and
   trigger a view query to force the view to be updated. For more
   information, see :ref:`update-notifications`.

None of these can completely eliminate the need for the indexes to be
rebuilt or updated when the view is accessed, but they may lessen the
effects on end-users of the index update affecting the user experience.

Another alternative is to allow users to access a 'stale' version of the
view index, rather than forcing the index to be updated and displaying
the updated results. Using a stale view may not return the latest
information, but will return the results of the view query using an
existing version of the index.

For example, to access the existing stale view ``by_recipe`` in the
``recipes`` design document:

.. code-block:: text

    http://couchdb:5984/recipes/_design/recipes/_view/by_recipe?stale=ok

Accessing a stale view:

-  Does not trigger a rebuild of the view indexes, even if there have
   been changes since the last access.

-  Returns the current version of the view index, if a current version
   exists.

-  Returns an empty result set if the given view index does exist.

As an alternative, you use the ``update_after`` value to the ``stale``
parameter. This causes the view to be returned as a stale view, but for
the update process to be triggered after the view information has been
returned to the client.

In addition to using stale views, you can also make use of the
``update_seq`` query argument. Using this query argument generates the
view information including the update sequence of the database from
which the view was generated. The returned value can be compared this to
the current update sequence exposed in the database information
(returned by :ref:`api/db.get`).

Sorting Returned Rows
---------------------

Each element within the returned array is sorted using native UTF-8
sorting according to the contents of the key portion of the emitted
content. The basic order of output is as follows:

-  ``null``

-  ``false``

-  ``true``

-  Numbers

-  Text (case sensitive, lowercase first)

-  Arrays (according to the values of each element, in order)

-  Objects (according to the values of keys, in key order)

You can reverse the order of the returned view information by using the
``descending`` query value set to true. For example, Retrieving the list
of recipes using the ``by_title`` (limited to 5 records) view:

.. code-block:: javascript

    {
       "offset" : 0,
       "rows" : [
          {
             "id" : "3-tiersalmonspinachandavocadoterrine",
             "key" : "3-tier salmon, spinach and avocado terrine",
             "value" : [
                null,
                "3-tier salmon, spinach and avocado terrine"
             ]
          },
          {
             "id" : "Aberffrawcake",
             "key" : "Aberffraw cake",
             "value" : [
                null,
                "Aberffraw cake"
             ]
          },
          {
             "id" : "Adukiandorangecasserole-microwave",
             "key" : "Aduki and orange casserole - microwave",
             "value" : [
                null,
                "Aduki and orange casserole - microwave"
             ]
          },
          {
             "id" : "Aioli-garlicmayonnaise",
             "key" : "Aioli - garlic mayonnaise",
             "value" : [
                null,
                "Aioli - garlic mayonnaise"
             ]
          },
          {
             "id" : "Alabamapeanutchicken",
             "key" : "Alabama peanut chicken",
             "value" : [
                null,
                "Alabama peanut chicken"
             ]
          }
       ],
       "total_rows" : 2667
    }

Requesting the same in descending order will reverse the entire view
content. For example the request

.. code-block:: http

    GET http://couchdb:5984/recipes/_design/recipes/_view/by_title?limit=5&descending=true
    Accept: application/json
    Content-Type: application/json

Returns the last 5 records from the view:

.. code-block:: javascript

    {
       "offset" : 0,
       "rows" : [
          {
             "id" : "Zucchiniinagrodolcesweet-sourcourgettes",
             "key" : "Zucchini in agrodolce (sweet-sour courgettes)",
             "value" : [
                null,
                "Zucchini in agrodolce (sweet-sour courgettes)"
             ]
          },
          {
             "id" : "Zingylemontart",
             "key" : "Zingy lemon tart",
             "value" : [
                null,
                "Zingy lemon tart"
             ]
          },
          {
             "id" : "Zestyseafoodavocado",
             "key" : "Zesty seafood avocado",
             "value" : [
                null,
                "Zesty seafood avocado"
             ]
          },
          {
             "id" : "Zabaglione",
             "key" : "Zabaglione",
             "value" : [
                null,
                "Zabaglione"
             ]
          },
          {
             "id" : "Yogurtraita",
             "key" : "Yogurt raita",
             "value" : [
                null,
                "Yogurt raita"
             ]
          }
       ],
       "total_rows" : 2667
    }

The sorting direction is applied before the filtering applied using the
``startkey`` and ``endkey`` query arguments. For example the following
query:

.. code-block:: http

    GET http://couchdb:5984/recipes/_design/recipes/_view/by_ingredient?startkey=%22carrots%22&endkey=%22egg%22
    Accept: application/json
    Content-Type: application/json

Will operate correctly when listing all the matching entries between
“carrots” and ``egg``. If the order of output is reversed with the
``descending`` query argument, the view request will return no entries:

.. code-block:: http

    GET http://couchdb:5984/recipes/_design/recipes/_view/by_ingredient?descending=true&startkey=%22carrots%22&endkey=%22egg%22
    Accept: application/json
    Content-Type: application/json

The returned result is empty:

.. code-block:: javascript

    {
       "total_rows" : 26453,
       "rows" : [],
       "offset" : 21882
    }

The results will be empty because the entries in the view are reversed
before the key filter is applied, and therefore the ``endkey`` of “egg”
will be seen before the ``startkey`` of “carrots”, resulting in an empty
list.

Instead, you should reverse the values supplied to the ``startkey`` and
``endkey`` parameters to match the descending sorting applied to the
keys. Changing the previous example to:

.. code-block:: http

    GET http://couchdb:5984/recipes/_design/recipes/_view/by_ingredient?descending=true&startkey=%22egg%22&endkey=%22carrots%22
    Accept: application/json
    Content-Type: application/json

Specifying Start and End Values
-------------------------------

.. todo:: Specifying Start and End Values

The ``startkey`` and ``endkey`` query arguments can be used to specify
the range of values to be displayed when querying the view.

Using Limits and Skipping Rows
------------------------------

.. todo:: Using Limits and Skipping Rows

TBC

View Reduction and Grouping
---------------------------

.. todo:: View Reduction and Grouping

TBC

.. _api/ddoc/view.post:

``POST /db/_design/design-doc/_view/view-name``
===============================================

* **Method**: ``POST /db/_design/design-doc/_view/view-name``
* **Request**:  List of keys to be returned from specified view
* **Response**:  JSON of the documents returned by the view
* **Admin Privileges Required**: no
* **Query Arguments**:

  * **Argument**: descending

    * **Description**:  Return the documents in descending by key order
    * **Optional**: yes
    * **Type**: boolean
    * **Default**: false

  * **Argument**: endkey

    * **Description**:  Stop returning records when the specified key is reached
    * **Optional**: yes
    * **Type**: string

  * **Argument**: endkey_docid

    * **Description**:  Stop returning records when the specified document ID
      is reached
    * **Optional**: yes
    * **Type**: string

  * **Argument**: group

    * **Description**:  Group the results using the reduce function to a group
      or single row
    * **Optional**: yes
    * **Type**: boolean
    * **Default**: false

  * **Argument**: group_level

    * **Description**:  Specify the group level to be used
    * **Optional**: yes
    * **Type**: numeric

  * **Argument**: include_docs

    * **Description**:  Include the full content of the documents in the return
    * **Optional**: yes
    * **Type**: boolean
    * **Default**: false

  * **Argument**: inclusive_end

    * **Description**:  Specifies whether the specified end key should be
      included in the result
    * **Optional**: yes
    * **Type**: boolean
    * **Default**: true

  * **Argument**: key

    * **Description**:  Return only documents that match the specified key
    * **Optional**: yes
    * **Type**: string

  * **Argument**: limit

    * **Description**:  Limit the number of the returned documents to the
      specified number
    * **Optional**: yes
    * **Type**: numeric

  * **Argument**: reduce

    * **Description**:  Use the reduction function
    * **Optional**: yes
    * **Type**: boolean
    * **Default**: true

  * **Argument**: skip

    * **Description**:  Skip this number of records before starting to return
      the results
    * **Optional**: yes
    * **Type**: numeric
    * **Default**: 0

  * **Argument**: stale

    * **Description**:  Allow the results from a stale view to be used
    * **Optional**: yes
    * **Type**: string
    * **Default**:
    * **Supported Values**:

      * **ok**: Allow stale views

  * **Argument**: startkey

    * **Description**:  Return records starting with the specified key
    * **Optional**: yes
    * **Type**: string

  * **Argument**: startkey_docid

    * **Description**:  Return records starting with the specified document ID
    * **Optional**: yes
    * **Type**: string

  * **Argument**: update_seq

    * **Description**:  Include the update sequence in the generated results
    * **Optional**: yes
    * **Type**: boolean
    * **Default**: false

Executes the specified ``view-name`` from the specified ``design-doc``
design document. Unlike the ``GET`` method for accessing views, the
``POST`` method supports the specification of explicit keys to be
retrieved from the view results. The remainder of the ``POST`` view
functionality is identical to the :ref:`api/ddoc/view.get` API.

For example, the request below will return all the recipes where the key
for the view matches either “claret” or “clear apple cider” :

.. code-block:: http

    POST http://couchdb:5984/recipes/_design/recipes/_view/by_ingredient
    Content-Type: application/json

    {
       "keys" : [
          "claret",
          "clear apple juice"
       ]
    }


The returned view data contains the standard view information, but only
where the keys match.

.. code-block:: javascript

    {
       "total_rows" : 26484,
       "rows" : [
          {
             "value" : [
                "Scotch collops"
             ],
             "id" : "Scotchcollops",
             "key" : "claret"
          },
          {
             "value" : [
                "Stand pie"
             ],
             "id" : "Standpie",
             "key" : "clear apple juice"
          }
       ],
       "offset" : 6324
    }

Multi-document Fetching
-----------------------

By combining the ``POST`` method to a given view with the
``include_docs=true`` query argument you can obtain multiple documents
from a database. The result is more efficient than using multiple
:ref:`api/doc.get` requests.

For example, sending the following request for ingredients matching
“claret” and “clear apple juice”:

.. code-block:: http

    POST http://couchdb:5984/recipes/_design/recipes/_view/by_ingredient?include_docs=true
    Content-Type: application/json

    {
       "keys" : [
          "claret",
          "clear apple juice"
       ]
    }

Returns the full document for each recipe:

.. code-block:: javascript

    {
       "offset" : 6324,
       "rows" : [
          {
             "doc" : {
                "_id" : "Scotchcollops",
                "_rev" : "1-bcbdf724f8544c89697a1cbc4b9f0178",
                "cooktime" : "8",
                "ingredients" : [
                   {
                      "ingredient" : "onion",
                      "ingredtext" : "onion, peeled and chopped",
                      "meastext" : "1"
                   },
                ...
                ],
                "keywords" : [
                   "cook method.hob, oven, grill@hob",
                   "diet@wheat-free",
                   "diet@peanut-free",
                   "special collections@classic recipe",
                   "cuisine@british traditional",
                   "diet@corn-free",
                   "diet@citrus-free",
                   "special collections@very easy",
                   "diet@shellfish-free",
                   "main ingredient@meat",
                   "occasion@christmas",
                   "meal type@main",
                   "diet@egg-free",
                   "diet@gluten-free"
                ],
                "preptime" : "10",
                "servings" : "4",
                "subtitle" : "This recipe comes from an old recipe book of 1683 called 'The Gentlewoman's Kitchen'. This is an excellent way of making a rich and full-flavoured meat dish in a very short time.",
                "title" : "Scotch collops",
                "totaltime" : "18"
             },
             "id" : "Scotchcollops",
             "key" : "claret",
             "value" : [
                "Scotch collops"
             ]
          },
          {
             "doc" : {
                "_id" : "Standpie",
                "_rev" : "1-bff6edf3ca2474a243023f2dad432a5a",
                "cooktime" : "92",
                "ingredients" : [
    ...            ],
                "keywords" : [
                   "diet@dairy-free",
                   "diet@peanut-free",
                   "special collections@classic recipe",
                   "cuisine@british traditional",
                   "diet@corn-free",
                   "diet@citrus-free",
                   "occasion@buffet party",
                   "diet@shellfish-free",
                   "occasion@picnic",
                   "special collections@lunchbox",
                   "main ingredient@meat",
                   "convenience@serve with salad for complete meal",
                   "meal type@main",
                   "cook method.hob, oven, grill@hob / oven",
                   "diet@cow dairy-free"
                ],
                "preptime" : "30",
                "servings" : "6",
                "subtitle" : "Serve this pie with pickled vegetables and potato salad.",
                "title" : "Stand pie",
                "totaltime" : "437"
             },
             "id" : "Standpie",
             "key" : "clear apple juice",
             "value" : [
                "Stand pie"
             ]
          }
       ],
       "total_rows" : 26484
    }
