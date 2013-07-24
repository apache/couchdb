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

.. _changes:

.. _api/db/changes:
.. _api/db/changes.get:

``GET /db/_changes``
====================

* **Method**: ``GET /db/_changes``
* **Request**: None
* **Response**: JSON success statement
* **Admin Privileges Required**: no
* **Query Arguments**:

  * **Argument**: doc_ids

    * **Description**:  Specify the list of documents IDs to be filtered
    * **Optional**: yes
    * **Type**: json
    * **Default**: none

  * **Argument**: feed

    * **Description**: Type of the :ref:`changes <changes>` feed
    * **Optional**: yes
    * **Type**: string
    * **Default**: normal
    * **Supported Values**:

      * **continuous**: :ref:`Continuous <changes/continuous>` mode
      * **eventsource**: :ref:`Event source <changes/eventsource>` mode
      * **longpoll**: :ref:`Long polling <changes/longpoll>` mode
      * **normal**: :ref:`Normal <changes/normal>` mode

  * **Argument**: filter

    * **Description**:  Filter function from a design document to get updates
    * **Optional**: yes
    * **Type**: string
    * **Default**: none
    * **Supported Values**:

  * **Argument**: heartbeat

    * **Description**:  Period after which an empty line is sent during longpoll
      or continuous
    * **Optional**: yes
    * **Type**: numeric
    * **Default**: 60000
    * **Quantity**: milliseconds

  * **Argument**: include_docs

    * **Description**:  Include the document with the result
    * **Optional**: yes
    * **Type**: boolean
    * **Default**: false

  * **Argument**: limit

    * **Description**:  Maximum number of rows rows to return
    * **Optional**: yes
    * **Type**: numeric
    * **Default**: none

  * **Argument**: since

    * **Description**:  Start the results from changes immediately after the
      specified sequence number
    * **Optional**: yes
    * **Type**: numeric
    * **Default**: 0

Obtains a list of the changes made to the database. This can be used to
monitor for update and modifications to the database for post processing
or synchronization.

.. seealso::

   :ref:`Detailed description of available changes feed types <changes>`

The return structure for ``normal`` and ``longpoll`` modes is a JSON
array of changes objects, and the last update sequence number. The
structure is described in the following table.

+----------------------------------+-------------------------------------------+
| Field                            | Description                               |
+==================================+===========================================+
| last_seq                         | Last change sequence number.              |
+----------------------------------+-------------------------------------------+
| results [array]                  | Changes made to a database                |
+----------------------------------+-------------------------------------------+
|         changes [array]          | List of changes, field-by-field, for this |
|                                  | document                                  |
+----------------------------------+-------------------------------------------+
|         id                       | Document ID                               |
+----------------------------------+-------------------------------------------+
|         seq                      | Update sequence number                    |
+----------------------------------+-------------------------------------------+

The return format for ``continuous`` mode the server sends a ``CRLF``
(carriage-return, linefeed) delimited line for each change. Each line
contains the `JSON object`_.

You can also request the full contents of each document change (instead
of just the change notification) by using the ``include_docs``
parameter.

Filtering
---------

You can filter the contents of the changes feed in a number of ways. The
most basic way is to specify one or more document IDs to the query. This
causes the returned structure value to only contain changes for the
specified IDs. Note that the value of this query argument should be a
JSON formatted array.

You can also filter the ``_changes`` feed by defining a filter function
within a design document. The specification for the filter is the same
as for replication filters. You specify the name of the filter function
to the ``filter`` parameter, specifying the design document name and
filter name. For example:

.. code-block:: http

    GET /db/_changes?filter=design_doc/filtername

The ``_changes`` feed can be used to watch changes to specific document
ID's or the list of ``_design`` documents in a database. If the
``filters`` parameter is set to ``_doc_ids`` a list of doc IDs can be
passed in the ``doc_ids`` parameter as a JSON array. For more
information, see :ref:`changes`.
