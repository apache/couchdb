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

.. _best-practices/iso-date:

============================================
Using an ISO Formatted Date for Document IDs
============================================

The `ISO 8601 date standard <http://en.wikipedia.org/wiki/ISO_8601>`_ describes a useful
scheme for representing a date string in a Year-Month-DayTHour:Minute:Second.microsecond
format. For time-bound documents in a CouchDB database this can be a very handy way to
create a unique identifier, since JavaScript can directly use it to create a Date object.
Using this sample ``map`` function:

.. code-block:: javascript

    function(doc) {
      var dt = new Date(doc._id);
      emit([dt.getDate(), doc.widget], 1);
    }

simply use ``group_level`` to zoom in on whatever time you wish to use.

.. code-block:: bash

    curl -X GET "http://localhost:5984/transactions/_design/widget_count/_view/toss?group_level=1"

    {"rows":[
    {"key":[20],"value":10},
    {"key":[21],"value":20}
    ]}

    curl -X GET "http://localhost:5984/transactions/_design/widget_count/_view/toss?group_level=2"

    {"rows":[
    {"key":[20,widget],"value":10},
    {"key":[21,widget],"value":10},
    {"key":[21,thing],"value":10}
    ]}

Another method is using ``parseint()`` and ``datetime.substr()`` to cut out useful values
for a return key:

.. code-block:: javascript

    function (doc) {
      var datetime = doc._id;
      var year = parseInt(datetime.substr(0, 4));
      var month = parseInt(datetime.substr(5, 2), 10);
      var day = parseInt(datetime.substr(8, 2), 10);
      var hour = parseInt(datetime.substr(11, 2), 10);
      var minute = parseInt(datetime.substr(14, 2), 10);
      emit([doc.widget, year, month, day, hour, minute], 1);
    }
