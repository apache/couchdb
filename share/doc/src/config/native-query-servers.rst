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

.. highlight:: ini

.. _config/native_query_servers:

``[native_query_servers]`` :: Native Erlang Query Server
========================================================

.. warning::

   Due to security restrictions, the Erlang query server is disabled by
   default.

   Unlike the JavaScript query server, the Erlang one does not runs in a sandbox
   mode. This means that Erlang code has full access to your OS,
   filesystem and network, which may lead to security issues. While Erlang
   functions are faster than JavaScript ones, you need to be careful
   about running them, especially if they were written by someone else.

CouchDB has a native Erlang query server, allowing you to write your map/reduce
functions in Erlang. There is need no longer to manually install `erlview`_,
unless you are running an old version (pre 0.10) of CouchDB.

First, you'll need to edit your `local.ini` to include a
``[native_query_servers]`` section::

  [native_query_servers]
  erlang = {couch_native_process, start_link, []}

To see these changes you will also need to restart the server.
To test out using :ref:`Erlang views <queryserver_erlang>`, visit the
`Futon` admin interface, create a new database and open a temporary view.
You should now be able to select ``erlang`` from the language drop-down.

Let's try an example of map/reduce functions which count the total documents at
each number of revisions (there are x many documents at version "1", and y
documents at "2"... etc). Add a few documents to the database, then enter the
following functions as a temporary view:

.. code-block:: erlang

  %% Map Function
  fun({Doc}) ->
    <<K,_/binary>> = proplists:get_value(<<"_rev">>, Doc, null),
    V = proplists:get_value(<<"_id">>, Doc, null),
    Emit(<<K>>, V)
  end.

  %% Reduce Function
  fun(Keys, Values, ReReduce) -> length(Values) end.

If all has gone well, after running the view you should see a list of the total
number of documents at each revision number.

.. _erlview: https://github.com/mmcdanie/erlview
