.. Licensed under the Apache License, Version 2.0 (the "License")you may not
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

.. _config/compaction:

``[compaction]`` :: Compaction Daemon Rules
===========================================

List of compaction rules for the
:ref:`compaction daemon <config/daemons/compaction_daemon>`.
The daemon compacts databases and their respective view groups when all the
condition parameters are satisfied. Configuration can be per database or
global, and it has the following format::

  [compaction]
  database_name = [ {ParamName, ParamValue}, {ParamName, ParamValue}, ... ]
  _default = [ {ParamName, ParamValue}, {ParamName, ParamValue}, ... ]


For example::

  [compaction]
  _default = [{db_fragmentation, "70%"}, {view_fragmentation, "60%"}, {from, "23:00"}, {to, "04:00"}]

Possible parameters:

- ``db_fragmentation``: If the ratio (as an integer percentage), of the amount
  of old data (and its supporting metadata) over the database file size is equal
  to or greater then this value, this database compaction condition is
  satisfied. This value is computed as::

    (file_size - data_size) / file_size * 100

  The data_size and file_size values can be obtained when
  querying a :ref:`database's information URI <api/db.get>`.

- ``view_fragmentation``: If the ratio (as an integer percentage), of the amount
  of old data (and its supporting metadata) over the view index (view group)
  file size is equal to or greater then this value, then this view index
  compaction condition is satisfied. This value is computed as::

    (file_size - data_size) / file_size * 100

  The data_size and file_size values can be obtained when querying a
  :ref:`view group's information URI <api/ddoc/info>`.

- ``from`` and ``to``: The period for which a database (and its view groups)
  compaction is allowed. The value for these parameters must obey the format::

    HH:MM - HH:MM  (HH in [0..23], MM in [0..59])

- ``strict_window``: If a compaction is still running after the end of the
  allowed period, it will be canceled if this parameter is set to `true`.
  It defaults to `false` and it's meaningful only if the *period* parameter is
  also specified.

- ``parallel_view_compaction``: If set to `true`, the database and its views are
  compacted in parallel. This is only useful on certain setups, like for example
  when the database and view index directories point to different disks.
  It defaults to `false`.

Before a compaction is triggered, an estimation of how much free disk space is
needed is computed. This estimation corresponds to 2 times the data size of
the database or view index. When there's not enough free disk space to compact
a particular database or view index, a warning message is logged.

Examples:

#. ``[{db_fragmentation, "70%"}, {view_fragmentation, "60%"}]``

   The `foo` database is compacted if its fragmentation is 70% or more.
   Any view index of this database is compacted only if its fragmentation
   is 60% or more.

#. ``[{db_fragmentation, "70%"}, {view_fragmentation, "60%"}, {from, "00:00"}, {to, "04:00"}]``

   Similar to the preceding example but a compaction (database or view index)
   is only triggered if the current time is between midnight and 4 AM.

#. ``[{db_fragmentation, "70%"}, {view_fragmentation, "60%"}, {from, "00:00"}, {to, "04:00"}, {strict_window, true}]``

   Similar to the preceding example - a compaction (database or view index)
   is only triggered if the current time is between midnight and 4 AM. If at
   4 AM the database or one of its views is still compacting, the compaction
   process will be canceled.

#. ``[{db_fragmentation, "70%"}, {view_fragmentation, "60%"}, {from, "00:00"}, {to, "04:00"}, {strict_window, true}, {parallel_view_compaction, true}]``

   Similar to the preceding example, but a database and its views can be
   compacted in parallel.


.. _config/compaction_daemon:

``[compaction_daemon]`` :: Configuration of Compaction Daemon
=============================================================

These options are under ``[compaction_daemon]`` section and belong to
:ref:`compaction daemon <config/daemons/compaction_daemon>`.


.. _config/compaction_daemon/check_interval:

``check_interval``
------------------

The delay, in seconds, between each check for which database and view indexes
need to be compacted::

  [compaction_daemon]
  check_interval = 300


.. _config/compaction_daemon/min_file_size:

``min_file_size``
-----------------

If a database or view index file is smaller then this value (in bytes),
compaction will not happen. Very small files always have a very high
fragmentation therefore it's not worth to compact them::

  [compaction_daemon]
  min_file_size = 131072


