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

.. default-domain:: config

.. highlight:: ini

========================
Compaction Configuration
========================

.. _conifg/database_compaction:

Database Compaction Options
===========================

.. config:section:: database_compaction :: Database Compaction Options

  .. config:option:: doc_buffer_size :: Documents buffer size

    Specifies the copy buffer's maximum size in bytes::

      [database_compaction]
      doc_buffer_size = 524288


  .. config:option:: checkpoint_after :: Checkpoint trigger

    Triggers a checkpoint after the specified amount of bytes were successfully
    copied to the compacted database::

      [database_compaction]
      checkpoint_after = 5242880


.. _config/compactions:

Compaction Daemon Rules
=======================

.. config:section:: compactions :: Compaction Daemon Rules

  A list of rules to determine when to run automatic compaction. The
  :option:`daemons/compaction_daemon` compacts databases and their respective
  view groups when all the condition parameters are satisfied. Configuration
  can be per-database or global, and it has the following format::

    [compactions]
    database_name = [ {ParamName, ParamValue}, {ParamName, ParamValue}, ... ]
    _default = [ {ParamName, ParamValue}, {ParamName, ParamValue}, ... ]


  For example::

    [compactions]
    _default = [{db_fragmentation, "70%"}, {view_fragmentation, "60%"}, {from, "23:00"}, {to, "04:00"}]

  - ``db_fragmentation``: If the ratio of legacy data, including metadata, to
    current data in the database file size is equal to or greater than this
    value, this condition is satisfied. The percentage is expressed as an
    integer percentage. This value is computed as::

      (file_size - data_size) / file_size * 100

    The data_size and file_size values can be obtained when
    querying :http:get:`/{db}`.

  - ``view_fragmentation``: If the ratio of legacy data, including metadata, to
    current data in a view index file size is equal to or greater then this
    value, this database compaction condition is satisfied. The percentage is
    expressed as an integer percentage. This value is computed as::

      (file_size - data_size) / file_size * 100

    The data_size and file_size values can be obtained when querying a
    :ref:`view group's information URI <api/ddoc/info>`.

  - ``from`` and ``to``: The period for which a database (and its view group)
    compaction is allowed. The value for these parameters must obey the format::

      HH:MM - HH:MM  (HH in [0..23], MM in [0..59])

  - ``strict_window``: If a compaction is still running after the end of the
    allowed period, it will be canceled if this parameter is set to `true`.
    It defaults to `false` and is meaningful only if the *period* parameter is
    also specified.

  - ``parallel_view_compaction``: If set to `true`, the database and its views
    are compacted in parallel. This is only useful on certain setups, like
    for example when the database and view index directories point to different
    disks. It defaults to `false`.

  Before a compaction is triggered, an estimation of how much free disk space is
  needed is computed. This estimation corresponds to two times the data size of
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

Configuration of Compaction Daemon
==================================

.. config:section:: compaction_daemon :: Configuration of Compaction Daemon

  .. config:option:: check_interval

    The delay, in seconds, between each check for which database and view
    indexes need to be compacted::

      [compaction_daemon]
      check_interval = 300


  .. config:option:: min_file_size

    If a database or view index file is smaller than this value (in bytes),
    compaction will not happen. Very small files always have high fragmentation,
    so compacting them is inefficient.

    ::

      [compaction_daemon]
      min_file_size = 131072


.. _config/view_compaction:

Views Compaction Options
========================

.. config:section:: view_compaction :: Views Compaction Options


  .. config:option:: keyvalue_buffer_size :: Key-Values buffer size

    Specifies maximum copy buffer size in bytes used during compaction::

      [view_compaction]
      keyvalue_buffer_size = 2097152
