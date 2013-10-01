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

=========================
CouchDB Internal Services
=========================


.. _config/daemons:

CouchDB Daemonized Mini Apps
============================

.. config:section:: daemons :: CouchDB Daemonized Mini Apps


  .. config:option:: auth_cache

    This daemon provides authentication caching to avoid repeated opening and
    closing of the `_users` database for each request requiring authentication::

      [daemons]
      auth_cache={couch_auth_cache, start_link, []}



  .. config:option:: compaction_daemon

    :ref:`Automatic compaction <config/compactions>` daemon::

      [daemons]
      compaction_daemon={couch_compaction_daemon, start_link, []}



  .. config:option:: external_manager

    `External` processes manager::

      [daemons]
      external_manager={couch_external_manager, start_link, []}



  .. config:option:: httpd

    HTTP server daemon::

      [daemons]
      httpd={couch_httpd, start_link, []}



  .. config:option:: httpsd

    Provides :ref:`SSL support <config/ssl>`. The default ssl port CouchDB
    listens on is `6984`::

      [daemons]
      httpsd = {couch_httpd, start_link, [https]}




  .. config:option:: index_server

    The `couch_index` application is responsible for managing all of the
    different types of indexers. This manages the process handling for
    keeping track of the index state as well as managing the updater and
    compactor handling::

      [daemons]
      index_server={couch_index_server, start_link, []}



  .. config:option:: os_daemons

    :ref:`OS Daemons <config/os_daemons>` manager::

      [daemons]
      os_daemons={couch_os_daemons, start_link, []}



  .. config:option:: query_servers

    :ref:`Query servers <config/query_servers>` manager::

      [daemons]
      query_servers={couch_query_servers, start_link, []}



  .. config:option:: replicator_manager

    Replications manager::

      [daemons]
      replicator_manager={couch_replicator_manager, start_link, []}



  .. config:option:: stats_aggregator

    Runtime statistics aggregator::

      [daemons]
      stats_aggregator={couch_stats_aggregator, start, []}



  .. config:option:: stats_collector

    Runtime statistics collector::

      [daemons]
      stats_collector={couch_stats_collector, start, []}



  .. config:option:: uuids

    :ref:`UUIDs <config/uuids>` generator daemon::

      [daemons]
      uuids={couch_uuids, start, []}



  .. config:option:: vhosts

    :ref:`Virtual hosts <config/vhosts>` manager. Provides dynamic add of vhosts
    without restart, wildcards support and dynamic routing via pattern matching
    ::

      [daemons]
      vhosts={couch_httpd_vhost, start_link, []}

