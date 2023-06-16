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

.. default-domain:: config
.. highlight:: ini

==========================
Disk Monitor Configuration
==========================

Apache CouchDB can react proactively when disk space gets low.

.. _config/disk_monitor:

Disk Monitor Options
====================

.. config:section:: disk_monitor :: Disk Monitor Options

  .. versionadded:: 3.4

    .. config:option:: background_view_indexing_threshold

        The percentage of used disk space on the ``view_index_dir`` above
        which CouchDB will no longer start background view indexing jobs.
        Defaults to ``80``. ::

            [disk_monitor]
            background_view_indexing_threshold = 80

    .. config:option:: interactive_database_writes_threshold

        The percentage of used disk space on the ``database_dir`` above
        which CouchDB will no longer allow interactive document updates
        (writes or deletes).

        Replicated updates and database deletions are still permitted.

        In a clustered write an error will be returned if
        enough nodes are above the ``interactive_database_writes_threshold``.

        Defaults to ``90``. ::

            [disk_monitor]
            interactive_database_writes_threshold = 90

    .. config:option:: enable :: enable disk monitoring

        Enable disk monitoring subsystem. Defaults to ``false``. ::

            [disk_monitor]
            enable = false

    .. config:option:: interactive_view_indexing_threshold

        The percentage of used disk space on the ``view_index_dir`` above
        which CouchDB will no longer update stale view indexes when queried.

        View indexes that are already up to date can still be queried, and stale
        view indexes can be queried if either ``stale=ok`` or ``update=false`` are
        set.

        Attempts to query a stale index without either parameter will yield a
        ``507 Insufficient Storage`` error. Defaults to ``90``. ::

            [disk_monitor]
            interactive_view_indexing_threshold = 90
