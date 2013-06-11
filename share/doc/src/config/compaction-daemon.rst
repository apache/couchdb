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

