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

.. _config/log:

``[log]`` :: Logging options
============================

CouchDB logging configuration.

.. _config/log/file:

``file`` :: Logging file path
-----------------------------

Specifies the location of file for logging output::

  [log]
  file = /var/log/couchdb/couch.log

This path should be readable and writable for user that runs CouchDB service
(`couchdb` by default).

.. _config/log/level:

``level`` :: Logging verbose level
----------------------------------

.. versionchanged:: 1.3: Added ``warning`` level.

Logging level defines how verbose and detailed logging will be::

  [log]
  level = info

Available levels:

- ``debug``: Very informative and detailed debug logging. Includes HTTP headers,
  external processes communications, authorization information and more;
- ``info``: Informative logging. Includes HTTP requests headlines, startup of
  external processes etc.
- ``warning``: Warning messages are alerts about edge situations that may lead
  to errors. For instance, compaction daemon alerts about low or insufficient
  disk space at this level.
- ``error``: Error level includes only things that going wrong, crush reports
  and HTTP error responses (5xx codes).
- ``none``: Disables logging any messages.

.. _config/log/include_sasl:

``include_sasl``
----------------

Includes `SASL`_ information in logs::

  [log]
  include_sasl = true

.. _SASL: http://www.erlang.org/doc/apps/sasl/


.. _config/log_level_by_module:

``[log_level_by_module]`` :: Per module logging
===============================================

.. versionadded:: 1.3

In this section you can specify :ref:`log level <config/log/level>` on a
per-module basis::

  [log_level_by_module]
  couch_httpd = debug
  couch_replicator = info
  couch_query_servers = error

See `src/*/*.erl`_ for available modules.

.. _src/*/*.erl: https://git-wip-us.apache.org/repos/asf?p=couchdb.git;a=tree;f=src;hb=HEAD
