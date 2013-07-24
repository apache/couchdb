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

======================
HTTP Resource Handlers
======================

.. _config/httpd_global_handlers:

``[httpd_global_handlers]`` :: Global HTTP Handlers
===================================================

These HTTP resources are provided for CouchDB server root level.

.. _config/httpd_global_handlers/root:

:ref:`/ <api/server/root>`
--------------------------

::

  [httpd_global_handlers]
  / = {couch_httpd_misc_handlers, handle_welcome_req, <<"Welcome">>}


.. _config/httpd_global_handlers/favicon.ico:

:ref:`favicon.ico <api/server/favicon>`
---------------------------------------

The favicon handler looks for `favicon.ico` file within specified directory::

  [httpd_global_handlers]
  favicon.ico = {couch_httpd_misc_handlers, handle_favicon_req, "/usr/share/couchdb/www"}


.. _config/httpd_global_handlers/_active_tasks:

:ref:`_active_tasks <api/server/active_tasks>`
----------------------------------------------

::

  [httpd_global_handlers]
  _active_tasks = {couch_httpd_misc_handlers, handle_task_status_req}


.. _config/httpd_global_handlers/_all_dbs:

:ref:`_all_dbs <api/server/all_dbs>`
------------------------------------

Provides list of all server's databases::

  [httpd_global_handlers]
  _all_dbs = {couch_httpd_misc_handlers, handle_all_dbs_req}

.. note::

   Sometimes you don't want to disclose database names for everyone, but you
   also don't like/want/able to setup any proxies in front of CouchDB. Removing
   this handler disables ``_all_dbs`` resource and there will be no way to get
   list of available databases.

   Same also is true for other resource handlers.


.. _config/httpd_global_handlers/_config:

:ref:`_config <api/config>`
---------------------------

Provides resource to work with CouchDB config :ref:`remotely <api/config>`.
Any config changes that was made via HTTP API are applied automatically on fly
and doesn't requires server instance to be restarted::

  [httpd_global_handlers]
  _config = {couch_httpd_misc_handlers, handle_config_req}


.. _config/httpd_global_handlers/_log:

:ref:`_log <api/server/log>`
----------------------------

::

  [httpd_global_handlers]
  _log = {couch_httpd_misc_handlers, handle_log_req}


.. _config/httpd_global_handlers/_oauth:

``_oauth``
----------

::

  [httpd_global_handlers]
  _oauth = {couch_httpd_oauth, handle_oauth_req}


.. _config/httpd_global_handlers/_replicate:

:ref:`_replicate <api/server/replicate>`
----------------------------------------

Provides API to run :ref:`temporary replications <api/server/replicate>`::

  [httpd_global_handlers]
  _replicate = {couch_replicator_httpd, handle_req}


.. _config/httpd_global_handlers/_restart:

:ref:`_restart <api/server/restart>`
------------------------------------

::

  [httpd_global_handlers]
  _restart = {couch_httpd_misc_handlers, handle_restart_req}


.. _config/httpd_global_handlers/_session:

``_session``
------------

Provides resource with information about current user's session::

  [httpd_global_handlers]
  _session = {couch_httpd_auth, handle_session_req}


.. _config/httpd_global_handlers/_stats:

:ref:`_stats <api/server/stats>`
--------------------------------

::

  [httpd_global_handlers]
  _stats = {couch_httpd_stats_handlers, handle_stats_req}


.. _config/httpd_global_handlers/_utils:

:ref:`_utils <api/server/utils>`
--------------------------------

The :ref:`_utils <api/server/utils>` handler serves `Futon`'s web administration
page::

  [httpd_global_handlers]
  _utils = {couch_httpd_misc_handlers, handle_utils_dir_req, "/usr/share/couchdb/www"}

In similar way, you may setup custom handler to let CouchDB serve on disk static
files.


.. _config/httpd_global_handlers/_uuids:

:ref:`_uuids <api/server/uuids>`
--------------------------------

Provides resource to get UUIDs generated on server side::

  [httpd_global_handlers]
  _uuids = {couch_httpd_misc_handlers, handle_uuids_req}


.. _config/httpd_db_handlers:

``[httpd_db_handlers]`` :: Database HTTP Handlers
=================================================

These HTTP resources are provided for CouchDB database level in context of the
related one.

.. _config/httpd_db_handlers/_all_docs:

:ref:`_all_docs <api/db/all_docs>`
----------------------------------

::

  [httpd_db_handlers]
  _all_docs = {couch_mrview_http, handle_all_docs_req}


.. _config/httpd_db_handlers/_changes:

:ref:`_changes <changes>`
-------------------------

::

  [httpd_db_handlers]
  _changes = {couch_httpd_db, handle_changes_req}


.. _config/httpd_db_handlers/_compact:

:ref:`_compact <api/db/compact>`
--------------------------------

::

  [httpd_db_handlers]
  _compact = {couch_httpd_db, handle_compact_req}


.. _config/httpd_db_handlers/_design:

:ref:`_design <api/ddoc>`
-------------------------

::

  [httpd_db_handlers]
  _design = {couch_httpd_db, handle_design_req}


.. _config/httpd_db_handlers/_temp_view:

:ref:`_temp_view <api/db/temp_view>`
------------------------------------

::

  [httpd_db_handlers]
  _temp_view = {couch_mrview_http, handle_temp_view_req}


.. _config/httpd_db_handlers/_view_cleanup:

:ref:`_view_cleanup <api/db/view_cleanup>`
------------------------------------------

::

  [httpd_db_handlers]
  _view_cleanup = {couch_mrview_http, handle_cleanup_req}


.. _config/httpd_design_handlers:

``[httpd_design_handlers]`` :: Design Documents HTTP Handlers
=============================================================

These HTTP resources are provided for design documents.

.. _config/httpd_design_handlers/_compact:

:ref:`_compact <api/db/compact/ddoc>`
-------------------------------------

::

  [httpd_design_handlers]
  _compact = {couch_mrview_http, handle_compact_req}


.. _config/httpd_design_handlers/_info:

:ref:`_info <api/ddoc/info>`
----------------------------

::

  [httpd_design_handlers]
  _info = {couch_mrview_http, handle_info_req}


.. _config/httpd_design_handlers/_list:

:ref:`_list <api/ddoc/list>`
----------------------------

::

  [httpd_design_handlers]
  _list = {couch_mrview_show, handle_view_list_req}


.. _config/httpd_design_handlers/_rewrite:

:ref:`_rewrite <api/ddoc/rewrite>`
----------------------------------

::

  [httpd_design_handlers]
  _rewrite = {couch_httpd_rewrite, handle_rewrite_req}


.. _config/httpd_design_handlers/_show:

:ref:`_show <api/ddoc/show>`
----------------------------

::

  [httpd_design_handlers]
  _show = {couch_mrview_show, handle_doc_show_req}


.. _config/httpd_design_handlers/_update:

:ref:`_update <api/ddoc/update>`
--------------------------------

::

  [httpd_design_handlers]
  _update = {couch_mrview_show, handle_doc_update_req}


.. _config/httpd_design_handlers/_view:

:ref:`_view <api/ddoc/view>`
----------------------------

::

  [httpd_design_handlers]
  _view = {couch_mrview_http, handle_view_req}

