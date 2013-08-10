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

.. _api/config:

=====================
Configuration Methods
=====================

The CouchDB API Server Configuration Methods provide an interface to
query and update the various configuration values within a running
CouchDB instance.

A list of the available methods and URL paths are provided below:

+--------+-------------------------+-------------------------------------------+
| Method | Path                    | Description                               |
+========+=========================+===========================================+
| GET    | /_config                | Obtain a list of the entire server        |
|        |                         | configuration                             |
+--------+-------------------------+-------------------------------------------+
| GET    | /_config/section        | Get all the configuration values for the  |
|        |                         | specified section                         |
+--------+-------------------------+-------------------------------------------+
| GET    | /_config/section/key    | Get a specific section/configuration value|
+--------+-------------------------+-------------------------------------------+
| PUT    | /_config/section/key    | Set the specified configuration value     |
+--------+-------------------------+-------------------------------------------+
| DELETE | /_config/section/key    | Delete the current setting                |
+--------+-------------------------+-------------------------------------------+

``/_config``
============

.. http:get:: /_config

  Returns the entire CouchDB server configuration as a JSON structure. The
  structure is organized by different configuration sections, with
  individual values.

  :code 200: Request completed successfully
  :code 401: Administrator's privileges required

  **Request**

  .. code-block:: http

    GET /_config HTTP/1.1
    Accept: application/json
    Host: localhost:5984

  **Response**:

  .. code-block:: http

    HTTP/1.1 200 OK
    Cache-Control: must-revalidate
    Content-Length: 4148
    Content-Type: application/json
    Date: Sat, 10 Aug 2013 12:01:42 GMT
    Server: CouchDB/1.4.0 (Erlang OTP/R16B)

    {
      "attachments": {
          "compressible_types": "text/*, application/javascript, application/json,  application/xml",
          "compression_level": "8"
      },
      "couch_httpd_auth": {
          "auth_cache_size": "50",
          "authentication_db": "_users",
          "authentication_redirect": "/_utils/session.html",
          "require_valid_user": "false",
          "timeout": "600"
      },
      "couchdb": {
          "database_dir": "/var/lib/couchdb",
          "delayed_commits": "true",
          "max_attachment_chunk_size": "4294967296",
          "max_dbs_open": "100",
          "max_document_size": "4294967296",
          "os_process_timeout": "5000",
          "uri_file": "/var/lib/couchdb/couch.uri",
          "util_driver_dir": "/usr/lib64/couchdb/erlang/lib/couch-1.0.1/priv/lib",
          "view_index_dir": "/var/lib/couchdb"
      },
      "daemons": {
          "auth_cache": "{couch_auth_cache, start_link, []}",
          "db_update_notifier": "{couch_db_update_notifier_sup, start_link, []}",
          "external_manager": "{couch_external_manager, start_link, []}",
          "httpd": "{couch_httpd, start_link, []}",
          "query_servers": "{couch_query_servers, start_link, []}",
          "stats_aggregator": "{couch_stats_aggregator, start, []}",
          "stats_collector": "{couch_stats_collector, start, []}",
          "uuids": "{couch_uuids, start, []}",
          "view_manager": "{couch_view, start_link, []}"
      },
      "httpd": {
          "allow_jsonp": "false",
          "authentication_handlers": "{couch_httpd_oauth, oauth_authentication_handler}, {couch_httpd_auth, cookie_authentication_handler}, {couch_httpd_auth, default_authentication_handler}",
          "bind_address": "192.168.0.2",
          "default_handler": "{couch_httpd_db, handle_request}",
          "max_connections": "2048",
          "port": "5984",
          "secure_rewrites": "true",
          "vhost_global_handlers": "_utils, _uuids, _session, _oauth, _users"
      },
      "httpd_db_handlers": {
          "_changes": "{couch_httpd_db, handle_changes_req}",
          "_compact": "{couch_httpd_db, handle_compact_req}",
          "_design": "{couch_httpd_db, handle_design_req}",
          "_temp_view": "{couch_httpd_view, handle_temp_view_req}",
          "_view_cleanup": "{couch_httpd_db, handle_view_cleanup_req}"
      },
      "httpd_design_handlers": {
          "_info": "{couch_httpd_db,   handle_design_info_req}",
          "_list": "{couch_httpd_show, handle_view_list_req}",
          "_rewrite": "{couch_httpd_rewrite, handle_rewrite_req}",
          "_show": "{couch_httpd_show, handle_doc_show_req}",
          "_update": "{couch_httpd_show, handle_doc_update_req}",
          "_view": "{couch_httpd_view, handle_view_req}"
      },
      "httpd_global_handlers": {
          "/": "{couch_httpd_misc_handlers, handle_welcome_req, <<\"Welcome\">>}",
          "_active_tasks": "{couch_httpd_misc_handlers, handle_task_status_req}",
          "_all_dbs": "{couch_httpd_misc_handlers, handle_all_dbs_req}",
          "_config": "{couch_httpd_misc_handlers, handle_config_req}",
          "_log": "{couch_httpd_misc_handlers, handle_log_req}",
          "_oauth": "{couch_httpd_oauth, handle_oauth_req}",
          "_replicate": "{couch_httpd_misc_handlers, handle_replicate_req}",
          "_restart": "{couch_httpd_misc_handlers, handle_restart_req}",
          "_session": "{couch_httpd_auth, handle_session_req}",
          "_stats": "{couch_httpd_stats_handlers, handle_stats_req}",
          "_utils": "{couch_httpd_misc_handlers, handle_utils_dir_req, \"/usr/share/couchdb/www\"}",
          "_uuids": "{couch_httpd_misc_handlers, handle_uuids_req}",
          "favicon.ico": "{couch_httpd_misc_handlers, handle_favicon_req, \"/usr/share/couchdb/www\"}"
      },
      "log": {
          "file": "/var/log/couchdb/couch.log",
          "include_sasl": "true",
          "level": "info"
      },
      "query_server_config": {
          "reduce_limit": "true"
      },
      "query_servers": {
          "javascript": "/usr/bin/couchjs /usr/share/couchdb/server/main.js"
      },
      "replicator": {
          "max_http_pipeline_size": "10",
          "max_http_sessions": "10"
      },
      "stats": {
          "rate": "1000",
          "samples": "[0, 60, 300, 900]"
      },
      "uuids": {
          "algorithm": "utc_random"
      }
    }
        

.. _api/config/section:

``/_config/section``
====================

.. http:get:: /_config/{section}

  Gets the configuration structure for a single section.

  :code 200: Request completed successfully
  :code 401: Administrator's privileges required

  **Request**:

  .. code-block:: http

    GET /_config/httpd HTTP/1.1
    Accept: application/json
    Host: localhost:5984

  **Response**:

  .. code-block:: http

    HTTP/1.1 200 OK
    Cache-Control: must-revalidate
    Content-Length: 444
    Content-Type: application/json
    Date: Sat, 10 Aug 2013 12:10:40 GMT
    Server: CouchDB/1.4.0+build.c843cef (Erlang OTP/R16B)

    {
        "allow_jsonp": "false",
        "authentication_handlers": "{couch_httpd_oauth, oauth_authentication_handler}, {couch_httpd_auth, cookie_authentication_handler}, {couch_httpd_auth, default_authentication_handler}",
        "bind_address": "127.0.0.1",
        "default_handler": "{couch_httpd_db, handle_request}",
        "enable_cors": "false",
        "log_max_chunk_size": "1000000",
        "port": "5984",
        "secure_rewrites": "true",
        "vhost_global_handlers": "_utils, _uuids, _session, _oauth, _users"
    }


.. _api/config/section/key:

``/_config/section/key``
========================

.. http:get:: /_config/{section}/{key}

  Gets a single configuration value from within a specific configuration
  section.

  :code 200: Request completed successfully
  :code 401: Administrator's privileges required

  **Request**:

  .. code-block:: http

    GET /_config/log/level HTTP/1.1
    Accept: application/json
    Host: localhost:5984

  **Response**:

  .. code-block:: http

    HTTP/1.1 200 OK
    Cache-Control: must-revalidate
    Content-Length: 8
    Content-Type: application/json
    Date: Sat, 10 Aug 2013 12:12:59 GMT
    Server: CouchDB/1.3.1 (Erlang OTP/R15B02)

    "debug"


  .. note::
     The returned value will be the JSON of the value, which may be a
     string or numeric value, or an array or object. Some client
     environments may not parse simple strings or numeric values as valid JSON.


.. http:put:: /_config/{section}/{key}

  Updates a configuration value. The new value should be supplied in the
  request body in the corresponding JSON format. If you are setting a string
  value, you must supply a valid JSON string. In response CouchDB sends old
  value for target section key.

  :reqheader Content-Type: :mimetype:`application/json`
  :code 200: Request completed successfully
  :code 400: Invalid JSON request body
  :code 401: Administrator's privileges required
  :code 500: Error setting configuration

  **Request**:

  .. code-block:: http

    PUT /_config/log/level HTTP/1.1
    Accept: application/json
    Content-Length: 7
    Content-Type: application/json
    Host: localhost:5984

    "info"

  **Response**:

  .. code-block:: http

    HTTP/1.1 200 OK
    Cache-Control: must-revalidate
    Content-Length: 8
    Content-Type: application/json
    Date: Sat, 10 Aug 2013 12:12:59 GMT
    Server: CouchDB/1.3.1 (Erlang OTP/R15B02)

    "debug"


.. http:delete:: /_config/{section}/{key}

  Deletes a configuration value. The returned JSON will be the value of
  the configuration parameter before it was deleted.

  :code 200: Request completed successfully
  :code 401: Administrator's privileges required
  :code 404: Specified configuration option not found

  **Request**:

  .. code-block:: http

    DELETE /_config/log/level HTTP/1.1
    Accept: application/json
    Host: localhost:5984

  **Response**:

  .. code-block:: http

    HTTP/1.1 200 OK
    Cache-Control: must-revalidate
    Content-Length: 7
    Content-Type: application/json
    Date: Sat, 10 Aug 2013 12:29:03 GMT
    Server: CouchDB/1.3.1 (Erlang OTP/R15B02)

    "info"
