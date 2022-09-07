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

=============
Configuration
=============

The CouchDB Server Configuration API provide an interface to query and update
the various configuration values within a running CouchDB instance.

Accessing the local node's configuration
========================================

The literal string ``_local`` serves as an alias for the local node name, so
for all configuration URLs, ``{node-name}`` may be replaced with ``_local``, to
interact with the local node's configuration.

``/_node/{node-name}/_config``
==============================

.. http:get:: /_node/{node-name}/_config
    :synopsis: Obtains a list of the entire server configuration

    Returns the entire CouchDB server configuration as a JSON structure. The
    structure is organized by different configuration sections, with
    individual values.

    :<header Accept: - :mimetype:`application/json`
                     - :mimetype:`text/plain`
    :>header Content-Type: - :mimetype:`application/json`
                           - :mimetype:`text/plain; charset=utf-8`
    :code 200: Request completed successfully
    :code 401: CouchDB Server Administrator privileges required

    **Request**

    .. code-block:: http

        GET /_node/nonode@nohost/_config HTTP/1.1
        Accept: application/json
        Host: localhost:5984

    **Response**:

    .. code-block:: http

        HTTP/1.1 200 OK
        Cache-Control: must-revalidate
        Content-Length: 4148
        Content-Type: application/json
        Date: Sat, 10 Aug 2013 12:01:42 GMT
        Server: CouchDB (Erlang/OTP)

        {
            "attachments": {
                "compressible_types": "text/*, application/javascript, application/json,  application/xml",
                "compression_level": "8"
            },
            "couchdb": {
                "users_db_suffix": "_users",
                "database_dir": "/var/lib/couchdb",
                "max_attachment_chunk_size": "4294967296",
                "max_dbs_open": "100",
                "os_process_timeout": "5000",
                "uri_file": "/var/lib/couchdb/couch.uri",
                "util_driver_dir": "/usr/lib64/couchdb/erlang/lib/couch-1.5.0/priv/lib",
                "view_index_dir": "/var/lib/couchdb"
            },
            "chttpd": {
                "allow_jsonp": "false",
                "backlog": "512",
                "bind_address": "0.0.0.0",
                "port": "5984",
                "require_valid_user": "false",
                "socket_options": "[{sndbuf, 262144}, {nodelay, true}]",
                "server_options": "[{recbuf, undefined}]",
                "secure_rewrites": "true"
            },
            "httpd": {
                "authentication_handlers": "{couch_httpd_auth, cookie_authentication_handler}, {couch_httpd_auth, default_authentication_handler}",
                "bind_address": "192.168.0.2",
                "max_connections": "2048",
                "port": "5984",
            },
            "log": {
                "writer": "file",
                "file": "/var/log/couchdb/couch.log",
                "include_sasl": "true",
                "level": "info"
            },
            "query_server_config": {
                "reduce_limit": "true"
            },
            "replicator": {
                "max_http_pipeline_size": "10",
                "max_http_sessions": "10"
            },
            "stats": {
                "interval": "10"
            },
            "uuids": {
                "algorithm": "utc_random"
            }
        }

.. versionchanged: 2.0.0 The config endpoint from ``/_config`` to
   ``/_node/{node-name}/_config``.

.. _api/config/section:

``/_node/{node-name}/_config/{section}``
========================================

.. http:get:: /_node/{node-name}/_config/{section}
    :synopsis: Returns all the configuration values for the specified section

    Gets the configuration structure for a single section.

    :param section: Configuration section name
    :<header Accept: - :mimetype:`application/json`
                     - :mimetype:`text/plain`
    :>header Content-Type: - :mimetype:`application/json`
                           - :mimetype:`text/plain; charset=utf-8`
    :code 200: Request completed successfully
    :code 401: CouchDB Server Administrator privileges required

    **Request**:

    .. code-block:: http

        GET /_node/nonode@nohost/_config/httpd HTTP/1.1
        Accept: application/json
        Host: localhost:5984

    **Response**:

    .. code-block:: http

        HTTP/1.1 200 OK
        Cache-Control: must-revalidate
        Content-Length: 444
        Content-Type: application/json
        Date: Sat, 10 Aug 2013 12:10:40 GMT
        Server: CouchDB (Erlang/OTP)

        {
            "authentication_handlers": "{couch_httpd_auth, cookie_authentication_handler}, {couch_httpd_auth, default_authentication_handler}",
            "bind_address": "127.0.0.1",
            "default_handler": "{couch_httpd_db, handle_request}",
            "port": "5984"
        }

.. _api/config/section/key:

``/_node/{node-name}/_config/{section}/{key}``
==============================================

.. http:get:: /_node/{node-name}/_config/{section}/{key}
    :synopsis: Returns a specific section/configuration value

    Gets a single configuration value from within a specific configuration
    section.

    :param section: Configuration section name
    :param key: Configuration option name
    :<header Accept: - :mimetype:`application/json`
                     - :mimetype:`text/plain`
    :>header Content-Type: - :mimetype:`application/json`
                           - :mimetype:`text/plain; charset=utf-8`
    :code 200: Request completed successfully
    :code 401: CouchDB Server Administrator privileges required

    **Request**:

    .. code-block:: http

        GET /_node/nonode@nohost/_config/log/level HTTP/1.1
        Accept: application/json
        Host: localhost:5984

    **Response**:

    .. code-block:: http

        HTTP/1.1 200 OK
        Cache-Control: must-revalidate
        Content-Length: 8
        Content-Type: application/json
        Date: Sat, 10 Aug 2013 12:12:59 GMT
        Server: CouchDB (Erlang/OTP)

        "debug"

    .. note::
        The returned value will be the JSON of the value, which may be a string
        or numeric value, or an array or object. Some client environments may
        not parse simple strings or numeric values as valid JSON.

.. http:put:: /_node/{node-name}/_config/{section}/{key}
    :synopsis: Sets the specified configuration value

    Updates a configuration value. The new value should be supplied in the
    request body in the corresponding JSON format. If you are setting a string
    value, you must supply a valid JSON string. In response CouchDB sends old
    value for target section key.

    :param section: Configuration section name
    :param key: Configuration option name
    :<header Accept: - :mimetype:`application/json`
                     - :mimetype:`text/plain`
    :<header Content-Type: :mimetype:`application/json`
    :>header Content-Type: - :mimetype:`application/json`
                           - :mimetype:`text/plain; charset=utf-8`
    :code 200: Request completed successfully
    :code 400: Invalid JSON request body
    :code 401: CouchDB Server Administrator privileges required
    :code 500: Error setting configuration

    **Request**:

    .. code-block:: http

        PUT /_node/nonode@nohost/_config/log/level HTTP/1.1
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
        Server: CouchDB (Erlang/OTP)

        "debug"

.. http:delete:: /_node/{node-name}/_config/{section}/{key}
    :synopsis: Removes the current setting

    Deletes a configuration value. The returned JSON will be the value of the
    configuration parameter before it was deleted.

    :param section: Configuration section name
    :param key: Configuration option name
    :<header Accept: - :mimetype:`application/json`
                     - :mimetype:`text/plain`
    :>header Content-Type: - :mimetype:`application/json`
                           - :mimetype:`text/plain; charset=utf-8`
    :code 200: Request completed successfully
    :code 401: CouchDB Server Administrator privileges required
    :code 404: Specified configuration option not found

    **Request**:

    .. code-block:: http

        DELETE /_node/nonode@nohost/_config/log/level HTTP/1.1
        Accept: application/json
        Host: localhost:5984

    **Response**:

    .. code-block:: http

        HTTP/1.1 200 OK
        Cache-Control: must-revalidate
        Content-Length: 7
        Content-Type: application/json
        Date: Sat, 10 Aug 2013 12:29:03 GMT
        Server: CouchDB (Erlang/OTP)

        "info"

.. _api/config/reload:

``/_node/{node-name}/_config/_reload``
======================================

.. versionadded:: 3.0

.. http:post:: /_node/{node-name}/_config/_reload
    :synopsis: Reload the configuration from disk

    Reloads the configuration from disk. This has a side effect of
    flushing any in-memory configuration changes that have not been
    committed to disk.

    **Request**:

    .. code-block:: http

        POST /_node/nonode@nohost/_config/_reload HTTP/1.1
        Host: localhost:5984

    **Response**:

    .. code-block:: http

        HTTP/1.1 200 OK
        Cache-Control: must-revalidate
        Content-Length: 12
        Content-Type: application/json
        Date: Tues, 21 Jan 2020 11:09:35
        Server: CouchDB/3.0.0 (Erlang OTP)

        {"ok":true}
