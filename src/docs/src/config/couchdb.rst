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

==================
Base Configuration
==================

.. _config/couchdb:

Base CouchDB Options
====================

.. config:section:: couchdb :: Base CouchDB Options

    .. config:option:: attachment_stream_buffer_size :: Attachment streaming buffer

        Higher values may result in better read performance due to fewer read
        operations and/or more OS page cache hits. However, they can also
        increase overall response time for writes when there are many
        attachment write requests in parallel. ::

            [couchdb]
            attachment_stream_buffer_size = 4096

    .. config:option:: database_dir :: Databases location directory

        Specifies location of CouchDB database files (``*.couch`` named). This
        location should be writable and readable for the user the CouchDB
        service runs as (``couchdb`` by default). ::

            [couchdb]
            database_dir = /var/lib/couchdb

    .. config:option:: default_security :: Default security

        .. versionchanged:: 3.0 ``admin_only`` is now the default.

        Default security object for databases if not explicitly set. When set
        to ``everyone``, anyone can performs reads and writes. When set to
        ``admin_only``, only admins can read and write. When set to
        ``admin_local``, sharded databases can be read and written by anyone
        but the shards can only be read and written by admins. ::

            [couchdb]
            default_security = admin_only

    .. config:option:: enable_database_recovery :: Enable database recovery

        Enable this to only "soft-delete" databases when
        :ref:`DELETE /{db} <api/db>` DELETE  requests are made. This will
        rename all shards of the database with a suffix of the form
        ``<dbname>.YMD.HMS.deleted.couchdb``. You can then manually delete these
        files later, as desired.

        Default is ``false``. ::

            [couchdb]
            enable_database_recovery = false

    .. config:option:: file_compression :: Compression method for documents

        .. versionchanged:: 1.2 Added `Google Snappy`_ compression algorithm.

        Method used to compress everything that is appended to database and
        view index files, except for attachments (see the
        :section:`attachments` section). Available methods are:

        * ``none``: no compression
        * ``snappy``: use Google Snappy, a very fast compressor/decompressor
        * ``deflate_N``: use zlib's deflate; ``N`` is the compression level
          which ranges from ``1`` (fastest, lowest compression ratio) to ``9``
          (slowest, highest compression ratio)

        ::

            [couchdb]
            file_compression = snappy

        .. _Google Snappy: http://code.google.com/p/snappy/

    .. config:option:: maintenance_mode :: Maintenance mode

        A CouchDB node may be put into two distinct maintenance modes by setting
        this configuration parameter.

        * ``true``: The node will not respond to clustered requests from other
          nodes and the /_up endpoint will return a 404 response.
        * ``nolb``: The /_up endpoint will return a 404 response.
        * ``false``: The node responds normally, /_up returns a 200 response.

        It is expected that the administrator has configured a load balancer
        in front of the CouchDB nodes in the cluster. This load balancer should
        use the /_up endpoint to determine whether or not to send HTTP requests
        to any particular node. For HAProxy, the following config is
        appropriate:

        .. code-block:: none

          http-check disable-on-404
          option httpchk GET /_up

    .. config:option:: max_dbs_open :: Limit of simultaneously opened databases

        This option places an upper bound on the number of databases that can
        be open at once. CouchDB reference counts database accesses internally
        and will close idle databases as needed. Sometimes it is necessary to
        keep more than the default open at once, such as in deployments where
        many databases will be replicating continuously. ::

            [couchdb]
            max_dbs_open = 100

    .. config:option:: max_document_size :: Limit maximum document body size

        .. versionchanged:: 3.0.0

        Limit maximum document body size. Size is calculated based on the
        serialized Erlang representation of the JSON document body, because
        that reflects more accurately the amount of storage consumed on disk.
        In particular, this limit does not include attachments.

        HTTP requests which create or update documents will fail with error
        code 413 if one or more documents is larger than this configuration
        value.

        In case of ``_update`` handlers, document size is checked after the
        transformation and right before being inserted into the database. ::

            [couchdb]
            max_document_size = 8000000 ; bytes

        .. warning::
           Before version 2.1.0 this setting was implemented by simply checking
           http request body sizes. For individual document updates via `PUT`
           that approximation was close enough, however that is not the case
           for ``_bulk_docs`` endpoint. After 2.1.0 a separate configuration
           parameter was defined: :config:option:`chttpd/max_http_request_size`,
           which can be used to limit maximum http request sizes. After upgrade,
           it is advisable to review those settings and adjust them accordingly.

    .. config:option:: os_process_timeout :: External processes time limit

        If an external process, such as a query server or external process,
        runs for this amount of milliseconds without returning any results, it
        will be terminated. Keeping this value smaller ensures you get
        expedient errors, but you may want to tweak it for your specific
        needs. ::

            [couchdb]
            os_process_timeout = 5000 ; 5 sec

    .. config:option:: single_node :: Start in single node mode.

        .. versionadded:: 3.0.0

        When this configuration setting is set to ``true``, automatically
        create the system databases on startup. Must be set ``false`` for a
        clustered CouchDB installation.

    .. config:option:: uri_file :: Discovery CouchDB help file

        This file contains the full `URI`_ that can be used to access this
        instance of CouchDB. It is used to help discover the port CouchDB is
        running on (if it was set to ``0`` (e.g. automatically assigned any
        free one). This file should be writable and readable for the user that
        runs the CouchDB service (``couchdb`` by default). ::

            [couchdb]
            uri_file = /var/run/couchdb/couchdb.uri

        .. _URI: http://en.wikipedia.org/wiki/URI

    .. config:option:: users_db_security_editable :: Protect ``_users`` DB security obj

        .. versionadded:: 3.0.0

        When this configuration setting is set to ``false``, reject any attempts
        to modify the ``_users`` database security object. Modification of this
        object is deprecated in 3.x and will be completely disallowed in CouchDB
        4.x.

    .. config:option:: users_db_suffix :: Users database suffix

        Specifies the suffix (last component of a name) of the system database
        for storing CouchDB users. ::

            [couchdb]
            users_db_suffix = _users

        .. warning::
            If you change the database name, do not forget to remove or clean
            up the old database, since it will no longer be protected by
            CouchDB.

    .. config:option:: util_driver_dir :: CouchDB binary utility drivers

        Specifies location of binary drivers (`icu`, `ejson`, etc.). This
        location and its contents should be readable for the user that runs the
        CouchDB service. ::

            [couchdb]
            util_driver_dir = /usr/lib/couchdb/erlang/lib/couch-1.5.0/priv/lib

    .. config:option:: uuid :: CouchDB server UUID

        .. versionadded:: 1.3

        Unique identifier for this CouchDB server instance. ::

            [couchdb]
            uuid = 0a959b9b8227188afc2ac26ccdf345a6

    .. config:option:: view_index_dir :: View indexes location directory

        Specifies location of CouchDB view index files. This location should be
        writable and readable for the user that runs the CouchDB service
        (``couchdb`` by default). ::

            [couchdb]
            view_index_dir = /var/lib/couchdb
