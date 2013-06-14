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

.. _config/httpd:

``[httpd]`` :: HTTP Server Options
==================================

These options are under ``[httpd]`` section.

.. _config/httpd/allow_jsonp:

``allow_jsonp`` :: Enables JSONP support
----------------------------------------

The ``true`` value of this option enables `JSONP`_ support (it's ``false`` by
default)::

  [httpd]
  allow_jsonp = false


.. _JSONP: http://www.json-p.org/


.. _config/httpd/authentication_handlers:

``authentication_handlers`` :: Authentication handlers
------------------------------------------------------

List of used authentication handlers that used by CouchDB. You may extend them
via third-party plugins or remove some of them if you won't let users to use one
of provided methods::

  [httpd]
  authentication_handlers = {couch_httpd_oauth, oauth_authentication_handler}, {couch_httpd_auth, cookie_authentication_handler}, {couch_httpd_auth, default_authentication_handler}

- ``{couch_httpd_oauth, oauth_authentication_handler}``: handles OAuth;
- ``{couch_httpd_auth, cookie_authentication_handler}``: used for Cookie auth;
- ``{couch_httpd_auth, proxy_authentication_handler}``: used for Proxy auth;
- ``{couch_httpd_auth, default_authentication_handler}``: used for Basic auth;
- ``{couch_httpd_auth, null_authentication_handler}``: disables auth.
  Everlasting `Admin Party`!


.. _config/httpd/bind_address:

``bind_address`` :: Listen IP address
-------------------------------------

Defines the IP address by which CouchDB will be accessible::

  [httpd]
  bind_address = 127.0.0.1

To let CouchDB listen any available IP address, just setup ``0.0.0.0`` value::

  [httpd]
  bind_address = 0.0.0.0

For IPv6 support you need to set ``::1`` if you want to let CouchDB listen local
address::

  [httpd]
  bind_address = ::1

or ``::`` for any available::

  [httpd]
  bind_address = ::


.. _config/httpd/changes_timeout:

``changes_timeout`` :: Changes feed timeout
-------------------------------------------

Specifies default `timeout` value for :ref:`Changes Feed <changes>` in
milliseconds (60000 by default)::

  [httpd]
  changes_feed = 60000 ; 60 seconds


.. _config/httpd/config_whitelist:

``config_whitelist`` :: Config options while list
-------------------------------------------------

Sets the configuration modification whitelist. Only whitelisted values may be
changed via the :ref:`config API <api/config>`. To allow the admin to change
this value over HTTP, remember to include ``{httpd,config_whitelist}`` itself.
Excluding it from the list would require editing this file to update the
whitelist::

  [httpd]
  config_whitelist = [{httpd,config_whitelist}, {log,level}, {etc,etc}]


.. _config/httpd/default_handler:

``default_handler`` :: Default request handler
----------------------------------------------

Specifies default HTTP requests handler::

  [httpd]
  default_handler = {couch_httpd_db, handle_request}


.. _config/httpd/enable_cors:

``enable_cors`` :: Activates CORS
---------------------------------

.. versionadded:: 1.3

Controls :ref:`CORS <config/cors>` feature::

  [httpd]
  enable_cors = false


.. _config/httpd/log_max_chunk_size:

``log_max_chunk_size`` :: Logs chunk size
-----------------------------------------

Defines maximum chunk size in bytes for :ref:`_log <api/misc/log>` resource::

  [httpd]
  log_max_chunk_size = 1000000


.. _config/httpd/port:

``port`` :: Listen port
-----------------------

Defined the port number to listen::

  [httpd]
  port = 5984

To let CouchDB handle any free port, set this option to ``0``::

  [httpd]
  port = 0

After that, CouchDB URI could be located within the URI file.


.. _config/httpd/redirect_vhost_handler:

``redirect_vhost_handler`` :: Virtual Hosts custom redirect handler
-------------------------------------------------------------------

This option allows to change the default function that handles requests to
:ref:`virtual hosts <config/vhosts>`::

  [httpd]
  redirect_vhost_handler = {Module, Fun}

Specified function take 2 arguments: the Mochiweb request object and the target
path.


.. _config/httpd/server_options:

``server_options`` :: MochiWeb Server Options
---------------------------------------------

Server options for the `MochiWeb`_ component of CouchDB can be added to the
configuration files::

  [httpd]
  server_options = [{backlog, 128}, {acceptor_pool_size, 16}]


.. _MochiWeb: https://github.com/mochi/mochiweb


.. _config/httpd/secure_rewrites:

``secure_rewrites`` :: Default request handler
----------------------------------------------

This option allow to isolate databases via subdomains::

  [httpd]
  secure_rewrites = true


.. _config/httpd/socket_options:

``socket_options`` :: Socket Options
------------------------------------

The socket options for the listening socket in CouchDB can be specified as a
list of tuples. For example::

  [httpd]
  socket_options = [{recbuf, 262144}, {sndbuf, 262144}, {nodelay, true}]

The options supported are a subset of full options supported by the
TCP/IP stack. A list of the supported options are provided in the
`Erlang inet`_ documentation.

.. _Erlang inet: http://www.erlang.org/doc/man/inet.html#setopts-2


.. _config/httpd/vhost_global_handlers:

``vhost_global_handlers`` :: Virtual hosts global handlers
----------------------------------------------------------

List of global handlers that are available for
:ref:`virtual hosts <config/vhosts>`::

  [httpd]
  vhost_global_handlers = _utils, _uuids, _session, _oauth, _users


.. _config/httpd/x_forwarded_host:

``x_forwarded_host`` :: X-Forwarder-Host
----------------------------------------

The `x_forwarded_host` header (``X-Forwarded-Host`` by default) is used to
forward the original value of the ``Host`` header field in case, for example,
if a reverse proxy is rewriting the "Host" header field to some internal host
name before forward the request to CouchDB::

  [httpd]
  x_forwarded_host = X-Forwarded-Host

This header has higher priority above ``Host`` one, if only it exists in the
request.

.. _config/httpd/x_forwarded_proto:

``x_forwarded_proto`` :: X-Forwarder-Proto
------------------------------------------

`x_forwarded_proto` header (``X-Forwarder-Proto`` by default) is used for
identifying the originating protocol of an HTTP request, since a reverse proxy
may communicate with CouchDB instance using HTTP even if the request to
the reverse proxy is HTTPS::

  [httpd]
  x_forwarded_proto = X-Forwarded-Proto


.. _config/httpd/x_forwarded_ssl:

``x_forwarded_ssl`` :: X-Forwarder-Ssl
--------------------------------------

The `x_forwarded_ssl` header (``X-Forwarded-Ssl`` by default) tells CouchDB that
it should use the `https` scheme instead of the `http`. Actually, it's a synonym
for ``X-Forwarded-Proto: https`` header, but used by some reverse proxies::

  [httpd]
  x_forwarded_ssl = X-Forwarded-Ssl


.. _config/httpd/WWW-Authenticate:

``WWW-Authenticate`` :: Force basic auth
----------------------------------------

Set this option to trigger basic-auth popup on unauthorized requests::

  [httpd]
  WWW-Authenticate = Basic realm="Welcome to the Couch!"

