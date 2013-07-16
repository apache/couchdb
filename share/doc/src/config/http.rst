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

===================
CouchDB HTTP Server
===================

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



.. _config/ssl:

``[ssl]`` :: Secure Socket Level Options
========================================

CouchDB supports SSL natively. All your secure connection needs can
now be served without needing to setup and maintain a separate proxy server
that handles SSL.

SSL setup can be tricky, but the configuration in CouchDB was designed
to be as easy as possible. All you need is two files; a certificate and
a private key. If you bought an official SSL certificate from a
certificate authority, both should be in your possession already.

If you just want to try this out and don't want to pay anything upfront,
you can create a self-signed certificate. Everything will work the same,
but clients will get a warning about an insecure certificate.

You will need the `OpenSSL`_ command line tool installed. It probably
already is.

.. code-block:: bash

    shell> mkdir /etc/couchdb/cert
    shell> cd /etc/couchdb/cert
    shell> openssl genrsa > privkey.pem
    shell> openssl req -new -x509 -key privkey.pem -out couchdb.pem -days 1095
    shell> chmod 600 privkey.pem couchdb.pem
    shell> chown couchdb privkey.pem couchdb.pem

Now, you need to edit CouchDB's configuration, either by editing your
``local.ini`` file or using the ``/_config`` API calls or the
configuration screen in Futon. Here is what you need to do in
``local.ini``, you can infer what needs doing in the other places.

At first, :ref:`enable HTTPS daemon <config/daemons/httpsd>`::

  [daemons]
  httpsd = {couch_httpd, start_link, [https]}

Next, under ``[ssl]`` section setup newly generated certificates::

  [ssl]
  cert_file = /etc/couchdb/cert/couchdb.pem
  key_file = /etc/couchdb/cert/privkey.pem

For more information please read `certificates HOWTO`_.

Now start (or restart) CouchDB. You should be able to connect to it
using HTTPS on port 6984:

.. code-block:: bash

    shell> curl https://127.0.0.1:6984/
    curl: (60) SSL certificate problem, verify that the CA cert is OK. Details:
    error:14090086:SSL routines:SSL3_GET_SERVER_CERTIFICATE:certificate verify failed
    More details here: http://curl.haxx.se/docs/sslcerts.html

    curl performs SSL certificate verification by default, using a "bundle"
    of Certificate Authority (CA) public keys (CA certs). If the default
    bundle file isn't adequate, you can specify an alternate file
    using the --cacert option.
    If this HTTPS server uses a certificate signed by a CA represented in
    the bundle, the certificate verification probably failed due to a
    problem with the certificate (it might be expired, or the name might
    not match the domain name in the URL).
    If you'd like to turn off curl's verification of the certificate, use
    the -k (or --insecure) option.

Oh no what happened?! — Remember, clients will notify their users that
your certificate is self signed. ``curl`` is the client in this case and
it notifies you. Luckily you trust yourself (don't you?) and you can
specify the ``-k`` option as the message reads:

.. code-block:: bash

    shell> curl -k https://127.0.0.1:6984/
    {"couchdb":"Welcome","version":"1.3.0"}

All done.

.. _`certificates HOWTO`: http://www.openssl.org/docs/HOWTO/certificates.txt
.. _OpenSSL: http://www.openssl.org/

.. _config/ssl/cacert_file:

``cacert_file`` :: CA Certificate file
--------------------------------------

Path to file containing PEM encoded CA certificates (trusted certificates used
for verifying a peer certificate). May be omitted if you do not want to verify
the peer::

  [ssl]
  cacert_file = /etc/ssl/certs/ca-certificates.crt


.. _config/ssl/cert_file:

``cert_file`` :: Certificate file
---------------------------------

Path to a file containing the user's certificate::

  [ssl]
  cert_file = /etc/couchdb/cert/couchdb.pem


.. _config/ssl/key_file:

``key_file`` :: Certificate key file
------------------------------------

Path to file containing user's private PEM encoded key::

  [ssl]
  key_file = /etc/couchdb/cert/privkey.pem


.. _config/ssl/password:

``password`` :: Certificate key password
----------------------------------------

String containing the user's password. Only used if the private keyfile is
password protected::

  [ssl]
  password = somepassword


.. _config/ssl/ssl_certificate_max_depth:

``ssl_certificate_max_depth`` :: Maximum peer certificate depth
---------------------------------------------------------------

Maximum peer certificate depth (must be set even if certificate validation is
off)::

  [ssl]
  ssl_certificate_max_depth = 1


.. _config/ssl/verify_fun:

``verify_fun`` :: SSL verification function
-------------------------------------------

The verification fun (optional) if not specified, the default
verification fun will be used::

  [ssl]
  verify_fun = {Module, VerifyFun}


.. _config/ssl/verify_ssl_certificates:

``verify_ssl_certificates`` :: Enable certificate verification
--------------------------------------------------------------

Set to `true` to validate peer certificates::

  [ssl]
  verify_ssl_certificates = false



.. _cors:
.. _config/cors:

``[cors]`` :: Cross-Origin Resource Sharing
===========================================

.. versionadded:: 1.3 added CORS support, see JIRA :issue:`431`

`CORS`, or "Cross-Origin Resource Sharing", allows a resource such as a web
page running JavaScript inside a browser, to make AJAX requests
(XMLHttpRequests) to a different domain, without compromising the security
of either party.

A typical use case is to have a static website hosted on a CDN make
requests to another resource, such as a hosted CouchDB instance. This
avoids needing an intermediary proxy, using `JSONP` or similar workarounds
to retrieve and host content.

While CouchDB's integrated HTTP server has support for document attachments
makes this less of a constraint for pure CouchDB projects, there are many
cases where separating the static content from the database access is
desirable, and CORS makes this very straightforward.

By supporting CORS functionality, a CouchDB instance can accept direct
connections to protected databases and instances, without the browser
functionality being blocked due to same-origin constraints. CORS is
supported today on over 90% of recent browsers.

CORS support is provided as experimental functionality in 1.3, and as such
will need to be enabled specifically in CouchDB's configuration. While all
origins are forbidden from making requests by default, support is available
for simple requests, preflight requests and per-vhost configuration.

This section requires :ref:`enable_cors <config/httpd/enable_cors>` option have
``true`` value::

  [httpd]
  enable_cors = true


Global Setup
------------

These options are under ``[cors]`` section. They are have global affect for
all CORS-enabled instances.


.. _config/cors/credentials:

``credentials``
^^^^^^^^^^^^^^^

By default, neither authentication headers nor cookies are included in
requests and responses. To do so requires both setting
``XmlHttpRequest.withCredentials = true`` on the request object in the
browser and enabling credentials support in CouchDB.

::

  [cors]
  credentials = true

CouchDB will respond to a credentials-enabled CORS request with an additional
header, ``Access-Control-Allow-Credentials=true``.


.. _config/cors/origins:

``origins``
^^^^^^^^^^^

List of origins separated by a comma, ``*`` means accept all.
You can’t set ``origins = *`` and ``credentials = true`` option at the same
time::

  [cors]
  origins = *

Access can be restricted by protocol, host and optionally by port. Origins must
follow the scheme: http://example.com:80::

  [cors]
  origins = http://localhost, https://localhost, http://couch.mydev.name:8080

Note that by default, no origins are accepted. You must define them explicitly.


.. _config/cors/headers:

``headers``
^^^^^^^^^^^

List of accepted headers separated by a comma::

  [cors]
  headers = X-Couch-Id, X-Couch-Rev


.. _config/cors/methods:

``methods``
^^^^^^^^^^^

List of accepted methods::

  [cors]
  methods = GET,POST


.. _config/cors/vhost:

Per Virtual Host Configuration
------------------------------

To set the options for a :ref:`config/vhosts`, you will need to create a section
with the vhost name prefixed by ``cors:`` .
Example case for the vhost `example.com`::

  [cors:example.com]
  credentials = false
  ; List of origins separated by a comma
  origins = *
  ; List of accepted headers separated by a comma
  headers = X-CouchDB-Header
  ; List of accepted methods
  methods = HEAD, GET

.. seealso::

   Original JIRA `implementation ticket <https://issues.apache.org/jira/browse/COUCHDB-431>`_

   Standards and References:

   - IETF RFCs relating to methods: :rfc:`2618`, :rfc:`2817`, :rfc:`5789`
   - IETF RFC for Web Origins: :rfc:`6454`
   - W3C `CORS standard <http://www.w3.org/TR/cors>`_

   Mozilla Developer Network Resources:

   - `Same origin policy for URIs <https://developer.mozilla.org/en-US/docs/Same-origin_policy_for_file:_URIs>`_
   - `HTTP Access Control <https://developer.mozilla.org/En/HTTP_access_control>`_
   - `Server-side Access Control <https://developer.mozilla.org/En/Server-Side_Access_Control>`_
   - `Javascript same origin policy <https://developer.mozilla.org/en-US/docs/Same_origin_policy_for_JavaScript>`_

   Client-side CORS support and usage:

   - `CORS browser support matrix <http://caniuse.com/cors>`_
   - `COS tutorial <http://www.html5rocks.com/en/tutorials/cors/>`_
   - `XHR with CORS <http://hacks.mozilla.org/2009/07/cross-site-xmlhttprequest-with-cors/>`_



.. _config/vhosts:

``[vhosts]`` :: Virtual Hosts
=============================

CouchDB can map requests to different locations based on the ``Host`` header,
even if they arrive on the some inbound IP address.

This allows different virtual hosts on the same machine to map to different
databases or design documents, etc. The most common use case is to map a
virtual host to a :ref:`Rewrite Handler <api/ddoc/rewrite>`, to provide full
control over the application's URIs.

To add a virtual host, add a `CNAME` pointer to the DNS for your domain
name. For development and testing, it is sufficient to add an entry in
the hosts file, typically `/etc/hosts`` on Unix-like operating systems:

.. code-block:: text

   # CouchDB vhost definitions, refer to local.ini for further details
   127.0.0.1       couchdb.local

Test that this is working:

.. code-block:: bash

   $ ping -n 2 couchdb.local
   PING couchdb.local (127.0.0.1) 56(84) bytes of data.
   64 bytes from localhost (127.0.0.1): icmp_req=1 ttl=64 time=0.025 ms
   64 bytes from localhost (127.0.0.1): icmp_req=2 ttl=64 time=0.051 ms

Finally, add an entry to your :ref:`configuration file <config>` in the
``[vhosts]`` section::

  [vhosts]
  couchdb.local:5984 = /example
  *.couchdb.local:5984 = /example

If your CouchDB is listening on the default HTTP port, or is sitting
behind a proxy, then don't specify a port number in the `vhost` key.

The first line will rewrite the request to display the content of the `example`
database. This rule works only if the ``Host`` header is ``couchdb.local`` and
won't work for `CNAMEs`. Second rule on the other hand match all `CNAMEs` to
`example` db. So `www.couchdb.local` or `db.couchdb.local` will work.


.. _config/vhosts/rewriting:

Rewriting Hosts to path
-----------------------

Like in the :ref:`_rewrite <api/ddoc/rewrite>` handler you could match some
variable and use them to create the target path. Some examples::

  [vhosts]
  *.couchdb.local = /*
  :dbname. = /:dbname
  :ddocname.:dbname.example.com = /:dbname/_design/:ddocname/_rewrite


First rule pass wildcard as `dbname`. The second one does the same, but uses a
variable name. And the third one allows you to use any app with `ddocname` in
any database with `dbname`.

You could also change the default function to handle request by changing
the setting :ref:`redirect_vhost_handler <config/httpd/redirect_vhost_handler>`
in :ref:`httpd <config/httpd>` config section.
