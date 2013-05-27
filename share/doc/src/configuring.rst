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

.. _configuring:

=============
Configuration
=============

.. todo:: Configuring CouchDB

CouchDB Configuration Files
===========================

.. todo:: CouchDB Configuration Files

Configuration File Locations
============================

CouchDB reads files from the following locations, in the following
order.

1. ``PREFIX/default.ini``

2. ``PREFIX/default.d/*``

3. ``PREFIX/local.ini``

4. ``PREFIX/local.d/*``

Settings in successive documents override the settings in earlier
entries. For example, setting the ``bind_address`` parameter in
``local.ini`` would override any setting in ``default.ini``.

.. warning::
   The ``default.ini`` file may be overwritten during an upgrade or
   re-installation, so localised changes should be made to the
   ``local.ini`` file or files within the ``local.d`` directory.

.. _update-notifications:

Update Notifications
====================

.. todo:: Update Notifications


MochiWeb Server Options
=======================

Server options for the MochiWeb component of CouchDB can be added to the
configuration files. Settings should be added to the ``server_options``
option of the ``[httpd]`` section of ``local.ini``. For example:

.. code-block:: ini

    [httpd]
    server_options = [{backlog, 128}, {acceptor_pool_size, 16}]

Socket Options Configuration Setting
====================================

The socket options for the listening socket in CouchDB can now be set
within the CouchDB configuration file. The setting should be added to
the ``[httpd]`` section of the file using the option name
``socket_options``. The specification is as a list of tuples. For
example:

.. code-block:: ini

    [httpd]
    socket_options = [{recbuf, 262144}, {sndbuf, 262144}, {nodelay, true}]

The options supported are a subset of full options supported by the
TCP/IP stack. A list of the supported options are provided in the
`Erlang inet`_ documentation.

.. _Erlang inet: http://www.erlang.org/doc/man/inet.html#setopts-2

Virtual Hosts
=============

CouchDB, since 0.11.0, can map requests to different locations based on
the ``Host`` header, even if they arrive on the some inbound IP address.

This allows different virtual hosts on the same machine to map to different
databases or design documents, etc. The most common use case is to map a
virtual host to a Rewrite Handler, to provide full control over the
application's URIs.

To add a virtual host, add a CNAME pointer to the DNS for your domain
name. For development and testing, it is sufficient to add an entry in
the hosts file, typically `/etc/hosts`` on Unix-like operating systems:

.. code-block:: bash

    # CouchDB vhost definitions, refer to local.ini for further details
    127.0.0.1       sofa.couchdb

Test that this is working:

.. code-block:: bash

    $ ping sofa.couchdb
    PING sofa.couchdb (127.0.0.1) 56(84) bytes of data.
    64 bytes from localhost.localdomain (127.0.0.1): icmp_req=1 ttl=64 time=0.025 ms
    64 bytes from localhost.localdomain (127.0.0.1): icmp_req=2 ttl=64 time=0.051 ms
    ^C

Finally, add an entry to your :ref:`configuration file <configuring>` in the ``[vhosts]``
section:

.. code-block:: ini

    [vhosts]
    sofa.couchdb:5984 = /sofa/_design/sofa/_rewrite

If your CouchDB is listening on the default HTTP port, or is sitting
behind a proxy, then don't specify a port number in the vhost key.

With the above setup, a request to ``http://sofa.couchdb:5984/sweet-o``
will be mapped to
``http://127.0.0.1:5984/sofa/_design/sofa/_rewrite/sweet-o``

.. versionadded:: 0.11.0 added `vhosts` functionality

HTTP Rewrite Handler
====================

Following on from `virtual hosts`_, CouchDB includes a custom URL rewriter.
All rewriting is done from ``/dbname/_design/ddocname/_rewrite`` by default.

The rewriter is flexible, and can handle methods and custom query formats.

Each rule should be in the ``rewrites`` top-level key of the design doc.
Example of a complete rule :

.. code-block:: json

    {
        ....
        "rewrites": [
        {
            "from": "",
            "to": "index.html",
            "method": "GET",
            "query": {}
        }
        ]
    }


**from**: is the path rule used to bind current uri to the rule. It
uses pattern matching for that.

**to**: rule to rewrite an url. It can contain variables depending on
binding variables discovered during pattern matching and query args
(url args and from the query member.)

**method**: method to bind the request method to the rule. If method
is missing, any method will be matched in the rewrite.

**query**: optional query arguments, that may contain dynamic variables,
by binding keys in the to be used with the matching URL.

``to`` and ``from`` are paths with patterns. The pattern can be strings starting
with  ``:`` or ``*``, for example ``/somepath/:var/*``.

The pattern matching is done by first matching the request method to a
rule. Then it will try to match the path to one specific rule. If no rule
match, then a 404 error is displayed.

The path is converted into an erlang list, by regex splitting on ``/``. Each
variable is converted into an atom. The subsequent pattern matching step is
done by splitting ``/`` in the request url into a list of atoms. A string
pattern will match the equivalent token. The ``*`` atom will match any number
of tokens, but may only be present as the last pattern in the path. If all
tokens are matched, and all path terms have been consumed, then the overall
path specification matches.

Once a matching ``from`` rule is found we rewrite the request url using the
``from``, ``to``, and ``query`` members. Each identified token will be reused
within the rule, and in the subsequent query if required. The identified
tokens are matched to the rule and will replace var. If ``*`` is found in
the rule it will contain any remaining suffix.

The rewriter is re-entrant, and has a configurable recursion limit, set
by default at 100.

Configuring Server Administrators
=================================

A default CouchDB install provides admin-level access to all connecting users.
This configuration is known as ``Admin Party``, and is not recommended for
in-production usage. You can crash the party simply by creating the first
admin account. CouchDB server administrators and passwords are not stored
in the ``_users`` database, but in the ``local.ini`` file, which should be
appropriately secured and readable only by system administrators.

.. code-block:: ini

    [admins]
    ;admin = mysecretpassword
    admin = -hashed-6d3c30241ba0aaa4e16c6ea99224f915687ed8cd,7f4a3e05e0cbc6f48a0035e3508eef90
    architect = -pbkdf2-43ecbd256a70a3a2f7de40d2374b6c3002918834,921a12f74df0c1052b3e562a23cd227f,10000

Administrators can be added directly to the ``[admins]`` section, and when
CouchDB is restarted, the passwords will be salted and encrypted. You may
also use the HTTP interface to create administrator accounts; this way,
you don't need to restart CouchDB, and there's no need to temporarily store
or transmit passwords in plaintext. The HTTP ``_config/admins`` endpoint
supports querying, deleting or creating new admin accounts:

.. code-block:: bash

    shell> GET /_config/admins HTTP/1.1
        Accept: application/json
        Host: localhost:5984

    HTTP/1.1 200 OK
        Cache-Control: must-revalidate
        Content-Length: 196
        Content-Type: application/json
        Date: Fri, 30 Nov 2012 11:37:18 GMT
        Server: CouchDB/1.3.0 (Erlang OTP/R15B02)

.. code-block:: json

        {
            "admin": "-hashed-6d3c30241ba0aaa4e16c6ea99224f915687ed8cd,7f4a3e05e0cbc6f48a0035e3508eef90",
            "architect": "-pbkdf2-43ecbd256a70a3a2f7de40d2374b6c3002918834,921a12f74df0c1052b3e562a23cd227f,10000"
        }

Further details are available in ``security_``, including configuring the
work factor for ``PBKDF2``, and the algorithm itself at
`PBKDF2 (RFC-2898) <http://tools.ietf.org/html/rfc2898>`_.

.. versionadded::
    1.3.0 ``PBKDF2`` server-side hashed salted password support added,
    now as a synchronous call for the ``_config/admins`` API.

OS Daemons
==========

CouchDB now supports starting external processes. The support is simple
and enables CouchDB to start each configured OS daemon. If the daemon
stops at any point, CouchDB will restart it (with protection to ensure
regularly failing daemons are not repeatedly restarted).

The daemon starting process is one-to-one; for each each configured
daemon in the configuration file, CouchDB will start exactly one
instance. If you need to run multiple instances, then you must create
separate individual configurations. Daemons are configured within the
``[os_daemons]`` section of your configuration file (``local.ini``). The
format of each configured daemon is:

.. code-block:: ini

    NAME = PATH ARGS

Where ``NAME`` is an arbitrary (and unique) name to identify the daemon;
``PATH`` is the full path to the daemon to be executed; ``ARGS`` are any
required arguments to the daemon.

For example:

.. code-block:: ini

    [os_daemons]
    basic_responder = /usr/local/bin/responder.js

There is no interactivity between CouchDB and the running process, but
you can use the OS Daemons service to create new HTTP servers and
responders and then use the new proxy service to redirect requests and
output to the CouchDB managed service. For more information on proxying,
see :ref:`http-proxying`. For further background on the OS Daemon service, see
`CouchDB Externals API`_.

.. _CouchDB Externals API: http://davispj.com/2010/09/26/new-couchdb-externals-api.html

Native SSL Support
==================

CouchDB |version| supports SSL natively. All your secure connection needs can
now be served without needing to setup and maintain a separate proxy server
that handles SSL.

SSL setup can be tricky, but the configuration in CouchDB was designed
to be as easy as possible. All you need is two files; a certificate and
a private key. If you bought an official SSL certificate from a
certificate authority, both should be in your possession already.

If you just want to try this out and don't want to pay anything upfront,
you can create a self-signed certificate. Everything will work the same,
but clients will get a warning about an insecure certificate.

You will need the OpenSSL command line tool installed. It probably
already is.

::

    shell> mkdir cert && cd cert
    shell> openssl genrsa > privkey.pem
    shell> openssl req -new -x509 -key privkey.pem -out mycert.pem -days 1095
    shell> ls
    mycert.pem privkey.pem

Now, you need to edit CouchDB's configuration, either by editing your
``local.ini`` file or using the ``/_config`` API calls or the
configuration screen in Futon. Here is what you need to do in
``local.ini``, you can infer what needs doing in the other places.

Be sure to make these edits. Under ``[daemons]`` you should see:

::

    ; enable SSL support by uncommenting the following line and supply the PEM's below.
    ; the default ssl port CouchDB listens on is 6984
    ;httpsd = {couch_httpd, start_link, [https]}

Here uncomment the last line:

::

    httpsd = {couch_httpd, start_link, [https]}

Next, under ``[ssl]`` you will see:

::

    ;cert_file = /full/path/to/server_cert.pem
    ;key_file = /full/path/to/server_key.pem

Uncomment and adjust the paths so it matches your system's paths:

::

    cert_file = /home/jan/cert/mycert.pem
    key_file = /home/jan/cert/privkey.pem

For more information please read
`http://www.openssl.org/docs/HOWTO/certificates.txt`_.

Now start (or restart) CouchDB. You should be able to connect to it
using HTTPS on port 6984:

::

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

::

    shell> curl -k https://127.0.0.1:6984/
    {"couchdb":"Welcome","version":"|version|"}

All done.

.. _`http://www.openssl.org/docs/HOWTO/certificates.txt`: http://www.openssl.org/docs/HOWTO/certificates.txt

.. _http-proxying:

HTTP Proxying
=============

The HTTP proxy feature makes it easy to map and redirect different
content through your CouchDB URL. The proxy works by mapping a pathname
and passing all content after that prefix through to the configured
proxy address.

Configuration of the proxy redirect is handled through the
``[httpd_global_handlers]`` section of the CouchDB configuration file
(typically ``local.ini``). The format is:

.. code-block:: ini

    [httpd_global_handlers]
    PREFIX = {couch_httpd_proxy, handle_proxy_req, <<"DESTINATION">>}


Where:

-  ``PREFIX``

   Is the string that will be matched. The string can be any valid
   qualifier, although to ensure that existing database names are not
   overridden by a proxy configuration, you can use an underscore
   prefix.

-  ``DESTINATION``

   The fully-qualified URL to which the request should be sent. The
   destination must include the ``http`` prefix. The content is used
   verbatim in the original request, so you can also forward to servers
   on different ports and to specific paths on the target host.

The proxy process then translates requests of the form:

.. code-block:: text

    http://couchdb:5984/PREFIX/path

To:

.. code-block:: text

    DESTINATION/path

.. note::
   Everything after ``PREFIX`` including the required forward slash
   will be appended to the ``DESTINATION``.

The response is then communicated back to the original client.

For example, the following configuration:

.. code-block:: ini

    _google = {couch_httpd_proxy, handle_proxy_req, <<"http://www.google.com">>}

Would forward all requests for ``http://couchdb:5984/_google`` to the
Google website.

The service can also be used to forward to related CouchDB services,
such as Lucene:

.. code-block:: ini

    [httpd_global_handlers]
    _fti = {couch_httpd_proxy, handle_proxy_req, <<"http://127.0.0.1:5985">>}

.. note::
   The proxy service is basic. If the request is not identified by the
   ``DESTINATION``, or the remainder of the ``PATH`` specification is
   incomplete, the original request URL is interpreted as if the
   ``PREFIX`` component of that URL does not exist.

   For example, requesting ``http://couchdb:5984/_intranet/media`` when
   ``/media`` on the proxy destination does not exist, will cause the
   request URL to be interpreted as ``http://couchdb:5984/media``. Care
   should be taken to ensure that both requested URLs and destination
   URLs are able to cope.

.. _cors:

Cross-Origin Resource Sharing
=============================

CORS, or "Cross-Origin Resource Sharing", allows a resource such as a web
page running JavaScript inside a browser, to make AJAX requests
(XMLHttpRequests) to a different domain, without compromising the security
of either party.

A typical use case is to have a static website hosted on a CDN make
requests to another resource, such as a hosted CouchDB instance. This
avoids needing an intermediary proxy, using JSONP or similar workarounds
to retrieve and host content.

While CouchDB's integrated HTTP server and support for document attachments
makes this less of a constraint for pure CouchDB projects, there are many
cases where separating the static content from the database access is
desirable, and CORS makes this very straightforward.

By supporting CORS functionality, a CouchDB instance can accept direct
connections to protected databases and instances, without the browser
functionality being blocked due to same-origin constraints. CORS is
supported today on over 90% of recent browsers.

CORS support is provided as experimental functionality in 1.3.0, and as such
will need to be enabled specifically in CouchDB's configuration. While all
origins are forbidden from making requests by default, support is available
for simple requests, preflight requests and per-vhost configuration.

.. versionadded:: 1.3.0

Enabling CORS
-------------

To enable CORS support, you need to set the ``enable_cors = true`` option
in the ``[httpd]`` section of ``local.ini``, and add a ``[cors]`` section
containing a ``origins = *`` setting. Note that by default, no origins are
accepted; you must either use a wildcard or whitelist.

.. code-block:: ini

    [httpd]
    enable_cors = true

    [cors]
    origins = *

Passing Credentials
-------------------

By default, neither authentication headers nor cookies are included in
requests and responses. To do so requires both setting
`XmlHttpRequest.withCredentials = true` on the request object in the
browser and enabling credentials support in CouchDB.

.. code-block:: ini

    [cors]
    credentials = true

CouchDB will respond to a credentials-enabled CORS request with an additional
header, `Access-Control-Allow-Credentials=true`.

Tightening Access
-----------------

Access can be restricted by protocol, host and optionally by port:

.. code-block:: ini

    [cors]
    ; List of origins, separated by a comma (protocol, host, optional port)
    ; refer to http://tools.ietf.org/html/rfc6454 for specification
    origins = http://localhost, https://localhost, http://www.number10.gov.uk:80

Specific HTTP methods may also be restricted:

.. code-block:: ini

    [cors]
    ; List of accepted methods, comma-separated
    ; refer to http://tools.ietf.org/html/rfc2616, rfc2817, rfc5789
    methods = GET, POST, PUT, DELETE

Configuration per vhost
-----------------------

All CORS-related settings may be configured on a per-vhost basis. For example,
the configuration section for `http://example.com/` would be contained in:

.. code-block:: ini

    [cors:http://example.com]
    credentials = false
    origins = *
    methods = GET, PUT, HEAD

Useful References
-----------------

- Original JIRA `implementation ticket <https://issues.apache.org/jira/browse/COUCHDB-431>`_

Standards and References:

- IETF RFCs relating to methods `RFC 2618 <http://tools.ietf.org/html/rfc2616>`_,
  `RFC 2817 <http://tools.ietf.org/html/rfc2817>`_, and
  `RFC 5789 <http://tools.ietf.org/html/rfc5789>`_
- IETF RFC 6454 for `Web Origins <http://tools.ietf.org/html/rfc6454>`_
- W3C `CORS standard <http://www.w3.org/TR/cors>`_

Mozilla Developer Network Resources:

- `Same origin policy for URIs <https://developer.mozilla.org/en-US/docs/Same-origin_policy_for_file:_URIs>`_
- `HTTP Access Control <https://developer.mozilla.org/En/HTTP_access_control>`_
- `Server-side Access Control <https://developer.mozilla.org/En/Server-Side_Access_Control>`_
- `Javascript same origin policy <https://developer.mozilla.org/en-US/docs/Same_origin_policy_for_JavaScript>`_

Client-side CORS support and usage:

- `CORS browser support matrix <http://caniuse.com/cors>`_
- `CORS tutorial <http://www.html5rocks.com/en/tutorials/cors/>`_
- `Cross-Site XMLHttpRequests with CORS <http://hacks.mozilla.org/2009/07/cross-site-xmlhttprequest-with-cors>`_
