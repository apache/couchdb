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

===================
CouchDB HTTP Server
===================

.. _config/httpd:

HTTP Server Options
===================

.. config:section:: chttpd :: Clustered HTTP Server Options

    .. note::
        In CouchDB 2.x and 3.x, the `chttpd` section refers to the standard, clustered
        port. All use of CouchDB, aside from a few specific maintenance tasks as
        described in this documentation, should be performed over this port.

    .. config:option:: bind_address :: HTTP port IP address binding

        Defines the IP address by which the clustered port is available::

            [chttpd]
            bind_address = 127.0.0.1

        To let CouchDB listen any available IP address, use ``0.0.0.0``::

            [chttpd]
            bind_address = 0.0.0.0

        For IPv6 support you need to set ``::1`` if you want to let CouchDB
        listen correctly::

            [chttpd]
            bind_address = ::1

        or ``::`` for any available::

            [chttpd]
            bind_address = ::

    .. config:option:: port :: Listen port

        Defines the port number to listen::

            [chttpd]
            port = 5984

        To let CouchDB use any free port, set this option to ``0``::

            [chttpd]
            port = 0

    .. config:option:: prefer_minimal :: Sends minimal set of headers

        If a request has the header ``"Prefer": "return=minimal"``, CouchDB
        will only send the headers that are listed for the ``prefer_minimal``
        configuration.::

            [chttpd]
            prefer_minimal = Cache-Control, Content-Length, Content-Range, Content-Type, ETag, Server, Transfer-Encoding, Vary

        .. warning::
            Removing the Server header from the settings will mean that
            the CouchDB server header is replaced with the
            MochiWeb server header.

    .. config:option:: authentication_handlers :: Authentication handlers

        List of authentication handlers used by CouchDB. You may
        extend them via third-party plugins or remove some of them if you won't
        let users to use one of provided methods::

            [chttpd]
            authentication_handlers = {chttpd_auth, cookie_authentication_handler}, {chttpd_auth, default_authentication_handler}

        - ``{chttpd_auth, cookie_authentication_handler}``: used for Cookie auth;
        - ``{chttpd_auth, proxy_authentication_handler}``: used for Proxy auth;
        - ``{chttpd_auth, jwt_authentication_handler}``: used for JWT auth;
        - ``{chttpd_auth, default_authentication_handler}``: used for Basic auth;
        - ``{couch_httpd_auth, null_authentication_handler}``: disables auth, breaks CouchDB.

    .. config:option:: buffer_response :: Buffered response

        .. versionchanged:: 3.1.1

        Set this to ``true`` to delay the start of a response until the end has
        been calculated. This increases memory usage, but simplifies client error
        handling as it eliminates the possibility that a response may be deliberately
        terminated midway through, due to a timeout. This config value may be changed
        at runtime, without impacting any in-flight responses.

        Even if this is set to ``false`` (the default), buffered responses can be
        enabled on a per-request basis for any delayed JSON response call by adding
        ``?buffer_response=true`` to the request's parameters.

    .. config:option:: allow_jsonp :: Enables JSONP support

        .. versionchanged:: 3.2 moved from [httpd] to [chttpd] section

        The ``true`` value of this option enables `JSONP`_ support (it's
        ``false`` by default)::

            [chttpd]
            allow_jsonp = false

        .. _JSONP: https://en.wikipedia.org/wiki/JSONP

    .. config:option:: changes_timeout :: Changes feed timeout

        .. versionchanged:: 3.2 moved from [httpd] to [chttpd] section

        Specifies default `timeout` value for :ref:`Changes Feed <changes>` in
        milliseconds (60000 by default)::

            [chttpd]
            changes_timeout = 60000 ; 60 seconds

    .. config:option:: config_whitelist :: Config options while list

        .. versionchanged:: 3.2 moved from [httpd] to [chttpd] section

        Sets the configuration modification whitelist. Only whitelisted values
        may be changed via the :ref:`config API <api/config>`. To allow the
        admin to change this value over HTTP, remember to include
        ``{chttpd,config_whitelist}`` itself. Excluding it from the list would
        require editing this file to update the whitelist::

            [chttpd]
            config_whitelist = [{chttpd,config_whitelist}, {log,level}, {etc,etc}]

    .. config:option:: enable_cors :: Activates CORS

        .. versionadded:: 1.3
        .. versionchanged:: 3.2 moved from [httpd] to [chttpd] section

        Controls :ref:`CORS <config/cors>` feature::

            [chttpd]
            enable_cors = false

    .. config:option:: secure_rewrites :: Default request handler

        .. versionchanged:: 3.2 moved from [httpd] to [chttpd] section

        This option allow to isolate databases via subdomains::

            [chttpd]
            secure_rewrites = true

    .. config:option:: x_forwarded_host :: X-Forwarder-Host

        .. versionchanged:: 3.2 moved from [httpd] to [chttpd] section

        The `x_forwarded_host` header (``X-Forwarded-Host`` by default) is used
        to forward the original value of the ``Host`` header field in case, for
        example, if a reverse proxy is rewriting the "Host" header field to
        some internal host name before forward the request to CouchDB::

            [chttpd]
            x_forwarded_host = X-Forwarded-Host

        This header has higher priority above ``Host`` one, if only it exists
        in the request.

    .. config:option:: x_forwarded_proto :: X-Forwarder-Proto

        .. versionchanged:: 3.2 moved from [httpd] to [chttpd] section

        `x_forwarded_proto` header (``X-Forwarder-Proto`` by default) is used
        for identifying the originating protocol of an HTTP request, since a
        reverse proxy may communicate with CouchDB instance using HTTP even if
        the request to the reverse proxy is HTTPS::

            [chttpd]
            x_forwarded_proto = X-Forwarded-Proto

    .. config:option:: x_forwarded_ssl :: X-Forwarder-Ssl

        .. versionchanged:: 3.2 moved from [httpd] to [chttpd] section

        The `x_forwarded_ssl` header (``X-Forwarded-Ssl`` by default) tells
        CouchDB that it should use the `https` scheme instead of the `http`.
        Actually, it's a synonym for ``X-Forwarded-Proto: https`` header, but
        used by some reverse proxies::

            [chttpd]
            x_forwarded_ssl = X-Forwarded-Ssl

    .. config:option:: enable_xframe_options :: Controls X-Frame-Options header

        .. versionchanged:: 3.2 moved from [httpd] to [chttpd] section

        Controls :ref:`Enables or disabled <config/xframe_options>` feature::

            [chttpd]
            enable_xframe_options = false

    .. config:option:: max_http_request_size :: Maximum HTTP request body size

        .. versionchanged:: 3.2 moved from [httpd] to [chttpd] section

        Limit the maximum size of the HTTP request body. This setting applies
        to all requests and it doesn't discriminate between single vs.
        multi-document operations. So setting it to 1MB would block a
        `PUT` of a document larger than 1MB, but it might also block a
        ``_bulk_docs`` update of 1000 1KB documents, or a multipart/related
        update of a small document followed by two 512KB attachments. This
        setting is intended to be used as a protection against maliciously
        large HTTP requests rather than for limiting maximum document sizes. ::

            [chttpd]
            max_http_request_size = 4294967296 ; 4 GB

        .. warning::
           Before version 2.1.0 :config:option:`couchdb/max_document_size` was
           implemented effectively as ``max_http_request_size``. That is, it
           checked HTTP request bodies instead of document sizes. After the
           upgrade, it is advisable to review the usage of these configuration
           settings.

.. config:section:: httpd :: HTTP Server Options

    .. versionchanged:: 3.2 These options were moved to [chttpd] section:
                        `allow_jsonp`, `changes_timeout`, `config_whitelist`,
                        `enable_cors`, `secure_rewrites`, `x_forwarded_host`,
                        `x_forwarded_proto`, `x_forwarded_ssl`,
                        `enable_xframe_options`, `max_http_request_size`.

    .. config:option:: server_options :: MochiWeb Server Options

        Server options for the MochiWeb component of CouchDB can be added to
        the configuration files::

            [httpd]
            server_options = [{backlog, 128}, {acceptor_pool_size, 16}]

        The options supported are a subset of full options supported by the
        TCP/IP stack. A list of the supported options are provided in the
        `Erlang inet`_ documentation.

        .. _Erlang inet: http://www.erlang.org/doc/man/inet.html#setopts-2

    .. config:option:: socket_options :: Socket Options

        The socket options for the listening socket in CouchDB, as set at the
        beginning of ever request, can be specified as a list of tuples. For example::

            [httpd]
            socket_options = [{sndbuf, 262144}]

        The options supported are a subset of full options supported by the
        TCP/IP stack. A list of the supported options are provided in the
        `Erlang inet`_ documentation.

        .. _Erlang inet: http://www.erlang.org/doc/man/inet.html#setopts-2

.. _config/ssl:

HTTPS (SSL/TLS) Options
=======================

.. config:section:: ssl :: HTTPS (SSL/TLS) Options

    CouchDB supports TLS/SSL natively, without the use of a proxy server.

    HTTPS setup can be tricky, but the configuration in CouchDB was designed to
    be as easy as possible. All you need is two files; a certificate and a
    private key. If you have an official certificate from a certificate
    authority, both should be in your possession already.

    If you just want to try this out and don't want to go through the hassle of
    obtaining an official certificate, you can create a self-signed certificate.
    Everything will work the same, but clients will get a warning about an insecure
    certificate.

    You will need the `OpenSSL`_ command line tool installed. It probably
    already is.

    .. code-block:: bash

        shell> mkdir /etc/couchdb/cert
        shell> cd /etc/couchdb/cert
        shell> openssl genrsa > privkey.pem
        shell> openssl req -new -x509 -key privkey.pem -out couchdb.pem -days 1095
        shell> chmod 600 privkey.pem couchdb.pem
        shell> chown couchdb privkey.pem couchdb.pem

    Now, you need to edit CouchDB's configuration, by editing your
    ``local.ini`` file. Here is what you need to do.

    Under the ``[ssl]`` section, enable HTTPS and set up the newly generated
    certificates::

        [ssl]
        enable = true
        cert_file = /etc/couchdb/cert/couchdb.pem
        key_file = /etc/couchdb/cert/privkey.pem

    For more information please read `certificates HOWTO`_.

    Now start (or restart) CouchDB. You should be able to connect to it
    using HTTPS on port 6984:

    .. code-block:: console

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

    Oh no! What happened?! Remember, clients will notify their users that your
    certificate is self signed. ``curl`` is the client in this case and it
    notifies you. Luckily you trust yourself (don't you?) and you can specify
    the ``-k`` option as the message reads:

    .. code-block:: console

        shell> curl -k https://127.0.0.1:6984/
        {"couchdb":"Welcome","version":"1.5.0"}

    All done.

    For performance reasons, and for ease of setup, you may still wish to
    terminate HTTPS connections at your load balancer / reverse proxy, then use
    unencrypted HTTP between it and your CouchDB cluster. This is a recommended
    approach.

    Additional detail may be available in the `CouchDB wiki`_.

    .. _`certificates HOWTO`: http://www.openssl.org/docs/HOWTO/certificates.txt
    .. _OpenSSL: http://www.openssl.org/
    .. _`CouchDB wiki`: https://cwiki.apache.org/confluence/pages/viewpage.action?pageId=48203146

    .. config:option:: cacert_file :: CA Certificate file

        The path to a file containing PEM encoded CA certificates. The CA
        certificates are used to build the server certificate chain, and for
        client authentication. Also the CAs are used in the list of acceptable
        client CAs passed to the client when a certificate is requested. May be
        omitted if there is no need to verify the client and if there are not
        any intermediate CAs for the server certificate::

            [ssl]
            cacert_file = /etc/ssl/certs/ca-certificates.crt

    .. config:option:: cert_file :: Certificate file

        Path to a file containing the user's certificate::

            [ssl]
            cert_file = /etc/couchdb/cert/couchdb.pem

    .. config:option:: key_file :: Certificate key file

        Path to file containing user's private PEM encoded key::

            [ssl]
            key_file = /etc/couchdb/cert/privkey.pem

    .. config:option:: password :: Certificate key password

        String containing the user's password. Only used if the private key file
        is password protected::

            [ssl]
            password = somepassword

    .. config:option:: ssl_certificate_max_depth :: Maximum peer certificate depth

        Maximum peer certificate depth (must be set even if certificate
        validation is off)::

            [ssl]
            ssl_certificate_max_depth = 1

    .. config:option:: verify_fun :: SSL verification function

        The verification fun (optional) if not specified, the default
        verification fun will be used::

            [ssl]
            verify_fun = {Module, VerifyFun}

    .. config:option:: verify_ssl_certificates :: Enable certificate verification

        Set to ``true`` to validate peer certificates::

            [ssl]
            verify_ssl_certificates = false

    .. config:option:: fail_if_no_peer_cert :: Require presence of client certificate if certificate verification is enabled

        Set to ``true`` to terminate the TLS/SSL handshake with a
        ``handshake_failure`` alert message if the client does not send a
        certificate. Only used if ``verify_ssl_certificates`` is ``true``. If set
        to ``false`` it will only fail if the client sends an invalid certificate
        (an empty certificate is considered valid)::

            [ssl]
            fail_if_no_peer_cert = false

    .. config:option:: secure_renegotiate :: Enable secure renegotiation

        Set to ``true`` to reject renegotiation attempt that does not live up to
        RFC 5746::

            [ssl]
            secure_renegotiate = true

    .. config:option:: ciphers :: Specify permitted server cipher list

        Set to the cipher suites that should be supported which can be
        specified in erlang format "{ecdhe_ecdsa,aes_128_cbc,sha256}" or
        in OpenSSL format "ECDHE-ECDSA-AES128-SHA256". ::

            [ssl]
            ciphers = ["ECDHE-ECDSA-AES128-SHA256", "ECDHE-ECDSA-AES128-SHA"]

    .. config:option:: tls_versions :: Specify permitted server SSL/TLS protocol versions

        Set to a list of permitted SSL/TLS protocol versions::

            [ssl]
            tls_versions = [tlsv1 | 'tlsv1.1' | 'tlsv1.2']

.. _cors:
.. _config/cors:

Cross-Origin Resource Sharing
=============================

.. config:section:: cors :: Cross-Origin Resource Sharing

    .. versionadded:: 1.3 added CORS support, see JIRA :issue:`431`
    .. versionchanged:: 3.2 moved from [httpd] to [chttpd] section

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

    This section requires :option:`chttpd/enable_cors` option have
    ``true`` value::

        [chttpd]
        enable_cors = true

    .. config:option:: credentials

        By default, neither authentication headers nor cookies are included in
        requests and responses. To do so requires both setting
        ``XmlHttpRequest.withCredentials = true`` on the request object in the
        browser and enabling credentials support in CouchDB. ::

            [cors]
            credentials = true

        CouchDB will respond to a credentials-enabled CORS request with an
        additional header, ``Access-Control-Allow-Credentials=true``.

    .. config:option:: origins

        List of origins separated by a comma, ``*`` means accept all. You can’t
        set ``origins = *`` and ``credentials = true`` option at the same
        time::

            [cors]
            origins = *

        Access can be restricted by protocol, host and optionally by port.
        Origins must follow the scheme: http://example.com:80::

            [cors]
            origins = http://localhost, https://localhost, http://couch.mydev.name:8080

        Note that by default, no origins are accepted. You must define them
        explicitly.

    .. config:option:: headers

        List of accepted headers separated by a comma::

            [cors]
            headers = X-Couch-Id, X-Couch-Rev

    .. config:option:: methods

        List of accepted methods::

            [cors]
            methods = GET,POST

    .. config:option:: max_age

        Sets the ``Access-Control-Max-Age`` header in seconds. Use it to
        avoid repeated ``OPTIONS`` requests.

            [cors]
            max_age = 3600

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
        - `JavaScript same origin policy <https://developer.mozilla.org/en-US/docs/Same_origin_policy_for_JavaScript>`_

        Client-side CORS support and usage:

        - `CORS browser support matrix <http://caniuse.com/cors>`_
        - `COS tutorial <http://www.html5rocks.com/en/tutorials/cors/>`_
        - `XHR with CORS <http://hacks.mozilla.org/2009/07/cross-site-xmlhttprequest-with-cors/>`_

Per Virtual Host Configuration
------------------------------

.. warning::

    Virtual Hosts are deprecated in CouchDB 3.0, and will be removed in CouchDB 4.0.

To set the options for a :section:`vhosts`, you will need to create a section
with the vhost name prefixed by ``cors:``. Example case for the vhost
`example.com`::

    [cors:example.com]
    credentials = false
    ; List of origins separated by a comma
    origins = *
    ; List of accepted headers separated by a comma
    headers = X-CouchDB-Header
    ; List of accepted methods
    methods = HEAD, GET

A video from 2010 on vhost and rewrite configuration `is available
<https://vimeo.com/20773112>`_, but is not guaranteed to match current syntax
or behaviour.

.. _config/vhosts:

Virtual Hosts
=============

.. warning::

    Virtual Hosts are deprecated in CouchDB 3.0, and will be removed in CouchDB 4.0.

.. config:section:: vhosts :: Virtual Hosts

    CouchDB can map requests to different locations based on the ``Host``
    header, even if they arrive on the same inbound IP address.

    This allows different virtual hosts on the same machine to map to different
    databases or design documents, etc. The most common use case is to map a
    virtual host to a :ref:`Rewrite Handler <api/ddoc/rewrite>`, to provide
    full control over the application's URIs.

    To add a virtual host, add a `CNAME` pointer to the DNS for your domain
    name. For development and testing, it is sufficient to add an entry in the
    hosts file, typically `/etc/hosts`` on Unix-like operating systems:

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

    If your CouchDB is listening on the the default HTTP port (80), or is
    sitting behind a proxy, then you don't need to specify a port number in the
    ``vhost`` key.

    The first line will rewrite the request to display the content of the
    `example` database. This rule works only if the ``Host`` header is
    ``couchdb.local`` and won't work for `CNAMEs`. The second rule, on the
    other hand, matches all `CNAMEs` to `example` db, so that both
    `www.couchdb.local` and `db.couchdb.local` will work.

Rewriting Hosts to a Path
-------------------------

Like in the :ref:`_rewrite <api/ddoc/rewrite>` handler you can match some
variable and use them to create the target path. Some examples::

    [vhosts]
    *.couchdb.local = /*
    :dbname. = /:dbname
    :ddocname.:dbname.example.com = /:dbname/_design/:ddocname/_rewrite

The first rule passes the wildcard as ``dbname``. The second one does the same,
but uses a variable name. And the third one allows you to use any URL with
``ddocname`` in any database with ``dbname``.

.. _xframe_options:
.. _config/xframe_options:

X-Frame-Options
=============================

X-Frame-Options is a response header that controls whether a http response
can be embedded in a <frame>, <iframe> or <object>. This is a security
feature to help against clickjacking.

    [x_frame_options]
    ; Settings same-origin will return X-Frame-Options: SAMEORIGIN.
    ; If same origin is set, it will ignore the hosts setting
    ; same_origin = true
    ; Settings hosts will
    ; return X-Frame-Options: ALLOW-FROM https://example.com/
    ; List of hosts separated by a comma. * means accept all
    ; hosts =

If xframe_options is enabled it will return ``X-Frame-Options: DENY`` by default.
If ``same_origin`` is enabled it will return ``X-Frame-Options: SAMEORIGIN``.
A ``X-FRAME-OPTIONS: ALLOW-FROM url`` will be returned when ``same_origin``
is false, and the HOST header matches one of the urls in the ``hosts`` config.
Otherwise a ``X-Frame-Options: DENY`` will be returned.
