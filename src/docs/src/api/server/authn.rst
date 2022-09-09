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

.. _api/auth:

==============
Authentication
==============

Interfaces for obtaining session and authorization data.

.. note::
    We also strongly recommend you :ref:`set up SSL <config/ssl>` to
    improve all authentication methods' security.

.. _api/auth/basic:

Basic Authentication
====================

`Basic authentication`_ (:rfc:`2617`) is a quick and simple way to authenticate
with CouchDB. The main drawback is the need to send user credentials with each
request which may be insecure and could hurt operation performance (since
CouchDB must compute the password hash with every request):

**Request**:

.. code-block:: http

    GET / HTTP/1.1
    Accept: application/json
    Authorization: Basic cm9vdDpyZWxheA==
    Host: localhost:5984

**Response**:

.. code-block:: http

    HTTP/1.1 200 OK
    Cache-Control: must-revalidate
    Content-Length: 177
    Content-Type: application/json
    Date: Mon, 03 Dec 2012 00:44:47 GMT
    Server: CouchDB (Erlang/OTP)

    {
        "couchdb":"Welcome",
        "uuid":"0a959b9b8227188afc2ac26ccdf345a6",
        "version":"1.3.0",
        "vendor": {
            "version":"1.3.0",
            "name":"The Apache Software Foundation"
        }
    }

.. _Basic authentication: http://en.wikipedia.org/wiki/Basic_access_authentication

.. _api/auth/cookie:

Cookie Authentication
=====================

For cookie authentication (:rfc:`2109`) CouchDB generates a token that the
client can use for the next few requests to CouchDB. Tokens are valid until
a timeout. When CouchDB sees a valid token in a subsequent request, it will
authenticate the user by this token without requesting the password again. By
default, cookies are valid for 10 minutes, but it's :config:option:`adjustable
<chttpd_auth/timeout>`. Also it's possible to make cookies
:config:option:`persistent <chttpd_auth/allow_persistent_cookies>`.

To obtain the first token and thus authenticate a user for the first time, the
``username`` and ``password`` must be sent to the :ref:`_session API
<api/auth/session>`.

.. _api/auth/session:

``/_session``
-------------

.. http:post:: /_session
    :synopsis: Authenticates user by Cookie-based user login

    Initiates new session for specified user credentials by providing `Cookie`
    value.

    :<header Content-Type: - :mimetype:`application/x-www-form-urlencoded`
                           - :mimetype:`application/json`
    :query string next: Enforces redirect after successful login to the
      specified location. This location is relative from server root.
      *Optional*.
    :form name: User name
    :form password: Password
    :>header Set-Cookie: Authorization token
    :>json boolean ok: Operation status
    :>json string name: Username
    :>json array roles: List of user roles
    :code 200: Successfully authenticated
    :code 302: Redirect after successful authentication
    :code 401: Username or password wasn't recognized

    **Request**:

    .. code-block:: http

        POST /_session HTTP/1.1
        Accept: application/json
        Content-Length: 24
        Content-Type: application/x-www-form-urlencoded
        Host: localhost:5984

        name=root&password=relax

    It's also possible to send data as JSON:

    .. code-block:: http

        POST /_session HTTP/1.1
        Accept: application/json
        Content-Length: 37
        Content-Type: application/json
        Host: localhost:5984

        {
            "name": "root",
            "password": "relax"
        }

    **Response**:

    .. code-block:: http

        HTTP/1.1 200 OK
        Cache-Control: must-revalidate
        Content-Length: 43
        Content-Type: application/json
        Date: Mon, 03 Dec 2012 01:23:14 GMT
        Server: CouchDB (Erlang/OTP)
        Set-Cookie: AuthSession=cm9vdDo1MEJCRkYwMjq0LO0ylOIwShrgt8y-UkhI-c6BGw; Version=1; Path=/; HttpOnly

        {"ok":true,"name":"root","roles":["_admin"]}

    If ``next`` query parameter was provided the response will trigger
    redirection to the specified location in case of successful authentication:

    **Request**:

    .. code-block:: http

        POST /_session?next=/blog/_design/sofa/_rewrite/recent-posts HTTP/1.1
        Accept: application/json
        Content-Type: application/x-www-form-urlencoded
        Host: localhost:5984

        name=root&password=relax

    **Response**:

    .. code-block:: http

        HTTP/1.1 302 Moved Temporarily
        Cache-Control: must-revalidate
        Content-Length: 43
        Content-Type: application/json
        Date: Mon, 03 Dec 2012 01:32:46 GMT
        Location: http://localhost:5984/blog/_design/sofa/_rewrite/recent-posts
        Server: CouchDB (Erlang/OTP)
        Set-Cookie: AuthSession=cm9vdDo1MEJDMDEzRTp7Vu5GKCkTxTVxwXbpXsBARQWnhQ; Version=1; Path=/; HttpOnly

        {"ok":true,"name":null,"roles":["_admin"]}

.. http:get:: /_session
    :synopsis: Returns Cookie-based login user information

    Returns information about the authenticated user, including a
    :ref:`userctx_object`, the authentication method and database that were
    used, and a list of configured authentication handlers on the server.

    :query boolean basic: Accept `Basic Auth` by requesting this resource.
      *Optional*.
    :>json boolean ok: Operation status
    :>json object userCtx: User context for the current user
    :>json object info: Server authentication configuration
    :code 200: Successfully authenticated.
    :code 401: Username or password wasn't recognized.

    **Request**:

    .. code-block:: http

        GET /_session HTTP/1.1
        Host: localhost:5984
        Accept: application/json
        Cookie: AuthSession=cm9vdDo1MEJDMDQxRDpqb-Ta9QfP9hpdPjHLxNTKg_Hf9w

    **Response**:

    .. code-block:: http

        HTTP/1.1 200 OK
        Cache-Control: must-revalidate
        Content-Length: 175
        Content-Type: application/json
        Date: Fri, 09 Aug 2013 20:27:45 GMT
        Server: CouchDB (Erlang/OTP)
        Set-Cookie: AuthSession=cm9vdDo1MjA1NTBDMTqmX2qKt1KDR--GUC80DQ6-Ew_XIw; Version=1; Path=/; HttpOnly

        {
            "info": {
                "authenticated": "cookie",
                "authentication_db": "_users",
                "authentication_handlers": [
                    "cookie",
                    "default"
                ]
            },
            "ok": true,
            "userCtx": {
                "name": "root",
                "roles": [
                    "_admin"
                ]
            }
        }

.. http:delete:: /_session
    :synopsis: Logout Cookie-based user

    Closes user's session by instructing the browser to clear the cookie. This
    does not invalidate the session from the server's perspective, as there is
    no way to do this because CouchDB cookies are stateless. This means calling
    this endpoint is purely optional from a client perspective, and it does not
    protect against theft of a session cookie.

    :code 200: Successfully close session.

    **Request**:

    .. code-block:: http

        DELETE /_session HTTP/1.1
        Accept: application/json
        Cookie: AuthSession=cm9vdDo1MjA1NEVGMDo1QXNQkqC_0Qmgrk8Fw61_AzDeXw
        Host: localhost:5984

    **Response**:

    .. code-block:: http

        HTTP/1.1 200 OK
        Cache-Control: must-revalidate
        Content-Length: 12
        Content-Type: application/json
        Date: Fri, 09 Aug 2013 20:30:12 GMT
        Server: CouchDB (Erlang/OTP)
        Set-Cookie: AuthSession=; Version=1; Path=/; HttpOnly

        {
            "ok": true
        }

.. _api/auth/proxy:

Proxy Authentication
====================

.. note::
    To use this authentication method make sure that the
    ``{chttpd_auth, proxy_authentication_handler}`` value is added to the
    list of the active :config:option:`chttpd/authentication_handlers`:

    .. code-block:: ini

        [chttpd]
        authentication_handlers = {chttpd_auth, cookie_authentication_handler}, {chttpd_auth, proxy_authentication_handler}, {chttpd_auth, default_authentication_handler}

`Proxy authentication` is very useful in case your application already uses
some external authentication service and you don't want to duplicate users and
their roles in CouchDB.

This authentication method allows creation of a :ref:`userctx_object` for
remotely authenticated user. By default, the client just needs to pass specific
headers to CouchDB with related requests:

- :config:option:`X-Auth-CouchDB-UserName <chttpd_auth/x_auth_username>`:
  username;
- :config:option:`X-Auth-CouchDB-Roles <chttpd_auth/x_auth_roles>`:
  comma-separated (``,``) list of user roles;
- :config:option:`X-Auth-CouchDB-Token <chttpd_auth/x_auth_token>`:
  authentication token. When
  :config:option:`proxy_use_secret <chttpd_auth/proxy_use_secret>`
  is set (which is strongly recommended!), this header provides an HMAC of the
  username to authenticate and the secret token to prevent requests from
  untrusted sources. (Use the SHA1 of the username and sign with the secret)

**Creating the token (example with openssl)**:

.. code-block:: sh

    echo -n "foo" | openssl dgst -sha1 -hmac "the_secret"
    # (stdin)= 22047ebd7c4ec67dfbcbad7213a693249dbfbf86

**Request**:

.. code-block:: http

    GET /_session HTTP/1.1
    Host: localhost:5984
    Accept: application/json
    Content-Type: application/json; charset=utf-8
    X-Auth-CouchDB-Roles: users,blogger
    X-Auth-CouchDB-UserName: foo
    X-Auth-CouchDB-Token: 22047ebd7c4ec67dfbcbad7213a693249dbfbf86

**Response**:

.. code-block:: http

    HTTP/1.1 200 OK
    Cache-Control: must-revalidate
    Content-Length: 190
    Content-Type: application/json
    Date: Fri, 14 Jun 2013 10:16:03 GMT
    Server: CouchDB (Erlang/OTP)

    {
        "info": {
            "authenticated": "proxy",
            "authentication_db": "_users",
            "authentication_handlers": [
                "cookie",
                "proxy",
                "default"
            ]
        },
        "ok": true,
        "userCtx": {
            "name": "foo",
            "roles": [
                "users",
                "blogger"
            ]
        }
    }

Note that you don't need to request :ref:`session <api/auth/session>`
to be authenticated by this method if all required HTTP headers are provided.

.. _api/auth/jwt:

JWT Authentication
====================

.. note::
    To use this authentication method, make sure that the
    ``{chttpd_auth, jwt_authentication_handler}`` value is added to the
    list of the active :config:option:`chttpd/authentication_handlers`:

    .. code-block:: ini

        [chttpd]
        authentication_handlers = {chttpd_auth, cookie_authentication_handler}, {chttpd_auth, jwt_authentication_handler}, {chttpd_auth, default_authentication_handler}

``JWT authentication`` enables CouchDB to use externally-generated JWT tokens
instead of defining users or roles in the ``_users`` database.

The JWT authentication handler requires that all JWT tokens are signed by a key that
CouchDB has been configured to trust (there is no support for JWT's "NONE" algorithm).

Additionally, CouchDB can be configured to reject JWT tokens that are
missing a configurable set of claims (e.g, a CouchDB administrator
could insist on the ``exp`` claim).

Only claims listed in required checks are validated. Additional claims will be ignored.

Two sections of config exist to configure JWT authentication;

The :config:option:`required_claims <jwt_auth/required_claims>` config
setting is a comma-separated list of additional mandatory JWT claims
that must be present in any presented JWT token. A `:code 400:Bad
Request` is sent if any are missing.

The ``alg`` claim is mandatory as it used to lookup the correct key for verifying the
signature.

The ``sub`` claim is mandatory and is used as the CouchDB user's name if the JWT token
is valid.

A private claim called ``_couchdb.roles`` is optional. If presented,
as a JSON array of strings, it is used as the CouchDB user's roles
list as long as the JWT token is valid.

.. code-block:: ini

    ; [jwt_keys]
    ; Configure at least one key here if using the JWT auth handler.
    ; If your JWT tokens do not include a "kid" attribute, use "_default"
    ; as the config key, otherwise use the kid as the config key.
    ; Examples
    ; hmac:_default = aGVsbG8=
    ; hmac:foo = aGVsbG8=
    ; The config values can represent symmetric and asymmetrics keys.
    ; For symmetrics keys, the value is base64 encoded;
    ; hmac:_default = aGVsbG8= # base64-encoded form of "hello"
    ; For asymmetric keys, the value is the PEM encoding of the public
    ; key with newlines replaced with the escape sequence \n.
    ; rsa:foo = -----BEGIN PUBLIC KEY-----\nMIIBIjAN...IDAQAB\n-----END PUBLIC KEY-----\n
    ; ec:bar = -----BEGIN PUBLIC KEY-----\nMHYwEAYHK...AzztRs\n-----END PUBLIC KEY-----\n

The ``jwt_key`` section lists all the keys that this CouchDB server trusts. You
should ensure that all nodes of your cluster have the same list.

JWT tokens that do not include a ``kid`` claim will be validated against the
``$alg:_default`` key.

It is mandatory to specify the algorithm associated with every key for security
reasons (notably presenting a HMAC-signed token using an RSA or EC public key
that the server trusts:
https://auth0.com/blog/critical-vulnerabilities-in-json-web-token-libraries/).

**Request**:

.. code-block:: http

    GET /_session HTTP/1.1
    Host: localhost:5984
    Accept: application/json
    Content-Type: application/json; charset=utf-8
    Authorization: Bearer <JWT token>

**Response**:

.. code-block:: http

    HTTP/1.1 200 OK
    Cache-Control: must-revalidate
    Content-Length: 188
    Content-Type: application/json
    Date: Sun, 19 Apr 2020 08:29:15 GMT
    Server: CouchDB (Erlang/OTP)

    {
        "info": {
            "authenticated": "jwt",
            "authentication_db": "_users",
            "authentication_handlers": [
                "cookie",
                "proxy",
                "default"
            ]
        },
        "ok": true,
        "userCtx": {
            "name": "foo",
            "roles": [
                "users",
                "blogger"
            ]
        }
    }

Note that you don't need to request :ref:`session <api/auth/session>`
to be authenticated by this method if the required HTTP header is provided.
