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

================================
Authentication and Authorization
================================

.. _config/admins:

Server Administrators
=====================

.. config:section:: admins :: Server Administrators

.. versionchanged:: 3.0.0

    CouchDB requires an admin account to start. If an admin account has not
    been created, CouchDB will print an error message and terminate.

    CouchDB server administrators and passwords are not stored in the
    ``_users`` database, but in the last ``[admins]`` section that CouchDB
    finds when loading its ini files. See :config:intro for details on config
    file order and behaviour. This file (which could be something like
    ``/opt/couchdb/etc/local.ini`` or
    ``/opt/couchdb/etc/local.d/10-admins.ini`` when CouchDB is installed from
    packages) should be appropriately secured and     readable only by system
    administrators::

        [admins]
        ;admin = mysecretpassword
        admin = -hashed-6d3c30241ba0aaa4e16c6ea99224f915687ed8cd,7f4a3e05e0cbc6f48a0035e3508eef90
        architect = -pbkdf2-43ecbd256a70a3a2f7de40d2374b6c3002918834,921a12f74df0c1052b3e562a23cd227f,10000

    Administrators can be added directly to the ``[admins]`` section, and when
    CouchDB is restarted, the passwords will be salted and encrypted. You may
    also use the HTTP interface to create administrator accounts; this way,
    you don't need to restart CouchDB, and there's no need to temporarily store
    or transmit passwords in plaintext. The HTTP
    ``/_node/{node-name}/_config/admins`` endpoint supports querying, deleting
    or creating new admin accounts:

    .. code-block:: http

        GET /_node/nonode@nohost/_config/admins HTTP/1.1
        Accept: application/json
        Host: localhost:5984

    .. code-block:: http

        HTTP/1.1 200 OK
        Cache-Control: must-revalidate
        Content-Length: 196
        Content-Type: application/json
        Date: Fri, 30 Nov 2012 11:37:18 GMT
        Server: CouchDB (Erlang/OTP)

    .. code-block:: json

        {
            "admin": "-hashed-6d3c30241ba0aaa4e16c6ea99224f915687ed8cd,7f4a3e05e0cbc6f48a0035e3508eef90",
            "architect": "-pbkdf2-43ecbd256a70a3a2f7de40d2374b6c3002918834,921a12f74df0c1052b3e562a23cd227f,10000"
        }

    If you already have a salted, encrypted password string (for example, from
    an old ini file, or from a different CouchDB server), then you can store
    the "raw" encrypted string, without having CouchDB doubly encrypt it.

    .. code-block:: http

        PUT /_node/nonode@nohost/_config/admins/architect?raw=true HTTP/1.1
        Accept: application/json
        Content-Type: application/json
        Content-Length: 89
        Host: localhost:5984

        "-pbkdf2-43ecbd256a70a3a2f7de40d2374b6c3002918834,921a12f74df0c1052b3e562a23cd227f,10000"

    .. code-block:: http

        HTTP/1.1 200 OK
        Cache-Control: must-revalidate
        Content-Length: 89
        Content-Type: application/json
        Date: Fri, 30 Nov 2012 11:39:18 GMT
        Server: CouchDB (Erlang/OTP)

        "-pbkdf2-43ecbd256a70a3a2f7de40d2374b6c3002918834,921a12f74df0c1052b3e562a23cd227f,10000"

    Further details are available in `security`, including configuring the work
    factor for ``PBKDF2``, and the algorithm itself at
    `PBKDF2 (RFC-2898) <http://tools.ietf.org/html/rfc2898>`_.

    .. versionchanged::
        1.4 `PBKDF2` server-side hashed salted password support added, now as a
        synchronous call for the ``_config/admins`` API.

.. _config/chttpd_auth:

Authentication Configuration
============================

.. config:section:: chttpd :: Clustered Authentication Configuration

    .. config:option:: require_valid_user :: Force user authentication

        .. versionchanged:: 3.2 moved from [couch_httpd_auth] to [chttpd] section

        When this option is set to ``true``, no requests are allowed from
        anonymous users. Everyone must be authenticated. ::

            [chttpd]
            require_valid_user = false

    .. config:option:: require_valid_user_except_for_up :: Force user auth (mostly)

        When this option is set to ``true``, no requests are allowed from
        anonymous users, *except* for the ``/_up`` endpoint. Everyone else must
        be authenticated. ::

            [chttpd]
            require_valid_user_except_for_up = false

.. config:section:: chttpd_auth :: Authentication Configuration

    .. versionchanged:: 3.2 These options were moved to [chttpd_auth] section:
                        `authentication_redirect`, `timeout`,
                        `auth_cache_size`, `allow_persistent_cookies`, `iterations`,
                        `min_iterations`, `max_iterations`, `secret`, `users_db_public`,
                        `x_auth_roles`, `x_auth_token`, `x_auth_username`,
                        `cookie_domain`, `same_site`.

    .. config:option:: allow_persistent_cookies :: Persistent cookies

        .. versionchanged:: 3.2 moved from [couch_httpd_auth] to [chttpd_auth] section

        When set to ``true``, CouchDB will set the Max-Age and Expires attributes
        on the cookie, which causes user agents (like browsers) to preserve the cookie
        over restarts. ::

            [chttpd_auth]
            allow_persistent_cookies = true

    .. config:option:: cookie_domain :: Cookie Domain

        .. versionadded:: 2.1.1
        .. versionchanged:: 3.2 moved from [couch_httpd_auth] to [chttpd_auth] section

        Configures the ``domain`` attribute of the ``AuthSession`` cookie. By default the
        ``domain`` attribute is empty, resulting in the cookie being set on CouchDB's domain. ::

            [chttpd_auth]
            cookie_domain = example.com

    .. config:option:: same_site :: SameSite

        .. versionadded:: 3.0.0
        .. versionchanged:: 3.2 moved from [couch_httpd_auth] to [chttpd_auth] section

        When this option is set to a non-empty value, a ``SameSite`` attribute is added to
        the ``AuthSession`` cookie. Valid values are ``none``, ``lax`` or ``strict``.::

            [chttpd_auth]
            same_site = strict

    .. config:option:: auth_cache_size :: Authentication cache

        .. versionchanged:: 3.2 moved from [couch_httpd_auth] to [chttpd_auth] section

        Number of :ref:`userctx_object` to cache in memory, to reduce disk
        lookups. ::

            [chttpd_auth]
            auth_cache_size = 50

    .. config:option:: authentication_redirect :: Default redirect for authentication requests

        .. versionchanged:: 3.2 moved from [couch_httpd_auth] to [chttpd_auth] section

        Specifies the location for redirection on successful authentication if
        a ``text/html`` response is accepted by the client (via an ``Accept``
        header). ::

            [chttpd_auth]
            authentication_redirect = /_utils/session.html

    .. config:option:: hash_algorithms :: Supported hash algorithms for cookie auth

        .. versionadded:: 3.3

        Sets the HMAC hash algorithm used for cookie authentication. You can provide a
        comma-separated list of hash algorithms. New cookie sessions or
        session updates are calculated with the first hash algorithm. All values in the
        list can be used to decode the cookie session. ::

            [chttpd_auth]
            hash_algorithms = sha256, sha

        .. note::
            You can select any hash algorithm the version of erlang used in your CouchDB
            install supports. The common list of available hashes might be: ::

                sha, sha224, sha256, sha384, sha512

            To retrieve a complete list of supported hash algorithms you can use our
            ``bin/remsh`` script and retrieve a full list of available hash algorithms
            with ``crypto:supports(hashs).``.

        .. warning::
            We do not recommend using the following hash algorithms: ::

                md4, md5

    .. config:option:: iterations :: PBKDF2 iterations count

        .. versionadded:: 1.3
        .. versionchanged:: 3.2 moved from [couch_httpd_auth] to [chttpd_auth] section

        The number of iterations for password hashing by the PBKDF2 algorithm.
        A higher  number provides better hash durability, but comes at a cost
        in performance for each request that requires authentication.
        When using hundreds of thousands of iterations, use session cookies, or the performance hit will be huge.
        (The internal hashing algorithm is SHA1, which affects the recommended number of iterations.) ::

            [chttpd_auth]
            iterations = 10000

    .. config:option:: min_iterations :: Minimum PBKDF2 iterations count

        .. versionadded:: 1.6
        .. versionchanged:: 3.2 moved from [couch_httpd_auth] to [chttpd_auth] section

        The minimum number of iterations allowed for passwords hashed by the
        PBKDF2 algorithm. Any user with fewer iterations is forbidden. ::

            [chttpd_auth]
            min_iterations = 100

    .. config:option:: max_iterations :: Maximum PBKDF2 iterations count

        .. versionadded:: 1.6
        .. versionchanged:: 3.2 moved from [couch_httpd_auth] to [chttpd_auth] section

        The maximum number of iterations allowed for passwords hashed by the
        PBKDF2 algorithm. Any user with greater iterations is forbidden. ::

            [chttpd_auth]
            max_iterations = 100000

    .. config:option:: password_regexp :: Password regular expressions

        .. versionadded:: 3.2

        A list of
        `Regular Expressions <https://erlang.org/doc/man/re.html#regexp_syntax>`_
        to check new/changed passwords.
        When set, new user passwords must **match** all RegExp in this list.

        A RegExp can be paired with a *reason text*:
        ``[{"RegExp", "reason text"}, ...]``.
        If a RegExp doesn't match, its *reason text* will be appended to the
        default reason of ``Password does not conform to requirements.`` ::

            [couch_httpd_auth]
            ; Password must be 10 chars long and have one or more uppercase and
            ; lowercase char and one or more numbers.
            password_regexp = [{".{10,}", "Min length is 10 chars."}, "[A-Z]+", "[a-z]+", "\\d+"]

    .. config:option:: proxy_use_secret :: Force proxy auth to use secret token

        .. versionchanged:: 3.2 moved from [couch_httpd_auth] to [chttpd_auth] section

        When this option is set to ``true``, the
        :option:`chttpd_auth/secret` option is required for
        :ref:`api/auth/proxy`. ::

            [chttpd_auth]
            proxy_use_secret = false

    .. config:option:: public_fields :: User documents public fields

        .. versionadded:: 1.4
        .. versionchanged:: 3.2 moved from [couch_httpd_auth] to [chttpd_auth] section

        A comma-separated list of field names in user documents (in
        :option:`couchdb/users_db_suffix`) that can be read by any
        user. If unset or not specified, authenticated users can only retrieve
        their own document. ::

            [chttpd_auth]
            public_fields = first_name, last_name, contacts, url

        .. note::
            Using the ``public_fields`` allowlist for user document properties
            requires setting the :option:`chttpd_auth/users_db_public`
            option to ``true`` (the latter option has no other purpose)::

                [chttpd_auth]
                users_db_public = true

    .. config:option:: secret :: Authentication secret token

        .. versionchanged:: 3.2 moved from [couch_httpd_auth] to [chttpd_auth] section

        The secret token is used for :ref:`api/auth/proxy` and for :ref:`api/auth/cookie`. ::

            [chttpd_auth]
            secret = 92de07df7e7a3fe14808cef90a7cc0d91

    .. config:option:: timeout :: Session timeout

        .. versionchanged:: 3.2 moved from [couch_httpd_auth] to [chttpd_auth] section

        Number of seconds since the last request before sessions will be
        expired. ::

            [chttpd_auth]
            timeout = 600

    .. config:option:: users_db_public :: Publish user documents

        .. versionadded:: 1.4
        .. versionchanged:: 3.2 moved from [couch_httpd_auth] to [chttpd_auth] section

        Allow all users to view user documents. By default, only admins may
        browse all users documents, while users may browse only their own
        document. ::

            [chttpd_auth]
            users_db_public = false

    .. config:option:: x_auth_roles :: Proxy Auth roles header

        .. versionchanged:: 3.2 moved from [couch_httpd_auth] to [chttpd_auth] section

        The HTTP header name (``X-Auth-CouchDB-Roles`` by default) that
        contains the list of a user's roles, separated by a comma. Used for
        :ref:`api/auth/proxy`. ::

            [chttpd_auth]
            x_auth_roles = X-Auth-CouchDB-Roles

    .. config:option:: x_auth_token :: Proxy Auth token header

        .. versionchanged:: 3.2 moved from [couch_httpd_auth] to [chttpd_auth] section

        The HTTP header name (``X-Auth-CouchDB-Token`` by default) containing
        the token used to authenticate the authorization. This token is an
        `HMAC-SHA1` created from the :option:`chttpd_auth/secret` and
        :option:`chttpd_auth/x_auth_username`. The secret key should be
        the same on the client and the CouchDB node. This token is optional if
        the value of the :option:`chttpd_auth/proxy_use_secret` option is
        not ``true``. Used for :ref:`api/auth/proxy`. ::

            [chttpd_auth]
            x_auth_token = X-Auth-CouchDB-Token

    .. config:option:: x_auth_username :: Proxy Auth username header

        .. versionchanged:: 3.2 moved from [couch_httpd_auth] to [chttpd_auth] section

        The HTTP header name (``X-Auth-CouchDB-UserName`` by default)
        containing the username. Used for :ref:`api/auth/proxy`. ::

            [chttpd_auth]
            x_auth_username = X-Auth-CouchDB-UserName

.. config:section:: jwt_auth :: JWT Authentication

    .. config:option:: required_claims :: Mandatory claims in JWT tokens

        This parameter is a comma-separated list of additional mandatory JWT claims
        that must be present in any presented JWT token. A :http:statuscode:`404`
        is sent if any are missing. ::

            [jwt_auth]
                required_claims = exp,iat
