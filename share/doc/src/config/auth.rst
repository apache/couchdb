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

  A default CouchDB install provides admin-level access to all connecting users.
  This configuration is known as `Admin Party`, and is not recommended for
  in-production usage. You can crash the party simply by creating the first
  admin account. CouchDB server administrators and passwords are not stored
  in the ``_users`` database, but in the ``local.ini`` file, which should be
  appropriately secured and readable only by system administrators::

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

  .. code-block:: http

     GET /_config/admins HTTP/1.1
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

  If you already have a salted, encrypted password string (for example,
  from an old ``local.ini`` file, or from a different CouchDB server), then
  you can store the "raw" encrypted string, without having CouchDB doubly
  encrypt it.

  .. code-block:: http

     PUT /_config/admins/architect?raw=true HTTP/1.1
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

  Further details are available in `security`, including configuring the
  work factor for ``PBKDF2``, and the algorithm itself at
  `PBKDF2 (RFC-2898) <http://tools.ietf.org/html/rfc2898>`_.

  .. versionchanged:: 1.4 `PBKDF2` server-side hashed salted password support
     added, now as a synchronous call for the ``_config/admins`` API.


.. _config/couch_httpd_auth:

Authentication Configuration
============================

.. config:section:: couch_httpd_auth :: Authentication Configuration


  .. config:option:: allow_persistent_cookies :: Persistent cookies

    Makes cookies persistent if ``true``.

    ::

      [couch_httpd_auth]
      allow_persistent_cookies = false


  .. config:option:: auth_cache_size :: Authentication cache

    Number of :ref:`userctx_object` to cache in memory, to reduce disk lookups.

    ::

      [couch_httpd_auth]
      auth_cache_size = 50


  .. config:option:: authentication_db :: Users database

    Specifies the name of the system database for storing CouchDB users.

    ::

      [couch_httpd_auth]
      authentication_db = _users

    .. warning::
       If you change the database name, do not forget to remove or clean
       up the old database, since it will no longer be protected by CouchDB.


  .. config:option:: authentication_redirect :: Default redirect for authentication requests

    Specifies the location for redirection on successful authentication if a
    ``text/html`` response is accepted by the client (via an ``Accept`` header).

    ::

      [couch_httpd_auth]
      authentication_redirect = /_utils/session.html


  .. config:option:: iterations :: PBKDF2 iterations count

    .. versionadded:: 1.3

    The number of iterations for password hashing by the PBKDF2 algorithm.
    A higher  number provides better hash durability, but comes at a cost in
    performance for each request that requires authentication.

    ::

      [couch_httpd_auth]
      iterations = 10000


  .. config:option:: proxy_use_secret :: Force proxy auth use secret token

    When this option is set to ``true``,  the :option:`couch_httpd_auth/secret`
    option is required for :ref:`api/auth/proxy`.

    ::

      [couch_httpd_auth]
      proxy_use_secret = false


  .. config:option:: public_fields :: User documents public fields

    .. versionadded:: 1.4

    A comma-separated list of field names in user documents (in
    :option:`couch_httpd_auth/authentication_db`) that can
    be read by any user. If unset or not specified, authenticated users can only
    retrieve their own document.

    ::

      [couch_httpd_auth]
      public_fields = first_name, last_name, contacts, url

    .. note::
       Using the ``public_fields`` whitelist for user document properties
       requires setting the :option:`couch_httpd_auth/users_db_public`
       option to ``true`` (the latter option has no other purpose)::

         [couch_httpd_auth]
         users_db_public = true


  .. config:option:: require_valid_user :: Force user authentication

    When this option is set to ``true``, no requests are allowed from anonymous
    users. Everyone must be authenticated.

    ::

      [couch_httpd_auth]
      require_valid_user = false


  .. config:option:: secret :: Proxy Auth secret token

    The secret token used for :ref:`api/auth/proxy` method.

    ::

      [couch_httpd_auth]
      secret = 92de07df7e7a3fe14808cef90a7cc0d91


  .. config:option:: timeout :: Session timeout

    Number of seconds since the last request before sessions will be expired.

    ::

      [couch_httpd_auth]
      timeout = 600


  .. config:option:: users_db_public :: Publish user documents

    .. versionadded:: 1.4

    Allow all users to view user documents. By default, only admins may browse
    all users documents, while users may browse only their own document.

    ::

      [couch_httpd_auth]
      users_db_public = false


  .. config:option:: x_auth_roles :: Proxy Auth roles header

    The HTTP header name (``X-Auth-CouchDB-Roles`` by default) that contains the
    list of a user's roles, separated by a comma. Used for
    :ref:`api/auth/proxy`.

    ::

      [couch_httpd_auth]
      x_auth_roles = X-Auth-CouchDB-Roles


  .. config:option:: x_auth_token :: Proxy Auth token header

    The HTTP header name (``X-Auth-CouchDB-Token`` by default) containing the
    token used to authenticate the authorization. This token is an `HMAC-SHA1`
    created from the :option:`couch_httpd_auth/secret` and
    :option:`couch_httpd_auth/x_auth_username`. The secret key should be
    the same on the client and the CouchDB node. This token is optional if
    the value of the :option:`couch_httpd_auth/proxy_use_secret` option is not
    ``true``. Used for :ref:`api/auth/proxy`.

    ::

      [couch_httpd_auth]
      x_auth_roles = X-Auth-CouchDB-Token


  .. config:option:: x_auth_username :: Proxy Auth username header

    The HTTP header name (``X-Auth-CouchDB-UserName`` by default) containing the
    username. Used for :ref:`api/auth/proxy`.

    ::

      [couch_httpd_auth]
      x_auth_username = X-Auth-CouchDB-UserName


.. _config/couch_httpd_oauth:

HTTP OAuth Configuration
========================

.. config:section:: couch_httpd_oauth :: HTTP OAuth Configuration

  .. versionadded:: 1.2

  .. config:option:: use_users_db

  CouchDB is able to store OAuth credentials within user documents instead of
  config file by using this option::

    [couch_httpd_oauth]
    use_users_db = true

  If set to ``true``, OAuth token and consumer secrets will be looked up in the
  :option:`authentication database <couch_httpd_auth/authentication_db>`.
  These secrets are stored in a top level field named ``"oauth"`` in user
  documents, as below.

  .. code-block:: javascript

      {
          "_id": "org.couchdb.user:joe",
          "type": "user",
          "name": "joe",
          "password_sha": "fe95df1ca59a9b567bdca5cbaf8412abd6e06121",
          "salt": "4e170ffeb6f34daecfd814dfb4001a73"
          "roles": ["foo", "bar"],
          "oauth": {
              "consumer_keys": {
                  "consumerKey1": "key1Secret",
                  "consumerKey2": "key2Secret"
              },
              "tokens": {
                  "token1": "token1Secret",
                  "token2": "token2Secret"
             }
          }
      }


.. _config/oauth:

OAuth Configuration
===================

.. config:section:: oauth_* :: OAuth Configuration

  To let users be authenticated by :ref:`api/auth/oauth` (:rfc:`5849`), three
  special sections must be set up in the :ref:`configuration <config>` file:

  1. The Consumer secret:

  ::

    [oauth_consumer_secrets]
    consumer1 = sekr1t

  2. Token secrets:

  ::

    [oauth_token_secrets]
    token1 = tokensekr1t

  3. A mapping from tokens to users:

  ::

    [oauth_token_users]
    token1 = couchdb_username
