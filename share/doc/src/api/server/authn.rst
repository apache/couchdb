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

======================
Authentication Methods
======================

The CouchDB Authentication methods provide an interface for obtaining
session and authorization data.

A list of the available methods and URL paths are provided below:

+--------+-------------------------+-------------------------------------------+
| Method | Path                    | Description                               |
+========+=========================+===========================================+
| GET    | /_session               | Returns cookie based login user           |
|        |                         | information                               |
+--------+-------------------------+-------------------------------------------+
| POST   | /_session               | Do cookie based user login                |
+--------+-------------------------+-------------------------------------------+
| DELETE | /_session               | Logout cookie based user                  |
+--------+-------------------------+-------------------------------------------+

.. note:: We're also strongly recommend you to
   :ref:`setup SSL <config/ssl>` to improve all authentication methods security.


.. _api/auth/basic:

Basic Authentication
====================

`Basic authentication`_ (:rfc:`2617`) is a quick and simple way to authenticate
with CouchDB. The main drawback is the need to send user credentials with each
request which may be insecure and could hurt operation performance (since
CouchDB must compute password hash with every request):

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
    Server: CouchDB/1.3.0 (Erlang OTP/R15B02)

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
authenticate user by this token without requesting the password again. By
default, cookies are valid for 10 minutes, but it's :ref:`adjustable
<config/couch_httpd_auth/timeout>`. Also it's possible to make cookies
:ref:`persistent <config/couch_httpd_auth/allow_persistent_cookies>`

To obtain the first token and thus authenticate a user for the first time, the
`username` and `password` must be sent to the
:ref:`_session API <api/auth/session>`.

.. _api/auth/session:

``_session``
------------

.. http:post:: /_session

  Initiates new session for specified user credentials by providing `Cookie`
  value.

  :<header Content-Type: - :mimetype:`application/x-www-form-urlencoded`
                         - :mimetype:`application/json`
  :query string next: Enforces redirect after successful login to the specified
    location. This location is relative from server root. *Optional*.
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
    Server: CouchDB/1.3.0 (Erlang OTP/R15B02)
    Set-Cookie: AuthSession=cm9vdDo1MEJCRkYwMjq0LO0ylOIwShrgt8y-UkhI-c6BGw; Version=1; Path=/; HttpOnly

    {"ok":true,"name":"root","roles":["_admin"]}

  If ``next`` query parameter was provided the response will trigger redirection
  to the specified location in case of successful authentication:

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
    Server: CouchDB/1.3.0 (Erlang OTP/R15B02)
    Set-Cookie: AuthSession=cm9vdDo1MEJDMDEzRTp7Vu5GKCkTxTVxwXbpXsBARQWnhQ; Version=1; Path=/; HttpOnly

    {"ok":true,"name":null,"roles":["_admin"]}


.. http:get:: /_session

  Returns complete information about authenticated user.
  This information contains :ref:`userctx_object`, authentication method and
  available ones and authentication database.

  :query boolean basic: Accept `Basic Auth` by requesting this resource.
    *Optional*.
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
    Server: CouchDB/1.3.0 (Erlang OTP/R15B02)
    Set-Cookie: AuthSession=cm9vdDo1MjA1NTBDMTqmX2qKt1KDR--GUC80DQ6-Ew_XIw; Version=1; Path=/; HttpOnly

    {
        "info": {
            "authenticated": "cookie",
            "authentication_db": "_users",
            "authentication_handlers": [
                "oauth",
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

  Closes user's session.

  :code 200: Successfully close session.
  :code 401: User wasn't authenticated.

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
    Server: CouchDB/1.3.0 (Erlang OTP/R15B02)
    Set-Cookie: AuthSession=; Version=1; Path=/; HttpOnly

    {
        "ok": true
    }


.. _api/auth/proxy:

Proxy Authentication
====================

.. note::
   To use this authentication method make sure that the
   ``{couch_httpd_auth, proxy_authentication_handler}`` value in added to
   the list of the active
   :ref:`authentication handlers <config/httpd/authentication_handlers>`:

   .. code-block:: ini

      [httpd]
      authentication_handlers = {couch_httpd_oauth, oauth_authentication_handler}, {couch_httpd_auth, cookie_authentication_handler}, {couch_httpd_auth, proxy_authentication_handler}, {couch_httpd_auth, default_authentication_handler}


`Proxy authentication` is very useful in case your application already uses
some external authentication service and you don't want to duplicate users and
their roles in CouchDB.

This authentication method allows creation of a :ref:`userctx_object` for
remotely authenticated user. By default, the client just need to pass specific
headers to CouchDB with related request:

- :ref:`X-Auth-CouchDB-UserName <config/couch_httpd_auth/x_auth_username>`:
  username;
- :ref:`X-Auth-CouchDB-Roles <config/couch_httpd_auth/x_auth_roles>`:
  list of user roles separated by a comma (``,``);
- :ref:`X-Auth-CouchDB-Token <config/couch_httpd_auth/x_auth_token>`:
  authentication token. Optional, but strongly recommended to
  :ref:`force token be required <config/couch_httpd_auth/proxy_use_secret>`
  to prevent requests from untrusted sources.

**Request**:

.. code-block:: http

    GET /_session HTTP/1.1
    Host: localhost:5984
    Accept: application/json
    Content-Type: application/json; charset=utf-8
    X-Auth-CouchDB-Roles: users,blogger
    X-Auth-CouchDB-UserName: foo

**Response**:

.. code-block:: http

    HTTP/1.1 200 OK
    Cache-Control: must-revalidate
    Content-Length: 190
    Content-Type: application/json
    Date: Fri, 14 Jun 2013 10:16:03 GMT
    Server: CouchDB/1.3.0 (Erlang OTP/R15B03)

    {
        "info": {
            "authenticated": "proxy",
            "authentication_db": "_users",
            "authentication_handlers": [
                "oauth",
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


.. _api/auth/oauth:

OAuth Authentication
====================

CouchDB supports OAuth 1.0 authentication (:rfc:`5849`). OAuth provides a method
for clients to access server resources  without sharing real credentials
(username and password).

First, :ref:`configure oauth <config/oauth>`, by setting consumer and token
with their secrets and binding token to real CouchDB username.

Probably, it's not good idea to work with plain curl, let use some scripting
language like Python:

.. code-block:: python

  #!/usr/bin/env python2
  from oauth import oauth # pip install oauth
  import httplib

  URL = 'http://localhost:5984/_session'
  CONSUMER_KEY = 'consumer1'
  CONSUMER_SECRET = 'sekr1t'
  TOKEN = 'token1'
  SECRET = 'tokensekr1t'

  consumer = oauth.OAuthConsumer(CONSUMER_KEY, CONSUMER_SECRET)
  token = oauth.OAuthToken(TOKEN, SECRET)
  req = oauth.OAuthRequest.from_consumer_and_token(
      consumer,
      token=token,
      http_method='GET',
      http_url=URL,
      parameters={}
  )
  req.sign_request(oauth.OAuthSignatureMethod_HMAC_SHA1(), consumer,token)

  headers = req.to_header()
  headers['Accept'] = 'application/json'

  con = httplib.HTTPConnection('localhost', 5984)
  con.request('GET', URL, headers=headers)
  resp = con.getresponse()
  print resp.read()

or Ruby:

.. code-block:: ruby

  #!/usr/bin/env ruby

  require 'oauth' # gem install oauth

  URL = 'http://localhost:5984'
  CONSUMER_KEY = 'consumer1'
  CONSUMER_SECRET = 'sekr1t'
  TOKEN = 'token1'
  SECRET = 'tokensekr1t'

  @consumer = OAuth::Consumer.new CONSUMER_KEY,
                                  CONSUMER_SECRET,
                                  {:site => URL}

  @access_token = OAuth::AccessToken.new(@consumer, TOKEN, SECRET)

  puts @access_token.get('/_session').body


Both snippets produces similar request and response pair:

.. code-block:: http

    GET /_session HTTP/1.1
    Host: localhost:5984
    Accept: application/json
    Authorization: OAuth realm="", oauth_nonce="81430018", oauth_timestamp="1374561749", oauth_consumer_key="consumer1", oauth_signature_method="HMAC-SHA1", oauth_version="1.0", oauth_token="token1", oauth_signature="o4FqJ8%2B9IzUpXH%2Bk4rgnv7L6eTY%3D"

.. code-block:: http

    HTTP/1.1 200 OK
    Cache-Control : must-revalidate
    Content-Length : 167
    Content-Type : application/json
    Date : Tue, 23 Jul 2013 06:51:15 GMT
    Server: CouchDB/1.3.0 (Erlang OTP/R15B02)


    {
      "ok": true,
      "info": {
        "authenticated": "oauth",
        "authentication_db": "_users",
        "authentication_handlers": ["oauth", "cookie", "default"]
      },
      "userCtx": {
        "name": "couchdb_username",
        "roles": []
      }
    }

There we request the :ref:`_session <api/auth/session>` resource to ensure
that authentication was successful and the target CouchDB username is correct.
Change the target URL to request required resource.
