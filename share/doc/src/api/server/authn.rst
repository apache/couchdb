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

`Basic authentication`_ (:rfc:`2617`) is a quick and simple way to be
authenticated by CouchDB. The main flaw of this simplicity is the need to send
user credentials with each request which may be insecure and hurts operations
performance (since CouchDB must compute password hash with every request):

.. code-block:: http

    GET / HTTP/1.1
    Authorization: Basic cm9vdDpyZWxheA==
    Host: localhost:5984
    Accept: application/json

.. code-block:: http

    HTTP/1.1 200 OK
    Server: CouchDB/1.3.0 (Erlang OTP/R15B02)
    Date: Mon, 03 Dec 2012 00:44:47 GMT
    Content-Type: application/json
    Content-Length: 177
    Cache-Control: must-revalidate

.. code-block:: javascript

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

For cookie authentication (:rfc:`2109`) CouchDB generates a one-time token that
the client can use for next requests to CouchDB. When CouchDB sees non expired
the token in a subsequent request, it will authenticate user by this token
without requesting the password again. By default, cookies are valid for
10 minutes, but it's :ref:`adjustable <config/couch_httpd_auth/timeout>`.
Also it's possible to make cookies
:ref:`persistent <config/couch_httpd_auth/allow_persistent_cookies>`

To obtain the first token and thus authenticate a user for the first time, the
`username` and `password` must be sent to the
:ref:`_session API <api/auth/session>`.

.. _api/auth/session:
.. _api/auth/session.post:

``POST /_session``
------------------

* **Method**: ``POST /_session``
* **Request**: User credentials
* **Response**: User information
* **Admin Privileges Required**: no
* **Query Arguments**:

  * **Argument**: next

    * **Description**: Enforces redirect after successful login
    * **Optional**: yes
    * **Value**: Relative path from server root

* **HTTP Headers**

  * **Header**: ``Content-Type``

    * **Description**: Credentials data format
    * **Optional**: no
    * **Value**: ``application/x-www-form-urlencoded``

* **Return Codes**:

  * **200**:
    Successfully authenticated

  * **302**:
    Redirect after successful authentication

  * **401**:
    Username or password wasn't recognized

Initiates new session for specified user credentials by providing `Cookie`
value. Credentials should be defined in ``application/x-www-form-urlencoded``
format with `name` and `password` fields.

.. code-block:: http

    POST /_session HTTP/1.1
    Host: localhost:5984
    Accept: application/json
    Content-Length: 24
    Content-Type: application/x-www-form-urlencoded

.. code-block:: text

    name=root&password=relax

In case of success will be returned next response:

.. code-block:: http

    HTTP/1.1 200 OK
    Set-Cookie: AuthSession=cm9vdDo1MEJCRkYwMjq0LO0ylOIwShrgt8y-UkhI-c6BGw; Version=1; Path=/; HttpOnly
    Server: CouchDB/1.3.0 (Erlang OTP/R15B02)
    Date: Mon, 03 Dec 2012 01:23:14 GMT
    Content-Type: application/json
    Content-Length: 43
    Cache-Control: must-revalidate

.. code-block:: javascript

    {"ok":true,"name":null,"roles":["_admin"]}

If ``next`` query parameter was provided the response will trigger redirection
to the specified location in case of successful authentication:

.. code-block:: http

    GET /_session?next=/blog/_design/sofa/_rewrite/recent-posts HTTP/1.1
    Host: localhost:5984
    Accept: application/json

.. code-block:: http

    HTTP/1.1 302 Moved Temporarily
    Set-Cookie: AuthSession=cm9vdDo1MEJDMDEzRTp7Vu5GKCkTxTVxwXbpXsBARQWnhQ; Version=1; Path=/; HttpOnly
    Server: CouchDB/1.3.0 (Erlang OTP/R15B02)
    Location: http://localhost:5984/blog/_design/sofa/_rewrite/recent-posts
    Date: Mon, 03 Dec 2012 01:32:46 GMT
    Content-Type: application/json
    Content-Length: 43
    Cache-Control: must-revalidate

.. code-block:: javascript

    {"ok":true,"name":null,"roles":["_admin"]}


.. _api/auth/session.get:

``GET /_session``
-----------------

* **Method**: ``GET /_session``
* **Request**: None
* **Response**: User information
* **Admin Privileges Required**: no
* **Query Arguments**:

  * **Argument**: basic

    * **Description**: Accept `Basic Auth` by requesting this resource
    * **Optional**: yes
    * **Value**: ``true``

* **Return Codes**:

  * **200**:
    Successfully authenticated.

  * **401**:
    Username or password wasn't recognized.

Returns complete information about authenticated user:

.. code-block:: http

    GET /_session HTTP/1.1
    Host: localhost:5984
    Accept: application/json
    Cookie: AuthSession=cm9vdDo1MEJDMDQxRDpqb-Ta9QfP9hpdPjHLxNTKg_Hf9w

.. code-block:: javascript

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

This information contains :ref:`userctx_object`, authentication method and
available ones and authentication database.

.. _api/auth/session.delete:

``DELETE /_session``
--------------------

* **Method**: ``DELETE /_session``
* **Request**: None
* **Response**: Status
* **Admin Privileges Required**: no

* **Return Codes**:

  * **200**:
    Successfully close session.

  * **401**:
    Username or password wasn't recognized.

Closes user's session. If everything is ok, the response is:

.. code-block:: javascript

    {"ok":true}


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


`Proxy authentication` is very useful in case when your application is already
uses some external authentication service and you won't duplicate users and
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

.. code-block:: http

    GET /_session HTTP/1.1
    Host: localhost:5984
    Accept: application/json
    Content-Type: application/json; charset=utf-8
    X-Auth-CouchDB-Roles: users,blogger
    X-Auth-CouchDB-UserName: foo

CouchDB sends the response:

.. code-block:: http

    HTTP/1.1 200 OK
    Cache-Control: must-revalidate
    Content-Length: 190
    Content-Type: application/json
    Date: Fri, 14 Jun 2013 10:16:03 GMT
    Server: CouchDB/1.3.0 (Erlang OTP/R15B03)

.. code-block:: javascript

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


Note, that you don't need to request :ref:`session <api/auth/session>` resource
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
    Server : CouchDB/1.3.1 (Erlang OTP/R16B)

.. code-block:: javascript

  {
    "ok": true,
    "info": {
      "authenticated": "oauth"
      "authentication_db": "_users",
      "authentication_handlers": ["oauth", "cookie", "default"]
    },
    "userCtx": {
      "name": "couchdb_username",
      "roles": []
    }
  }

There we'd requested the :ref:`_session <api/auth/session>` resource to ensure
that authentication was successful and target CouchDB username is correct.
Change the target URL to request required resource.
