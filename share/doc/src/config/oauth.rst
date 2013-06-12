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

.. _config/oauth:

``[oauth]`` :: OAuth Configuration
==================================

.. _config/oauth/oauth_consumer_secrets:
.. _config/oauth/oauth_token_secrets:
.. _config/oauth/oauth_token_users:

Store credentials within config
-------------------------------

To let users be authenticated by `OAuth` (:rfc:`5849`) method there is need to
setup three special sections in :ref:`configuration <config>` file:

1. Consumer secret:

::

  [oauth_consumer_secrets]
  example.com = sekr1t

2. Token secrets:

::

  [oauth_token_secrets]
  token1 = tokensekr1t

3. Tokens to users mapping:

::

  [oauth_token_users]
  token1 = couchdb_username


.. _config/couch_httpd_oauth:
.. _config/couch_httpd_oauth/use_users_db:

Store OAuth credentials within auth database
--------------------------------------------

.. versionadded:: 1.2: CouchDB is able to store OAuth credentials within users
   documents instead of config file.

::

  [couch_httpd_oauth]
  use_users_db = true

If set to ``true``, OAuth token and consumer secrets will be looked up
in the authentication database (``_user``). These secrets are stored in
a top level field named ``"oauth"`` in user documents. Example:

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

