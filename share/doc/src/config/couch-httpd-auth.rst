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

.. _config/couch_httpd_auth:

``[couch_httpd_auth]`` :: Authentication Configuration
======================================================

These options are under ``[couch_httpd_auth]`` section.


.. _config/couch_httpd_auth/allow_persistent_cookies:

``allow_persistent_cookies`` :: Persistent cookies
--------------------------------------------------

Makes cookies persistent if ``true``::

  [couch_httpd_auth]
  allow_persistent_cookies = false


.. _config/couch_httpd_auth/auth_cache_size:

``auth_cache_size`` :: Authentication cache
-------------------------------------------

Number of :ref:`userctx_object` to cache in memory to reduce disk lookups::

  [couch_httpd_auth]
  auth_cache_size = 50


.. _config/couch_httpd_auth/authentication_db:

``authentication_db`` :: Users database
---------------------------------------

Specifies name of the system database for storing CouchDB users::

  [couch_httpd_auth]
  authentication_db = _users

.. warning:: If there was any reasons to change this name for you, don't forget
   to remove/cleanup old database since it wouldn't be protected by CouchDB
   anymore.


.. _config/couch_httpd_auth/authentication_redirect:

``authentication_redirect`` :: Default redirect for authentication requests
---------------------------------------------------------------------------

Specifies location for redirection on successful authentication if ``text/html``
response accepted by client (via ``Accept`` header)::

  [couch_httpd_auth]
  authentication_redirect = /_utils/session.html


.. _config/couch_httpd_auth/iterations:

``iterations`` :: PBKDF2 iterations count
-----------------------------------------

.. versionadded:: 1.3

Number of iterations for password hashing by PBKDF2 algorithm. Higher number
provides better hash durability, but with cost of performance on each request
that requires authentication::

  [couch_httpd_auth]
  iterations = 10000


.. _config/couch_httpd_auth/require_valid_user:

``require_valid_user`` :: Force users authentication
----------------------------------------------------

When this option ``true`` no requests allowed from anonymous users - everyone
should be authenticated::

  [couch_httpd_auth]
  require_valid_user = false


.. _config/couch_httpd_auth/timeout:

``timeout`` :: Session timeout
------------------------------

Number of seconds since the last request before session will be expired::

  [couch_httpd_auth]
  timeout = 600

