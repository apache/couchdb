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

.. _cors:
.. _config/cors:

``[cors]`` :: CORS
==================

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
