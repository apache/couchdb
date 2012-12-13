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

.. _cors:

================
Introducing CORS
================

CORS, or "Cross-origin resource sharing" allows a resource such as a web
page running JavaScript inside a browser, to make AJAX requests
(XMLHttpRequests) to a different domain, without compromising the security
of either party.

A typical use case is to have a static website hosted on a CDN make
requests to another resource, such as a hosted CouchDB instance. This
avoids needing an intermediary proxy, using JSONP or similar workarounds
to retrieve and host content.

While CouchDB's integrated HTTP server and support for document attachments
makes this less of a constraint for pure Couch projects, there are many
cases where separating the static content from the database access is
desirable, and CORS makes this very straightforwards indeed.

By supporting CORS functionality, a CouchDB instance can accept direct
connections to protected DBs and instances, without the browser
functionality being blocked due to the same origin constraint. CORS is
widely supported today on over 90% of browsers.

CORS support is provided as experimental functionality in 1.3.0, and as
such will need to be enabled specifically in CouchDB's configuration.

.. versionadded:: 1.3.0 added CORS support see JIRA :issue:`431`

Features
========

-  Simple requests for a couchdb instance
-  Preflight requests for a couchdb instance
-  Configuration for a specific CouchDB vhost
-  All origins are excluded by default

Configuration
=============

Enabling CORS
-------------

To enable CORS support, you need to set the option
``enable_cors = true`` in the ``[httpd]`` section of ``local.ini``, and
``[cors]`` section with ``origins = *``. Note that by default, no
origins are accepted, you must either use a wildcard or whitelist.

.. code-block:: ini

    [httpd]
    enable_cors = true

    [cors]
    origins = *

Passing Credentials
-------------------

By default, neither authentication headers nor cookies are included
in requests and responses. To do so requires both setting
`XmlHttpRequest.withCredentials = true` on the request object in the
browser, and additionally enabling it within CouchDB.

.. code-block:: ini

    [cors]
    credentials = true

CouchDB will respond to a credentials-enabled CORS request with an
additional header, `Access-Control-Allow-Credentials=true`.

Tightening Access
-----------------

Restricting by Protocol, Host and optional Port
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: ini

    [cors]
    ; List of origins, separated by a comma (protocol, host, optional port)
    ; refer to http://tools.ietf.org/html/rfc6454 for specification
    origins = http://localhost, https://localhost, http://www.number10.gov.uk:80

Restricting Accepted Methods
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Methods may be further restricted. These apply to all CORS-enabled instances.

.. code-block:: ini

    [cors]
    ; List of accepted methods, comma-separated
    ; refer to http://tools.ietf.org/html/rfc2616, rfc2817, rfc5789
    methods = GET, POST, PUT, DELETE


Configuration per vhost
-----------------------

All the above parameters `origins`, `methods`, `headers`, `credentials`
may be individually configured per CouchDB vhost. For example, the
configuration section for `http://example.com/` would be contained in:

.. code-block:: ini

    [cors:http://example.com]
    credentials = false
    origins = *
    methods = GET, PUT, HEAD

Useful References
-----------------

- Original JIRA `implementation ticket <https://issues.apache.org/jira/browse/COUCHDB-431>`_

Standards and References:

- IETF RFCs relating to methods `rfc2618 <http://tools.ietf.org/html/rfc2616>`_,
  `rfc2817 <http://tools.ietf.org/html/rfc2817>`_, and
  `rfc5789 <http://tools.ietf.org/html/rfc5789>`_.
- IETF RFC for `Web Origins <http://tools.ietf.org/html/rfc6454>`_
- W3C `CORS standard <http://www.w3.org/TR/cors>`_

Mozilla Developer Network Resources:

- `Same origin policy for URIs <https://developer.mozilla.org/en-US/docs/Same-origin_policy_for_file:_URIs>`_
- `HTTP Access Control <https://developer.mozilla.org/En/HTTP_access_control>`_
- `Server-side Access Control <https://developer.mozilla.org/En/Server-Side_Access_Control>`_
- `Javascript same origin policy <https://developer.mozilla.org/en-US/docs/Same_origin_policy_for_JavaScript>`_


Client-side CORS support and usage:

- `CORS browser support matrix <http://caniuse.com/cors>`_
- `COS tutorial <http://www.html5rocks.com/en/tutorials/cors/>`_
- `<http://hacks.mozilla.org/2009/07/cross-site-xmlhttprequest-with-cors/>`_

