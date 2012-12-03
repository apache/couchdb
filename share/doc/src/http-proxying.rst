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

.. _http-proxying:

HTTP Proxying
=============

The HTTP proxy feature makes it easy to map and redirect different
content through your CouchDB URL. The proxy works by mapping a pathname
and passing all content after that prefix through to the configured
proxy address.

Configuration of the proxy redirect is handled through the
``[httpd_global_handlers]`` section of the CouchDB configuration file
(typically ``local.ini``). The format is:

.. code-block:: ini

    [httpd_global_handlers]
    PREFIX = {couch_httpd_proxy, handle_proxy_req, <<"DESTINATION">>}


Where:

-  ``PREFIX``

   Is the string that will be matched. The string can be any valid
   qualifier, although to ensure that existing database names are not
   overridden by a proxy configuration, you can use an underscore
   prefix.

-  ``DESTINATION``

   The fully-qualified URL to which the request should be sent. The
   destination must include the ``http`` prefix. The content is used
   verbatim in the original request, so you can also forward to servers
   on different ports and to specific paths on the target host.

The proxy process then translates requests of the form:

.. code-block:: text

    http://couchdb:5984/PREFIX/path

To:

.. code-block:: text

    DESTINATION/path

.. note::
   Everything after ``PREFIX`` including the required forward slash
   will be appended to the ``DESTINATION``.

The response is then communicated back to the original client.

For example, the following configuration:

.. code-block:: ini

    _google = {couch_httpd_proxy, handle_proxy_req, <<"http://www.google.com">>}

Would forward all requests for ``http://couchdb:5984/_google`` to the
Google website.

The service can also be used to forward to related CouchDB services,
such as Lucene:

.. code-block:: ini

    [httpd_global_handlers]
    _fti = {couch_httpd_proxy, handle_proxy_req, <<"http://127.0.0.1:5985">>}

.. note::
   The proxy service is basic. If the request is not identified by the
   ``DESTINATION``, or the remainder of the ``PATH`` specification is
   incomplete, the original request URL is interpreted as if the
   ``PREFIX`` component of that URL does not exist.

   For example, requesting ``http://couchdb:5984/_intranet/media`` when
   ``/media`` on the proxy destination does not exist, will cause the
   request URL to be interpreted as ``http://couchdb:5984/media``. Care
   should be taken to ensure that both requested URLs and destination
   URLs are able to cope.
