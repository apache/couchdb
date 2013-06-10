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

.. _config/vhosts:

``[vhosts]`` :: Virtual Hosts
=============================

CouchDB can map requests to different locations based on the ``Host`` header,
even if they arrive on the some inbound IP address.

This allows different virtual hosts on the same machine to map to different
databases or design documents, etc. The most common use case is to map a
virtual host to a Rewrite Handler, to provide full control over the
application's URIs.

To add a virtual host, add a CNAME pointer to the DNS for your domain
name. For development and testing, it is sufficient to add an entry in
the hosts file, typically `/etc/hosts`` on Unix-like operating systems:

.. code-block:: text

   # CouchDB vhost definitions, refer to local.ini for further details
   127.0.0.1       sofa.couchdb

Test that this is working:

.. code-block:: bash

   $ ping -n 2 sofa.couchdb
   PING sofa.couchdb (127.0.0.1) 56(84) bytes of data.
   64 bytes from localhost (127.0.0.1): icmp_req=1 ttl=64 time=0.025 ms
   64 bytes from localhost (127.0.0.1): icmp_req=2 ttl=64 time=0.051 ms

Finally, add an entry to your :ref:`configuration file <config>` in the
``[vhosts]`` section::

  [vhosts]
  sofa.couchdb:5984 = /sofa/_design/sofa/_rewrite

If your CouchDB is listening on the default HTTP port, or is sitting
behind a proxy, then don't specify a port number in the `vhost` key.

With the above setup, a request to ``http://sofa.couchdb:5984/sweet-o``
will be mapped to ``http://127.0.0.1:5984/sofa/_design/sofa/_rewrite/sweet-o``
