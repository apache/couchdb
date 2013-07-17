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

==========
Replicator
==========

.. _config/replicator:

``[replicator]`` :: Replicator Database Configuration
=====================================================

.. versionadded:: 1.2

.. _config/replicator/db:

``db``
------

Specifies replicator database name::

  [replicator]
  db = _replicator


.. _config/replicator/max_replication_retry_count:

``max_replication_retry_count``
-------------------------------

Maximum replication retry count can be a non-negative integer or "infinity"::

  [replicator]
  max_replication_retry_count = 10


.. _config/replicator/worker_batch_size:

``worker_batch_size``
---------------------

With lower batch sizes checkpoints are done more frequently. Lower batch sizes
also reduce the total amount of used RAM memory::

  [replicator]
  worker_batch_size = 500


.. _config/replicator/worker_processes:

``worker_processes``
--------------------

More worker processes can give higher network throughput but can also imply more
disk and network IO::

  [replicator]
  worker_processes = 4


.. _config/replicator/http_connections:

``http_connections``
--------------------

Maximum number of HTTP connections per replication::

  [replicator]
  http_connections = 20


.. _config/replicator/connection_timeout:

``connection_timeout``
----------------------

HTTP connection timeout per replication.
Even for very fast/reliable networks it might need to be increased if a remote
database is too busy::

  [replicator]
  connection_timeout = 30000


.. _config/replicator/retries_per_request:

``retries_per_request``
-----------------------

If a request fails, the replicator will retry it up to N times::

  [replicator]
  retries_per_request = 10


.. _config/replicator/socket_options:

``socket_options``
------------------

Some socket options that might boost performance in some scenarios:

- ``{nodelay, boolean()}``
- ``{sndbuf, integer()}``
- ``{recbuf, integer()}``
- ``{priority, integer()}``

See the `inet`_ Erlang module's man page for the full list of options::

  [replicator]
  socket_options = [{keepalive, true}, {nodelay, false}]

.. _inet: http://www.erlang.org/doc/man/inet.html#setopts-2


.. _config/replicator/cert_file:

``cert_file``
-------------

Path to a file containing the user's certificate::

  [replicator]
  cert_file = /full/path/to/server_cert.pem


.. _config/replicator/key_file:

``key_file``
------------

Path to file containing user's private PEM encoded key::

  [replicator]
  key_file = /full/path/to/server_key.pem


.. _config/replicator/password:

``password``
------------

String containing the user's password. Only used if the private keyfile is
password protected::

  [replicator]
  password = somepassword


.. _config/replicator/verify_ssl_certificates:

``verify_ssl_certificates``
---------------------------

Set to true to validate peer certificates::

  [replicator]
  verify_ssl_certificates = false


.. _config/replicator/ssl_trusted_certificates_file:

``ssl_trusted_certificates_file``
---------------------------------

File containing a list of peer trusted certificates (in the PEM format)::

  [replicator]
  ssl_trusted_certificates_file = /etc/ssl/certs/ca-certificates.crt


.. _config/replicator/ssl_certificate_max_depth:

``ssl_certificate_max_depth``
-----------------------------

Maximum peer certificate depth (must be set even if certificate validation is
off)::

  [replicator]
  ssl_certificate_max_depth = 3

