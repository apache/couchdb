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

.. _cluster/tls_erlang_distribution:

=======================
TLS Erlang Distribution
=======================
The main purpose is specifically to allow using TLS for Erlang distribution
between nodes, with the ability to connect to some nodes using TCP as well.
TLS distribution will enhance data security during data migration between
nodes.

This section describes how to enable TLS distribution for additional
verification and security.

Reference: `Using TLS for Erlang Distribution`_

.. _Using TLS for Erlang Distribution: https://erlang.org/doc/apps/ssl/ssl_distribution.html

Generate Certificate
====================
To distribute using TLS, appropriate certificates need to be provided.
In the following example (couch_dist.conf), the cert.pem certificate must be
trusted by a root certificate known to the server, and the erlserver.pem file
contains the "certificate" and its "private key".

    .. code-block:: text

        [{server,
          [{cacertfile, "</absolute_path/to/ca-cert.pem>"},
           {certfile,   "</absolute_path/to/erlserver.pem>"},
           {secure_renegotiate, true},
           {verify, verify_peer},
           {fail_if_no_peer_cert, true}]},
         {client,
          [{cacertfile, "</absolute_path/to/ca-cert.pem>"},
           {keyfile,    "</absolute_path/to/key.pem>"},
           {certfile,   "</absolute_path/to/cert.pem>"},
           {secure_renegotiate, true},
           {verify, verify_peer}]}].

You can use ``{verify, verify_peer}`` to enable verification,
but it requires appropriate certificates to verify.

This is an example of generating certificates.

    .. code-block:: bash

        $ git clone https://github.com/rnewson/elixir-certs
        $ cd elixir-certs
        $ ./certs self-signed \
            --out-cert ca-cert.pem --out-key ca-key.pem \
            --template root-ca \
            --subject "/CN=CouchDB Root CA"
        $./certs create-cert \
            --issuer-cert ca-cert.pem --issuer-key ca-key.pem \
            --out-cert cert.pem --out-key key.pem \
            --template server \
            --subject "/CN=<hostname>"
        $ cat key.pem cert.pem >erlserver.pem

    .. note::
        * The above examples are **not** an endorsement of specific expiration limits, key sizes, or algorithms.
        * If option ``verify_peer`` is set, the ``server_name_indication`` option should also be specified.
        * The option ``{fail_if_no_peer_cert, true}`` should only be used on the server side in OTP 26,
          for previous versions it can be specified both on the server side and client side.
        * When generating certificates, make sure Common Name (FQDN) should be different in CA certificate and certificate.
          Also, FQDN in the certificate should be the same as the hostname.

Config Settings
===============
To enable TLS distribution, make sure to set custom parameters in ``vm.args``.

    .. code-block:: text

        # Don't forget to override the paths to point to your cert and conf file!

        -proto_dist couch
        -couch_dist no_tls \"clouseau@127.0.0.1\"
        -ssl_dist_optfile </absolute_path/to/couch_dist.conf>

    .. note::
       * The default value of ``no_tls`` is ``false``. If the user does not
         set any ``no_tls`` flag, all nodes will use ``TCP``.
       * To ensure "search" works, make sure to set ``no_tls`` option for the
         ``clouseau`` node. By default, this will be ``"clouseau@127.0.0.1"``.

The ``no_tls`` flag can have these values:

#. Use ``TLS`` only, set to ``false`` (default value), such as:

    .. code-block:: text

        -couch_dist no_tls false

#. Use ``TCP`` only, set to ``true``, such as:

    .. code-block:: text

        -couch_dist no_tls true

#. Specify some nodes to use ``TCP``, others to use ``TLS``, such as:

    .. code-block:: text

        # Specify node1 and node2 to use TCP, others use TLS

        -couch_dist no_tls '"node1@127.0.0.1"'
        -couch_dist no_tls \"node2@127.0.0.1\"

    .. code-block:: text

        # Any nodes end with "@127.0.0.1" will use TCP, others use TLS

        -couch_dist no_tls \"*@127.0.0.1\"

    .. note::
       **Asterisk(*)**: matches a sequence of zero or more occurrences of the regular
       expression.

       **Question mark(?)**: matches zero or one occurrences of the regular expression.

Connect to Remsh
================
Start Erlang using a remote shell connected to Node.

* If the node uses ``TCP``:

    .. code-block:: bash

        $ ./remsh

* If the node uses ``TLS``:

    .. code-block:: bash

        $ ./remsh -t </absolute_path/to/couch_dist.conf>
