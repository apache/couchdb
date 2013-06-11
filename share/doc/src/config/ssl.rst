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

.. _config/ssl:

``[ssl]`` :: Secure Socket Level Options
========================================

CouchDB supports SSL natively. All your secure connection needs can
now be served without needing to setup and maintain a separate proxy server
that handles SSL.

SSL setup can be tricky, but the configuration in CouchDB was designed
to be as easy as possible. All you need is two files; a certificate and
a private key. If you bought an official SSL certificate from a
certificate authority, both should be in your possession already.

If you just want to try this out and don't want to pay anything upfront,
you can create a self-signed certificate. Everything will work the same,
but clients will get a warning about an insecure certificate.

You will need the `OpenSSL`_ command line tool installed. It probably
already is.

.. code-block:: bash

    shell> mkdir /etc/couchdb/cert
    shell> cd /etc/couchdb/cert
    shell> openssl genrsa > privkey.pem
    shell> openssl req -new -x509 -key privkey.pem -out couchdb.pem -days 1095
    shell> chmod 600 privkey.pem couchdb.pem
    shell> chown couchdb privkey.pem couchdb.pem

Now, you need to edit CouchDB's configuration, either by editing your
``local.ini`` file or using the ``/_config`` API calls or the
configuration screen in Futon. Here is what you need to do in
``local.ini``, you can infer what needs doing in the other places.

At first, :ref:`enable HTTPS daemon <config/daemons/httpsd>`::

  [daemons]
  httpsd = {couch_httpd, start_link, [https]}

Next, under ``[ssl]`` section setup newly generated certificates::

  [ssl]
  cert_file = /etc/couchdb/cert/couchdb.pem
  key_file = /etc/couchdb/cert/privkey.pem

For more information please read `certificates HOWTO`_.

Now start (or restart) CouchDB. You should be able to connect to it
using HTTPS on port 6984:

.. code-block:: bash

    shell> curl https://127.0.0.1:6984/
    curl: (60) SSL certificate problem, verify that the CA cert is OK. Details:
    error:14090086:SSL routines:SSL3_GET_SERVER_CERTIFICATE:certificate verify failed
    More details here: http://curl.haxx.se/docs/sslcerts.html

    curl performs SSL certificate verification by default, using a "bundle"
    of Certificate Authority (CA) public keys (CA certs). If the default
    bundle file isn't adequate, you can specify an alternate file
    using the --cacert option.
    If this HTTPS server uses a certificate signed by a CA represented in
    the bundle, the certificate verification probably failed due to a
    problem with the certificate (it might be expired, or the name might
    not match the domain name in the URL).
    If you'd like to turn off curl's verification of the certificate, use
    the -k (or --insecure) option.

Oh no what happened?! — Remember, clients will notify their users that
your certificate is self signed. ``curl`` is the client in this case and
it notifies you. Luckily you trust yourself (don't you?) and you can
specify the ``-k`` option as the message reads:

.. code-block:: bash

    shell> curl -k https://127.0.0.1:6984/
    {"couchdb":"Welcome","version":"1.3.0"}

All done.

.. _`certificates HOWTO`: http://www.openssl.org/docs/HOWTO/certificates.txt
.. _OpenSSL: http://www.openssl.org/

There are more options are under ``[ssl]`` section.

.. _config/ssl/cacert_file:

``cacert_file`` :: CA Certificate file
--------------------------------------

Path to file containing PEM encoded CA certificates (trusted certificates used
for verifying a peer certificate). May be omitted if you do not want to verify
the peer::

  [ssl]
  cacert_file = /etc/ssl/certs/ca-certificates.crt


.. _config/ssl/cert_file:

``cert_file`` :: Certificate file
---------------------------------

Path to a file containing the user's certificate::

  [ssl]
  cert_file = /etc/couchdb/cert/couchdb.pem


.. _config/ssl/key_file:

``key_file`` :: Certificate key file
------------------------------------

Path to file containing user's private PEM encoded key::

  [ssl]
  key_file = /etc/couchdb/cert/privkey.pem


.. _config/ssl/password:

``password`` :: Certificate key password
----------------------------------------

String containing the user's password. Only used if the private keyfile is
password protected::

  [ssl]
  password = somepassword


.. _config/ssl/ssl_certificate_max_depth:

``ssl_certificate_max_depth`` :: Maximum peer certificate depth
---------------------------------------------------------------

Maximum peer certificate depth (must be set even if certificate validation is
off)::

  [ssl]
  ssl_certificate_max_depth = 1


.. _config/ssl/verify_fun:

``verify_fun`` :: SSL verification function
-------------------------------------------

The verification fun (optional) if not specified, the default
verification fun will be used::

  [ssl]
  verify_fun = {Module, VerifyFun}


.. _config/ssl/verify_ssl_certificates:

``verify_ssl_certificates`` :: Enable certificate verification
--------------------------------------------------------------

Set to `true` to validate peer certificates::

  [ssl]
  verify_ssl_certificates = false

