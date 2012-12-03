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

.. _pretty_urls:

===========
Pretty URLs
===========

CouchDB incorporates a straightforwards URL routing and rewriting engine.
While it is not as comprehensive as a front-end proxy, it caters for most
needs out of the box, including using virtual hostnames to map URLs to
specific databases, and provides more complex use cases through a rewriting
engine.

This `post
<http://blog.couchbase.com/what%E2%80%99s-new-apache-couchdb-011-%E2%80%94-part-one-nice-urls-rewrite-rules-and-virtual-hosts>`_ has a detailed example of a combined vhost and rewriter configuration.

Virtual Hosts
=============

CouchDB, since 0.11.0, can map requests to different locations based on
the ``Host`` header, even if they arrive on the some inbound IP address.

This allows different virtual hosts on the same machine to map to different
databases or design documents, etc. The most common use case is to map a
virtual host to a Rewrite Handler, to provide full control over the
application's URIs.

To add a virtual host, add a CNAME pointer to the DNS for your domain
name. For development and testing, it is sufficient to add an entry in
the hosts file, typically `/etc/hosts`` on Unix-like operating systems:

.. code-block:: bash

    # CouchDB vhost definitions, refer to local.ini for further details
    127.0.0.1       sofa.couchdb

Test that this is working:

.. code-block:: bash

    $ ping sofa.couchdb
    PING sofa.couchdb (127.0.0.1) 56(84) bytes of data.
    64 bytes from localhost.localdomain (127.0.0.1): icmp_req=1 ttl=64 time=0.025 ms
    64 bytes from localhost.localdomain (127.0.0.1): icmp_req=2 ttl=64 time=0.051 ms
    ^C

Finally, add an entry to your :ref:`configuration file <configuring>` in the ``[vhosts]``
section:

.. code-block:: ini

    [vhosts]
    sofa.couchdb:5984 = /sofa/_design/sofa/_rewrite

If your CouchDB is listening on the default HTTP port, or is sitting
behind a proxy, then don't specify a port number in the vhost key.

With the above setup, a request to ``http://sofa.couchdb:5984/sweet-o``
will be mapped to
``http://127.0.0.1:5984/sofa/_design/sofa/_rewrite/sweet-o``

.. versionadded:: 0.11.0 added `vhosts` functionality

HTTP Rewrite Handler
====================

Following on from `virtual hosts`_, CouchDB includes a custom URL rewriter.
All rewriting is done from ``/dbname/_design/ddocname/_rewrite`` by default.

The rewriter is flexible, and can handle methods and custom query formats.

Each rule should be in the ``rewrites`` top-level key of the design doc.
Example of a complete rule :

.. code-block:: json

    {
        ....
        "rewrites": [
        {
            "from": "",
            "to": "index.html",
            "method": "GET",
            "query": {}
        }
        ]
    }


**from**: is the path rule used to bind current uri to the rule. It
uses pattern matching for that.

**to**: rule to rewrite an url. It can contain variables depending on
binding variables discovered during pattern matching and query args
(url args and from the query member.)

**method**: method to bind the request method to the rule. If method
is missing, any method will be matched in the rewrite.

**query**: optional query arguments, that may contain dynamic variables,
by binding keys in the to be used with the matching URL.

``to`` and ``from`` are paths with patterns. The pattern can be strings starting
with  ``:`` or ``*``, for example ``/somepath/:var/*``.

The pattern matching is done by first matching the request method to a
rule. Then it will try to match the path to one specific rule. If no rule
match, then a 404 error is displayed.

The path is converted into an erlang list, by regex splitting on ``/``. Each
variable is converted into an atom. The subsequent pattern matching step is
done by splitting ``/`` in the request url into a list of atoms. A string
pattern will match the equivalent token. The ``*`` atom will match any number
of tokens, but may only be present as the last pattern in the path. If all
tokens are matched, and all path terms have been consumed, then the overall
path specification matches.

Once a matching ``from`` rule is found we rewrite the request url using the
``from``, ``to``, and ``query`` members. Each identified token will be reused
within the rule, and in the subsequent query if required. The identified
tokens are matched to the rule and will replace var. If ``*`` is found in
the rule it will contain any remaining suffix.

The rewriter is re-entrant, and has a configurable recursion limit, set
by default at 100.
