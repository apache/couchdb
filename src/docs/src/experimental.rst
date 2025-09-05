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

.. _experimental:

=====================
Experimental Features
=====================

This is a list of experimental features in CouchDB. They are included in
a release because the development team is requesting feedback from the
larger developer community. As such, please play around with these
features and send us feedback, thanks!

Use at your own risk! Do not rely on these features for critical applications.

Content-Security-Policy (CSP) Header Support for /_utils (Fauxton)
==================================================================

This will just work with Fauxton. You can enable it in your config: you
can enable the feature in general and change the default header that is
sent for everything in /_utils.

    .. code-block:: ini

        [csp]
        enable = true

Then restart CouchDB.

.. _nouveauconfig:

Nouveau Server (new Apache Lucene integration)
==============================================

Enable nouveau in config and run the Java service.

    .. code-block:: ini

        [nouveau]
        enable = true

Have fun!

Couch Stats Resource Tracker (CSRT)
===================================

Couch Stats Resource Tracker, aka CSRT, is an experimental real time stats
tracking system designed to augment the existing the existing
`couch_stats:increment_counter` invocations with real time process local stats
collection, querying, RPC deltas, and powerful filtered logging of resources
induced by HTTP and RPC worker requests.

.. toctree::
    :caption: Couch Stats Resource Tracker Links

    csrt/index

.. seealso::

    :doc:`/config/csrt`
