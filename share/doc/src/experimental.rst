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
larger developer community. As such, please play around with these features
and send us feedback, thanks!

Use at your own risk! Do not rely on these features for critical
applications.

NodeJS Query Server
===================

The NodeJS Query Server is an alternative runtime environment for
the default JavaScript Query Server that runs on top of Node.JS and
not SpiderMonkey like the default Query Server.


Setup
-----

You will need to install Node.JS version 0.10.0 or later. See `Node.JS
Downloads <http://nodejs.org/download/>`_ for options.

1. Install the `couchjs-node` binary. Either via NPM:

.. code-block:: shell

    npm install -g couchjs

Or from the CouchDB sources:

.. code-block:: shell

    cd src/couchjs-node
    npm link

.. note:: **NPM in non-standard locations**

    If your Node.JS installation doesn’t store binaries in `/usr/local/bin`
    you will need to adjust CouchDB’s configuration. Add this to your `local.ini`
    file:

    .. code-block:: ini

      [query_servers]
      nodejs = /path/to/couchjs-node /path/to/couchdb/share/server/main.js

    And then restart your CouchDB instance.

2. Done. Now you can create design documents with the `language` parameter
set to `nodejs` and all JavaScript functions in this design document will
be processed by the Node.JS query server.

Enjoy!


