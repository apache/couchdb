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

.. _fauxton/install:

=============
Fauxton Setup
=============

Fauxton is included with CouchDB 2.0, so make sure CouchDB is running, then go to::

    http://127.0.0.1:5984/_utils/

You can also upgrade to the latest version of Fauxton by using npm::

    $ npm install -g fauxton
    $ fauxton

(Recent versions of `node.js`_ and `npm`_ are required.)

.. _node.js: http://nodejs.org/
.. _npm: https://npmjs.org/doc/README.html

Fauxton Visual Guide
====================
You can find the Visual Guide here:
    http://couchdb.apache.org/fauxton-visual-guide

Development Server
==================

Recent versions of `node.js`_ and `npm`_ are required.

.. _node.js: http://nodejs.org/
.. _npm: https://npmjs.org/doc/README.html

Using the dev server is the easiest way to use Fauxton, specially when developing for it::

    $ git clone https://github.com/apache/couchdb-fauxton.git
    $ npm install && npm run dev

Understanding Fauxton Code layout
=================================

Each bit of functionality is its own separate module or addon.

All core modules are stored under `app/module` and any addons that are optional
are under `app/addons`.

We use `backbone.js`_ and `Backbone.layoutmanager`_ quite heavily, so best to
get an idea how they work. Its best at this point to read through a couple of
the modules and addons to get an idea of how they work.

Two good starting points are `app/addon/config` and `app/modules/databases`.

Each module must have a `base.js` file, this is read and compile when Fauxton is
deployed.

The `resource.js` file is usually for your ``Backbone.Models`` and
``Backbone.Collections``, `view.js` for your ``Backbone.Views``.

The `routes.js` is used to register a url path for your view along with what
layout, data, breadcrumbs and api point is required for the view.

.. _backbone.js: http://backbonejs.org/
.. _Backbone.layoutmanager: https://github.com/tbranyen/backbone.layoutmanager

ToDo items
----------

Checkout `JIRA` or `GitHub Issues`_  for a list of items to do.

.. _JIRA: https://issues.apache.org/jira/browse/COUCHDB/component/12320406
.. _GitHub Issues: https://github.com/apache/couchdb-fauxton/issues
