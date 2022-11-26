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

.. default-domain:: config
.. highlight:: ini

=======
QuickJS
=======

Configure QuickJS Javascript Engine.

QuickJS is a new Javascript Engine installed alongside the default
Spidermonkey engine. It's disabled by default, but may be enabled via
configuration settings.

The configuration toggle to enable and disable QuickJS by default is in
:ref:`couchdb <config/couchdb>` ``js_engine`` section.

To help evaluate design doc compatibility, without the penalty of resetting all
the views on a cluster, there is a :ref:`scanner <config/scanner>` plugin,
which will traverse databases and design docs, compile and then execute some of
the view functions using both engines and report incompatibilities.

.. _config/quickjs:

.. versionadded:: 3.4

QuickJS Options
===============

.. config:section:: quickjs :: QuickJS Engine Configuration

    .. config:option:: memory_limit_bytes

        Set QuickJS memory limit in bytes. The default is ``undefined`` and the
        built-in C default of 64MB is used. ::

            [quickjs]
            memory_limit_bytes = 67108864

.. config:section:: couch_quickjs_scanner_plugin :: QuickJS Scanner Plugin Settings

Enable QuickJS Scanner plugin in :ref:`couch_scanner_plugins
<config/scanner>` ``couch_scanner_plugins`` section.

    .. config:option:: max_ddocs

        Limit the number of design docs processed per database. ::

            [couch_quickjs_scanner_plugin]
            max_ddocs = 100

    .. config:option:: max_shards

        Limit the number of shards processed per database. ::

            [couch_quickjs_scanner_plugin]
            max_shards = 4

    .. config:option:: max_docs

        Limit the number of documents processed per database. These are the max
        number of documents sent to the design doc functions. ::

            [couch_quickjs_scanner_plugin]
            max_docs = 1000

    .. config:option:: max_step

        Limit the maximum step size when processing docs. Given that total
        number of documents in a shard as N, if the max_docs is M, then the
        step S = N / M. Then only every S documents will be sampled and
        processed. ::

            [couch_quickjs_scanner_plugin]
            max_step = 1000

    .. config:option:: max_batch_items

        Maximum document batch size to gather before feeding them through each
        design doc on both QuickJS and Spidermonkey engines and compare the
        results. ::

            [couch_quickjs_scanner_plugin]
            max_batch_items = 100

    .. config:option:: max_batch_size

        Maximum memory usage for a document batch size. ::

            [couch_quickjs_scanner_plugin]
            max_batch_size = 16777216

    .. config:option:: after

        A common :ref:`scanner <config/scanner>` setting to
        configure when to execute the plugin after it's enabled. By default
        it's ``restart``, so the plugin would start running after a node
        restart::

            [couch_quickjs_scanner_plugin]
            after = restart

    .. config:option:: repeat

        A common :ref:`scanner <config/scanner>` setting to
        configure when to execute the plugin after it's enabled. By default
        it's ``restart``, so the plugin would start running after a node
        restart::

            [couch_quickjs_scanner_plugin]
            repeat = restart
