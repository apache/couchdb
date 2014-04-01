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

.. _config/intro:

=============================
Introduction Into Configuring
=============================

Configuration files
-------------------

.. warning::
   The following section covering load order of config files
   applies only to UNIX-ish systems.
   For Windows, only the provided ``default.ini`` and ``local.ini``
   files are relevant. These can of course have content
   appended, which achieves the same type of functionality
   as outlined for UNIX-ish systems below.

By default, CouchDB reads configuration files from the following locations,
in the following order:

#. ``LOCALCONFDIR/default.ini``

#. ``LOCALCONFDIR/default.d/*.ini``

#. ``PLUGINS_DIR/*/priv/default.d/*.ini``

#. ``LOCALCONFDIR/local.ini``

#. ``LOCALCONFDIR/local.d/*.ini``

The ``LOCALCONFDIR`` points to the directory that contains configuration files
(``/usr/local/etc/couchdb`` by default). This variable may vary from the
target operation system and may be changed during building from the source code.
For binary distributions, it mostly points to the installation path
(e.g. ``C:\Program Files\CouchDB\etc\couchdb`` for Windows).

To see the actual configuration files chain run in shell::

  couchdb -c

This will print out all *actual* configuration files that will form the result
CouchDB configuration::

  /etc/couchdb/default.ini
  /etc/couchdb/default.d/geocouch.ini
  /etc/couchdb/local.ini
  /etc/couchdb/local.d/geocouch.ini
  /etc/couchdb/local.d/vendor.ini

Settings in successive documents override the settings in earlier entries.
For example, setting the :option:`httpd/bind_address` parameter in ``local.ini``
would override any setting in ``default.ini``.

.. warning::
   The ``default.ini`` file may be overwritten during an upgrade or
   re-installation, so localised changes should be made to the
   ``local.ini`` file or files within the ``local.d`` directory.

The configuration files chain may be changed by specifying additional sources
by using next command line options:

- ``-a``: adds configuration file to the chain
- ``-A``: adds configuration directory to the chain

Let's add these options and see how the configuration chain changes::

  shell> couchdb -c -a /home/couchdb/custom.ini
  /etc/couchdb/default.ini
  /etc/couchdb/default.d/geocouch.ini
  /etc/couchdb/local.ini
  /etc/couchdb/local.d/geocouch.ini
  /etc/couchdb/local.d/vendor.ini
  /home/couchdb/custom.ini

In case when `/home/couchdb/custom.ini` exists it will be added to
the configuration chain.


Parameter names and values
--------------------------

All parameter names are *case-sensitive*. Every parameter takes a value of one
of five types: `boolean`, `integer`, `string`, `tuple`_ and `proplist`_. Boolean
values can be written as ``true`` or ``false``.

Parameters with value type of `tuple` or `proplist` are following the Erlang
requirement for style and naming.

.. _proplist: http://www.erlang.org/doc/man/proplists.html
.. _tuple: http://www.erlang.org/doc/reference_manual/data_types.html#id66049


Setting parameters via the configuration file
---------------------------------------------

The common way to set some parameters is to edit the `local.ini` file which is
mostly located in the `etc/couchdb` directory relative your installation path
root.

For example::

  ; This is a comment
  [section]
  param = value ; inline comments are allowed

Each configuration file line may contains `section` definition, `parameter`
specification, empty (space and newline characters only) or `commented` line.
You can setup `inline` commentaries for `sections` or `parameters`.

The `section` defines group of parameters that are belongs to some specific
CouchDB subsystem. For instance, :section:`httpd` section holds not only HTTP
server parameters, but also others that directly interacts with it.

The `parameter` specification contains two parts divided by the `equal` sign
(``=``): the parameter name on the left side and the parameter value on the
right one. The leading and following whitespace for ``=`` is an optional to
improve configuration readability.

.. note::
   In case when you'd like to remove some parameter from the `default.ini`
   without modifying that file, you may override in `local.ini`, but
   without any value::

     [httpd_global_handlers]
     _all_dbs =

   This could be read as: "remove the `_all_dbs` parameter from
   the `httpd_global_handlers` section if it was ever set before".


The semicolon (``;``) signs about `commentary` start: everything after this
character is counted as commentary and doesn't process by CouchDB.

After editing of configuration file CouchDB server instance should be restarted
to apply these changes.


Setting parameters via the HTTP API
-----------------------------------

Alternatively, configuration parameters could be set via the
:ref:`HTTP API <api/config>`. This API allows to change CouchDB configuration
on-the-fly without requiring a server restart::

  curl -X PUT http://localhost:5984/_config/uuids/algorithm -d '"random"'

In the response the old parameter's value returns::

  "sequential"

You should be careful with changing configuration via the HTTP API since it's
easy to make CouchDB unavailable. For instance, if you'd like to change the
:option:`httpd/bind_address` for a new one::

  curl -X PUT http://localhost:5984/_config/httpd/bind_address -d '"10.10.0.128"'

However, if you make a typo, or the specified IP address is not available
from your network, CouchDB will be unavailable for you in both cases and
the only way to resolve this will be by remoting into the server, correcting
the errant file, and restarting CouchDB. To protect yourself against such
accidents you may set the :option:`httpd/config_whitelist` of permitted
configuration parameters for updates via the HTTP API. Once this option is set,
further changes to non-whitelisted parameters must take place via the
configuration file, and in most cases, also requires a server restart before
hand-edited options take effect.
