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

.. _config/os_daemons:

``[os_daemons]`` :: OS Daemons
==============================

This is a simple feature that allows users to configure CouchDB so that it
maintains a given OS level process alive. If the process dies for any reason,
CouchDB will restart it. If the process restarts too often, then CouchDB will
mark it has halted and not attempt to restart it. The default max restart rate
is ``3`` times in the last ``5`` seconds. These parameters are adjustable.

Commands that are started in this manner will have access to a simple
API over stdio to request configuration parameters or to add log
statements to CouchDB's logs.

To configure an OS process as a CouchDB os_daemon, create a section
in your `local.ini` like such::

  [os_daemons]
  daemon_name = /path/to/command -with args

This will make CouchDB bring up the command and attempt to keep it
alive. To request a configuration parameter, an `os_daemon` can write
a simple JSON message to stdout like such::

  ["get", "os_daemons"]\n

which would return::

  {"daemon_name": "/path/to/command -with args"}\n

Or::

  ["get", "os_daemons", "daemon_name"]\n

which would return::

  "/path/to/command -with args"\n

There's no restriction on what configuration variables are visible.
There's also no method for altering the configuration.

If you would like your OS daemon to be restarted in the event that
the configuration changes, you can send the following messages::

  ["register", $(SECTION)]\n

When anything in that section changes, your OS process will be
rebooted so it can pick up the new configuration settings. If you
want to listen for changes on a specific key, you can send something
like::

  ["register", $(SECTION), $(KEY)]\n

In this case, CouchDB will only restart your daemon if that exact
section/key pair changes, instead of anything in that entire section.

Logging commands look like::

  ["log", $(JSON_MESSAGE)]\n

Where ``$(JSON_MESSAGE)`` is arbitrary JSON data. These messages are
logged at the 'info' level. If you want to log at a different level
you can pass messages like such::

  ["log", $(JSON_MESSAGE), {"level": $(LEVEL)}]\n

Where ``$(LEVEL)`` is one of "debug", "info", or "error".

When implementing a daemon process to be managed by CouchDB you
should remember to use a method like checking the parent process
id or if stdin has been closed. These flags can tell you if
your daemon process has been orphaned so you can exit cleanly.

There is no interactivity between CouchDB and the running process, but
you can use the OS Daemons service to create new HTTP servers and
responders and then use the new proxy service to redirect requests and
output to the CouchDB managed service. For more information on proxying,
see :ref:`http-proxying`. For further background on the OS Daemon service, see
`CouchDB Externals API`_.

.. _CouchDB Externals API: http://davispj.com/2010/09/26/new-couchdb-externals-api.html
