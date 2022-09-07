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

.. _cluster/troubleshooting:

============================================
Troubleshooting CouchDB 3 with WeatherReport
============================================

.. _cluster/troubleshooting/overview:

Overview
========

WeatherReport is an OTP application and set of tools that diagnoses
common problems which could affect a CouchDB version 3 node or cluster
(version 4 or later is not supported). It is accessed via the
``weatherreport`` command line escript.

Here is a basic example of using ``weatherreport`` followed immediately
by the command's output:

.. code-block:: bash

    $ weatherreport --etc /path/to/etc
    [warning] Cluster member node3@127.0.0.1 is not connected to this node. Please check whether it is down.

.. _cluster/troubleshooting/usage:

Usage
=====

For most cases, you can just run the ``weatherreport`` command as
shown above.  However, sometimes you might want to know some extra
detail, or run only specific checks. For that, there are command-line
options. Execute ``weatherreport --help`` to learn more about these
options:

.. code-block:: bash

    $ weatherreport --help
    Usage: weatherreport [-c <path>] [-d <level>] [-e] [-h] [-l] [check_name ...]

      -c, --etc                 Path to the CouchDB configuration directory
      -d, --level               Minimum message severity level (default: notice)
      -l, --list                Describe available diagnostic tasks
      -e, --expert              Perform more detailed diagnostics
      -h, --help                Display help/usage
      check_name                A specific check to run

To get an idea of what checks will be run, use the `--list` option:

.. code-block:: bash

    $ weatherreport --list
    Available diagnostic checks:

      custodian            Shard safety/liveness checks
      disk                 Data directory permissions and atime
      internal_replication Check the number of pending internal replication jobs
      ioq                  Check the total number of active IOQ requests
      mem3_sync            Check there is a registered mem3_sync process
      membership           Cluster membership validity
      memory_use           Measure memory usage
      message_queues       Check for processes with large mailboxes
      node_stats           Check useful erlang statistics for diagnostics
      nodes_connected      Cluster node liveness
      process_calls        Check for large numbers of processes with the same current/initial call
      process_memory       Check for processes with high memory usage
      safe_to_rebuild      Check whether the node can safely be taken out of service
      search               Check the local search node is responsive
      tcp_queues           Measure the length of tcp queues in the kernel

If you want all the gory details about what WeatherReport is doing,
you can run the checks at a more verbose logging level with
the ``--level`` option:

.. code-block:: bash

    $ weatherreport --etc /path/to/etc --level debug
    [debug] Not connected to the local cluster node, trying to connect. alive:false connect_failed:undefined
    [debug] Starting distributed Erlang.
    [debug] Connected to local cluster node 'node1@127.0.0.1'.
    [debug] Local RPC: mem3:nodes([]) [5000]
    [debug] Local RPC: os:getpid([]) [5000]
    [debug] Running shell command: ps -o pmem,rss -p 73905
    [debug] Shell command output:
    %MEM    RSS
    0.3  25116

    [debug] Local RPC: erlang:nodes([]) [5000]
    [debug] Local RPC: mem3:nodes([]) [5000]
    [warning] Cluster member node3@127.0.0.1 is not connected to this node. Please check whether it is down.
    [info] Process is using 0.3% of available RAM, totalling 25116 KB of real memory.

Most times you'll want to use the defaults, but any syslog severity
name will do (from most to least verbose): ``debug, info, notice,
warning, error, critical, alert, emergency``.

Finally, if you want to run just a single diagnostic or a list of
specific ones, you can pass their name(s):

.. code-block:: bash

    $ weatherreport --etc /path/to/etc nodes_connected
    [warning] Cluster member node3@127.0.0.1 is not connected to this node. Please check whether it is down.
