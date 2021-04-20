# Weather Report

`weatherreport` is an escript and set of tools that diagnoses common problems which could affect a CouchDB node or cluster.

## Overview

Here is a basic example of using `weatherreport` followed immediately by the command's output:

```bash
$ ./weatherreport --etc /path/to/etc
[warning] Cluster member node3@127.0.0.1 is not connected to this node. Please check whether it is down.
```

## Installation

WeatherReport depends on features introduced by Erlang version R14B04, so verify that you've installed this version of Erlang before proceeding with installation.

Installation is currently a matter of cloning the git repository and running `make`.

## Usage

For most cases, you can just run the `weatherreport` command as given at the top of this README. However, sometimes you might want to know some extra detail or run only specific checks. For that, there are command-line options. Execute `weatherreport --help` to learn more about these options:

```bash
weatherreport --help
Usage: weatherreport [-d <level>] [-e] [-h] [-l] [check_name ...]

  -d, --level		Minimum message severity level (default: notice)
  -l, --list		Describe available diagnostic tasks
  -e, --expert		Perform more detailed diagnostics
  -h, --help		Display help/usage
  check_name		A specific check to run
```

To get an idea of what checks will be run, use the `--list` option:

```bash
weatherreport diag --list
Available diagnostic checks:

  membership           Cluster membership validity
  memory_use           Measure memory usage
  nodes_connected      Cluster node liveness
```

If you want all the gory details about what WeatherReport is doing, you can run the checks at a more verbose logging level with the --level option:

```bash
$ ./weatherreport --etc /path/to/etc -d debug
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
```

Most times you'll want to use the defaults, but any Syslog severity name will do (from most to least verbose): `debug, info, notice, warning, error, critical, alert, emergency`.

Finally, if you want to run just a single diagnostic or a list of specific ones, you can pass their name(s):

```bash
$ ./weatherreport --etc /path/to/etc nodes_connected
[warning] Cluster member node3@127.0.0.1 is not connected to this node. Please check whether it is down.
```

## Contributing

0. Read DEVELOPMENT.md
1. Fork the project on [Github](https://github.com/cloudant/weatherreport).
2. Make your changes or additions on a "topic" branch, test and
   document them. If you are making a new diagnostic, make sure you
   give some module-level information about the checks it
   performs. *Note*: diagnostics _should not_ make modifications, only
   inspect things.
3. Push to your fork and send a pull-request.
4. A project committer will review your pull-request and get back to you.
