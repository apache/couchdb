# Riaknostic [![Build Status](https://secure.travis-ci.org/basho/riaknostic.png?branch=master)](http://travis-ci.org/basho/riaknostic)

`riaknostic` is an escript and set of tools that diagnoses common problems which could affect a Riak node or cluster. When experiencing any problem with Riak, `riaknostic` should be the first thing run during troubleshooting. The tool is integrated with Riak via the `riak-admin` script.

## Overview

To diagnose problems with Riak, Riaknostic uses a series of checks which are derived from the experience of the Basho Client Services Team as well as numerous public discussions on the mailing list, IRC room, and other online media.

Here is a basic example of using `riaknostic` followed immediately by the command's output:

```bash
$ riak-admin diag
15:34:52.736 [warning] Riak crashed at Wed, 07 Dec 2011 21:47:50 GMT, leaving
crash dump in /srv/riak/log/erl_crash.dump. Please inspect or remove the file.
15:34:52.736 [notice] Data directory /srv/riak/data/bitcask is not mounted with 'noatime'. Please remount its disk with the 'noatime' flag to improve
performance.
```

As shown in the above output, Riaknostic tells us about two problems right away. First, an Erlang crash dump is present, indicating that Riak has experienced a crash. Second, a performance problem is mentioned (disk mounted without `noatime` argument)along with a helpful tip to resolve the issue.

## Installation

**Important**: If you are running Riak v1.3.0 or greater, you already have Riaknostic, so you can skip to the **Usage** section below.

Riaknostic depends on features introduced by Erlang version R14B04, so verify that you've installed this version of Erlang before proceeding with installation.

To install `riaknostic`, download the latest package version, and extract it within the directory shown for your operating system in the following table:

<table class="bordered-table zebra-striped">
    <thead>
        <tr><th>Platform</th><th>Directory</th></tr>
    </thead>
    <tbody>
        <tr>
            <td>Linux (Redhat, CentOS, Debian, Ubuntu)</td>
            <td><code>/usr/lib/riak/lib</code></td>
        </tr>
        <tr>
            <td>Linux (Fedora)</td>
            <td><code>/usr/lib64/riak/lib</code></td>
        </tr>
        <tr>
            <td>Solaris, OpenSolaris</td>
            <td><code>/opt/riak/lib</code></td>
        </tr>
        <tr>
            <td>SmartOS (Joyent)</td>
            <td><code>/opt/local/lib/riak/lib</code></td>
        </tr>
        <tr>
            <td>Mac OS/X or Self-built</td>
            <td><code>$RIAK/lib</code>
                (where <code>$RIAK=rel/riak</code> for source installs,
                or the directory where you unpacked the package)</td>
        </tr>
    </tbody>
</table>

An example Riaknostic installation for Linux looks like this:

```bash
wget https://github.com/basho/riaknostic/downloads/riaknostic-1.0.2.tar.gz -P /tmp
cd /usr/lib/riak/lib
sudo tar xzvf /tmp/riaknostic-1.0.2.tar.gz
```

The package will expand to a `riaknostic/` directory which contains the `riaknostic` script, source code in the `src/` directory, and documentation.

Now try it out!

## Usage

For most cases, you can just run the `riak-admin diag` command as given at the top of this README. However, sometimes you might want to know some extra detail or run only specific checks. For that, there are command-line options. Execute `riaknostic --help` to learn more about these options:

```bash
riak-admin diag --help
Usage: riak-admin diag [-d <level>] [-l] [-h] [check_name ...]

  -d, --level		Minimum message severity level (default: notice)
  -l, --list		Describe available diagnostic tasks
  -h, --help		Display help/usage
  check_name		A specific check to run
```

To get an idea of what checks will be run, use the `--list` option:

```bash
riak-admin diag --list
Available diagnostic checks:

  disk                 Data directory permissions and atime
  dumps                Find crash dumps
  memory_use           Measure memory usage
  nodes_connected      Cluster node liveness
  ring_membership      Cluster membership validity
  ring_size            Ring size valid
```

If you want all the gory details about what Riaknostic is doing, you can run the checks at a more verbose logging level with the --level option:

```bash
riak-admin diag --level debug
18:34:19.708 [debug] Lager installed handler lager_console_backend into lager_event
18:34:19.720 [debug] Lager installed handler error_logger_lager_h into error_logger
18:34:19.720 [info] Application lager started on node nonode@nohost
18:34:20.736 [debug] Not connected to the local Riak node, trying to connect. alive:false connect_failed:undefined
18:34:20.737 [debug] Starting distributed Erlang.
18:34:20.740 [debug] Supervisor net_sup started erl_epmd:start_link() at pid <0.42.0>
18:34:20.742 [debug] Supervisor net_sup started auth:start_link() at pid <0.43.0>
18:34:20.771 [debug] Supervisor net_sup started net_kernel:start_link(['riak_diag87813@127.0.0.1',longnames]) at pid <0.44.0>
18:34:20.771 [debug] Supervisor kernel_sup started erl_distribution:start_link(['riak_diag87813@127.0.0.1',longnames]) at pid <0.41.0>
18:34:20.781 [debug] Supervisor inet_gethost_native_sup started undefined at pid <0.49.0>
18:34:20.782 [debug] Supervisor kernel_safe_sup started inet_gethost_native:start_link() at pid <0.48.0>
18:34:20.834 [debug] Connected to local Riak node 'riak@127.0.0.1'.
18:34:20.939 [debug] Local RPC: os:getpid([]) [5000]
18:34:20.939 [debug] Running shell command: ps -o pmem,rss,command -p 83144
18:34:20.946 [debug] Shell command output:
%MEM    RSS COMMAND
 0.4  31004 /srv/riak/erts-5.8.4/bin/beam.smp -K true -A 64 -W w -- -root /srv/riak/rel/riak -progname riak -- -home /Users/sean -- -boot /srv/riak/releases/1.0.2/riak -embedded -config /srv/riak/etc/app.config -name riak@127.0.0.1 -setcookie riak -- console

18:34:20.960 [warning] Riak crashed at Wed, 07 Dec 2011 21:47:50 GMT, leaving crash dump in /srv/riak/log/erl_crash.dump. Please inspect or remove the file.
18:34:20.961 [notice] Data directory /srv/riak/data/bitcask is not mounted with 'noatime'. Please remount its disk with the 'noatime' flag to improve performance.
18:34:20.961 [info] Riak process is using 0.4% of available RAM, totalling 31004 KB of real memory.
```

Most times you'll want to use the defaults, but any Syslog severity name will do (from most to least verbose): `debug, info, notice, warning, error, critical, alert, emergency`.

Finally, if you want to run just a single diagnostic or a list of specific ones, you can pass their name(s):

```bash
riak-admin diag dumps
18:41:24.083 [warning] Riak crashed at Wed, 07 Dec 2011 21:47:50 GMT, leaving crash dump in /srv/riak/log/erl_crash.dump. Please inspect or remove the file.
```

## Contributing

0. Read DEVELOPMENT.md
1. Fork the project on [Github](https://github.com/basho/riaknostic).
2. Make your changes or additions on a "topic" branch, test and
   document them. If you are making a new diagnostic, make sure you
   give some module-level information about the checks it
   performs. *Note*: diagnostics _should not_ make modifications to
   Riak, only inspect things.
3. Push to your fork and send a pull-request.
4. A Basho Developer Advocate or Engineer will review your
   pull-request and get back to you.