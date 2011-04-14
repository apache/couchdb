# Riaknostic Development

Riaknostic requires a sane GNU build system and a recent version of
Erlang.  It has `lager` and `getopt` as dependencies, so those must be
compatible with your version of Erlang. Release versions are currently
built with Erlang version R14B03, while development versions are targeted at Erlang version R14B04.

See the `rebar.config` file for more details.

To build Riaknostic, simply run `make`:

```bash
$ make
./rebar get-deps
==> riaknostic (get-deps)
Pulling lager from {git,"git://github.com/basho/lager",{branch,"master"}}
Cloning into lager...
Pulling getopt from {git,"git://github.com/jcomellas/getopt.git","2981dfe"}
Cloning into getopt...
==> lager (get-deps)
==> getopt (get-deps)
./rebar compile
==> lager (compile)
Compiled src/lager_util.erl
Compiled src/lager_transform.erl
Compiled src/lager_sup.erl
Compiled src/lager_mochiglobal.erl
Compiled src/lager_stdlib.erl
Compiled src/lager_handler_watcher_sup.erl
Compiled src/lager_handler_watcher.erl
Compiled src/lager_trunc_io.erl
Compiled src/lager_crash_log.erl
Compiled src/lager_file_backend.erl
Compiled src/lager_app.erl
Compiled src/lager.erl
Compiled src/lager_console_backend.erl
Compiled src/lager_format.erl
Compiled src/error_logger_lager_h.erl
==> getopt (compile)
Compiled src/getopt.erl
==> riaknostic (compile)
Compiled src/riaknostic_check.erl
Compiled src/riaknostic_util.erl
Compiled src/riaknostic_node.erl
Compiled src/riaknostic_check_ring_size.erl
Compiled src/riaknostic_check_ring_membership.erl
Compiled src/riaknostic_config.erl
Compiled src/riaknostic_check_memory_use.erl
Compiled src/riaknostic_check_nodes_connected.erl
Compiled src/riaknostic_check_dumps.erl
Compiled src/riaknostic.erl
Compiled src/riaknostic_check_disk.erl
./rebar escriptize
==> lager (escriptize)
==> getopt (escriptize)
==> riaknostic (escriptize)
```

Now you can invoke the script manually via the below command:

```bash
$ ./riaknostic --etc ~/code/riak/rel/riak/etc --base ~/code/riak/rel/riak --user `whoami` [other options]
```

To generate the edoc reference, use `make docs` and then open the
`doc/index.html` file in your browser. Detailed discussion of the
internal APIs that you can use in developing new diagnostics is found
in the edocs.

## Contributing

Have an idea for a diagnostic? Want to improve the way Riaknostic works? Fork the [github repository](https://github.com/basho/riaknostic) and send us a pull-request with your changes! The code is documented with `edoc`, so give the [API Docs](http://riaknostic.basho.com/edoc/index.html) a read before you contribute.

### Developing for Riaknostic Without a Riak Instance

If you want to run the `riaknostic` script while developing, and you don't have it hooked up to your local Riak, you can invoke it directly like so:

```bash
./riaknostic --etc ~/code/riak/rel/riak/etc --base ~/code/riak/rel/riak --user `whoami` [other options]
```

The extra options are usually assigned by the `riak-admin` script for you, but here's how to set them:

* `--etc`:	Where your Riak configuration directory is, in the example above it's in the generated directory of a source checkout of Riak.
* `--base`:	The "base" directory of Riak, usually the root of the generated directory or `/usr/lib/riak` on Linux, for example. Scan the `riak-admin` script for how the `RUNNER_BASE_DIR` variable is assigned on your platform.
* `--user`:	What user/UID the Riak node runs as. In a source checkout, it's the current user, on most systems, it's `riak`.