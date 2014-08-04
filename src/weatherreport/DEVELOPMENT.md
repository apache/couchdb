# WeatherReport Development

WeatherReport requires a sane GNU build system and a recent version of
Erlang.  It has `twig` and `getopt` as dependencies, so those must be
compatible with your version of Erlang. Release versions are currently
built with Erlang version R14B03, while development versions are targeted at Erlang version R14B04.

See the `rebar.config` file for more details.

To build WeatherReport, simply run `make`:

```bash
$ make
./rebar get-deps
==> weatherreport (get-deps)
Pulling getopt from {git,"git://github.com/jcomellas/getopt.git",
                         {tag,"v0.4.3"}}
Cloning into 'getopt'...
Pulling twig from {git,"https://github.com/cloudant/twig.git",master}
Cloning into 'twig'...
Pulling config from {git,"git://github.com/cloudant/config.git",{tag,"0.2.5"}}
Cloning into 'config'...
==> getopt (get-deps)
==> twig (get-deps)
==> config (get-deps)
./rebar compile
==> getopt (compile)
Compiled src/getopt.erl
==> twig (compile)
Compiled src/twig_sup.erl
Compiled src/twig_util.erl
Compiled src/twig_app.erl
Compiled src/twig_event_handler.erl
Compiled src/twig.erl
Compiled src/twig_monitor.erl
Compiled src/trunc_io.erl
==> config (compile)
Compiled src/config_listener.erl
Compiled src/config_sup.erl
Compiled src/config_app.erl
Compiled src/config_util.erl
Compiled src/config_writer.erl
Compiled src/config.erl
==> weatherreport (compile)
Compiled src/weatherreport_check.erl
Compiled src/weatherreport_config.erl
Compiled src/weatherreport_util.erl
Compiled src/weatherreport_node.erl
Compiled src/weatherreport_check_nodes_connected.erl
Compiled src/weatherreport_check_memory_use.erl
Compiled src/weatherreport_check_membership.erl
Compiled src/weatherreport.erl
./rebar escriptize
==> getopt (escriptize)
==> twig (escriptize)
==> config (escriptize)
==> weatherreport (escriptize)
```

Now you can invoke the script manually via the below command:

```bash
$ ./weatherreport --etc /path/to/etc [other options]
```

To generate the edoc reference, use `make docs` and then open the
`doc/index.html` file in your browser. Detailed discussion of the
internal APIs that you can use in developing new diagnostics is found
in the edocs.

## Contributing

We want your code! Fork the [github repository](https://github.com/cloudant/weatherreport) and send a pull request if you'd like to add a new check, contribute improvements to existing checks or improve the way WeatherReport works.
