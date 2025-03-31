Couch Scanner
=============

Couch Scanner is an application which traverses all the dbs and docs in the
background and emits various reports. There is a common traversal mechanism
which finds all the dbs, design docs, docs and calls various reporting plugins.
Reporting plugins implement a common API and may search for various strings in
the doc bodies, examine design docs for certain features, check index sizes, or
issue any other reports.

Two plugins are initially included with the application are:
  * [couch_scanner_plugin_find](src/couch_scanner_plugin_find.erl) : Find
    occurrences of any regular expressions in the cluster. It scans through
    document bodies, db name and doc ids. This can be used, for instance, to
    search accidentally leaked secrets (API keys, passwords).
  * [couch_scanner_ddoc_features](src/couch_scanner_ddoc_features.erl) : Report
    on various features used by design docs. By default it will report features
    which will be deprecated in CouchDB 4.x such as shows, lists, rewrites,
    etc. But also can be configured to search for javascript filter definition,
    custom javascript reduce functions and a few other things.

By default no plugins are enabled so the scanner application won't do anything.
Plugins can be enabled in the configuration system by setting:

For instance, to enable `couch_scanner_ddoc_features` plugin use:

```
[couch_scanner_plugins]
couch_scanner_plugin_ddoc_features = true
```

If a node is put in maintenance mode all the plugins will be automatically
stopped on that node. When node is put back in production plugins will
automatically resume executing. It's also possible to pause plugin execution in
a remsh using the `couch_scanner:stop()` and then resume it later with
`couch_scanner:resume()`.

#### Application Structure

Top level application API is in the [couch_scanner](src/couch_scanner.erl) module.

 * status() -> return running status of plugins
 * stop() -> stop running plugins on this node
 * resume() -> resume running plugins on this node
 * checkpoints() -> inspect the value of all the node local checkpoints
 * reset_checkpoints() -> delete all the node local checkpoints

#####  couch_scanner_server

Plugins are run as individual processes. These processes are managed by
[couch_scanner_server](src/couch_scanner_server.erl) gen_server. The gen_server
inspects `couch_scanner_plugins` config section and then starts a new plugin
process for each configured plugin. Then it waits for the process to exit. When
the process exits it may exit normally, crash with an error, or indicate that
it should be rescheduled to run later. Later rescheduling is indicated by
exiting with a `{shutdown, {reschedule, UnixTimeSec}}` exit value. If the
plugin crashes repeatedly it will be penalized with an exponential back-off
starting at 30 seconds and up to 8 hours.

##### couch_scanner_plugin

Plugin processes are proc_lib processes spawned by the
[couch_scanner_plugin](src/couch_scanner_plugin.erl) module. After spawning the
processes will read the previously saved checkpoint from a `_local` checkpoint
doc in the shards db (`_dbs`) and start traversing databases from the last
checkpoint.

There is a plugin processes running for each configured plugin. On startup,
during traversal, checkpointing and before exiting it will call into the
corresponding API function. This works very much like the gen_server pattern.
The plugin API behavior is defined in the
[couch_scanner_plugin](src/couch_scanner_plugin.erl) module as `-callback`
directives. Plugin modules then refer to it as
`-behavior(couch_scanner_plugin)`.

Periodically it will checkpoint the last database it processed so if the node
crashes or is stopped it will continue processing where it left off.

During startup or when finished the plugin may exit with a
`{shutdown,{reschedule, UnixTimeSec}}` exit message to indicate that it wants
to be scheduled to run at a later time.

##### couch_scanner_checkpoint

Plugins periodically will checkpoint their database traversal progress to a
`_local` checkpoint doc in the `_dbs` database. Each plugin has their separate
checkpoint document. Plugin modules may implement the optional `checkpoint/1`
API and save some plugin specific data alongside the database traversal
checkpoint which gets saved automatically. For instance, it maybe useful for
plugins to save their start up configuration to detect when it changes so they
could restart their scanning. Or, if they want to accumulate some statistics
and only emit them at the end of the scan.

Reading and writing to checkpoints is done in the
[couch_scanner_checkpoint](src/couch_scanner_checkpoint.erl) module. For
debugging or operational intervention there is a `reset/0` call to delete all
the checkpoints on the local node.

##### couch_scanner_rate_limiter

[couch_scanner_rate_limiter](src/couch_scanner_rate_limiter.erl). To limit
plugin resource (CPU / IO) usage there is a rate limiting mechanism to ensure
all plugins can only open so many dbs and docs per second.
[couch_scanner_server](src/couch_scanner_server.erl) creates a shared token
bucket as an Erlang atomics array and periodically fills it with "tokens".
Plugins consume tokens every time they process a db or document. If they use up
all the tokens, they start to back-off exponentially. It's a simple AIMD [1]
algorithm. Parameters to configure it can be found in the `default.ini` file.

[1] https://en.wikipedia.org/wiki/Additive_increase/multiplicative_decrease
