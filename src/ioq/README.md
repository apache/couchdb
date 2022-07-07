### IOQ classes
The following are the list of IOQ classes:

* interactive
* db_update
* view_update
* db_compact
* view_compact
* internal_repl
* low


### Bypassing IOQ
One can configure an ioq bypass, which removes an IO class from prioritization,
as below:

    config:set("ioq.bypass", "view_update", "true")

Note that setting an IOQ bypass can effectively trump all other classes,
especially in the case of an interactive bypass v. compaction. This can lead
to high disk usage.

### Setting priorities
The priority for a class can also be set ala:

    config:set("ioq", "compaction", "0.3")

Or globally, using snippet/rpc:

    s:set_config("ioq", "compaction", "0.314", global)
    rpc:multicall(config, set, ["ioq", "compaction", "0.217"])

As the interactive class is 'everything else' its priority cannot be directly
set.
