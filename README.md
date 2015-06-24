# What it is

`couch_epi` is extensible plugin interface (EPI) for couchdb.

## Requirements

  1. Automatically discoverable
  2. Minimize apps that need to be started for tests
  3. Support release upgrades

## Glossary

  * service - an abstract functionality defined by unique name and API
  * provider - a self-contained implementation of `Service`'s API
  * subscriber - an application or a process which uses functionality provided by `Provider`
  * epi_key - is a routing key it has to be in on of the following forms
    - `{service_id :: atom(), key :: term()}` - for `couch_epi_data_source`
    - `service_id :: atom()` - for `couch_epi_functions`
  * handle - is opaque data structure returned from `couch_epi:get_handle(EpiKey)`

## Support release upgrade

We monitor the source of config information and have an ability to notify the subscriber.
The source is either a file for a `couch_epi_data_source` or module for `couch_epi_functions`.

If the subscriber wants to receive notifications when the config has been updated it can use:

    couch_epi:subscribe(App, Key, Module, Func, ExtraArgs)

The function would be called with following arguments

    Fun(App :: app(), Key :: key(),
        OldData :: notification(), NewData :: notification(),
        ExtraArgs :: term()

The `notification()` is either `{data, term()}` or `{modules, [module()]}`

## data example

Any application that wants to register some configuration data for a service using module
could add an entry in its supervision tree with something like:

    Spec = couch_epi_data:childspec(
        appname_stats, %% Id
        appname, %% CurrentApp
        {epi_key, {couch_stats, definitions}},
        appname_stats_config %% Module
    ).

When service provider wants to learn about all the installed config data for it to use
it would then just do something like:


     couch_epi:get(Handle, Service, Key)

There are also additional functions to get the same data in various formats

- `couch_epi:all(Handle)` - returns config data for all services for a given handle
- `couch_epi:get(Handle, Subscriber)` - returns config data for a given subscriber
- `couch_epi:get_value(Handle, Subscriber, Key)` - returns config data for a given subscriber and key
- `couch_epi:by_key(Handle, Key)` - returns config data for a given key
- `couch_epi:by_key(Handle)` - returns config data grouped by key
- `couch_epi:by_source(Handle)` - returns config data grouped by source (subscriber)
- `couch_epi:keys(Handle)` - returns list of configured keys
- `couch_epi:subscribers(Handle)` - return list of known subscribers



## data_source example

Any application that wants to register some configuration data for a service
could add an entry in its supervision tree with something like:

    Spec = couch_epi_data_source:childspec(
        appname_stats, %% Id
        appname, %% CurrentApp
        {epi_key, {couch_stats, definitions}},
        {priv_file, "couch_stats.cfg"},
        [{interval, 5000}]
    ).

Note we also support `{file, FilePath}` instead of `{priv_file, File}`

The query API is the same as for `data` (see `data example`)

# Function dispatch example

Any application that wants to register some functions for a service
could add an entry in its supervision tree with something like:

    Spec = couch_epi_functions:childspec(
        appname_stats, %% Id
        appname, %% CurrentApp
        {epi_key, my_service},
        my_module %% Module
    ).

Adding the entry would generate a dispatch methods for any exported function of modules passed.


When app wants to dispatch the call to all service providers it calls

    couch_epi:apply(Handle, ServiceId, Function, Args, Opts)

There are multiple ways of doing the apply which is controlled by Opts

  - ignore_errors - the call is wrapped into try/catch
  - concurrent - spawn a new process for every service provider
  - pipe - use output of one service provider as an input for the next one
  - ignore_providers - do not fail if there are no providers for the service are available

Notes:

  - `concurrent` is incompatible with `pipe`
