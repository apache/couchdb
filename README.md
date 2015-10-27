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

We monitor the modules involved in configuration of the service/provider so we
get notified when there is a code upgrade. We use this notification in order to:

  - regenerate dispatch module if needed
  - call notify/3 of a module implementing couch_epi_plugin behaviour

Call to notify/3 would be called for both providers and data_providers.

## data example

Any application that wants to register some configuration data for a service using module
could add an entry in its implementation of couch_epi_plugin behaviour:

    data_providers() ->
        [
            {{couch_stats, descriptions},
                {priv_file, "stats_descriptions.cfg"}, [{interval, 5000}]}
            {{couch_stats, descriptions},
                {file, "/tmp/extra_stats.cfg"}, [{interval, 5000}]},
            {{couch_stats, descriptions}, {module, my_stats}}
        ].

When service provider wants to learn about all the installed config data for it to use
it would then just do something like:


     couch_epi:get(Handle, Service, Key)

The service provider also have to mention the data keys it is using in its
implementation of couch_epi_plugin behaviour

    data_subscriptions() ->
        [{couch_stats, descriptions}].

There are also additional functions to get the same data in various formats

- `couch_epi:all(Handle)` - returns config data for all services for a given handle
- `couch_epi:get(Handle, Subscriber)` - returns config data for a given subscriber
- `couch_epi:get_value(Handle, Subscriber, Key)` - returns config data for a given subscriber and key
- `couch_epi:by_key(Handle, Key)` - returns config data for a given key
- `couch_epi:by_key(Handle)` - returns config data grouped by key
- `couch_epi:by_source(Handle)` - returns config data grouped by source (subscriber)
- `couch_epi:keys(Handle)` - returns list of configured keys
- `couch_epi:subscribers(Handle)` - return list of known subscribers


# Function dispatch example

Any application that wants to register implementation functions for a service
could add following into it's implementation of couch_epi_plugin behaviour:

    providers() ->
        [{my_service, module_which_implements_the_functions}].

Adding the entry would generate a dispatch methods for any exported function
of modules passed.

Services have to be defined in one of the implementations of couch_epi_plugin
behaviour as:

    services() ->
        [{my_service, module_to_monitor_for_codechange}].

When app wants to dispatch the call to all service providers it calls

    couch_epi:apply(Handle, ServiceId, Function, Args, Opts)

There are multiple ways of doing the apply which is controlled by Opts

  - ignore_errors - the call is wrapped into try/catch
  - concurrent - spawn a new process for every service provider
  - pipe - use output of one service provider as an input for the next one

Notes:

  - `concurrent` is incompatible with `pipe`
  - if there are multiple plugins providing same service they will be called in the order
    they listed in application:get_env(couch_epi, plugins)
  - if the same plugin provides multiple implementations of the same service
    the order is as defined in providers callback

# couch_epi_plugin behaviour

The module implementing behaviour need to export following functions:

  - Module:app/0 - Returns atom representing the application name
  - Module:providers/0 - Returns list of {service_id(), module()} tuples
    for defined providers
  - Module:services/0 - Returns list of {service_id(), module()} tuples
    for defined services
  - Module:data_subscriptions/0 - Returns list of keys we define
  - Module:data_providers/0 - Returns list of keys we provide
  - Module:processes/0 - Supervisor specs which we would be injected into
    application supervisor
  - Module:notify/3 - Notification callback
