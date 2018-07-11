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
  * epi_key - is a routing key it has to be in one of the following forms
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
            {{couch_stats, descriptions}, {static_module, my_stats}},
            {{couch_stats, descriptions}, {callback_module, my_stats}}
        ].

When service provider wants to learn about all the installed config data for it to use
it would then just do something like:


     couch_epi:get(Handle, Service, Key)

The service provider also has to mention the data keys it is using in its
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

The difference between `static_module` and `callback_module` providers is in how
couch_epi detects the changes. `static_module` is designed for the cases when you
have your data hardcoded in the module. For example you might have the following:

```
-export([data/0]).

data() ->
    [
        {[complex, key, 2], [
            {type, counter},
            {desc, bar}
        ]},
        {[complex, key, 1], [
            {type, counter},
            {desc, updated_foo}
        ]}
    ].
```

The changes are detected by relying on `vsn` module attribute. Therefore we
would notice the change only when data source module is recompiled.

The `callback_module` provider uses the return value from `data/0` to detect
changes and it is useful for cases when the data term is constructed dynamically.
For example to cache values of CouchDB config one could use the following:

```
-export([data/0]).
data() ->
    config:get("dreyfus").
```

# Function dispatch example

Any application that wants to register implementation functions for a service
could add the following into it's implementation of couch_epi_plugin behaviour:

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

## decide functionality

There are cases when we want to call configured providers until any of them
would make a decision. We also would want to be able to find out if any
decision has been made so we could call default handler. In order to be able
to do so there is couch_epi:decide/5. Every service which uses this feature
would get either:

  - no_decision
  - {decided, Decision :: term()}

The provider module should return one of the above results. The current logic is
to call all configured providers in order of their definition until we get
`{decided, term()}`. If none of the providers would return this term we would
return `no_decision`.

# couch_epi_plugin behaviour

The module implementing this behaviour needs to export the following functions:

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
