# My First CouchDB Plugin

A practical guide to developing CouchDB plugins.

*NOTE: This is incomplete, barely tested, works only with the 1867-feature-plugin branch of Apache CouchDB and expects that you understand some Erlang. This is mostly for early review, but if you are daring, you can learn someting already :)*


## Preparation

To get started, you need to install CouchDB from source, grab the CouchDB sources:

    git clone https://git-wip-us.apache.org/repos/asf/couchdb.git
    cd couchdb
    git checkout -b 1867-feature-plugin origin/1867-feature-plugins

Follow the instructions in `couchdb/INSTALL.Unix` and `couchdb/DEVELOPERS` to get a development environment going.

Be sure to install CouchDB into your system. If you want to install CouchDB into a development directory, make sure that the `bin/` folder of that directory is in your `PATH`.

Next, install *rebar* from <https://github.com/rebar/rebar>. Rebar is a build tool for Erlang projects and it makes our lives a lot easier.


## Quick Start


`my_first_couchdb_plugin` includes two directories `src` and `test` with an `.erl` file in them each. `src/my_first_couchdb_plugin.erl` is where our module code will live and `test/my_first_couchdb_plugin_tests.erl` will contain any tests for that code.

`src/my_first_couchdb_plugin.erl` now should look like this:

    -module(my_first_couchdb_plugin).

    -export([my_func/0]).

    my_func() ->
        ok.

It doesn’t do much, but you get your first module going. Let’s try to compile it.

`my_first_couchdb_plugin` comes with a `Makefile` that helps you with common tasks.

To compile your code, simply run:

    make

The output should be something like this:

    rebar compile
    ==> my_first_couchdb_plugin (compile)
    Compiled src/my_first_couchdb_plugin.erl

The compiled results are stored in a directory called `ebin/`. Poke around in there if you are interested on how this all looks.

To run CouchDB with your new plugin make sure CouchDB isn’t already running elsewhere and then do this:

    make dev

The output should look something like this:


    Erlang R15B03 (erts-5.9.3.1) [source] [64-bit] [smp:4:4] [async-threads:4] [hipe] [kernel-poll:true] [dtrace]

    Eshell V5.9.3.1  (abort with ^G)
    1> Apache CouchDB 1.3.0 (LogLevel=info) is starting.
    Apache CouchDB has started. Time to relax.
    [info] [<0.36.0>] Apache CouchDB has started on http://127.0.0.1:5984/

That means, CouchDB is now running normally, in the foreground as opposed to in the background like you would normally, so you get status and error messages in the terminal. And one more thing, this drops you into an interactive Erlang shell that runs inside your CouchDB instance. To see it, just hit enter and you will get:

    1>

This is the Erlang command prompt and you can enter arbitrary commands. To call our module function, type this:

    my_first_couchdb_plugin:my_func().

And then enter. The full stop at the end is essential. The output should look like this:

    1> my_first_couchdb_plugin:my_func().
    ok
    2>

`ok` is the return value of your function, if you remember the code of `my_first_couchdb_plugin,erl`, you see `ok` is the last statement before the final full stop in the function definition of `my_func()` and thus, it is the return value of that function and we see it in the command prompt. Then the command prompt waits for your next line of input with `2>` (the number increases with each entered command). To get out of the command prompt and to stop CouchDB, just hit `ctrl-c` twice.

Note: from now on you can just type `make dev`, it will run `make` for you internally and compile all changes you may have made in the meantime.

* * *

That is all that is needed to get started building a CouchDB plugin. The rest of this guide will explain how to hook into the various parts of CouchDB that allow you to do all sorts of fun things. That means you can write different types of plugins, once that handle HTTP requests, others that operate on database changes, and yet others that provide a deamon that does useful things for us.

* * *


## Publishing a Plugin

Publishing a plugin is both simple and not so simple. The mechanics are trivial, just type:

    make plugin

and you will see something like this:

    > make plugin
    rebar compile
    ==> my_first_couchdb_plugin (compile)
    my_first_couchdb_plugin-1.0.0-R15B03-1.4.0.tar.gz: 1/MeXYfxeBK7DQyk10/6ucIRusc=

That’s the easy part. The hard part is publishing the plugin. And since this is subject to change a lot in the near future, we will punt on explaining this in detail here, but to see how it works, look into this file in the CouchDB source distribution: `share/www/plugins.html`

* * *


## Creating an HTTP Handler

Our module above is not very useful on its own. You can call it when you are in the Erlang command prompt for your local CouchDB, but CouchDB itself doesn’t know what to do with it and you can’t do anything with it from CouchDB’s HTTP API. Let’s fix that!

CouchDB’s main API is HTTP and thus we can expect to have a lot of infrastructure to work with. Luckily, it is mostly straigtforward to get into and we don’t need to learn a whole lot before can get started, and learn more as we go along.

Before ge get goint, let’s create another file that handles all our HTTP handler code. Create `src/my_first_couchdb_plugin_httpd.erl` and put in the following contents:

    -module(my_first_couchdb_plugin_httpd).

    -export([handle_req/1]).

    -include_lib("couch/include/couch_db.hrl").

    handle_req(#httpd{method='GET'}=Req) ->
        couch_httpd:send_json(Req, {[{<<"hello">>, <<"world">>}]});
    handle_req(Req) ->
        couch_httpd:send_method_not_allowed(Req, "GET").

Let’s go through this line by line:

1. We define the module name, nothing to see here, move along.
3. We export the function `handle_req`, e.g. we make it available to other modules.
5. We include the standard CouchDB header. That gives us access to the internal data structures.
7. We define the function `handle_req`. In particular, we define a clause of that function in a way that it gets invoked whenever CouchDB receives an HTTP `GET` request. It takes one argument, `Req` and it it includes all HTTP request information. `Req` is an instance of the record type `#httpd{}` that we will learn a lot more about in the future.
8. We return a bit of JSON, the Erlang, or ejson, equivalent of `{"hello":"world"}`.
9. We define the final, or default clause for the `handle_req` function. This is a default for all `handle_req` functions and handles the case when an API endpoint was called with an unsupported HTTP method, or path or other request parameters. In our case it will say that our `handle_req` only ever will handle `GET` requests and if it ever sees anything else, a standard “method not allowed” response will be generated for us.


### Registering an HTTP Handler

Now we have a function that can handle HTTP requests, but we haven’t told CouchDB yet, for what API endpoint this should be called.

To do this, we need to take a little detour into the CouchDB configuration system, as CouchDB’s HTTP routing is fully dynamic and configurable at runtime.

To get an idea, open this file in CouchDB source directory: `etc/couchdb/default.ini.tpl.in`, don’t mind the triple file extensions, this is essentialy an `.ini` file. Now scroll down to the section `[httpd_global_handlers]`. You will find a list of API endpoints with mapping to the code in CouchDB that handles it. for example:

    _utils = {couch_httpd_misc_handlers, handle_utils_dir_req, "%localdatadir%/www"}

This means that `/_utils` is handled by the erlang module `couch_httpd_misc_handlers` and its function `handle_utils_dir_req` and it takes one additinal argument that is the document root for Futon.

Another example:

    _all_dbs = {couch_httpd_misc_handlers, handle_all_dbs_req}

This means that `/_all_dbs`, the API endpoint that allows you to list all databases in CouchDB is handled by, again, `couch_httpd_misc_handlers` but this time its function `handle_all_dbs_req`, which does not take an additional argument.

Say we want to make our `handle_req` function answer under the enpoint `/_my_plugin` (you want to start with an underscore here, as CouchDB will consider all other characters as real database names), we would add something like this:

    _my_plugin = {my_first_couchdb_plugin, handle_req}

But don’t add that to `etc/couchdb/default.ini.tpl.in`! Instead, craete a new `.ini` file in your plugin at `priv/default.d/my_first_couchdb_plugin.ini` with these contents:

    [httpd_global_handlers]
    _my_plugin = {my_first_couchdb_plugin_httpd, handle_req}
    

Don’t miss the new line at the end.

Now run `make dev` again, and then open a second terminal:

    curl http://127.0.0.1:5984/_my_plugin
    {"hello":"world"}

It worked, yay!

When we do a `POST` request, that should fail:

    curl -X POST http://127.0.0.1:5984/_my_plugin



TODO:
- show that POST fails as expected
- exploring #httpd{} and how to react to different kinds of HTTP requests
- hook up `handle_req` with `my_func`
...
* * *


## Creating a Daemon

TBD


TODO:
-  add “what can go wrong at this step”
