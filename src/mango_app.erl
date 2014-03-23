-module(mango_app).
-behavior(application).


-export([
    start/2,
    stop/1
]).


start(_Type, []) ->
    Port = list_to_integer(config:get("mongo", "port")),
    Backlog = list_to_integer(config:get("mongo", "backlog")),
    {ok, _} = ranch:start_listener(
            mango_tcp,
            Backlog,
        	ranch_tcp,
        	[{port, Port}],
        	mango_protocol,
        	[]
        ),
    mango_sup:start_link().


stop([]) ->
    ok.

