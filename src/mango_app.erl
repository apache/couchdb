-module(mango_app).
-behavior(application).


-export([
    start/2,
    stop/1
]).


start(_Type, []) ->
    {ok, _} = ranch:start_listener(
            mango_protocol,
            100,
        	ranch_tcp,
        	[{port, 27017}],
        	mango_protocol,
        	[]
        ),
    mango_sup:start_link().


stop([]) ->
    ok.

