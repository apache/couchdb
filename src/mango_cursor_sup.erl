-module(mango_cursor_sup).

-behavior(supervisor).

-export([
    start_link/0,
    init/1
]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init(_Arg) ->
    Children = [
        {
            mango_cursor,
            {mango_cursor, start_link, []},
            temporary,
            2000,
            worker,
            [mango_cursor]
        }
    ],
    {ok, {{simple_one_for_one, 10, 1}, Children}}.

