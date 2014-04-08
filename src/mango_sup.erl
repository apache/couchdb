-module(mango_sup).

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
            mango_cursor_sup,
            {mango_cursor_sup, start_link, []},
            permanent,
            infinity,
            supervisor,
            [mango_cursor_sup]
        }
    ],
    {ok, {{one_for_one, 10, 1}, Children}}.

