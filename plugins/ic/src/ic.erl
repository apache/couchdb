-module(ic).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = ic_prv_setup_eunit:init(State),
    {ok, State1}.
