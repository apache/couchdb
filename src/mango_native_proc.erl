-module(mango_native_proc).
-behavior(gen_server).


-export([
    start_link/0,
    set_timeout/2,
    prompt/2
]).

-export([
    init/1,
    terminate/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3
]).


-record(st, {
    indexes = [],
    timeout = 5000
}).


start_link() ->
    gen_server:start_link(?MODULE, [], []).


set_timeout(Pid, TimeOut) when is_integer(TimeOut), TimeOut > 0 ->
    gen_server:call(Pid, {set_timeout, TimeOut}).


prompt(Pid, Data) ->
    gen_server:call(Pid, {prompt, Data}).


init(_) ->
    {ok, #st{}}.


terminate(_Reason, _St) ->
    ok.


handle_call({set_timeout, TimeOut}, _From, St) ->
    {reply, ok, St#st{timeout=TimeOut}};

handle_call({prompt, [<<"reset">>]}, _From, St) ->
    {reply, true, St#st{indexes=[]}};

handle_call({prompt, [<<"reset">>, _QueryConfig]}, _From, St) ->
    {reply, true, St#st{indexes=[]}};

handle_call({prompt, [<<"add_fun">>, IndexInfo]}, _From, St) ->
    Indexes = St#st.indexes ++ [IndexInfo],
    NewSt = St#st{indexes = Indexes},
    {reply, true, NewSt};

handle_call({prompt, [<<"map_doc">>, Doc]}, _From, St) ->
    {reply, map_doc(St, mango_json:to_binary(Doc)), St};

handle_call({prompt, [<<"reduce">>, _, _]}, _From, St) ->
    {reply, null, St};

handle_call({prompt, [<<"rereduce">>, _, _]}, _From, St) ->
    {reply, null, St};

handle_call(Msg, _From, St) ->
    {stop, {invalid_call, Msg}, {invalid_call, Msg}, St}.


handle_cast(garbage_collect, St) ->
    erlang:garbage_collect(),
    {noreply, St};

handle_cast(Msg, St) ->
    {stop, {invalid_cast, Msg}, St}.


handle_info(Msg, St) ->
    {stop, {invalid_info, Msg}, St}.


code_change(_OldVsn, St, _Extra) ->
    {ok, St}.


map_doc(#st{indexes=Indexes}, Doc) ->
    lists:map(fun(Idx) -> get_index_entries(Idx, Doc) end, Indexes).


get_index_entries({IdxProps}, Doc) ->
    {Fields} = couch_util:get_value(<<"fields">>, IdxProps),
    MissingIsNull = couch_util:get_value(<<"missing_is_null">>, IdxProps),
    Values0 = lists:map(fun({Field, _Dir}) ->
        case mango_doc:get_field(Doc, Field) of
            not_found -> not_found;
            bad_path -> not_found;
            Else -> Else
        end
    end, Fields),
    Values1 = set_nulls(Values0, MissingIsNull),
    case lists:member(not_found, Values1) of
        true ->
            [];
        false ->
            [[Values1, null]]
    end.


set_nulls([], _) ->
    [];
set_nulls([not_found | Rest], true) ->
    [null | set_nulls(Rest, true)];
set_nulls([Else | Rest], SetNull) ->
    [Else | set_nulls(Rest, SetNull)].
