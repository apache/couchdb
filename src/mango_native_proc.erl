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
    {reply, map_doc(St, to_binary(Doc)), St};

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
        mango_doc:get_field(Doc, Field)
    end, Fields),
    Values1 = set_nulls(Values0, MissingIsNull),
    case lists:member(not_found, Values1) of
        true ->
            [];
        false ->
            case has_one_array(Values1) of
                true ->
                    Expanded = expand_array(Values1),
                    [[K, null] || K <- Expanded];
                false ->
                    [[Values1, null]]
            end
    end.


set_nulls([], _) ->
    [];
set_nulls([not_found | Rest], true) ->
    [null | set_nulls(Rest, true)];
set_nulls([Else | Rest], false) ->
    [Else | set_nulls(Rest, false)].


has_one_array(Values) ->
    1 == lists:foldl(fun(V, Acc) ->
        Acc + (if is_list(V) -> 1; true -> 0 end)
    end, 0, Values).


expand_array(Values) ->
    {Prefix, Array, Tail} = split_array(Values),
    lists:map(fun(V) ->
        Prefix ++ [V] ++ Tail
    end, Array).


split_array([Values | Rest]) when is_list(Values) ->
    {[], Values, Rest};
split_array([Value | Rest]) ->
    {Prefix, Values, Tail} = split_array(Rest),
    {[Value | Prefix], Values, Tail}.



to_binary({Props}) ->
    Pred = fun({Key, Value}) ->
        {to_binary(Key), to_binary(Value)}
    end,
    {lists:map(Pred, Props)};
to_binary(Data) when is_list(Data) ->
    [to_binary(D) || D <- Data];
to_binary(null) ->
    null;
to_binary(true) ->
    true;
to_binary(false) ->
    false;
to_binary(Data) when is_atom(Data) ->
    list_to_binary(atom_to_list(Data));
to_binary(Data) when is_number(Data) ->
    Data;
to_binary(Data) when is_binary(Data) ->
    Data.
