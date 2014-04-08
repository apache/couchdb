-module(mango_cursor_view).

-export([
    init/5,
    run/1
]).

-export([
    handle_message/2
]).


-include_lib("couch/include/couch_db.hrl").


-record(st, {
    cursor,
    index,
    ranges,
    selector,
    opts,
    ctx,
    acc = []
}).


init(Index, Ranges, Selector, Opts, Ctx) ->
    St = #st{
        cursor = self(),
        index = Index,
        ranges = Ranges,
        selector = Selector,
        opts = Opts,
        ctx = Ctx
    },
    {Pid, _Ref} = erlang:spawn_monitor(?MODULE, run, [St]),
    {ok, Pid}.


run(#st{index=Idx}=St) ->
    erlang:monitor(St#st.cursor),
    DbName = couch_index:dbname(Idx),
    DDoc = couch_index:ddoc(Idx),
    Name = couch_index:name(Idx),
    Args = #view_query_args{
        start_key = start_key(St#st.ranges),
        end_key = end_key(St#st.ranges),
        limit = limit(St#st.opts),
        include_docs = true
    },
    CB = fun ?MODULE:handle_message/2,
    fabric:query_view(DbName, DDoc, Name, CB, St, Args).


handle_message({total_and_offset, _, _}, St) ->
    {ok, St};
handle_message({row, {Props}}, St) ->
    Doc = couch_util:get_value(doc, Props),
    Acc = case mango_selector:match(St#st.selector, Doc) of
        true ->
            [Doc | St#st.acc];
        false ->
            St#st.acc
    end,
    case length(Acc) >= batch_size(St#st.opts) of
        true ->
            send_batch(St, Acc);
        false ->
            {ok, St#st{acc = Acc}}
    end;
handle_message(complete, St) ->
    case length(St#st.acc) > 0 of
        true ->
            send_batch(St, St#st.acc);
        false ->
            {ok, normal}
    end;
handle_message({error, Reason}, _St) ->
    {error, Reason}.


send_batch(#st{cursor=Cursor}=St, Batch) ->
    Cursor ! {self(), batch, Batch},
    Timeout = inactivity_timeout(St#st.opts),
    receive
        {Cursor, next} ->
            {ok, St#st{acc = []}};
        {Cursor, close} ->
            erlang:exit(normal);
        {'DOWN', _, _, Cursor, _} ->
            erlang:exit(normal)
        after Timeout ->
            erlang:exit(timeout)
    end.


limit(Opts) ->
    case lists:keyfind(limit, 1, Opts) of
        {_, L} when is_integer(L), L > 0 ->
            L;
        _ ->
            % Views just expect a big number for "no limit".
            % A bit silly, but when in Rome...
            10000000000
    end.


batch_size(Opts) ->
    case lists:keyfind(batch_size, 1, Opts) of
        {_, Count} when is_integer(Count), Count > 0 ->
            Count;
        _ ->
            25
    end.


inactivity_timeout(Opts) ->
    case lists:keyfind(timeout, 1, Opts) of
        {_, TO} when is_integer(TO), TO > 0 ->
            TO;
        _ ->
            % Mongo default of 10m
            600000
    end.


start_key([]) ->
    [];
start_key([{'$gt', Key, _, _} | Rest]) ->
    [Key | start_key(Rest)];
start_key([{'$gte', Key, _, _} | Rest]) ->
    [Key | start_key(Rest)];
start_key([{'$eq', Key, '$eq', Key} | Rest]) ->
    [Key | start_key(Rest)].


end_key([]) ->
    [];
end_key([{_, _, '$lt', Key} | Rest]) ->
    [Key | end_key(Rest)];
end_key([{_, _, '$lte', Key} | Rest]) ->
    [Key | end_key(Rest)];
end_key([{'$eq', Key, '$eq', Key} | Rest]) ->
    [Key | end_key(Rest)].
