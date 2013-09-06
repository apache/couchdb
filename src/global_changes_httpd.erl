%% Copyright 2013 Cloudant

-module(global_changes_httpd).

-export([handle_global_changes_req/1]).

-include_lib("couch/include/couch_db.hrl").

-record(acc, {
    heartbeat_interval,
    last_data_sent_time,
    feed,
    prepend,
    resp,
    etag,
    username
}).

handle_global_changes_req(#httpd{method='GET'}=Req) ->
    Db = global_changes_util:get_dbname(),
    Feed = couch_httpd:qs_value(Req, "feed", "normal"),
    Options = parse_global_changes_query(Req),
    Heartbeat = case lists:keyfind(heartbeat, 1, Options) of
        {heartbeat, Other} -> Other;
        {heartbeat, true} -> 60000;
        false -> false
    end,
    chttpd:verify_is_server_admin(Req),
    Acc = #acc{
        username=admin,
        feed=Feed,
        resp=Req,
        heartbeat_interval=Heartbeat
    },
    case Feed of
        "normal" ->
            {ok, Info} = fabric:get_db_info(Db),
            Etag = chttpd:make_etag(Info),
            chttpd:etag_respond(Req, Etag, fun() ->
                fabric:changes(Db, fun changes_callback/2, Acc#acc{etag=Etag}, Options)
            end);
        Feed when Feed =:= "continuous"; Feed =:= "longpoll" ->
            fabric:changes(Db, fun changes_callback/2, Acc, Options);
        _ ->
            Msg = <<"Supported `feed` types: normal, continuous, longpoll">>,
            throw({bad_request, Msg})
    end;
handle_global_changes_req(Req) ->
    chttpd:send_method_not_allowed(Req, "GET").


transform_change(Username, _Resp, {Props}) ->
    {id, Id} = lists:keyfind(id, 1, Props),
    {seq, Seq} = lists:keyfind(seq, 1, Props),
    Info = case binary:split(Id, <<":">>) of
        [Event0, DbNameAndUsername] ->
            case binary:split(DbNameAndUsername, <<"/">>) of
                [AccountName0, DbName0] ->
                    {Event0, AccountName0, DbName0};
                [DbName0] ->
                    {Event0, '_admin', DbName0}
            end;
        _ ->
            skip
    end,
    case Info of
        % Matches the client's username
        {Event, Username, DbName} when Username /= admin ->
            {[
                {dbname, DbName},
                {type, Event},
                {seq, Seq}
            ]};
        % Client is an admin, show them everything.
        {Event, AccountName, DbName} when Username == admin ->
            {[
                {dbname, DbName},
                {type, Event},
                {account, AccountName},
                {seq, Seq}
            ]};
        _ ->
            skip
    end.


% callbacks for continuous feed (newline-delimited JSON Objects)
changes_callback(start, #acc{feed="continuous"}=Acc) ->
    #acc{resp=Req} = Acc,
    {ok, Resp} = chttpd:start_delayed_json_response(Req, 200),
    {ok, Acc#acc{resp=Resp, last_data_sent_time=os:timestamp()}};
changes_callback({change, Change0}, #acc{feed="continuous"}=Acc) ->
    #acc{resp=Resp, username=Username} = Acc,
    case transform_change(Username, Resp, Change0) of
        skip ->
            {ok, maybe_send_heartbeat(Acc)};
        Change ->
            Line = [?JSON_ENCODE(Change) | "\n"],
            {ok, Resp1} = chttpd:send_delayed_chunk(Resp, Line),
            {ok, Acc#acc{resp=Resp1, last_data_sent_time=os:timestamp()}}
    end;
changes_callback({stop, EndSeq}, #acc{feed="continuous"}=Acc) ->
    #acc{resp=Resp} = Acc,
    {ok, Resp1} = chttpd:send_delayed_chunk(Resp,
        [?JSON_ENCODE({[{<<"last_seq">>, EndSeq}]}) | "\n"]),
    chttpd:end_delayed_json_response(Resp1);

% callbacks for longpoll and normal (single JSON Object)
changes_callback(start, #acc{feed="normal", etag=Etag}=Acc)
        when Etag =/= undefined ->
    #acc{resp=Req} = Acc,
    FirstChunk = "{\"results\":[\n",
    {ok, Resp} = chttpd:start_delayed_json_response(Req, 200,
       [{"Etag",Etag}], FirstChunk),
    {ok, Acc#acc{resp=Resp, prepend="", last_data_sent_time=os:timestamp()}};
changes_callback(start, Acc) ->
    #acc{resp=Req} = Acc,
    FirstChunk = "{\"results\":[\n",
    {ok, Resp} = chttpd:start_delayed_json_response(Req, 200, [], FirstChunk),
    {ok, Acc#acc{resp=Resp, prepend="", last_data_sent_time=os:timestamp()}};
changes_callback({change, Change0}, Acc) ->
    #acc{resp=Resp, prepend=Prepend, username=Username} = Acc,
    case transform_change(Username, Resp, Change0) of
        skip ->
            {ok, maybe_send_heartbeat(Acc)};
        Change ->
            #acc{resp=Resp, prepend=Prepend} = Acc,
            Line = [Prepend, ?JSON_ENCODE(Change)],
            {ok, Resp1} = chttpd:send_delayed_chunk(Resp, Line),
            Acc1 = Acc#acc{
                prepend=",\r\n",
                resp=Resp1,
                last_data_sent_time=os:timestamp()
            },
            {ok, Acc1}
    end;
changes_callback({stop, EndSeq}, Acc) ->
    #acc{resp=Resp} = Acc,
    {ok, Resp1} = chttpd:send_delayed_chunk(Resp,
        ["\n],\n\"last_seq\":", ?JSON_ENCODE(EndSeq), "}\n"]),
    chttpd:end_delayed_json_response(Resp1);

changes_callback(timeout, Acc) ->
    {ok, maybe_send_heartbeat(Acc)};

changes_callback({error, Reason}, #acc{resp=Req=#httpd{}}) ->
    chttpd:send_error(Req, Reason);
changes_callback({error, Reason}, Acc) ->
    #acc{etag=Etag, feed=Feed, resp=Resp} = Acc,
    case {Feed, Etag} of
        {"normal", Etag} when Etag =/= undefined ->
            chttpd:send_error(Resp, Reason);
        _ ->
            chttpd:send_delayed_error(Resp, Reason)
    end.


maybe_send_heartbeat(#acc{heartbeat_interval=false}=Acc) ->
    Acc;
maybe_send_heartbeat(Acc) ->
    #acc{last_data_sent_time=LastSentTime, heartbeat_interval=Interval, resp=Resp} = Acc,
    Now = os:timestamp(),
    case timer:now_diff(Now, LastSentTime) div 1000 > Interval of
        true ->
            {ok, Resp1} = chttpd:send_delayed_chunk(Resp, "\n"),
            Acc#acc{last_data_sent_time=Now, resp=Resp1};
        false ->
            Acc
    end.


parse_global_changes_query(Req) ->
    lists:foldl(fun({Key, Value}, Args) ->
        case {Key, Value} of
        {"feed", _} ->
            [{feed, Value} | Args];
        {"descending", "true"} ->
            [{dir, rev} | Args];
        {"since", _} ->
            [{since, Value} | Args];
        {"limit", _} ->
            [{limit, to_non_neg_int(Value)} | Args];
        {"heartbeat", "true"} ->
            [{heartbeat, true} | Args];
        {"heartbeat", _} ->
            [{heartbeat, to_non_neg_int(Value)} | Args];
        {"timeout", _} ->
            [{timeout, to_non_neg_int(Value)} | Args];
        _Else -> % unknown key value pair, ignore.
            Args
        end
    end, [], couch_httpd:qs(Req)).


to_non_neg_int(Value) ->
    try list_to_integer(Value) of
        V when V >= 0 ->
            V;
        _ ->
            throw({bad_request, invalid_integer})
    catch error:badarg ->
        throw({bad_request, invalid_integer})
    end.
