% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
% http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

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
    username,
    limit
}).

handle_global_changes_req(#httpd{method='GET'}=Req) ->
    Db = global_changes_util:get_dbname(),
    Feed = chttpd:qs_value(Req, "feed", "normal"),
    Options = parse_global_changes_query(Req),
    Heartbeat = case lists:keyfind(heartbeat, 1, Options) of
        {heartbeat, true} -> 60000;
        {heartbeat, Other} -> Other;
        false -> false
    end,
    % Limit is handled in the changes callback, since the limit count needs to
    % only account for changes which happen after the filter.
    Limit = couch_util:get_value(limit, Options),
    %Options1 = lists:keydelete(limit, 1, Options),
    Options1 = Options,
    Owner = allowed_owner(Req),
    Acc = #acc{
        username=Owner,
        feed=Feed,
        resp=Req,
        heartbeat_interval=Heartbeat,
        limit=Limit
    },
    case Feed of
        "normal" ->
            {ok, Info} = fabric:get_db_info(Db),
            Etag = chttpd:make_etag(Info),
            chttpd:etag_respond(Req, Etag, fun() ->
                fabric:changes(Db, fun changes_callback/2, Acc#acc{etag=Etag}, Options1)
            end);
        Feed when Feed =:= "continuous"; Feed =:= "longpoll" ->
            fabric:changes(Db, fun changes_callback/2, Acc, Options1);
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
        [Event0, DbName0] ->
            {Event0, DbName0};
        _ ->
            skip
    end,
    case Info of
        % Client is an admin, show them everything.
        {Event, DbName} when Username == admin ->
            {[
                {db_name, DbName},
                {type, Event},
                {seq, Seq}
            ]};
        _ ->
            skip
    end.


% This clause is only hit when _db_updates is queried with limit=0. For
% limit>0, the request is stopped by maybe_finish/1.
changes_callback({change, _}, #acc{limit=0}=Acc) ->
    {stop, Acc};

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
            Acc1 = Acc#acc{
                resp=Resp1,
                last_data_sent_time=os:timestamp()
            },
            maybe_finish(Acc1)
    end;
changes_callback({stop, EndSeq}, #acc{feed="continuous"}=Acc) ->
    % Temporary upgrade clause - Case 24236
    changes_callback({stop, EndSeq, null}, Acc);
changes_callback({stop, EndSeq, _Pending}, #acc{feed="continuous"}=Acc) ->
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
    {ok, Acc#acc{
        resp=Resp,
        prepend="",
        last_data_sent_time=os:timestamp()
    }};
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
            maybe_finish(Acc1)
    end;
changes_callback({stop, EndSeq}, Acc) ->
    % Temporary upgrade clause - Case 24236
    changes_callback({stop, EndSeq, null}, Acc);
changes_callback({stop, EndSeq, _Pending}, Acc) ->
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


maybe_finish(Acc) ->
    case Acc#acc.limit of
        1 ->
            {stop, Acc};
        undefined ->
            {ok, Acc};
        Limit ->
            {ok, Acc#acc{limit=Limit-1}}
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
        {"heartbeat", "false"} ->
            Args;
        {"heartbeat", _} ->
            [{heartbeat, to_non_neg_int(Value)} | Args];
        {"timeout", _} ->
            [{timeout, to_non_neg_int(Value)} | Args];
        _Else -> % unknown key value pair, ignore.
            Args
        end
    end, [], chttpd:qs(Req)).


to_non_neg_int(Value) ->
    try list_to_integer(Value) of
        V when V >= 0 ->
            V;
        _ ->
            throw({bad_request, invalid_integer})
    catch error:badarg ->
        throw({bad_request, invalid_integer})
    end.

allowed_owner(Req) ->
    case config:get("global_changes", "allowed_owner", undefined) of
    undefined ->
        chttpd:verify_is_server_admin(Req),
        admin;
    SpecStr ->
        {ok, {M, F, A}} = couch_util:parse_term(SpecStr),
        couch_util:validate_callback_exists(M, F, 2),
        M:F(Req, A)
    end.
