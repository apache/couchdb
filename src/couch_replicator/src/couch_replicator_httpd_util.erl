% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(couch_replicator_httpd_util).

-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").

-export([
    validate_rep_props/1,
    parse_int_param/5,
    parse_replication_state_filter/1,
    update_db_name/1,
    docs_acc_new/3,
    docs_acc_response/1,
    docs_cb/2
]).

-import(couch_httpd, [
    send_json/2,
    send_json/3,
    send_method_not_allowed/2
]).

-import(couch_util, [
    to_binary/1
]).


parse_replication_state_filter(undefined) ->
    [];  % This is the default (wildcard) filter
parse_replication_state_filter(States) when is_list(States) ->
    AllStates = couch_replicator:replication_states(),
    StrStates = [string:to_lower(S) || S <- string:tokens(States, ",")],
    AtomStates = try
        [list_to_existing_atom(S) || S <- StrStates]
    catch error:badarg ->
        Msg1 = io_lib:format("States must be one or more of ~w", [AllStates]),
        throw({query_parse_error, ?l2b(Msg1)})
    end,
    AllSet = sets:from_list(AllStates),
    StatesSet = sets:from_list(AtomStates),
    Diff = sets:to_list(sets:subtract(StatesSet, AllSet)),
    case Diff of
    [] ->
        AtomStates;
    _ ->
        Args = [Diff, AllStates],
        Msg2 = io_lib:format("Unknown states ~w. Choose from: ~w", Args),
        throw({query_parse_error, ?l2b(Msg2)})
    end.


parse_int_param(Req, Param, Default, Min, Max) ->
    IntVal = try
        list_to_integer(chttpd:qs_value(Req, Param, integer_to_list(Default)))
    catch error:badarg ->
        Msg1 = io_lib:format("~s must be an integer", [Param]),
        throw({query_parse_error, ?l2b(Msg1)})
    end,
    case IntVal >= Min andalso IntVal =< Max of
    true ->
        IntVal;
    false ->
        Msg2 = io_lib:format("~s not in range of [~w,~w]", [Param, Min, Max]),
        throw({query_parse_error, ?l2b(Msg2)})
    end.


validate_rep_props([]) ->
    ok;
validate_rep_props([{<<"query_params">>, {Params}}|Rest]) ->
    lists:foreach(fun
        ({_,V}) when is_binary(V) -> ok;
        ({K,_}) -> throw({bad_request,
            <<K/binary," value must be a string.">>})
        end, Params),
    validate_rep_props(Rest);
validate_rep_props([_|Rest]) ->
    validate_rep_props(Rest).


prepend_val(#vacc{prepend=Prepend}) ->
    case Prepend of
        undefined ->
            "";
        _ ->
            Prepend
    end.


maybe_flush_response(#vacc{bufsize=Size, threshold=Max} = Acc, Data, Len)
        when Size > 0 andalso (Size + Len) > Max ->
    #vacc{buffer = Buffer, resp = Resp} = Acc,
    {ok, R1} = chttpd:send_delayed_chunk(Resp, Buffer),
    {ok, Acc#vacc{prepend = ",\r\n", buffer = Data, bufsize = Len, resp = R1}};
maybe_flush_response(Acc0, Data, Len) ->
    #vacc{buffer = Buf, bufsize = Size} = Acc0,
    Acc = Acc0#vacc{
        prepend = ",\r\n",
        buffer = [Buf | Data],
        bufsize = Size + Len
    },
    {ok, Acc}.

docs_acc_new(Req, Db, Threshold) ->
     #vacc{db=Db, req=Req, threshold=Threshold}.

docs_acc_response(#vacc{resp = Resp}) ->
    Resp.

docs_cb({error, Reason}, #vacc{resp=undefined}=Acc) ->
    {ok, Resp} = chttpd:send_error(Acc#vacc.req, Reason),
    {ok, Acc#vacc{resp=Resp}};

docs_cb(complete, #vacc{resp=undefined}=Acc) ->
    % Nothing in view
    {ok, Resp} = chttpd:send_json(Acc#vacc.req, 200, {[{rows, []}]}),
    {ok, Acc#vacc{resp=Resp}};

docs_cb(Msg, #vacc{resp=undefined}=Acc) ->
    %% Start response
    Headers = [],
    {ok, Resp} = chttpd:start_delayed_json_response(Acc#vacc.req, 200, Headers),
    docs_cb(Msg, Acc#vacc{resp=Resp, should_close=true});

docs_cb({error, Reason}, #vacc{resp=Resp}=Acc) ->
    {ok, Resp1} = chttpd:send_delayed_error(Resp, Reason),
    {ok, Acc#vacc{resp=Resp1}};

docs_cb(complete, #vacc{resp=Resp, buffer=Buf, threshold=Max}=Acc) ->
    % Finish view output and possibly end the response
    {ok, Resp1} = chttpd:close_delayed_json_object(Resp, Buf, "\r\n]}", Max),
    case Acc#vacc.should_close of
        true ->
            {ok, Resp2} = chttpd:end_delayed_json_response(Resp1),
            {ok, Acc#vacc{resp=Resp2}};
        _ ->
            {ok, Acc#vacc{resp=Resp1, meta_sent=false, row_sent=false,
                prepend=",\r\n", buffer=[], bufsize=0}}
    end;

docs_cb({meta, Meta}, #vacc{meta_sent=false, row_sent=false}=Acc) ->
    % Sending metadata as we've not sent it or any row yet
    Parts = case couch_util:get_value(total, Meta) of
        undefined -> [];
        Total -> [io_lib:format("\"total_rows\":~p", [adjust_total(Total)])]
    end ++ case couch_util:get_value(offset, Meta) of
        undefined -> [];
        Offset -> [io_lib:format("\"offset\":~p", [Offset])]
    end ++ ["\"docs\":["],
    Chunk = [prepend_val(Acc), "{", string:join(Parts, ","), "\r\n"],
    {ok, AccOut} = maybe_flush_response(Acc, Chunk, iolist_size(Chunk)),
    {ok, AccOut#vacc{prepend="", meta_sent=true}};


docs_cb({meta, _Meta}, #vacc{}=Acc) ->
    %% ignore metadata
    {ok, Acc};

docs_cb({row, Row}, #vacc{meta_sent=false}=Acc) ->
    %% sorted=false and row arrived before meta
    % Adding another row
    Chunk = [prepend_val(Acc), "{\"docs\":[\r\n", row_to_json(Row)],
    maybe_flush_response(Acc#vacc{meta_sent=true, row_sent=true}, Chunk, iolist_size(Chunk));

docs_cb({row, Row}, #vacc{meta_sent=true}=Acc) ->
    % Adding another row
    Chunk = [prepend_val(Acc), row_to_json(Row)],
    maybe_flush_response(Acc#vacc{row_sent=true}, Chunk, iolist_size(Chunk)).


update_db_name({Props}) ->
    {value, {database, DbName}, Props1} = lists:keytake(database, 1, Props),
    {[{database, normalize_db_name(DbName)} | Props1]}.

normalize_db_name(<<"shards/", _/binary>> = DbName) ->
    mem3:dbname(DbName);
normalize_db_name(DbName) ->
    DbName.

row_to_json(Row) ->
    Doc0 = couch_util:get_value(doc, Row),
    Doc1 = update_db_name(Doc0),
    ?JSON_ENCODE(Doc1).


%% Adjust Total as there is an automatically created validation design doc
adjust_total(Total) when is_integer(Total), Total > 0 ->
    Total - 1;
adjust_total(Total) when is_integer(Total) ->
    0.
