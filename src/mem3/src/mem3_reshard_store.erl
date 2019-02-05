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

-module(mem3_reshard_store).


-export([
   init/3,

   store_job/2,
   load_job/2,
   delete_job/2,
   get_jobs/1,

   store_state/1,
   load_state/1,
   delete_state/1,  % for debugging

   job_to_ejson_props/2,
   state_to_ejson_props/1
]).


-include_lib("couch/include/couch_db.hrl").
-include("mem3_reshard.hrl").


-spec init(#state{}, binary(), binary()) -> #state{}.
init(#state{} = State, JobPrefix, StateDocId) ->
    State#state{
        job_prefix = <<?LOCAL_DOC_PREFIX, JobPrefix/binary>>,
        state_id = <<?LOCAL_DOC_PREFIX, StateDocId/binary>>
    }.


-spec store_job(#state{}, #job{}) -> ok.
store_job(#state{job_prefix = Prefix}, #job{id = Id} = Job) ->
    with_shards_db(fun(Db) ->
        DocId = <<Prefix/binary, Id/binary>>,
        ok = update_doc(Db, DocId, job_to_ejson_props(Job))
    end).


-spec load_job(#state{}, binary()) -> {ok, {[_]}} | not_found.
load_job(#state{job_prefix = Prefix}, Id) ->
    with_shards_db(fun(Db) ->
        case load_doc(Db, <<Prefix/binary, Id/binary>>) of
            {ok, DocBody} ->
                {ok, job_from_ejson(DocBody)};
            not_found ->
                not_found
        end
    end).


-spec delete_job(#state{}, binary()) -> ok.
delete_job(#state{job_prefix = Prefix}, Id) ->
    with_shards_db(fun(Db) ->
        DocId = <<Prefix/binary, Id/binary>>,
        ok = delete_doc(Db, DocId)
    end).


-spec get_jobs(#state{}) -> [#job{}].
get_jobs(#state{job_prefix = Prefix}) ->
    with_shards_db(fun(Db) ->
        PrefixLen = byte_size(Prefix),
        FoldFun = fun(#doc{id = Id, body = Body}, Acc) ->
            case Id of
                <<Prefix:PrefixLen/binary, _/binary>> ->
                    {ok, [job_from_ejson(Body) | Acc]};
                _ ->
                    {stop, Acc}
           end
        end,
        Opts = [{start_key, Prefix}],
        {ok, Jobs} = couch_db:fold_local_docs(Db, FoldFun, [], Opts),
        lists:reverse(Jobs)
    end).


-spec store_state(#state{}) -> ok.
store_state(#state{state_id = DocId} = State) ->
    with_shards_db(fun(Db) ->
        ok = update_doc(Db, DocId, state_to_ejson_props(State))
    end).


-spec load_state(#state{}) -> #state{}.
load_state(#state{state_id = DocId} = State) ->
    with_shards_db(fun(Db) ->
        case load_doc(Db, DocId) of
            {ok, DocBody} ->
                state_from_ejson(State, DocBody);
            not_found ->
                State
        end
    end).


-spec delete_state(#state{}) -> ok.
delete_state(#state{state_id = DocId}) ->
    with_shards_db(fun(Db) ->
        ok = delete_doc(Db, DocId)
    end).


% Private API


with_shards_db(Fun) ->
    DbName = config:get("mem3", "shards_db", "_dbs"),
    case mem3_util:ensure_exists(DbName) of
        {ok, Db} ->
            try
                Fun(Db)
            after
                catch couch_db:close(Db)
            end;
        Else ->
            throw(Else)
    end.


delete_doc(Db, DocId) ->
    case couch_db:open_doc(Db, DocId, []) of
        {ok, #doc{revs = {_, Revs}}} ->
            {ok, _} = couch_db:delete_doc(Db, DocId, Revs),
            {ok, _} = couch_db:ensure_full_commit(Db),
            ok;
        {not_found, _} ->
            ok
    end.

update_doc(Db, DocId, Body) ->
    DocProps = [{<<"_id">>, DocId}] ++ Body,
    Body1 = ?JSON_DECODE(?JSON_ENCODE({DocProps})),
    BaseDoc = couch_doc:from_json_obj(Body1),
    Doc = case couch_db:open_doc(Db, DocId, []) of
        {ok, #doc{revs = Revs}} ->
            BaseDoc#doc{revs = Revs};
        {not_found, _} ->
            BaseDoc
    end,
    case store_state() of
        true ->
            {ok, _} = couch_db:update_doc(Db, Doc, []),
            couch_log:notice("~p updated doc ~p ~p", [?MODULE, DocId, Body]),
            {ok, _} = couch_db:ensure_full_commit(Db),
            ok;
        false ->
            couch_log:debug("~p not storing state in ~p", [?MODULE, DocId]),
            ok
    end.


load_doc(Db, DocId) ->
    case couch_db:open_doc(Db, DocId, [ejson_body]) of
        {ok, #doc{body = Body}} ->
            couch_log:debug("~p loaded doc ~p ~p", [?MODULE, DocId, Body]),
            {ok, Body};
        {not_found, _} ->
            not_found
    end.


job_to_ejson_props(#job{} = Job) ->
    job_to_ejson_props(Job, []).


job_to_ejson_props(#job{source = Source, targets = Targets} = Job, Opts) ->
    Iso8601 = proplists:get_value(iso8601, Opts),
    History = history_to_ejson(Job#job.history, Iso8601),
    StartTime = case Iso8601 of
        true -> iso8601(Job#job.start_time);
        _ -> Job#job.start_time
    end,
    UpdateTime = case Iso8601 of
        true -> iso8601(Job#job.update_time);
        _ -> Job#job.update_time
    end,
    [
        {id, Job#job.id},
        {type, Job#job.type},
        {source, Source#shard.name},
        {targets, [T#shard.name || T <- Targets]},
        {job_state, Job#job.job_state},
        {split_state, Job#job.split_state},
        {state_info, state_info_to_ejson(Job#job.state_info)},
        {node, atom_to_binary(Job#job.node, utf8)},
        {start_time, StartTime},
        {update_time, UpdateTime},
        {history, History}
    ].


job_from_ejson({Props}) ->
    Id = couch_util:get_value(<<"id">>, Props),
    Type = couch_util:get_value(<<"type">>, Props),
    Source = couch_util:get_value(<<"source">>, Props),
    Targets = couch_util:get_value(<<"targets">>, Props),
    JobState = couch_util:get_value(<<"job_state">>, Props),
    SplitState = couch_util:get_value(<<"split_state">>, Props),
    StateInfo = couch_util:get_value(<<"state_info">>, Props),
    TStarted = couch_util:get_value(<<"start_time">>, Props),
    TUpdated = couch_util:get_value(<<"update_time">>, Props),
    History = couch_util:get_value(<<"history">>, Props),
    #job{
        id = Id,
        type = binary_to_atom(Type, utf8),
        job_state = binary_to_atom(JobState, utf8),
        split_state = binary_to_atom(SplitState, utf8),
        state_info = state_info_from_ejson(StateInfo),
        node = node(),
        start_time = TStarted,
        update_time = TUpdated,
        source = mem3_reshard:shard_from_name(Source),
        targets = [mem3_reshard:shard_from_name(T) || T <- Targets],
        history = history_from_ejson(History)
    }.


state_to_ejson_props(#state{} = State) ->
    [
        {state, atom_to_binary(State#state.state, utf8)},
        {state_info, state_info_to_ejson(State#state.state_info)},
        {update_time, State#state.update_time},
        {node, atom_to_binary(State#state.node, utf8)}
    ].


state_from_ejson(#state{} = State, {Props}) ->
    StateVal = couch_util:get_value(<<"state">>, Props),
    StateInfo = couch_util:get_value(<<"state_info">>, Props),
    TUpdated = couch_util:get_value(<<"update_time">>, Props),
    State#state{
        state = binary_to_atom(StateVal, utf8),
        state_info = state_info_from_ejson(StateInfo),
        node = node(),
        update_time = TUpdated
    }.


state_info_from_ejson({Props}) ->
    Props1 = [{binary_to_atom(K, utf8), couch_util:to_binary(V)}
        || {K, V} <- Props],
    lists:sort(Props1).


history_to_ejson(Hist, true) when is_list(Hist) ->
    [{[{timestamp, iso8601(T)}, {type, S}, {detail, D}]} || {S, D, T} <- Hist];

history_to_ejson(Hist, _) when is_list(Hist) ->
    [{[{timestamp, T}, {type, S}, {detail, D}]} || {S, D, T} <- Hist].


history_from_ejson(HistoryEJson) when is_list(HistoryEJson) ->
    lists:map(fun({EventProps}) ->
        Timestamp = couch_util:get_value(<<"timestamp">>, EventProps),
        State = couch_util:get_value(<<"type">>, EventProps),
        Detail = couch_util:get_value(<<"detail">>, EventProps),
        {Timestamp, binary_to_atom(State, utf8), Detail}
    end, HistoryEJson).


state_info_to_ejson(Props) ->
    {lists:sort([{K, couch_util:to_binary(V)} || {K, V} <- Props])}.


store_state() ->
    config:get_boolean("mem3_reshard", "store_state", true).


iso8601(UnixSec) ->
    Mega = UnixSec div 1000000,
    Sec = UnixSec rem 1000000,
    {{Y, Mon, D}, {H, Min, S}} = calendar:now_to_universal_time({Mega, Sec, 0}),
    Format = "~B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
    iolist_to_binary(io_lib:format(Format, [Y, Mon, D, H, Min, S])).
