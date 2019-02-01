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

   job_to_ejson_props/1,
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


job_to_ejson_props(#job{source = Source, targets = Targets} = Job) ->
    [
        {id, Job#job.id},
        {type, Job#job.type},
        {source, Source#shard.name},
        {targets, [T#shard.name || T <- Targets]},
        {job_state, Job#job.job_state},
        {split_state, Job#job.split_state},
        {state_info, state_info_to_ejson(Job#job.state_info)},
        {node, atom_to_binary(Job#job.node, utf8)},
        {time_created, Job#job.time_created},
        {time_started, Job#job.time_started},
        {time_updated, Job#job.time_updated}
    ].


job_from_ejson({Props}) ->
    Id = couch_util:get_value(<<"id">>, Props),
    Type = couch_util:get_value(<<"type">>, Props),
    Source = couch_util:get_value(<<"source">>, Props),
    Targets = couch_util:get_value(<<"targets">>, Props),
    JobState = couch_util:get_value(<<"job_state">>, Props),
    SplitState = couch_util:get_value(<<"split_state">>, Props),
    StateInfo = couch_util:get_value(<<"state_info">>, Props),
    TCreated = couch_util:get_value(<<"time_created">>, Props),
    TStarted = couch_util:get_value(<<"time_started">>, Props),
    TUpdated = couch_util:get_value(<<"time_updated">>, Props),
    #job{
        id = Id,
        type = binary_to_atom(Type, utf8),
        job_state = binary_to_atom(JobState, utf8),
        split_state = binary_to_atom(SplitState, utf8),
        state_info = state_info_from_ejson(StateInfo),
        node = node(),
        time_created = TCreated,
        time_started = TStarted,
        time_updated = TUpdated,
        source = mem3_reshard:shard_from_name(Source),
        targets = [mem3_reshard:shard_from_name(T) || T <- Targets]
    }.


state_to_ejson_props(#state{} = State) ->
    [
        {state, atom_to_binary(State#state.state, utf8)},
        {state_info, state_info_to_ejson(State#job.state_info)},
        {time_updated, State#state.time_updated},
        {node, atom_to_binary(State#state.node, utf8)}
    ].


state_from_ejson(#state{} = State, {Props}) ->
    StateVal = couch_util:get_value(<<"state">>, Props),
    StateInfo = couch_util:get_value(<<"state_info">>, Props),
    TUpdated = couch_util:get_value(<<"time_updated">>, Props),
    State#state{
        state = binary_to_atom(StateVal, utf8),
        state_info = state_info_from_ejson(StateInfo),
        node = node(),
        time_updated = TUpdated
    }.


state_info_from_ejson({Props}) ->
    Props1 = [{binary_to_atom(K, utf8), couch_util:to_binary(V)}
        || {K, V} <- Props],
    lists:sort(Props1).


state_info_to_ejson(Props) ->
    {lists:sort([{K, couch_util:to_binary(V)} || {K, V} <- Props])}.


store_state() ->
    config:get_boolean("mem3_reshard", "store_state", true).
