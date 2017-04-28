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

-module(couch_replicator_fabric_rpc).

-export([
   docs/3
]).

-include_lib("fabric/include/fabric.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").


docs(DbName, Options, Args0) ->
    set_io_priority(DbName, Options),
    #mrargs{skip = Skip, limit = Limit, extra = Extra} = Args0,
    FilterStates = proplists:get_value(filter_states, Extra),
    Args = Args0#mrargs{skip = 0, limit = Skip + Limit},
    HealthThreshold = couch_replicator_scheduler:health_threshold(),
    {ok, Db} = couch_db:open_int(DbName, Options),
    Acc = {DbName, FilterStates, HealthThreshold},
    couch_mrview:query_all_docs(Db, Args, fun docs_cb/2, Acc).


docs_cb({meta, Meta}, Acc) ->
    ok = rexi:stream2({meta, Meta}),
    {ok, Acc};
docs_cb({row, Row}, {DbName, States, HealthThreshold} = Acc) ->
    Id = couch_util:get_value(id, Row),
    Doc = couch_util:get_value(doc, Row),
    ViewRow = #view_row{
        id = Id,
        key = couch_util:get_value(key, Row),
        value = couch_util:get_value(value, Row)
    },
    case rep_doc_state(DbName, Id, Doc, States, HealthThreshold) of
        skip ->
            ok;
        Other ->
            ok = rexi:stream2(ViewRow#view_row{doc = Other})
    end,
    {ok, Acc};
docs_cb(complete, Acc) ->
    ok = rexi:stream_last(complete),
    {ok, Acc}.


set_io_priority(DbName, Options) ->
    case lists:keyfind(io_priority, 1, Options) of
    {io_priority, Pri} ->
        erlang:put(io_priority, Pri);
    false ->
        erlang:put(io_priority, {interactive, DbName})
    end.


%% Get the state of the replication document. If it is found and has a terminal
%% state then it can be filtered and either included in the results or skipped.
%% If it is not in a terminal state, look it up in the local doc processor ETS
%% table. If it is there then filter by state. If it is not found there either
%% then mark it as `undecided` and let the coordinator try to fetch it. The
%% The idea is to do as much work as possible locally and leave the minimum
%% amount of work for the coordinator.
rep_doc_state(_Shard, <<"_design/", _/binary>>, _, _, _) ->
    skip;
rep_doc_state(Shard, Id, {[_ | _]} = Doc, States, HealthThreshold) ->
    DbName = mem3:dbname(Shard),
    DocInfo = couch_replicator:info_from_doc(DbName, Doc),
    case get_doc_state(DocInfo) of
        null ->
            % Fetch from local doc processor. If there, filter by state.
            % If not there, mark as undecided. Let coordinator figure it out.
            case couch_replicator_doc_processor:doc_lookup(Shard, Id,
                    HealthThreshold) of
                {ok, EtsInfo} ->
                    State = get_doc_state(EtsInfo),
                    couch_replicator_utils:filter_state(State, States, EtsInfo);
                {error, not_found} ->
                    undecided
            end;
        OtherState when is_atom(OtherState) ->
            couch_replicator_utils:filter_state(OtherState, States, DocInfo)
    end.


get_doc_state({Props})->
    couch_util:get_value(state, Props).
