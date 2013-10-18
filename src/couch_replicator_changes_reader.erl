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

-module(couch_replicator_changes_reader).

% Public API
-export([start_link/4]).

% Exported for code reloading
-export([read_changes/6]).

-include_lib("couch/include/couch_db.hrl").
-include("couch_replicator_api_wrap.hrl").
-include("couch_replicator.hrl").

-import(couch_util, [
    get_value/2
]).

start_link(StartSeq, #httpdb{} = Db, ChangesQueue, Options) ->
    Parent = self(),
    spawn_link(fun() ->
        put(last_seq, StartSeq),
        put(retries_left, Db#httpdb.retries),
        ?MODULE:read_changes(Parent, StartSeq, Db#httpdb{retries = 0}, ChangesQueue, Options, 1)
    end);
start_link(StartSeq, Db, ChangesQueue, Options) ->
    Parent = self(),
    spawn_link(fun() ->
        ?MODULE:read_changes(Parent, StartSeq, Db, ChangesQueue, Options, 1)
    end).

read_changes(Parent, StartSeq, Db, ChangesQueue, Options, Ts) ->
    try
        couch_replicator_api_wrap:changes_since(Db, all_docs, StartSeq,
            fun(#doc_info{high_seq = Seq, id = Id} = DocInfo) ->
                case Id of
                <<>> ->
                    % Previous CouchDB releases had a bug which allowed a doc
                    % with an empty ID to be inserted into databases. Such doc
                    % is impossible to GET.
                    couch_log:error("Replicator: ignoring document with empty ID in "
                        "source database `~s` (_changes sequence ~p)",
                        [couch_replicator_api_wrap:db_uri(Db), Seq]);
                _ ->
                    ok = couch_work_queue:queue(ChangesQueue, DocInfo)
                end,
                put(last_seq, Seq);
            ({last_seq, LS}) ->
                case get_value(continuous, Options) of
                true ->
                    % LS should never be undefined, but it doesn't hurt to be
                    % defensive inside the replicator.
                    Seq = case LS of undefined -> get(last_seq); _ -> LS end,
                    OldSeq = get(last_seq),
                    if Seq == OldSeq -> ok; true ->
                        Msg = {report_seq_done, {Ts, Seq}, #rep_stats{}},
                        ok = gen_server:call(Parent, Msg, infinity)
                    end,
                    put(last_seq, Seq),
                    throw(recurse);
                _ ->
                    % This clause is unreachable today, but let's plan ahead
                    % for the future where we checkpoint against last_seq
                    % instead of the sequence of the last change.  The two can
                    % differ substantially in the case of a restrictive filter.
                    ok
                end
            end, Options),
        couch_work_queue:close(ChangesQueue)
    catch
        throw:recurse ->
            LS = get(last_seq),
            read_changes(Parent, LS, Db, ChangesQueue, Options, Ts+1);
        exit:{http_request_failed, _, _, _} = Error ->
        case get(retries_left) of
        N when N > 0 ->
            put(retries_left, N - 1),
            LastSeq = get(last_seq),
            Db2 = case LastSeq of
            StartSeq ->
                couch_log:notice("Retrying _changes request to source database ~s"
                    " with since=~p in ~p seconds",
                    [couch_replicator_api_wrap:db_uri(Db), LastSeq, Db#httpdb.wait / 1000]),
                ok = timer:sleep(Db#httpdb.wait),
                Db#httpdb{wait = 2 * Db#httpdb.wait};
            _ ->
                couch_log:notice("Retrying _changes request to source database ~s"
                    " with since=~p", [couch_replicator_api_wrap:db_uri(Db), LastSeq]),
                Db
            end,
            read_changes(Parent, LastSeq, Db2, ChangesQueue, Options, Ts);
        _ ->
            exit(Error)
        end
    end.
