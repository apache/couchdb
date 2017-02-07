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
    {ok, spawn_link(fun() ->
        put(last_seq, StartSeq),
        put(retries_left, Db#httpdb.retries),
        ?MODULE:read_changes(Parent, StartSeq, Db#httpdb{retries = 0},
            ChangesQueue, Options, 1)
    end)};
start_link(StartSeq, Db, ChangesQueue, Options) ->
    Parent = self(),
    {ok, spawn_link(fun() ->
        ?MODULE:read_changes(Parent, StartSeq, Db, ChangesQueue, Options, 1)
    end)}.

read_changes(Parent, StartSeq, Db, ChangesQueue, Options, Ts) ->
    Continuous = couch_util:get_value(continuous, Options),
    try
        couch_replicator_api_wrap:changes_since(Db, all_docs, StartSeq,
            fun(Item) ->
                process_change(Item, {Parent, Db, ChangesQueue, Continuous, Ts})
            end, Options),
        couch_work_queue:close(ChangesQueue)
    catch
        throw:recurse ->
            LS = get(last_seq),
            read_changes(Parent, LS, Db, ChangesQueue, Options, Ts+1);
        throw:retry_no_limit ->
            LS = get(last_seq),
            read_changes(Parent, LS, Db, ChangesQueue, Options, Ts);
        throw:{retry_limit, Error} ->
        couch_stats:increment_counter(
            [couch_replicator, changes_read_failures]
        ),
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


process_change(#doc_info{id = <<>>} = DocInfo, {_, Db, _, _, _}) ->
    % Previous CouchDB releases had a bug which allowed a doc with an empty ID
    % to be inserted into databases. Such doc is impossible to GET.
    couch_log:error("Replicator: ignoring document with empty ID in "
        "source database `~s` (_changes sequence ~p)",
        [couch_replicator_api_wrap:db_uri(Db), DocInfo#doc_info.high_seq]);

process_change(#doc_info{id = Id} = DocInfo, {Parent, Db, ChangesQueue, _, _}) ->
    case is_doc_id_too_long(byte_size(Id)) of
        true ->
            ShortId = lists:sublist(binary_to_list(Id), 64),
            SourceDb = couch_replicator_api_wrap:db_uri(Db),
            couch_log:error("Replicator: document id `~s...` from source db "
                " `~s` is too long, ignoring.", [ShortId, SourceDb]),
            Stats = couch_replicator_stats:new([{doc_write_failures, 1}]),
            ok = gen_server:call(Parent, {add_stats, Stats}, infinity);
        false ->
            ok = couch_work_queue:queue(ChangesQueue, DocInfo),
            put(last_seq, DocInfo#doc_info.high_seq)
    end;

process_change({last_seq, LS}, {Parent, _, _, true = _Continuous, Ts}) ->
    % LS should never be undefined, but it doesn't hurt to be defensive inside
    % the replicator.
    Seq = case LS of undefined -> get(last_seq); _ -> LS end,
    OldSeq = get(last_seq),
    if Seq == OldSeq -> ok; true ->
        Msg = {report_seq_done, {Ts, Seq}, couch_replicator_stats:new()},
        ok = gen_server:call(Parent, Msg, infinity)
    end,
    put(last_seq, Seq),
    throw(recurse);

process_change({last_seq, _}, _) ->
    % This clause is unreachable today, but let's plan ahead for the future
    % where we checkpoint against last_seq instead of the sequence of the last
    % change.  The two can differ substantially in the case of a restrictive
    % filter.
    ok.

is_doc_id_too_long(IdLength) ->
    ConfigMax = config:get_integer("replicator", "max_document_id_length", 0),
    ConfigMax > 0 andalso IdLength > ConfigMax.
