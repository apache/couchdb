#!/usr/bin/env escript
%% -*- erlang -*-

% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License.  You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
% License for the specific language governing permissions and limitations under
% the License.

%% XXX: Figure out how to -include("couch_db.hrl")

-record(doc, {id= <<"">>, revs={0, []}, body={[]},
            attachments=[], deleted=false, meta=[]}).

-record(http_db, {
    url,
    auth = [],
    resource = "",
    headers = [
        {"User-Agent", "CouchDb/"++couch_server:get_version()},
        {"Accept", "application/json"},
        {"Accept-Encoding", "gzip"}
    ],
    qs = [],
    method = get,
    body = nil,
    options = [
        {response_format,binary},
        {inactivity_timeout, 30000}
    ],
    retries = 10,
    pause = 1,
    conn = nil
}).

config_files() ->
    lists:map(fun test_util:build_file/1, [
        "etc/couchdb/default_dev.ini",
        "etc/couchdb/local_dev.ini"
    ]).

main(_) ->
    test_util:init_code_path(),
    
    etap:plan(12),
    case (catch test()) of
        ok ->
            etap:end_tests();
        Other ->
            etap:diag(io_lib:format("Test died abnormally: ~p", [Other])),
            etap:bail(Other)
    end,
    ok.

test() ->
    couch_server:start(config_files()),
    ibrowse:start(),
    crypto:start(),

    couch_server:delete(<<"etap-test-source">>, []),
    couch_server:delete(<<"etap-test-target">>, []),

    Dbs1 = setup(),
    test_all(local, local),
    ok = teardown(Dbs1),

    Dbs2 = setup(),
    test_all(local, remote),
    ok = teardown(Dbs2),

    Dbs3 = setup(),
    test_all(remote, local),
    ok = teardown(Dbs3),

    Dbs4 = setup(),
    test_all(remote, remote),
    ok = teardown(Dbs4),

    ok.

test_all(SrcType, TgtType) ->
    test_unchanged_db(SrcType, TgtType),
    test_multiple_changes(SrcType, TgtType),
    test_changes_not_missing(SrcType, TgtType).

test_unchanged_db(SrcType, TgtType) ->
    {ok, Pid1} = start_changes_feed(SrcType, 0, false),
    {ok, Pid2} = start_missing_revs(TgtType, Pid1),
    etap:is(
        couch_rep_missing_revs:next(Pid2),
        complete,
        io_lib:format(
            "(~p, ~p) no missing revs if source is unchanged",
            [SrcType, TgtType])
    ).

test_multiple_changes(SrcType, TgtType) ->
    Expect = {2, [generate_change(), generate_change()]},
    {ok, Pid1} = start_changes_feed(SrcType, 0, false),
    {ok, Pid2} = start_missing_revs(TgtType, Pid1),
    etap:is(
        get_all_missing_revs(Pid2, {0, []}),
        Expect,
        io_lib:format("(~p, ~p) add src docs, get missing tgt revs + high seq",
            [SrcType, TgtType])
    ).

test_changes_not_missing(SrcType, TgtType) ->
    %% put identical changes on source and target
    Id = couch_util:new_uuid(),
    {Id, _Seq, [Rev]} = Expect = generate_change(Id, {[]}, get_db(source)),
    {Id, _, [Rev]} = generate_change(Id, {[]}, get_db(target)),

    %% confirm that this change is not in missing revs feed
    {ok, Pid1} = start_changes_feed(SrcType, 0, false),
    {ok, Pid2} = start_missing_revs(TgtType, Pid1),
    {HighSeq, AllRevs} = get_all_missing_revs(Pid2, {0, []}),

    %% etap:none/3 has a bug, so just define it correctly here
    etap:is(
        lists:member(Expect, AllRevs),
        false,
        io_lib:format(
            "(~p, ~p) skip revs that already exist on target",
            [SrcType, TgtType])
    ).

generate_change() ->
    generate_change(couch_util:new_uuid()).

generate_change(Id) ->
    generate_change(Id, {[]}).

generate_change(Id, EJson) ->
    generate_change(Id, EJson, get_db(source)).

generate_change(Id, EJson, Db) ->
    Doc = couch_doc:from_json_obj(EJson),
    Seq = get_update_seq(),
    {ok, Rev} = couch_db:update_doc(Db, Doc#doc{id = Id}, [full_commit]),
    couch_db:close(Db),
    {Id, Seq+1, [Rev]}.

get_all_missing_revs(Pid, {HighSeq, Revs}) ->
    case couch_rep_missing_revs:next(Pid) of
    complete ->
        {HighSeq, lists:flatten(lists:reverse(Revs))};
    {Seq, More} ->
        get_all_missing_revs(Pid, {Seq, [More|Revs]})
    end.

get_db(source) ->
    {ok, Db} = couch_db:open(<<"etap-test-source">>, []),
    Db;
get_db(target) ->
    {ok, Db} = couch_db:open(<<"etap-test-target">>, []),
    Db.

get_update_seq() ->
    Db = get_db(source),
    Seq = couch_db:get_update_seq(Db),
    couch_db:close(Db),
    Seq.

setup() ->
    {ok, DbA} = couch_db:create(<<"etap-test-source">>, []),
    {ok, DbB} = couch_db:create(<<"etap-test-target">>, []),
    [DbA, DbB].

teardown([DbA, DbB]) ->
    couch_db:close(DbA),
    couch_db:close(DbB),
    couch_server:delete(<<"etap-test-source">>, []),
    couch_server:delete(<<"etap-test-target">>, []),
    ok.

start_changes_feed(local, Since, Continuous) ->
    Props = [{<<"continuous">>, Continuous}],
    couch_rep_changes_feed:start_link(self(), get_db(source), Since, Props);
start_changes_feed(remote, Since, Continuous) ->
    Props = [{<<"continuous">>, Continuous}],
    Db = #http_db{url = "http://127.0.0.1:5984/etap-test-source/"},
    couch_rep_changes_feed:start_link(self(), Db, Since, Props).

start_missing_revs(local, Changes) ->
    couch_rep_missing_revs:start_link(self(), get_db(target), Changes, []);
start_missing_revs(remote, Changes) ->
    Db = #http_db{url = "http://127.0.0.1:5984/etap-test-target/"},
    couch_rep_missing_revs:start_link(self(), Db, Changes, []).
