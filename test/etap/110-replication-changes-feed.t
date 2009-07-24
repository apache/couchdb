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

main(_) ->
    code:add_pathz("src/couchdb"),
    code:add_pathz("src/ibrowse"),
    code:add_pathz("src/mochiweb"),
    
    etap:plan(17),
    case (catch test()) of
        ok ->
            etap:end_tests();
        Other ->
            etap:diag(io_lib:format("Test died abnormally: ~p", [Other])),
            etap:bail(Other)
    end,
    ok.

test() ->
    couch_server:start(
        ["etc/couchdb/default_dev.ini", "etc/couchdb/local_dev.ini"]
    ),
    ibrowse:start(),
    crypto:start(),

    couch_server:delete(<<"etap-test-db">>, []),
    {ok, Db1} = couch_db:create(<<"etap-test-db">>, []),
    test_all(local),
    couch_db:close(Db1),
    couch_server:delete(<<"etap-test-db">>, []),

    couch_server:delete(<<"etap-test-db">>, []),
    {ok, Db2} = couch_db:create(<<"etap-test-db">>, []),
    test_all(remote),
    test_remote_only(),
    couch_db:close(Db2),
    couch_server:delete(<<"etap-test-db">>, []),

    ok.

test_all(Type) ->
    test_unchanged_db(Type),
    test_simple_change(Type),
    test_since_parameter(Type),
    test_continuous_parameter(Type),
    test_conflicts(Type),
    test_deleted_conflicts(Type),
    test_non_blocking_call(Type).

test_remote_only() ->
    test_chunk_reassembly(remote).

test_unchanged_db(Type) ->
    {ok, Pid} = couch_rep_changes_feed:start({Type, get_dbname(Type)}, []),
    etap:is(
        couch_rep_changes_feed:next(Pid),
        complete,
        io_lib:format(
            "(~p) changes feed for unchanged DB is automatically complete",
            [Type])
    ).

test_simple_change(Type) ->
    Expect = generate_change(),
    {ok, Pid} = couch_rep_changes_feed:start({Type, get_dbname(Type)}, []),
    etap:is(
        {couch_rep_changes_feed:next(Pid), couch_rep_changes_feed:next(Pid)},
        {Expect, complete},
        io_lib:format("(~p) change one document, get one row", [Type])
    ).

test_since_parameter(Type) ->
    {ok, Pid} = couch_rep_changes_feed:start({Type, get_dbname(Type)}, 
        [{since, get_update_seq()}]),
    etap:is(
        couch_rep_changes_feed:next(Pid),
        complete,
        io_lib:format(
            "(~p) since query-string parameter allows us to skip changes",
            [Type])
    ).

test_continuous_parameter(Type) ->
    {ok, Pid} = couch_rep_changes_feed:start({Type, get_dbname(Type)},
        [{since, get_update_seq()}, {continuous, true}]),

    % make the changes_feed request before the next update
    Self = self(),
    spawn(fun() -> 
        Change = couch_rep_changes_feed:next(Pid), 
        Self ! {actual, Change}
    end),

    Expect = generate_change(),
    etap:is(
        receive {actual, Actual} -> Actual end,
        Expect,
        io_lib:format(
            "(~p) continuous query-string parameter picks up new changes",
            [Type])
    ),

    ok = couch_rep_changes_feed:stop(Pid).

test_conflicts(Type) ->
    Since = get_update_seq(),
    Expect = generate_conflict(),
    {ok, Pid} = couch_rep_changes_feed:start({Type, get_dbname(Type)}, 
        [{since, Since}]),
    etap:is(
        {couch_rep_changes_feed:next(Pid), couch_rep_changes_feed:next(Pid)},
        {Expect, complete},
        io_lib:format("(~p) conflict revisions show up in feed", [Type])
    ).

test_deleted_conflicts(Type) ->
    Since = get_update_seq(),
    {ExpectProps} = generate_conflict(),

    %% delete the conflict revision
    Id = proplists:get_value(<<"id">>, ExpectProps),
    [Win, {[{<<"rev">>, Lose}]}] = proplists:get_value(<<"changes">>, ExpectProps),
    Doc = couch_doc:from_json_obj({[
        {<<"_id">>, Id},
        {<<"_rev">>, Lose},
        {<<"_deleted">>, true}
    ]}),
    Db = get_db(),
    {ok, Rev} = couch_db:update_doc(Db, Doc, [full_commit]),
    couch_db:close(Db),

    Expect = {[
        {<<"seq">>, get_update_seq()},
        {<<"id">>, Id},
        {<<"changes">>, [Win, {[{<<"rev">>, couch_doc:rev_to_str(Rev)}]}]}
    ]},
    
    {ok, Pid} = couch_rep_changes_feed:start({Type, get_dbname(Type)}, 
        [{since, Since}]),
    etap:is(
        {couch_rep_changes_feed:next(Pid), couch_rep_changes_feed:next(Pid)},
        {Expect, complete},
        io_lib:format("(~p) deleted conflict revisions show up in feed", [Type])
    ).

test_non_blocking_call(Type) ->
    Since = get_update_seq(),
    {ok, Pid} = couch_rep_changes_feed:start({Type, get_dbname(Type)},
        [{since, Since}, {continuous, true}]),
    etap:is(
        couch_rep_changes_feed:all(Pid),
        [],
        io_lib:format("(~p) all() returns empty list if no changes available",
            [Type])
    ),
    Expect1 = generate_change(),
    Expect2 = generate_change(),
    timer:sleep(100),
    etap:is(
        couch_rep_changes_feed:all(Pid),
        [Expect1, Expect2],
        io_lib:format("(~p) all() returns full list of outstanding changes",
            [Type])
    ),
    ok = couch_rep_changes_feed:stop(Pid).

test_chunk_reassembly(Type) ->
    Since = get_update_seq(),
    Expect = [generate_change() || _I <- lists:seq(1,30)],
    {ok, Pid} = couch_rep_changes_feed:start({Type, get_dbname(Type)},
        [{since, Since}]),
    timer:sleep(100),
    etap:is(
        couch_rep_changes_feed:all(Pid),
        Expect,
        io_lib:format("(~p) reassembles chunks split across TCP frames",
            [Type])
    ).

generate_change() ->
    generate_change(couch_util:new_uuid()).

generate_change(Id) ->
    generate_change(Id, {[]}).

generate_change(Id, EJson) ->
    Doc = couch_doc:from_json_obj(EJson),
    Db = get_db(),
    {ok, Rev} = couch_db:update_doc(Db, Doc#doc{id = Id}, [full_commit]),
    couch_db:close(Db),
    {[
        {<<"seq">>, get_update_seq()},
        {<<"id">>, Id},
        {<<"changes">>, [{[{<<"rev">>, couch_doc:rev_to_str(Rev)}]}]}
    ]}.

generate_conflict() ->
    Id = couch_util:new_uuid(),
    Db = get_db(),
    Doc1 = (couch_doc:from_json_obj({[<<"foo">>, <<"bar">>]}))#doc{id = Id},
    Doc2 = (couch_doc:from_json_obj({[<<"foo">>, <<"baz">>]}))#doc{id = Id},
    {ok, Rev1} = couch_db:update_doc(Db, Doc1, [full_commit]),
    {ok, Rev2} = couch_db:update_doc(Db, Doc2, [full_commit, all_or_nothing]),
    
    %% relies on undocumented CouchDB conflict winner algo and revision sorting!
    RevList = [{[{<<"rev">>, couch_doc:rev_to_str(R)}]} || R
        <- lists:sort(fun(A,B) -> B<A end, [Rev1,Rev2])],
    {[
        {<<"seq">>, get_update_seq()},
        {<<"id">>, Id},
        {<<"changes">>, RevList}
    ]}.
    
get_db() ->
    {ok, Db} = couch_db:open(<<"etap-test-db">>, []),
    Db.

get_dbname(local) ->
    "etap-test-db";
get_dbname(remote) ->
    "http://127.0.0.1:5984/etap-test-db".

get_update_seq() ->
    Db = get_db(),
    Seq = couch_db:get_update_seq(Db),
    couch_db:close(Db),
    Seq.
