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

-module(fabric_bench).

-export([
    opts/0,
    go/0,
    go/1,
    doc/2,
    body/1,
    delete_old_dbs/0
]).

-define(VERSION, "1").
-define(PREFIX, "fabricbenchdb-").
-define(MAX_DB_AGE_USEC, (60 * 60 * 8 * 1000000)).

opts() ->
    #{
        q => default,
        n => default,
        % Doc size type: small, medium or large
        doc_size => medium,
        % How many total docs to insert using _bulk_docs. These are the
        % docs used for reads and streaming bechmarks
        docs => 100000,
        % Batch size used for _bulk_docs insertion
        batch_size => 1000,
        % How many individual doc updates to do
        individual_docs => 1000
    }.

go() ->
    go(#{}).

go(#{} = Opts) ->
    #{q := Q, n := N} = maps:merge(opts(), Opts),
    QN = [{q, Q}, {n, N}],
    DbOpts = [{K, V} || {K, V} <- QN, V =/= default],
    ok = delete_old_dbs(),
    Db = db_name(),
    ok = fabric:create_db(Db, DbOpts),
    Shards = disable_compaction(Db),
    try
        go(Db, Opts)
    after
        ok = fabric:delete_db(Db),
        clear_compaction_settings(Shards)
    end.

go(Db, #{} = Opts0) when is_binary(Db) ->
    Opts = maps:merge(opts(), Opts0),
    #{
        doc_size := DocSize,
        docs := BulkDocs,
        batch_size := BatchSize,
        individual_docs := IndividualDocs
    } = Opts,

    log_opts(Opts),
    log_environment_info(Db),

    io:format("~n *** Inserting ~B docs~n", [BulkDocs]),
    {T1, {Ok, Accepted}} = run(fun() -> bulk_docs(Db, DocSize, BatchSize, BulkDocs, {0, 0}) end),
    log("Add ~p docs, ok:~p/accepted:~p", [BulkDocs, Ok, Accepted], T1, BulkDocs),

    % Avoid a lagging internal replicator skewing test results, so wait
    % for the backlog to clear.
    wait_for_internal_replicator(),

    {ok, N} = fabric:get_doc_count(Db),
    case N =/= BulkDocs of
        true -> throw({unexpected_doc_count, N, BulkDocs});
        false -> ok
    end,

    {T2, _} = run(fun() ->
        rand:seed(default, 0),
        get_docs(Db, N, N)
    end),
    log("Get random doc ~pX", [N], T2, N),

    {T3, N} = run(fun() -> get_all_docs(Db, []) end),
    log("All docs", [], T3, N),

    {T4, N} = run(fun() -> get_all_docs(Db, [{include_docs, true}]) end),
    log("All docs w/ include_docs", [], T4, N),

    {T5, N} = run(fun() -> get_changes(Db, []) end),
    log("Changes", [], T5, N),

    {T6, _} = run(fun() -> put_docs(Db, DocSize, IndividualDocs) end),
    log("Single doc updates ~pX", [IndividualDocs], T6, IndividualDocs),

    % Rough mixed ops/second read/write rate for the doc counts total docs.
    % Exclude the final put_docs as that adds more documents
    TSec = round((T1 + T2 + T3 + T5 + T6) / 1000000),
    io:format(" * Time to run all benchmarks            (sec): ~7B~n", [TSec]),
    ok.

old_enough_db(Db) when is_binary(Db) ->
    case string:prefix(Db, ?PREFIX) of
        nomatch ->
            false;
        Ts when is_binary(Ts) ->
            NowUSec = os:system_time(microsecond),
            try binary_to_integer(Ts) of
                AgeUSec when (NowUSec - AgeUSec) > ?MAX_DB_AGE_USEC ->
                    true;
                _ ->
                    false
            catch
                _:_ ->
                    false
            end
    end.

delete_old_dbs() ->
    {ok, Dbs} = fabric:all_dbs(),
    [ok = fabric:delete_db(Db) || Db <- Dbs, old_enough_db(Db)],
    ok.

db_name() ->
    Suffix = integer_to_binary(os:system_time(microsecond)),
    <<?PREFIX, Suffix/binary>>.

bulk_docs(_, _, _, BulkDocs, Acc = {_, _}) when BulkDocs =< 0 ->
    Acc;
bulk_docs(Db, DocSize, BatchSize, BulkDocs, {Ok, Accepted}) ->
    DocCount = min(BatchSize, BulkDocs),
    Docs = [doc(BulkDocs - I, DocSize) || I <- lists:seq(0, DocCount - 1)],
    Acc1 =
        case fabric:update_docs(Db, Docs, []) of
            {ok, [_ | _]} -> {Ok + 1, Accepted};
            {accepted, [_ | _]} -> {Ok, Accepted + 1};
            Error -> throw({unexpected_bulk_get_error, Error})
        end,
    bulk_docs(Db, DocSize, BatchSize, BulkDocs - BatchSize, Acc1).

put_docs(_, _, 0) ->
    ok;
put_docs(Db, DocSize, DocId) ->
    {ok, _} = fabric:update_doc(Db, doc(random, DocSize), []),
    put_docs(Db, DocSize, DocId - 1).

get_docs(_, _, 0) ->
    ok;
get_docs(Db, DocCount, N) ->
    ok = get_doc(Db, rand:uniform(DocCount)),
    get_docs(Db, DocCount, N - 1).

get_doc(Db, DocId) ->
    DocIdBin = integer_to_binary(DocId),
    case fabric:open_doc(Db, DocIdBin, []) of
        {ok, Doc} ->
            _ = couch_doc:to_json_obj(Doc, []),
            ok;
        {not_found, missing} ->
            not_found
    end.

get_db_info(Db) ->
    {ok, Info} = fabric:get_db_info(Db),
    {CLInfo} = proplists:get_value(cluster, Info),
    Q = proplists:get_value(q, CLInfo),
    N = proplists:get_value(n, CLInfo),
    [{q, Q}, {n, N}].

get_all_docs(Db, Opts) ->
    {ok, Acc} = fabric:all_docs(Db, [], fun all_docs_cb/2, 0, Opts),
    Acc.

all_docs_cb({row, Row}, Acc) ->
    _ = jiffy:encode({Row}),
    {ok, Acc + 1};
all_docs_cb({meta, _}, Acc) ->
    {ok, Acc};
all_docs_cb(complete, Acc) ->
    {ok, Acc}.

get_changes(Db, Opts) ->
    {ok, Acc} = fabric:changes(Db, fun changes_cb/2, 0, Opts),
    Acc.

changes_cb(start, Acc) ->
    {ok, Acc};
changes_cb({change, Row}, Acc) ->
    _ = jiffy:encode(Row),
    {ok, Acc + 1};
changes_cb(timeout, Acc) ->
    {ok, Acc};
changes_cb({stop, _, _}, Acc) ->
    {ok, Acc}.

run(Fun) ->
    T0 = ts(),
    Res = fabric_util:isolate(Fun),
    {ts() - T0, Res}.

log_environment_info(Db) ->
    [{q, Q}, {n, N}] = get_db_info(Db),
    Nodes = length(all_nodes()),
    {OsType, OsDetail} = os:type(),
    CouchVersion = couch_server:get_version(),
    GitSha = couch_server:get_git_sha(),
    VmInfo = erlang:system_info(system_version),
    io:format(
        "~n *** Environment~n"
        " * Nodes        : ~p~n"
        " * Bench ver.   : ~s~n"
        " * N            : ~p~n"
        " * Q            : ~p~n"
        " * OS           : ~p/~p~n"
        " * Couch ver.   : ~s~n"
        " * Couch git sha: ~s~n"
        " * VM details   : ~s",
        [Nodes, ?VERSION, N, Q, OsType, OsDetail, CouchVersion, GitSha, VmInfo]
    ).

log_opts(Opts) ->
    io:format(" *** Parameters~n", []),
    KVs = lists:sort(maps:to_list(Opts)),
    [io:format(" * ~-16s : ~p~n", [K, V]) || {K, V} <- KVs],
    ok.

log(FStr, FArgs, T, N) ->
    Str = lists:flatten(io_lib:format(FStr, FArgs)),
    io:format(" * ~-38s (Hz): ~7B~n", [Str, hz(T, N)]).

ts() ->
    erlang:monotonic_time(microsecond).

hz(Dt, Count) ->
    % We are doing very rough numbers so emphasize it
    % by rounding a few positions based on the total
    % doc count number.
    Hz = round(1000000 * Count / Dt),
    Round =
        if
            Hz > 100000 -> 10000;
            Hz > 10000 -> 1000;
            Hz > 1000 -> 100;
            Hz > 100 -> 10;
            true -> 1
        end,
    round(Hz / Round) * Round.

wait_for_internal_replicator() ->
    case nodes() of
        [] ->
            ok;
        [_ | _] ->
            timer:sleep(4000),
            Backlog = mem3_backlog(),
            case Backlog > 0 of
                true ->
                    io:format("    ---  mem3_sync backlog: ~p~n", [Backlog]),
                    wait_for_internal_replicator();
                false ->
                    timer:sleep(4000)
            end
    end.

% This setting is not persisted so if node crashes it will be reset
disable_compaction(Db) ->
    Shards = mem3:shards(Db),
    lists:foreach(
        fun(S) ->
            Name = binary_to_list(mem3:name(S)),
            Node = mem3:node(S),
            Args = ["smoosh.ignore", Name, "true", _Persist = false],
            ok = erpc:call(Node, config, set, Args, 15000)
        end,
        Shards
    ),
    Shards.

clear_compaction_settings([_ | _] = Shards) ->
    lists:foreach(
        fun(S) ->
            Name = binary_to_list(mem3:name(S)),
            Node = mem3:node(S),
            Args = ["smoosh.ignore", Name, _Persist = false],
            ok = erpc:call(Node, config, delete, Args, 15000)
        end,
        Shards
    ),
    ok.

all_nodes() ->
    [node() | nodes()].

mem3_backlog() ->
    Resps = erpc:multicall(all_nodes(), mem3_sync, get_backlog, []),
    lists:foldl(fun({ok, Bl}, Acc) -> max(Bl, Acc) end, 0, Resps).

doc(random, DocSize) ->
    doc(rand:uniform(1 bsl 128), DocSize);
doc(Id, DocSize) when is_integer(Id), is_atom(DocSize) ->
    {[{<<"_id">>, integer_to_binary(Id)}] ++ body(DocSize)}.

hexbin(Size) ->
    binary:encode_hex(crypto:strong_rand_bytes(round(Size / 2))).

body(small) ->
    [
        {<<"random_val">>, rand:uniform(1 bsl 50)},
        {<<"worker_id">>, 72},
        {<<"foo">>, <<"1209809812904880912">>},
        {<<"bar">>, <<"asdfasdf">>},
        {<<"baz">>, <<"eeefffwww">>},
        {<<"blah">>, <<"lkqjwelkrjlqwejkrklqwjeklrjkl lkjasdflk jaslkdfj ">>},
        {<<"num1">>, <<"123555123">>},
        {<<"num2">>, <<"90812091289054">>},
        {<<"biz">>, <<",zmxncv lkjaf qwerlkj">>},
        {<<"zab">>, <<"zooooooob">>},
        {<<"mtime">>, <<"1453234712345">>},
        {<<"ctime">>, <<"1453234712345">>},
        {<<"bool">>, false}
    ];
body(medium) ->
    % From a random json generator with a few tweaks
    [
        {<<"random_val">>, rand:uniform(1 bsl 50)},
        {<<"index">>, 0},
        {<<"blob1">>, hexbin(1024)},
        {<<"guid">>, <<"f9b39716-285a-4e9c-8574-790f42b9631e">>},
        {<<"isActive">>, true},
        {<<"name">>, <<"Abc Def Xyz">>},
        {<<"company">>, <<"FOOCORPINCLLC">>},
        {<<"about">>,
            <<"Ad aute anim eiusmod consequat ullamco excepteur cupidatat. Sunt consectetur tempor culpa incididunt voluptate enim dolore ex ullamco occaecat irure consectetur anim. Incididunt sint do non exercitation culpa cupidatat.\r\n">>},
        {<<"extra">>,
            <<"Sit proident labore aliquip do duis irure eu esse quis dolore non qui anim minim. Commodo et pariatur Lorem commodo ea consequat. Excepteur tempor commodo voluptate sunt anim id est occaecat nostrud culpa magna dolor aliqua incididunt. Qui nisi occaecat qui velit minim do occaecat.\r\nExercitation amet ut ut et et elit consequat ex ea eiusmod incididunt. Incididunt laborum magna sit qui ex qui ullamco fugiat reprehenderit qui. Consequat nulla sit duis minim esse velit sint et officia officia. Cillum dolore occaecat tempor dolore reprehenderit id culpa consequat sint. Est quis velit pariatur Lorem Lorem. Eiusmod veniam ex reprehenderit nulla non veniam.\r\n">>},
        {<<"registered">>, <<"2016-06-05T12:41:48 +04:00">>},
        {<<"latitude">>, 16.091562},
        {<<"longitude">>, -83.309904},
        {<<"tags">>, [
            <<"esse irure eiusmod ad reprehenderit commodo Lorem fugiat nulla esse velit pariatur dolore et exercitation">>,
            <<"consequat elit laboris labore laboris Lorem non enim non cillum eiusmod quis nisi culpa proident">>,
            <<"esse cillum ex in incididunt adipisicing qui esse eiusmod consectetur tempor labore consequat excepteur incididunt">>,
            <<"occaecat occaecat non reprehenderit reprehenderit nulla mollit et minim velit fugiat elit occaecat cillum ad">>,
            <<"fugiat aliqua esse non pariatur reprehenderit ea culpa non ex culpa et exercitation commodo aute">>,
            <<"elit magna voluptate fugiat voluptate aliquip officia dolore non laboris velit amet excepteur tempor veniam">>,
            <<"officia mollit nulla cupidatat occaecat Lorem mollit magna consectetur dolor qui eiusmod commodo eiusmod sit">>,
            <<"aliquip magna dolore commodo qui amet ipsum cupidatat cillum eu veniam voluptate ipsum sint reprehenderit">>,
            <<"ea elit id labore mollit magna non commodo magna culpa amet id amet duis do">>,
            <<"proident deserunt id fugiat sunt ipsum sit aute aute eu ex consectetur proident consequat ea">>,
            <<"est do consequat aute reprehenderit ea et do magna adipisicing tempor laboris duis aliquip aute">>,
            <<"cupidatat ad occaecat et do Lorem sint duis dolore irure magna quis excepteur ex tempor">>,
            <<"et nisi pariatur deserunt quis Lorem laborum dolore magna qui ex quis ea ea anim">>,
            <<"do laboris magna aliqua est laborum reprehenderit ut eiusmod do qui irure Lorem dolore dolor">>,
            <<"quis in commodo ex pariatur adipisicing eu ad nulla exercitation irure elit nisi excepteur id">>
        ]},
        {<<"array_ints">>, [I || I <- lists:seq(1, 1000)]},
        {<<"array_floats">>, [float(I) || I <- lists:seq(1, 1000)]},
        {<<"blob2">>, hexbin(1024)}
    ];
body(large) ->
    % These are mostly incompressible, should be about ~128KB or so
    body(medium) ++ [{integer_to_binary(Field), hexbin(4096)} || Field <- lists:seq(1, 32)].
