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

-module(mem3_reshard_changes_feed_test).


-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("mem3/src/mem3_reshard.hrl").

-define(ID, <<"_id">>).


setup() ->
    Db1 = ?tempdb(),
    create_db(Db1, [{q, 1}, {n, 1}]),
    #{db1 => Db1}.


teardown(#{} = Dbs) ->
    mem3_reshard:reset_state(),
    maps:map(fun(_, Db) -> delete_db(Db) end, Dbs).


start_couch() ->
    test_util:start_couch(?CONFIG_CHAIN, [mem3, fabric]).


stop_couch(Ctx) ->
    test_util:stop_couch(Ctx).


mem3_reshard_changes_feed_test_() ->
    {
        "mem3 shard split changes feed tests",
        {
            setup,
            fun start_couch/0, fun stop_couch/1,
            {
                foreach,
                fun setup/0, fun teardown/1,
                [
                    fun normal_feed/1
                ]
            }
        }
    }.


normal_feed(#{db1 := Db}) ->
    ?_test(begin
        DocSpec = #{
            docs => [1, 10]
        },
        add_test_docs(Db, DocSpec),

        %% get me the original changes
        Args0 = #changes_args{feed = "normal", dir = fwd, since = 0},
        {ok, _, EndSeq0} = get_changes_feed(Db, Args0),

        %% confirm we respect update since
        Args = Args0#changes_args{since = EndSeq0},

        Args1 = #changes_args{since = EndSeq0},
        {ok, Acc0, EndSeq1} = get_changes_feed(Db, Args1),
        ?assertEqual([], Acc0),
        ?assertEqual(EndSeq0, EndSeq1),

        % Split the one shard
        [#shard{name = Shard}] = lists:sort(mem3:local_shards(Db)),
        {ok, JobId} = mem3_reshard:start_split_job(Shard),
        wait_state(JobId, completed),

        % Perform some basic checks that the shard was split
        ResultShards = lists:sort(mem3:local_shards(Db)),
        ?assertEqual(2, length(ResultShards)),
        [#shard{range = R1}, #shard{range = R2}] = ResultShards,
        ?assertEqual([16#00000000, 16#7fffffff], R1),
        ?assertEqual([16#80000000, 16#ffffffff], R2),

        %% for refs
        % -record(changes_args, {
        %     feed = "normal",
        %     dir = fwd,
        %     since = 0,
        %     limit = 1000000000000000,
        %     style = main_only,
        %     heartbeat,
        %     timeout,
        %     filter = "",
        %     filter_fun,
        %     filter_args = [],
        %     include_docs = false,
        %     doc_options = [],
        %     conflicts = false,
        %     db_open_options = []
        % }).

        %% try to pull changes since after reshard
        {ok, Acc, EndSeq} = get_changes_feed(Db, Args),
        %% fails
        %% ?assertEqual([], Acc),

        %% debug from here
        lists:foreach(fun(#{id := ID, seq := Seq}) ->
            U = fabric_view_changes:unpack_seqs(Seq, Db),
            ?debugFmt("~s => ~p", [ID, U])
        end, Acc),

        {ok, UpdateSeq} = get_update_seq(Db),
        ?debugVal(EndSeq),
        ?debugVal(UpdateSeq),

        Unpacked1 = fabric_view_changes:unpack_seqs(EndSeq, Db),
        ?debugVal(Unpacked1)
    end).


wait_state(JobId, State) ->
    test_util:wait(fun() ->
        case mem3_reshard:job(JobId) of
            {ok, {Props}} ->
                case couch_util:get_value(job_state, Props) of
                    State -> ok;
                    _ -> timer:sleep(100), wait
                end;
            {error, not_found} -> timer:sleep(100), wait
        end
    end, 30000).


get_changes_feed(Db, Args) ->
    with_proc(fun() ->
        fabric:changes(Db, fun changes_callback/2, [], Args)
    end).

changes_callback(start, Acc) ->
    {ok, Acc};
changes_callback({change, {Change}}, Acc) ->
    CM = maps:from_list(Change),
    {ok, [CM | Acc]};
changes_callback({stop, EndSeq, _Pending = 0}, Acc) ->
    {ok, lists:reverse(Acc), EndSeq}.


get_update_seq(DbName) ->
    with_proc(fun() ->
        {ok, Info} = fabric:get_db_info(DbName),
        UpdateSeq = couch_util:get_value(update_seq, Info),
        {ok, UpdateSeq}
    end).


create_db(DbName, Opts) ->
    GL = erlang:group_leader(),
    with_proc(fun() -> fabric:create_db(DbName, Opts) end, GL).


delete_db(DbName) ->
    GL = erlang:group_leader(),
    with_proc(fun() -> fabric:delete_db(DbName, [?ADMIN_CTX]) end, GL).


with_proc(Fun) ->
    with_proc(Fun, undefined, 30000).


with_proc(Fun, GroupLeader) ->
    with_proc(Fun, GroupLeader, 30000).


with_proc(Fun, GroupLeader, Timeout) ->
    {Pid, Ref} = spawn_monitor(fun() ->
        case GroupLeader of
            undefined -> ok;
            _ -> erlang:group_leader(GroupLeader, self())
        end,
        exit({with_proc_res, Fun()})
    end),
    receive
        {'DOWN', Ref, process, Pid, {with_proc_res, Res}} ->
            Res;
        {'DOWN', Ref, process, Pid, Error} ->
            error(Error)
    after Timeout ->
        erlang:demonitor(Ref, [flush]),
        exit(Pid, kill),
        error({with_proc_timeout, Fun, Timeout})
   end.


add_test_docs(DbName, #{} = DocSpec) ->
    Docs = docs(maps:get(docs, DocSpec, []))
        ++ ddocs(mrview, maps:get(mrview, DocSpec, []))
        ++ ddocs(search, maps:get(search, DocSpec, []))
        ++ ddocs(geo, maps:get(geo, DocSpec, []))
        ++ ldocs(maps:get(local, DocSpec, [])),
    Res = update_docs(DbName, Docs),
    Docs1 = lists:map(fun({Doc, {ok, {RevPos, Rev}}}) ->
        Doc#doc{revs = {RevPos, [Rev]}}
    end, lists:zip(Docs, Res)),
    case delete_docs(maps:get(delete, DocSpec, []), Docs1) of
        [] -> ok;
        [_ | _] = Deleted -> update_docs(DbName, Deleted)
    end,
    ok.


update_docs(DbName, Docs) ->
    with_proc(fun() ->
        case fabric:update_docs(DbName, Docs, [?ADMIN_CTX]) of
            {accepted, Res} -> Res;
            {ok, Res} -> Res
        end
    end).


delete_docs([S, E], Docs) when E >= S ->
    ToDelete = [doc_id(<<"">>, I) || I <- lists:seq(S, E)],
    lists:filtermap(fun(#doc{id = Id} = Doc) ->
        case lists:member(Id, ToDelete) of
            true -> {true, Doc#doc{deleted = true}};
            false -> false
        end
    end, Docs);
delete_docs(_, _) ->
    [].


docs([S, E]) when E >= S ->
    [doc(<<"">>, I) || I <- lists:seq(S, E)];
docs(_) ->
    [].


ddocs(Type, [S, E]) when E >= S ->
    Body = ddprop(Type),
    BType = atom_to_binary(Type, utf8),
    [doc(<<"_design/", BType/binary>>, I, Body, 0) || I <- lists:seq(S, E)];
ddocs(_, _) ->
    [].


ldocs([S, E]) when E >= S ->
    [doc(<<"_local/">>, I, bodyprops(), 0) || I <- lists:seq(S, E)];
ldocs(_) ->
    [].



doc(Pref, Id) ->
    Body = bodyprops(),
    doc(Pref, Id, Body, 42).


doc(Pref, Id, BodyProps, AttSize) ->
    #doc{
        id = doc_id(Pref, Id),
        body = {BodyProps},
        atts = atts(AttSize)
    }.


doc_id(Pref, Id) ->
    IdBin = iolist_to_binary(io_lib:format("~5..0B", [Id])),
    <<Pref/binary, IdBin/binary>>.


ddprop(mrview) ->
    [
        {<<"views">>, {[
             {<<"v1">>, {[
                 {<<"map">>, <<"function(d){emit(d);}">>}
             ]}}
        ]}}
    ];

ddprop(geo) ->
    [
        {<<"st_indexes">>, {[
            {<<"area">>, {[
                {<<"analyzer">>, <<"standard">>},
                {<<"index">>, <<"function(d){if(d.g){st_index(d.g)}}">> }
            ]}}
        ]}}
    ];

ddprop(search) ->
    [
        {<<"indexes">>, {[
            {<<"types">>, {[
                {<<"index">>, <<"function(d){if(d.g){st_index(d.g.type)}}">>}
            ]}}
        ]}}
   ].


bodyprops() ->
    [
        {<<"g">>, {[
            {<<"type">>, <<"Polygon">>},
            {<<"coordinates">>, [[[-71.0, 48.4], [-70.0, 48.4], [-71.0, 48.4]]]}
        ]}}
   ].


atts(0) ->
    [];

atts(Size) when is_integer(Size), Size >= 1 ->
    Data = << <<"x">> || _ <- lists:seq(1, Size) >>,
    [couch_att:new([
        {name, <<"att">>},
        {type, <<"app/binary">>},
        {att_len, Size},
        {data, Data}
    ])].
