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

-module(mem3_shard_split_debug).


-include_lib("couch/include/couch_db.hrl").
-include_lib("mem3/src/mem3_shard_split.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl"). % for all_docs function


-export([
    start_test_job/0,
    start_test_job/1,
    create_test_db/1,
    add_test_docs/4,
    all_docs/0,
    all_docs/1,
    trace_enable/0,
    trace_disable/0
]).


trace_enable() ->
    dbg:stop_clear(),
    dbg:tracer(),
    %trace_funs(mem3_shard_split, [start_job, init, spawn_job, report, checkpoint]),
    trace_funs(mem3_shard_split_index, [design_docs, target_indices, indices]),
    %%trace_funs(mem3_shard_split_job, [init, terminate, next_state, retry_state]),
    %% dbg:tpl(mem3_rep, find_missing_revs, [{
    %%     ['$1'],
    %%     [{is_tuple,'$1'}, {'=:=',{element, 2,'$1'},2000}],
    %%     [{return_trace}]
    %% }]),
    %dbg:tpl(mem3_rep, filter_doc, [{['$1','$2'],[{'=/=','$1',undefined}],[ok]}]),
    dbg:p(all,c).


trace_funs(Mod, Funs) ->
    [dbg:tpl(Mod, F, x) || F <- Funs].


trace_disable() ->
    dbg:stop_clear().


start_test_job() ->
    start_test_job(<<"db1">>).


start_test_job(DbName) ->
    mem3_shard_split:reset_state(),
    io:format("~nCreating db: ~p~n", [DbName]),
    create_test_db(DbName),
    add_test_docs(DbName, [0, 10], [0, 1], [0, 1]),
    timer:sleep(2100), % to ensure mem3 replicator did some work
    [#shard{name=ShardName}| _] = mem3:local_shards(<<"db1">>),
    io:format("Starting job for ~p~n", [ShardName]),
    state_intercept_init(),
    state_intercept(topoff1, fun(Job) ->
        JobStr = mem3_shard_split_job:jobfmt(Job),
        io:format("Intercept in topoff1 ~s. Addding docs~n", [JobStr]),
        %io:fread("Press key when ready to continue...", ""),
        add_test_docs(DbName, [200, 201], [10, 11], [10, 11]),
        timer:sleep(1000),
        ok
    end),
    state_intercept(topoff2, fun(Job) ->
        JobStr = mem3_shard_split_job:jobfmt(Job),
        io:format("Intercept in topoff2 ~s. Addding docs~n", [JobStr]),
        add_test_docs(DbName, [300, 301], [12, 13], [12, 13]),
        timer:sleep(1000),
        ok
    end),
    %trace_enable(),
    %io:fread("Press key when ready to start job...", ""),
    Res = mem3_shard_split:start_job(ShardName),
    io:format("Job started:~p. Waiting for update_shardmap1 state ~n", [Res]),
    state_wait(completed, 180000),
    io:format("Job completed, adding extra documents ~n", []),
    %io:fread("Press key when ready to add extra docs...~n", []),
    timer:sleep(1000),
    add_test_docs(DbName, [400, 401], [14, 15], [16, 17]),
    io:format("Done~n", []),
    ok.


all_docs() ->
    all_docs(<<"db1">>).

all_docs(DbName) ->
    Cb = fun(M, A) -> io:format(" all_docs cb: ~p~n~n", [M]), {ok, A} end,
    fabric:all_docs(DbName, Cb, ok, #mrargs{}).


state_intercept(State, Fun) ->
    ets:insert(?MODULE, {State, Fun}).

state_intercept_init() ->
    meck:unload(),
    catch ets:delete(?MODULE),
    ets:new(?MODULE, [named_table, public, set]),
    meck:new(mem3_shard_split, [passthrough]),
    GL = erlang:group_leader(),
    meck:expect(mem3_shard_split, checkpoint,
        fun(Server, #job{} = Job) ->
            case ets:lookup(?MODULE, Job#job.split_state) of
                [{_, Fun}] ->
                    {_, Ref} = spawn_monitor(fun() ->
                        erlang:group_leader(GL, self()),
                        Fun(Job)
                    end),
                    receive
                       {'DOWN', Ref, _, _, normal} -> ok;
                       {'DOWN', Ref, _, _, Err} -> erlang:error({error, Err})
                    after 60000 ->
                        erlang:demonitor(Ref, [flush]),
                        erlang:error({error, timeout})
                    end;
                [] -> ok
            end,
            meck:passthrough([Server, Job])
        end
    ).


state_wait(State, Timeout) ->
    Args = ['_', #job{split_state = State, _ = '_'}],
    ok = meck:wait(mem3_shard_split, checkpoint, Args, Timeout).


create_test_db(DbName) ->
    catch fabric:delete_db(DbName),
    fabric:create_db(DbName, [{q,1}, {n,3}]).


add_test_docs(DbName, DocRange, DDocRange, LDocRange) ->
    Docs = docs(DocRange) ++ ddocs(DDocRange) ++ ldocs(LDocRange),
    case fabric:update_docs(DbName, Docs, [{w, "3"}, ?ADMIN_CTX]) of
        {accepted, _} -> ok;
        {ok, _} -> ok
    end.


docs([S, E]) when E > S ->
    [doc(<<"">>, I) || I <- lists:seq(S, E)];
docs(_) -> [].



ddocs([S, E]) when E > S ->
    Body = ddprop(mrview) ++ ddprop(geo) ++ ddprop(dreyfus),
    [doc(<<"_design/">>, I, Body, 0) || I <- lists:seq(S, E)];
ddocs(_) -> [].


ldocs([S, E]) when E > S ->
    [doc(<<"_local/">>, I, bodyprops(), 0) || I <- lists:seq(S, E)];
ldocs(_) -> [].



doc(Pref, Id) ->
    Body = bodyprops(),
    doc(Pref, Id, Body, 42).


doc(Pref, Id, BodyProps, AttSize) ->
    IdBin = iolist_to_binary(io_lib:format("~5..0B", [Id])),
    #doc{
        id = <<Pref/binary, IdBin/binary>>,
        body = {BodyProps},
        atts = atts(AttSize)
    }.


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

ddprop(dreyfus) ->
    [
        {<<"indexes">>, {[
            {<<"types">>, {{
                {<<"index">>, <<"function(d){if(d.g){st_index(d.g.type)}}">>}
            }}}
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
        {data, Data} %fun(_Bytes) ->  << <<"x">> || _ <- lists:seq(1, Size) >> end}
    ])].
