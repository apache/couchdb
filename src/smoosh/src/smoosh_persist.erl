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

-module(smoosh_persist).

-export([
    unpersist/1,
    persist/3,
    check_setup/0
]).

-include_lib("kernel/include/file.hrl").
-include_lib("couch/include/couch_db.hrl").

-define(SUFFIX, ".smooshq").

% Public API

unpersist(Name) ->
    Enabled = config:get_boolean("smoosh", "persist", false),
    Capacity = smoosh_utils:capacity(Name),
    unpersist(Enabled, Name, Capacity).

persist(Waiting, #{} = Active, #{} = Starting) ->
    Enabled = config:get_boolean("smoosh", "persist", false),
    persist(Enabled, Waiting, Active, Starting).

% Validate peristence setup for read/write/delete access. Emit warnings if
% there are any failures. Call this function once during startup. During
% runtime errors are ignored. Smoosh persistence is opportunistic, so if we
% cannot read or write we just move on.
%
check_setup() ->
    Enabled = config:get_boolean("smoosh", "persist", false),
    try
        check_setup(Enabled)
    catch
        throw:{fail, Msg, Error} ->
            LogMsg = "~s : " ++ Msg ++ " failed in directory ~p : ~p",
            Args = [?MODULE, state_dir(), Error],
            couch_log:warning(LogMsg, Args),
            {error, {Msg, Error}}
    end.

% Private functions

unpersist(false, Name, _Capacity) ->
    smoosh_priority_queue:new(Name);
unpersist(true, Name, Capacity) ->
    Path = file_path(Name),
    case read(Path) of
        {ok, Map} -> smoosh_priority_queue:from_map(Name, Capacity, Map);
        {error, _} -> smoosh_priority_queue:new(Name)
    end.

persist(false, _Waiting, _Active, _Starting) ->
    ok;
persist(true, Waiting, Active, Starting) ->
    Name = smoosh_priority_queue:name(Waiting),
    WMap = smoosh_priority_queue:to_map(Waiting),
    % Starting and active jobs are at priority level `infinity` as they are
    % already running. We want them to be the first ones to continue after
    % restart. We're relying on infinity sorting higher than float and integer
    % numeric values here.
    AMap = maps:map(fun(_, _) -> infinity end, Active),
    SMap = maps:from_list([{K, infinity} || K <- maps:values(Starting)]),
    Path = file_path(Name),
    write(maps:merge(WMap, maps:merge(AMap, SMap)), Path).

check_setup(false) ->
    disabled;
check_setup(true) ->
    StateDir = state_dir(),
    Path = filename:join(StateDir, "smooshq.test"),
    Data = #{<<"test">> => 1},
    case file:read_file_info(StateDir) of
        {ok, #file_info{access = A}} when A == read; A == read_write ->
            ok;
        {ok, #file_info{access = Invalid}} ->
            throw({fail, "read access", Invalid});
        {error, Error1} ->
            throw({fail, "read", Error1})
    end,
    case write(Data, Path) of
        ok -> ok;
        {error, Error2} -> throw({fail, "write", Error2})
    end,
    file:delete(Path, [raw]).

write(#{} = QData, Path) when is_list(Path), map_size(QData) == 0 ->
    % Save a few bytes by deleting the persisted queue data if
    % there are no waiting/starting or active jobs
    file:delete(Path, [raw]);
write(#{} = QData, Path) when is_list(Path) ->
    Bin = term_to_binary(QData, [compressed, {minor_version, 2}]),
    TmpPath = tmp_path(Path),
    case file:write_file(TmpPath, Bin, [raw]) of
        ok -> file:rename(TmpPath, Path);
        {error, _} = Error -> Error
    end.

read(Path) ->
    case file:read_file(Path) of
        {ok, Bin} ->
            try binary_to_term(Bin, [safe]) of
                #{} = QData -> {ok, QData};
                _ -> {error, term_not_a_map}
            catch
                _:_ ->
                    {error, invalid_term}
            end;
        {error, _} = Error ->
            Error
    end.

tmp_path(Path) ->
    Time = abs(erlang:system_time()),
    Path ++ "." ++ integer_to_list(Time) ++ ".tmp".

file_path(Name) ->
    StateDir = filename:absname(state_dir()),
    filename:join(StateDir, Name ++ ?SUFFIX).

state_dir() ->
    Dir = config:get("smoosh", "state_dir", "."),
    filename:absname(Dir).

-ifdef(TEST).

-include_lib("couch/include/couch_eunit.hrl").

persist_unpersist_test_() ->
    {
        foreach,
        fun() ->
            meck:expect(config, get, fun(_, _, Default) -> Default end)
        end,
        fun(_) ->
            meck:unload()
        end,
        [
            ?TDEF_FE(t_write_read_delete),
            ?TDEF_FE(t_fail_write_read_delete),
            ?TDEF_FE(t_corrupted_read),
            ?TDEF_FE(t_check_setup),
            ?TDEF_FE(t_persist_unpersist_disabled),
            ?TDEF_FE(t_persist_unpersist_enabled),
            ?TDEF_FE(t_persist_unpersist_errors)
        ]
    }.

t_write_read_delete(_) ->
    Path = file_path("foochan"),
    Data = #{<<"a">> => 1},

    ?assertEqual(ok, write(Data, Path)),
    ?assertMatch({ok, _}, file:read_file_info(Path)),

    ReadRes = read(Path),
    ?assertMatch({ok, #{}}, ReadRes),
    {ok, ReadData} = ReadRes,
    ?assertEqual(Data, ReadData),

    ?assertEqual(ok, write(#{}, Path)),
    ?assertEqual({error, enoent}, file:read_file_info(Path)).

t_fail_write_read_delete(_) ->
    meck:expect(config, get, fun("smoosh", "state_dir", _) -> "./x" end),
    Path = file_path("foochan"),
    ?assertEqual({error, enoent}, write(#{<<"a">> => 1}, Path)),
    ?assertEqual({error, enoent}, read(Path)),
    ?assertEqual({error, enoent}, write(#{}, Path)).

t_corrupted_read(_) ->
    Path = file_path("foochan"),
    ?assertEqual(ok, write(#{<<"a">> => 1}, Path)),

    ok = file:write_file(Path, ?term_to_bin(foo), [raw]),
    ?assertEqual({error, term_not_a_map}, read(Path)),

    ok = file:write_file(Path, <<"42">>, [raw]),
    ?assertEqual({error, invalid_term}, read(Path)),

    ?assertEqual(ok, write(#{}, Path)),
    ?assertEqual({error, enoent}, file:read_file_info(Path)).

t_check_setup(_) ->
    ?assertEqual(disabled, check_setup()),

    meck:expect(config, get_boolean, fun("smoosh", "persist", _) -> true end),
    ?assertEqual(ok, check_setup()),

    TDir = ?tempfile(),
    meck:expect(config, get, fun("smoosh", "state_dir", _) -> TDir end),
    ?assertEqual({error, {"read", enoent}}, check_setup()),

    Dir = state_dir(),
    ok = file:make_dir(Dir),
    % Can't write, only read
    ok = file:change_mode(Dir, 8#500),
    ?assertEqual({error, {"write", eacces}}, check_setup()),
    % Can't read, only write
    ok = file:change_mode(Dir, 8#300),
    ?assertEqual({error, {"read access", write}}, check_setup()),
    ok = file:del_dir_r(Dir).

t_persist_unpersist_disabled(_) ->
    Name = "chan1",
    Q = smoosh_priority_queue:new(Name),
    [K1, K2, K3] = [<<"x">>, {<<"x">>, <<"_design/y">>}, {index_cleanup, <<"z">>}],
    Active = #{K1 => self()},
    Starting = #{make_ref() => K2},
    Q1 = smoosh_priority_queue:in(K3, 1.0, 9999, Q),

    ?assertEqual(ok, persist(Q1, Active, Starting)),

    Q2 = unpersist(Name),
    ?assertEqual(Name, smoosh_priority_queue:name(Q2)),
    ?assertEqual(#{max => 0, min => 0, size => 0}, smoosh_priority_queue:info(Q2)).

t_persist_unpersist_enabled(_) ->
    Name = "chan2",
    Q = smoosh_priority_queue:new(Name),
    Keys = [K1, K2, K3] = [<<"x">>, {<<"x">>, <<"y">>}, {index_cleanup, <<"z">>}],
    Active = #{K1 => self()},
    Starting = #{make_ref() => K2},
    Q1 = smoosh_priority_queue:in(K3, 1.0, 9999, Q),

    meck:expect(config, get_boolean, fun("smoosh", "persist", _) -> true end),
    ?assertEqual(ok, persist(Q1, Active, Starting)),

    Q2 = unpersist(Name),
    ?assertEqual(Name, smoosh_priority_queue:name(Q2)),
    Info2 = smoosh_priority_queue:info(Q2),
    ?assertEqual(#{max => infinity, min => 1.0, size => 3}, Info2),
    ?assertEqual(Keys, drain_q(Q2)),

    % Try to persist the already unpersisted queue
    ?assertEqual(ok, persist(Q2, #{}, #{})),
    Q3 = unpersist(Name),
    ?assertEqual(Name, smoosh_priority_queue:name(Q3)),
    Info3 = smoosh_priority_queue:info(Q2),
    ?assertEqual(#{max => infinity, min => 1.0, size => 3}, Info3),
    ?assertEqual(Keys, drain_q(Q3)).

t_persist_unpersist_errors(_) ->
    Name = "chan3",
    Q = smoosh_priority_queue:new(Name),
    Q1 = smoosh_priority_queue:in(<<"x">>, 1.0, 9999, Q),

    meck:expect(config, get_boolean, fun("smoosh", "persist", _) -> true end),

    TDir = ?tempfile(),
    meck:expect(config, get, fun
        ("smoosh", "state_dir", _) -> TDir;
        (_, _, Default) -> Default
    end),

    Q2 = unpersist(Name),
    ?assertEqual(Name, smoosh_priority_queue:name(Q2)),
    ?assertEqual(#{max => 0, min => 0, size => 0}, smoosh_priority_queue:info(Q2)),

    Dir = state_dir(),
    ok = file:make_dir(Dir),

    % Can't write, only read
    ok = file:change_mode(Dir, 8#500),
    ?assertEqual({error, eacces}, persist(Q1, #{}, #{})),

    Q3 = unpersist(Name),
    ?assertEqual(Name, smoosh_priority_queue:name(Q3)),
    ?assertEqual(#{max => 0, min => 0, size => 0}, smoosh_priority_queue:info(Q3)),

    ok = file:del_dir_r(Dir).

drain_q(Q) ->
    lists:reverse(drain_q(Q, [])).

drain_q(Q, Acc) ->
    case smoosh_priority_queue:out(Q) of
        false -> Acc;
        {Key, Q1} -> drain_q(Q1, [Key | Acc])
    end.

-endif.
