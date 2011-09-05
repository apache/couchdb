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

-module(couch_compaction_daemon).
-behaviour(gen_server).

% public API
-export([start_link/0, config_change/3]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_info/2, handle_cast/2]).
-export([code_change/3, terminate/2]).

-include("couch_db.hrl").

-define(CONFIG_ETS, couch_compaction_daemon_config).

-record(state, {
    loop_pid
}).

-record(config, {
    db_frag = nil,
    view_frag = nil,
    period = nil,
    cancel = false,
    parallel_view_compact = false
}).

-record(period, {
    from = nil,
    to = nil
}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init(_) ->
    process_flag(trap_exit, true),
    ?CONFIG_ETS = ets:new(?CONFIG_ETS, [named_table, set, protected]),
    ok = couch_config:register(fun ?MODULE:config_change/3),
    load_config(),
    Server = self(),
    Loop = spawn_link(fun() -> compact_loop(Server) end),
    {ok, #state{loop_pid = Loop}}.


config_change("compactions", DbName, NewValue) ->
    ok = gen_server:cast(?MODULE, {config_update, DbName, NewValue}).


handle_cast({config_update, DbName, deleted}, State) ->
    true = ets:delete(?CONFIG_ETS, ?l2b(DbName)),
    {noreply, State};

handle_cast({config_update, DbName, Config}, #state{loop_pid = Loop} = State) ->
    case parse_config(DbName, Config) of
    {ok, NewConfig} ->
        WasEmpty = (ets:info(?CONFIG_ETS, size) =:= 0),
        true = ets:insert(?CONFIG_ETS, {?l2b(DbName), NewConfig}),
        case WasEmpty of
        true ->
            Loop ! {self(), have_config};
        false ->
            ok
        end;
    error ->
        ok
    end,
    {noreply, State}.


handle_call(Msg, _From, State) ->
    {stop, {unexpected_call, Msg}, State}.


handle_info({'EXIT', Pid, Reason}, #state{loop_pid = Pid} = State) ->
    {stop, {compaction_loop_died, Reason}, State}.


terminate(_Reason, _State) ->
    true = ets:delete(?CONFIG_ETS).


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


compact_loop(Parent) ->
    {ok, _} = couch_server:all_databases(
        fun(DbName, Acc) ->
            case ets:info(?CONFIG_ETS, size) =:= 0 of
            true ->
                {stop, Acc};
            false ->
                case get_db_config(DbName) of
                nil ->
                    ok;
                {ok, Config} ->
                    case check_period(Config) of
                    true ->
                        maybe_compact_db(DbName, Config);
                    false ->
                        ok
                    end
                end,
                {ok, Acc}
            end
        end, ok),
    case ets:info(?CONFIG_ETS, size) =:= 0 of
    true ->
        receive {Parent, have_config} -> ok end;
    false ->
        PausePeriod = list_to_integer(
            couch_config:get("compaction_daemon", "check_interval", "300")),
        ok = timer:sleep(PausePeriod * 1000)
    end,
    compact_loop(Parent).


maybe_compact_db(DbName, Config) ->
    case (catch couch_db:open_int(DbName, [])) of
    {ok, Db} ->
        DDocNames = db_ddoc_names(Db),
        case can_db_compact(Config, Db) of
        true ->
            {ok, DbCompactPid} = couch_db:start_compact(Db),
            TimeLeft = compact_time_left(Config),
            case Config#config.parallel_view_compact of
            true ->
                ViewsCompactPid = spawn_link(fun() ->
                    maybe_compact_views(DbName, DDocNames, Config)
                end),
                ViewsMonRef = erlang:monitor(process, ViewsCompactPid);
            false ->
                ViewsCompactPid = nil,
                ViewsMonRef = nil
            end,
            DbMonRef = erlang:monitor(process, DbCompactPid),
            receive
            {'DOWN', DbMonRef, process, _, normal} ->
                couch_db:close(Db),
                case Config#config.parallel_view_compact of
                true ->
                    ok;
                false ->
                    maybe_compact_views(DbName, DDocNames, Config)
                end;
            {'DOWN', DbMonRef, process, _, Reason} ->
                couch_db:close(Db),
                ?LOG_ERROR("Compaction daemon - an error ocurred while"
                    " compacting the database `~s`: ~p", [DbName, Reason])
            after TimeLeft ->
                ?LOG_INFO("Compaction daemon - canceling compaction for database"
                    " `~s` because it's exceeding the allowed period.",
                    [DbName]),
                erlang:demonitor(DbMonRef, [flush]),
                ok = couch_db:cancel_compact(Db),
                couch_db:close(Db)
            end,
            case ViewsMonRef of
            nil ->
                ok;
            _ ->
                receive
                {'DOWN', ViewsMonRef, process, _, _Reason} ->
                    ok
                after TimeLeft + 1000 ->
                    % Under normal circunstances, the view compaction process
                    % should have finished already.
                    erlang:demonitor(ViewsMonRef, [flush]),
                    unlink(ViewsCompactPid),
                    exit(ViewsCompactPid, kill)
                end
            end;
        false ->
            couch_db:close(Db),
            maybe_compact_views(DbName, DDocNames, Config)
        end;
    _ ->
        ok
    end.


maybe_compact_views(_DbName, [], _Config) ->
    ok;
maybe_compact_views(DbName, [DDocName | Rest], Config) ->
    case check_period(Config) of
    true ->
        case maybe_compact_view(DbName, DDocName, Config) of
        ok ->
            maybe_compact_views(DbName, Rest, Config);
        timeout ->
            ok
        end;
    false ->
        ok
    end.


db_ddoc_names(Db) ->
    {ok, _, DDocNames} = couch_db:enum_docs(
        Db,
        fun(#full_doc_info{id = <<"_design/", _/binary>>, deleted = true}, _, Acc) ->
            {ok, Acc};
        (#full_doc_info{id = <<"_design/", Id/binary>>}, _, Acc) ->
            {ok, [Id | Acc]};
        (_, _, Acc) ->
            {stop, Acc}
        end, [], [{start_key, <<"_design/">>}, {end_key_gt, <<"_design0">>}]),
    DDocNames.


maybe_compact_view(DbName, GroupId, Config) ->
    DDocId = <<"_design/", GroupId/binary>>,
    case (catch couch_view:get_group_info(DbName, DDocId)) of
    {ok, GroupInfo} ->
        case can_view_compact(Config, DbName, GroupId, GroupInfo) of
        true ->
            {ok, CompactPid} = couch_view_compactor:start_compact(DbName, GroupId),
            TimeLeft = compact_time_left(Config),
            MonRef = erlang:monitor(process, CompactPid),
            receive
            {'DOWN', MonRef, process, CompactPid, normal} ->
                ok;
            {'DOWN', MonRef, process, CompactPid, Reason} ->
                ?LOG_ERROR("Compaction daemon - an error ocurred while compacting"
                    " the view group `~s` from database `~s`: ~p",
                    [GroupId, DbName, Reason]),
                ok
            after TimeLeft ->
                ?LOG_INFO("Compaction daemon - canceling the compaction for the "
                    "view group `~s` of the database `~s` because it's exceeding"
                    " the allowed period.", [GroupId, DbName]),
                erlang:demonitor(MonRef, [flush]),
                ok = couch_view_compactor:cancel_compact(DbName, GroupId),
                timeout
            end;
        false ->
            ok
        end;
    Error ->
        ?LOG_ERROR("Error opening view group `~s` from database `~s`: ~p",
            [GroupId, DbName, Error]),
        ok
    end.


compact_time_left(#config{cancel = false}) ->
    infinity;
compact_time_left(#config{period = nil}) ->
    infinity;
compact_time_left(#config{period = #period{to = {ToH, ToM} = To}}) ->
    {H, M, _} = time(),
    case To > {H, M} of
    true ->
        ((ToH - H) * 60 * 60 * 1000) + (abs(ToM - M) * 60 * 1000);
    false ->
        ((24 - H + ToH) * 60 * 60 * 1000) + (abs(ToM - M) * 60 * 1000)
    end.


get_db_config(DbName) ->
    case ets:lookup(?CONFIG_ETS, DbName) of
    [] ->
        case ets:lookup(?CONFIG_ETS, <<"_default">>) of
        [] ->
            nil;
        [{<<"_default">>, Config}] ->
            {ok, Config}
        end;
    [{DbName, Config}] ->
        {ok, Config}
    end.


can_db_compact(#config{db_frag = Threshold} = Config, Db) ->
    case check_period(Config) of
    false ->
        false;
    true ->
        {ok, DbInfo} = couch_db:get_db_info(Db),
        {Frag, SpaceRequired} = frag(DbInfo),
        ?LOG_DEBUG("Fragmentation for database `~s` is ~p%, estimated space for"
           " compaction is ~p bytes.", [Db#db.name, Frag, SpaceRequired]),
        case check_frag(Threshold, Frag) of
        false ->
            false;
        true ->
            Free = free_space(couch_config:get("couchdb", "database_dir")),
            case Free >= SpaceRequired of
            true ->
                true;
            false ->
                ?LOG_INFO("Compaction daemon - skipping database `~s` "
                    "compaction: the estimated necessary disk space is about ~p"
                    " bytes but the currently available disk space is ~p bytes.",
                   [Db#db.name, SpaceRequired, Free]),
                false
            end
        end
    end.

can_view_compact(Config, DbName, GroupId, GroupInfo) ->
    case check_period(Config) of
    false ->
        false;
    true ->
        case couch_util:get_value(updater_running, GroupInfo) of
        true ->
            false;
        false ->
            {Frag, SpaceRequired} = frag(GroupInfo),
            ?LOG_DEBUG("Fragmentation for view group `~s` (database `~s`) is "
                "~p%, estimated space for compaction is ~p bytes.",
                [GroupId, DbName, Frag, SpaceRequired]),
            case check_frag(Config#config.view_frag, Frag) of
            false ->
                false;
            true ->
                Free = free_space(couch_config:get("couchdb", "view_index_dir")),
                case Free >= SpaceRequired of
                true ->
                    true;
                false ->
                    ?LOG_INFO("Compaction daemon - skipping view group `~s` "
                        "compaction (database `~s`): the estimated necessary "
                        "disk space is about ~p bytes but the currently available"
                        " disk space is ~p bytes.",
                        [GroupId, DbName, SpaceRequired, Free]),
                    false
                end
            end
        end
    end.


check_period(#config{period = nil}) ->
    true;
check_period(#config{period = #period{from = From, to = To}}) ->
    {HH, MM, _} = erlang:time(),
    case From < To of
    true ->
        ({HH, MM} >= From) andalso ({HH, MM} < To);
    false ->
        ({HH, MM} >= From) orelse ({HH, MM} < To)
    end.


check_frag(nil, _) ->
    true;
check_frag(Threshold, Frag) ->
    Frag >= Threshold.


frag(Props) ->
    FileSize = couch_util:get_value(disk_size, Props),
    MinFileSize = list_to_integer(
        couch_config:get("compaction_daemon", "min_file_size", "131072")),
    case FileSize < MinFileSize of
    true ->
        {0, FileSize};
    false ->
        case couch_util:get_value(data_size, Props) of
        null ->
            {100, FileSize};
        0 ->
            {0, FileSize};
        DataSize ->
            Frag = round(((FileSize - DataSize) / FileSize * 100)),
            {Frag, space_required(DataSize)}
        end
    end.

% Rough, and pessimistic, estimation of necessary disk space to compact a
% database or view index.
space_required(DataSize) ->
    round(DataSize * 2.0).


load_config() ->
    lists:foreach(
        fun({DbName, ConfigString}) ->
            case parse_config(DbName, ConfigString) of
            {ok, Config} ->
                true = ets:insert(?CONFIG_ETS, {?l2b(DbName), Config});
            error ->
                ok
            end
        end,
        couch_config:get("compactions")).

parse_config(DbName, ConfigString) ->
    case (catch do_parse_config(ConfigString)) of
    {ok, Conf} ->
        {ok, Conf};
    incomplete_period ->
        ?LOG_ERROR("Incomplete period ('to' or 'from' missing) in the compaction"
            " configuration for database `~s`", [DbName]),
        error;
    _ ->
        ?LOG_ERROR("Invalid compaction configuration for database "
            "`~s`: `~s`", [DbName, ConfigString]),
        error
    end.

do_parse_config(ConfigString) ->
    {ok, ConfProps} = couch_util:parse_term(ConfigString),
    {ok, #config{period = Period} = Conf} = config_record(ConfProps, #config{}),
    case Period of
    nil ->
        {ok, Conf};
    #period{from = From, to = To} when From =/= nil, To =/= nil ->
        {ok, Conf};
    #period{} ->
        incomplete_period
    end.

config_record([], Config) ->
    {ok, Config};

config_record([{db_fragmentation, V} | Rest], Config) ->
    [Frag] = string:tokens(V, "%"),
    config_record(Rest, Config#config{db_frag = list_to_integer(Frag)});

config_record([{view_fragmentation, V} | Rest], Config) ->
    [Frag] = string:tokens(V, "%"),
    config_record(Rest, Config#config{view_frag = list_to_integer(Frag)});

config_record([{from, V} | Rest], #config{period = Period0} = Config) ->
    Time = parse_time(V),
    Period = case Period0 of
    nil ->
        #period{from = Time};
    #period{} ->
        Period0#period{from = Time}
    end,
    config_record(Rest, Config#config{period = Period});

config_record([{to, V} | Rest], #config{period = Period0} = Config) ->
    Time = parse_time(V),
    Period = case Period0 of
    nil ->
        #period{to = Time};
    #period{} ->
        Period0#period{to = Time}
    end,
    config_record(Rest, Config#config{period = Period});

config_record([{strict_window, true} | Rest], Config) ->
    config_record(Rest, Config#config{cancel = true});

config_record([{strict_window, false} | Rest], Config) ->
    config_record(Rest, Config#config{cancel = false});

config_record([{parallel_view_compaction, true} | Rest], Config) ->
    config_record(Rest, Config#config{parallel_view_compact = true});

config_record([{parallel_view_compaction, false} | Rest], Config) ->
    config_record(Rest, Config#config{parallel_view_compact = false}).


parse_time(String) ->
    [HH, MM] = string:tokens(String, ":"),
    {list_to_integer(HH), list_to_integer(MM)}.


free_space(Path) ->
    DiskData = lists:sort(
        fun({PathA, _, _}, {PathB, _, _}) ->
            length(filename:split(PathA)) > length(filename:split(PathB))
        end,
        disksup:get_disk_data()),
    free_space_rec(abs_path(Path), DiskData).

free_space_rec(_Path, []) ->
    undefined;
free_space_rec(Path, [{MountPoint0, Total, Usage} | Rest]) ->
    MountPoint = abs_path(MountPoint0),
    case MountPoint =:= string:substr(Path, 1, length(MountPoint)) of
    false ->
        free_space_rec(Path, Rest);
    true ->
        trunc(Total - (Total * (Usage / 100))) * 1024
    end.

abs_path(Path0) ->
    Path = filename:absname(Path0),
    case lists:last(Path) of
    $/ ->
        Path;
    _ ->
        Path ++ "/"
    end.
