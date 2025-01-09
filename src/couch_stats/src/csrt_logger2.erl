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

-module(csrt_logger2).

-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("couch_stats_resource_tracker.hrl").

%% Process lifetime logging api
-export([
    get_tracker/0,
    is_logging_enabled/0,
    log_process_lifetime_report/1,
    put_tracker/1,
    stop_tracker/0,
    stop_tracker/1,
    track/1,
    tracker/1
]).

%% JSON Conversion API
-export([
    convert_type/1,
    convert_pidref/1,
    convert_pid/1,
    convert_ref/1,
    to_json/1
]).

%% Raw API that bypasses is_enabled checks
-export([
    do_lifetime_report/1,
    do_status_report/1,
    do_report/2,
    maybe_report/2
]).

track(#rctx{pid_ref=PidRef}) ->
    case get_tracker() of
        undefined ->
            Pid = spawn(?MODULE, tracker, [PidRef]),
            put_tracker(Pid),
            Pid;
        Pid when is_pid(Pid) ->
            Pid
    end.

tracker({Pid, _Ref}=PidRef) ->
    MonRef = erlang:monitor(process, Pid),
    receive
        stop ->
            %% TODO: do we need cleanup here?
            log_process_lifetime_report(PidRef),
            csrt_server:destroy_context(PidRef),
            ok;
        {'DOWN', MonRef, _Type, _0DPid, _Reason0} ->
            %% TODO: should we pass reason to log_process_lifetime_report?
            %% Reason = case Reason0 of
            %%     {shutdown, Shutdown0} ->
            %%         Shutdown = atom_to_binary(Shutdown0),
            %%         <<"shutdown: ", Shutdown/binary>>;
            %%     Reason0 ->
            %%         Reason0
            %% end,
            %% TODO: should we send the induced work delta to the coordinator?
            log_process_lifetime_report(PidRef),
            csrt_server:destroy_context(PidRef),
            ok
    end.

log_process_lifetime_report(PidRef) ->
    case csrt:is_enabled() andalso is_logging_enabled() of
        true ->
            case csrt:conf_get("logger_type", "csrt_logger") of
                "csrt_logger" ->
                    csrt_logger:maybe_report(PidRef);
                _ ->
                    Rctx = csrt_server:get_resource(PidRef),
                    case should_log(Rctx) of
                       true ->
                            do_lifetime_report(Rctx);
                        _ ->
                            ok
                    end
            end;
        false ->
            ok
    end.

is_logging_enabled() ->
    logging_enabled() =/= false.

logging_enabled() ->
    case csrt:conf_get("log_pid_usage_report", "coordinator") of
        "coordinator" ->
            coordinator;
        "true" ->
            true;
        _ ->
            false
    end.

should_log(undefined) ->
    false;
should_log(#rctx{}=Rctx) ->
    should_log(Rctx, logging_enabled()).

should_log(undefined, _) ->
    false;
should_log(#rctx{}, true) ->
    true;
should_log(#rctx{}, false) ->
    false;
should_log(#rctx{type = #coordinator{}}, coordinator) ->
    true;
should_log(#rctx{type = #rpc_worker{mod=fabric_rpc, func=FName}}, _) ->
    case csrt:conf_get("log_fabric_rpc") of
        "true" ->
            true;
        undefined ->
            false;
        Name ->
            Name =:= atom_to_list(FName)
    end;
should_log(#rctx{}, _) ->
    false.

%% TODO: decide on API
maybe_report(_ReportName, PidRef) ->
    log_process_lifetime_report(PidRef).

do_lifetime_report(Rctx) ->
    do_report("csrt-pid-usage-lifetime", Rctx).

do_status_report(Rctx) ->
    do_report("csrt-pid-usage-status", Rctx).

do_report(ReportName, #rctx{}=Rctx) ->
    couch_log:report(ReportName, to_json(Rctx)).

%%
%% Conversion API for outputting JSON
%%

convert_type(#coordinator{method=Verb0, path=Path, mod=M0, func=F0}) ->
    M = atom_to_binary(M0),
    F = atom_to_binary(F0),
    Verb = atom_to_binary(Verb0),
    <<"coordinator-{", M/binary, ":", F/binary, "}:", Verb/binary, ":", Path/binary>>;
convert_type(#rpc_worker{mod=M0, func=F0, from=From0}) ->
    M = atom_to_binary(M0),
    F = atom_to_binary(F0),
    From = convert_pidref(From0),
    <<"rpc_worker-{", From/binary, "}:", M/binary, ":", F/binary>>;
convert_type(undefined) ->
    null.

convert_pidref({Parent0, ParentRef0}) ->
    Parent = convert_pid(Parent0),
    ParentRef = convert_ref(ParentRef0),
    <<Parent/binary, ":", ParentRef/binary>>;
convert_pidref(null) ->
    null;
convert_pidref(undefined) ->
    null.

convert_pid(Pid) when is_pid(Pid) ->
    list_to_binary(pid_to_list(Pid)).

convert_ref(Ref) when is_reference(Ref) ->
    list_to_binary(ref_to_list(Ref)).

to_json(#rctx{}=Rctx) ->
    #{
        updated_at => Rctx#rctx.updated_at,
        started_at => Rctx#rctx.started_at,
        pid_ref => convert_pidref(Rctx#rctx.pid_ref),
        nonce => Rctx#rctx.nonce,
        dbname => Rctx#rctx.dbname,
        username => Rctx#rctx.username,
        db_open => Rctx#rctx.db_open,
        docs_read => Rctx#rctx.docs_read,
        js_filter => Rctx#rctx.js_filter,
        js_filtered_docs => Rctx#rctx.js_filtered_docs,
        rows_read => Rctx#rctx.rows_read,
        type => convert_type(Rctx#rctx.type),
        get_kp_node => Rctx#rctx.get_kp_node,
        get_kv_node => Rctx#rctx.get_kv_node,
        write_kp_node => Rctx#rctx.write_kp_node,
        write_kv_node => Rctx#rctx.write_kv_node,
        changes_returned => Rctx#rctx.changes_returned,
        ioq_calls => Rctx#rctx.ioq_calls
    }.

%%
%% Process lifetime logging api
%%

get_tracker() ->
    get(?TRACKER_PID).

put_tracker(Pid) when is_pid(Pid) ->
    put(?TRACKER_PID, Pid).

stop_tracker() ->
    stop_tracker(get_tracker()).

stop_tracker(undefined) ->
    ok;
stop_tracker(Pid) when is_pid(Pid) ->
    Pid ! stop.
