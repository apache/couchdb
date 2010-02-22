%%%-------------------------------------------------------------------
%%% File:      bootstrap_receiver.erl
%%% @author    Brad Anderson <brad@cloudant.com>
%%% @copyright 2009 Brad Anderson
%%% @doc
%%%
%%% @end
%%%
%%% @since 2009-09-22 by Brad Anderson
%%%-------------------------------------------------------------------
-module(bootstrap_receiver).
-author('brad@cloudant.com').

-include("../include/config.hrl").
-include("../include/common.hrl").

%% API
-export([start_link/6, loop/6, fetch_shard/5]).


%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec
%% @doc
%% @end
%%--------------------------------------------------------------------
start_link(FromNode, ToNode, Partition, DbName, Timeout, Manager) ->
    Pid = proc_lib:spawn_link(ToNode, bootstrap_receiver, loop,
                              [FromNode, Partition, DbName, Timeout, Manager,
                               self()]),
    sync_wait(Pid, Timeout).


loop(FromNode, Partition, DbName, Timeout, Manager, Parent) ->
    proc_lib:init_ack(Parent, {ok, self()}),
    fetch_shard(FromNode, Partition, DbName, Timeout, Manager).


%% @doc run at "ToNode" via spawn_link
fetch_shard(FromNode, Partition, DbName, Timeout, Manager) ->
    Directory = couch_config:get("couchdb", "database_dir"),
    [_NodeName, Hostname] = string:tokens(atom_to_list(FromNode), "@"),
    SrcFile = binary_to_list(partitions:shard_name(Partition, DbName)),
    DestFile = showroom_utils:full_filename(Partition, DbName, Directory),
    Authn = fetch_authn(),
    Port = fetch_port(),
    Url = lists:concat(["http://", Authn, Hostname, Port, "/", SrcFile,
                        ".couch"]),
    Options = [{save_response_to_file, DestFile},
               {inactivity_timeout, Timeout}],
    case filelib:ensure_dir(DestFile) of
    ok -> ok;
    {error, eexist} -> ok; % duh!
    Other -> throw(Other)
    end,
    ?LOG_DEBUG("~n"
               "Directory: ~p~n"
               "Hostname : ~p~n"
               "SrcFile  : ~p~n"
               "DestFile : ~p~n"
               "Url      : ~p~n"
               "Options  : ~p~n"
               , [Directory, Hostname, SrcFile, DestFile, Url, Options]),
    case ibrowse:send_req(Url, [], get, [], Options, infinity) of
        {ok, "200", _Headers, Body} ->
            ?LOG_DEBUG("~nBootstrap ibrowse req Body: ~p~n", [Body]),
            Manager ! {receiver_done, FromNode, node(), Partition, DbName,
                       self()};
        Error ->
            ?LOG_ERROR("~nBootstrap ibrowse req Error: ~p~n", [Error]),
            throw(Error)
    end.


%%====================================================================
%% Internal functions
%%====================================================================


%% from proc_lib.erl in otp r13b01
sync_wait(Pid, Timeout) ->
    receive
	{ack, Pid, Return} ->
	    Return;
	{'EXIT', Pid, Reason} ->
	    {error, Reason}
    after Timeout ->
	    unlink(Pid),
	    exit(Pid, kill),
	    flush(Pid),
	    {error, timeout}
    end.


flush(Pid) ->
    receive
	{'EXIT', Pid, _} ->
	    true
    after 0 ->
	    true
    end.


fetch_authn() ->
    User = couch_config:get("shard_moving", "user", ""),
    Pass = couch_config:get("shard_moving", "pass", ""),
    if
    length(User) > 0 andalso length(Pass) > 0 ->
            lists:concat([User, ":", Pass, "@"]);
    true -> ""
    end.


fetch_port() ->
    Port = couch_config:get("shard_moving", "port", "8080"),
    if
    Port =:= "80" -> "";
    true -> lists:concat([":", Port])
    end.
