% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
% http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(setup).

-export([enable_cluster/1, finish_cluster/1, add_node/1, receive_cookie/1]).
-export([is_cluster_enabled/0, has_cluster_system_dbs/1, cluster_system_dbs/0]).
-export([enable_single_node/1, is_single_node_enabled/1]).

-include_lib("../couch/include/couch_db.hrl").


require_admins(undefined, {undefined, undefined}) ->
    % no admin in CouchDB, no admin in request
    throw({error, "Cluster setup requires admin account to be configured"});
require_admins(_,_) ->
    ok.

require_node_count(undefined) ->
    throw({error, "Cluster setup requires node_count to be configured"});
require_node_count(_) ->
    ok.

error_local_bind_address() ->
    throw({error, "Cluster setup requires a remote bind_address (not 127.0.0.1 nor ::1)"}).

error_invalid_bind_address(InvalidBindAddress) ->
    throw({error, io:format("Setup requires a valid IP bind_address. " ++
                         "~p is invalid.", [InvalidBindAddress])}).

require_remote_bind_address(OldBindAddress, NewBindAddress) ->
    case {OldBindAddress, NewBindAddress} of
        {"127.0.0.1", undefined} -> error_local_bind_address();
        {_, <<"127.0.0.1">>} -> error_local_bind_address();
        {"::1", undefined} -> error_local_bind_address();
        {_, <<"::1">>} -> error_local_bind_address();
        {_, undefined} -> ok;
        {_, PresentNewBindAddress} -> require_valid_bind_address(PresentNewBindAddress)
    end.

require_valid_bind_address(BindAddress) ->
    ListBindAddress = binary_to_list(BindAddress),
    case inet_parse:address(ListBindAddress) of
        {ok, _} -> ok;
        {error, _} -> error_invalid_bind_address(ListBindAddress)
    end.

is_cluster_enabled() ->
    % bind_address != 127.0.0.1 AND admins != empty
    BindAddress = config:get("chttpd", "bind_address"),
    Admins = config:get("admins"),
    case {BindAddress, Admins} of
        {"127.0.0.1", _} -> false;
        {_,[]} -> false;
        {_,_} -> true
    end.

is_single_node_enabled(Dbs) ->
    % admins != empty AND dbs exist
    Admins = config:get("admins"),
    HasDbs = has_cluster_system_dbs(Dbs),
    case {Admins, HasDbs} of
        {[], _} -> false;
        {_, false} -> false;
        {_,_} -> true
    end.

cluster_system_dbs() ->
    ["_users", "_replicator"].


has_cluster_system_dbs([]) ->
    true;
has_cluster_system_dbs([Db|Dbs]) ->
    case catch fabric:get_db_info(Db) of
        {ok, _} -> has_cluster_system_dbs(Dbs);
        _ -> false
    end.

enable_cluster(Options) ->

    case couch_util:get_value(remote_node, Options, undefined) of
        undefined ->
            enable_cluster_int(Options, is_cluster_enabled());
        _ ->
            enable_cluster_http(Options)
    end.

get_remote_request_options(Options) ->
    case couch_util:get_value(remote_current_user, Options, undefined) of
        undefined ->
            [];
        _ ->
            [
                {basic_auth, {
                    binary_to_list(couch_util:get_value(remote_current_user, Options)),
                    binary_to_list(couch_util:get_value(remote_current_password, Options))
                }}
            ]
    end.

enable_cluster_http(Options) ->
    % POST to nodeB/_setup
    RequestOptions = get_remote_request_options(Options),
    AdminUsername = couch_util:get_value(username, Options),
    AdminPasswordHash = config:get("admins", binary_to_list(AdminUsername)),

    Body = ?JSON_ENCODE({[
        {<<"action">>, <<"enable_cluster">>},
        {<<"username">>, AdminUsername},
        {<<"password_hash">>, ?l2b(AdminPasswordHash)},
        {<<"bind_address">>, couch_util:get_value(bind_address, Options)},
        {<<"port">>, couch_util:get_value(port, Options)},
        {<<"node_count">>, couch_util:get_value(node_count, Options)}
    ]}),

    Headers = [
        {"Content-Type","application/json"}
    ],

    RemoteNode = couch_util:get_value(remote_node, Options),
    Port = get_port(couch_util:get_value(port, Options, 5984)),

    Url = binary_to_list(<<"http://", RemoteNode/binary, ":", Port/binary, "/_cluster_setup">>),

    case ibrowse:send_req(Url, Headers, post, Body, RequestOptions) of
        {ok, "201", _, _} ->
            ok;
        Else ->
            {error, Else}
    end.

enable_cluster_int(_Options, true) ->
    {error, cluster_enabled};
enable_cluster_int(Options, false) ->

    % if no admin in config and no admin in req -> error
    CurrentAdmins = config:get("admins"),
    NewCredentials = {
        proplists:get_value(username, Options),
        case proplists:get_value(password_hash, Options) of
          undefined -> proplists:get_value(password, Options);
          Pw -> Pw
        end
    },
    ok = require_admins(CurrentAdmins, NewCredentials),
    % if bind_address == 127.0.0.1 and no bind_address in req -> error
    CurrentBindAddress = config:get("chttpd","bind_address"),
    NewBindAddress = proplists:get_value(bind_address, Options),
    ok = require_remote_bind_address(CurrentBindAddress, NewBindAddress),
    NodeCount = couch_util:get_value(node_count, Options),
    ok = require_node_count(NodeCount),
    Port = proplists:get_value(port, Options),

    setup_node(NewCredentials, NewBindAddress, NodeCount, Port),
    couch_log:debug("Enable Cluster: ~p~n", [Options]).

set_admin(Username, Password) ->
    config:set("admins", binary_to_list(Username), binary_to_list(Password), #{sensitive => true}).

setup_node(NewCredentials, NewBindAddress, NodeCount, Port) ->
    case NewCredentials of
        {undefined, undefined} ->
            ok;
        {Username, Password} ->
            set_admin(Username, Password)
    end,

    ok = require_valid_bind_address(NewBindAddress),
    case NewBindAddress of
        undefined ->
            config:set("chttpd", "bind_address", "0.0.0.0");
        NewBindAddress ->
            config:set("chttpd", "bind_address", binary_to_list(NewBindAddress))
    end,

    % for single node setups, set n=1, for larger setups, don’t
    % exceed n=3 as a default
    config:set_integer("cluster", "n", min(NodeCount, 3)),

    case Port of
        undefined ->
            ok;
        Port when is_binary(Port) ->
            config:set("chttpd", "port", binary_to_list(Port));
        Port when is_integer(Port) ->
            config:set_integer("chttpd", "port", Port)
    end.


finish_cluster(Options) ->
    % ensure that uuid is set
    couch_server:get_uuid(),

    ok = wait_connected(),
    ok = sync_admins(),
    ok = sync_uuid(),
    ok = sync_auth_secret(),
    Dbs = proplists:get_value(ensure_dbs_exist, Options, cluster_system_dbs()),
    finish_cluster_int(Dbs, has_cluster_system_dbs(Dbs)).


wait_connected() ->
    Nodes = other_nodes(),
    Result = test_util:wait(fun() ->
        case disconnected(Nodes) of
            [] -> ok;
            _ -> wait
        end
    end),
    case Result of
        timeout ->
            Reason = "Cluster setup timed out waiting for nodes to connect",
            throw({setup_error, Reason});
        ok ->
            ok
    end.


other_nodes() ->
    mem3:nodes() -- [node()].


disconnected(Nodes) ->
    lists:filter(fun(Node) ->
        case net_adm:ping(Node) of
            pong -> false;
            pang -> true
        end
    end, Nodes).


sync_admins() ->
    ok = lists:foreach(fun({User, Pass}) ->
        sync_admin(User, Pass)
    end, config:get("admins")).


sync_admin(User, Pass) ->
    sync_config("admins", User, Pass).


sync_uuid() ->
    Uuid = config:get("couchdb", "uuid"),
    sync_config("couchdb", "uuid", Uuid).

sync_auth_secret() ->
    Secret = config:get("couch_httpd_auth", "secret"),
    sync_config("couch_httpd_auth", "secret", Secret).


sync_config(Section, Key, Value) ->
    {Results, Errors} = rpc:multicall(other_nodes(), config, set,
        [Section, Key, Value]),
    case validate_multicall(Results, Errors) of
        ok ->
            ok;
        error ->
            couch_log:error("~p sync_admin results ~p errors ~p",
                [?MODULE, Results, Errors]),
            Reason = "Cluster setup unable to sync admin passwords",
            throw({setup_error, Reason})
    end.


validate_multicall(Results, Errors) ->
    AllOk = lists:all(fun
        (ok) -> true;
        (_) -> false
    end, Results),
    case AllOk andalso Errors == [] of
        true ->
            ok;
        false ->
            error
    end.


finish_cluster_int(_Dbs, true) ->
    {error, cluster_finished};
finish_cluster_int(Dbs, false) ->
    lists:foreach(fun fabric:create_db/1, Dbs).


enable_single_node(Options) ->
    % if no admin in config and no admin in req -> error
    CurrentAdmins = config:get("admins"),
    NewCredentials = {
        proplists:get_value(username, Options),
        case proplists:get_value(password_hash, Options) of
          undefined -> proplists:get_value(password, Options);
          Pw -> Pw
        end
    },
    ok = require_admins(CurrentAdmins, NewCredentials),
    % skip bind_address validation, anything is fine
    NewBindAddress = proplists:get_value(bind_address, Options),
    Port = proplists:get_value(port, Options),

    setup_node(NewCredentials, NewBindAddress, 1, Port),
    Dbs = proplists:get_value(ensure_dbs_exist, Options, cluster_system_dbs()),
    finish_cluster_int(Dbs, has_cluster_system_dbs(Dbs)),
    couch_log:debug("Enable Single Node: ~p~n", [Options]).


add_node(Options) ->
    add_node_int(Options, is_cluster_enabled()).

add_node_int(_Options, false) ->
    {error, cluster_not_enabled};
add_node_int(Options, true) ->
    couch_log:debug("add node_int: ~p~n", [Options]),
    ErlangCookie = erlang:get_cookie(),

    % POST to nodeB/_setup
    RequestOptions = [
        {basic_auth, {
            binary_to_list(proplists:get_value(username, Options)),
            binary_to_list(proplists:get_value(password, Options))
        }}
    ],

    Body = ?JSON_ENCODE({[
        {<<"action">>, <<"receive_cookie">>},
        {<<"cookie">>, atom_to_binary(ErlangCookie, utf8)}
    ]}),

    Headers = [
        {"Content-Type","application/json"}
    ],

    Host = proplists:get_value(host, Options),
    Port = get_port(proplists:get_value(port, Options, 5984)),
    Name = proplists:get_value(name, Options, get_default_name(Port)),

    Url = binary_to_list(<<"http://", Host/binary, ":", Port/binary, "/_cluster_setup">>),

    case ibrowse:send_req(Url, Headers, post, Body, RequestOptions) of
        {ok, "201", _, _} ->
            % when done, PUT :5986/nodes/nodeB
            create_node_doc(Host, Name);
        Else ->
            Else
    end.

get_port(Port) when is_integer(Port) ->
    list_to_binary(integer_to_list(Port));
get_port(Port) when is_list(Port) ->
    list_to_binary(Port);
get_port(Port) when is_binary(Port) ->
    Port.

create_node_doc(Host, Name) ->
    {ok, Db} = couch_db:open_int(<<"_nodes">>, []),
    Doc = {[{<<"_id">>, <<Name/binary, "@", Host/binary>>}]},
    Options = [],
    CouchDoc = couch_doc:from_json_obj(Doc),

    couch_db:update_doc(Db, CouchDoc, Options).

get_default_name(Port) ->
    case Port of
        % shortcut for easier development
        <<"15984">> ->
            <<"node1">>;
        <<"25984">> ->
            <<"node2">>;
        <<"35984">> ->
            <<"node3">>;
        % by default, all nodes have the user `couchdb`
        _ ->
            <<"couchdb">>
    end.

receive_cookie(Options) ->
    Cookie = proplists:get_value(cookie, Options),
    erlang:set_cookie(node(), binary_to_atom(Cookie, latin1)).
