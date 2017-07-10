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

error_bind_address() ->
    throw({error, "Cluster setup requires bind_addres != 127.0.0.1"}).

require_bind_address("127.0.0.1", undefined) ->
    error_bind_address();
require_bind_address("127.0.0.1", <<"127.0.0.1">>) ->
    error_bind_address();
require_bind_address(_, _) ->
    ok.

is_cluster_enabled() ->
    % bind_address != 127.0.0.1 AND admins != empty
    BindAddress = config:get("chttpd", "bind_address"),
    Admins = config:get("admins"),
    case {BindAddress, Admins} of
        {"127.0.0.1", _} -> no;
        {_,[]} -> no;
        {_,_} -> ok
    end.

is_single_node_enabled(Dbs) ->
    % admins != empty AND dbs exist
    Admins = config:get("admins"),
    HasDbs = has_cluster_system_dbs(Dbs),
    case {Admins, HasDbs} of
        {[], _} -> no;
        {_, no} -> no;
        {_,_} -> ok
    end.

cluster_system_dbs() ->
    ["_users", "_replicator", "_global_changes"].


has_cluster_system_dbs([]) ->
    ok;
has_cluster_system_dbs([Db|Dbs]) ->
    case catch fabric:get_db_info(Db) of
        {ok, _} -> has_cluster_system_dbs(Dbs);
        _ -> no
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
            couch_log:notice("send_req: ~p~n", [Else]),
            {error, Else}
    end.

enable_cluster_int(_Options, ok) ->
    {error, cluster_enabled};
enable_cluster_int(Options, no) ->

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
    ok = require_bind_address(CurrentBindAddress, NewBindAddress),

    NodeCount = couch_util:get_value(node_count, Options),
    ok = require_node_count(NodeCount),

    Port = proplists:get_value(port, Options),

    setup_node(NewCredentials, NewBindAddress, NodeCount, Port),
    couch_log:notice("Enable Cluster: ~p~n", [Options]).

setup_node(NewCredentials, NewBindAddress, NodeCount, Port) ->
    case NewCredentials of
        {undefined, undefined} ->
            ok;
        {Username, Password} ->
            config:set("admins", binary_to_list(Username), binary_to_list(Password))
    end,

    case NewBindAddress of
        undefined ->
            config:set("chttpd", "bind_address", "0.0.0.0");
        NewBindAddress ->
            config:set("chttpd", "bind_address", binary_to_list(NewBindAddress))
    end,

    config:set_integer("cluster", "n", NodeCount),

    case Port of
        undefined ->
            ok;
        Port when is_binary(Port) ->
            config:set("chttpd", "port", binary_to_list(Port));
        Port when is_integer(Port) ->
            config:set_integer("chttpd", "port", Port)
    end.


finish_cluster(Options) ->
    Dbs = proplists:get_value(ensure_dbs_exist, Options),
    case Dbs of
        undefined ->
            finish_cluster_int(cluster_system_dbs(), has_cluster_system_dbs(cluster_system_dbs()));
        Dbs ->
            finish_cluster_int(Dbs, has_cluster_system_dbs(Dbs))
    end.

finish_cluster_int(_Dbs, ok) ->
    {error, cluster_finished};
finish_cluster_int(Dbs, no) ->
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
    Dbs = proplists:get_value(ensure_dbs_exist, Options),
    case Dbs of
        undefined ->
            finish_cluster_int(cluster_system_dbs(), has_cluster_system_dbs(cluster_system_dbs()));
        Dbs ->
            finish_cluster_int(Dbs, has_cluster_system_dbs(Dbs))
    end,
    couch_log:notice("Enable Single Node: ~p~n", [Options]).


add_node(Options) ->
    add_node_int(Options, is_cluster_enabled()).

add_node_int(_Options, no) ->
    {error, cluster_not_enabled};
add_node_int(Options, ok) ->
    couch_log:notice("add node_int: ~p~n", [Options]),
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
            couch_log:notice("send_req: ~p~n", [Else]),
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
