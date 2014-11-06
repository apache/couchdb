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

-export([enable_cluster/1, finish_cluster/0, add_node/1]).
-include_lib("../couch/include/couch_db.hrl").


require_admins(undefined, {undefined, undefined}) ->
    % no admin in CouchDB, no admin in request
    throw({error, "Cluster setup requires admin account to be configured"});
require_admins(_,_) ->
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
    BindAddress = config:get("httpd", "bind_address"),
    Admins = config:get("admins"),
    case {BindAddress, Admins} of
        {"127.0.0.1", _} -> no;
        {_,[]} -> no;
        {_,_} -> ok
    end.
        

has_cluster_system_dbs() ->
    % GET /_users /_replicator /_cassim

    Users = fabric:get_db_info("_users"),
    Replicator = fabric:get_db_info("_replicator"),
    Cassim = fabric:get_db_info("_cassim"),
    case {Users, Replicator, Cassim} of
        {{ok, _}, {ok, _}, {ok, _}} -> ok;
        _Else -> no
    end.

enable_cluster(Options) ->
    enable_cluster_int(Options, is_cluster_enabled()).

enable_cluster_int(_Options, ok) ->
    {error, cluster_enabled};
enable_cluster_int(Options, no) ->

    % if no admin in config and no admin in req -> error
    CurrentAdmins = config:get("admins"),
    NewCredentials = {
        proplists:get_value(username, Options),
        proplists:get_value(password, Options)
    },

    % if bind_address == 127.0.0.1 and no bind_address in req -> error
    CurrentBindAddress = config:get("httpd","bind_address"),
    NewBindAddress = proplists:get_value(bind_address, Options),
    ok = require_admins(CurrentAdmins, NewCredentials),
    ok = require_bind_address(CurrentBindAddress, NewBindAddress),
    
    case NewCredentials of
        {undefined, undefined} ->
            ok;
        {Username, Password} ->
            % TODO check if this gets hashed
            config:set("admins", binary_to_list(Username), binary_to_list(Password))
    end,

    case NewBindAddress of
        undefined ->
            config:set("httpd", "bind_address", "0.0.0.0");
        NewBindAddress ->
            config:set("httpd", "bind_address", binary_to_list(NewBindAddress))
    end,
    
    Port = proplists:get_value(port, Options),
    case Port of
        undefined ->
            ok;
        Port ->
            config:set("httpd", "port", integer_to_list(Port))
    end,
    io:format("~nEnable Cluster: ~p~n", [Options]).
    %cluster_state:set(enabled).

finish_cluster() ->
    finish_cluster_int(has_cluster_system_dbs()).
finish_cluster_int(no) ->
    {error, cluster_finished};
finish_cluster_int(ok) ->
    io:format("~nFinish Cluster~n").
    % create clustered databases (_users, _replicator, _cassim/_metadata
    % am I in enabled mode, are there nodes?


add_node(Options) ->
    add_node_int(Options, is_cluster_enabled()).

add_node_int(_Options, no) ->
    {error, cluster_not_enabled};
add_node_int(Options, ok) ->
    io:format("~nadd node: ~p~n", [Options]),
    ErlangCookie = erlang:get_cookie(),

    % POST to nodeB/_setup
    RequestOptions = [
        {basic_auth, {
            proplists:get_value(username, Options),
            proplists:get_value(password, Options)
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
    Port = proplists:get_value(port, Options, <<"5984">>),
    Url = binary_to_list(<<"http://", Host/binary, ":", Port/binary, "/_setup">>),

    io:format("~nUrl: ~p~n", [Url]),
    io:format("~nBody: ~p~n", [Body]),

    case ibrowse:send_req(Url, Headers, post, Body, RequestOptions) of
        {ok, 200, _, _} ->
            % when done, PUT :5986/nodes/nodeB
            create_node_doc(Host, Port);
        Else ->
            io:format("~nsend_req: ~p~n", [Else]),
            Else
    end.


create_node_doc(Host, Port) ->
    {ok, Db} = couch_db:open_int("nodes"),
    Doc = {[{<<"_id">>, <<Host/binary, ":", Port/binary>>}]},
    Options = [],
    couch_db:update_doc(Db, Doc, Options).
