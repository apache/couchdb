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

-module(couch_prometheus_http).

-compile(tuple_calls).

-export([
    start_link/0,
    handle_request/1
]).

-include("couch_prometheus.hrl").
-include_lib("couch/include/couch_db.hrl").

start_link() ->
    IP =
        case config:get("prometheus", "bind_address", "any") of
            "any" -> any;
            Else -> Else
        end,
    Port = config:get("prometheus", "port"),
    ok = couch_httpd:validate_bind_address(IP),

    Options = [
        {name, ?MODULE},
        {loop, fun ?MODULE:handle_request/1},
        {ip, IP},
        {port, Port}
    ],
    case mochiweb_http:start(Options) of
        {ok, Pid} ->
            {ok, Pid};
        {error, Reason} ->
            io:format("Failure to start Mochiweb: ~s~n", [Reason]),
            {error, Reason}
    end.

handle_request(MochiReq) ->
    RawUri = MochiReq:get(raw_path),
    {"/" ++ Path, _, _} = mochiweb_util:urlsplit_path(RawUri),
    PathParts = string:tokens(Path, "/"),
    try
        case PathParts of
            ["_node", Node, "_prometheus"] ->
                send_prometheus(MochiReq, Node);
            _ ->
                send_error(MochiReq, 404, <<"not_found">>, <<>>)
        end
    catch
        T:R ->
            Body = list_to_binary(io_lib:format("~p:~p", [T, R])),
            send_error(MochiReq, 500, <<"server_error">>, Body)
    end.

send_prometheus(MochiReq, Node) ->
    Type = "text/plain; version=" ++ ?PROMETHEUS_VERSION,
    Headers =
        couch_httpd:server_header() ++
            [
                {<<"Content-Type">>, ?l2b(Type)}
            ],
    Body = call_node(Node, couch_prometheus_server, scrape, []),
    send_resp(MochiReq, 200, Headers, Body).

send_resp(MochiReq, Status, ExtraHeaders, Body) ->
    Headers = couch_httpd:server_header() ++ ExtraHeaders,
    MochiReq:respond({Status, Headers, Body}).

send_error(MochiReq, Code, Error, Reason) ->
    Headers =
        couch_httpd:server_header() ++
            [
                {<<"Content-Type">>, <<"application/json">>}
            ],
    JsonError =
        {[
            {<<"error">>, Error},
            {<<"reason">>, Reason}
        ]},
    Body = ?JSON_ENCODE(JsonError),
    MochiReq:respond({Code, Headers, Body}).

call_node("_local", Mod, Fun, Args) ->
    call_node(node(), Mod, Fun, Args);
call_node(Node0, Mod, Fun, Args) when is_list(Node0) ->
    Node1 =
        try
            list_to_existing_atom(Node0)
        catch
            error:badarg ->
                NoNode = list_to_binary(Node0),
                throw({not_found, <<"no such node: ", NoNode/binary>>})
        end,
    call_node(Node1, Mod, Fun, Args);
call_node(Node, Mod, Fun, Args) when is_atom(Node) ->
    case rpc:call(Node, Mod, Fun, Args) of
        {badrpc, nodedown} ->
            Reason = list_to_binary(io_lib:format("~s is down", [Node])),
            throw({error, {nodedown, Reason}});
        Else ->
            Else
    end.
