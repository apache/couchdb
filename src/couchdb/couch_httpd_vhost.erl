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

-module(couch_httpd_vhost).
-behaviour(gen_server).

-export([start_link/0, config_change/2, reload/0, get_state/0, dispatch_host/1]).
-export([urlsplit_netloc/2, redirect_to_vhost/2]).


-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("couch_db.hrl").

-define(SEPARATOR, $\/).
-define(MATCH_ALL, {bind, '*'}).

-record(vhosts_state, {
        vhosts,
        vhost_globals,
        vhosts_fun}).

%% doc the vhost manager.
%% This gen_server keep state of vhosts added to the ini and try to
%% match the Host header (or forwarded) against rules built against
%% vhost list. 
%%
%% Declaration of vhosts take place in the configuration file :
%%
%% [vhosts]
%% example.com = /example
%% *.example.com = /example
%%
%% The first line will rewrite the rquest to display the content of the
%% example database. This rule works only if the Host header is
%% 'example.com' and won't work for CNAMEs. Second rule on the other hand
%% match all CNAMES to example db. So www.example.com or db.example.com
%% will work.
%%
%% The wildcard ('*') should always be the last in the cnames:
%%
%%      "*.db.example.com = /"  will match all cname on top of db
%% examples to the root of the machine.
%%
%% 
%% Rewriting Hosts to path
%% -----------------------
%%
%% Like in the _rewrite handler you could match some variable and use
%them to create the target path. Some examples:
%%
%%    [vhosts]
%%    *.example.com = /*
%%    :dbname.example.com = /:dbname
%%    :ddocname.:dbname.example.com = /:dbname/_design/:ddocname/_rewrite
%%
%% First rule pass wildcard as dbname, second do the same but use a
%% variable name and the third one allows you to use any app with
%% @ddocname in any db with @dbname .
%%
%% You could also change the default function to handle request by
%% changing the setting `redirect_vhost_handler` in `httpd` section of
%% the Ini:
%%
%%    [httpd]
%%    redirect_vhost_handler = {Module, Fun}
%%
%% The function take 2 args : the mochiweb request object and the target
%%% path. 

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc reload vhosts rules
reload() ->
    gen_server:call(?MODULE, reload).

get_state() ->
    gen_server:call(?MODULE, get_state).

%% @doc Try to find a rule matching current Host heade. some rule is
%% found it rewrite the Mochiweb Request else it return current Request.
dispatch_host(MochiReq) ->
    #vhosts_state{
        vhost_globals = VHostGlobals,
        vhosts = VHosts,
        vhosts_fun=Fun} = get_state(),

    {"/" ++ VPath, Query, Fragment} = mochiweb_util:urlsplit_path(MochiReq:get(raw_path)),
    VPathParts =  string:tokens(VPath, "/"),

    XHost = couch_config:get("httpd", "x_forwarded_host", "X-Forwarded-Host"),
    VHost = case MochiReq:get_header_value(XHost) of
        undefined ->
            case MochiReq:get_header_value("Host") of
                undefined -> [];
                Value1 -> Value1
            end;
        Value -> Value
    end,
    {VHostParts, VhostPort} = split_host_port(VHost),
    FinalMochiReq = case try_bind_vhost(VHosts, lists:reverse(VHostParts),
            VhostPort, VPathParts) of
        no_vhost_matched -> MochiReq;
        {VhostTarget, NewPath} ->
            case vhost_global(VHostGlobals, MochiReq) of
                true ->
                    MochiReq;
                _Else ->
                    NewPath1 = mochiweb_util:urlunsplit_path({NewPath, Query,
                                          Fragment}),
                    MochiReq1 = mochiweb_request:new(MochiReq:get(socket),
                                      MochiReq:get(method),
                                      NewPath1,
                                      MochiReq:get(version),
                                      MochiReq:get(headers)),
                    Fun(MochiReq1, VhostTarget)
            end
    end,
    FinalMochiReq.

append_path("/"=_Target, "/"=_Path) ->
    "/";
append_path(Target, Path) ->
    Target ++ Path.

% default redirect vhost handler 
redirect_to_vhost(MochiReq, VhostTarget) ->
    Path = MochiReq:get(raw_path),
    Target = append_path(VhostTarget, Path),

    ?LOG_DEBUG("Vhost Target: '~p'~n", [Target]),

    Headers = mochiweb_headers:enter("x-couchdb-vhost-path", Path, 
        MochiReq:get(headers)),

    % build a new mochiweb request
    MochiReq1 = mochiweb_request:new(MochiReq:get(socket),
                                      MochiReq:get(method),
                                      Target,
                                      MochiReq:get(version),
                                      Headers),
    % cleanup, It force mochiweb to reparse raw uri.
    MochiReq1:cleanup(),
    MochiReq1.

%% if so, then it will not be rewritten, but will run as a normal couchdb request.
%* normally you'd use this for _uuids _utils and a few of the others you want to 
%% keep available on vhosts. You can also use it to make databases 'global'.
vhost_global( VhostGlobals, MochiReq) ->
    RawUri = MochiReq:get(raw_path),
    {"/" ++ Path, _, _} = mochiweb_util:urlsplit_path(RawUri),

    Front = case couch_httpd:partition(Path) of
    {"", "", ""} ->
        "/"; % Special case the root url handler
    {FirstPart, _, _} ->
        FirstPart
    end,
    [true] == [true||V <- VhostGlobals, V == Front].

%% bind host
%% first it try to bind the port then the hostname.
try_bind_vhost([], _HostParts, _Port, _PathParts) ->
    no_vhost_matched;
try_bind_vhost([VhostSpec|Rest], HostParts, Port, PathParts) ->
    {{VHostParts, VPort, VPath}, Path} = VhostSpec,
    case bind_port(VPort, Port) of
        ok -> 
            case bind_vhost(lists:reverse(VHostParts), HostParts, []) of
                {ok, Bindings, Remainings} ->
                    case bind_path(VPath, PathParts) of
                        {ok, PathParts1} ->
                            Path1 = make_target(Path, Bindings, Remainings, []),
                            {make_path(Path1), make_path(PathParts1)};
                        fail -> 
                            try_bind_vhost(Rest, HostParts, Port,
                                PathParts)
                    end;
                fail -> try_bind_vhost(Rest, HostParts, Port, PathParts)
            end;
        fail ->  try_bind_vhost(Rest, HostParts, Port, PathParts)
    end.

%% doc: build new patch from bindings. bindings are query args
%% (+ dynamic query rewritten if needed) and bindings found in
%% bind_path step. 
%% TODO: merge code with rewrite. But we need to make sure we are
%% in string here.
make_target([], _Bindings, _Remaining, Acc) ->
    lists:reverse(Acc);
make_target([?MATCH_ALL], _Bindings, Remaining, Acc) ->
    Acc1 = lists:reverse(Acc) ++ Remaining,
    Acc1;
make_target([?MATCH_ALL|_Rest], _Bindings, Remaining, Acc) ->
    Acc1 = lists:reverse(Acc) ++ Remaining,
    Acc1;
make_target([{bind, P}|Rest], Bindings, Remaining, Acc) ->
    P2 = case couch_util:get_value({bind, P}, Bindings) of
        undefined ->  "undefined";
        P1 -> P1
    end,
    make_target(Rest, Bindings, Remaining, [P2|Acc]);
make_target([P|Rest], Bindings, Remaining, Acc) ->
    make_target(Rest, Bindings, Remaining, [P|Acc]).

%% bind port
bind_port(Port, Port) -> ok;
bind_port('*', _) -> ok;
bind_port(_,_) -> fail.

%% bind bhost
bind_vhost([],[], Bindings) -> {ok, Bindings, []};
bind_vhost([?MATCH_ALL], [], _Bindings) -> fail;
bind_vhost([?MATCH_ALL], Rest, Bindings) -> {ok, Bindings, Rest};
bind_vhost([], _HostParts, _Bindings) -> fail;
bind_vhost([{bind, Token}|Rest], [Match|RestHost], Bindings) -> 
    bind_vhost(Rest, RestHost, [{{bind, Token}, Match}|Bindings]);
bind_vhost([Cname|Rest], [Cname|RestHost], Bindings) ->
    bind_vhost(Rest, RestHost, Bindings);
bind_vhost(_, _, _) -> fail.

%% bind path
bind_path([], PathParts) ->
    {ok, PathParts};
bind_path(_VPathParts, []) ->
    fail;
bind_path([Path|VRest],[Path|Rest]) ->
   bind_path(VRest, Rest);
bind_path(_, _) ->
    fail.

% utilities


%% create vhost list from ini
make_vhosts() ->
    Vhosts = lists:foldl(fun({Vhost, Path}, Acc) ->
                    [{parse_vhost(Vhost), split_path(Path)}|Acc]
            end, [], couch_config:get("vhosts")),

    lists:reverse(lists:usort(Vhosts)).


parse_vhost(Vhost) ->
    case urlsplit_netloc(Vhost, []) of
        {[], Path} ->
            {make_spec("*", []), '*', Path};
        {HostPort, []} ->
            {H, P} = split_host_port(HostPort),
            H1 = make_spec(H, []),
            {H1, P, []};
        {HostPort, Path} ->
            {H, P} = split_host_port(HostPort),
            H1 = make_spec(H, []),
            {H1, P, string:tokens(Path, "/")}
    end.
            

split_host_port(HostAsString) ->
    case string:rchr(HostAsString, $:) of
        0 ->
            {split_host(HostAsString), '*'};
        N ->
            HostPart = string:substr(HostAsString, 1, N-1), 
            case (catch erlang:list_to_integer(string:substr(HostAsString, 
                            N+1, length(HostAsString)))) of
                {'EXIT', _} ->
                    {split_host(HostAsString), '*'};
                Port ->
                    {split_host(HostPart), Port}
            end
    end.

split_host(HostAsString) ->
    string:tokens(HostAsString, "\.").

split_path(Path) ->
    make_spec(string:tokens(Path, "/"), []).


make_spec([], Acc) ->
    lists:reverse(Acc);
make_spec([""|R], Acc) ->
    make_spec(R, Acc);
make_spec(["*"|R], Acc) ->
    make_spec(R, [?MATCH_ALL|Acc]);
make_spec([P|R], Acc) ->
    P1 = parse_var(P),
    make_spec(R, [P1|Acc]).


parse_var(P) ->
    case P of 
        ":" ++ Var ->
            {bind, Var};
        _ -> P
    end.


% mochiweb doesn't export it.
urlsplit_netloc("", Acc) ->
    {lists:reverse(Acc), ""};
urlsplit_netloc(Rest=[C | _], Acc) when C =:= $/; C =:= $?; C =:= $# ->
    {lists:reverse(Acc), Rest};
urlsplit_netloc([C | Rest], Acc) ->
    urlsplit_netloc(Rest, [C | Acc]).

make_path(Parts) ->
     "/" ++ string:join(Parts,[?SEPARATOR]).

init(_) ->
    ok = couch_config:register(fun ?MODULE:config_change/2),
    
    %% load configuration
    {VHostGlobals, VHosts, Fun} = load_conf(),
    State = #vhosts_state{
        vhost_globals=VHostGlobals,
        vhosts=VHosts,
        vhosts_fun=Fun},
    {ok, State}.

handle_call(reload, _From, _State) ->
    {VHostGlobals, VHosts, Fun} = load_conf(),
    {reply, ok, #vhosts_state{
            vhost_globals=VHostGlobals,
            vhosts=VHosts,
            vhosts_fun=Fun}};
handle_call(get_state, _From, State) ->
    {reply, State, State};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

config_change("httpd", "vhost_global_handlers") ->
    ?MODULE:reload();
config_change("httpd", "redirect_vhost_handler") ->
    ?MODULE:reload();
config_change("vhosts", _) ->
    ?MODULE:reload().

load_conf() ->
    %% get vhost globals
    VHostGlobals = re:split(couch_config:get("httpd",
            "vhost_global_handlers",""), "\\s*,\\s*",[{return, list}]),

    %% build vhosts matching rules
    VHosts = make_vhosts(),

    %% build vhosts handler fun
    DefaultVHostFun = "{couch_httpd_vhost, redirect_to_vhost}",
    Fun = couch_httpd:make_arity_2_fun(couch_config:get("httpd",
            "redirect_vhost_handler", DefaultVHostFun)),

    {VHostGlobals, VHosts, Fun}.
