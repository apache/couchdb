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

-module(couch_dist).

-export([
    childspecs/0,
    listen/1,
    listen/2,
    accept/1,
    accept_connection/5,
    setup/5,
    close/1,
    select/1,
    is_node_name/1
]).

% Just for tests
-export([no_tls/1, get_init_args/0]).

childspecs() ->
    {ok, [
        {ssl_dist_sup, {ssl_dist_sup, start_link, []}, permanent, infinity, supervisor, [
            ssl_dist_sup
        ]}
    ]}.

listen(Name) ->
    NodeName =
        case is_atom(Name) of
            true -> atom_to_list(Name);
            false -> Name
        end,
    Host = get_node_host(),
    Mod = inet_dist(NodeName ++ "@" ++ Host),
    Mod:listen(NodeName).

listen(Name, Host) ->
    NodeName =
        case is_atom(Name) of
            true -> atom_to_list(Name);
            false -> Name
        end,
    Mod = inet_dist(NodeName ++ "@" ++ Host),
    Mod:listen(NodeName, Host).

accept(Listen) ->
    Mod = inet_dist(node()),
    Mod:accept(Listen).

accept_connection(AcceptPid, DistCtrl, MyNode, Allowed, SetupTime) ->
    Mod = inet_dist(MyNode),
    Mod:accept_connection(AcceptPid, DistCtrl, MyNode, Allowed, SetupTime).

setup(Node, Type, MyNode, LongOrShortNames, SetupTime) ->
    Mod = inet_dist(Node),
    Mod:setup(Node, Type, MyNode, LongOrShortNames, SetupTime).

close(Socket) ->
    inet_tls_dist:close(Socket).

select(Node) ->
    inet_tls_dist:select(Node).

is_node_name(Node) ->
    inet_tls_dist:is_node_name(Node).

get_init_args() ->
    init:get_argument(couch_dist).

get_node_host() ->
    % Cannot use `node()` since distribution hasn't started yet. Use
    % similar logic as erl_distribition and net_kernel to parse it
    % from the arguments list
    case {init:get_argument(sname), init:get_argument(name)} of
        {{ok, [[SName]]}, _} ->
            case split_host(SName) of
                [$@ | Host] when length(Host) > 0 ->
                    Host;
                _ ->
                    inet_db:gethostname()
            end;
        {error, {ok, [[Name]]}} ->
            case split_host(Name) of
                [$@ | Host] when length(Host) > 0 ->
                    Host;
                _ ->
                    OwnHost = inet_db:gethostname(),
                    case inet_db:res_option(domain) of
                        Domain when is_list(Domain), length(Domain) > 0 ->
                            OwnHost ++ "." ++ Domain;
                        _ ->
                            OwnHost
                    end
            end
    end.

split_host(Name) ->
    % Copied from net_kernel. Modifed to return Host only
    {_, Host} = lists:splitwith(fun(C) -> C =/= $@ end, Name),
    Host.

inet_dist(Node) ->
    case no_tls(Node) of
        true -> inet_tcp_dist;
        false -> inet_tls_dist
    end.

no_tls(NodeName) when is_atom(NodeName) ->
    no_tls(atom_to_list(NodeName));
no_tls(NodeName) when is_list(NodeName) ->
    case ?MODULE:get_init_args() of
        {ok, Args} ->
            GlobPatterns = [V || [K, V] <- Args, K == "no_tls"],
            lists:any(fun(P) -> match(NodeName, P) end, GlobPatterns);
        error ->
            false
    end.

match(_NodeName, "true") ->
    true;
match(_NodeName, "false") ->
    false;
match(NodeName, Pattern) ->
    {ok, RE} =
        case string:split(Pattern, [$"], all) of
            ["", GlobPattern, ""] -> to_re(GlobPattern);
            _ -> to_re(Pattern)
        end,
    re:run(NodeName, RE) /= nomatch.

to_re(GlobPattern) ->
    re:compile([$^, lists:flatmap(fun glob_re/1, GlobPattern), $$]).

glob_re($*) ->
    ".*";
glob_re($?) ->
    ".";
glob_re(C) ->
    [C].
