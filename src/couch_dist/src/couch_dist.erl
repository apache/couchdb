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

-export([childspecs/0, listen/2, accept/1, accept_connection/5,
    setup/5, close/1, select/1, is_node_name/1]).

childspecs() ->
    {ok, [{ssl_dist_sup, {ssl_dist_sup, start_link, []},
        permanent, infinity, supervisor, [ssl_dist_sup]}]}.

listen(Name, Host) ->
    inet_tls_dist:listen(Name, Host).


accept(Listen) ->
    inet_tls_dist:accept(Listen).


accept_connection(AcceptPid, DistCtrl, MyNode, Allowed, SetupTime) ->
    case no_tls(MyNode) of
        true ->
            inet_tcp_dist:accept_connection(
                AcceptPid, DistCtrl, MyNode, Allowed, SetupTime);
        false ->
            inet_tls_dist:accept_connection(
                AcceptPid, DistCtrl, MyNode, Allowed, SetupTime)
    end.


setup(Node, Type, MyNode, LongOrShortNames, SetupTime) ->
    case no_tls(Node) of
      true ->
        inet_tcp_dist:setup(
            Node, Type, MyNode, LongOrShortNames, SetupTime);
      false ->
        inet_tls_dist:setup(
            Node, Type, MyNode, LongOrShortNames, SetupTime)
    end.


close(Socket) ->
    inet_tls_dist:close(Socket).


select(Node) ->
    inet_tls_dist:select(Node).


is_node_name(Node) ->
    inet_tls_dist:is_node_name(Node).


no_tls(NodeName) when is_atom(NodeName) ->
    no_tls(atom_to_list(NodeName));

no_tls(NodeName) when is_list(NodeName) ->
    {ok, Args} = init:get_argument(couch_dist),
    GlobPatterns = [V || [K, V] <- Args, K == "no_tls"],
    GlobPattern = case GlobPatterns of
                      ["true"] -> ["*@127.0.0.1"];
                      _ -> GlobPatterns
                  end,
    lists:any(fun(P) -> match(NodeName, P) end, GlobPattern).


match(Name, GlobPattern) ->
    {ok, RE} = to_re(GlobPattern),
    re:run(Name, RE) /= nomatch.


to_re(GlobPattern) ->
    re:compile([$^, lists:flatmap(fun glob_re/1, GlobPattern), $$]).


glob_re($*) ->
    ".*";
glob_re($?) ->
    ".";
glob_re(C) ->
    [C].
