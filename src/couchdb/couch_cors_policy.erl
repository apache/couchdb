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

-module(couch_cors_policy).
-export([global_config/0, check/2, check/3]).

-include("couch_db.hrl").

check(DbConfig, #httpd{}=Req) ->
    check(global_config(), DbConfig, Req).

check(Global, Local, #httpd{}=Req)
        when is_list(Global) andalso is_list(Local) ->
    {Httpd} = couch_util:get_value(<<"httpd">>, Global, {[]}),
    Enabled = couch_util:get_value(<<"cors_enabled">>, Httpd, false),
    case Enabled of
        true -> [];
        _ -> false
    end.


global_config() ->
    % Return the globally-configured CORS settings in a format identical
    % to that in the _security object.
    Enabled = couch_util:to_existing_atom(
                couch_config:get("httpd", "cors_enabled", "false")),
    DomainsToOrigins = binary_section("origins"),
    Global = [ {<<"httpd">>, {[ {<<"cors_enabled">>, Enabled} ]}}
             , {<<"origins">>, {DomainsToOrigins}}
             ],

    AllOrigins = lists:flatten([ re:split(Line, ",\\s*")
                               || {_Vhost, Line} <- DomainsToOrigins]),

    lists:foldl(fun(OriginName, Config) ->
        Stanza = {OriginName, binary_section(OriginName)},
        lists:keystore(OriginName, 1, Config, Stanza)
    end, Global, AllOrigins).

binary_section(Section) ->
    % Return a config section, with all strings converted to binaries.
    SectionStr = couch_config:get(Section),
    BoolOrBinary = fun("true") -> true;
                      ("false") -> false;
                      (Val) -> ?l2b(Val)
    end,
    [ {?l2b(Key), BoolOrBinary(Val)} || {Key, Val} <- SectionStr ].

% vim: sts=4 sw=4 et
