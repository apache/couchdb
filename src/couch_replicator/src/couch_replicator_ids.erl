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

-module(couch_replicator_ids).

-export([
    replication_id/1,
    replication_id/2,
    convert/1
]).

-include_lib("ibrowse/include/ibrowse.hrl").

-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_replicator/include/couch_replicator_api_wrap.hrl").
-include("couch_replicator.hrl").

% replication_id/1 and replication_id/2 will attempt to fetch
% filter code for filtered replications. If fetching or parsing
% of the remotely fetched filter code fails they throw:
%  {filter_fetch_error, Error} exception.
%

replication_id(#rep{options = Options} = Rep) ->
    BaseId = replication_id(Rep, ?REP_ID_VERSION),
    {BaseId, maybe_append_options([continuous, create_target], Options)}.


% Versioned clauses for generating replication IDs.
% If a change is made to how replications are identified,
% please add a new clause and increase ?REP_ID_VERSION.

replication_id(#rep{} = Rep, 4) ->
    UUID = couch_server:get_uuid(),
    SrcInfo = get_v4_endpoint(Rep#rep.source),
    TgtInfo = get_v4_endpoint(Rep#rep.target),
    maybe_append_filters([UUID, SrcInfo, TgtInfo], Rep);

replication_id(#rep{} = Rep, 3) ->
    UUID = couch_server:get_uuid(),
    Src = get_rep_endpoint(Rep#rep.source),
    Tgt = get_rep_endpoint(Rep#rep.target),
    maybe_append_filters([UUID, Src, Tgt], Rep);

replication_id(#rep{} = Rep, 2) ->
    {ok, HostName} = inet:gethostname(),
    Port = case (catch mochiweb_socket_server:get(couch_httpd, port)) of
    P when is_number(P) ->
        P;
    _ ->
        % On restart we might be called before the couch_httpd process is
        % started.
        % TODO: we might be under an SSL socket server only, or both under
        % SSL and a non-SSL socket.
        % ... mochiweb_socket_server:get(https, port)
        list_to_integer(config:get("httpd", "port", "5984"))
    end,
    Src = get_rep_endpoint(Rep#rep.source),
    Tgt = get_rep_endpoint(Rep#rep.target),
    maybe_append_filters([HostName, Port, Src, Tgt], Rep);

replication_id(#rep{} = Rep, 1) ->
    {ok, HostName} = inet:gethostname(),
    Src = get_rep_endpoint(Rep#rep.source),
    Tgt = get_rep_endpoint(Rep#rep.target),
    maybe_append_filters([HostName, Src, Tgt], Rep).


-spec convert([_] | binary() | {string(), string()}) -> {string(), string()}.
convert(Id) when is_list(Id) ->
    convert(?l2b(Id));
convert(Id0) when is_binary(Id0) ->
    % Spaces can result from mochiweb incorrectly unquoting + characters from
    % the URL path. So undo the incorrect parsing here to avoid forcing
    % users to url encode + characters.
    Id = binary:replace(Id0, <<" ">>, <<"+">>, [global]),
    lists:splitwith(fun(Char) -> Char =/= $+ end, ?b2l(Id));
convert({BaseId, Ext} = Id) when is_list(BaseId), is_list(Ext) ->
    Id.


% Private functions

maybe_append_filters(Base,
        #rep{source = Source, options = Options}) ->
    Base2 = Base ++
        case couch_replicator_filters:parse(Options) of
        {ok, nil} ->
            [];
        {ok, {view, Filter, QueryParams}} ->
            [Filter, QueryParams];
        {ok, {user, {Doc, Filter}, QueryParams}} ->
            case couch_replicator_filters:fetch(Doc, Filter, Source) of
                {ok, Code} ->
                    [Code, QueryParams];
                {error, Error} ->
                    throw({filter_fetch_error, Error})
            end;
        {ok, {docids, DocIds}} ->
            [DocIds];
        {ok, {mango, Selector}} ->
            [Selector];
        {error, FilterParseError} ->
            throw({error, FilterParseError})
        end,
    couch_util:to_hex(couch_hash:md5_hash(term_to_binary(Base2))).


maybe_append_options(Options, RepOptions) ->
    lists:foldl(fun(Option, Acc) ->
        Acc ++
        case couch_util:get_value(Option, RepOptions, false) of
        true ->
            "+" ++ atom_to_list(Option);
        false ->
            ""
        end
    end, [], Options).


get_rep_endpoint(#httpdb{url=Url, headers=Headers}) ->
    DefaultHeaders = (#httpdb{})#httpdb.headers,
    {remote, Url, Headers -- DefaultHeaders}.


get_v4_endpoint(#httpdb{} = HttpDb) ->
    {remote, Url, Headers} = get_rep_endpoint(HttpDb),
    {{UserFromHeaders, _}, HeadersWithoutBasicAuth} =
        couch_replicator_utils:remove_basic_auth_from_headers(Headers),
    {UserFromUrl, Host, NonDefaultPort, Path} = get_v4_url_info(Url),
    User = pick_defined_value([UserFromUrl, UserFromHeaders]),
    OAuth = undefined, % Keep this to ensure checkpoints don't change
    {remote, User, Host, NonDefaultPort, Path, HeadersWithoutBasicAuth, OAuth}.


pick_defined_value(Values) ->
    case [V || V <- Values, V /= undefined] of
        [] ->
            undefined;
        DefinedValues ->
            hd(DefinedValues)
    end.


get_v4_url_info(Url) when is_binary(Url) ->
    get_v4_url_info(binary_to_list(Url));
get_v4_url_info(Url) ->
    case ibrowse_lib:parse_url(Url) of
        {error, invalid_uri} ->
            % Tolerate errors here to avoid a bad user document
            % crashing the replicator
            {undefined, Url, undefined, undefined};
        #url{
            protocol = Schema,
            username = User,
            host = Host,
            port = Port,
            path = Path
        } ->
            NonDefaultPort = get_non_default_port(Schema, Port),
            {User, Host, NonDefaultPort, Path}
    end.


get_non_default_port(https, 443) ->
    default;
get_non_default_port(http, 80) ->
    default;
get_non_default_port(http, 5984) ->
    default;
get_non_default_port(_Schema, Port) ->
    Port.


-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").


replication_id_convert_test_() ->
    [?_assertEqual(Expected, convert(Id)) || {Expected, Id} <- [
        {{"abc", ""}, "abc"},
        {{"abc", ""}, <<"abc">>},
        {{"abc", "+x+y"}, <<"abc+x+y">>},
        {{"abc", "+x+y"}, {"abc", "+x+y"}},
        {{"abc", "+x+y"}, <<"abc x y">>}
    ]].

http_v4_endpoint_test_() ->
    [?_assertMatch({remote, User, Host, Port, Path, HeadersNoAuth, undefined},
        get_v4_endpoint(#httpdb{url = Url, headers = Headers})) ||
            {{User, Host, Port, Path, HeadersNoAuth}, {Url, Headers}} <- [
                {
                    {undefined, "host", default, "/", []},
                    {"http://host", []}
                },
                {
                    {undefined, "host", default, "/", []},
                    {"https://host", []}
                },
                {
                    {undefined, "host", default, "/", []},
                    {"http://host:5984", []}
                },
                {
                    {undefined, "host", 1, "/", []},
                    {"http://host:1", []}
                },
                {
                    {undefined, "host", 2, "/", []},
                    {"https://host:2", []}
                },
                {
                    {undefined, "host", default, "/", [{"h","v"}]},
                    {"http://host", [{"h","v"}]}
                },
                {
                    {undefined, "host", default, "/a/b", []},
                    {"http://host/a/b", []}
                },
                {
                    {"user", "host", default, "/", []},
                    {"http://user:pass@host", []}
                },
                {
                    {"user", "host", 3, "/", []},
                    {"http://user:pass@host:3", []}
                },
                {
                    {"user", "host", default, "/", []},
                    {"http://user:newpass@host", []}
                },
                {
                    {"user", "host", default, "/", []},
                    {"http://host", [basic_auth("user","pass")]}
                },
                {
                    {"user", "host", default, "/", []},
                    {"http://host", [basic_auth("user","newpass")]}
                },
                {
                    {"user1", "host", default, "/", []},
                    {"http://user1:pass1@host", [basic_auth("user2","pass2")]}
                },
                {
                    {"user", "host", default, "/", [{"h", "v"}]},
                    {"http://host", [{"h", "v"}, basic_auth("user","pass")]}
                },
                {
                    {undefined, "random_junk", undefined, undefined},
                    {"random_junk", []}
                },
                {
                    {undefined, "host", default, "/", []},
                    {"http://host", [{"Authorization", "Basic bad"}]}
                }
        ]
    ].


basic_auth(User, Pass) ->
    B64Auth = base64:encode_to_string(User ++ ":" ++ Pass),
    {"Authorization", "Basic " ++ B64Auth}.


-endif.
