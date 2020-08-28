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
    base_id/2,
    job_id/3,
    job_id/2,
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

replication_id(#{?OPTIONS := Options} = Rep) ->
    BaseId = base_id(Rep, ?REP_ID_VERSION),
    UseOpts = [<<"continuous">>, <<"create_target">>],
    ExtId = maybe_append_options(UseOpts, Options),
    RepId = iolist_to_binary([BaseId, ExtId]),
    {RepId, BaseId}.


% Versioned clauses for generating replication IDs.
% If a change is made to how replications are identified,
% please add a new clause and increase ?REP_ID_VERSION.

base_id(#{?SOURCE := Src, ?TARGET := Tgt} = Rep, 4) ->
    UUID = couch_server:get_uuid(),
    SrcInfo = get_v4_endpoint(Src),
    TgtInfo = get_v4_endpoint(Tgt),
    maybe_append_filters([UUID, SrcInfo, TgtInfo], Rep);

base_id(#{?SOURCE := Src0, ?TARGET := Tgt0} = Rep, 3) ->
    UUID = couch_server:get_uuid(),
    Src = get_rep_endpoint(Src0),
    Tgt = get_rep_endpoint(Tgt0),
    maybe_append_filters([UUID, Src, Tgt], Rep);

base_id(#{?SOURCE := Src0, ?TARGET := Tgt0} = Rep, 2) ->
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
    Src = get_rep_endpoint(Src0),
    Tgt = get_rep_endpoint(Tgt0),
    maybe_append_filters([HostName, Port, Src, Tgt], Rep);

base_id(#{?SOURCE := Src0, ?TARGET := Tgt0} = Rep, 1) ->
    {ok, HostName} = inet:gethostname(),
    Src = get_rep_endpoint(Src0),
    Tgt = get_rep_endpoint(Tgt0),
    maybe_append_filters([HostName, Src, Tgt], Rep).


-spec job_id(#{}, binary() | null, binary() | null) -> binary().
job_id(#{} = Rep, null = _DbUUID, null = _DocId) ->
    #{
        ?SOURCE := Src,
        ?TARGET := Tgt,
        ?REP_USER := UserName,
        ?OPTIONS := Options
    } = Rep,
    UUID = couch_server:get_uuid(),
    SrcInfo = get_v4_endpoint(Src),
    TgtInfo = get_v4_endpoint(Tgt),
    UseOpts = [<<"continuous">>, <<"create_target">>],
    Opts = maybe_append_options(UseOpts, Options),
    IdParts = [UUID, SrcInfo, TgtInfo, UserName, Opts],
    maybe_append_filters(IdParts, Rep, false);

job_id(#{} = _Rep, DbUUID, DocId) when is_binary(DbUUID), is_binary(DocId) ->
    job_id(DbUUID, DocId).


-spec job_id(binary(), binary()) -> binary().
job_id(DbUUID, DocId) when is_binary(DbUUID), is_binary(DocId) ->
    <<DbUUID/binary, "|", DocId/binary>>.


-spec convert(binary()) -> binary().
convert(Id0) when is_binary(Id0) ->
    % Spaces can result from mochiweb incorrectly unquoting + characters from
    % the URL path. So undo the incorrect parsing here to avoid forcing
    % users to url encode + characters.
    binary:replace(Id0, <<" ">>, <<"+">>, [global]).


% Private functions

maybe_append_filters(Base, #{} = Rep) ->
    maybe_append_filters(Base, Rep, true).


maybe_append_filters(Base, #{} = Rep, FetchFilter) ->
    #{
        ?SOURCE := Source,
        ?OPTIONS := Options
    } = Rep,
    Base2 = Base ++
        case couch_replicator_filters:parse(Options) of
        {ok, nil} ->
            [];
        {ok, {view, Filter, QueryParams}} ->
            [Filter, QueryParams];
        {ok, {user, {Doc, Filter}, QueryParams}} when FetchFilter =:= true ->
            case couch_replicator_filters:fetch(Doc, Filter, Source) of
                {ok, Code} ->
                    [Code, QueryParams];
                {error, Error} ->
                    throw({filter_fetch_error, Error})
            end;
        {ok, {user, {Doc, Filter}, QueryParams}} when FetchFilter =:= false ->
            [Doc, Filter, QueryParams];
        {ok, {docids, DocIds}} ->
            [DocIds];
        {ok, {mango, Selector}} ->
            [Selector];
        {error, FilterParseError} ->
            throw({error, FilterParseError})
        end,
    Res = couch_util:to_hex(couch_hash:md5_hash(term_to_binary(Base2))),
    list_to_binary(Res).


maybe_append_options(Options, #{} = RepOptions) ->
    lists:foldl(fun(Option, Acc) ->
        Acc ++
        case maps:get(Option, RepOptions, false) of
            true -> "+" ++ binary_to_list(Option);
            false -> ""
        end
    end, [], Options).


get_rep_endpoint(#{<<"url">> := Url0, <<"headers">> := Headers0}) ->
    % We turn everything to lists and proplists to calculate the same
    % replication ID as CouchDB <= 3.x
    Url = binary_to_list(Url0),
    Headers1 = maps:fold(fun(K, V, Acc) ->
        [{binary_to_list(K), binary_to_list(V)} | Acc]
    end, [], Headers0),
    Headers2 = lists:keysort(1, Headers1),
    DefaultHeaders = (#httpdb{})#httpdb.headers,
    {remote, Url, Headers2 -- DefaultHeaders}.


get_v4_endpoint(#{} = HttpDb) ->
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
-include_lib("fabric/test/fabric2_test.hrl").


replication_id_convert_test_() ->
    [?_assertEqual(Expected, convert(Id)) || {Expected, Id} <- [
        {<<"abc">>, <<"abc">>},
        {<<"abc+x">>, <<"abc+x">>},
        {<<"abc+x">>, <<"abc x">>},
        {<<"abc+x+y">>, <<"abc+x+y">>},
        {<<"abc+x+y">>, <<"abc x y">>}
    ]].


http_v4_endpoint_test_() ->
    [?_assertMatch({remote, User, Host, Port, Path, HeadersNoAuth, undefined},
        get_v4_endpoint(#{<<"url">> => Url, <<"headers">> => Headers})) ||
            {{User, Host, Port, Path, HeadersNoAuth}, {Url, Headers}} <- [
                {
                    {undefined, "host", default, "/", []},
                    {<<"http://host">>, #{}}
                },
                {
                    {undefined, "host", default, "/", []},
                    {<<"https://host">>, #{}}
                },
                {
                    {undefined, "host", default, "/", []},
                    {<<"http://host:5984">>, #{}}
                },
                {
                    {undefined, "host", 1, "/", []},
                    {<<"http://host:1">>, #{}}
                },
                {
                    {undefined, "host", 2, "/", []},
                    {<<"https://host:2">>, #{}}
                },
                {
                    {undefined, "host", default, "/", [{"h", "v"}]},
                    {<<"http://host">>, #{<<"h">> => <<"v">>}}
                },
                {
                    {undefined, "host", default, "/a/b", []},
                    {<<"http://host/a/b">>, #{}}
                },
                {
                    {"user", "host", default, "/", []},
                    {<<"http://user:pass@host">>, #{}}
                },
                {
                    {"user", "host", 3, "/", []},
                    {<<"http://user:pass@host:3">>, #{}}
                },
                {
                    {"user", "host", default, "/", []},
                    {<<"http://user:newpass@host">>, #{}}
                },
                {
                    {"user", "host", default, "/", []},
                    {<<"http://host">>, basic_auth(<<"user">>, <<"pass">>)}
                },
                {
                    {"user", "host", default, "/", []},
                    {<<"http://host">>, basic_auth(<<"user">>, <<"newpass">>)}
                },
                {
                    {"user1", "host", default, "/", []},
                    {<<"http://user1:pass1@host">>, basic_auth(<<"user2">>,
                        <<"pass2">>)}
                },
                {
                    {"user", "host", default, "/", [{"h", "v"}]},
                    {<<"http://host">>, maps:merge(#{<<"h">> => <<"v">>},
                        basic_auth(<<"user">>, <<"pass">>))}
                },
                {
                    {undefined, "random_junk", undefined, undefined},
                    {<<"random_junk">>, #{}}
                },
                {
                    {undefined, "host", default, "/", []},
                    {<<"http://host">>, #{<<"Authorization">> =>
                        <<"Basic bad">>}}
                }
        ]
    ].


basic_auth(User, Pass) ->
    B64Auth = base64:encode(<<User/binary, ":", Pass/binary>>),
    #{<<"Authorization">> => <<"Basic ", B64Auth/binary>>}.


version4_matches_couchdb3_test_() ->
    {
        foreach,
        fun setup/0,
        fun teardown/1,
        [
            ?TDEF_FE(id_matches_couchdb3)
        ]
    }.


setup() ->
    meck:expect(config, get, fun(_, _, Default) -> Default end).


teardown(_) ->
    meck:unload().


id_matches_couchdb3(_) ->
    {ok, Rep} = couch_replicator_parse:parse_rep(#{
        <<"source">> => <<"http://adm:pass@127.0.0.1/abc">>,
        <<"target">> => <<"http://adm:pass@127.0.0.1/xyz">>,
        <<"create_target">> => true,
        <<"continuous">> => true
    }, null),
    meck:expect(couch_server, get_uuid, 0, "somefixedid"),
    {RepId, BaseId} = replication_id(Rep),
    % Calculated on CouchDB 3.x
    RepId3x = <<"ff71e1208f93ba054eb60e7ca8683fe4+continuous+create_target">>,
    BaseId3x = <<"ff71e1208f93ba054eb60e7ca8683fe4">>,
    ?assertEqual(RepId3x, RepId),
    ?assertEqual(BaseId3x, BaseId).


-endif.
