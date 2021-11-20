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

-module(couch_replicator_utils).

-export([
    parse_rep_doc/2,
    replication_id/2,
    sum_stats/2,
    is_deleted/1,
    rep_error_to_binary/1,
    get_json_value/2,
    get_json_value/3,
    pp_rep_id/1,
    iso8601/1,
    filter_state/3,
    normalize_rep/1,
    ejson_state_info/1,
    get_basic_auth_creds/1,
    remove_basic_auth_creds/1,
    normalize_basic_auth/1
]).

-include_lib("ibrowse/include/ibrowse.hrl").
-include_lib("couch/include/couch_db.hrl").
-include("couch_replicator.hrl").
-include_lib("couch_replicator/include/couch_replicator_api_wrap.hrl").

-import(couch_util, [
    get_value/2,
    get_value/3
]).

rep_error_to_binary(Error) ->
    couch_util:to_binary(error_reason(Error)).

error_reason({shutdown, Error}) ->
    error_reason(Error);
error_reason({error, {Error, Reason}}) when
    is_atom(Error), is_binary(Reason)
->
    io_lib:format("~s: ~s", [Error, Reason]);
error_reason({error, Reason}) ->
    Reason;
error_reason(Reason) ->
    Reason.

get_json_value(Key, Props) ->
    get_json_value(Key, Props, undefined).

get_json_value(Key, Props, Default) when is_atom(Key) ->
    Ref = make_ref(),
    case get_value(Key, Props, Ref) of
        Ref ->
            get_value(?l2b(atom_to_list(Key)), Props, Default);
        Else ->
            Else
    end;
get_json_value(Key, Props, Default) when is_binary(Key) ->
    Ref = make_ref(),
    case get_value(Key, Props, Ref) of
        Ref ->
            get_value(list_to_atom(?b2l(Key)), Props, Default);
        Else ->
            Else
    end.

% pretty-print replication id
-spec pp_rep_id(#rep{} | rep_id()) -> string().
pp_rep_id(#rep{id = RepId}) ->
    pp_rep_id(RepId);
pp_rep_id({Base, Extension}) ->
    Base ++ Extension.

% NV: TODO: this function is not used outside api wrap module
% consider moving it there during final cleanup
is_deleted(Change) ->
    get_json_value(<<"deleted">>, Change, false).

% NV: TODO: proxy some functions which used to be here, later remove
% these and replace calls to their respective modules
replication_id(Rep, Version) ->
    couch_replicator_ids:replication_id(Rep, Version).

sum_stats(S1, S2) ->
    couch_replicator_stats:sum_stats(S1, S2).

parse_rep_doc(Props, UserCtx) ->
    couch_replicator_docs:parse_rep_doc(Props, UserCtx).

-spec iso8601(erlang:timestamp()) -> binary().
iso8601({_Mega, _Sec, _Micro} = Timestamp) ->
    {{Y, Mon, D}, {H, Min, S}} = calendar:now_to_universal_time(Timestamp),
    Format = "~B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
    iolist_to_binary(io_lib:format(Format, [Y, Mon, D, H, Min, S])).

%% Filter replication info ejson by state provided. If it matches return
%% the input value, if it doesn't return 'skip'. This is used from replicator
%% fabric coordinator and worker.
-spec filter_state(atom(), [atom()], {[_ | _]}) -> {[_ | _]} | skip.
filter_state(null = _State, _States, _Info) ->
    skip;
filter_state(_ = _State, [] = _States, Info) ->
    Info;
filter_state(State, States, Info) ->
    case lists:member(State, States) of
        true ->
            Info;
        false ->
            skip
    end.

remove_basic_auth_from_headers(Headers) ->
    Headers1 = mochiweb_headers:make(Headers),
    case mochiweb_headers:get_value("Authorization", Headers1) of
        undefined ->
            {{undefined, undefined}, Headers};
        Auth ->
            {Basic, Base64} = lists:splitwith(fun(X) -> X =/= $\s end, Auth),
            maybe_remove_basic_auth(string:to_lower(Basic), Base64, Headers1)
    end.

maybe_remove_basic_auth("basic", " " ++ Base64, Headers) ->
    Headers1 = mochiweb_headers:delete_any("Authorization", Headers),
    {decode_basic_creds(Base64), mochiweb_headers:to_list(Headers1)};
maybe_remove_basic_auth(_, _, Headers) ->
    {{undefined, undefined}, mochiweb_headers:to_list(Headers)}.

decode_basic_creds(Base64) ->
    try re:split(base64:decode(Base64), ":", [{return, list}, {parts, 2}]) of
        [User, Pass] ->
            {User, Pass};
        _ ->
            {undefined, undefined}
    catch
        % Tolerate invalid B64 values here to avoid crashing replicator
        error:function_clause ->
            {undefined, undefined}
    end.

% Normalize a #rep{} record such that it doesn't contain time dependent fields
% pids (like httpc pools), and options / props are sorted. This function would
% used during comparisons.
-spec normalize_rep(#rep{} | nil) -> #rep{} | nil.
normalize_rep(nil) ->
    nil;
normalize_rep(#rep{} = Rep) ->
    #rep{
        source = couch_replicator_api_wrap:normalize_db(Rep#rep.source),
        target = couch_replicator_api_wrap:normalize_db(Rep#rep.target),
        % already sorted in make_options/1
        options = Rep#rep.options,
        type = Rep#rep.type,
        view = Rep#rep.view,
        doc_id = Rep#rep.doc_id,
        db_name = Rep#rep.db_name
    }.

-spec ejson_state_info(binary() | nil) -> binary() | null.
ejson_state_info(nil) ->
    null;
ejson_state_info(Info) when is_binary(Info) ->
    {[{<<"error">>, Info}]};
ejson_state_info([]) ->
    % Status not set yet => null for compatibility reasons
    null;
ejson_state_info([{_, _} | _] = Info) ->
    {Info};
ejson_state_info(Info) ->
    ErrMsg = couch_replicator_utils:rep_error_to_binary(Info),
    {[{<<"error">>, ErrMsg}]}.

-spec get_basic_auth_creds(#httpdb{}) ->
    {string(), string()} | {undefined, undefined}.
get_basic_auth_creds(#httpdb{auth_props = AuthProps}) ->
    case couch_util:get_value(<<"basic">>, AuthProps) of
        undefined ->
            {undefined, undefined};
        {UserPass} when is_list(UserPass) ->
            User = couch_util:get_value(<<"username">>, UserPass),
            Pass = couch_util:get_value(<<"password">>, UserPass),
            case {User, Pass} of
                _ when is_binary(User), is_binary(Pass) ->
                    {binary_to_list(User), binary_to_list(Pass)};
                _Other ->
                    {undefined, undefined}
            end;
        _Other ->
            {undefined, undefined}
    end.

-spec remove_basic_auth_creds(#httpd{}) -> #httpdb{}.
remove_basic_auth_creds(#httpdb{auth_props = Props} = HttpDb) ->
    Props1 = lists:keydelete(<<"basic">>, 1, Props),
    HttpDb#httpdb{auth_props = Props1}.

-spec set_basic_auth_creds(string(), string(), #httpd{}) -> #httpdb{}.
set_basic_auth_creds(undefined, undefined, #httpdb{} = HttpDb) ->
    HttpDb;
set_basic_auth_creds(User, Pass, #httpdb{} = HttpDb) when
    is_list(User), is_list(Pass)
->
    HttpDb1 = remove_basic_auth_creds(HttpDb),
    Props = HttpDb1#httpdb.auth_props,
    UserPass =
        {[
            {<<"username">>, list_to_binary(User)},
            {<<"password">>, list_to_binary(Pass)}
        ]},
    Props1 = lists:keystore(<<"basic">>, 1, Props, {<<"basic">>, UserPass}),
    HttpDb1#httpdb{auth_props = Props1}.

-spec extract_creds_from_url(string()) ->
    {ok, {string() | undefined, string() | undefined}, string()}
    | {error, term()}.
extract_creds_from_url(Url) ->
    case ibrowse_lib:parse_url(Url) of
        {error, Error} ->
            {error, Error};
        #url{username = undefined, password = undefined} ->
            {ok, {undefined, undefined}, Url};
        #url{protocol = Proto, username = User, password = Pass} ->
            % Excise user and pass parts from the url. Try to keep the host,
            % port and path as they were in the original.
            Prefix = lists:concat([Proto, "://", User, ":", Pass, "@"]),
            Suffix = lists:sublist(Url, length(Prefix) + 1, length(Url) + 1),
            NoCreds = lists:concat([Proto, "://", Suffix]),
            {ok, {User, Pass}, NoCreds}
    end.

% Normalize basic auth credentials so they are set only in the auth props
% object. If multiple basic auth credentials are provided, the resulting
% credentials are picked in the following order.
%  1) {"auth": "basic": {"username":.., "password": ...} ...}
%  2) URL userinfo part
%  3) "Authentication" : "basic $base64" headers
%
-spec normalize_basic_auth(#httpdb{}) -> #httpdb{}.
normalize_basic_auth(#httpdb{} = HttpDb) ->
    #httpdb{url = Url, headers = Headers} = HttpDb,
    {HeaderCreds, HeadersNoCreds} = remove_basic_auth_from_headers(Headers),
    {UrlCreds, UrlWithoutCreds} =
        case extract_creds_from_url(Url) of
            {ok, Creds = {_, _}, UrlNoCreds} ->
                {Creds, UrlNoCreds};
            {error, _Error} ->
                % Don't crash replicator if user provided an invalid
                % userinfo part
                {undefined, undefined}
        end,
    AuthCreds = {_, _} = get_basic_auth_creds(HttpDb),
    HttpDb1 = remove_basic_auth_creds(HttpDb#httpdb{
        url = UrlWithoutCreds,
        headers = HeadersNoCreds
    }),
    {User, Pass} =
        case {AuthCreds, UrlCreds, HeaderCreds} of
            {{U, P}, {_, _}, {_, _}} when is_list(U), is_list(P) -> {U, P};
            {{_, _}, {U, P}, {_, _}} when is_list(U), is_list(P) -> {U, P};
            {{_, _}, {_, _}, {U, P}} -> {U, P}
        end,
    set_basic_auth_creds(User, Pass, HttpDb1).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

remove_basic_auth_from_headers_test_() ->
    [
        ?_assertMatch(
            {{User, Pass}, NoAuthHeaders},
            remove_basic_auth_from_headers(Headers)
        )
     || {{User, Pass, NoAuthHeaders}, Headers} <- [
            {
                {undefined, undefined, []},
                []
            },
            {
                {undefined, undefined, [{"h", "v"}]},
                [{"h", "v"}]
            },
            {
                {undefined, undefined, [{"Authorization", "junk"}]},
                [{"Authorization", "junk"}]
            },
            {
                {undefined, undefined, []},
                [{"Authorization", "basic X"}]
            },
            {
                {"user", "pass", []},
                [{"Authorization", "Basic " ++ b64creds("user", "pass")}]
            },
            {
                {"user", "pass", []},
                [{"AuThorization", "Basic " ++ b64creds("user", "pass")}]
            },
            {
                {"user", "pass", []},
                [{"Authorization", "bAsIc " ++ b64creds("user", "pass")}]
            },
            {
                {"user", "pass", [{"h", "v"}]},
                [
                    {"Authorization", "Basic " ++ b64creds("user", "pass")},
                    {"h", "v"}
                ]
            }
        ]
    ].

b64creds(User, Pass) ->
    base64:encode_to_string(User ++ ":" ++ Pass).

normalize_rep_test_() ->
    {
        setup,
        fun() ->
            meck:expect(
                config,
                get,
                fun(_, _, Default) -> Default end
            )
        end,
        fun(_) -> meck:unload() end,
        ?_test(begin
            EJson1 =
                {[
                    {<<"source">>, <<"http://host.com/source_db">>},
                    {<<"target">>, <<"http://target.local/db">>},
                    {<<"doc_ids">>, [<<"a">>, <<"c">>, <<"b">>]},
                    {<<"other_field">>, <<"some_value">>}
                ]},
            Rep1 = couch_replicator_docs:parse_rep_doc_without_id(EJson1),
            EJson2 =
                {[
                    {<<"other_field">>, <<"unrelated">>},
                    {<<"target">>, <<"http://target.local/db">>},
                    {<<"source">>, <<"http://host.com/source_db">>},
                    {<<"doc_ids">>, [<<"c">>, <<"a">>, <<"b">>]},
                    {<<"other_field2">>, <<"unrelated2">>}
                ]},
            Rep2 = couch_replicator_docs:parse_rep_doc_without_id(EJson2),
            ?assertEqual(normalize_rep(Rep1), normalize_rep(Rep2))
        end)
    }.

get_basic_auth_creds_test() ->
    Check = fun(Props) ->
        get_basic_auth_creds(#httpdb{auth_props = Props})
    end,

    ?assertEqual({undefined, undefined}, Check([])),

    ?assertEqual({undefined, undefined}, Check([null])),

    ?assertEqual({undefined, undefined}, Check([{<<"other">>, <<"x">>}])),

    ?assertEqual({undefined, undefined}, Check([{<<"basic">>, []}])),

    UserPass1 = {[{<<"username">>, <<"u">>}, {<<"password">>, <<"p">>}]},
    ?assertEqual({"u", "p"}, Check([{<<"basic">>, UserPass1}])),

    UserPass3 = {[{<<"username">>, <<"u">>}, {<<"password">>, null}]},
    ?assertEqual({undefined, undefined}, Check([{<<"basic">>, UserPass3}])).

remove_basic_auth_creds_test() ->
    Check = fun(Props) ->
        HttpDb = remove_basic_auth_creds(#httpdb{auth_props = Props}),
        HttpDb#httpdb.auth_props
    end,

    ?assertEqual([], Check([])),

    ?assertEqual([{<<"other">>, {[]}}], Check([{<<"other">>, {[]}}])),

    ?assertEqual(
        [],
        Check([
            {<<"basic">>,
                {[
                    {<<"username">>, <<"u">>},
                    {<<"password">>, <<"p">>}
                ]}}
        ])
    ),

    ?assertEqual(
        [{<<"other">>, {[]}}],
        Check([
            {<<"basic">>,
                {[
                    {<<"username">>, <<"u">>},
                    {<<"password">>, <<"p">>}
                ]}},
            {<<"other">>, {[]}}
        ])
    ).

set_basic_auth_creds_test() ->
    Check = fun(User, Pass, Props) ->
        HttpDb = set_basic_auth_creds(User, Pass, #httpdb{auth_props = Props}),
        HttpDb#httpdb.auth_props
    end,

    ?assertEqual([], Check(undefined, undefined, [])),

    ?assertEqual(
        [{<<"other">>, {[]}}],
        Check(
            undefined,
            undefined,
            [{<<"other">>, {[]}}]
        )
    ),

    ?assertEqual(
        [
            {<<"basic">>,
                {[
                    {<<"username">>, <<"u">>},
                    {<<"password">>, <<"p">>}
                ]}}
        ],
        Check("u", "p", [])
    ),

    ?assertEqual(
        [
            {<<"other">>, {[]}},
            {<<"basic">>,
                {[
                    {<<"username">>, <<"u">>},
                    {<<"password">>, <<"p">>}
                ]}}
        ],
        Check("u", "p", [{<<"other">>, {[]}}])
    ).

normalize_basic_creds_test_() ->
    DefaultHeaders = (#httpdb{})#httpdb.headers,
    [
        ?_assertEqual(Expect, normalize_basic_auth(Input))
     || {Input, Expect} <- [
            {
                #httpdb{url = "http://u:p@x.y/db"},
                #httpdb{url = "http://x.y/db", auth_props = auth_props("u", "p")}
            },
            {
                #httpdb{url = "http://u:p@h:80/db"},
                #httpdb{url = "http://h:80/db", auth_props = auth_props("u", "p")}
            },
            {
                #httpdb{url = "https://u:p@h/db"},
                #httpdb{url = "https://h/db", auth_props = auth_props("u", "p")}
            },
            {
                #httpdb{url = "http://u:p@[2001:db8:a1b:12f9::1]/db"},
                #httpdb{
                    url = "http://[2001:db8:a1b:12f9::1]/db",
                    auth_props = auth_props("u", "p")
                }
            },
            {
                #httpdb{
                    url = "http://h/db",
                    headers =
                        DefaultHeaders ++
                            [
                                {"Authorization", "Basic " ++ b64creds("u", "p")}
                            ]
                },
                #httpdb{url = "http://h/db", auth_props = auth_props("u", "p")}
            },
            {
                #httpdb{
                    url = "http://h/db",
                    headers =
                        DefaultHeaders ++
                            [
                                {"Authorization", "Basic " ++ b64creds("u", "p@")}
                            ]
                },
                #httpdb{url = "http://h/db", auth_props = auth_props("u", "p@")}
            },
            {
                #httpdb{
                    url = "http://h/db",
                    headers =
                        DefaultHeaders ++
                            [
                                {"Authorization", "Basic " ++ b64creds("u", "p@%40")}
                            ]
                },
                #httpdb{url = "http://h/db", auth_props = auth_props("u", "p@%40")}
            },
            {
                #httpdb{
                    url = "http://h/db",
                    headers =
                        DefaultHeaders ++
                            [
                                {"aUthoriZation", "bASIC " ++ b64creds("U", "p")}
                            ]
                },
                #httpdb{url = "http://h/db", auth_props = auth_props("U", "p")}
            },
            {
                #httpdb{
                    url = "http://u1:p1@h/db",
                    headers =
                        DefaultHeaders ++
                            [
                                {"Authorization", "Basic " ++ b64creds("u2", "p2")}
                            ]
                },
                #httpdb{url = "http://h/db", auth_props = auth_props("u1", "p1")}
            },
            {
                #httpdb{
                    url = "http://u1:p1@h/db",
                    auth_props = [
                        {<<"basic">>,
                            {[
                                {<<"username">>, <<"u2">>},
                                {<<"password">>, <<"p2">>}
                            ]}}
                    ]
                },
                #httpdb{url = "http://h/db", auth_props = auth_props("u2", "p2")}
            },
            {
                #httpdb{
                    url = "http://u1:p1@h/db",
                    auth_props = [
                        {<<"basic">>,
                            {[
                                {<<"username">>, <<"u2">>},
                                {<<"password">>, <<"p2">>}
                            ]}}
                    ],
                    headers =
                        DefaultHeaders ++
                            [
                                {"Authorization", "Basic " ++ b64creds("u3", "p3")}
                            ]
                },
                #httpdb{url = "http://h/db", auth_props = auth_props("u2", "p2")}
            }
        ]
    ].

auth_props(User, Pass) when is_list(User), is_list(Pass) ->
    [
        {<<"basic">>,
            {[
                {<<"username">>, list_to_binary(User)},
                {<<"password">>, list_to_binary(Pass)}
            ]}}
    ].

-endif.
