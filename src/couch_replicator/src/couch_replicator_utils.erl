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
   rep_error_to_binary/1,
   iso8601/0,
   iso8601/1,
   rfc1123_local/0,
   rfc1123_local/1,
   remove_basic_auth_from_headers/1,
   normalize_rep/1,
   compare_reps/2,
   default_headers_map/0,
   parse_replication_states/1,
   parse_int_param/5,
   proplist_options/1
]).


-include_lib("couch/include/couch_db.hrl").
-include("couch_replicator.hrl").
-include_lib("couch_replicator/include/couch_replicator_api_wrap.hrl").


rep_error_to_binary(Error) ->
    couch_util:to_binary(error_reason(Error)).


error_reason({shutdown, Error}) ->
    error_reason(Error);
error_reason({error, {Error, Reason}})
  when is_atom(Error), is_binary(Reason) ->
    io_lib:format("~s: ~s", [Error, Reason]);
error_reason({error, Reason}) ->
    Reason;
error_reason(Reason) ->
    Reason.


-spec iso8601() -> binary().
iso8601() ->
    iso8601(erlang:system_time(second)).


-spec iso8601(integer()) -> binary().
iso8601(Sec) when is_integer(Sec) ->
    Time = unix_sec_to_timestamp(Sec),
    {{Y, Mon, D}, {H, Min, S}} = calendar:now_to_universal_time(Time),
    Format = "~B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
    iolist_to_binary(io_lib:format(Format, [Y, Mon, D, H, Min, S])).


rfc1123_local() ->
    list_to_binary(httpd_util:rfc1123_date()).


rfc1123_local(Sec) ->
    Time = unix_sec_to_timestamp(Sec),
    Local = calendar:now_to_local_time(Time),
    list_to_binary(httpd_util:rfc1123_date(Local)).


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


-spec compare_reps(#{} | null, #{} | null) -> boolean().
compare_reps(Rep1, Rep2) ->
    NormRep1 = normalize_rep(Rep1),
    NormRep2 = normalize_rep(Rep2),
    NormRep1 =:= NormRep2.


% Normalize a rep map such that it doesn't contain time dependent fields
% pids (like httpc pools), and options / props are sorted. This function would
% used during comparisons.
-spec normalize_rep(#{} | null) -> #{} | null.
normalize_rep(null) ->
    null;

normalize_rep(#{} = Rep)->
    #{
        ?SOURCE := Source,
        ?TARGET := Target,
        ?OPTIONS := Options
    } = Rep,
    #{
        ?SOURCE => normalize_endpoint(Source),
        ?TARGET => normalize_endpoint(Target),
        ?OPTIONS => Options
    }.


normalize_endpoint(<<DbName/binary>>) ->
    DbName;

normalize_endpoint(#{} = Endpoint) ->
    Ks = [
        <<"url">>,
        <<"auth_props">>,
        <<"headers">>,
        <<"timeout">>,
        <<"ibrowse_options">>,
        <<"retries">>,
        <<"http_connections">>,
        <<"proxy_url">>
    ],
    maps:with(Ks, Endpoint).


default_headers_map() ->
    lists:foldl(fun({K, V}, Acc) ->
        Acc#{list_to_binary(K) => list_to_binary(V)}
    end, #{}, (#httpdb{})#httpdb.headers).


parse_replication_states(undefined) ->
    [];  % This is the default (wildcard) filter

parse_replication_states(States) when is_list(States) ->
    All = [?ST_RUNNING, ?ST_FAILED, ?ST_COMPLETED, ?ST_PENDING, ?ST_CRASHING],
    AllSet = sets:from_list(All),
    BinStates = [?l2b(string:to_lower(S)) || S <- string:tokens(States, ",")],
    StatesSet = sets:from_list(BinStates),
    Diff = sets:to_list(sets:subtract(StatesSet, AllSet)),
    case Diff of
        [] ->
            BinStates;
        _ ->
            Args = [Diff, All],
            Msg2 = io_lib:format("Unknown states ~p. Choose from: ~p", Args),
            throw({query_parse_error, ?l2b(Msg2)})
    end.


parse_int_param(Req, Param, Default, Min, Max) ->
    IntVal = try
        list_to_integer(chttpd:qs_value(Req, Param, integer_to_list(Default)))
    catch error:badarg ->
        Msg1 = io_lib:format("~s must be an integer", [Param]),
        throw({query_parse_error, ?l2b(Msg1)})
    end,
    case IntVal >= Min andalso IntVal =< Max of
    true ->
        IntVal;
    false ->
        Msg2 = io_lib:format("~s not in range of [~w,~w]", [Param, Min, Max]),
        throw({query_parse_error, ?l2b(Msg2)})
    end.


proplist_options(#{} = OptionsMap) ->
    maps:fold(fun(K, V, Acc) ->
        [{binary_to_atom(K, utf8), V} | Acc]
    end, [], OptionsMap).


unix_sec_to_timestamp(Sec) when is_integer(Sec) ->
    MegaSecPart = Sec div 1000000,
    SecPart = Sec - MegaSecPart * 1000000,
    {MegaSecPart, SecPart, 0}.


-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

remove_basic_auth_from_headers_test_() ->
    [?_assertMatch({{User, Pass}, NoAuthHeaders},
        remove_basic_auth_from_headers(Headers)) ||
        {{User, Pass, NoAuthHeaders}, Headers} <- [
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
        fun() -> meck:expect(config, get,
            fun(_, _, Default) -> Default end)
        end,
        fun(_) -> meck:unload() end,
        ?_test(begin
            EJson1 = {[
                {<<"source">>, <<"http://host.com/source_db">>},
                {<<"target">>, <<"http://target.local/db">>},
                {<<"doc_ids">>, [<<"a">>, <<"c">>, <<"b">>]},
                {<<"other_field">>, <<"some_value">>}
            ]},
            Rep1 = couch_replicator_parse:parse_rep_doc(EJson1),
            EJson2 = {[
                {<<"other_field">>, <<"unrelated">>},
                {<<"target">>, <<"http://target.local/db">>},
                {<<"source">>, <<"http://host.com/source_db">>},
                {<<"doc_ids">>, [<<"c">>, <<"a">>, <<"b">>]},
                {<<"other_field2">>, <<"unrelated2">>}
            ]},
            Rep2 = couch_replicator_parse:parse_rep_doc(EJson2),
            ?assertEqual(normalize_rep(Rep1), normalize_rep(Rep2))
        end)
    }.


normalize_endpoint() ->
    HttpDb =  #httpdb{
        url = "http://host/db",
        auth_props = [{"key", "val"}],
        headers = [{"k2","v2"}, {"k1","v1"}],
        timeout = 30000,
        ibrowse_options = [{k2, v2}, {k1, v1}],
        retries = 10,
        http_connections = 20
    },
    Expected = HttpDb#httpdb{
        headers = [{"k1","v1"}, {"k2","v2"}],
        ibrowse_options = [{k1, v1}, {k2, v2}]
    },
    ?assertEqual(Expected, normalize_endpoint(HttpDb)),
    ?assertEqual(<<"local">>, normalize_endpoint(<<"local">>)).


-endif.
