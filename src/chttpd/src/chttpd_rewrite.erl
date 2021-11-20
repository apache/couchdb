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
%
% bind_path is based on bind method from Webmachine

%% @doc Module for URL rewriting by pattern matching.

-module(chttpd_rewrite).

-compile(tuple_calls).

-export([handle_rewrite_req/3]).
-include_lib("couch/include/couch_db.hrl").

-define(SEPARATOR, $\/).
-define(MATCH_ALL, {bind, <<"*">>}).

handle_rewrite_req(#httpd{} = Req, Db, DDoc) ->
    RewritesSoFar = erlang:get(?REWRITE_COUNT),
    MaxRewrites = chttpd_util:get_chttpd_config_integer("rewrite_limit", 100),
    case RewritesSoFar >= MaxRewrites of
        true ->
            throw({bad_request, <<"Exceeded rewrite recursion limit">>});
        false ->
            erlang:put(?REWRITE_COUNT, RewritesSoFar + 1)
    end,
    case get_rules(DDoc) of
        Rules when is_list(Rules) ->
            do_rewrite(Req, Rules);
        Rules when is_binary(Rules) ->
            case couch_query_servers:rewrite(Req, Db, DDoc) of
                undefined ->
                    chttpd:send_error(
                        Req,
                        404,
                        <<"rewrite_error">>,
                        <<"Invalid path.">>
                    );
                Rewrite ->
                    do_rewrite(Req, Rewrite)
            end;
        undefined ->
            chttpd:send_error(
                Req,
                404,
                <<"rewrite_error">>,
                <<"Invalid path.">>
            )
    end.

get_rules(#doc{body = {Props}}) ->
    couch_util:get_value(<<"rewrites">>, Props).

do_rewrite(#httpd{mochi_req = MochiReq} = Req, {Props} = Rewrite) when is_list(Props) ->
    case couch_util:get_value(<<"code">>, Props) of
        undefined ->
            Method = rewrite_method(Req, Rewrite),
            Headers = rewrite_headers(Req, Rewrite),
            Path = ?b2l(rewrite_path(Req, Rewrite)),
            NewMochiReq = mochiweb_request:new(
                MochiReq:get(socket),
                Method,
                Path,
                MochiReq:get(version),
                Headers
            ),
            Body =
                case couch_util:get_value(<<"body">>, Props) of
                    undefined -> erlang:get(mochiweb_request_body);
                    B -> B
                end,
            NewMochiReq:cleanup(),
            case Body of
                undefined -> [];
                _ -> erlang:put(mochiweb_request_body, Body)
            end,
            couch_log:debug("rewrite to ~p", [Path]),
            chttpd:handle_request_int(NewMochiReq);
        Code ->
            chttpd:send_response(
                Req,
                Code,
                case couch_util:get_value(<<"headers">>, Props) of
                    undefined -> [];
                    {H1} -> H1
                end,
                rewrite_body(Rewrite)
            )
    end;
do_rewrite(
    #httpd{
        method = Method,
        path_parts = [_DbName, <<"_design">>, _DesignName, _Rewrite | PathParts],
        mochi_req = MochiReq
    } = Req,
    Rules
) when is_list(Rules) ->
    % create dispatch list from rules
    Prefix = path_prefix(Req),
    QueryList = lists:map(fun decode_query_value/1, chttpd:qs(Req)),

    DispatchList = [make_rule(Rule) || {Rule} <- Rules],
    Method1 = couch_util:to_binary(Method),

    %% get raw path by matching url to a rule.
    RawPath =
        case
            try_bind_path(
                DispatchList,
                Method1,
                PathParts,
                QueryList
            )
        of
            no_dispatch_path ->
                throw(not_found);
            {NewPathParts, Bindings} ->
                Parts = [quote_plus(X) || X <- NewPathParts],

                % build new path, reencode query args, eventually convert
                % them to json
                Bindings1 = maybe_encode_bindings(Bindings),
                Path = iolist_to_binary([
                    string:join(Parts, [?SEPARATOR]),
                    [["?", mochiweb_util:urlencode(Bindings1)] || Bindings1 =/= []]
                ]),

                % if path is relative detect it and rewrite path
                safe_relative_path(Prefix, Path)
        end,

    % normalize final path (fix levels "." and "..")
    RawPath1 = ?b2l(normalize_path(RawPath)),

    couch_log:debug("rewrite to ~p ~n", [RawPath1]),

    % build a new mochiweb request
    MochiReq1 = mochiweb_request:new(
        MochiReq:get(socket),
        MochiReq:get(method),
        RawPath1,
        MochiReq:get(version),
        MochiReq:get(headers)
    ),

    % cleanup, It force mochiweb to reparse raw uri.
    MochiReq1:cleanup(),

    chttpd:handle_request_int(MochiReq1).

rewrite_method(#httpd{method = Method}, {Props}) ->
    DefaultMethod = couch_util:to_binary(Method),
    couch_util:get_value(<<"method">>, Props, DefaultMethod).

rewrite_path(#httpd{} = Req, {Props} = Rewrite) ->
    Prefix = path_prefix(Req),
    RewritePath =
        case couch_util:get_value(<<"path">>, Props) of
            undefined ->
                throw({<<"rewrite_error">>, <<"Rewrite result must produce a new path.">>});
            P ->
                P
        end,
    SafeRelativePath = safe_relative_path(Prefix, RewritePath),
    NormalizedPath = normalize_path(SafeRelativePath),
    QueryParams = rewrite_query_params(Req, Rewrite),
    case QueryParams of
        <<"">> ->
            NormalizedPath;
        QueryParams ->
            <<NormalizedPath/binary, "?", QueryParams/binary>>
    end.

rewrite_query_params(#httpd{} = Req, {Props}) ->
    RequestQS = chttpd:qs(Req),
    RewriteQS =
        case couch_util:get_value(<<"query">>, Props) of
            undefined -> RequestQS;
            {V} -> V
        end,
    RewriteQSEsc = [{chttpd:quote(K), chttpd:quote(V)} || {K, V} <- RewriteQS],
    iolist_to_binary(string:join([[K, "=", V] || {K, V} <- RewriteQSEsc], "&")).

rewrite_headers(#httpd{mochi_req = MochiReq}, {Props}) ->
    case couch_util:get_value(<<"headers">>, Props) of
        undefined ->
            MochiReq:get(headers);
        {H} ->
            mochiweb_headers:enter_from_list(
                lists:map(fun({Key, Val}) -> {?b2l(Key), ?b2l(Val)} end, H),
                MochiReq:get(headers)
            )
    end.

rewrite_body({Props}) ->
    Body =
        case couch_util:get_value(<<"body">>, Props) of
            undefined -> erlang:get(mochiweb_request_body);
            B -> B
        end,
    case Body of
        undefined ->
            [];
        _ ->
            erlang:put(mochiweb_request_body, Body),
            Body
    end.

path_prefix(#httpd{path_parts = [DbName, <<"_design">>, DesignName | _]}) ->
    EscapedDesignName = ?l2b(couch_util:url_encode(DesignName)),
    EscapedDbName = ?l2b(couch_util:url_encode(DbName)),
    DesignId = <<"_design/", EscapedDesignName/binary>>,
    <<"/", EscapedDbName/binary, "/", DesignId/binary>>.

safe_relative_path(Prefix, Path) ->
    case mochiweb_util:safe_relative_path(?b2l(Path)) of
        undefined ->
            <<Prefix/binary, "/", Path/binary>>;
        V0 ->
            V1 = ?l2b(V0),
            <<Prefix/binary, "/", V1/binary>>
    end.

quote_plus({bind, X}) ->
    mochiweb_util:quote_plus(X);
quote_plus(X) ->
    mochiweb_util:quote_plus(X).

%% @doc Try to find a rule matching current url. If none is found
%% 404 error not_found is raised
try_bind_path([], _Method, _PathParts, _QueryList) ->
    no_dispatch_path;
try_bind_path([Dispatch | Rest], Method, PathParts, QueryList) ->
    [{PathParts1, Method1}, RedirectPath, QueryArgs, Formats] = Dispatch,
    case bind_method(Method1, Method) of
        true ->
            case bind_path(PathParts1, PathParts, []) of
                {ok, Remaining, Bindings} ->
                    Bindings1 = Bindings ++ QueryList,
                    % we parse query args from the rule and fill
                    % it eventually with bindings vars
                    QueryArgs1 = make_query_list(
                        QueryArgs,
                        Bindings1,
                        Formats,
                        []
                    ),
                    % remove params in QueryLists1 that are already in
                    % QueryArgs1
                    Bindings2 = lists:foldl(
                        fun({K, V}, Acc) ->
                            K1 = to_binding(K),
                            KV =
                                case couch_util:get_value(K1, QueryArgs1) of
                                    undefined -> [{K1, V}];
                                    _V1 -> []
                                end,
                            Acc ++ KV
                        end,
                        [],
                        Bindings1
                    ),

                    FinalBindings = Bindings2 ++ QueryArgs1,
                    NewPathParts = make_new_path(
                        RedirectPath,
                        FinalBindings,
                        Remaining,
                        []
                    ),
                    {NewPathParts, FinalBindings};
                fail ->
                    try_bind_path(Rest, Method, PathParts, QueryList)
            end;
        false ->
            try_bind_path(Rest, Method, PathParts, QueryList)
    end.

%% rewriting dynamically the quey list given as query member in
%% rewrites. Each value is replaced by one binding or an argument
%% passed in url.
make_query_list([], _Bindings, _Formats, Acc) ->
    Acc;
make_query_list([{Key, {Value}} | Rest], Bindings, Formats, Acc) ->
    Value1 = {Value},
    make_query_list(Rest, Bindings, Formats, [{to_binding(Key), Value1} | Acc]);
make_query_list([{Key, Value} | Rest], Bindings, Formats, Acc) when is_binary(Value) ->
    Value1 = replace_var(Value, Bindings, Formats),
    make_query_list(Rest, Bindings, Formats, [{to_binding(Key), Value1} | Acc]);
make_query_list([{Key, Value} | Rest], Bindings, Formats, Acc) when is_list(Value) ->
    Value1 = replace_var(Value, Bindings, Formats),
    make_query_list(Rest, Bindings, Formats, [{to_binding(Key), Value1} | Acc]);
make_query_list([{Key, Value} | Rest], Bindings, Formats, Acc) ->
    make_query_list(Rest, Bindings, Formats, [{to_binding(Key), Value} | Acc]).

replace_var(<<"*">> = Value, Bindings, Formats) ->
    get_var(Value, Bindings, Value, Formats);
replace_var(<<":", Var/binary>> = Value, Bindings, Formats) ->
    get_var(Var, Bindings, Value, Formats);
replace_var(Value, _Bindings, _Formats) when is_binary(Value) ->
    Value;
replace_var(Value, Bindings, Formats) when is_list(Value) ->
    lists:reverse(
        lists:foldl(
            fun
                (<<":", Var/binary>> = Value1, Acc) ->
                    [get_var(Var, Bindings, Value1, Formats) | Acc];
                (Value1, Acc) ->
                    [Value1 | Acc]
            end,
            [],
            Value
        )
    );
replace_var(Value, _Bindings, _Formats) ->
    Value.

maybe_json(Key, Value) ->
    case
        lists:member(Key, [
            <<"key">>,
            <<"startkey">>,
            <<"start_key">>,
            <<"endkey">>,
            <<"end_key">>,
            <<"keys">>
        ])
    of
        true ->
            ?JSON_ENCODE(Value);
        false ->
            Value
    end.

get_var(VarName, Props, Default, Formats) ->
    VarName1 = to_binding(VarName),
    Val = couch_util:get_value(VarName1, Props, Default),
    maybe_format(VarName, Val, Formats).

maybe_format(VarName, Value, Formats) ->
    case couch_util:get_value(VarName, Formats) of
        undefined ->
            Value;
        Format ->
            format(Format, Value)
    end.

format(<<"int">>, Value) when is_integer(Value) ->
    Value;
format(<<"int">>, Value) when is_binary(Value) ->
    format(<<"int">>, ?b2l(Value));
format(<<"int">>, Value) when is_list(Value) ->
    case (catch list_to_integer(Value)) of
        IntVal when is_integer(IntVal) ->
            IntVal;
        _ ->
            Value
    end;
format(<<"bool">>, Value) when is_binary(Value) ->
    format(<<"bool">>, ?b2l(Value));
format(<<"bool">>, Value) when is_list(Value) ->
    case string:to_lower(Value) of
        "true" -> true;
        "false" -> false;
        _ -> Value
    end;
format(_Format, Value) ->
    Value.

%% doc: build new patch from bindings. bindings are query args
%% (+ dynamic query rewritten if needed) and bindings found in
%% bind_path step.
make_new_path([], _Bindings, _Remaining, Acc) ->
    lists:reverse(Acc);
make_new_path([?MATCH_ALL], _Bindings, Remaining, Acc) ->
    Acc1 = lists:reverse(Acc) ++ Remaining,
    Acc1;
make_new_path([?MATCH_ALL | _Rest], _Bindings, Remaining, Acc) ->
    Acc1 = lists:reverse(Acc) ++ Remaining,
    Acc1;
make_new_path([{bind, P} | Rest], Bindings, Remaining, Acc) ->
    P2 =
        case couch_util:get_value({bind, P}, Bindings) of
            undefined -> <<"undefined">>;
            P1 -> iolist_to_binary(P1)
        end,
    make_new_path(Rest, Bindings, Remaining, [P2 | Acc]);
make_new_path([P | Rest], Bindings, Remaining, Acc) ->
    make_new_path(Rest, Bindings, Remaining, [P | Acc]).

%% @doc If method of the query fith the rule method. If the
%% method rule is '*', which is the default, all
%% request method will bind. It allows us to make rules
%% depending on HTTP method.
bind_method(?MATCH_ALL, _Method) ->
    true;
bind_method({bind, Method}, Method) ->
    true;
bind_method(_, _) ->
    false.

%% @doc bind path. Using the rule from we try to bind variables given
%% to the current url by pattern matching
bind_path([], [], Bindings) ->
    {ok, [], Bindings};
bind_path([?MATCH_ALL], Rest, Bindings) when is_list(Rest) ->
    {ok, Rest, Bindings};
bind_path(_, [], _) ->
    fail;
bind_path([{bind, Token} | RestToken], [Match | RestMatch], Bindings) ->
    bind_path(RestToken, RestMatch, [{{bind, Token}, Match} | Bindings]);
bind_path([Token | RestToken], [Token | RestMatch], Bindings) ->
    bind_path(RestToken, RestMatch, Bindings);
bind_path(_, _, _) ->
    fail.

%% normalize path.
normalize_path(Path) when is_binary(Path) ->
    normalize_path(?b2l(Path));
normalize_path(Path) when is_list(Path) ->
    Segments = normalize_path1(string:tokens(Path, "/"), []),
    NormalizedPath = string:join(Segments, [?SEPARATOR]),
    iolist_to_binary(["/", NormalizedPath]).

normalize_path1([], Acc) ->
    lists:reverse(Acc);
normalize_path1([".." | Rest], Acc) ->
    Acc1 =
        case Acc of
            [] -> [".." | Acc];
            [T | _] when T =:= ".." -> [".." | Acc];
            [_ | R] -> R
        end,
    normalize_path1(Rest, Acc1);
normalize_path1(["." | Rest], Acc) ->
    normalize_path1(Rest, Acc);
normalize_path1([Path | Rest], Acc) ->
    normalize_path1(Rest, [Path | Acc]).

%% @doc transform json rule in erlang for pattern matching
make_rule(Rule) ->
    Method =
        case couch_util:get_value(<<"method">>, Rule) of
            undefined -> ?MATCH_ALL;
            M -> to_binding(M)
        end,
    QueryArgs =
        case couch_util:get_value(<<"query">>, Rule) of
            undefined -> [];
            {Args} -> Args
        end,
    FromParts =
        case couch_util:get_value(<<"from">>, Rule) of
            undefined -> [?MATCH_ALL];
            From -> parse_path(From)
        end,
    ToParts =
        case couch_util:get_value(<<"to">>, Rule) of
            undefined ->
                throw({error, invalid_rewrite_target});
            To ->
                parse_path(To)
        end,
    Formats =
        case couch_util:get_value(<<"formats">>, Rule) of
            undefined -> [];
            {Fmts} -> Fmts
        end,
    [{FromParts, Method}, ToParts, QueryArgs, Formats].

parse_path(Path) ->
    {ok, SlashRE} = re:compile(<<"\\/">>),
    path_to_list(re:split(Path, SlashRE), [], 0).

%% @doc convert a path rule (from or to) to an erlang list
%% * and path variable starting by ":" are converted
%% in erlang atom.
path_to_list([], Acc, _DotDotCount) ->
    lists:reverse(Acc);
path_to_list([<<>> | R], Acc, DotDotCount) ->
    path_to_list(R, Acc, DotDotCount);
path_to_list([<<"*">> | R], Acc, DotDotCount) ->
    path_to_list(R, [?MATCH_ALL | Acc], DotDotCount);
path_to_list([<<"..">> | R], Acc, DotDotCount) when DotDotCount == 2 ->
    case chttpd_util:get_chttpd_config_boolean("secure_rewrites", true) of
        false ->
            path_to_list(R, [<<"..">> | Acc], DotDotCount + 1);
        true ->
            couch_log:notice(
                "insecure_rewrite_rule ~p blocked",
                [lists:reverse(Acc) ++ [<<"..">>] ++ R]
            ),
            throw({insecure_rewrite_rule, "too many ../.. segments"})
    end;
path_to_list([<<"..">> | R], Acc, DotDotCount) ->
    path_to_list(R, [<<"..">> | Acc], DotDotCount + 1);
path_to_list([P | R], Acc, DotDotCount) ->
    P1 =
        case P of
            <<":", Var/binary>> ->
                to_binding(Var);
            _ ->
                P
        end,
    path_to_list(R, [P1 | Acc], DotDotCount).

maybe_encode_bindings([]) ->
    [];
maybe_encode_bindings(Props) ->
    lists:foldl(
        fun
            ({{bind, <<"*">>}, _V}, Acc) ->
                Acc;
            ({{bind, K}, V}, Acc) ->
                V1 = iolist_to_binary(maybe_json(K, V)),
                [{K, V1} | Acc]
        end,
        [],
        Props
    ).

decode_query_value({K, V}) ->
    case
        lists:member(K, [
            "key",
            "startkey",
            "start_key",
            "endkey",
            "end_key",
            "keys"
        ])
    of
        true ->
            {to_binding(K), ?JSON_DECODE(V)};
        false ->
            {to_binding(K), ?l2b(V)}
    end.

to_binding({bind, V}) ->
    {bind, V};
to_binding(V) when is_list(V) ->
    to_binding(?l2b(V));
to_binding(V) ->
    {bind, V}.
