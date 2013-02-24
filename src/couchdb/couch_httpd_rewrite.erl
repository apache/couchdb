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

-module(couch_httpd_rewrite).
-export([handle_rewrite_req/3]).
-include("couch_db.hrl").

-define(SEPARATOR, $\/).
-define(MATCH_ALL, {bind, <<"*">>}).


%% doc The http rewrite handler. All rewriting is done from
%% /dbname/_design/ddocname/_rewrite by default.
%%
%% each rules should be in rewrites member of the design doc.
%% Ex of a complete rule :
%%
%%  {
%%      ....
%%      "rewrites": [
%%      {
%%          "from": "",
%%          "to": "index.html",
%%          "method": "GET",
%%          "query": {}
%%      }
%%      ]
%%  }
%%
%%  from: is the path rule used to bind current uri to the rule. It
%% use pattern matching for that.
%%
%%  to: rule to rewrite an url. It can contain variables depending on binding
%% variables discovered during pattern matching and query args (url args and from
%% the query member.)
%%
%%  method: method to bind the request method to the rule. by default "*"
%%  query: query args you want to define they can contain dynamic variable
%% by binding the key to the bindings
%%
%%
%% to and from are path with  patterns. pattern can be string starting with ":" or
%% "*". ex:
%% /somepath/:var/*
%%
%% This path is converted in erlang list by splitting "/". Each var are
%% converted in atom. "*" is converted to '*' atom. The pattern matching is done
%% by splitting "/" in request url in a list of token. A string pattern will
%% match equal token. The star atom ('*' in single quotes) will match any number
%% of tokens, but may only be present as the last pathtern in a pathspec. If all
%% tokens are matched and all pathterms are used, then the pathspec matches. It works
%% like webmachine. Each identified token will be reused in to rule and in query
%%
%% The pattern matching is done by first matching the request method to a rule. by
%% default all methods match a rule. (method is equal to "*" by default). Then
%% It will try to match the path to one rule. If no rule match, then a 404 error
%% is displayed.
%%
%% Once a rule is found we rewrite the request url using the "to" and
%% "query" members. The identified token are matched to the rule and
%% will replace var. if '*' is found in the rule it will contain the remaining
%% part if it exists.
%%
%% Examples:
%%
%% Dispatch rule            URL             TO                  Tokens
%%
%% {"from": "/a/b",         /a/b?k=v        /some/b?k=v         var =:= b
%% "to": "/some/"}                                              k = v
%%
%% {"from": "/a/b",         /a/b            /some/b?var=b       var =:= b
%% "to": "/some/:var"}
%%
%% {"from": "/a",           /a              /some
%% "to": "/some/*"}
%%
%% {"from": "/a/*",         /a/b/c          /some/b/c
%% "to": "/some/*"}
%%
%% {"from": "/a",           /a              /some
%% "to": "/some/*"}
%%
%% {"from": "/a/:foo/*",    /a/b/c          /some/b/c?foo=b     foo =:= b
%% "to": "/some/:foo/*"}
%%
%% {"from": "/a/:foo",     /a/b             /some/?k=b&foo=b    foo =:= b
%% "to": "/some",
%%  "query": {
%%      "k": ":foo"
%%  }}
%%
%% {"from": "/a",           /a?foo=b        /some/b             foo =:= b
%% "to": "/some/:foo",
%%  }}



handle_rewrite_req(#httpd{
        path_parts=[DbName, <<"_design">>, DesignName, _Rewrite|PathParts],
        method=Method,
        mochi_req=MochiReq}=Req, _Db, DDoc) ->

    % we are in a design handler
    DesignId = <<"_design/", DesignName/binary>>,
    Prefix = <<"/", DbName/binary, "/", DesignId/binary>>,
    QueryList = lists:map(fun decode_query_value/1, couch_httpd:qs(Req)),

     RewritesSoFar = erlang:get(?REWRITE_COUNT),
     MaxRewrites = list_to_integer(couch_config:get("httpd", "rewrite_limit", "100")),
     case RewritesSoFar >= MaxRewrites of
         true ->
             throw({bad_request, <<"Exceeded rewrite recursion limit">>});
         false ->
             erlang:put(?REWRITE_COUNT, RewritesSoFar + 1)
    end,
    #doc{body={Props}} = DDoc,

    % get rules from ddoc
    case couch_util:get_value(<<"rewrites">>, Props) of
        undefined ->
            couch_httpd:send_error(Req, 404, <<"rewrite_error">>,
                <<"Invalid path.">>);
        Bin when is_binary(Bin) ->
            couch_httpd:send_error(Req, 400, <<"rewrite_error">>,
                <<"Rewrite rules are a String. They must be a JSON Array.">>);
        Rules ->
            % create dispatch list from rules
            DispatchList =  [make_rule(Rule) || {Rule} <- Rules],
            Method1 = couch_util:to_binary(Method),

            %% get raw path by matching url to a rule.
            RawPath = case try_bind_path(DispatchList, Method1, 
                    PathParts, QueryList) of
                no_dispatch_path ->
                    throw(not_found);
                {NewPathParts, Bindings} ->
                    Parts = [quote_plus(X) || X <- NewPathParts],

                    % build new path, reencode query args, eventually convert
                    % them to json
                    Bindings1 = maybe_encode_bindings(Bindings),
                    Path = binary_to_list(
                        iolist_to_binary([
                                string:join(Parts, [?SEPARATOR]),
                                [["?", mochiweb_util:urlencode(Bindings1)] 
                                    || Bindings1 =/= [] ]
                            ])),
                    
                    % if path is relative detect it and rewrite path
                    case mochiweb_util:safe_relative_path(Path) of
                        undefined ->
                            ?b2l(Prefix) ++ "/" ++ Path;
                        P1 ->
                            ?b2l(Prefix) ++ "/" ++ P1
                    end

                end,

            % normalize final path (fix levels "." and "..")
            RawPath1 = ?b2l(iolist_to_binary(normalize_path(RawPath))),

            % In order to do OAuth correctly, we have to save the
            % requested path. We use default so chained rewriting
            % wont replace the original header.
            Headers = mochiweb_headers:default("x-couchdb-requested-path",
                                             MochiReq:get(raw_path),
                                             MochiReq:get(headers)),

            ?LOG_DEBUG("rewrite to ~p ~n", [RawPath1]),

            % build a new mochiweb request
            MochiReq1 = mochiweb_request:new(MochiReq:get(socket),
                                             MochiReq:get(method),
                                             RawPath1,
                                             MochiReq:get(version),
                                             Headers),

            % cleanup, It force mochiweb to reparse raw uri.
            MochiReq1:cleanup(),

            #httpd{
                db_url_handlers = DbUrlHandlers,
                design_url_handlers = DesignUrlHandlers,
                default_fun = DefaultFun,
                url_handlers = UrlHandlers,
                user_ctx = UserCtx
            } = Req,
            erlang:put(pre_rewrite_user_ctx, UserCtx),
            couch_httpd:handle_request_int(MochiReq1, DefaultFun,
                    UrlHandlers, DbUrlHandlers, DesignUrlHandlers)
        end.

quote_plus({bind, X}) ->
    mochiweb_util:quote_plus(X);
quote_plus(X) ->
    mochiweb_util:quote_plus(X).

%% @doc Try to find a rule matching current url. If none is found
%% 404 error not_found is raised
try_bind_path([], _Method, _PathParts, _QueryList) ->
    no_dispatch_path;
try_bind_path([Dispatch|Rest], Method, PathParts, QueryList) ->
    [{PathParts1, Method1}, RedirectPath, QueryArgs, Formats] = Dispatch,
    case bind_method(Method1, Method) of
        true ->
            case bind_path(PathParts1, PathParts, []) of
                {ok, Remaining, Bindings} ->
                    Bindings1 = Bindings ++ QueryList,
                    % we parse query args from the rule and fill
                    % it eventually with bindings vars
                    QueryArgs1 = make_query_list(QueryArgs, Bindings1,
                        Formats, []),
                    % remove params in QueryLists1 that are already in
                    % QueryArgs1
                    Bindings2 = lists:foldl(fun({K, V}, Acc) ->
                        K1 = to_binding(K),
                        KV = case couch_util:get_value(K1, QueryArgs1) of
                            undefined -> [{K1, V}];
                            _V1 -> []
                        end,
                        Acc ++ KV
                    end, [], Bindings1),

                    FinalBindings = Bindings2 ++ QueryArgs1,
                    NewPathParts = make_new_path(RedirectPath, FinalBindings,
                                    Remaining, []),
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
make_query_list([{Key, {Value}}|Rest], Bindings, Formats, Acc) ->
    Value1 = {Value},
    make_query_list(Rest, Bindings, Formats, [{to_binding(Key), Value1}|Acc]);
make_query_list([{Key, Value}|Rest], Bindings, Formats, Acc) when is_binary(Value) ->
    Value1 = replace_var(Value, Bindings, Formats),
    make_query_list(Rest, Bindings, Formats, [{to_binding(Key), Value1}|Acc]);
make_query_list([{Key, Value}|Rest], Bindings, Formats, Acc) when is_list(Value) ->
    Value1 = replace_var(Value, Bindings, Formats),
    make_query_list(Rest, Bindings, Formats, [{to_binding(Key), Value1}|Acc]);
make_query_list([{Key, Value}|Rest], Bindings, Formats, Acc) ->
    make_query_list(Rest, Bindings, Formats, [{to_binding(Key), Value}|Acc]).

replace_var(<<"*">>=Value, Bindings, Formats) ->
    get_var(Value, Bindings, Value, Formats);
replace_var(<<":", Var/binary>> = Value, Bindings, Formats) ->
    get_var(Var, Bindings, Value, Formats);
replace_var(Value, _Bindings, _Formats) when is_binary(Value) ->
    Value;
replace_var(Value, Bindings, Formats) when is_list(Value) ->
    lists:reverse(lists:foldl(fun
                (<<":", Var/binary>>=Value1, Acc) ->
                    [get_var(Var, Bindings, Value1, Formats)|Acc];
                (Value1, Acc) ->
                    [Value1|Acc]
            end, [], Value));
replace_var(Value, _Bindings, _Formats) ->
    Value.
                    
maybe_json(Key, Value) ->
    case lists:member(Key, [<<"key">>, <<"startkey">>, <<"start_key">>,
                <<"endkey">>, <<"end_key">>, <<"keys">>]) of
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
make_new_path([?MATCH_ALL|_Rest], _Bindings, Remaining, Acc) ->
    Acc1 = lists:reverse(Acc) ++ Remaining,
    Acc1;
make_new_path([{bind, P}|Rest], Bindings, Remaining, Acc) ->
    P2 = case couch_util:get_value({bind, P}, Bindings) of
        undefined -> << "undefined">>;
        P1 -> 
            iolist_to_binary(P1)
    end,
    make_new_path(Rest, Bindings, Remaining, [P2|Acc]);
make_new_path([P|Rest], Bindings, Remaining, Acc) ->
    make_new_path(Rest, Bindings, Remaining, [P|Acc]).


%% @doc If method of the query fith the rule method. If the
%% method rule is '*', which is the default, all
%% request method will bind. It allows us to make rules
%% depending on HTTP method.
bind_method(?MATCH_ALL, _Method ) ->
    true;
bind_method({bind, Method}, Method) ->
    true;
bind_method(_, _) ->
    false.


%% @doc bind path. Using the rule from we try to bind variables given
%% to the current url by pattern matching
bind_path([], [], Bindings) ->
    {ok, [], Bindings};
bind_path([?MATCH_ALL], [Match|_RestMatch]=Rest, Bindings) ->
    {ok, Rest, [{?MATCH_ALL, Match}|Bindings]};
bind_path(_, [], _) ->
    fail;
bind_path([{bind, Token}|RestToken],[Match|RestMatch],Bindings) ->
    bind_path(RestToken, RestMatch, [{{bind, Token}, Match}|Bindings]);
bind_path([Token|RestToken], [Token|RestMatch], Bindings) ->
    bind_path(RestToken, RestMatch, Bindings);
bind_path(_, _, _) ->
    fail.


%% normalize path.
normalize_path(Path)  ->
    "/" ++ string:join(normalize_path1(string:tokens(Path,
                "/"), []), [?SEPARATOR]).


normalize_path1([], Acc) ->
    lists:reverse(Acc);
normalize_path1([".."|Rest], Acc) ->
    Acc1 = case Acc of
        [] -> [".."|Acc];
        [T|_] when T =:= ".." -> [".."|Acc];
        [_|R] -> R
    end,
    normalize_path1(Rest, Acc1);
normalize_path1(["."|Rest], Acc) ->
    normalize_path1(Rest, Acc);
normalize_path1([Path|Rest], Acc) ->
    normalize_path1(Rest, [Path|Acc]).


%% @doc transform json rule in erlang for pattern matching
make_rule(Rule) ->
    Method = case couch_util:get_value(<<"method">>, Rule) of
        undefined -> ?MATCH_ALL;
        M -> to_binding(M)
    end,
    QueryArgs = case couch_util:get_value(<<"query">>, Rule) of
        undefined -> [];
        {Args} -> Args
        end,
    FromParts  = case couch_util:get_value(<<"from">>, Rule) of
        undefined -> [?MATCH_ALL];
        From ->
            parse_path(From)
        end,
    ToParts  = case couch_util:get_value(<<"to">>, Rule) of
        undefined ->
            throw({error, invalid_rewrite_target});
        To ->
            parse_path(To)
        end,
    Formats = case couch_util:get_value(<<"formats">>, Rule) of
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
path_to_list([<<>>|R], Acc, DotDotCount) ->
    path_to_list(R, Acc, DotDotCount);
path_to_list([<<"*">>|R], Acc, DotDotCount) ->
    path_to_list(R, [?MATCH_ALL|Acc], DotDotCount);
path_to_list([<<"..">>|R], Acc, DotDotCount) when DotDotCount == 2 ->
    case couch_config:get("httpd", "secure_rewrites", "true") of
    "false" ->
        path_to_list(R, [<<"..">>|Acc], DotDotCount+1);
    _Else ->
        ?LOG_INFO("insecure_rewrite_rule ~p blocked", [lists:reverse(Acc) ++ [<<"..">>] ++ R]),
        throw({insecure_rewrite_rule, "too many ../.. segments"})
    end;
path_to_list([<<"..">>|R], Acc, DotDotCount) ->
    path_to_list(R, [<<"..">>|Acc], DotDotCount+1);
path_to_list([P|R], Acc, DotDotCount) ->
    P1 = case P of
        <<":", Var/binary>> ->
            to_binding(Var);
        _ -> P
    end,
    path_to_list(R, [P1|Acc], DotDotCount).

maybe_encode_bindings([]) ->
    [];
maybe_encode_bindings(Props) -> 
    lists:foldl(fun 
            ({{bind, <<"*">>}, _V}, Acc) ->
                Acc;
            ({{bind, K}, V}, Acc) ->
                V1 = iolist_to_binary(maybe_json(K, V)),
                [{K, V1}|Acc]
        end, [], Props).
                
decode_query_value({K,V}) ->
    case lists:member(K, ["key", "startkey", "start_key",
                "endkey", "end_key", "keys"]) of
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
