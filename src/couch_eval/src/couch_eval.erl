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

-module(couch_eval).

-export([
    acquire_map_context/6,
    release_map_context/1,
    map_docs/2,
    with_context/2,
    try_compile/4
]).

-include_lib("couch/include/couch_db.hrl").

-type db_name() :: binary().
-type doc_id() :: binary().
-type ddoc_id() :: binary().
-type language() :: binary().
-type sig() :: binary().
-type lib() :: any().
-type map_fun() :: binary().
-type map_funs() :: [map_fun()].
-type result() :: {doc_id(), [[{any(), any()}]]}.
-type api_mod() :: atom().
-type context() :: {api_mod(), any()}.
-type function_type() :: binary() | atom().
-type function_name() :: binary().
-type function_src() :: binary().
-type error(_Error) :: no_return().

-type context_opts() :: #{
    db_name := db_name(),
    ddoc_id => ddoc_id(),
    language => language(),
    sig => sig(),
    lib => lib(),
    map_funs => map_funs(),
    api_mod => api_mod()
}.

-type with_context_opts() :: #{
    language := language()
}.

-callback acquire_map_context(context_opts()) -> {ok, any()} | {error, any()}.
-callback release_map_context(context()) -> ok | {error, any()}.
-callback map_docs(context(), [doc()]) -> {ok, [result()]} | {error, any()}.
-callback acquire_context() -> {ok, any()} | {error, any()}.
-callback release_context(context()) -> ok | {error, any()}.
-callback try_compile(context(), function_type(), function_name(), function_src()) -> ok.

-spec acquire_map_context(
    db_name(),
    ddoc_id(),
    language(),
    sig(),
    lib(),
    map_funs()
) ->
    {ok, context()}
    | error({invalid_eval_api_mod, Language :: binary()})
    | error({unknown_eval_api_language, Language :: binary()}).
acquire_map_context(DbName, DDocId, Language, Sig, Lib, MapFuns) ->
    ApiMod = get_api_mod(Language),
    CtxOpts = #{
        db_name => DbName,
        ddoc_id => DDocId,
        language => Language,
        sig => Sig,
        lib => Lib,
        map_funs => MapFuns
    },
    case ApiMod:acquire_map_context(CtxOpts) of
        {ok, Ctx} ->
            {ok, {ApiMod, Ctx}};
        {error, Error} ->
            {error, Error}
    end.

-spec release_map_context(context()) -> ok | {error, any()}.
release_map_context(nil) ->
    ok;
release_map_context({ApiMod, Ctx}) ->
    ApiMod:release_map_context(Ctx).

-spec map_docs(context(), [doc()]) -> {ok, result()} | {error, any()}.
map_docs({ApiMod, Ctx}, Docs) ->
    ApiMod:map_docs(Ctx, Docs).

-spec with_context(with_context_opts(), function()) ->
    any()
    | error({invalid_eval_api_mod, Language :: binary()})
    | error({unknown_eval_api_language, Language :: binary()}).
with_context(#{language := Language}, Fun) ->
    {ok, Ctx} = acquire_context(Language),
    try
        Fun(Ctx)
    after
        release_context(Ctx)
    end.

-spec try_compile(context(), function_type(), function_name(), function_src()) -> ok.
try_compile({_ApiMod, _Ctx}, reduce, <<_/binary>>, disabled) ->
    % Reduce functions may be disabled. Accept that as a valid configuration.
    ok;
try_compile({ApiMod, Ctx}, FuncType, FuncName, FuncSrc) ->
    ApiMod:try_compile(Ctx, FuncType, FuncName, FuncSrc).

acquire_context(Language) ->
    ApiMod = get_api_mod(Language),
    {ok, Ctx} = ApiMod:acquire_context(),
    {ok, {ApiMod, Ctx}}.

release_context(nil) ->
    ok;
release_context({ApiMod, Ctx}) ->
    ApiMod:release_context(Ctx).

get_api_mod(Language) when is_binary(Language) ->
    try
        LangStr = binary_to_list(Language),
        ModStr = config:get("couch_eval.languages", LangStr),
        if
            ModStr /= undefined -> ok;
            true -> erlang:error({unknown_eval_api_language, Language})
        end,
        list_to_existing_atom(ModStr)
    catch
        error:badarg ->
            erlang:error({invalid_eval_api_mod, Language})
    end.
