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
    map_docs/2
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

-type context_opts() :: #{
    db_name := db_name(),
    ddoc_id => ddoc_id(),
    language => language(),
    sig => sig(),
    lib => lib(),
    map_funs => map_funs(),
    api_mod => api_mod()
}.


-callback acquire_map_context(context_opts()) -> {ok, any()} | {error, any()}.
-callback release_map_context(context()) -> ok | {error, any()}.
-callback map_docs(context(), [doc()]) -> {ok, [result()]} | {error, any()}.


-spec acquire_map_context(
        db_name(),
        ddoc_id(),
        language(),
        sig(),
        lib(),
        map_funs()
    ) -> {ok, context()} | {error, any()}.
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
    {ok, Ctx} = ApiMod:acquire_map_context(CtxOpts),
    {ok, {ApiMod, Ctx}}.


-spec release_map_context(context()) -> ok | {error, any()}.
release_map_context({ApiMod, Ctx}) ->
    ApiMod:release_map_context(Ctx).


-spec map_docs(context(), [doc()]) -> {ok, result()} | {error, any()}.
map_docs({ApiMod, Ctx}, Docs) ->
    ApiMod:map_docs(Ctx, Docs).


get_api_mod(Language) when is_binary(Language) ->
    try
        LangStr = binary_to_list(Language),
        ModStr = config:get("couch_eval.languages", LangStr),
        if ModStr /= undefined -> ok; true ->
            erlang:error({unknown_eval_api_language, Language})
        end,
        list_to_existing_atom(ModStr)
    catch error:badarg ->
        erlang:error({invalid_eval_api_mod, Language})
    end.
