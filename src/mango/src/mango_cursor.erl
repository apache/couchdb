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

-module(mango_cursor).


-export([
    create/3,
    explain/1,
    execute/3,
    maybe_filter_indexes_by_ddoc/2,
    maybe_add_warning/3
]).


-include_lib("couch/include/couch_db.hrl").
-include("mango.hrl").
-include("mango_cursor.hrl").
-include("mango_idx.hrl").


-ifdef(HAVE_DREYFUS).
-define(CURSOR_MODULES, [
    mango_cursor_view,
    mango_cursor_text,
    mango_cursor_special
]).
-else.
-define(CURSOR_MODULES, [
    mango_cursor_view,
    mango_cursor_special
]).
-endif.

-define(SUPERVISOR, mango_cursor_sup).


create(Db, Selector0, Opts) ->
    Selector = mango_selector:normalize(Selector0),
    UsableIndexes = mango_idx:get_usable_indexes(Db, Selector0, Opts),

    {use_index, IndexSpecified} = proplists:lookup(use_index, Opts),
    case {length(UsableIndexes), length(IndexSpecified)} of
        {0, 1} ->
            ?MANGO_ERROR({no_usable_index, selector_unsupported});
        {0, 0} ->
            AllDocs = mango_idx:special(Db),
            create_cursor(Db, AllDocs, Selector, Opts);
        _ ->
            create_cursor(Db, UsableIndexes, Selector, Opts)
    end.


explain(#cursor{}=Cursor) ->
    #cursor{
        index = Idx,
        selector = Selector,
        opts = Opts0,
        limit = Limit,
        skip = Skip,
        fields = Fields
    } = Cursor,
    Mod = mango_idx:cursor_mod(Idx),
    Opts = lists:keydelete(user_ctx, 1, Opts0),
    {[
        {dbname, mango_idx:dbname(Idx)},
        {index, mango_idx:to_json(Idx)},
        {selector, Selector},
        {opts, {Opts}},
        {limit, Limit},
        {skip, Skip},
        {fields, Fields}
    ] ++ Mod:explain(Cursor)}.


execute(#cursor{index=Idx}=Cursor, UserFun, UserAcc) ->
    Mod = mango_idx:cursor_mod(Idx),
    Mod:execute(Cursor, UserFun, UserAcc).


maybe_filter_indexes_by_ddoc(Indexes, Opts) ->
    case lists:keyfind(use_index, 1, Opts) of
        {use_index, []} ->
            %We remove any indexes that have a selector 
            % since they are only used when specified via use_index
            remove_indexes_with_selector(Indexes);
        {use_index, [DesignId]} ->
            filter_indexes(Indexes, DesignId);
        {use_index, [DesignId, ViewName]} ->
            filter_indexes(Indexes, DesignId, ViewName)
    end.


filter_indexes(Indexes, DesignId0) ->
    DesignId = case DesignId0 of
        <<"_design/", _/binary>> ->
            DesignId0;
        Else ->
            <<"_design/", Else/binary>>
    end,
    FiltFun = fun(I) -> mango_idx:ddoc(I) == DesignId end,
    lists:filter(FiltFun, Indexes).


filter_indexes(Indexes0, DesignId, ViewName) ->
    Indexes = filter_indexes(Indexes0, DesignId),
    FiltFun = fun(I) -> mango_idx:name(I) == ViewName end,
    lists:filter(FiltFun, Indexes).


remove_indexes_with_selector(Indexes) ->
    FiltFun = fun(Idx) -> 
        case mango_idx:get_idx_selector(Idx) of
            undefined -> true;
            _ -> false
        end
    end,
    lists:filter(FiltFun, Indexes).


create_cursor(Db, Indexes, Selector, Opts) ->
    [{CursorMod, CursorModIndexes} | _] = group_indexes_by_type(Indexes),
    CursorMod:create(Db, CursorModIndexes, Selector, Opts).


group_indexes_by_type(Indexes) ->
    IdxDict = lists:foldl(fun(I, D) ->
        dict:append(mango_idx:cursor_mod(I), I, D)
    end, dict:new(), Indexes),
    % The first cursor module that has indexes will be
    % used to service this query. This is so that we
    % don't suddenly switch indexes for existing client
    % queries.
    lists:flatmap(fun(CMod) ->
        case dict:find(CMod, IdxDict) of
            {ok, CModIndexes} ->
                [{CMod, CModIndexes}];
            error ->
                []
        end
    end, ?CURSOR_MODULES).


maybe_add_warning(UserFun, #idx{type = IndexType}, UserAcc) ->
    case IndexType of
        <<"special">> ->
            Arg = {add_key, warning, <<"no matching index found, create an index to optimize query time">>},
            {_Go, UserAcc0} = UserFun(Arg, UserAcc),
            UserAcc0;
        _ ->
            UserAcc
    end.