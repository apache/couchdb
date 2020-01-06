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
    remove_indexes_with_partial_filter_selector/1,
    maybe_add_warning/3,
    maybe_noop_range/2
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
    UsableIndexes = mango_idx:get_usable_indexes(Db, Selector, Opts),
    case mango_cursor:maybe_filter_indexes_by_ddoc(UsableIndexes, Opts) of
        [] ->
            % use_index doesn't match a valid index - fall back to a valid one
            create_cursor(Db, UsableIndexes, Selector, Opts);
        UserSpecifiedIndex ->
            create_cursor(Db, UserSpecifiedIndex, Selector, Opts)
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
        {partitioned, mango_idx:partitioned(Idx)},
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
            [];
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


remove_indexes_with_partial_filter_selector(Indexes) ->
    FiltFun = fun(Idx) ->
        case mango_idx:get_partial_filter_selector(Idx) of
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


% warn if the _all_docs index was used to fulfil a query
no_index_warning(#idx{type = Type}) when Type =:= <<"special">> ->
    couch_stats:increment_counter([mango, unindexed_queries]),
    [<<"No matching index found, create an index to optimize query time.">>];
no_index_warning(_) ->
    [].


% warn if user specified an index which doesn't exist or isn't valid
% for the selector.
% In this scenario, Mango will ignore the index hint and auto-select an index.
invalid_index_warning(Index, Opts) ->
    UseIndex = lists:keyfind(use_index, 1, Opts),
    invalid_index_warning_int(Index, UseIndex).


invalid_index_warning_int(Index, {use_index, [DesignId]}) ->
    case filter_indexes([Index], DesignId) of
        [] ->
            couch_stats:increment_counter([mango, query_invalid_index]),
            Reason = fmt("_design/~s was not used because it does not contain a valid index for this query.",
                [ddoc_name(DesignId)]),
            [Reason];
        _ ->
            []
    end;
invalid_index_warning_int(Index, {use_index, [DesignId, ViewName]}) ->
    case filter_indexes([Index], DesignId, ViewName) of
        [] ->
            couch_stats:increment_counter([mango, query_invalid_index]),
            Reason = fmt("_design/~s, ~s was not used because it is not a valid index for this query.",
                [ddoc_name(DesignId), ViewName]),
            [Reason];
        _ ->
            []
    end;
invalid_index_warning_int(_, _) ->
    [].


maybe_add_warning(UserFun, #cursor{index = Index, opts = Opts}, UserAcc) ->
    W0 = invalid_index_warning(Index, Opts),
    W1 = no_index_warning(Index),
    Warnings = lists:append([W0, W1]),
    case Warnings of
        [] ->
            UserAcc;
        _ ->
            WarningStr = lists:join(<<"\n">>, Warnings),
            Arg = {add_key, warning, WarningStr},
            {_Go, UserAcc1} = UserFun(Arg, UserAcc),
            UserAcc1
    end.

% When there is an empty array for certain operators, we don't actually
% want to execute the query so we deny it by making the range [empty].
% To clarify, we don't want this query to execute: {"$or": []}. Results should
% be empty. We do want this query to execute: {"age": 22, "$or": []}. It should
% return the same results as {"age": 22}
maybe_noop_range({[{Op, []}]}, IndexRanges) ->
    Noops = [<<"$all">>, <<"$and">>, <<"$or">>, <<"$in">>],
    case lists:member(Op, Noops) of
        true ->
            [empty];
        false ->
            IndexRanges
    end;
maybe_noop_range(_, IndexRanges) ->
    IndexRanges.


fmt(Format, Args) ->
    iolist_to_binary(io_lib:format(Format, Args)).


ddoc_name(<<"_design/", Name/binary>>) ->
    Name;

ddoc_name(Name) ->
    Name.
