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


maybe_add_warning(UserFun, #cursor{index = Index, opts = Opts}, UserAcc) ->
    NoIndexWarning = case Index#idx.type of
        <<"special">> ->
            <<"no matching index found, create an index to optimize query time">>;
        _ ->
            ok
    end,

    UseIndexInvalidWarning = case lists:keyfind(use_index, 1, Opts) of
        {use_index, []} ->
            NoIndexWarning;
        {use_index, [DesignId]} ->
            case filter_indexes([Index], DesignId) of
                [] ->
                    fmt("_design/~s was not used because it does not contain a valid index for this query.", 
                        [ddoc_name(DesignId)]);
                _ ->
                    NoIndexWarning
            end;
        {use_index, [DesignId, ViewName]} ->
            case filter_indexes([Index], DesignId, ViewName) of
                [] ->
                    fmt("_design/~s, ~s was not used because it is not a valid index for this query.", 
                        [ddoc_name(DesignId), ViewName]);
                _ ->
                    NoIndexWarning
            end
    end,

    maybe_add_warning_int(UseIndexInvalidWarning, UserFun, UserAcc).


maybe_add_warning_int(ok, _, UserAcc) ->
   UserAcc;

maybe_add_warning_int(Warning, UserFun, UserAcc) ->
    Arg = {add_key, warning, Warning},
    {_Go, UserAcc0} = UserFun(Arg, UserAcc),
    UserAcc0.


fmt(Format, Args) ->
    iolist_to_binary(io_lib:format(Format, Args)).


ddoc_name(<<"_design/", Name/binary>>) ->
    Name;

ddoc_name(Name) ->
    Name.
