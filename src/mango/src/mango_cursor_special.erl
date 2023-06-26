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

-module(mango_cursor_special).

-export([
    create/4,
    explain/1,
    execute/3
]).

-export([
    handle_message/2
]).

-include("mango.hrl").
-include("mango_cursor.hrl").

-spec create(Db, {Indexes, Trace}, Selector, Options) -> {ok, #cursor{}} when
    Db :: database(),
    Indexes :: [#idx{}],
    Trace :: trace(),
    Selector :: selector(),
    Options :: cursor_options().
create(Db, {Indexes, Trace0}, Selector, Opts) ->
    InitialRange = mango_idx_view:field_ranges(Selector),
    CatchAll = [{<<"_id">>, {'$gt', null, '$lt', mango_json_max}}],
    % order matters here - we only want to use the catchall index
    % if no other range can fulfill the query (because we know)
    % catchall is the most expensive range
    FieldRanges = InitialRange ++ CatchAll,
    Composited = mango_cursor_view:composite_indexes(Indexes, FieldRanges),
    {{Index, IndexRanges}, SortedIndexRanges} = mango_cursor_view:choose_best_index(Composited),

    Limit = couch_util:get_value(limit, Opts, mango_opts:default_limit()),
    Skip = couch_util:get_value(skip, Opts, 0),
    Fields = couch_util:get_value(fields, Opts, all_fields),
    Bookmark = couch_util:get_value(bookmark, Opts),

    IndexRanges1 = mango_cursor:maybe_noop_range(Selector, IndexRanges),
    Trace = maps:merge(Trace0, #{sorted_index_ranges => SortedIndexRanges}),

    {ok, #cursor{
        db = Db,
        index = Index,
        ranges = IndexRanges1,
        trace = Trace,
        selector = Selector,
        opts = Opts,
        limit = Limit,
        skip = Skip,
        fields = Fields,
        bookmark = Bookmark
    }}.

explain(Cursor) ->
    mango_cursor_view:explain(Cursor).

execute(Cursor0, UserFun, UserAcc) ->
    mango_cursor_view:execute(Cursor0, UserFun, UserAcc).

handle_message(Msg, Cursor) ->
    mango_cursor_view:handle_message(Msg, Cursor).

-ifdef(TEST).
-include_lib("couch/include/couch_eunit.hrl").

create_test() ->
    Index = #idx{type = <<"special">>, def = all_docs},
    Indexes = [Index],
    Ranges = [{'$gt', null, '$lt', mango_json_max}],
    Trace = #{},
    Selector = {[]},
    Options = [{limit, limit}, {skip, skip}, {fields, fields}, {bookmark, bookmark}],
    Cursor =
        #cursor{
            db = db,
            index = Index,
            ranges = Ranges,
            selector = Selector,
            opts = Options,
            limit = limit,
            skip = skip,
            fields = fields,
            bookmark = bookmark,
            trace = #{sorted_index_ranges => [{Index, Ranges, 0}]}
        },
    ?assertEqual({ok, Cursor}, create(db, {Indexes, Trace}, Selector, Options)).

misc_test_() ->
    {
        foreach,
        fun() -> meck:new(mango_cursor_view) end,
        fun(_) -> meck:unload(mango_cursor_view) end,
        [
            ?TDEF_FE(t_explain),
            ?TDEF_FE(t_execute),
            ?TDEF_FE(t_handle_message)
        ]
    }.

t_explain(_) ->
    meck:expect(mango_cursor_view, explain, [cursor], meck:val(explain_cursor)),
    ?assertEqual(explain_cursor, explain(cursor)),
    ?assert(meck:called(mango_cursor_view, explain, '_')).

t_execute(_) ->
    meck:expect(mango_cursor_view, execute, [cursor, userfn, useracc], meck:val(execute_cursor)),
    ?assertEqual(execute_cursor, execute(cursor, userfn, useracc)),
    ?assert(meck:called(mango_cursor_view, execute, '_')).

t_handle_message(_) ->
    meck:expect(mango_cursor_view, handle_message, [message, cursor], meck:val(result)),
    ?assertEqual(result, handle_message(message, cursor)),
    ?assert(meck:called(mango_cursor_view, handle_message, '_')).
-endif.
