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

-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").
-include("mango_cursor.hrl").

create(Db, Indexes, Selector, Opts) ->
    InitialRange = mango_idx_view:field_ranges(Selector),
    CatchAll = [{<<"_id">>, {'$gt', null, '$lt', mango_json_max}}],
    % order matters here - we only want to use the catchall index
    % if no other range can fulfill the query (because we know)
    % catchall is the most expensive range
    FieldRanges = InitialRange ++ CatchAll,
    Composited = mango_cursor_view:composite_indexes(Indexes, FieldRanges),
    {Index, IndexRanges} = mango_cursor_view:choose_best_index(Db, Composited),

    Limit = couch_util:get_value(limit, Opts, mango_opts:default_limit()),
    Skip = couch_util:get_value(skip, Opts, 0),
    Fields = couch_util:get_value(fields, Opts, all_fields),
    Bookmark = couch_util:get_value(bookmark, Opts),

    IndexRanges1 = mango_cursor:maybe_noop_range(Selector, IndexRanges),

    {ok, #cursor{
        db = Db,
        index = Index,
        ranges = IndexRanges1,
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
