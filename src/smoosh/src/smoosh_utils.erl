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

-module(smoosh_utils).
-include_lib("couch/include/couch_db.hrl").

-export([get/2, get/3, split/1, stringify/1, ignore_db/1]).
-export([db_channels/0, view_channels/0, cleanup_channels/0]).
-export([in_allowed_window/1]).
-export([concurrency/1, capacity/1]).
-export([log_level/2]).
-export([validate_arg/1]).

-define(BUILT_IN_DB_CHANNELS, "upgrade_dbs,ratio_dbs,slack_dbs").
-define(BUILT_IN_VIEW_CHANNELS, "upgrade_views,ratio_views,slack_views").
-define(BUILT_IN_CLEANUP_CHANNELS, "index_cleanup").

-define(INDEX_CLEANUP, index_cleanup).

-define(DEFAULT_CONCURRENCY, "1").
-define(DEFAULT_CAPACITY, "9999").

get(Channel, Key) ->
    ?MODULE:get(Channel, Key, undefined).

get(Channel, Key, Default) ->
    config:get("smoosh." ++ Channel, Key, Default).

split(undefined) ->
    [];
split("") ->
    [];
split(CSV) ->
    re:split(CSV, "\\s*,\\s*", [{return, list}, trim]).

stringify({?INDEX_CLEANUP, DbName}) ->
    io_lib:format("~s index_cleanup", [DbName]);
stringify({DbName, GroupId}) ->
    io_lib:format("~s ~s", [DbName, GroupId]);
stringify(DbName) ->
    io_lib:format("~s", [DbName]).

ignore_db({?INDEX_CLEANUP, DbName}) ->
    ignore_db(DbName);
ignore_db({DbName, _GroupName}) ->
    ignore_db(DbName);
ignore_db(DbName) when is_binary(DbName) ->
    ignore_db(?b2l(DbName));
ignore_db(DbName) when is_list(DbName) ->
    case config:get("smoosh.ignore", DbName, false) of
        "true" ->
            true;
        _ ->
            false
    end.

in_allowed_window(Channel) ->
    From = parse_time(?MODULE:get(Channel, "from"), {00, 00}),
    To = parse_time(?MODULE:get(Channel, "to"), {24, 00}),
    in_allowed_window(From, To).

in_allowed_window(From, To) ->
    {_, {HH, MM, _}} = calendar:universal_time(),
    case From < To of
        true ->
            ({HH, MM} >= From) andalso ({HH, MM} < To);
        false ->
            ({HH, MM} >= From) orelse ({HH, MM} < To)
    end.

parse_time(undefined, Default) ->
    Default;
parse_time(String, Default) ->
    case string:tokens(String, ":") of
        [HH, MM] ->
            try
                {list_to_integer(HH), list_to_integer(MM)}
            catch
                error:badarg ->
                    couch_log:error("Malformed compaction schedule configuration: ~s", [String]),
                    Default
            end;
        _Else ->
            couch_log:error("Malformed compaction schedule configuration: ~s", [String]),
            Default
    end.

log_level(Key, Default) when is_list(Key), is_list(Default) ->
    list_to_existing_atom(config:get("smoosh", Key, Default)).

db_channels() ->
    ConfStr = config:get("smoosh", "db_channels"),
    channel_list(ConfStr, ?BUILT_IN_DB_CHANNELS).

view_channels() ->
    Conf = config:get("smoosh", "view_channels"),
    channel_list(Conf, ?BUILT_IN_VIEW_CHANNELS).

cleanup_channels() ->
    Conf = config:get("smoosh", "cleanup_channels"),
    channel_list(Conf, ?BUILT_IN_CLEANUP_CHANNELS).

channel_list(ConfStr, Default) ->
    DefaultList = split(Default),
    ConfList = split(ConfStr),
    lists:usort(DefaultList ++ ConfList).

concurrency(ChannelName) ->
    list_to_integer(?MODULE:get(ChannelName, "concurrency", ?DEFAULT_CONCURRENCY)).

capacity(ChannelName) ->
    list_to_integer(?MODULE:get(ChannelName, "capacity", ?DEFAULT_CAPACITY)).

% Validate enqueue arg at the front of the API instead of adding ?l2b and ?b2l
% everywhere internally
%
validate_arg({?INDEX_CLEANUP, DbName}) when is_list(DbName) ->
    validate_arg({?INDEX_CLEANUP, ?l2b(DbName)});
validate_arg(DbName) when is_list(DbName) ->
    validate_arg(?l2b(DbName));
validate_arg({DbName, GroupId}) when is_list(DbName) ->
    validate_arg({?l2b(DbName), GroupId});
validate_arg({DbName, GroupId}) when is_list(GroupId) ->
    validate_arg({DbName, ?l2b(GroupId)});
validate_arg(DbName) when is_binary(DbName) ->
    DbName;
validate_arg({DbName, GroupId}) when is_binary(DbName), is_binary(GroupId) ->
    {DbName, GroupId};
validate_arg({?INDEX_CLEANUP, DbName}) when is_binary(DbName) ->
    {?INDEX_CLEANUP, DbName};
validate_arg(_) ->
    error(invalid_smoosh_arg).

-ifdef(TEST).

-include_lib("couch/include/couch_eunit.hrl").

smoosh_util_validate_test() ->
    ?assertEqual(<<"x">>, validate_arg(<<"x">>)),
    ?assertEqual(<<"x">>, validate_arg("x")),
    ?assertEqual({<<"x">>, <<"y">>}, validate_arg({"x", "y"})),
    ?assertEqual({<<"x">>, <<"y">>}, validate_arg({<<"x">>, "y"})),
    ?assertEqual({<<"x">>, <<"y">>}, validate_arg({"x", <<"y">>})),
    ?assertEqual({<<"x">>, <<"y">>}, validate_arg({<<"x">>, <<"y">>})),
    ?assertEqual({?INDEX_CLEANUP, <<"x">>}, validate_arg({?INDEX_CLEANUP, "x"})),
    ?assertEqual({?INDEX_CLEANUP, <<"x">>}, validate_arg({?INDEX_CLEANUP, <<"x">>})),
    ?assertError(invalid_smoosh_arg, validate_arg(foo)),
    ?assertError(invalid_smoosh_arg, validate_arg({foo, bar})),
    ?assertError(invalid_smoosh_arg, validate_arg({?INDEX_CLEANUP, foo})).

smoosh_utils_test_() ->
    {
        foreach,
        fun() ->
            meck:new(calendar, [passthrough, unstick]),
            meck:expect(config, get, fun(_, _, Default) -> Default end)
        end,
        fun(_) ->
            meck:unload()
        end,
        [
            ?TDEF_FE(t_channel_list_default),
            ?TDEF_FE(t_channel_list_configured),
            ?TDEF_FE(t_capacity),
            ?TDEF_FE(t_concurrency),
            ?TDEF_FE(t_stringify),
            ?TDEF_FE(t_ignore_db),
            ?TDEF_FE(t_log_level),
            ?TDEF_FE(t_allowed_window)
        ]
    }.

t_channel_list_default(_) ->
    ?assertEqual(
        ["ratio_dbs", "slack_dbs", "upgrade_dbs"],
        db_channels()
    ),
    ?assertEqual(
        ["ratio_views", "slack_views", "upgrade_views"],
        view_channels()
    ),
    ?assertEqual(
        ["index_cleanup"],
        cleanup_channels()
    ).

t_channel_list_configured(_) ->
    meck_chans("db_channels", "foo_dbs"),
    ?assertEqual(
        ["foo_dbs", "ratio_dbs", "slack_dbs", "upgrade_dbs"],
        db_channels()
    ),
    meck_chans("view_channels", "foo_views"),
    ?assertEqual(
        ["foo_views", "ratio_views", "slack_views", "upgrade_views"],
        view_channels()
    ),
    meck_chans("cleanup_channels", "foo_cleanup"),
    ?assertEqual(
        ["foo_cleanup", "index_cleanup"],
        cleanup_channels()
    ),
    meck_chans("db_channels", undefined),
    ?assertEqual(
        ["ratio_dbs", "slack_dbs", "upgrade_dbs"],
        db_channels()
    ),
    meck_chans("db_channels", ""),
    ?assertEqual(
        ["ratio_dbs", "slack_dbs", "upgrade_dbs"],
        db_channels()
    ).

t_capacity(_) ->
    ?assertEqual(9999, capacity("foo")),
    meck:expect(config, get, fun("smoosh.foo", "capacity", _) -> "2" end),
    ?assertEqual(2, capacity("foo")).

t_concurrency(_) ->
    ?assertEqual(1, concurrency("foo")),
    meck:expect(config, get, fun("smoosh.foo", "concurrency", _) -> "2" end),
    ?assertEqual(2, concurrency("foo")).

t_stringify(_) ->
    Flat = fun(L) -> lists:flatten(L) end,
    ?assertEqual("x index_cleanup", Flat(stringify({?INDEX_CLEANUP, <<"x">>}))),
    ?assertEqual("x y", Flat(stringify({<<"x">>, <<"y">>}))),
    ?assertEqual("x", Flat(stringify(<<"x">>))).

t_ignore_db(_) ->
    ?assertNot(ignore_db(<<"x">>)),
    ?assertNot(ignore_db({<<"x">>, <<"z">>})),
    ?assertNot(ignore_db({?INDEX_CLEANUP, <<"x">>})),
    meck:expect(config, get, fun("smoosh.ignore", "x", _) -> "true" end),
    ?assert(ignore_db(<<"x">>)),
    ?assert(ignore_db({<<"x">>, <<"z">>})),
    ?assert(ignore_db({?INDEX_CLEANUP, <<"x">>})).

t_log_level(_) ->
    ?assertEqual(notice, log_level("compaction_log", "notice")),
    meck:expect(config, get, fun("smoosh", "compaction_log", _) -> "error" end),
    ?assertEqual(error, log_level("compaction_log", "notice")).

t_allowed_window(_) ->
    ?assert(in_allowed_window("baz")),

    meck_from_to("blah", undefined),
    ?assert(in_allowed_window("foo")),

    meck_from_to("X:Y", "9.5:Z"),
    ?assert(in_allowed_window("foo")),

    meck_time(3, 30),

    meck_from_to("02:30", "04:30"),
    ?assert(in_allowed_window("foo")),

    meck_from_to("3:35", "3:50"),
    ?assertNot(in_allowed_window("foo")),

    meck_from_to("1:20", "3:29"),
    ?assertNot(in_allowed_window("foo")),

    meck_from_to("23:30", "3:52"),
    ?assert(in_allowed_window("foo")),

    meck_from_to("23:30", "3:29"),
    ?assertNot(in_allowed_window("foo")).

meck_time(H, M) ->
    meck:expect(calendar, universal_time, 0, {{2000, 10, 10}, {H, M, 42}}).

meck_from_to(From, To) ->
    meck:expect(config, get, fun
        ("smoosh.foo", "from", _) -> From;
        ("smoosh.foo", "to", _) -> To
    end).

meck_chans(Type, Channels) ->
    meck:expect(config, get, fun("smoosh", T, _) when T =:= Type -> Channels end).

-endif.
