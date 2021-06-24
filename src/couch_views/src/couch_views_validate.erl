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

-module(couch_views_validate).

-export([
    validate_args/1,
    validate_args/3,
    validate_ddoc/2
]).

-define(LOWEST_KEY, null).
-define(HIGHEST_KEY, {<<255, 255, 255, 255>>}).

-include_lib("couch/include/couch_db.hrl").
-include("couch_views.hrl").

% There is another almost identical validate_args in couch_views_util. They
% should probably be merged at some point in the future.
%
validate_args(Args) ->
    GroupLevel = determine_group_level(Args),
    Reduce = Args#mrargs.reduce,
    case Reduce == undefined orelse is_boolean(Reduce) of
        true -> ok;
        _ -> mrverror(<<"Invalid `reduce` value.">>)
    end,

    case {Args#mrargs.view_type, Reduce} of
        {map, true} -> mrverror(<<"Reduce is invalid for map-only views.">>);
        _ -> ok
    end,

    case {Args#mrargs.view_type, GroupLevel, Args#mrargs.keys} of
        {red, exact, _} ->
            ok;
        {red, _, KeyList} when is_list(KeyList) ->
            Msg = <<"Multi-key fetches for reduce views must use `group=true`">>,
            mrverror(Msg);
        _ ->
            ok
    end,

    case Args#mrargs.keys of
        Keys when is_list(Keys) -> ok;
        undefined -> ok;
        _ -> mrverror(<<"`keys` must be an array of strings.">>)
    end,

    case {Args#mrargs.keys, Args#mrargs.start_key, Args#mrargs.end_key} of
        {undefined, _, _} ->
            ok;
        {[], _, _} ->
            ok;
        {[_ | _], undefined, undefined} ->
            ok;
        _ ->
            mrverror(<<
                "`keys` is incompatible with `key`"
                ", `start_key` and `end_key`"
            >>)
    end,

    case Args#mrargs.start_key_docid of
        undefined -> ok;
        SKDocId0 when is_binary(SKDocId0) -> ok;
        _ -> mrverror(<<"`start_key_docid` must be a string.">>)
    end,

    case Args#mrargs.end_key_docid of
        undefined -> ok;
        EKDocId0 when is_binary(EKDocId0) -> ok;
        _ -> mrverror(<<"`end_key_docid` must be a string.">>)
    end,

    case Args#mrargs.direction of
        fwd -> ok;
        rev -> ok;
        _ -> mrverror(<<"Invalid direction.">>)
    end,

    case {Args#mrargs.limit >= 0, Args#mrargs.limit == undefined} of
        {true, _} -> ok;
        {_, true} -> ok;
        _ -> mrverror(<<"`limit` must be a positive integer.">>)
    end,

    case Args#mrargs.skip < 0 of
        true -> mrverror(<<"`skip` must be >= 0">>);
        _ -> ok
    end,

    case {Args#mrargs.view_type, GroupLevel} of
        {red, exact} -> ok;
        {_, 0} -> ok;
        {red, Int} when is_integer(Int), Int >= 0 -> ok;
        {red, _} -> mrverror(<<"`group_level` must be >= 0">>);
        {map, _} -> mrverror(<<"Invalid use of grouping on a map view.">>)
    end,

    case Args#mrargs.stable of
        true -> ok;
        false -> ok;
        _ -> mrverror(<<"Invalid value for `stable`.">>)
    end,

    case Args#mrargs.update of
        true -> ok;
        false -> ok;
        lazy -> ok;
        _ -> mrverror(<<"Invalid value for `update`.">>)
    end,

    case is_boolean(Args#mrargs.inclusive_end) of
        true -> ok;
        _ -> mrverror(<<"Invalid value for `inclusive_end`.">>)
    end,

    case {Args#mrargs.view_type, Args#mrargs.include_docs} of
        {red, true} -> mrverror(<<"`include_docs` is invalid for reduce">>);
        {_, ID} when is_boolean(ID) -> ok;
        _ -> mrverror(<<"Invalid value for `include_docs`">>)
    end,

    case {Args#mrargs.view_type, Args#mrargs.conflicts} of
        {_, undefined} -> ok;
        {map, V} when is_boolean(V) -> ok;
        {red, undefined} -> ok;
        {map, _} -> mrverror(<<"Invalid value for `conflicts`.">>);
        {red, _} -> mrverror(<<"`conflicts` is invalid for reduce views.">>)
    end,

    SKDocId =
        case {Args#mrargs.direction, Args#mrargs.start_key_docid} of
            {fwd, undefined} -> <<>>;
            {rev, undefined} -> <<255>>;
            {_, SKDocId1} -> SKDocId1
        end,

    EKDocId =
        case {Args#mrargs.direction, Args#mrargs.end_key_docid} of
            {fwd, undefined} -> <<255>>;
            {rev, undefined} -> <<>>;
            {_, EKDocId1} -> EKDocId1
        end,

    case is_boolean(Args#mrargs.sorted) of
        true -> ok;
        _ -> mrverror(<<"Invalid value for `sorted`.">>)
    end,

    Args#mrargs{
        start_key_docid = SKDocId,
        end_key_docid = EKDocId,
        group_level = GroupLevel
    }.

validate_args(Db, DDoc, Args0) ->
    {ok, State} = couch_views_util:ddoc_to_mrst(fabric2_db:name(Db), DDoc),
    Args1 = apply_limit(State#mrst.partitioned, Args0),
    validate_args(State, Args1).

validate_ddoc(#{} = Db, DDoc) ->
    DbName = fabric2_db:name(Db),
    IsPartitioned = fabric2_db:is_partitioned(Db),
    validate_ddoc(DbName, IsPartitioned, DDoc).

% Private functions

validate_ddoc(DbName, _IsDbPartitioned, DDoc) ->
    ok = validate_ddoc_fields(DDoc#doc.body),
    GetName = fun
        (#mrview{map_names = [Name | _]}) -> Name;
        (#mrview{reduce_funs = [{Name, _} | _]}) -> Name;
        (_) -> null
    end,
    ValidateView = fun(Ctx, #mrview{def = MapSrc, reduce_funs = Reds} = View) ->
        couch_eval:try_compile(Ctx, map, GetName(View), MapSrc),
        lists:foreach(
            fun
                ({_RedName, <<"_sum", _/binary>>}) ->
                    ok;
                ({_RedName, <<"_count", _/binary>>}) ->
                    ok;
                ({_RedName, <<"_stats", _/binary>>}) ->
                    ok;
                ({_RedName, <<"_approx_count_distinct", _/binary>>}) ->
                    ok;
                ({_RedName, <<"_", _/binary>> = Bad}) ->
                    Msg = ["`", Bad, "` is not a supported reduce function."],
                    throw({invalid_design_doc, Msg});
                ({RedName, RedSrc}) ->
                    couch_eval:try_compile(Ctx, reduce, RedName, RedSrc)
            end,
            Reds
        )
    end,
    {ok, #mrst{
        language = Lang,
        views = Views
    }} = couch_views_util:ddoc_to_mrst(DbName, DDoc),

    Views =/= [] andalso
        couch_eval:with_context(#{language => Lang}, fun(Ctx) ->
            lists:foreach(fun(V) -> ValidateView(Ctx, V) end, Views)
        end),
    ok.

validate_args(#mrst{} = State, Args0) ->
    Args = validate_args(Args0),

    ViewPartitioned = State#mrst.partitioned,
    Partition = get_extra(Args, partition),

    case {ViewPartitioned, Partition} of
        {true, undefined} ->
            Msg1 = <<
                "`partition` parameter is mandatory "
                "for queries to this view."
            >>,
            mrverror(Msg1);
        {true, _} ->
            apply_partition(Args, Partition);
        {false, undefined} ->
            Args;
        {false, Value} when is_binary(Value) ->
            Msg2 = <<
                "`partition` parameter is not "
                "supported in this design doc"
            >>,
            mrverror(Msg2)
    end.

validate_ddoc_fields(DDoc) ->
    MapFuncType = map_function_type(DDoc),
    lists:foreach(
        fun(Path) ->
            validate_ddoc_fields(DDoc, Path)
        end,
        [
            [{<<"filters">>, object}, {any, [object, string]}],
            [{<<"language">>, string}],
            [{<<"lists">>, object}, {any, [object, string]}],
            [{<<"options">>, object}],
            [{<<"options">>, object}, {<<"include_design">>, boolean}],
            [{<<"options">>, object}, {<<"local_seq">>, boolean}],
            [{<<"options">>, object}, {<<"partitioned">>, boolean}],
            [{<<"rewrites">>, [string, array]}],
            [{<<"shows">>, object}, {any, [object, string]}],
            [{<<"updates">>, object}, {any, [object, string]}],
            [{<<"validate_doc_update">>, string}],
            [{<<"views">>, object}, {<<"lib">>, object}],
            [{<<"views">>, object}, {any, object}, {<<"map">>, MapFuncType}],
            [{<<"views">>, object}, {any, object}, {<<"reduce">>, string}]
        ]
    ),
    require_map_function_for_views(DDoc),
    ok.

require_map_function_for_views({Props}) ->
    case couch_util:get_value(<<"views">>, Props) of
        undefined ->
            ok;
        {Views} ->
            lists:foreach(
                fun
                    ({<<"lib">>, _}) ->
                        ok;
                    ({Key, {Value}}) ->
                        case couch_util:get_value(<<"map">>, Value) of
                            undefined ->
                                throw(
                                    {invalid_design_doc,
                                        <<"View `", Key/binary, "` must contain map function">>}
                                );
                            _ ->
                                ok
                        end
                end,
                Views
            ),
            ok
    end.

validate_ddoc_fields(DDoc, Path) ->
    case validate_ddoc_fields(DDoc, Path, []) of
        ok ->
            ok;
        {error, {FailedPath0, Type0}} ->
            FailedPath = iolist_to_binary(join(FailedPath0, <<".">>)),
            Type = format_type(Type0),
            throw(
                {invalid_design_doc,
                    <<"`", FailedPath/binary, "` field must have ", Type/binary, " type">>}
            )
    end.

validate_ddoc_fields(undefined, _, _) ->
    ok;
validate_ddoc_fields(_, [], _) ->
    ok;
validate_ddoc_fields({KVS} = Props, [{any, Type} | Rest], Acc) ->
    lists:foldl(
        fun
            ({Key, _}, ok) ->
                validate_ddoc_fields(Props, [{Key, Type} | Rest], Acc);
            ({_, _}, {error, _} = Error) ->
                Error
        end,
        ok,
        KVS
    );
validate_ddoc_fields({KVS} = Props, [{Key, Type} | Rest], Acc) ->
    case validate_ddoc_field(Props, {Key, Type}) of
        ok ->
            validate_ddoc_fields(
                couch_util:get_value(Key, KVS),
                Rest,
                [Key | Acc]
            );
        error ->
            {error, {[Key | Acc], Type}};
        {error, Key1} ->
            {error, {[Key1 | Acc], Type}}
    end.

validate_ddoc_field(undefined, Type) when is_atom(Type) ->
    ok;
validate_ddoc_field(_, any) ->
    ok;
validate_ddoc_field(Value, Types) when is_list(Types) ->
    lists:foldl(
        fun
            (_, ok) -> ok;
            (Type, _) -> validate_ddoc_field(Value, Type)
        end,
        error,
        Types
    );
validate_ddoc_field(Value, string) when is_binary(Value) ->
    ok;
validate_ddoc_field(Value, array) when is_list(Value) ->
    ok;
validate_ddoc_field({Value}, object) when is_list(Value) ->
    ok;
validate_ddoc_field(Value, boolean) when is_boolean(Value) ->
    ok;
validate_ddoc_field({Props}, {any, Type}) ->
    validate_ddoc_field1(Props, Type);
validate_ddoc_field({Props}, {Key, Type}) ->
    validate_ddoc_field(couch_util:get_value(Key, Props), Type);
validate_ddoc_field(_, _) ->
    error.

validate_ddoc_field1([], _) ->
    ok;
validate_ddoc_field1([{Key, Value} | Rest], Type) ->
    case validate_ddoc_field(Value, Type) of
        ok ->
            validate_ddoc_field1(Rest, Type);
        error ->
            {error, Key}
    end.

map_function_type({Props}) ->
    case couch_util:get_value(<<"language">>, Props) of
        <<"query">> -> object;
        _ -> string
    end.

format_type(Type) when is_atom(Type) ->
    ?l2b(atom_to_list(Type));
format_type(Types) when is_list(Types) ->
    iolist_to_binary(join(lists:map(fun atom_to_list/1, Types), <<" or ">>)).

join(L, Sep) ->
    join(L, Sep, []).

join([H | []], _, Acc) ->
    [H | Acc];
join([H | T], Sep, Acc) ->
    join(T, Sep, [Sep, H | Acc]).

determine_group_level(#mrargs{group = undefined, group_level = undefined}) ->
    0;
determine_group_level(#mrargs{group = false, group_level = undefined}) ->
    0;
determine_group_level(#mrargs{group = false, group_level = Level}) when Level > 0 ->
    mrverror(<<"Can't specify group=false and group_level>0 at the same time">>);
determine_group_level(#mrargs{group = true, group_level = undefined}) ->
    exact;
determine_group_level(#mrargs{group_level = GroupLevel}) ->
    GroupLevel.

mrverror(Mesg) ->
    throw({query_parse_error, Mesg}).

apply_partition(#mrargs{keys = [{p, _, _} | _]} = Args, _Partition) ->
    % already applied
    Args;
apply_partition(#mrargs{keys = Keys} = Args, Partition) when Keys /= undefined ->
    Args#mrargs{keys = [{p, Partition, K} || K <- Keys]};
apply_partition(#mrargs{start_key = {p, _, _}, end_key = {p, _, _}} = Args, _Partition) ->
    % already applied.
    Args;
apply_partition(Args, Partition) ->
    #mrargs{
        direction = Dir,
        start_key = StartKey,
        end_key = EndKey
    } = Args,

    {DefSK, DefEK} =
        case Dir of
            fwd -> {?LOWEST_KEY, ?HIGHEST_KEY};
            rev -> {?HIGHEST_KEY, ?LOWEST_KEY}
        end,

    SK0 =
        if
            StartKey /= undefined -> StartKey;
            true -> DefSK
        end,
    EK0 =
        if
            EndKey /= undefined -> EndKey;
            true -> DefEK
        end,

    Args#mrargs{
        start_key = {p, Partition, SK0},
        end_key = {p, Partition, EK0}
    }.

get_extra(#mrargs{} = Args, Key) ->
    couch_util:get_value(Key, Args#mrargs.extra).

apply_limit(ViewPartitioned, Args) ->
    Options = Args#mrargs.extra,
    IgnorePQLimit = lists:keyfind(ignore_partition_query_limit, 1, Options),
    LimitType =
        case {ViewPartitioned, IgnorePQLimit} of
            {true, false} -> "partition_query_limit";
            {true, _} -> "query_limit";
            {false, _} -> "query_limit"
        end,

    MaxLimit = config:get_integer(
        "query_server_config",
        LimitType,
        ?MAX_VIEW_LIMIT
    ),

    % Set the highest limit possible if a user has not
    % specified a limit
    Args1 =
        case Args#mrargs.limit == ?MAX_VIEW_LIMIT of
            true -> Args#mrargs{limit = MaxLimit};
            false -> Args
        end,

    if
        Args1#mrargs.limit =< MaxLimit ->
            Args1;
        true ->
            Fmt = "Limit is too large, must not exceed ~p",
            mrverror(io_lib:format(Fmt, [MaxLimit]))
    end.
