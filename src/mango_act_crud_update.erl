-module(mango_act_crud_update).

-export([
    init/2,
    run/3,

    format_error/1
]).


-include_lib("couch/include/couch_db.hrl").
-include("mango.hrl").


-record(st, {
    selector,
    update,
    opts
}).


init(Db, {Props}) ->
    {ok, Opts} = mango_opts:validate(Props, opts()),
    [<<"update">>, Selector, Update, Upsert, Limit, Sort, R, W] = Opts,
    #st{
        selector = Selector,
        update = Update,
        opts = [
            {user_ctx, Db#db.user_ctx},
            {upsert, Upsert},
            {limit, Limit},
            {sort, Sort},
            {r, integer_to_list(R)},
            {w, integer_to_list(W)}
        ]
    }.


run(_Writer, Db, St) ->
    #st{
        selector = Selector,
        update = Update,
        opts = Opts
    } = St,
    {Status, Resp} = mango_crud:update(Db, Selector, Update, Opts),
    twig:log(err, "RESP: ~p", [Resp]),
    {ok, {[
        {ok, (Status == ok)},
        {result, Resp}
    ]}}.


format_error(Else) ->
    mango_util:fmt("Unknown error: ~p", [Else]).


opts() ->
    [
        {<<"action">>, [
            {assert, <<"update">>}
        ]},
        {<<"selector">>, [
            {validator, fun mango_opts:validate_selector/1}
        ]},
        {<<"update">>, [
            {validator, fun mango_opts:is_object/1}
        ]},
        {<<"upsert">>, [
            {optional, true},
            {default, false},
            {validator, fun mango_opts:is_boolean/1}
        ]},
        {<<"limit">>, [
            {optional, true},
            {default, 1},
            {validator, fun mango_opts:is_pos_integer/1}
        ]},
        {<<"sort">>, [
            {optional, true},
            {default, []},
            {validator, fun mango_opts:validate_sort/1}
        ]},
        {<<"r">>, [
            {optional, true},
            {default, 1},
            {validator, fun mango_opts:is_pos_integer/1}
        ]},
        {<<"w">>, [
            {optional, true},
            {default, 2},
            {validator, fun mango_opts:is_pos_integer/1}
        ]}
    ].
