-module(mango_act_crud_find).

-export([
    init/2,
    run/3,

    format_error/1
]).


-include_lib("couch/include/couch_db.hrl").
-include("mango.hrl").


-record(st, {
    selector,
    opts
}).


init(Db, {Props}) ->
    {ok, Opts} = mango_opts:validate(Props, opts()),
    [<<"find">>, Selector, Limit, Skip, Sort, Fields, R, Conflicts] = Opts,
    #st{
        selector = Selector,
        opts = [
            {user_ctx, Db#db.user_ctx},
            {limit, Limit},
            {skip, Skip},
            {sort, Sort},
            {fields, Fields},
            {r, integer_to_list(R)},
            {conflicts, Conflicts}
        ]
    }.


run(Writer, Db, St) ->
    #st{
        selector = Selector,
        opts = Opts
    } = St,
    {ok, NewWriter} = resp_open(Writer),
    {ok, LastWriter} = do_find(Db, Selector, Opts, NewWriter),
    resp_close(LastWriter).


format_error(Else) ->
    mango_util:fmt("Unknown error: ~p", [Else]).


opts() ->
    [
        {<<"action">>, [
            {assert, <<"find">>}
        ]},
        {<<"selector">>, [
            {validator, fun mango_opts:validate_selector/1}
        ]},
        {<<"limit">>, [
            {optional, true},
            {default, 25},
            {validator, fun mango_opts:is_pos_integer/1}
        ]},
        {<<"skip">>, [
            {optional, true},
            {default, 0},
            {validator, fun mango_opts:is_non_neg_integer/1}
        ]},
        {<<"sort">>, [
            {optional, true},
            {default, []},
            {validator, fun mango_opts:validate_sort/1}
        ]},
        {<<"fields">>, [
            {optional, true},
            {default, []},
            {validator, fun mango_opts:validate_fields/1}
        ]},
        {<<"r">>, [
            {optional, true},
            {default, 2},
            {validator, fun mango_opts:is_pos_integer/1}
        ]},
        {<<"conflicts">>, [
            {optional, true},
            {default, false},
            {validator, fun mango_opts:is_boolean/1}
        ]}
    ].


resp_open(Writer) ->
    mango_writer:script(Writer, [
        obj_open,
        {obj_pair, <<"ok">>, true},
        {obj_key, <<"result">>},
        arr_open
    ]).


resp_close(Writer) ->
    mango_writer:script(Writer, [
        arr_close,
        obj_close
    ]).


do_find(Db, Sel, Opts, Writer) ->
    Self = self(),
    Ref = make_ref(),
    {Pid, _} = spawn_monitor(fun() ->
        CB = fun handle_doc/2,
        mango_crud:find(Db, Sel, CB, {Self, Ref}, Opts),
        Self ! {Ref, self(), complete}
    end),
    run_find(Writer, Pid, Ref).


handle_doc({row, Doc}, {Parent, Ref} = St) ->
    Parent ! {Ref, self(), Doc},
    receive
        {Ref, Status} ->
            {Status, St}
        after 300000 ->
            {stop, St}
    end.


run_find(Writer, Pid, Ref) ->
    receive
        {Ref, _From, complete} ->
            {ok, Writer};
        {Ref, From, Doc} ->
            % Its important to note that From is not the same
            % pid() as Pid because Pid did a mango_util:defer/3
            {ok, NewWriter} = mango_writer:add_value(Writer, Doc),
            From ! {Ref, ok},
            run_find(NewWriter, Pid, Ref);
        {'DOWN', _, _, Pid, Reason} ->
            throw({error, Reason})
        after 60000 ->
            throw({error, timeout})
    end.
