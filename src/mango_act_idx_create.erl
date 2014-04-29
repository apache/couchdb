-module(mango_act_idx_create).

-export([
    init/2,
    run/3,

    format_error/1
]).


-record(st, {
    def,
    opts
}).


-include_lib("couch/include/couch_db.hrl").
-include("mango.hrl").


init(Db, {Props}) ->
    {ok, Opts} = mango_opts:validate(Props, opts()),
    [<<"create_index">>, Def, Type, Name, DDoc] = Opts,
    #st{
        def = Def,
        opts = [
            {user_ctx, Db#db.user_ctx},
            {type, Type},
            {name, Name},
            {ddoc, DDoc}
        ]
    }.


run(_Writer, Db, St) ->
    {ok, Status} = mango_index:create(Db, St#st.def, St#st.opts),
    {ok, {[
        {ok, true},
        {result, Status}
    ]}}.


format_error({invalid_index_type, Type}) ->
    mango_util:fmt("Invalid index type: ~w", [Type]);
format_error(Else) ->
    mango_util:fmt("Unknown error: ~p", [Else]).


opts() ->
    [
        {<<"action">>, [
            {assert, <<"create_index">>}
        ]},
        {<<"index">>, []},
        {<<"type">>, [
            {optional, true},
            {default, <<"json">>},
            {validator, fun mango_opts:is_string/1}
        ]},
        {<<"name">>, [
            {optional, true},
            {default, auto_name},
            {validator, fun validate_name/1}
        ]},
        {<<"ddoc">>, [
            {optional, true},
            {default, auto_name},
            {validator, fun validate_name/1}
        ]}
    ].


validate_name(auto_name) ->
    {ok, auto_name};
validate_name(Else) ->
    mango_opts:is_string(Else).

