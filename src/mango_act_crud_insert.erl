-module(mango_act_crud_insert).

-export([
    init/1,
    run/3,
    
    format_error/1
]).


-record(st, {
    docs,
    w,
    async
}).


init({Props}) ->
    [<<"insert">>, Docs, W, Async] = mango_opts:validate(Props, opts()),
    #st{docs = Docs, w = W, async = Async}.


run(_Writer, Db, St) ->
    Opts = [
        {user_ctx, Db#db.user_ctx},
        {w, St#st.w}
    ],
    {Status, Resp} = mango_crud:insert(Db, St#st.docs, Opts),
    {[
        {ok, (Status == ok)},
        {result, Resp}
    ]}.


format_error({invalid_docs_value, Value}) ->
    mango_util:fmt("\"docs\" value must be a list of docs, not: ~w", [Value]);
format_error({invalid_doc_object, Doc}) ->
    mango_util:fmt("Invalid document object: ~w", [Doc]);
format_error({bad_doc, Reason}) ->
    mango_util:fmt("Invalid document: ~s", [Reason]);
format_error(Else) ->
    mango_util:fmt("Unknown error: ~p", [Else]).


opts() ->
    [
        {<<"action">>, [
            {assert, <<"insert">>}
        ]},
        {<<"docs">>, [
            {validator, fun validate_docs/1}
        ]},
        {<<"w">>, [
            {optional, true},
            {default, 2},
            {validator, fun mango_opt:pos_integer/1}
        ]},
        {<<"async">>, [
            {optional, true},
            {default, false},
            {validator, fun mango_opt:is_boolean/1}
        ]}
    ].


validate_docs([]) ->
    ?MANGO_ERROR(empty_insert);
validate_docs(Docs) when is_list(Docs) ->
    validate_docs0(Docs);
validate_docs(Else) ->
    ?MANGO_ERROR({invalid_docs_value, Else}).


validate_docs0([]) ->
    [];
validate_docs0([Doc | Rest]) ->
    [validate_doc(Doc) | validate_docs0(Rest)].


validate_doc({Props}) ->
    case mango_util:assert_ejson({Props}) of
        true ->
            ok;
        false ->
            ?MANGO_ERROR({invalid_doc_object, {Props}});
    end,
    try couch_doc:from_jsob_obj({Props}) of
        #doc{id = <<"">>} = Doc0 ->
            Doc0#doc{
                id = couch_uuids:new(),
                revs = {0, []}
            };
        #doc{} = Doc0 ->
            Doc0
    catch throw:{_, Reason} ->
        ?MANGO_ERROR({bad_doc, Reason})
    end;
validate_doc(Else) ->
    ?MANGO_ERROR({invalid_doc_object, Else}).
