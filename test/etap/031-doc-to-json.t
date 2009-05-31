#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./src/couchdb -pa ./src/mochiweb -sasl errlog_type false -noshell


%% XXX: Figure out how to -include("couch_db.hrl")
-record(doc, {id= <<"">>, revs={0, []}, body={[]},
            attachments=[], deleted=false, meta=[]}).

main(_) ->
    code:add_pathz("src/couchdb"),
    etap:plan(unknown),
    case (catch test()) of
        ok ->
            etap:end_tests();
        Other ->
            etap:diag(io_lib:format("Test died abnormally: ~p", [Other])),
            etap:bail()
    end,
    ok.

test() ->
    ok = test_to_json_success(),
    ok.
    
test_to_json_success() ->
    Cases = [
        {
            #doc{},
            {[{<<"_id">>, <<"">>}]},
            "Empty docs are {\"_id\": \"\"}"
        },
        {
            #doc{id= <<"foo">>},
            {[{<<"_id">>, <<"foo">>}]},
            "_id is added."
        },
        {
            #doc{revs={5, ["foo"]}},
            {[{<<"_id">>, <<>>}, {<<"_rev">>, <<"5-foo">>}]},
            "_rev is added."
        },
        {
            [revs],
            #doc{revs={5, [<<"first">>, <<"second">>]}},
            {[
                {<<"_id">>, <<>>},
                {<<"_rev">>, <<"5-first">>},
                {<<"_revisions">>, {[
                    {<<"start">>, 5},
                    {<<"ids">>, [<<"first">>, <<"second">>]}
                ]}}
            ]},
            "_revisions include with revs option"
        },
        {
            #doc{body={[{<<"foo">>, <<"bar">>}]}},
            {[{<<"_id">>, <<>>}, {<<"foo">>, <<"bar">>}]},
            "Arbitrary fields are added."
        },
        {
            #doc{deleted=true, body={[{<<"foo">>, <<"bar">>}]}},
            {[{<<"_id">>, <<>>}, {<<"_deleted">>, true}]},
            "Deleted docs drop body members."
        },
        {
            #doc{meta=[
                {revs_info, 4, [{<<"fin">>, deleted}, {<<"zim">>, missing}]}
            ]},
            {[
                {<<"_id">>, <<>>},
                {<<"_revs_info">>, [
                    {[{<<"rev">>, <<"4-fin">>}, {<<"status">>, <<"deleted">>}]},
                    {[{<<"rev">>, <<"3-zim">>}, {<<"status">>, <<"missing">>}]}
                ]}
            ]},
            "_revs_info field is added correctly."
        },
        {
            #doc{meta=[{local_seq, 5}]},
            {[{<<"_id">>, <<>>}, {<<"_local_seq">>, 5}]},
            "_local_seq is added as an integer."
        },
        {
            #doc{meta=[{conflicts, [{3, <<"yep">>}, {1, <<"snow">>}]}]},
            {[
                {<<"_id">>, <<>>},
                {<<"_conflicts">>, [<<"3-yep">>, <<"1-snow">>]}
            ]},
            "_conflicts is added as an array of strings."
        },
        {
            #doc{meta=[{deleted_conflicts, [{10923, <<"big_cowboy_hat">>}]}]},
            {[
                {<<"_id">>, <<>>}, 
                {<<"_deleted_conflicts">>, [<<"10923-big_cowboy_hat">>]}
            ]},
            "_deleted_conflicsts is added as an array of strings."
        },
        {
            #doc{attachments=[
                {<<"big.xml">>, {<<"xml/sucks">>, {fun() -> ok end, 400}}},
                {<<"fast.json">>, {<<"json/ftw">>, <<"{\"so\": \"there!\"}">>}}
            ]},
            {[
                {<<"_id">>, <<>>},
                {<<"_attachments">>, {[
                    {<<"big.xml">>, {[
                        {<<"stub">>, true},
                        {<<"content_type">>, <<"xml/sucks">>},
                        {<<"length">>, 400}
                    ]}},
                    {<<"fast.json">>, {[
                        {<<"stub">>, true},
                        {<<"content_type">>, <<"json/ftw">>},
                        {<<"length">>, 16}
                    ]}}
                ]}}
            ]},
            "Attachments attached as stubs only include a length."
        },
        {
            [attachments],
            #doc{attachments=[
                {<<"stuff.txt">>,
                    {<<"text/plain">>, {fun() -> <<"diet pepsi">> end, 10}}},
                {<<"food.now">>, {<<"application/food">>, <<"sammich">>}}
            ]},
            {[
                {<<"_id">>, <<>>},
                {<<"_attachments">>, {[
                    {<<"stuff.txt">>, {[
                        {<<"content_type">>, <<"text/plain">>},
                        {<<"data">>, <<"ZGlldCBwZXBzaQ==">>}
                    ]}},
                    {<<"food.now">>, {[
                        {<<"content_type">>, <<"application/food">>},
                        {<<"data">>, <<"c2FtbWljaA==">>}
                    ]}}
                ]}}
            ]},
            "Attachments included inline with attachments option."
        }
    ],

    lists:foreach(fun
        ({Doc, EJson, Mesg}) ->
            etap:is(couch_doc:to_json_obj(Doc, []), EJson, Mesg);
        ({Options, Doc, EJson, Mesg}) ->
            etap:is(couch_doc:to_json_obj(Doc, Options), EJson, Mesg)
    end, Cases),
    ok.

