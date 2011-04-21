#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./src/couchdb -pa ./src/mochiweb -sasl errlog_type false -noshell

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

%% XXX: Figure out how to -include("couch_db.hrl")
-record(doc, {id= <<"">>, revs={0, []}, body={[]},
            atts=[], deleted=false, meta=[]}).
-record(att, {name, type, att_len, disk_len, md5= <<>>, revpos=0, data,
            encoding=identity}).

main(_) ->
    test_util:init_code_path(),
    etap:plan(12),
    case (catch test()) of
        ok ->
            etap:end_tests();
        Other ->
            etap:diag(io_lib:format("Test died abnormally: ~p", [Other])),
            etap:bail()
    end,
    ok.

test() ->
    couch_config:start_link(test_util:config_files()),
    couch_config:set("attachments", "compression_level", "0", false),
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
            {[{<<"_id">>, <<>>}, {<<"foo">>, <<"bar">>}, {<<"_deleted">>, true}]},
            "Deleted docs no longer drop body members."
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
            #doc{atts=[
                #att{
                    name = <<"big.xml">>, 
                    type = <<"xml/sucks">>, 
                    data = fun() -> ok end,
                    revpos = 1,
                    att_len = 400,
                    disk_len = 400
                },
                #att{
                    name = <<"fast.json">>, 
                    type = <<"json/ftw">>, 
                    data = <<"{\"so\": \"there!\"}">>,
                    revpos = 1,
                    att_len = 16,
                    disk_len = 16
                }
            ]},
            {[
                {<<"_id">>, <<>>},
                {<<"_attachments">>, {[
                    {<<"big.xml">>, {[
                        {<<"content_type">>, <<"xml/sucks">>},
                        {<<"revpos">>, 1},
                        {<<"length">>, 400},
                        {<<"stub">>, true}
                    ]}},
                    {<<"fast.json">>, {[
                        {<<"content_type">>, <<"json/ftw">>},
                        {<<"revpos">>, 1},
                        {<<"length">>, 16},
                        {<<"stub">>, true}
                    ]}}
                ]}}
            ]},
            "Attachments attached as stubs only include a length."
        },
        {
            [attachments],
            #doc{atts=[
                #att{
                    name = <<"stuff.txt">>,
                    type = <<"text/plain">>,
                    data = fun() -> <<"diet pepsi">> end,
                    revpos = 1,
                    att_len = 10,
                    disk_len = 10
                },
                #att{
                    name = <<"food.now">>,
                    type = <<"application/food">>,
                    revpos = 1,
                    data = <<"sammich">>
                }
            ]},
            {[
                {<<"_id">>, <<>>},
                {<<"_attachments">>, {[
                    {<<"stuff.txt">>, {[
                        {<<"content_type">>, <<"text/plain">>},
                        {<<"revpos">>, 1},
                        {<<"data">>, <<"ZGlldCBwZXBzaQ==">>}
                    ]}},
                    {<<"food.now">>, {[
                        {<<"content_type">>, <<"application/food">>},
                        {<<"revpos">>, 1},
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

